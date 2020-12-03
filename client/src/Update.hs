{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Update
    ( update
    , Msg(..)
    , State(..)
    , Output(..)
    , Init(..)
    , AuthStatus(..)
    , NotLoggedIn(..)
    , Ready(..)
    , ListenStatus(..)
    , ToServer(..)
    ) where

import qualified Control.Exception as E
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString.Base64.URL as B64
import Data.Word (Word8)
import qualified Text.Hex
import qualified Network.Simple.TCP as Tcp
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Data.ByteArray as Ba
import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Data.IntSet as IntSet
import System.IO.Error (isDoesNotExistError)
import Data.Bits ((.&.), shiftR)


data Output
    = GetArgsO
    | GenerateSecretKeyO
    | CloseTcpO Tcp.Socket
    | TcpSendO Tcp.Socket B.ByteString
    | ReadSecretKeyO
    | PrintO T.Text
    | DoNothingO
    | BatchO [Output]
    | MakeTcpConnO
    | ReadMessageFromStdInO
    | WriteKeyToFileO B.ByteString
    | TcpListenO Tcp.Socket Int
    deriving (Eq, Show)


data Msg
    = StartM
    | TcpConnM (Either E.IOException (Tcp.Socket, Tcp.SockAddr))
    | TcpSendResultM (Either E.IOException ())
    | StdInM B.ByteString
    | ArgsM [String]
    | SecretKeyFileM (Either E.IOException B.ByteString)
    | BatchM [Maybe Msg]
    | FromServerM (Either E.IOException (Maybe B.ByteString))
    | NewSecretKeyM Ed.SecretKey
    deriving Show


data State
    = ReadyS Ready
    | InitS Init
    | FinishedS
    deriving (Show, Eq)


data Init
    = EmptyI
    | GettingKeysFromFileI
    | GeneratingSecretKeyI
    deriving (Show, Eq)


data Ready
    = Ready
        { secretKey :: Ed.SecretKey
        , authStatus :: AuthStatus
        , readingStdIn :: Maybe Ed.PublicKey
        }
        deriving (Show, Eq)


data ListenStatus
    = GettingLength
    | GettingBody
    deriving (Show, Eq)


data AuthStatus
    = LoggedInA (Tcp.Socket, ListenStatus)
    | NotLoggedInA NotLoggedIn
    deriving (Show, Eq)


data NotLoggedIn
    = SendWhenLoggedIn (Maybe (Tcp.Socket, ListenStatus)) ToServer
    | JustNotLoggedIn
    deriving (Show, Eq)


updateReady :: State -> (Ready -> (Output, State)) -> (Output, State)
updateReady model f =
    case model of
    InitS _ ->
        (DoNothingO, model)

    ReadyS ready ->
        f ready

    FinishedS ->
        (DoNothingO, model)


usage :: T.Text
usage =
    "Get usage\n\
    \\n\
    \    $ bwt help\n\
    \\n\
    \Get my user ID\n\
    \\n\
    \    $ bwt myid\n\
    \\n\
    \Download a message to STDOUT\n\
    \\n\
    \    $ bwt get\n\
    \\n\
    \Send a message from STDIN\n\
    \\n\
    \    $ bwt send <recipient ID>\n"


updateOnTcpConn
    :: State
    -> Tcp.Socket
    -> (Output, State)
updateOnTcpConn model socket =
    updateReady model $ \ready ->
    ( TcpListenO socket 2
    , ReadyS $
        ready
        { authStatus =
            case authStatus ready of
            LoggedInA _ ->
                authStatus ready

            NotLoggedInA JustNotLoggedIn ->
                authStatus ready

            NotLoggedInA (SendWhenLoggedIn _ toServer) ->
                NotLoggedInA $
                SendWhenLoggedIn
                (Just (socket, GettingLength))
                toServer
        }
    )


stdInP :: P.Parser T.Text
stdInP = do
    msg <- inboxMessageP
    P.endOfInput
    return msg


updateOnStdIn :: State -> B.ByteString -> (Output, State)
updateOnStdIn model raw =
    let
    pass = (DoNothingO, model)
    in
    case P.parseOnly stdInP raw of
    Left err ->
        (toUser $ BadMessageU err, FinishedS)

    Right newMsg ->
        updateReady model $ \ready ->
        case readingStdIn ready of
        Nothing ->
            pass

        Just publicKey ->
            ( MakeTcpConnO
            , ReadyS $
              ready
                { authStatus =
                    NotLoggedInA $
                    SendWhenLoggedIn Nothing $
                    SendMessageT publicKey newMsg
                }
            )


updateOnNewSecretKey :: State -> Ed.SecretKey -> (Output, State)
updateOnNewSecretKey model key =
    let
    pass = (DoNothingO, model)
    in
    case model of
    InitS EmptyI ->
        pass

    InitS GettingKeysFromFileI ->
        pass

    InitS GeneratingSecretKeyI ->
        ( BatchO [WriteKeyToFileO $ Ba.convert key, GetArgsO]
        , ReadyS $
          Ready
            { secretKey = key
            , authStatus = NotLoggedInA JustNotLoggedIn
            , readingStdIn = Nothing
            }
        )

    ReadyS _ ->
        pass

    FinishedS ->
        pass


updateOnMyIdArg :: State -> (Output, State)
updateOnMyIdArg model =
    case model of
    InitS _ ->
        (DoNothingO, model)

    ReadyS ready ->
        let
        public = Ed.toPublic $ secretKey ready
        idB64 = B64.encodeUnpadded $ Ba.convert public
        in
        case decodeUtf8' idB64 of
        Left err ->
            ( toUser $ InternalErrorU $
                mconcat
                [ "could not convert public key to Base64:\n"
                , T.pack $ show err
                ]
            , FinishedS
            )

        Right b64 ->
            (toUser $ MyIdU b64, FinishedS)

    FinishedS ->
        (DoNothingO, model)


updateOnGetArg :: State -> (Output, State)
updateOnGetArg model =
    let
    pass = (DoNothingO, model)
    in
    updateReady model $ \ready ->
    case authStatus ready of
    LoggedInA _ ->
        pass

    NotLoggedInA JustNotLoggedIn ->
        ( MakeTcpConnO
        , ReadyS $ ready
            { authStatus =
                NotLoggedInA $
                SendWhenLoggedIn Nothing GetMessageT
            }
        )

    NotLoggedInA (SendWhenLoggedIn (Just _) _) ->
        pass

    NotLoggedInA (SendWhenLoggedIn Nothing _) ->
        pass


updateOnSendArg :: State -> String -> (Output, State)
updateOnSendArg model rawRecipient =
    case parseRecipient rawRecipient of
    Left err ->
        (toUser $ InvalidRecipientU err, FinishedS)

    Right recipient ->
        updateReady model $ \ready ->
        ( ReadMessageFromStdInO
        , ReadyS $ ready { readingStdIn = Just recipient }
        )


updateOnBadSecretKeyFile :: State -> IOError -> (Output, State)
updateOnBadSecretKeyFile model ioErr =
    let
    pass = (DoNothingO, model)
    in
    if isDoesNotExistError ioErr then
    case model of
    ReadyS _ ->
        pass

    InitS GettingKeysFromFileI ->
        (GenerateSecretKeyO, InitS GeneratingSecretKeyI)

    InitS EmptyI ->
        pass

    InitS GeneratingSecretKeyI ->
        pass

    FinishedS ->
        pass

    else
    ( toUser $ InternalErrorU $
        "could not read secret key file:\n" <> T.pack (show ioErr)
    , FinishedS
    )


updateOnGoodSecretKeyFile :: State -> B.ByteString -> (Output, State)
updateOnGoodSecretKeyFile model raw =
    let
    pass = (DoNothingO, model)
    in
    case model of
    InitS EmptyI ->
        pass

    InitS GeneratingSecretKeyI ->
        pass

    InitS GettingKeysFromFileI ->
        case P.parseOnly secretSigningP raw of
        Left err ->
            ( toUser $ InternalErrorU $
              mconcat
              [ "corrupted secret key file: \n"
              , T.pack err
              , ": \n"
              , Text.Hex.encodeHex raw
              ]
            , FinishedS
            )

        Right key ->
            ( GetArgsO
            , ReadyS $
              Ready
                { secretKey = key
                , authStatus = NotLoggedInA JustNotLoggedIn
                , readingStdIn = Nothing
                }
            )

    ReadyS _ ->
        pass

    FinishedS ->
        pass


update :: State -> Msg -> (Output, State)
update model msg =
    let
    pass = (DoNothingO, model)
    in
    case msg of
    TcpSendResultM (Right ()) ->
        pass

    TcpSendResultM (Left _) ->
        updateReady model $ \ready ->
        ( BatchO [toUser NotConnectedU, killConn $ authStatus ready]
        , FinishedS
        )

    TcpConnM (Left _) ->
        (toUser NotConnectedU, FinishedS)

    TcpConnM (Right (socket, _)) ->
        updateOnTcpConn model socket

    StartM ->
        (ReadSecretKeyO, InitS GettingKeysFromFileI)

    StdInM raw ->
        updateOnStdIn model raw

    NewSecretKeyM key ->
        updateOnNewSecretKey model key

    ArgsM ["help"] ->
        (toUser UsageU, FinishedS)

    ArgsM ["myid"] ->
        updateOnMyIdArg model

    ArgsM ["get"] ->
        updateOnGetArg model

    ArgsM ["send", rawRecipient] ->
        updateOnSendArg model rawRecipient

    ArgsM _ ->
        (toUser BadArgsU, FinishedS)

    BatchM msgs ->
        let
        (outputs, newModel) = batchUpdate model msgs []
        in
        (BatchO outputs, newModel)

    SecretKeyFileM (Left ioErr) ->
        updateOnBadSecretKeyFile model ioErr

    SecretKeyFileM (Right raw) ->
        updateOnGoodSecretKeyFile model raw

    FromServerM (Left _) ->
        ( BatchO
            [ toUser NotConnectedU
            , case model of
                ReadyS ready ->
                    killConn $ authStatus ready

                _ ->
                    DoNothingO
            ]
        , FinishedS
        )

    FromServerM (Right Nothing) ->
        ( BatchO
            [ toUser NotConnectedU
            , case model of
                ReadyS ready ->
                    killConn $ authStatus ready

                _ ->
                    DoNothingO
            ]
        , FinishedS
        )

    FromServerM (Right (Just raw)) ->
        updateReady model $ fromServerUpdate raw


data ToUser
    = NotConnectedU
    | BadArgsU
    | BadMessageU String
    | NoMessagesU
    | NewMessageU Ed.PublicKey T.Text
    | BadMessageFromServerU B.ByteString String
    | InternalErrorU T.Text
    | MyIdU T.Text
    | UsageU
    | InvalidRecipientU T.Text


toUser :: ToUser -> Output
toUser message =
    PrintO $ prettyMessage message <> "\n"


prettyMessage :: ToUser -> T.Text
prettyMessage msg =
    case msg of
    InvalidRecipientU err ->
        "invalid recipient:\n" <> err

    BadMessageU err ->
        "bad message:\n" <> T.pack err

    NotConnectedU ->
        "could not connect to the internet"

    BadArgsU ->
        "bad arguments\n\nUsage instructions:\n" <> usage

    NoMessagesU ->
        "no messages"

    NewMessageU sender message ->
        case decodeUtf8' $ B64.encodeUnpadded $ Ba.convert sender of
        Left err ->
            mconcat
            [ "internal error: could not decode Base64 ByteString:\n"
            , T.pack $ show err
            , ":\n"
            , Text.Hex.encodeHex $ Ba.convert sender
            ]

        Right b64 ->
            b64 <> ": " <> message

    BadMessageFromServerU badMsg parseErr ->
        mconcat
        [ "Received bad message from server:\nraw bytes: "
        , Text.Hex.encodeHex badMsg
        , "\nparse error: "
        , T.pack parseErr
        ]

    InternalErrorU err ->
        "Internal error. This is a bug in the program.\n" <> err

    MyIdU id_ ->
        id_

    UsageU ->
        usage


parseRecipient :: String -> Either T.Text Ed.PublicKey
parseRecipient raw =
    case B64.decodeUnpadded $ encodeUtf8 $ T.pack raw of
    Left err ->
        Left $ "could not decode Base64: " <> T.pack err

    Right bs ->
        if B.length bs == Ed.publicKeySize then
        case Ed.publicKey bs of
        CryptoFailed err ->
            Left $
            mconcat
            [ "could not parse public key:\n"
            , T.pack $ show err
            , ":\n"
            , Text.Hex.encodeHex bs
            ]

        CryptoPassed key ->
            Right key

        else
        Left $
        mconcat
        [ "decoded key should be "
        , T.pack $ show Ed.publicKeySize
        , " bytes long, but was "
        , T.pack $ show $ B.length bs
        ]


-- Assocated data
authCodeA :: B.ByteString
authCodeA =
    B.pack [197, 154, 22, 2, 21, 159, 38, 105, 240, 15, 236, 142, 31, 124, 100, 71, 22, 117, 69, 163, 39, 221, 135, 100, 193, 244, 134, 63, 28, 226, 89, 31]


data ToServer
    = SignedAuthCodeT Ed.PublicKey Ed.Signature
    | SendMessageT Ed.PublicKey T.Text
    | GetMessageT
    deriving (Eq, Show)


encodeToServer :: ToServer -> B.ByteString
encodeToServer toServer =
    let
    encoded = encodeToServerHelp toServer
    len = B.length encoded
    in
    encodeUint16 len <> encoded


encodeUint16 :: Int -> B.ByteString
encodeUint16 i =
    B.pack
    [ fromIntegral $ i .&. 0xFF
    , fromIntegral $ (shiftR i 8) .&. 0xFF
    ]


encodeToServerHelp :: ToServer -> B.ByteString
encodeToServerHelp toServer =
    case toServer of
    SignedAuthCodeT publicKey signature ->
        mconcat
        [ B.singleton 0
        , Ba.convert publicKey
        , Ba.convert signature
        ]

    SendMessageT recipient message ->
        mconcat
        [ B.singleton 1
        , Ba.convert recipient
        , encodeUtf8 message
        ]

    GetMessageT ->
        B.singleton 2


onBodyFromServer :: Ready -> B.ByteString -> (Output, State)
onBodyFromServer ready raw =
    case P.parseOnly fromServerP raw of
    Left err ->
        ( BatchO
            [ toUser $ BadMessageFromServerU raw err
            , killConn $ authStatus ready
            ]
        , FinishedS
        )

    Right ok ->
        updateOnGoodFromServer ready ok


onLengthFromServer
    :: Ready
    -> Tcp.Socket
    -> B.ByteString
    -> (Output, State)
onLengthFromServer ready socket raw =
    case P.parseOnly uint16P raw of
    Left err ->
        ( BatchO
            [ toUser $ BadMessageFromServerU raw err
            , killConn $ authStatus ready
            ]
        , FinishedS
        )

    Right len ->
        ( TcpListenO socket len
        , ReadyS $
          ready { authStatus = LoggedInA (socket, GettingBody) }
        )


fromServerUpdate :: B.ByteString -> Ready -> (Output, State)
fromServerUpdate raw ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case authStatus ready of
    LoggedInA (_, GettingBody) ->
        onBodyFromServer ready raw

    LoggedInA (socket, GettingLength) ->
        onLengthFromServer ready socket raw

    NotLoggedInA (SendWhenLoggedIn Nothing _) ->
        pass

    NotLoggedInA (SendWhenLoggedIn (Just (_, GettingBody)) _) ->
        onBodyFromServer ready raw

    NotLoggedInA (SendWhenLoggedIn (Just (conn, GettingLength)) _) ->
        onLengthFromServer ready conn raw

    NotLoggedInA JustNotLoggedIn ->
        pass


updateOnGoodFromServer :: Ready -> FromServer -> (Output, State)
updateOnGoodFromServer ready fromServer =
    case fromServer of
    InboxMessageF sender message ->
        ( BatchO
            [ toUser $ NewMessageU sender message
            , killConn $ authStatus ready
            ]
        , FinishedS
        )

    AuthCodeToSignF authCode ->
        updateOnAuthCode ready authCode

    NoMessagesF ->
        ( BatchO
            [ toUser NoMessagesU
            , killConn $ authStatus ready
            ]
        , FinishedS
        )


updateOnAuthCode :: Ready -> AuthCode -> (Output, State)
updateOnAuthCode ready (AuthCode authCode) =
    let
    pass = (DoNothingO, ReadyS ready)
    publicKey = Ed.toPublic $ secretKey ready
    toSign = authCodeA <> authCode
    signature = Ed.sign (secretKey ready) publicKey toSign
    in
    case authStatus ready of
    LoggedInA _ ->
        pass

    NotLoggedInA JustNotLoggedIn ->
        pass

    NotLoggedInA (SendWhenLoggedIn Nothing _) ->
        pass

    NotLoggedInA (SendWhenLoggedIn (Just (socket, _)) toServer) ->
        ( BatchO
            [ TcpSendO socket $
                encodeToServer $
                SignedAuthCodeT publicKey signature
            , TcpSendO socket $ encodeToServer $ toServer
            , case toServer of
                SignedAuthCodeT _ _ ->
                    DoNothingO

                GetMessageT ->
                    TcpListenO socket 2

                SendMessageT _ _ ->
                    CloseTcpO socket
            ]
        , case toServer of
            SignedAuthCodeT _ _ ->
                ReadyS $
                ready
                    { authStatus =
                        LoggedInA (socket, GettingLength)
                    }

            GetMessageT ->
                ReadyS $
                ready
                    { authStatus =
                        LoggedInA (socket, GettingLength)
                    }

            SendMessageT _ _ ->
                FinishedS
        )


killConn :: AuthStatus -> Output
killConn auth =
    case auth of
    LoggedInA (socket, _) ->
        CloseTcpO socket

    NotLoggedInA (SendWhenLoggedIn Nothing _) ->
        DoNothingO

    NotLoggedInA (SendWhenLoggedIn (Just (socket, _)) _) ->
        CloseTcpO socket

    NotLoggedInA JustNotLoggedIn ->
        DoNothingO


fromServerP :: P.Parser FromServer
fromServerP = do
    msg <- P.choice
        [ do
            _ <- P.word8 0
            AuthCodeToSignF <$> authCodeP
        , do
            _ <- P.word8 1
            sender <- publicKeyP
            InboxMessageF sender <$> inboxMessageP
        , do
            _ <- P.word8 2
            return NoMessagesF
        ]
    P.endOfInput
    return msg


authCodeLength :: Int
authCodeLength =
    32


authCodeP :: P.Parser AuthCode
authCodeP = do
    raw <- P.take authCodeLength
    return $ AuthCode raw


inboxMessageP :: P.Parser T.Text
inboxMessageP = do
    raw <- P.scan 0 msgScanner
    if B.null raw then
        fail "empty"
    else
        case decodeUtf8' raw of
            Left err ->
                fail $ show err

            Right valid -> do
                return valid



msgScanner :: Int -> Word8 -> Maybe Int
msgScanner counter byte =
    if counter < 100 && validByte byte then
    Just $ counter + 1
    else
    Nothing


validBytes :: IntSet.IntSet
validBytes =
    IntSet.fromList
        [ 32 -- space
        , 33 -- !
        , 34 -- "
        , 35 -- #
        , 36 -- $
        , 37 -- %
        , 38 -- &
        , 39 -- '
        , 40 -- (
        , 41 -- )
        , 42 -- *
        , 43 -- +
        , 44 -- ,
        , 45 -- -
        , 46 -- .
        , 47 -- /
        , 48 -- 0
        , 49 -- 1
        , 50 -- 2
        , 51 -- 3
        , 52 -- 4
        , 53 -- 5
        , 54 -- 6
        , 55 -- 7
        , 56 -- 8
        , 57 -- 9
        , 58 -- :
        , 59 -- ;
        , 60 -- <
        , 61 -- =
        , 62 -- >
        , 63 -- ?
        , 64 -- @
        , 65 -- A
        , 66 -- B
        , 67 -- C
        , 68 -- D
        , 69 -- E
        , 70 -- F
        , 71 -- G
        , 72 -- H
        , 73 -- I
        , 74 -- J
        , 75 -- K
        , 76 -- J
        , 77 -- M
        , 78 -- N
        , 79 -- O
        , 80 -- P
        , 81 -- Q
        , 82 -- R
        , 83 -- S
        , 84 -- T
        , 85 -- U
        , 86 -- V
        , 87 -- W
        , 88 -- X
        , 89 -- Y
        , 90 -- Z
        , 91 -- [
        , 92 -- \
        , 93 -- ]
        , 94 -- ^
        , 95 -- _
        -- 96 is backtick, which I don't want
        , 97 -- a
        , 98 -- b
        , 99 -- c
        , 100 -- d
        , 101 -- e
        , 102 -- f
        , 103 -- g
        , 104 -- h
        , 105 -- i
        , 106 -- j
        , 107 -- k
        , 108 -- l
        , 109 -- m
        , 110 -- n
        , 111 -- o
        , 112 -- p
        , 113 -- q
        , 114 -- r
        , 115 -- s
        , 116 -- t
        , 117 -- u
        , 118 -- v
        , 119 -- w
        , 120 -- x
        , 121 -- y
        , 122 -- z
        , 123 -- {
        , 124 -- |
        , 125 -- }
        , 126 -- ~
        ]


validByte :: Word8 -> Bool
validByte byte =
    IntSet.member (fromIntegral byte) validBytes


publicKeyP :: P.Parser Ed.PublicKey
publicKeyP = do
    raw <- P.take Ed.publicKeySize
    case Ed.publicKey raw of
        CryptoFailed err ->
            fail $ show err

        CryptoPassed key ->
            return key


data FromServer
    = AuthCodeToSignF AuthCode
    | InboxMessageF Ed.PublicKey T.Text
    | NoMessagesF


newtype AuthCode
    = AuthCode B.ByteString


batchUpdate :: State -> [Maybe Msg] -> [Output] -> ([Output], State)
batchUpdate model msgs outputs =
    case msgs of
    [] ->
        (outputs, model)

    Nothing : sgs ->
        batchUpdate model sgs outputs

    Just m : sgs ->
        let
        (output, newModel) = update model m
        in
        batchUpdate newModel sgs (output : outputs)


secretSigningP :: P.Parser Ed.SecretKey
secretSigningP = do
    raw <- P.take Ed.secretKeySize
    case Ed.secretKey raw of
        CryptoFailed err ->
            fail $ show err

        CryptoPassed key ->
            return key


uint16P :: P.Parser Int
uint16P = do
    b0 <- uint8P
    b1 <- uint8P
    return $ b0 + b1 * 256


uint8P :: P.Parser Int
uint8P =
    fromIntegral <$> P.anyWord8

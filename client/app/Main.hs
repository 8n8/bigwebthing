{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main (main) where

import qualified Data.Text.IO as Tio
import System.Environment (getArgs)
import qualified Control.Exception as E
import qualified Control.Concurrent.STM as Stm
import qualified Control.Concurrent.STM.TQueue as Q
import qualified Control.Concurrent.STM.TVar as TVar
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


main :: IO ()
main = do
    model <- TVar.newTVarIO (InitS EmptyI)
    updateIo model StartM


updateIo :: TVar.TVar State -> Msg -> IO ()
updateIo mainState msg = do
    output <- Stm.atomically $ do
        model <- TVar.readTVar mainState
        let (output, newModel) = update model msg
        TVar.writeTVar mainState newModel
        return output
    io mainState output


io :: TVar.TVar State -> Output -> IO ()
io mainState output =
    case output of
    CloseTcpO socket ->
        Tcp.closeSock socket

    TcpSendO socket msg -> do
        eitherError <- E.try $ Tcp.send socket msg
        updateIo mainState $ TcpSendResultM eitherError

    WriteKeyToFileO key -> do
        B.writeFile keysPath key

    PrintO msg -> do
        Tio.putStr msg

    ReadMessageFromStdInO -> do
        stdin <- B.getContents
        updateIo mainState $ StdInM stdin

    GenerateSecretKeyO -> do
        key <- Ed.generateSecretKey
        updateIo mainState $ NewSecretKeyM key

    GetArgsO -> do
        args <- getArgs
        updateIo mainState $ ArgsM args

    ReadSecretKeyO -> do
        raw <- E.try $ B.readFile keysPath
        updateIo mainState $ SecretKeyFileM raw

    DoNothingO ->
        return ()

    BatchO outputs -> do
        mapM_ (io mainState) outputs

    BytesInQO q bytes ->
        Stm.atomically $
            Q.writeTQueue q bytes

    MakeTcpConnO ->
        makeTcpConn mainState


maxMessageLength =
    16000


makeTcpConn :: TVar.TVar State -> IO ()
makeTcpConn model = do
    res <- E.try $ Tcp.connect serverUrl serverPort $ \(conn, _) -> do
            updateIo model $ TcpConnM conn
            tcpListen model conn
    case res of
        Left err ->
            updateIo model $ NoInternetM err

        Right () ->
            return ()


tcpRecv :: Tcp.Socket -> Int -> IO (Either String B.ByteString)
tcpRecv socket size = do
    eitherMaybe <- E.try $ Tcp.recv socket size
    case eitherMaybe :: Either E.IOException (Maybe B.ByteString) of
        Left err ->
            return $ Left $ show err

        Right Nothing ->
            return $ Left "Nothing"

        Right (Just message) ->
            return $ Right message


tcpListen :: TVar.TVar State -> Tcp.Socket -> IO ()
tcpListen model conn = do
    maybeRawLength <- tcpRecv conn 2
    case maybeRawLength of
        Left err ->
            updateIo model $
            BadTcpRecvM $ "could not get length: " <> err <> "\n"

        Right rawLength ->
            case parseLength rawLength of
            Left err ->
                updateIo model $ BadTcpRecvM err

            Right len ->
                if len > maxMessageLength then
                updateIo model $ BadTcpRecvM "message too long"
                else do
                    maybeMessage <- tcpRecv conn len
                    case maybeMessage of
                        Left err ->
                            updateIo model $ BadTcpRecvM err

                        Right message -> do
                            updateIo model $ FromServerM message
                            tcpListen model conn


serverUrl =
    "localhost"


serverPort =
    "11453"


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
    | BytesInQO Q B.ByteString
    | ReadMessageFromStdInO
    | WriteKeyToFileO B.ByteString


data Msg
    = StartM
    | TcpSendResultM (Either E.IOException ())
    | BadTcpRecvM String
    | StdInM B.ByteString
    | ArgsM [String]
    | SecretKeyFileM (Either E.IOException B.ByteString)
    | BatchM [Maybe Msg]
    | FromServerM B.ByteString
    | NewSecretKeyM Ed.SecretKey
    | NoInternetM E.IOException
    | TcpConnM Tcp.Socket


instance Show Msg where
    show msg =
        case msg of
        StartM ->
            "StartM"

        TcpSendResultM except ->

            "TcpSendResultM (" <> show except <> ")"

        BadTcpRecvM err ->
            "BadTcpRecvM " <> err

        StdInM bytes ->
            "StdInM " <> showBytes bytes

        ArgsM args ->
            "ArgsM " <> show args

        SecretKeyFileM f ->
            "SecretKeyFileM " <> show f

        BatchM _ ->
            "BatchM"

        FromServerM m ->
            "FromServerM " <> showBytes m

        NewSecretKeyM key ->
            "NewSecretKeyM " <> show key

        NoInternetM except ->
            "NoInternetM " <> show except

        TcpConnM conn ->
            "TcpConnM " <> show conn


showBytes :: B.ByteString -> String
showBytes bytes =
    T.unpack $ Text.Hex.encodeHex bytes


data State
    = ReadyS Ready
    | InitS Init
    | FinishedS
    deriving Show


data Init
    = EmptyI
    | GettingKeysFromFileI
    | GeneratingSecretKeyI
    deriving Show


data Ready
    = Ready
        { secretKey :: Ed.SecretKey
        , authStatus :: AuthStatus
        , readingStdIn :: Maybe Ed.PublicKey
        }
        deriving Show


data AuthStatus
    = LoggedInA Tcp.Socket
    | NotLoggedInA NotLoggedIn
    deriving Show


data NotLoggedIn
    = SendWhenLoggedIn (Maybe Tcp.Socket) ToServer
    | JustNotLoggedIn
    deriving Show


type Q
    = Q.TQueue B.ByteString


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


keysPath :: FilePath
keysPath =
    "bigwebthingSECRET"


update :: State -> Msg -> (Output, State)
update model msg =
    let
    pass = (DoNothingO, model)
    in
    case msg of
    BadTcpRecvM _ ->
        let
        errMsg = (toUser NotConnectedU, FinishedS)
        in
        case model of
        FinishedS ->
            (DoNothingO, model)

        ReadyS _ ->
            errMsg

        InitS _ ->
            errMsg

    TcpSendResultM (Right ()) ->
        pass

    TcpSendResultM (Left _) ->
        ( toUser NotConnectedU
        , FinishedS
        )

    TcpConnM socket ->
        updateReady model $ \ready ->
        ( DoNothingO
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
                    SendWhenLoggedIn (Just socket) toServer
            }
        )

    NoInternetM ioErr ->
        ( BatchO
            [ toUser NotConnectedU
            , PrintO $ T.pack $ show ioErr
            ]
        , FinishedS
        )

    StartM ->
        (ReadSecretKeyO, InitS GettingKeysFromFileI)

    StdInM raw ->
        case P.parseOnly inboxMessageP raw of
        Left err ->
            ( toUser $ BadMessageU err
            , FinishedS
            )

        Right newMsg ->
            updateReady model $ \ready ->
            case readingStdIn ready of
            Nothing ->
                pass

            Just publicKey ->
                case authStatus ready of
                LoggedInA _ ->
                    pass

                NotLoggedInA (SendWhenLoggedIn Nothing _) ->
                    pass

                NotLoggedInA (SendWhenLoggedIn (Just _) _) ->
                    pass

                NotLoggedInA JustNotLoggedIn ->
                    ( MakeTcpConnO
                    , ReadyS $
                      ready
                        { authStatus =
                            NotLoggedInA $
                            SendWhenLoggedIn Nothing $
                            SendMessageT publicKey newMsg
                        }
                    )

    NewSecretKeyM key ->
        case model of
        InitS EmptyI ->
            pass

        InitS GettingKeysFromFileI ->
            pass

        InitS GeneratingSecretKeyI ->
            ( BatchO
                [ WriteKeyToFileO $ Ba.convert key
                , GetArgsO
                ]
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

    ArgsM ["help"] ->
        (PrintO usage, FinishedS)

    ArgsM ["myid"] ->
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
                ( PrintO $
                  mconcat
                  [ "internal error:\n"
                  , "could not convert public key to Base64:\n"
                  , T.pack $ show err
                  ]
                , FinishedS
                )

            Right b64 ->
                (PrintO $ b64 <> "\n", FinishedS)

        FinishedS ->
            (DoNothingO, model)

    ArgsM ["get"] ->
        case model of
        ReadyS ready ->
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

        InitS EmptyI ->
            pass

        InitS GettingKeysFromFileI ->
            pass

        InitS GeneratingSecretKeyI ->
            pass

        FinishedS ->
            pass

    ArgsM ["send", rawRecipient] ->
        case parseRecipient rawRecipient of
        Left err ->
            (PrintO $ "invalid recipient: " <> err, model)

        Right recipient ->
            case model of
            ReadyS ready ->
                ( ReadMessageFromStdInO
                , ReadyS $ ready { readingStdIn = Just recipient }
                )

            InitS _ ->
                (DoNothingO, model)

            FinishedS ->
                (DoNothingO, model)

    ArgsM _ ->
        (toUser BadArgsU, FinishedS)

    BatchM msgs ->
        let
        (outputs, newModel) = batchUpdate model msgs []
        in
        (BatchO outputs, newModel)

    SecretKeyFileM (Left ioErr) ->
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
        ( PrintO $
          mconcat
          [ "internal error in reading secret key file:\n"
          , T.pack $ show ioErr
          ]
        , FinishedS
        )

    SecretKeyFileM (Right raw) ->
        case model of
        InitS EmptyI ->
            pass

        InitS GeneratingSecretKeyI ->
            pass

        InitS GettingKeysFromFileI ->
            case P.parseOnly secretSigningP raw of
            Left err ->
                ( PrintO $
                  mconcat
                  [ "internal error: corrupted secret key file: \n"
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

    FromServerM raw ->
        updateReady model $ fromServerUpdate raw


data ToUser
    = NotConnectedU
    | BadArgsU
    | BadMessageU String
    | NoMessagesU
    | NewMessageU Ed.PublicKey T.Text


toUser :: ToUser -> Output
toUser =
    PrintO . prettyMessage


prettyMessage :: ToUser -> T.Text
prettyMessage msg =
    case msg of
    BadMessageU err ->
        "bad message:\n" <> T.pack err <> "\n"

    NotConnectedU ->
        "could not connect to the internet\n"

    BadArgsU ->
        "bad arguments\n\nUsage instructions:\n" <> usage <> "\n"

    NoMessagesU ->
        "no messages\n"

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
    deriving Show


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


fromServerUpdate :: B.ByteString -> Ready -> (Output, State)
fromServerUpdate raw ready =
    case P.parseOnly fromServerP raw of
    Left err ->
        ( PrintO $
          mconcat
          [ "Received bad message from server:\n"
          , Text.Hex.encodeHex raw
          , T.pack err
          ]
        , FinishedS
        )

    Right (InboxMessageF sender message) ->
        ( toUser $ NewMessageU sender message
        , FinishedS
        )

    Right (AuthCodeToSignF (AuthCode authCode)) ->
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

        NotLoggedInA (SendWhenLoggedIn (Just socket) toServer) ->
            ( BatchO
                [ TcpSendO socket $
                    encodeToServer $
                    SignedAuthCodeT publicKey signature
                , TcpSendO socket $ encodeToServer $ toServer
                ]
            , ReadyS $ ready { authStatus = LoggedInA socket }
            )

    Right NoMessagesF ->
        ( BatchO 
            [ toUser NoMessagesU
            , killConn $ authStatus ready
            ]
        , FinishedS
        )


killConn :: AuthStatus -> Output
killConn auth =
    case auth of
    LoggedInA socket ->
        CloseTcpO socket

    NotLoggedInA (SendWhenLoggedIn Nothing _) ->
        DoNothingO

    NotLoggedInA (SendWhenLoggedIn (Just socket) _) ->
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
    _ <- P.word8 1
    raw <- P.scan 0 msgScanner
    case decodeUtf8' raw of
        Left err ->
            fail $ show err

        Right valid ->
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


parseLength :: B.ByteString -> Either String Int
parseLength bytes2 =
    P.parseOnly uint16P bytes2


uint8P :: P.Parser Int
uint8P =
    fromIntegral <$> P.anyWord8

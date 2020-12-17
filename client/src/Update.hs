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
    , Drg(..)
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
import qualified Crypto.Error as Ce
import qualified Data.ByteArray as Ba
import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Data.IntSet as IntSet
import System.IO.Error (isDoesNotExistError)
import Data.Bits ((.&.), shiftR)
import qualified Crypto.MAC.Poly1305 as Mac
import qualified Crypto.Cipher.ChaChaPoly1305 as ChaChaP
import qualified Crypto.Random as Rand


data Output
    = GetArgsO
    | GenerateSecretKeyO
    | GetRandomGenO
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
    | RandomGenM Drg
    | TcpConnErrM (Either E.IOException ())
    | TcpConnM Tcp.Socket
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
    | GettingRandomGenI Ed.SecretKey
    deriving (Show, Eq)


data Ready
    = Ready
        { signingKey :: Ed.SecretKey
        , authStatus :: AuthStatus
        , awaitingEncrypted :: Maybe SecretKey
        , randomGen :: Drg
        , keyAndIdToPrint :: Maybe (SecretKey, MessageId)
        }
        deriving (Show, Eq)


newtype Drg
    = Drg Rand.SystemDRG


instance Show Drg where
    show (Drg _) =
        "SystemDRG"


instance Eq Drg where
    (==) _ _ =
        True


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


encrypt
    :: B.ByteString
    -> Nonce
    -> SecretKey
    -> Ce.CryptoFailable Encrypted
encrypt plaintext (Nonce nonce) (SecretKey key) =
    do
    state1 <- ChaChaP.initialize key nonce
    let
        state2 = ChaChaP.finalizeAAD $ ChaChaP.appendAAD ad state1
        (ciphertext, state3) = ChaChaP.encrypt plaintext state2
        auth = ChaChaP.finalize state3
    return $ Encrypted (Auth auth) (Nonce nonce) ciphertext


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
    \Download a message to STDOUT\n\
    \\n\
    \    $ bwt get <message ID>\n\
    \\n\
    \Send a message from STDIN\n\
    \\n\
    \    $ bwt send\n"


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
    _ <- P.word8 10 -- newline
    P.endOfInput
    return msg


makeMessageId :: Drg -> (MessageId, Drg)
makeMessageId (Drg drg) =
    let
    (raw, gen') = Rand.randomBytesGenerate messageIdLength drg
    in
    (MessageId raw, Drg gen')


makeNonce :: Drg -> Ce.CryptoFailable (Nonce, Drg)
makeNonce (Drg drg) =
    do
    let (raw, gen') = Rand.randomBytesGenerate nonceLength drg
    nonce <- ChaChaP.nonce12 (raw :: B.ByteString)
    return (Nonce nonce, Drg gen')


updateOnStdIn :: State -> B.ByteString -> (Output, State)
updateOnStdIn model raw =
    case P.parseOnly stdInP raw of
    Left err ->
        (toUser $ BadMessageU err, FinishedS)

    Right newMsg ->
        updateReady model $ \ready ->
        let
        (secretKey, gen1) = makeSecretKey $ randomGen ready
        (messageId, gen2) = makeMessageId gen1
        encoded = encodeUtf8 newMsg
        in
        case makeNonce gen2 of
        Ce.CryptoFailed _ ->
            (toUser $ BadEncryptU, FinishedS)

        Ce.CryptoPassed (nonce, gen3) ->
            case encrypt encoded nonce secretKey of
            Ce.CryptoFailed _ ->
                (toUser $ BadEncryptU, FinishedS)

            Ce.CryptoPassed encrypted ->
                ( MakeTcpConnO
                , ReadyS $
                  ready
                    { authStatus =
                        NotLoggedInA $
                        SendWhenLoggedIn Nothing $
                        SendMessageT messageId encrypted
                    , randomGen = gen3
                    , keyAndIdToPrint = Just (secretKey, messageId)
                    }
                )


makeSecretKey :: Drg -> (SecretKey, Drg)
makeSecretKey (Drg gen) =
    let
    (raw, gen') = Rand.randomBytesGenerate secretKeyLength gen
    in
    (SecretKey raw, Drg gen')


updateOnRandomGen :: State -> Drg -> (Output, State)
updateOnRandomGen model drg =
    let
    pass = (DoNothingO, model)
    in
    case model of
    InitS EmptyI ->
        pass

    InitS GettingKeysFromFileI ->
        pass

    InitS GeneratingSecretKeyI ->
        pass

    InitS (GettingRandomGenI signing) ->
        ( GetArgsO
        , ReadyS $
            Ready
            { signingKey = signing
            , authStatus = NotLoggedInA JustNotLoggedIn
            , randomGen = drg
            , awaitingEncrypted = Nothing
            , keyAndIdToPrint = Nothing
            }
        )

    ReadyS _ ->
        pass

    FinishedS ->
        pass


updateOnNewSecretKey :: State -> Ed.SecretKey -> (Output, State)
updateOnNewSecretKey model key =
    let
    pass = (DoNothingO, model)
    in
    case model of
    InitS (GettingRandomGenI _) ->
        pass

    InitS EmptyI ->
        pass

    InitS GettingKeysFromFileI ->
        pass

    InitS GeneratingSecretKeyI ->
        ( BatchO [WriteKeyToFileO $ Ba.convert key, GetRandomGenO]
        , InitS $ GettingRandomGenI key
        )

    ReadyS _ ->
        pass

    FinishedS ->
        pass


parseKeyAndId :: String -> Either T.Text (SecretKey, MessageId)
parseKeyAndId raw =
    case B64.decodeUnpadded $ encodeUtf8 $ T.pack raw of
    Left err ->
        Left $ "could not decode Base64: " <> T.pack err

    Right bs ->
        if B.length bs == (secretKeyLength + messageIdLength) then
        Right
            (SecretKey $ B.take secretKeyLength bs
            , MessageId $ B.drop secretKeyLength bs
            )
        else
        Left $ "bad length: " <> T.pack (show (B.length bs))


secretKeyLength :: Int
secretKeyLength =
    32


updateOnGetArg :: State -> String -> (Output, State)
updateOnGetArg model rawKeyAndId =
    let
    pass = (DoNothingO, model)
    in
    case parseKeyAndId rawKeyAndId of
    Left err ->
        (toUser $ BadKeyAndIdU err, FinishedS)

    Right (key, id_) ->
        updateReady model $ \ready ->
        case authStatus ready of
        LoggedInA _ ->
            pass

        NotLoggedInA JustNotLoggedIn ->
            ( MakeTcpConnO
            , ReadyS $ ready
                { authStatus =
                    NotLoggedInA $
                    SendWhenLoggedIn Nothing (GetMessageT id_)
                , awaitingEncrypted = Just key
                }
            )

        NotLoggedInA (SendWhenLoggedIn (Just _) _) ->
            pass

        NotLoggedInA (SendWhenLoggedIn Nothing _) ->
            pass


updateOnSendArg :: State -> (Output, State)
updateOnSendArg model =
    (ReadMessageFromStdInO, model)


updateOnBadSecretKeyFile :: State -> IOError -> (Output, State)
updateOnBadSecretKeyFile model ioErr =
    let
    pass = (DoNothingO, model)
    in
    if isDoesNotExistError ioErr then
    case model of
    ReadyS _ ->
        pass

    InitS (GettingRandomGenI _) ->
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

    InitS (GettingRandomGenI _) ->
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
            (GetRandomGenO, InitS $ GettingRandomGenI key)

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
    TcpConnErrM (Right ()) ->
        pass

    TcpConnErrM (Left _) ->
        (toUser NotConnectedU, FinishedS)

    RandomGenM gen ->
        updateOnRandomGen model gen

    TcpSendResultM (Right ()) ->
        updateReady model $ \ready ->
        case keyAndIdToPrint ready of
        Nothing ->
            pass

        Just (secretKey, messageId) ->
            ( toUser $ KeyAndIdU secretKey messageId
            , ReadyS $ ready { keyAndIdToPrint = Nothing }
            )

    TcpSendResultM (Left _) ->
        (toUser NotConnectedU, FinishedS)

    TcpConnM socket ->
        updateOnTcpConn model socket

    StartM ->
        (ReadSecretKeyO, InitS GettingKeysFromFileI)

    StdInM raw ->
        updateOnStdIn model raw

    NewSecretKeyM key ->
        updateOnNewSecretKey model key

    ArgsM ["help"] ->
        (toUser UsageU, FinishedS)

    ArgsM ["get", rawKeyAndId] ->
        updateOnGetArg model rawKeyAndId

    ArgsM ["send"] ->
        updateOnSendArg model

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
        ( toUser NotConnectedU
        , FinishedS
        )

    FromServerM (Right Nothing) ->
        (toUser NotConnectedU, FinishedS)

    FromServerM (Right (Just raw)) ->
        updateReady model $ fromServerUpdate raw


data ToUser
    = NotConnectedU
    | KeyAndIdU SecretKey MessageId
    | BadArgsU
    | BadEncryptU
    | BadMessageU String
    | NoMessagesU
    | NewMessageU SecretKey Encrypted
    | BadMessageFromServerU B.ByteString String
    | InternalErrorU T.Text
    | MyIdU T.Text
    | UsageU
    | InvalidRecipientU T.Text
    | NoSuchMessageU MessageId
    | BadKeyAndIdU T.Text


toUser :: ToUser -> Output
toUser message =
    PrintO $ prettyMessage message <> "\n"


-- Just some random bytes used as associated data for the crypto.
ad :: B.ByteString
ad =
    B.pack
    [128, 11, 120, 170, 239, 221, 204, 199, 167, 135, 201, 227, 50, 174, 205, 18, 185, 253, 235, 180, 216, 87, 122, 98, 115, 13, 228, 143, 66, 212, 211, 36]


newtype SecretKey
    = SecretKey B.ByteString
    deriving (Eq, Show)


decrypt :: SecretKey -> Encrypted -> Ce.CryptoFailable B.ByteString
decrypt
    (SecretKey key)
    (Encrypted (Auth expectedAuth) (Nonce nonce) ciphertext) =
    do
    state1 <- ChaChaP.initialize key nonce
    let
        state2 = ChaChaP.finalizeAAD $ ChaChaP.appendAAD ad state1
        (plain, state3) = ChaChaP.decrypt ciphertext state2
        actualAuth = ChaChaP.finalize state3
    if expectedAuth == actualAuth then
        return $ Ba.convert plain
    else
        Ce.CryptoFailed Ce.CryptoError_MacKeyInvalid


prettyMessage :: ToUser -> T.Text
prettyMessage msg =
    case msg of
    BadKeyAndIdU err ->
        "bad lookup code: " <> err

    NoSuchMessageU (MessageId _) ->
        "no such message"

    BadEncryptU ->
        "internal error: could not encrypt message"

    KeyAndIdU (SecretKey secret) (MessageId messageId) ->
        case decodeUtf8' $ B64.encodeUnpadded (secret <> messageId) of
        Left _ ->
            "internal error: could not decode UTF-8"

        Right code ->
            "lookup code:\n" <> code

    InvalidRecipientU err ->
        "invalid recipient:\n" <> err

    BadMessageU err ->
        "bad message:\n" <> T.pack err

    NotConnectedU ->
        "could not connect to the internet"

    BadArgsU ->
        "bad arguments\n\nUsage instructions:\n\n" <> usage

    NoMessagesU ->
        "no messages"

    NewMessageU secretKey encrypted ->
        case decrypt secretKey encrypted of
        Ce.CryptoFailed _ ->
            "could not decrypt message"

        Ce.CryptoPassed plaintext ->
            case decodeUtf8' plaintext of
            Left _ ->
                "corrupted message"

            Right decoded ->
                decoded

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


-- Assocated data
authCodeA :: B.ByteString
authCodeA =
    B.pack [197, 154, 22, 2, 21, 159, 38, 105, 240, 15, 236, 142, 31, 124, 100, 71, 22, 117, 69, 163, 39, 221, 135, 100, 193, 244, 134, 63, 28, 226, 89, 31]


data ToServer
    = SignedAuthCodeT Ed.PublicKey Ed.Signature
    | SendMessageT MessageId Encrypted
    | GetMessageT MessageId
    deriving (Eq, Show)


data Encrypted
    = Encrypted Auth Nonce B.ByteString
    deriving (Eq, Show)


newtype Nonce
    = Nonce ChaChaP.Nonce


instance Show Nonce where
    show (Nonce n) =
        show $ B.unpack $ Ba.convert n
        

instance Eq Nonce where
    (==) (Nonce a) (Nonce b) =
        (Ba.convert a :: B.ByteString) ==
            (Ba.convert b :: B.ByteString)


newtype Auth
    = Auth Mac.Auth
    deriving Eq


instance Show Auth where
    show (Auth mac) =
        show $ B.unpack (Ba.convert mac :: B.ByteString)


maxPlainLength :: Int
maxPlainLength =
    100


authP :: P.Parser Auth
authP =
    do
    raw <- P.take 16
    case Mac.authTag raw of
        Ce.CryptoPassed auth ->
            return $ Auth auth

        Ce.CryptoFailed err ->
            fail $ show err


encryptedP :: P.Parser Encrypted
encryptedP =
    do
    auth <- authP
    nonce <- nonceP
    msg <- P.takeByteString
    if B.length msg > maxPlainLength then
        fail "message is too long"
    else
        return $ Encrypted auth nonce msg


nonceLength :: Int
nonceLength =
    12


nonceP :: P.Parser Nonce
nonceP =
    do
    raw <- P.take nonceLength
    case ChaChaP.nonce12 raw of
        Ce.CryptoPassed nonce ->
            return $ Nonce nonce

        Ce.CryptoFailed err ->
            fail $ show err


newtype MessageId
    = MessageId B.ByteString
    deriving (Eq, Show)


messageIdLength :: Int
messageIdLength =
    24


messageIdP :: P.Parser MessageId
messageIdP =
    MessageId <$> P.take messageIdLength


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

    SendMessageT messageId encrypted ->
        mconcat
        [ B.singleton 1
        , encodeMessageId messageId
        , encodeEncrypted encrypted
        ]

    GetMessageT (MessageId messageId) ->
        B.singleton 2 <> messageId


encodeMessageId :: MessageId -> B.ByteString
encodeMessageId (MessageId id_) =
    id_


encodeEncrypted :: Encrypted -> B.ByteString
encodeEncrypted (Encrypted auth nonce ciphertext) =
    encodeAuth auth <> encodeNonce nonce <> ciphertext


encodeAuth :: Auth -> B.ByteString
encodeAuth (Auth auth) =
    Ba.convert auth


encodeNonce :: Nonce -> B.ByteString
encodeNonce (Nonce n) =
    Ba.convert n


onBodyFromServer :: Ready -> B.ByteString -> (Output, State)
onBodyFromServer ready raw =
    case P.parseOnly fromServerP raw of
    Left err ->
        (toUser $ BadMessageFromServerU raw err, FinishedS)

    Right ok ->
        updateOnGoodFromServer ready ok


onlyLengthP :: P.Parser Int
onlyLengthP = do
    l <- uint16P
    P.endOfInput
    return l


onLengthFromServer
    :: Ready
    -> B.ByteString
    -> (Output, State)
onLengthFromServer ready raw =
    case P.parseOnly onlyLengthP raw of
    Left err ->
        (toUser $ BadMessageFromServerU raw err, FinishedS)

    Right len ->
        case authStatus ready of
        LoggedInA (socket, _) ->
            ( TcpListenO socket len
            , ReadyS $
              ready { authStatus = LoggedInA (socket, GettingBody) }
            )

        NotLoggedInA (SendWhenLoggedIn Nothing _) ->
            (DoNothingO, ReadyS ready)

        NotLoggedInA (SendWhenLoggedIn (Just (socket, _)) to) ->
            ( TcpListenO socket len
            , ReadyS $
                ready
                    { authStatus =
                        NotLoggedInA $
                        SendWhenLoggedIn
                        (Just (socket, GettingBody)) to
                    }
            )

        NotLoggedInA JustNotLoggedIn ->
            (DoNothingO, ReadyS ready)



fromServerUpdate :: B.ByteString -> Ready -> (Output, State)
fromServerUpdate raw ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case authStatus ready of
    LoggedInA (_, GettingBody) ->
        onBodyFromServer ready raw

    LoggedInA (_, GettingLength) ->
        onLengthFromServer ready raw

    NotLoggedInA (SendWhenLoggedIn Nothing _) ->
        pass

    NotLoggedInA (SendWhenLoggedIn (Just (_, GettingBody)) _) ->
        onBodyFromServer ready raw

    NotLoggedInA (SendWhenLoggedIn (Just (_, GettingLength)) _) ->
        onLengthFromServer ready raw

    NotLoggedInA JustNotLoggedIn ->
        pass


updateOnGoodFromServer :: Ready -> FromServer -> (Output, State)
updateOnGoodFromServer ready fromServer =
    case fromServer of
    InboxMessageF _ message ->
        case awaitingEncrypted ready of
        Nothing ->
            (DoNothingO, ReadyS ready)

        Just secret ->
            (toUser $ NewMessageU secret message, FinishedS)

    AuthCodeToSignF authCode ->
        updateOnAuthCode ready authCode

    NoSuchMessageF messageId ->
        (toUser $ NoSuchMessageU messageId, FinishedS)


updateOnAuthCode :: Ready -> AuthCode -> (Output, State)
updateOnAuthCode ready (AuthCode authCode) =
    let
    pass = (DoNothingO, ReadyS ready)
    publicKey = Ed.toPublic $ signingKey ready
    toSign = authCodeA <> authCode
    signature = Ed.sign (signingKey ready) publicKey toSign
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

                GetMessageT _ ->
                    TcpListenO socket 2

                SendMessageT _ _ ->
                    DoNothingO
            ]
        , case toServer of
            SignedAuthCodeT _ _ ->
                ReadyS $
                ready
                    { authStatus =
                        LoggedInA (socket, GettingLength)
                    }

            GetMessageT _ ->
                ReadyS $
                ready
                    { authStatus =
                        LoggedInA (socket, GettingLength)
                    }

            SendMessageT _ _ ->
                FinishedS
        )


fromServerP :: P.Parser FromServer
fromServerP = do
    msg <- P.choice
        [ do
            _ <- P.word8 0
            AuthCodeToSignF <$> authCodeP
        , do
            _ <- P.word8 1
            messageId <- messageIdP
            encrypted <- encryptedP
            return $ InboxMessageF messageId encrypted
        , do
            _ <- P.word8 2
            NoSuchMessageF <$> messageIdP
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


data FromServer
    = AuthCodeToSignF AuthCode
    | InboxMessageF MessageId Encrypted
    | NoSuchMessageF MessageId


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
    key <- case Ed.secretKey raw of
            Ce.CryptoFailed err ->
                fail $ show err

            Ce.CryptoPassed key ->
                return key
    P.endOfInput
    return key


uint16P :: P.Parser Int
uint16P = do
    b0 <- uint8P
    b1 <- uint8P
    return $ b0 + b1 * 256


uint8P :: P.Parser Int
uint8P =
    fromIntegral <$> P.anyWord8

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import System.FilePath ((</>))
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
import Control.Concurrent (forkIO, threadDelay)
import qualified Text.Hex
import qualified Network.Simple.TCP as Tcp
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Data.ByteArray as Ba
import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Data.IntSet as IntSet
import System.IO.Error (isDoesNotExistError)
import qualified System.Directory as Dir


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


msgQ :: Stm.STM (Q.TQueue Msg)
msgQ =
    Q.newTQueue


io :: TVar.TVar State -> Output -> IO ()
io mainState output =
    case output of
    GetHomeDirO -> do
        homeDir <- Dir.getHomeDirectory
        updateIo mainState $ HomeDirM homeDir

    WriteKeyToFileO path key -> do
        B.writeFile path key

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

    ReadSecretKeyO path -> do
        raw <- E.try $ B.readFile path
        updateIo mainState $ SecretKeyFileM raw

    DoNothingO ->
        return ()

    BatchO outputs -> do
        mapM_ (io mainState) outputs

    BytesInQO q bytes ->
        Stm.atomically $ do
            q_ <- q
            Q.writeTQueue q_ bytes

    StartTcpClientO ->
        tcpClient mainState


maxMessageLength =
    16000


tcpClient :: TVar.TVar State -> IO ()
tcpClient model = do
    res <- E.try $ Tcp.connect serverUrl serverPort $ \(conn, _) -> do
        _ <- forkIO $ tcpListen model conn
        tcpSend conn
    case res of
        Left err ->
            updateIo model $ NoInternetM err

        Right () ->
            return ()


tcpSend :: Tcp.Socket -> IO ()
tcpSend conn = do
    msg <- Stm.atomically $ do
       q <- toServerQ
       Q.readTQueue q
    Tcp.send conn msg
    tcpSend conn


restartTcp :: TVar.TVar State -> IO ()
restartTcp model = do
    Stm.atomically $ do
        q <- msgQ
        Stm.writeTQueue q RestartingTcpM
    threadDelay tcpDelay
    tcpClient model


tcpListen :: TVar.TVar State -> Tcp.Socket -> IO ()
tcpListen model conn = do
    maybeRawLength <- Tcp.recv conn 2
    case maybeRawLength of
        Nothing ->
            restartTcp model

        Just rawLength ->
            case parseLength rawLength of
            Left err ->
                error err

            Right len ->
                if len > maxMessageLength then
                error $
                    "TCP too long: " ++ show len
                else do
                    maybeMessage <- Tcp.recv conn len
                    case maybeMessage of
                        Nothing ->
                            restartTcp model

                        Just message -> do
                            Stm.atomically $ do
                                q <- msgQ
                                Q.writeTQueue q $ FromServerM $ message
                            tcpListen model conn


tcpDelay =
    20 * 1000000


serverUrl =
    "localhost"


serverPort =
    "11453"


data Output
    = GetArgsO
    | GenerateSecretKeyO
    | ReadSecretKeyO FilePath
    | PrintO T.Text
    | DoNothingO
    | BatchO [Output]
    | StartTcpClientO
    | BytesInQO Q B.ByteString
    | ReadMessageFromStdInO
    | WriteKeyToFileO FilePath B.ByteString
    | GetHomeDirO


data Msg
    = StartM
    | StdInM B.ByteString
    | ArgsM [String]
    | SecretKeyFileM (Either E.IOException B.ByteString)
    | BatchM [Maybe Msg]
    | FromServerM B.ByteString
    | RestartingTcpM
    | NewSecretKeyM Ed.SecretKey
    | HomeDirM FilePath
    | NoInternetM E.IOException
    deriving (Show)


data State
    = ReadyS Ready
    | InitS Init
    | FinishedS
    deriving (Show)


data Init
    = EmptyI
    | GettingHomeDirI
    | GettingKeysFromFileI FilePath
    | GeneratingSecretKeyI FilePath
    deriving Show


data Ready
    = Ready
        { secretKey :: Ed.SecretKey
        , authStatus :: AuthStatus
        , readingStdIn :: Maybe Ed.PublicKey
        }
        deriving Show


data AuthStatus
    = LoggedInA
    | NotLoggedInA NotLoggedIn
    deriving Show


data NotLoggedIn
    = SendWhenLoggedIn Ed.PublicKey T.Text
    | GetWhenLoggedIn
    | JustNotLoggedIn
    deriving Show


type Q
    = Stm.STM (Q.TQueue B.ByteString)


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


keysPath :: FilePath -> FilePath
keysPath homeDir =
    homeDir </> ".bigwebthingSECRET"


update :: State -> Msg -> (Output, State)
update model msg =
    let
    pass = (DoNothingO, model)
    in
    case msg of
    NoInternetM ioErr ->
        ( BatchO
            [ toUser NotConnectedU
            , PrintO $ T.pack $ show ioErr
            ]
        , FinishedS
        )

    StartM ->
        (GetHomeDirO, InitS GettingHomeDirI)

    HomeDirM homeDir ->
        case model of
        InitS GettingHomeDirI ->
            ( ReadSecretKeyO $ keysPath homeDir
            , InitS $ GettingKeysFromFileI homeDir
            )

        InitS EmptyI ->
            pass

        InitS (GettingKeysFromFileI _) ->
            pass

        InitS (GeneratingSecretKeyI _) ->
            pass

        ReadyS _ ->
            pass

        FinishedS ->
            pass

    StdInM raw ->
        case P.eitherResult $ P.parse inboxMessageP raw of
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
                ( sendToServer $ SendMessageT publicKey newMsg
                , ReadyS ready
                )

    NewSecretKeyM key ->
        case model of
        InitS GettingHomeDirI ->
            pass

        InitS EmptyI ->
            pass

        InitS (GettingKeysFromFileI _) ->
            pass

        InitS (GeneratingSecretKeyI homeDir) ->
            ( BatchO
                [ WriteKeyToFileO (keysPath homeDir) $ Ba.convert key
                , GetArgsO
                ]
            , ReadyS $ Ready
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
            LoggedInA ->
                pass

            NotLoggedInA (SendWhenLoggedIn _ _) ->
                pass

            NotLoggedInA JustNotLoggedIn ->
                ( StartTcpClientO
                , ReadyS $ ready
                    { authStatus = NotLoggedInA GetWhenLoggedIn }
                )

            NotLoggedInA GetWhenLoggedIn ->
                pass

        InitS GettingHomeDirI ->
            pass

        InitS EmptyI ->
            pass

        InitS (GettingKeysFromFileI _) ->
            pass

        InitS (GeneratingSecretKeyI _) ->
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

        InitS (GettingKeysFromFileI homeDir) ->
            (GenerateSecretKeyO, InitS (GeneratingSecretKeyI homeDir))

        InitS EmptyI ->
            pass

        InitS GettingHomeDirI ->
            pass

        InitS (GeneratingSecretKeyI _) ->
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

        InitS (GeneratingSecretKeyI _) ->
            pass

        InitS GettingHomeDirI ->
            pass

        InitS (GettingKeysFromFileI _) ->
            case P.eitherResult $ P.parse secretSigningP raw of
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
                , ReadyS $ Ready
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

    RestartingTcpM ->
        updateReady model restartingTcpUpdate


data ToUser
    = NotConnectedU
    | BadArgsU
    | BadMessageU String


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


sendToServer :: ToServer -> Output
sendToServer msg =
    BytesInQO toServerQ $ encodeToServer msg


data ToServer
    = SignedAuthCodeT Ed.PublicKey Ed.Signature
    | SendMessageT Ed.PublicKey T.Text
    | GetMessageT


encodeToServer :: ToServer -> B.ByteString
encodeToServer toServer =
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


restartingTcpUpdate :: Ready -> (Output, State)
restartingTcpUpdate ready =
    ( DoNothingO
    , ReadyS $
      case authStatus ready of
        LoggedInA ->
            ready { authStatus = NotLoggedInA JustNotLoggedIn }

        NotLoggedInA _ ->
            ready
    )


fromServerUpdate :: B.ByteString -> Ready -> (Output, State)
fromServerUpdate raw ready =
    case P.eitherResult $ P.parse fromServerP raw of
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
        case decodeUtf8' $ B64.encodeUnpadded $ Ba.convert sender of
        Left err ->
            ( PrintO $
              mconcat
              [ "internal error: could not decode Base64 ByteString:\n"
              , T.pack $ show err
              , ":\n"
              , Text.Hex.encodeHex $ Ba.convert sender
              ]
            , FinishedS
            )

        Right b64 ->
            (PrintO $ b64 <> ": " <> message, FinishedS)

    Right (AuthCodeToSignF (AuthCode authCode)) ->
            let
            publicKey = Ed.toPublic $ secretKey ready
            toSign = authCodeA <> authCode
            signature = Ed.sign (secretKey ready) publicKey toSign
            in
            ( sendToServer $ SignedAuthCodeT publicKey signature
            , ReadyS ready
            )


toServerQ :: Q
toServerQ =
    Q.newTQueue


fromServerP :: P.Parser FromServer
fromServerP = do
    msg <- P.choice
        [ do
            _ <- P.word8 0
            authCode <- authCodeP
            return $ AuthCodeToSignF authCode
        , do
            _ <- P.word8 5
            sender <- publicKeyP
            inboxMessage <- inboxMessageP
            return $ InboxMessageF sender inboxMessage
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
    if counter < 100 then
    if validByte byte then
    Just $ counter + 1
    else
    Nothing
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
    P.eitherResult $ P.parse uint16P bytes2


uint8P :: P.Parser Int
uint8P =
    fromIntegral <$> P.anyWord8

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Control.Concurrent.STM as Stm
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Handler.Warp (run)
import qualified Control.Concurrent.STM.TQueue as Q
import Crypto.Noise.DH.Curve25519 (Curve25519)
import Crypto.Noise.Hash.BLAKE2s (BLAKE2s)
import Crypto.Noise.Cipher.ChaChaPoly1305 (ChaChaPoly1305)
import Crypto.Noise.HandshakePatterns (noiseXX)
import qualified Crypto.Noise.DH as Dh
import qualified Crypto.Noise as Noise
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified System.Directory as Dir
import qualified Data.Text as T
import System.FilePath ((</>))
import qualified Data.Attoparsec.ByteString.Lazy as P
import qualified Graphics.UI.Webviewhs as Wv
import qualified Web.Scotty as Sc
import qualified Network.Wai as Wai
import qualified Network.WebSockets as Ws
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash.BLAKE2.BLAKE2b as Blake
import qualified System.IO as Io
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as Bc
import Data.Word (Word8)
import Control.Concurrent (forkIO, threadDelay)
import qualified Data.Set as Set
import qualified Network.Simple.TCP as Tcp
import qualified Data.Map as Map
import Control.Exception.Base (SomeException)
import qualified Data.Time.Clock as Clock
import Data.Text.Encoding (encodeUtf8)
import System.Entropy (getEntropy)
import qualified Data.Bits as Bits
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Crypto.KDF.Argon2 as Argon2


main :: IO ()
main =
    mainHelp (InitS EmptyI) StartM


msgQ :: Stm.STM (Q.TQueue Msg)
msgQ =
    Q.newTQueue


mainHelp :: State -> Msg -> IO ()
mainHelp oldState oldMsg = do
    let (output, newState) = update oldState oldMsg
    maybeMsg <- io output
    newMsg <- case maybeMsg of
        Nothing ->
            Stm.atomically $ do
                q <- msgQ
                Q.readTQueue q
        Just m ->
            return m
    mainHelp newState newMsg


io :: Output -> IO (Maybe Msg)
io output =
    case output of
        DoNothingO ->
            return Nothing

        GetAppDataDirO -> do
            dir <- Dir.getXdgDirectory Dir.XdgData ""
            return $ Just $ AppDataDirM dir

        MakeDirIfMissingO path -> do
            Dir.createDirectoryIfMissing False path
            return $ Just $ DirCreatedIfMissingM path

        ReadFileO path -> do
            bytes <- Bl.readFile path
            return $ Just $ FileContentsM path bytes

        BatchO outputs -> do
            inputs <- mapM io outputs
            return $ Just $ BatchM inputs

        StartUiO -> do
            Wv.createWindowAndBlock
                Wv.WindowParams
                    { Wv.windowParamsTitle = "BigWebThing"
                    , Wv.windowParamsUri = clientUrl <> "/static"
                    , Wv.windowParamsWidth = 800
                    , Wv.windowParamsHeight = 600
                    , Wv.windowParamsResizable = True
                    , Wv.windowParamsDebuggable = True
                    }
            return Nothing

        StartUiServerO -> do
            app <- httpApi
            run clientPort $ websocketsOr
                Ws.defaultConnectionOptions websocket app
            return Nothing

        ErrorO err ->
            error err

        GetTmpFileHandleO path -> do
            (filePath, handle) <-
                Io.openBinaryTempFileWithDefaultPermissions path ""
            return $ Just $ TmpFileHandleM filePath handle

        WriteToHandleO handle bytes path -> do
            Bl.hPut handle bytes
            return $ Just $ WrittenToHandleM path handle

        MoveFileO oldPath newPath -> do
            Dir.renameFile oldPath newPath
            return $ Just $ MovedFileM newPath

        BytesInQO q bytes -> do
            Stm.atomically $ do
                q_ <- q
                Q.writeTQueue q_ bytes
            return Nothing

        CloseFileO handle -> do
            Io.hClose handle
            return Nothing

        ReadFileLazyO path -> do
            bytes <- Bl.readFile path
            return $ Just $ LazyFileContentsM path bytes

        WriteFileO path contents -> do
            Bl.writeFile path contents
            return Nothing

        StartTcpClientO -> do
            tcpClient
            return Nothing

        DoesFileExistO path -> do
            exists <- Dir.doesFileExist path
            return $ Just $ FileExistenceM path exists

        MakeDhKeysO -> do
            keys <- mapM (\_ -> Dh.dhGenKey) ([1..] :: [Integer])
            return $ Just $ NewDhKeysM keys

        GetTimesO -> do
            times <-
                mapM
                    (\_ -> Clock.getCurrentTime)
                    ([1..] :: [Integer])
            return $ Just $ TimesM times

        AppendFileO path toAppend -> do
            _ <- Bl.appendFile path toAppend
            return Nothing

        MakeSessionKeyO -> do
            key <- getEntropy sessionKeyLength
            return $ Just $ NewSessionKeyM key


maxMessageLength =
    16000


tcpClient =
    Tcp.connect serverUrl serverPort $ \(conn, _) -> do
        _ <- forkIO $ tcpListen conn
        tcpSend conn


tcpSend :: Tcp.Socket -> IO ()
tcpSend conn = do
    msg <- Stm.atomically $ do
       q <- toServerQ
       Q.readTQueue q
    Tcp.sendLazy conn msg
    tcpSend conn


restartTcp :: IO ()
restartTcp = do
    Stm.atomically $ do
        q <- msgQ
        Stm.writeTQueue q RestartingTcpM
    threadDelay tcpDelay
    tcpClient


tcpListen :: Tcp.Socket -> IO ()
tcpListen conn = do
    maybeRawLength <- Tcp.recv conn 2
    case maybeRawLength of
        Nothing -> do
            restartTcp

        Just rawLength ->
            case parseLength $ Bl.fromStrict rawLength of
                Left err ->
                    error err

                Right len ->
                    if len > maxMessageLength then
                        error $
                            "TCP too long: " ++ show len
                    else do
                        maybeMessage <- Tcp.recv conn len
                        case maybeMessage of
                            Nothing -> do
                                restartTcp

                            Just message -> do
                                Stm.atomically $ do
                                    q <- msgQ
                                    Q.writeTQueue q $ FromServerM $
                                        Bl.fromStrict message
                                tcpListen conn



tcpDelay =
    20 * 1000000


serverUrl =
    "http://localhost"


serverPort =
    "11453"


instance Show MessageId where
    show (MessageId m) = show m


httpApi :: IO Wai.Application
httpApi =
    Sc.scottyApp $ do

        Sc.get "/static/:requested" $ do
            requested <- Sc.param "requested"
            Sc.file $ "static/" ++ requested

        Sc.post "/setblob" $
            httpPost SetBlobM

        Sc.post "/getBlob" $
            httpPost GetBlobM


httpPost
    :: (Bl.ByteString -> Q -> Msg)
    -> Sc.ActionM ()
httpPost msg =
    let
        responseQ :: Q
        responseQ =
            Q.newTQueue
    in do
        body <- Sc.body
        liftIO $ Stm.atomically $ do
            q <- msgQ
            Q.writeTQueue q $ msg body Q.newTQueue
        response <- liftIO $ Stm.atomically $ do
            q <- responseQ
            Q.readTQueue q
        Sc.raw response


clientPort :: Int
clientPort =
    11833


clientUrl :: T.Text
clientUrl =
    "http://localhost:" <> T.pack (show clientPort)


data Output
    = GetAppDataDirO
    | MakeDirIfMissingO FilePath
    | MakeSessionKeyO
    | DoNothingO
    | ReadFileO FilePath
    | BatchO [Output]
    | StartUiO
    | StartUiServerO
    | StartTcpClientO
    | ErrorO String
    | GetTmpFileHandleO FilePath
    | WriteToHandleO Io.Handle Bl.ByteString FilePath
    | MoveFileO FilePath FilePath
    | BytesInQO Q Bl.ByteString
    | CloseFileO Io.Handle
    | ReadFileLazyO FilePath
    | WriteFileO FilePath Bl.ByteString
    | DoesFileExistO FilePath
    | MakeDhKeysO
    | AppendFileO FilePath Bl.ByteString
    | GetTimesO


data Msg
    = StartM
    | AppDataDirM FilePath
    | DirCreatedIfMissingM FilePath
    | FileContentsM FilePath Bl.ByteString
    | BatchM [Maybe Msg]
    | SetBlobM Bl.ByteString Q
    | TmpFileHandleM FilePath Io.Handle
    | WrittenToHandleM FilePath Io.Handle
    | MovedFileM FilePath
    | GetBlobM Bl.ByteString Q
    | FromWebsocketM Ws.DataMessage
    | LazyFileContentsM FilePath Bl.ByteString
    | FromServerM Bl.ByteString
    | RestartingTcpM
    | FileExistenceM FilePath Bool
    | NewDhKeysM [Dh.KeyPair Curve25519]
    | TimesM [Clock.UTCTime]
    | NewSessionKeyM B.ByteString


websocket :: Ws.ServerApp
websocket pending = do
    conn <- Ws.acceptRequest pending
    _ <- forkIO $ websocketSend conn
    websocketReceive conn


websocketSend :: Ws.Connection -> IO ()
websocketSend conn = do
    out <- Stm.atomically $ do
        q <- toFrontendQ
        Q.readTQueue q
    Ws.sendDataMessage conn $ Ws.Binary out
    websocketSend conn


websocketReceive :: Ws.Connection -> IO ()
websocketReceive conn = do
    msg <- Ws.receiveDataMessage conn
    Stm.atomically $ do
        q <- msgQ
        Q.writeTQueue q (FromWebsocketM msg)
    websocketReceive conn


toFrontendQ :: Q
toFrontendQ =
    Q.newTQueue


newtype SessionKey
    = SessionKey Bl.ByteString


newtype RootPath
    = RootPath FilePath


data StaticKeys
    = StaticKeys
        { staticDh :: MyStatic
        , sessionKey :: SessionKey
        , userId :: UserId
        }


newtype Hash32
    = Hash32 B.ByteString


data BlobUpWait
    = BlobUpWait Bl.ByteString Q


data BlobUp
    = AwaitingHandleB
        Q
        Bl.ByteString
        Hash32
    | AwaitingTmpWriteB
        Q
        Hash32
        FilePath
    | AwaitingMoveB
        Q
        Hash32
        FilePath


promoteBlobsUp :: Jobs BlobUp BlobUpWait -> Jobs BlobUp BlobUpWait
promoteBlobsUp jobs =
    case jobs of
        NoJobs ->
            NoJobs

        Jobs _ [] ->
            NoJobs

        Jobs _ (BlobUpWait body q : aiting) ->
            Jobs (AwaitingHandleB q body (hash32 body)) aiting


data State
    = ReadyS Ready
    | InitS Init
    | FailedS


data Init
    = EmptyI
    | MakingRootDirI RootPath
    | MakingDhKeysI RootPath
    | GettingTimes RootPath [Dh.KeyPair Curve25519]
    | DoKeysExistI RootPath [Dh.KeyPair Curve25519] [Clock.UTCTime]
    | GettingKeysFromFileI
        RootPath [Dh.KeyPair Curve25519] [Clock.UTCTime]


data Ready = Ready
    { root :: RootPath
    , blobsUp :: Jobs BlobUp BlobUpWait
    , getBlob :: Jobs GetBlob GetBlobWait
    , messageLocks :: Set.Set MessageId
    , authStatus :: AuthStatus
    , handshakes :: Map.Map HandshakeId Handshake
    , newDhKeys :: [Dh.KeyPair Curve25519]
    , theTime :: [Clock.UTCTime]
    , whitelist :: Map.Map Username Fingerprint
    }


data HandshakeId
    = HandshakeId Username PublicEphemeral


instance Ord HandshakeId where
    compare (HandshakeId u1 p1) (HandshakeId u2 p2) =
        case (compare u1 u2, compare p1 p2) of
            (LT, LT) ->
                LT

            (LT, GT) ->
                LT

            (GT, LT) ->
                GT

            (GT, GT) ->
                GT

            (LT, EQ) ->
                LT

            (GT, EQ) ->
                GT

            (EQ, LT) ->
                LT

            (EQ, GT) ->
                GT

            (EQ, EQ) ->
                EQ


instance Eq PublicEphemeral where
    (==) (PublicEphemeral p1) (PublicEphemeral p2) =
        p1 == p2


instance Eq HandshakeId where
    (==) (HandshakeId u1 p1) (HandshakeId u2 p2) =
        u1 == u2 && p1 == p2


newtype PublicEphemeral
    = PublicEphemeral Bl.ByteString


instance Ord PublicEphemeral where
    compare (PublicEphemeral a) (PublicEphemeral b) =
        compare a b


data Handshake
    = Initiator InitiatorHandshake
    | ResponderSentEncryptedE MyEphemeral


data InitiatorHandshake
    = SentPlainE
    | ReceivedEncryptedE EncryptedEphemeral


newtype MyEphemeral
    = MyEphemeral (Dh.KeyPair Curve25519)


newtype EncryptedEphemeral
    = EncryptedEphemeral Bl.ByteString


data AuthStatus
    = GettingPowInfoA
    | GeneratingSessionKey Word8 Bl.ByteString
    | AwaitingUsername MyStatic SessionKey
    | LoggedIn StaticKeys


data Jobs a b
    = Jobs a [b]
    | NoJobs


data GetBlobWait
    = GetBlobWait Bl.ByteString Q


data GetBlob
    = GetBlob Q Hash32


type Q
    = Stm.STM (Q.TQueue Bl.ByteString)


keysPath :: RootPath -> FilePath
keysPath (RootPath root) =
    root </> "staticKeys"


makeRootPath :: FilePath -> FilePath
makeRootPath appDataDir =
    appDataDir </> "bigwebthing"


updateOnLazyFileContents
    :: FilePath
    -> Bl.ByteString
    -> Ready
    -> (Output, State)
updateOnLazyFileContents path bytes ready =
    case getBlob ready of
        NoJobs ->
            (DoNothingO, ReadyS ready)

        jobs@(Jobs (GetBlob q hash) _) ->
            if path == makeBlobPath (root ready) hash then
                ( BytesInQO q bytes
                , ReadyS $ ready { getBlob = promoteGetBlob jobs }
                )

            else
                (DoNothingO, ReadyS ready)


promoteGetBlob
    :: Jobs GetBlob GetBlobWait
    -> Jobs GetBlob GetBlobWait
promoteGetBlob jobs =
    case jobs of
        NoJobs ->
            NoJobs

        Jobs _ [] ->
            NoJobs

        Jobs _ (GetBlobWait body q : aiting) ->
            Jobs (GetBlob q (hash32 body)) aiting


updateReady :: State -> (Ready -> (Output, State)) -> (Output, State)
updateReady model f =
    case model of
        InitS _ ->
            (DoNothingO, model)

        FailedS ->
            (DoNothingO, model)

        ReadyS ready ->
            f ready


updateInit :: State -> (Init -> (Output, State)) -> (Output, State)
updateInit model f =
    case model of
        InitS init_ ->
            f init_

        FailedS ->
            (DoNothingO, model)

        ReadyS _ ->
            (DoNothingO, model)


update :: State -> Msg -> (Output, State)
update model msg =
    case msg of
        StartM ->
            ( BatchO [GetAppDataDirO, StartTcpClientO]
            , InitS EmptyI
            )

        LazyFileContentsM path bytes ->
            updateReady model $ updateOnLazyFileContents path bytes

        AppDataDirM path ->
            ( MakeDirIfMissingO $ makeRootPath path
            , InitS $ MakingRootDirI $ RootPath $ makeRootPath path
            )

        DirCreatedIfMissingM path ->
            updateInit model $ dirCreatedUpdate path

        FileContentsM path contents ->
            updateInit model $ fileContentsUpdate path contents

        BatchM msgs ->
            let
                (outputs, newModel) = batchUpdate model msgs []
            in
                (BatchO outputs, newModel)

        SetBlobM blob responseQ ->
            updateReady model $ setBlobUpdate blob responseQ

        TmpFileHandleM path handle ->
            updateReady model $ onTmpFileHandleUpdate path handle

        WrittenToHandleM path handle ->
            updateReady model $ onWrittenToTmp path handle

        MovedFileM new ->
            updateReady model $ onMovedFile new

        FromWebsocketM raw ->
            case raw of
                Ws.Text _ _ ->
                    ( ErrorO "received text message from websocket"
                    , FailedS
                    )

                Ws.Binary bin ->
                    case parseFromFrontend bin of
                        Left err ->
                            ( ErrorO $
                                "couldn't parse API input: " <> err
                            , FailedS
                            )

                        Right apiInput ->
                            uiApiUpdate apiInput model

        GetBlobM body q ->
            updateReady model $ onGetBlobUpdate body q

        FromServerM raw ->
            updateReady model $ fromServerUpdate raw

        RestartingTcpM ->
            updateReady model restartingTcpUpdate

        FileExistenceM path exists ->
            updateInit model $ fileExistenceUpdate path exists

        NewDhKeysM keys ->
            updateInit model $ newDhKeysUpdate keys

        TimesM times ->
            updateInit model $ updateOnTimes times

        NewSessionKeyM rawKey ->
            updateReady model $ updateOnNewSessionKey rawKey


updateOnNewSessionKey :: B.ByteString -> Ready -> (Output, State)
updateOnNewSessionKey raw ready =
    case authStatus ready of
        GettingPowInfoA ->
            pass

        GeneratingSessionKey difficulty unique ->
            signUpHelp difficulty unique raw ready

        AwaitingUsername _ _ ->
            pass

        LoggedIn _ ->
            pass

  where
    pass = (DoNothingO, ReadyS ready)


signUpHelp
    :: Word8
    -> Bl.ByteString
    -> B.ByteString
    -> Ready
    -> (Output, State)
signUpHelp difficulty unique raw ready =
    case newDhKeys ready of
        [] ->
            (ErrorO "no DH keys", FailedS)

        dh : keys ->
            let
                myStatic = MyStatic dh
                session = SessionKey $ Bl.fromStrict raw
                newReady = ready
                    { authStatus =
                        AwaitingUsername myStatic session
                    , newDhKeys = keys
                    }
                eitherSignUp =
                    makeSignUp difficulty unique session myStatic
            in case eitherSignUp of
                Left err ->
                    ( ErrorO $ "could not sign up: " <> err
                    , FailedS
                    )

                Right signUp ->
                    ( BatchO
                        [ BytesInQO toServerQ signUp
                        , dumpCache newReady
                        ]
                    , ReadyS newReady
                    )


makeSignUp
    :: Word8
    -> Bl.ByteString
    -> SessionKey
    -> MyStatic
    -> Either String Bl.ByteString
makeSignUp
    difficulty
    unique
    (SessionKey sessionKey)
    (MyStatic (_, public)) = do

    pow <- makePow difficulty unique
    return $ mconcat
        [ Bl.singleton 2
        , pow
        , sessionKey
        , Bl.fromStrict $ Noise.convert $ Dh.dhPubToBytes public
        ]


makePow :: Word8 -> Bl.ByteString -> Either String Bl.ByteString
makePow difficulty unique =
    powHelp difficulty unique 0


argonOptions :: Argon2.Options
argonOptions =
    Argon2.Options
        { iterations = 1
        , memory = 64 * 1024
        , parallelism = 4
        , variant = Argon2.Argon2id
        , version = Argon2.Version13
        }


powHelp
    :: Word8
    -> Bl.ByteString
    -> Integer
    -> Either String Bl.ByteString
powHelp difficulty unique counter =
    let
        candidate = encodeUint64 counter
        hashE = Argon2.hash
            argonOptions
            (Bl.toStrict $ unique <> candidate)
            (B.singleton 0)
            powHashLength
    in case hashE of
        CryptoPassed hash ->
            if isDifficult hash difficulty then
                Right candidate

            else
                powHelp difficulty unique (counter + 1)

        CryptoFailed err ->
            Left $ show err


encodeUint64 :: Integer -> Bl.ByteString
encodeUint64 i =
    Bl.pack $ map (encodeUint64Help i) (take 8 [0..])


encodeUint64Help :: Integer -> Int -> Word8
encodeUint64Help int counter =
    fromIntegral $ (int `Bits.shiftR` (counter * 8)) Bits..&. 0xFF


encodeUint32 :: Int -> Bl.ByteString
encodeUint32 i =
    Bl.pack $ map (encodeUint32Help i) (take 4 [0..])


encodeUint32Help :: Int -> Int -> Word8
encodeUint32Help int counter =
    fromIntegral $ int `Bits.shiftR` (counter * 8) Bits..&. 0xFF


powHashLength =
    32


isDifficult :: B.ByteString -> Word8 -> Bool
isDifficult bytes difficulty =
    B.all (> difficulty) bytes


updateOnTimes :: [Clock.UTCTime] -> Init -> (Output, State)
updateOnTimes times init_ =
    case init_ of
        EmptyI ->
            pass

        MakingRootDirI _ ->
            pass

        MakingDhKeysI _ ->
            pass

        GettingTimes root dhKeys ->
            ( DoesFileExistO $ keysPath root
            , InitS $ DoKeysExistI root dhKeys times
            )

        DoKeysExistI _ _ _ ->
            pass

        GettingKeysFromFileI _ _ _ ->
            pass

  where
    pass = (DoNothingO, InitS init_)


newDhKeysUpdate :: [Dh.KeyPair Curve25519] -> Init -> (Output, State)
newDhKeysUpdate newKeys init_ =
    case init_ of
        EmptyI ->
            pass

        MakingRootDirI _ ->
            pass

        MakingDhKeysI root ->
            (GetTimesO, InitS $ GettingTimes root newKeys)

        GettingTimes _ _ ->
            pass

        DoKeysExistI _ _ _ ->
            pass

        GettingKeysFromFileI _ _ _ ->
            pass

  where
    pass = (DoNothingO, InitS init_)


newtype MyStatic
    = MyStatic (Dh.KeyPair Curve25519)


fileExistenceUpdate :: FilePath -> Bool -> Init -> (Output, State)
fileExistenceUpdate path exists init_ =
    case init_ of
        EmptyI ->
            pass

        MakingRootDirI _ ->
            pass

        MakingDhKeysI _ ->
            pass

        GettingTimes _ _ ->
            pass

        DoKeysExistI root newKeys times ->
            case (exists, path == keysPath root) of
                (True, True) ->
                    ( ReadFileO $ keysPath root
                    , InitS $ GettingKeysFromFileI root newKeys times
                    )

                (True, False) ->
                    pass

                (False, True) ->
                    ( BytesInQO toServerQ $ Bl.singleton 0
                    , ReadyS $ Ready
                        { root
                        , blobsUp = NoJobs
                        , getBlob = NoJobs
                        , messageLocks = Set.empty
                        , authStatus = GettingPowInfoA
                        , handshakes = Map.empty
                        , newDhKeys = newKeys
                        , theTime = times
                        , whitelist = Set.empty
                        }
                    )

                (False, False) ->
                    pass

        GettingKeysFromFileI _ _ _ ->
            pass

  where
    pass = (DoNothingO, InitS init_)


restartingTcpUpdate :: Ready -> (Output, State)
restartingTcpUpdate ready =
    let
        startAuth =
            ( BytesInQO toServerQ $ Bl.singleton 0
            , ReadyS $ ready { authStatus = GettingPowInfoA }
            )
    in case authStatus ready of
        GettingPowInfoA ->
            startAuth

        GeneratingSessionKey _ _ ->
            startAuth

        AwaitingUsername _ _ ->
            startAuth

        LoggedIn (StaticKeys _ sessionKey userId) ->
            ( BytesInQO toServerQ $ logIn sessionKey userId
            , ReadyS ready
            )


logIn :: SessionKey -> UserId -> Bl.ByteString
logIn (SessionKey sessionKey) (UserId (Username username) _) =
    mconcat
        [ Bl.singleton 1
        , sessionKey
        , username
        ]


fromServerUpdate :: Bl.ByteString -> Ready -> (Output, State)
fromServerUpdate raw ready =
    case P.eitherResult $ P.parse fromServerP raw of
        Left err ->
            logErr ready $ T.concat
                [ "could not parse server message: "
                , T.pack err
                ]

        Right (NewMessageT username message) ->
            messageInUpdate username message ready

        Right (NewUsername userId) ->
            newUserIdUpdate userId ready

        Right (PowInfoT difficulty unique) ->
            newPowInfoUpdate difficulty unique ready

        Right (Price price) ->
            ( BytesInQO toFrontendQ $ Bl.singleton 9 <> price
            , ReadyS ready
            )


newPowInfoUpdate
    :: Word8
    -> Bl.ByteString
    -> Ready
    -> (Output, State)
newPowInfoUpdate difficulty unique ready =
    case authStatus ready of
        GettingPowInfoA ->
            ( MakeSessionKeyO
            , ReadyS $ ready
                { authStatus = GeneratingSessionKey difficulty unique
                }
            )

        GeneratingSessionKey _ _ ->
            pass

        AwaitingUsername _ _ ->
            pass

        LoggedIn _ ->
            pass

  where
    pass = (DoNothingO, ReadyS ready)


newUserIdUpdate :: UserId -> Ready -> (Output, State)
newUserIdUpdate userId ready =
    case authStatus ready of
        GettingPowInfoA ->
            pass

        GeneratingSessionKey _ _ ->
            pass

        AwaitingUsername staticKeys sessionKey ->
            let
                newReady =
                    ready
                        { authStatus = LoggedIn $
                            StaticKeys staticKeys sessionKey userId
                        }
            in
                ( BatchO
                    [ BytesInQO toServerQ (logIn sessionKey userId)
                    , dumpCache newReady
                    ]
                , ReadyS newReady
                )

        LoggedIn _ ->
            logErr
                ready
                "received new username but authstatus is LoggedIn"

  where
    pass = (DoNothingO, ReadyS ready)


num1stShakes =
    333


ciphertextP :: P.Parser Ciphertext
ciphertextP = do
    P.choice
        [ fmap FirstHandshakes firstShakesP
        , fmap SecondHandshakes secondShakesP
        , fmap TransportC transportP
        ]


secondShakeLen =
    64


secondShakesP :: P.Parser [SecondHandshake]
secondShakesP = do
    _ <- P.word8 1
    P.many1 secondShakeP


secondShakeP :: P.Parser SecondHandshake
secondShakeP = do
    key <- publicEphemeralP
    msg <- P.take secondShakeLen
    return $ SecondHandshake key $ Bl.fromStrict msg


noiseTransportLen =
    15958


transportP :: P.Parser Transport
transportP = do
    _ <- P.word8 2
    key <- publicEphemeralP
    noise <- P.take noiseTransportLen
    return $ Transport key $ Bl.fromStrict noise


firstShakesP :: P.Parser [PublicEphemeral]
firstShakesP = do
    _ <- P.word8 0
    P.count num1stShakes publicEphemeralP


publicKeyLen =
    32


publicEphemeralP :: P.Parser PublicEphemeral
publicEphemeralP = do
    fmap (PublicEphemeral . Bl.fromStrict) $ P.take publicKeyLen


logErr :: Ready -> T.Text -> (Output, State)
logErr ready err =
    case theTime ready of
        [] ->
            ( ErrorO "no timestamps available", FailedS)

        t : ime ->
            ( AppendFileO (logPath $ root ready) .
              Bl.fromStrict .
              encodeUtf8 $
              T.pack (show t) <> " " <> err <> "\n"
            , ReadyS $ ready { theTime = ime }
            )


messageInUpdate
    :: Username
    -> ClientToClient
    -> Ready
    -> (Output, State)
messageInUpdate from (ClientToClient raw) ready =
    case P.eitherResult $ P.parse ciphertextP raw of
        Left err ->
            logErr ready $
                "could not parse ciphertext: " <> T.pack err

        Right (FirstHandshakes msgs) ->
            firstHandshakesUpdate from ready msgs

        Right (SecondHandshakes secondShakes) ->
            secondHandshakesUpdate from ready secondShakes

        Right (TransportC _) ->
            undefined


secondHandshakesUpdate
    :: Username
    -> Ready
    -> [SecondHandshake]
    -> (Output, State)
secondHandshakesUpdate from ready shakes =
    ( DoNothingO
    , let
        shakeMap = make2ndShakeMap shakes from
        newShakes = Map.mapWithKey
            (add2ndShakes shakeMap)
            (handshakes ready)
      in ReadyS $ ready { handshakes = newShakes }
    )


add2ndShakes
    :: Map.Map HandshakeId Bl.ByteString
    -> HandshakeId
    -> Handshake
    -> Handshake
add2ndShakes new2ndShakes id_ oldShake =
    case Map.lookup id_ new2ndShakes of
        Nothing ->
            oldShake

        Just newMsg ->
            case oldShake of
                ResponderSentEncryptedE _ ->
                    oldShake

                Initiator SentPlainE ->
                    add2ndShake id_ newMsg

                Initiator (ReceivedEncryptedE _) ->
                    oldShake


add2ndShake :: HandshakeId -> Bl.ByteString -> Handshake
add2ndShake (HandshakeId username ephemeral) newMsg =
    Initiator $ ReceivedEncryptedE (EncryptedEphemeral newMsg)


make2ndShakeMap
    :: [SecondHandshake]
    -> Username
    -> Map.Map HandshakeId Bl.ByteString
make2ndShakeMap shakes username =
    Map.fromList $ map (make2ndShakeHelp username) shakes


make2ndShakeHelp
    :: Username
    -> SecondHandshake
    -> (HandshakeId, Bl.ByteString)
make2ndShakeHelp username (SecondHandshake publicEphemeral msg) =
    (HandshakeId username publicEphemeral, msg)


firstHandshakesUpdate
    :: Username
    -> Ready
    -> [PublicEphemeral]
    -> (Output, State)
firstHandshakesUpdate from ready msgs =
    case Map.lookup from (whitelist ready) of
        Nothing ->
            (logErr ready $ "unsolicited first handshakes from " <>
                T.pack (show from)
            , DoNothingO
            )

        Just (theirStatic, _) ->
            firstHandshakesUpdateHelp theirStatic from ready msgs


firstHandshakesUpdateHelp theirStatic from ready msgs =
    let
        myEphemerals = map MyEphemeral $
            take num1stShakes $ newDhKeys ready
        newHandshakes =
            makeResponderShakes myEphemerals msgs from
        dhKeys = drop num1stShakes $ newDhKeys ready
        newReady = ready
            { newDhKeys = dhKeys
            , handshakes =
                Map.union (handshakes ready) newHandshakes
            }
    in case authStatus ready of
        GettingPowInfoA ->
            pass

        GeneratingSessionKey _ _ ->
            pass

        AwaitingUsername _ _ ->
            pass

        LoggedIn (StaticKeys myStatic _ _) ->
            case sendNoiseKK2s myEphemerals msgs myStatic theirStatic from of
                Left err ->
                    logErr newReady err

                Right kk2s ->
                    ( BatchO [kk2s, dumpCache ready]
                    , ReadyS newReady
                    )

  where
    pass = (DoNothingO, ReadyS ready)


logPath :: RootPath -> FilePath
logPath (RootPath root) =
    root </> "log"


makeResponderShakes
    :: [MyEphemeral]
    -> [PublicEphemeral]
    -> Username
    -> Map.Map HandshakeId Handshake
makeResponderShakes myNewEphemerals firstShakes from =
    Map.fromList $ map (makeResponderShake from) $
        zip myNewEphemerals firstShakes


makeResponderShake
    :: Username
    -> (MyEphemeral, PublicEphemeral)
    -> (HandshakeId, Handshake)
makeResponderShake from (myEphemeral, theirEphemeral) =
    ( HandshakeId from theirEphemeral
    , ResponderSentEncryptedE myEphemeral
    )


sendNoiseKK2s
    :: [MyEphemeral]
    -> [PublicEphemeral]
    -> MyStatic
    -> TheirStatic
    -> Username
    -> Either T.Text Output
sendNoiseKK2s myEphemerals firsts myStatic theirStatic from =
    let
        max2nd = 166
        zipped = zip myEphemerals firsts
        first166 = take max2nd zipped
        second166 = take max2nd $ drop max2nd zipped
        third = drop (2 * max2nd) zipped
        eitherChunks =
            map
                (make2ndNoise from theirStatic myStatic)
                [first166, second166, third]
    in case allRight eitherChunks of
        Left err ->
            Left err

        Right chunks ->
            Right $ BatchO $ map (BytesInQO toServerQ) chunks


make2ndNoise
    :: Username
    -> TheirStatic
    -> MyStatic
    -> [(MyEphemeral, PublicEphemeral)]
    -> Either T.Text Bl.ByteString
make2ndNoise (Username username) theirStatic myStatic noises1 =
    let
        eitherNoises = map (makeOne2ndNoise theirStatic myStatic) noises1
    in case allRight eitherNoises of
        Left err ->
            Left err

        Right noises2 ->
            Right $ mconcat
                [ Bl.singleton 3
                , username
                , Bl.singleton 1
                , mconcat noises2
                ]


allRight :: [Either a b] -> Either a [b]
allRight eithers =
    allRightHelp eithers []


allRightHelp :: [Either a b] -> [b] -> Either a [b]
allRightHelp eithers accum =
    case eithers of
        [] ->
            Right $ reverse accum

        Left l : _ ->
            Left l

        Right r : remains ->
            allRightHelp remains (r : accum)


responderOptions
    :: TheirStatic
    -> MyStatic
    -> MyEphemeral
    -> Noise.HandshakeOpts Curve25519
responderOptions
    (TheirStatic theirStatic)
    (MyStatic myStatic)
    (MyEphemeral myEphemeral) =

    Noise.setRemoteStatic (Just theirStatic) .
    Noise.setLocalStatic (Just myStatic) .
    Noise.setLocalEphemeral (Just myEphemeral) $
    Noise.defaultHandshakeOpts Noise.ResponderRole ""


initiatorOptions
    :: TheirStatic
    -> MyStatic
    -> MyEphemeral
    -> Noise.HandshakeOpts Curve25519
initiatorOptions
    (TheirStatic theirStatic)
    (MyStatic myStatic)
    (MyEphemeral myEphemeral) =

    Noise.setRemoteStatic (Just theirStatic) .
    Noise.setLocalStatic (Just myStatic) .
    Noise.setLocalEphemeral (Just myEphemeral) $
    Noise.defaultHandshakeOpts Noise.InitiatorRole ""


type NoiseState
    = Noise.NoiseState ChaChaPoly1305 Curve25519 BLAKE2s


makeOne2ndNoise
    :: TheirStatic
    -> MyStatic
    -> (MyEphemeral, PublicEphemeral)
    -> Either T.Text Bl.ByteString
makeOne2ndNoise
    theirStatic
    myStatic
    (myEphemeral, PublicEphemeral refKey) =

    let
        options = responderOptions theirStatic myStatic myEphemeral
        noise0 = Noise.noiseState options noiseKK :: NoiseState
        msg0 = Noise.convert $ Bl.toStrict refKey
    in case Noise.readMessage msg0 noise0 of
        Noise.NoiseResultMessage _ noise1 ->
            case Noise.writeMessage "" noise1 of
                Noise.NoiseResultMessage secondShake _ ->
                    Right $ Bl.fromStrict $ Noise.convert secondShake

                Noise.NoiseResultNeedPSK _ ->
                    Left needPskErr

                Noise.NoiseResultException err ->
                    Left $ noiseException err

        Noise.NoiseResultNeedPSK _ ->
            Left needPskErr

        Noise.NoiseResultException err ->
            Left $ noiseException err


needPskErr =
    "could not decrypt first handshake message: NoiseResultNeedPSK"


noiseException :: SomeException -> T.Text
noiseException err =
    "could not decrypt first handshake message: " <>
    "NoiseResultException: " <>
    T.pack (show err)


dumpCache :: Ready -> Output
dumpCache ready =
    case getCache ready of
        Nothing ->
            DoNothingO

        Just cache ->
            WriteFileO (cachePath $ root ready) (encodeCache cache)


cachePath :: RootPath -> FilePath
cachePath (RootPath root) =
    root </> "memCache"


encodeCache :: MemCache -> Bl.ByteString
encodeCache memCache =
     mconcat
        [ encodeCryptoKeys $ keys memCache
        , encodeHandshakes $ handshakesM memCache
        , encodeWhitelist $ whitelistM memCache
        ]


getCache :: Ready -> Maybe MemCache
getCache ready =
    case authStatus ready of
        GettingPowInfoA ->
            Nothing

        GeneratingSessionKey _ _ ->
            Nothing

        AwaitingUsername _ _ ->
            Nothing

        LoggedIn keys ->
            Just $ MemCache
                { keys
                , handshakesM = handshakes ready
                , whitelistM = whitelist ready
                }


encodeWhitelist :: Set.Set UserId -> Bl.ByteString
encodeWhitelist whitelist =
    let
        asList = Set.toList whitelist
    in
        encodeUint32 (length asList) <>
        mconcat (map encodeUserId asList)


encodeCryptoKeys :: StaticKeys -> Bl.ByteString
encodeCryptoKeys
    (StaticKeys
        (MyStatic (secret, _))
        (SessionKey sessionKey)
        userId) =

    mconcat
        [ sessionKey
        , encodeUserId userId
        , Bl.fromStrict $ Noise.convert $ Dh.dhSecToBytes secret
        ]


encodeUserId :: UserId -> Bl.ByteString
encodeUserId (UserId (Username u) (Fingerprint f)) =
    u <> f


encodeHandshakes :: Map.Map HandshakeId Handshake -> Bl.ByteString
encodeHandshakes =
    mconcat . map encodeHandshake . Map.toList


encodeHandshake :: (HandshakeId, Handshake) -> Bl.ByteString
encodeHandshake (id_, shake) =
    encodeHandshakeId id_ <> encodeHandshakeShake shake


encodeHandshakeId :: HandshakeId -> Bl.ByteString
encodeHandshakeId (HandshakeId (Username u) (PublicEphemeral e)) =
    u <> e


encodeHandshakeShake :: Handshake -> Bl.ByteString
encodeHandshakeShake h =
    case h of
        Initiator i ->
            mconcat
                [ Bl.singleton 0
                , case i of
                    SentPlainE ->
                        Bl.singleton 0

                    ReceivedEncryptedE (EncryptedEphemeral e) ->
                        Bl.singleton 1 <> e
                ]

        ResponderSentEncryptedE (MyEphemeral (secret, _)) ->
            mconcat
                [ Bl.singleton 1
                , Bl.fromStrict $ Noise.convert $
                    Dh.dhSecToBytes secret
                ]


toServerQ :: Q
toServerQ =
    Q.newTQueue


messageInLength =
    15991


fromServerP :: P.Parser FromServer
fromServerP =
    P.choice
        [ do
            _ <- P.word8 0
            username <- usernameP
            messageIn <- fmap
                (ClientToClient . Bl.fromStrict)
                (P.take messageInLength)
            P.endOfInput
            return $ NewMessageT username messageIn
        , do
            _ <- P.word8 1
            username <- usernameP
            return $ NewUsername username
        , do
            _ <- P.word8 2
            difficulty <- uint8P
            unique <- P.take 16
            return $ PowInfoT
                (fromIntegral difficulty)
                (Bl.fromStrict unique)
        , do
            _ <- P.word8 3
            price <- P.take 4
            return $ Price $ Bl.fromStrict price
        ]


theirStaticP :: P.Parser TheirStatic
theirStaticP = do
    raw <- P.take 32
    case Dh.dhBytesToPub $ Noise.convert raw of
        Nothing ->
            fail "could not parse their static key"

        Just dhKey ->
            return $ TheirStatic dhKey


data FromServer
    = NewMessageT Username ClientToClient
    | NewUsername Username
    | PowInfoT Word8 Bl.ByteString
    | Price Bl.ByteString


data Ciphertext
    = FirstHandshakes [PublicEphemeral]
    | SecondHandshakes [SecondHandshake]
    | TransportC Transport


data SecondHandshake
    = SecondHandshake PublicEphemeral Bl.ByteString


data Transport
    = Transport PublicEphemeral Bl.ByteString


newtype TheirStatic
    = TheirStatic (Dh.PublicKey Curve25519)


newtype ClientToClient
    = ClientToClient Bl.ByteString


onGetBlobUpdate :: Bl.ByteString -> Q -> Ready -> (Output, State)
onGetBlobUpdate body q ready =
    case P.eitherResult $ P.parse blobHashP body of
        Left err ->
            ( ErrorO $ "could not parse GetBlob: " <> err
            , FailedS
            )

        Right hash ->
            case getBlob ready of
                NoJobs ->
                    ( ReadFileLazyO $ makeBlobPath (root ready) hash
                    , ReadyS $ ready
                        { getBlob = Jobs (GetBlob q hash) [] }
                    )

                Jobs current waiting ->
                    ( DoNothingO
                    , ReadyS $ ready
                        { getBlob = Jobs
                            current
                            (GetBlobWait body q : waiting)
                        }
                    )


parseFromFrontend :: Bl.ByteString -> Either String FromFrontend
parseFromFrontend raw =
    P.eitherResult $ P.parse fromFrontendP raw


data UserId
    = UserId Username Fingerprint
    deriving (Show)


instance Ord UserId where
    compare (UserId u1 f1) (UserId u2 f2) =
        case (compare u1 u2, compare f1 f2) of
            (LT, LT) ->
                LT

            (EQ, EQ) ->
                EQ

            (GT, GT) ->
                GT

            (GT, LT) ->
                GT

            (LT, GT) ->
                LT

            (LT, EQ) ->
                LT

            (GT, EQ) ->
                GT

            (EQ, GT) ->
                GT

            (EQ, LT) ->
                LT


newtype Fingerprint
    = Fingerprint Bl.ByteString
    deriving (Show)


instance Ord Fingerprint where
    compare (Fingerprint a) (Fingerprint b) =
        compare a b


newtype Username
    = Username Bl.ByteString
    deriving (Show)


instance Ord Username where
    compare (Username a) (Username b) =
        compare a b


instance Eq UserId where
    (==) (UserId u1 f1) (UserId u2 f2) =
        u1 == u2 && f1 == f2


instance Eq Fingerprint where
    (==) (Fingerprint a) (Fingerprint b) =
        a == b


instance Eq Username where
    (==) (Username a) (Username b) =
        a == b


userIdP :: P.Parser UserId
userIdP = do
    username <- P.take 8
    fingerprint <- P.take 8
    return $ UserId
        (Username $ Bl.fromStrict username)
        (Fingerprint $ Bl.fromStrict fingerprint)


blobHashP :: P.Parser Hash32
blobHashP = do
    hash <- hash32P
    P.endOfInput
    return hash


hash32P :: P.Parser Hash32
hash32P = do
    hash <- P.take 32
    return $ Hash32 hash


newtype Message
    = Message Bl.ByteString


newtype ChainId
    = ChainId Bl.ByteString


data FromFrontend
    = NewMessageA MessageId Message
    | WriteIndex Bl.ByteString
    | GetIndex
    | AddToWhitelist UserId
    | RemoveFromWhitelist UserId
    | GetMessage MessageId
    | GetWhitelist
    | GetMyId
    | GetChainSummary ChainId
    | GetPayments
    | GetPrice
    | GetMembership


newtype MessageId
    = MessageId Bl.ByteString


instance Ord MessageId where
    compare (MessageId a) (MessageId b) =
        compare a b


instance Eq MessageId where
    (==) (MessageId a) (MessageId b) =
        a == b


messageIdP :: P.Parser MessageId
messageIdP = do
    id_ <- P.take 24
    return $ MessageId $ Bl.fromStrict id_


messageP :: P.Parser Message
messageP = do
    message <- P.takeByteString
    return $ Message $ Bl.fromStrict message


chainIdP :: P.Parser ChainId
chainIdP = do
    chainId <- P.take 20
    return $ ChainId $ Bl.fromStrict chainId


fromFrontendP :: P.Parser FromFrontend
fromFrontendP = do
    input <- P.choice
        [ do
            _ <- P.word8 0
            messageId <- messageIdP
            message <- messageP
            return $ NewMessageA messageId message
        , do
            _ <- P.word8 1
            index <- P.takeByteString
            return $ WriteIndex $ Bl.fromStrict index
        , do
            _ <- P.word8 2
            return GetIndex
        , do
            _ <- P.word8 3
            userId <- userIdP
            return $ AddToWhitelist userId
        , do
            _ <- P.word8 4
            userId <- userIdP
            return $ RemoveFromWhitelist userId
        , do
            _ <- P.word8 5
            messageId <- messageIdP
            return $ GetMessage messageId
        , do
            _ <- P.word8 6
            return GetWhitelist
        , do
            _ <- P.word8 7
            return GetMyId
        , do
            _ <- P.word8 8
            chainId <- chainIdP
            return $ GetChainSummary chainId
        , do
            _ <- P.word8 9
            return GetPayments
        , do
            _ <- P.word8 10
            return GetPrice
        , do
            _ <- P.word8 11
            return GetMembership
        ]
    P.endOfInput
    return input


addToWhitelistUpdate :: UserId -> Ready -> (Output, State)
addToWhitelistUpdate (UserId username fingerprint) ready =
    let
        newReady :: Ready
        newReady =
            ready
                { whitelist =
                    Map.insert username fingerprint (whitelist ready)
                }
    in
        (dumpCache newReady, ReadyS newReady)


uiApiUpdate :: FromFrontend -> State -> (Output, State)
uiApiUpdate apiInput model =
    case apiInput of
        NewMessageA _ _ ->
            undefined

        AddToWhitelist userId ->
            updateReady model $ addToWhitelistUpdate userId

        RemoveFromWhitelist _ ->
            undefined

        GetMessage _ ->
            undefined

        GetWhitelist ->
            undefined

        GetMyId ->
            undefined

        WriteIndex _ ->
            undefined

        GetIndex ->
            undefined

        GetChainSummary _ ->
            undefined

        GetPayments ->
            undefined

        GetPrice ->
            undefined

        GetMembership ->
            undefined


onMovedFile :: FilePath -> Ready -> (Output, State)
onMovedFile new ready =
    case blobsUp ready of
        NoJobs ->
            pass

        Jobs (AwaitingMoveB q (Hash32 hash) toPath) [] ->
            if toPath == new then
                ( BytesInQO q (Bl.singleton 1 <> Bl.fromStrict hash)
                , ReadyS $ ready { blobsUp = NoJobs }
                )

            else
                pass

        Jobs
            (AwaitingMoveB q (Hash32 hash) toPath)
            (BlobUpWait blob newQ : _) ->

            if toPath == new then
                let
                    (output, newModel) = update
                        (ReadyS $ ready
                            { blobsUp =
                                promoteBlobsUp $ blobsUp ready
                            })
                        (SetBlobM blob newQ)
                in
                    ( BatchO
                        [ BytesInQO
                            q
                            (Bl.singleton 1 <>
                                Bl.fromStrict hash)
                        , output
                        ]
                    , newModel
                    )
            else
                pass

        Jobs (AwaitingTmpWriteB _ _ _) _ ->
            pass

        Jobs (AwaitingHandleB _ _ _) _ ->
            pass

  where
    pass = (DoNothingO, ReadyS ready)


onWrittenToTmp :: FilePath -> Io.Handle -> Ready -> (Output, State)
onWrittenToTmp writtenPath handle ready =
    case blobsUp ready of
        NoJobs ->
            pass

        Jobs (AwaitingTmpWriteB q hash expectPath) waiting ->
            if writtenPath == expectPath then
                let
                    blobPath = makeBlobPath (root ready) hash
                in
                    ( BatchO
                        [ MoveFileO writtenPath blobPath
                        , CloseFileO handle
                        ]
                    , ReadyS $ ready
                        { blobsUp =
                            Jobs
                                (AwaitingMoveB q hash blobPath)
                                waiting
                        }
                    )

            else
                pass

        Jobs (AwaitingHandleB _ _ _) _ ->
            pass

        Jobs (AwaitingMoveB _ _ _) _ ->
            pass

    where
        pass = (DoNothingO, ReadyS ready)


encodeHash :: Hash32 -> String
encodeHash (Hash32 hash) =
    Bc.unpack $ B64.encodeUnpadded hash


makeBlobPath :: RootPath -> Hash32 -> FilePath
makeBlobPath (RootPath rootPath) hash =
    rootPath </> encodeHash hash


onTmpFileHandleUpdate
    :: FilePath
    -> Io.Handle
    -> Ready
    -> (Output, State)
onTmpFileHandleUpdate path handle ready =
    case blobsUp ready of
        NoJobs ->
            (DoNothingO, ReadyS ready)

        Jobs (AwaitingHandleB q blob hash) waiting ->
            ( WriteToHandleO handle blob path
            , ReadyS $ ready {
                blobsUp = Jobs
                    (AwaitingTmpWriteB q hash path)
                    waiting
                }
            )

        Jobs _ _ ->
            (DoNothingO, ReadyS ready)


setBlobUpdate ::
    Bl.ByteString ->
    Q ->
    Ready ->
    (Output, State)
setBlobUpdate blob q ready =
    case blobsUp ready of
        NoJobs ->
            ( GetTmpFileHandleO $ tempPath $ root ready
            , ReadyS $ ready
                { blobsUp =
                    Jobs (AwaitingHandleB q blob (hash32 blob)) []
                }
            )

        Jobs current waiting ->
            ( DoNothingO
            , ReadyS $
                ready
                    { blobsUp =
                        Jobs current (BlobUpWait blob q : waiting)
                    }
            )


hash32 :: Bl.ByteString -> Hash32
hash32 =
    Hash32 .
    Blake.finalize 32 .
    Bl.foldrChunks Blake.update (Blake.initialize 32)


tempPath :: RootPath -> FilePath
tempPath (RootPath root) =
    root </> "temporary"


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


fileContentsUpdate
    :: FilePath
    -> Bl.ByteString
    -> Init
    -> (Output, State)
fileContentsUpdate path contents init_ =
    case init_ of
        EmptyI ->
            pass

        MakingRootDirI _ ->
            pass

        MakingDhKeysI _ ->
            pass

        GettingTimes _ _ ->
            pass

        DoKeysExistI _ _ _ ->
            pass

        GettingKeysFromFileI root dhKeys times ->
            if path == keysPath root then
                rawKeysUpdate contents root dhKeys times
            else
                pass

  where
    pass = (DoNothingO, InitS init_)


parseCrypto
    :: Bl.ByteString
    -> Either String MemCache
parseCrypto raw =
    P.eitherResult $ P.parse memCacheP raw


data MemCache
    = MemCache
        { keys :: StaticKeys
        , handshakesM :: Map.Map HandshakeId Handshake
        , whitelistM :: Map.Map Username Fingerprint
        }


memCacheP :: P.Parser MemCache
memCacheP = do
    keys <- myKeysP
    handshakesM <- handshakesP
    whitelistM <- whitelistP
    P.endOfInput
    return $ MemCache {keys, handshakesM, whitelistM}


whitelistP :: P.Parser (Map.Map Username Fingerprint)
whitelistP = do
    numWhites <- uint32P
    asList <- P.count numWhites oneWhitelistP
    return $ Map.fromList asList


oneWhitelistP :: P.Parser (Username, Fingerprint)
oneWhitelistP = do
    username <- usernameP
    fingerprint <- fingerprintP
    return (username, fingerprint)


fingerprintP :: P.Parser Fingerprint
fingerprintP = do
    raw <- P.take 8
    return $ Fingerprint $ Bl.fromStrict raw


handshakesP :: P.Parser (Map.Map HandshakeId Handshake)
handshakesP = do
    numshakes <- uint32P
    asList <- P.count numshakes handshakeP
    return $ Map.fromList asList


uint32P :: P.Parser Int
uint32P = do
    b0 <- uint8P
    b1 <- uint8P
    b2 <- uint8P
    b3 <- uint8P
    return $ b0 + b1 * 256 + b2 * 256 * 256 + b3 * 256 * 256 * 256


handshakeP :: P.Parser (HandshakeId, Handshake)
handshakeP = do
    handshakeId <- handshakeIdP
    handshake <- P.choice
        [ fmap Initiator initiatorP
        , responderP
        ]
    return (handshakeId, handshake)


initiatorP :: P.Parser InitiatorHandshake
initiatorP = do
    _ <- P.word8 0
    P.choice
        [ do
            _ <- P.word8 0
            return SentPlainE
        , do
            _ <- P.word8 1
            ephemeral <- encryptedEphemeralP
            return $ ReceivedEncryptedE ephemeral
        ]


responderP :: P.Parser Handshake
responderP = do
    _ <- P.word8 1
    myEphemeral <- myEphemeralP
    return $ ResponderSentEncryptedE myEphemeral


secretKeyLen =
    32


myEphemeralP :: P.Parser MyEphemeral
myEphemeralP = do
    rawSecret <- P.take secretKeyLen
    case Dh.dhBytesToPair $ Noise.convert rawSecret of
        Nothing ->
            fail "could not convert bytes to DH key pair"

        Just keypair ->
            return $ MyEphemeral keypair


encryptedEphemeralP :: P.Parser EncryptedEphemeral
encryptedEphemeralP =
    fmap EncryptedEphemeral $
    fmap Bl.fromStrict $
    P.take $
    16 + 32 + 16


handshakeIdP :: P.Parser HandshakeId
handshakeIdP = do
    username <- usernameP
    publicEphemeral <- fmap PublicEphemeral $
        fmap Bl.fromStrict $ P.take 32
    return $ HandshakeId username publicEphemeral


usernameP :: P.Parser Username
usernameP = do
    username <- P.take 8
    return $ Username $ Bl.fromStrict username


uint16P :: P.Parser Int
uint16P = do
    b0 <- uint8P
    b1 <- uint8P
    return $ b0 + b1 * 256


parseLength :: Bl.ByteString -> Either String Int
parseLength bytes2 =
    P.eitherResult $ P.parse uint16P bytes2


uint8P :: P.Parser Int
uint8P =
    fromIntegral <$> P.anyWord8


sessionKeyLength :: Int
sessionKeyLength =
    16


myKeysP :: P.Parser StaticKeys
myKeysP = do
    sessionKey <- P.take sessionKeyLength
    userId <- userIdP
    rawDhKey <- P.take secretKeyLen
    case Dh.dhBytesToPair $ Noise.convert rawDhKey of
        Nothing ->
            fail "could not parse secret key from file"

        Just dhKeys ->
            return $
                StaticKeys
                    { staticDh = MyStatic dhKeys
                    , sessionKey =
                        SessionKey $ Bl.fromStrict sessionKey
                    , userId = userId
                    }


rawKeysUpdate
    :: Bl.ByteString
    -> RootPath
    -> [Dh.KeyPair Curve25519]
    -> [Clock.UTCTime]
    -> (Output, State)
rawKeysUpdate rawCrypto root newDhKeys times =
    case parseCrypto rawCrypto of
        Left err ->
            ( ErrorO $ "could not parse static keys: " <> err
            , FailedS
            )

        Right memCache ->
            ( BatchO
                [ BytesInQO toServerQ $
                  logIn (sessionKey $ keys memCache) $
                  userId $
                  keys memCache
                ]
            , ReadyS $ Ready
                { root
                , blobsUp = NoJobs
                , getBlob = NoJobs
                , messageLocks = Set.empty
                , authStatus = LoggedIn $ keys memCache
                , handshakes = handshakesM memCache
                , newDhKeys
                , theTime = times
                , whitelist = whitelistM memCache
                }
            )


dirCreatedUpdate :: FilePath -> Init -> (Output, State)
dirCreatedUpdate path initModel =
    case initModel of
        EmptyI ->
            pass

        MakingRootDirI root@(RootPath r) ->
            if path == r then
                ( BatchO
                    [ StartUiServerO
                    , StartUiO
                    , MakeDhKeysO
                    ]
                , InitS $ MakingDhKeysI root
                )
            else
                pass

        MakingDhKeysI _ ->
            pass

        GettingTimes _ _ ->
            pass

        DoKeysExistI _ _ _ ->
            pass

        GettingKeysFromFileI _ _ _ ->
            pass

  where
    pass = (DoNothingO, InitS initModel)

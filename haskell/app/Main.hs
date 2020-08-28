{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GADTs #-}
module Main (main) where

import qualified Control.Concurrent.STM as Stm
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Handler.Warp (run)
import qualified Control.Concurrent.STM.TQueue as Q
import Crypto.Noise.DH.Curve25519 (Curve25519)
import Crypto.Noise.Cipher.ChaChaPoly1305 (ChaChaPoly1305)
import Crypto.Noise.Hash.BLAKE2s (BLAKE2s)
import qualified Crypto.Noise.DH as Dh
import qualified Crypto.Noise as Noise
import Crypto.Noise.HandshakePatterns (noiseKX)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified System.Directory as Dir
import qualified Database.SQLite.Simple as Sql
import Database.SQLite.Simple.ToField (ToField, toField)
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
import qualified System.Process as Process
import qualified Data.Set as Set
import qualified Data.Time.Clock as Clock
import qualified Codec.Archive.Tar as Tar
import qualified Crypto.Random as CryptoRand
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Network.Simple.TCP as Tcp
import qualified Data.Bits as Bits


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

        DbExecute_O path query -> do
            Sql.withConnection path $ \conn ->
                Sql.execute_ conn query
            return Nothing

        DbExecuteO path (DbQuery query params) -> do
            Sql.withConnection path $ \conn ->
                Sql.execute conn query params
            return Nothing

        DbPublicKeyO path (DbQuery query params) -> do
            rows <- Sql.withConnection path $ \conn ->
                Sql.query conn query params
            return $ Just $ DbPublicKeyM rows

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

        DoesDirExistO path -> do
            exists <- Dir.doesDirectoryExist path
            return $ Just $ DirExistsM path exists

        RunCommandO process -> do
            (out, err) <- Process.withCreateProcess
                process
                processProcess
            return $ Just $ CommandResultM out err process

        WriteFileO path contents -> do
            B.writeFile path contents
            return Nothing

        GetTimesO -> do
            times <- mapM
                (\_ -> Clock.getCurrentTime) ([1..] :: [Integer])
            return $ Just $ TimesM times 

        TarO base dirToTar -> do
            tarred <- Tar.pack base [dirToTar]
            return $ Just $ TarredM (base, dirToTar) tarred

        MakeBlobIdsO -> do
            drg <- CryptoRand.drgNew
            return $ Just $ RandomGenM drg

        StartTcpClientO -> do
            tcpClient
            return Nothing

        DoesFileExistO path -> do
            exists <- Dir.doesFileExist path
            return $ Just $ FileExistenceM path exists

        GenerateDhPairO -> do
            keys <- Dh.dhGenKey
            return $ Just $ NewDhKeysM keys
            


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
                                    Q.writeTQueue q $ TcpMsgM $ Bl.fromStrict message
                                tcpListen conn



tcpDelay =
    20 * 1000000

        
serverUrl =
    "http://localhost"


serverPort =
    "11453"


processProcess
    :: Maybe Io.Handle
    -> Maybe Io.Handle
    -> Maybe Io.Handle
    -> Process.ProcessHandle
    -> IO (StdOut, StdErr)
processProcess _ maybeStdoutH maybeStderrH _ = do
    stdOut <- drainMaybeHandle maybeStdoutH
    stdErr <- drainMaybeHandle maybeStderrH
    return $ (StdOut stdOut, StdErr stdErr)


instance Show MessageId where
    show (MessageId m) = show m


drainMaybeHandle :: Maybe Io.Handle -> IO (Maybe String)
drainMaybeHandle maybeHandle =
    case maybeHandle of
        Nothing ->
            return Nothing

        Just h -> do
            contents <- Io.hGetContents h
            return $ Just contents


newtype StdOut
    = StdOut (Maybe String)


newtype StdErr
    = StdErr (Maybe String)


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


data DbQuery where
    DbQuery :: Sql.ToRow q => Sql.Query -> q -> DbQuery


data Output
    = GetAppDataDirO
    | GetTimesO
    | MakeBlobIdsO
    | MakeDirIfMissingO FilePath
    | DoNothingO
    | ReadFileO FilePath
    | DbExecute_O FilePath Sql.Query
    | DbExecuteO FilePath DbQuery
    | DbPublicKeyO FilePath DbQuery
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
    | DoesDirExistO FilePath
    | RunCommandO Process.CreateProcess
    | WriteFileO FilePath B.ByteString
    | TarO FilePath FilePath
    | DoesFileExistO FilePath
    | GenerateDhPairO


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
    | DirExistsM FilePath Bool
    | CommandResultM StdOut StdErr Process.CreateProcess
    | TimesM [Clock.UTCTime]
    | TarredM (FilePath, FilePath) [Tar.Entry]
    | DbPublicKeyM [(Integer, B.ByteString)]
    | RandomGenM CryptoRand.ChaChaDRG
    | TcpMsgM Bl.ByteString
    | RestartingTcpM
    | FileExistenceM FilePath Bool
    | NewDhKeysM (Dh.KeyPair Curve25519)


websocket :: Ws.ServerApp
websocket pending = do
    conn <- Ws.acceptRequest pending
    _ <- forkIO $ websocketSend conn
    websocketReceive conn


websocketSend :: Ws.Connection -> IO ()
websocketSend conn = do
    out <- Stm.atomically $ do
        q <- toWebsocketQ
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


toWebsocketQ :: Q
toWebsocketQ =
    Q.newTQueue


newtype SessionKey
    = SessionKey Bl.ByteString

newtype RootPath
    = RootPath FilePath

data StaticKeys
    = StaticKeys MyStatic SessionKey UserId


newtype Hash32 = Hash32 B.ByteString


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
    | GettingTimesI RootPath
    | GettingRandomGenI RootPath [Clock.UTCTime]
    | DoKeysExistI RootPath [Clock.UTCTime] CryptoRand.ChaChaDRG
    | GettingKeysFromFileI RootPath [Clock.UTCTime] CryptoRand.ChaChaDRG


data Ready = Ready
    { root :: RootPath
    , blobsUp :: Jobs BlobUp BlobUpWait
    , getBlob :: Jobs GetBlob GetBlobWait
    , setMessage :: Jobs SetMessage SetMessageWait
    , sendMessage :: Maybe SendMessage
    , messageLocks :: Set.Set MessageId
    , drg :: CryptoRand.ChaChaDRG
    , time :: [Clock.UTCTime]
    , sendingEphemeral :: Bool
    , authStatus :: AuthStatus
    }


data PowInfo
    = PowInfo Word8 Bl.ByteString


data AuthStatus
    = GettingPowInfoA
    | GeneratingStaticKeys PowInfo
    | AwaitingUsername (Dh.KeyPair Curve25519) SessionKey
    | LoggedIn StaticKeys


data SendMessage
    = SendMessage
        { messageId :: MessageId
        , userId :: UserId
        , commit :: Commit
        , status :: SendStatus
        }


data SendStatus
    = BareCloning
    | Reading
    | ToStaticKeyFromDb PlainText
    | ToEphemeralFromServer PlainText TheirStatic
    | MakingMyEphemeral PlainText TheirStatic TheirEphemeral
    | SendingPointer PlainText NoiseState
    | SendingChunks PlainText NoiseState Hash32
    | PuttingSentInDb



newtype TheirStatic
    = TheirStatic (Dh.PublicKey Curve25519)


type NoiseState
    = Noise.NoiseState ChaChaPoly1305 Curve25519 BLAKE2s


newtype PlainText
    = PlainText Bl.ByteString


data SetMessageWait
    = SetMessageWait MessageId Subject MainBox Metadata


data SetMessage
    = DoesItExist MessageId Subject MainBox Metadata
    | Initializing MessageId Subject MainBox Metadata
    | SettingSubject MessageId Subject MainBox Metadata
    | SettingMainBox MessageId MainBox Metadata
    | SettingMetadata MessageId Metadata
    | GitAdding MessageId
    | GitCommitting MessageId


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


promoteGetBlob :: Jobs GetBlob GetBlobWait -> Jobs GetBlob GetBlobWait
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


updateOnNewTimes :: [Clock.UTCTime] -> Init -> (Output, State)
updateOnNewTimes times init_ =
    case init_ of
        EmptyI ->
            pass

        MakingRootDirI _ ->
            pass

        GettingTimesI root ->
            ( MakeBlobIdsO
            , InitS $ GettingRandomGenI root times
            )

        GettingRandomGenI _ _ ->
            pass

        DoKeysExistI _ _ _ ->
            pass

        GettingKeysFromFileI _ _ _ ->
            pass

  where
    pass = (DoNothingO, InitS init_)


dbGetPublicKey :: UserId -> DbQuery
dbGetPublicKey userId =
    DbQuery
        "SELECT username, public_key FROM public_keys WHERE username = ?;"
        (Sql.Only userId)


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

        TimesM times ->
            updateInit model $ updateOnNewTimes times

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
                    case parseApiInput bin of
                        Left err ->
                            ( ErrorO $
                                "couldn't parse API input: " <> err
                            , FailedS
                            )

                        Right apiInput ->
                            uiApiUpdate apiInput model

        GetBlobM body q ->
            updateReady model $ onGetBlobUpdate body q

        DirExistsM path exists ->
            updateReady model $ dirExistsUpdate path exists

        CommandResultM stdout stderr process ->
            updateReady model $
                commandResultUpdate process stdout stderr

        TarredM paths tarred ->
            updateReady model $
                tarUpdate paths tarred

        DbPublicKeyM rows ->
            updateReady model $ dbPublicKeyUpdate rows

        RandomGenM gen ->
            updateInit model $ randomGenUpdate gen

        TcpMsgM raw ->
            updateReady model $ tcpMessageUpdate raw

        RestartingTcpM ->
            updateReady model restartingTcpUpdate

        FileExistenceM path exists ->
            updateInit model $ fileExistenceUpdate path exists

        NewDhKeysM keys ->
            updateReady model $ newDhKeysUpdate keys


makeSignUp
    :: PowInfo
    -> SessionKey
    -> Dh.PublicKey Curve25519
    -> Either String Bl.ByteString
makeSignUp powInfo (SessionKey sessionKey) pubkey =
    case makePow powInfo of
        Left err ->
            Left err

        Right pow ->
            Right $ mconcat
                [ Bl.singleton 2
                , pow
                , sessionKey
                , Bl.fromStrict $ Noise.convert $
                    Dh.dhPubToBytes pubkey
                ]


argonOptions :: Argon2.Options
argonOptions =
    Argon2.Options
        { iterations = 1
        , memory = 64 * 1024
        , parallelism = 4
        , variant = Argon2.Argon2id
        , version = Argon2.Version13
        }


makePow :: PowInfo -> Either String Bl.ByteString
makePow powInfo =
    powInfoHelp powInfo 0


encodeUint8 :: Integer -> Bl.ByteString
encodeUint8 i =
    Bl.pack $ map (encodeUint8Help i) (take 8 [0..])


encodeUint8Help :: Integer -> Int -> Word8
encodeUint8Help int counter =
    fromIntegral $ (int `Bits.shiftR` (counter * 8)) Bits..&. 0xFF


powInfoHelp :: PowInfo -> Integer -> Either String Bl.ByteString
powInfoHelp p@(PowInfo difficulty unique) counter =
    let
        candidate = encodeUint8 counter
        hashE = Argon2.hash
            argonOptions
            (Bl.toStrict $ unique <> candidate)
            (B.singleton 0)
            32
    in
        case hashE of
            CryptoPassed hash ->
                if isDifficult hash difficulty then
                    Right candidate
        
                else
                    powInfoHelp p (counter + 1)

            CryptoFailed err ->
                Left $ show err



isDifficult :: B.ByteString -> Word8 -> Bool
isDifficult bytes difficulty =
    let
        unpacked = B.unpack bytes
        difficult = filter (> difficulty) unpacked
    in
        length unpacked == length difficult


uploadEphemeral :: Dh.PublicKey Curve25519 -> Bl.ByteString
uploadEphemeral pubkey =
    mconcat
        [ Bl.singleton 9
        , Bl.fromStrict $ Noise.convert $ Dh.dhPubToBytes pubkey
        ]


cacheEphemeral :: Dh.KeyPair Curve25519 -> DbQuery
cacheEphemeral (secret, public) =
    DbQuery
        "INSERT INTO my_ephemeral_keys (public, secret) \
        \VALUES (?, ?);"
        (encodePub public, encodeSec secret)


encodePub :: Dh.PublicKey Curve25519 -> B.ByteString
encodePub =
    Noise.convert . Dh.dhPubToBytes


encodeSec :: Dh.SecretKey Curve25519 -> B.ByteString
encodeSec =
    Noise.convert . Dh.dhSecToBytes


newDhKeysUpdate :: Dh.KeyPair Curve25519 -> Ready -> (Output, State)
newDhKeysUpdate keys ready =
    if sendingEphemeral ready then
        ( BatchO
            [ DbExecuteO (dbPath $ root ready) $ cacheEphemeral keys
            , BytesInQO toServerQ $ uploadEphemeral (snd keys)
            ]
        , ReadyS $ ready { sendingEphemeral = False }
        )

    else case ephemeralForAuthUpdate keys ready of
        Just used ->
            used

        Nothing ->
            ephemeralForSend keys ready


ephemeralForSend :: MyEphemeral -> Ready -> (Output, State)
ephemeralForSend keys ready =
    case sendMessage ready of
        Nothing ->
            pass

        Just send ->
            case status send of
                BareCloning ->
                    pass

                Reading ->
                    pass

                ToStaticKeyFromDb _ ->
                    pass

                ToEphemeralFromServer _ _ ->
                    pass

                MakingMyEphemeral plain theirStat theirEph ->
                    sendPointer
                        plain
                        theirStat
                        theirEph
                        keys
                        ready
                        send

  where
    pass = (DoNothingO, ReadyS ready)


sendPointer plain theirStatic theirEphem myEphem ready send =
    case authStatus send of
        GettingPowInfoA ->
            pass

        GeneratingStaticKeys _ ->
            pass

        AwaitingUsername _ _ ->
            pass

        LoggedIn (StaticKeys myKeys _ _) ->
            let
                (noise, pointer) = makePointer
                    theirStatic
                    theirEphem
                    myKeys
                    myEphem
            in
                ( BytesInQO toServerQ pointer
                , SendingPointer plain noise
                )

  where
    pass = (DoNothingO, ReadyS ready)


newtype MyEphemeral
    = MyEphemeral (Dh.KeyPair Curve25519)


newtype MyStatic
    = MyStatic (Dh.KeyPair Curve25519)


makeInitNoise :: TheirStatic -> MyStatic -> MyEphemeral -> NoiseState
makeInitNoise (TheirStatic theirStatic) (MyStatic myStatic) (MyEphemeral myEphemeral) =
    let
        options_ :: Noise.HandshakeOpts Curve25519
        options_ = Noise.defaultHandshakeOpts
            Noise.ResponderRole
            ""
        options =
            Noise.setRemoteStatic (Just theirStatic) .
            Noise.setLocalEphemeral (Just myEphemeral) .
            Noise.setLocalStatic (Just myStatic) $ options_
    in
        Noise.noiseState options noiseKX


makePointer
    :: TheirEphemeral
    -> NoiseState
    -> BlobId
    -> Either T.Text (Bl.ByteString, NoiseState)
makePointer (TheirEphemeral theirEphemeral) noise0 (BlobId blobId) =
    case Noise.readMessage theirEphemeral noise0 of
        err@(Noise.NoiseResultNeedPSK _) ->
            Left $ T.pack $ show err

        err@(Noise.NoiseResultException _) ->
            Left $ T.pack $ show err

        Noise.NoiseResultMessage "" noise1 ->
            case Noise.writeMessage "" noise1 of
                err@(Noise.NoiseResultNeedPSK _) ->
                    Left $ T.pack $ show err

                err@(Noise.NoiseResultException _) ->
                    Left $ T.pack $ show err

                Noise.NoiseResultMessage cipher2 noise2 ->
                    case Noise.writeMessage blobId noise2 of
                        err@(Noise.NoiseResultNeedPSK _) ->
                            Left $ T.pack $ show err

                        err@(Noise.NoiseResultException _) ->
                            Left $ T.pack $ show err

                        Noise.NoiseResultMessage cipher3 noise3 ->
                            Right (cipher2 <> cipher3, noise3)
            

        err@(Noise.NoiseResultMessage nonEmpty _) ->
            Left $ mconcat
                [ "non-empty payload on ephemeral key"
                , T.pack $ show err
                ]

                
ephemeralForAuthUpdate
    :: Dh.KeyPair Curve25519
    -> Ready
    -> Maybe (Output, State)
ephemeralForAuthUpdate keys ready =
    case authStatus ready of
        GettingPowInfoA ->
            Nothing

        GeneratingStaticKeys powInfo ->
            let
                (rawSessKey, newDrg) = CryptoRand.randomBytesGenerate
                    sessionKeyLength
                    (drg ready)
                sessionKey = SessionKey $ Bl.fromStrict rawSessKey
            in
                case makeSignUp powInfo sessionKey (snd keys) of
                    Left err -> Just
                        ( ErrorO $
                            "couldn't generate sign in: " ++ err
                        , FailedS
                        )

                    Right signUp -> Just
                        ( BytesInQO toServerQ signUp
                        , ReadyS $ ready
                            { authStatus =
                                AwaitingUsername keys sessionKey
                            , drg = newDrg
                            }
                        )

        AwaitingUsername _ _ ->
            Nothing

        LoggedIn _ ->
            Nothing


fileExistenceUpdate :: FilePath -> Bool -> Init -> (Output, State)
fileExistenceUpdate path exists init_ =
    case init_ of
        EmptyI ->
            pass

        MakingRootDirI _ ->
            pass

        GettingTimesI _ ->
            pass

        GettingRandomGenI _ _ ->
            pass

        DoKeysExistI root time drg ->
            case (exists, path == keysPath root) of
                (True, True) ->
                    ( ReadFileO $ keysPath root
                    , InitS $ GettingKeysFromFileI root time drg
                    )

                (True, False) -> 
                    pass

                (False, True) ->
                    ( BytesInQO toServerQ $ Bl.singleton 0
                    , ReadyS $ Ready
                        { root
                        , blobsUp = NoJobs
                        , getBlob = NoJobs
                        , setMessage = NoJobs
                        , sendMessage = Nothing
                        , messageLocks = Set.empty
                        , drg
                        , time
                        , sendingEphemeral = False
                        , authStatus = GettingPowInfoA
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

        GeneratingStaticKeys _ ->
            startAuth

        AwaitingUsername _ _ ->
            startAuth

        LoggedIn (StaticKeys _ sessionKey userId) ->
            ( BytesInQO toServerQ $ logIn sessionKey userId
            , ReadyS ready
            )


logIn :: SessionKey -> UserId -> Bl.ByteString
logIn (SessionKey sessionKey) userId =
    mconcat
        [ Bl.singleton 1
        , sessionKey
        , encodeUserId userId
        ]


encodeUserId :: UserId -> Bl.ByteString
encodeUserId (UserId userId) =
    let
        unsized = encodeUint userId
        size = Bl.length unsized
    in
        Bl.singleton (fromIntegral size) <> unsized


encodeUint :: Integer -> Bl.ByteString
encodeUint i =
    encodeUintHelp i i 0 []


encodeUintHelp
    :: Integer
    -> Integer
    -> Int
    -> [Word8]
    -> Bl.ByteString
encodeUintHelp int remainder counter accum =
    if remainder == 0 then
        Bl.pack $ reverse accum
    else let
        word :: Integer
        word =
            (int `Bits.shiftR` (counter * 8)) Bits..&. 0xFF

        value :: Integer
        value = word `Bits.shiftL` (counter * 8)    
    in
        encodeUintHelp
            int
            (remainder - value)
            (counter + 1)
            (fromIntegral word : accum)
        


tcpMessageUpdate raw ready =
    case P.eitherResult $ P.parse tcpP raw of
        Left err ->
            ( ErrorO $ "could not parse TCP message: " ++ err
            , FailedS
            )

        Right GiveMeEphemeral ->
            ( GenerateDhPairO
            , ReadyS $ ready { sendingEphemeral = True }
            )

        Right (NewMessageT _ _ _) ->
            undefined

        Right (NewUserId _) ->
            undefined

        Right (PowInfoT _ _) ->
            undefined

        Right (Price _) ->
            undefined

        Right (Ephemeral key userId) ->
            ephemeralFromServerUpdate key userId ready

        Right (NoEphemeral _) ->
            undefined

        Right (BlobT _ _) ->
            undefined


ephemeralFromServerUpdate
    :: TheirEphemeral
    -> UserId
    -> Ready
    -> (Output, State)
ephemeralFromServerUpdate key ownerId ready =
    case sendMessage ready of
        Nothing ->
            pass

        Just send ->
            case status send of
                BareCloning ->
                    pass

                Reading ->
                    pass

                ToStaticKeyFromDb _ ->
                    pass

                ToEphemeralFromServer plain theirStatic ->
                    if ownerId == userId send then
                        ( GenerateDhPairO
                        , ReadyS $ ready
                            { sendMessage = Just $ send
                                { status =
                                    MakingMyEphemeral
                                        plain
                                        theirStatic
                                        key
                                }
                            }
                        )

                    else
                        pass

                MakingMyEphemeral _ _ _ ->
                    pass

                SendingPointer _ _ ->
                    pass

                SendingChunks _ _ _ ->
                    pass

                PuttingSentInDb ->
                    pass

  where
    pass = (DoNothingO, ReadyS ready)


randomGenUpdate
    :: CryptoRand.ChaChaDRG
    -> Init
    -> (Output, State)
randomGenUpdate gen init_ =
    case init_ of
        EmptyI ->
            pass

        MakingRootDirI _ ->
            pass

        GettingTimesI _ ->
            pass
        
        GettingRandomGenI root times ->
            ( DoesFileExistO $ keysPath root
            , InitS $ DoKeysExistI root times gen
            )

        DoKeysExistI _ _ _ ->
            pass

        GettingKeysFromFileI _ _ _ ->
            pass
        
  where
    pass = (DoNothingO, InitS init_)


dbPublicKeyUpdate :: [(Integer, B.ByteString)] -> Ready -> (Output, State)
dbPublicKeyUpdate rows ready =
    case sendMessage ready of
        Nothing ->
            pass

        Just send ->
            case status send of
                BareCloning ->
                    pass

                Reading ->
                    pass

                ToStaticKeyFromDb plain ->
                    dbPublicKeyHelp rows ready send plain

                ToEphemeralFromServer _ _ ->
                    pass

                MakingMyEphemeral _ _ _ ->
                    pass

                SendingPointer _ _ ->
                    pass

                SendingChunks _ _ _ ->
                    pass

                PuttingSentInDb ->
                    pass
  where
    pass = (DoNothingO, ReadyS ready)


toServerQ :: Q
toServerQ =
    Q.newTQueue


getEphemeral :: UserId -> Bl.ByteString
getEphemeral userId =
    Bl.singleton 8 <> encodeUserId userId


dbPublicKeyHelp
    :: [(Integer, B.ByteString)]
    -> Ready
    -> SendMessage
    -> PlainText
    -> (Output, State)
dbPublicKeyHelp rows ready send plain =
    case rows of
        [] ->
            ( ErrorO $ "no static key for " ++ show (userId send)
            , FailedS
            )

        [(rawUserId, rawKey)] ->
            if UserId rawUserId == userId send then
                ( BytesInQO toServerQ $ getEphemeral (userId send)
                , ReadyS $ ready
                    { sendMessage = Just $ send
                        { status = ToEphemeralFromServer
                            plain (TheirStatic rawKey)
                        }
                    }
                )

            else
                (DoNothingO, ReadyS ready)

        _ : _ ->
            ( ErrorO $
                "multiple static keys for " ++
                show (userId send)
            , FailedS
            )


newtype MyPublicEphemeral
    = MyPublicEphemeral B.ByteString



tcpP :: P.Parser TcpMsg
tcpP =
    P.choice
        [ do
            _ <- P.word8 0
            return GiveMeEphemeral
        , do
            _ <- P.word8 1
            myEphemeral <- fmap MyPublicEphemeral $ P.take 32
            handshake <- fmap Handshake $ P.take 96
            pointer <- fmap Pointer $ P.take 48
            return $ NewMessageT myEphemeral handshake pointer
        , do
            _ <- P.word8 2
            userId <- userIdP
            return $ NewUserId userId
        , do
            _ <- P.word8 3
            difficulty <- uint8P
            unique <- P.take 16
            return $ PowInfoT difficulty unique
        , do
            _ <- P.word8 4
            price <- uint32P
            return $ Price price
        , do
            _ <- P.word8 5
            key <- fmap TheirEphemeral $ P.take 32
            userId <- userIdP
            return $ Ephemeral key userId
        , do
            _ <- P.word8 6
            userId <- userIdP
            return $ NoEphemeral userId
        , do
            _ <- P.word8 7
            blobId <- fmap BlobId $ P.take blobIdLen
            blob <- fmap EncryptedBlob $ P.takeByteString
            return $ BlobT blobId blob
        ]


blobIdLen :: Int
blobIdLen =
    32


instance Show UserId where
    show (UserId uId) =
        "UserId: " ++ show uId


tarUpdate
    :: (FilePath, FilePath)
    -> [Tar.Entry]
    -> Ready
    -> (Output, State)
tarUpdate paths tar ready =
    case sendMessage ready of
        Nothing ->
            pass

        Just send ->
            case status send of
                BareCloning ->
                    pass

                Reading ->
                    if paths ==
                        ( tempPath $ root ready
                        , clonedFilePath $ messageId send) then

                        ( DbPublicKeyO
                            (dbPath $ root ready)
                            (dbGetPublicKey $ userId send)
                        , ReadyS $ ready
                            { sendMessage = Just $ send
                                { status = ToStaticKeyFromDb $
                                    PlainText $ Tar.write tar
                                }
                            }
                        )

                    else
                        pass

                ToStaticKeyFromDb _ ->
                    pass

                ToEphemeralFromServer _ _ ->
                    pass

                MakingMyEphemeral _ _ _ ->
                    pass

                SendingPointer _ _ ->
                    pass

                SendingChunks _ _ _ ->
                    pass

                PuttingSentInDb ->
                    pass

  where
    pass = (DoNothingO, ReadyS ready)

                    


panicOnStdErr :: StdErr -> Process.CreateProcess -> (Output, State) -> (Output, State)
panicOnStdErr (StdErr stdErr) process ok =
    case stdErr of
        Nothing ->
            ok

        Just err ->
            (ErrorO $ mconcat
                [ "failed external process:\n"
                , show process
                , ":\n"
                , err
                ]
            , FailedS
            )


setMessageCommandCatcher
    :: Process.CreateProcess
    -> StdOut
    -> Ready
    -> Maybe (Output, State)
setMessageCommandCatcher command _ ready =
    case setMessage ready of
        NoJobs ->
            Nothing

        Jobs (DoesItExist _ _ _ _) _ ->
            Nothing

        Jobs (Initializing mId sub@(Subject s) box meta) waiting ->
            if command == gitInit (root ready) mId then Just
                ( WriteFileO (subjectPath (root ready) mId) s
                , ReadyS $ ready
                    { setMessage =
                        Jobs
                            (SettingSubject mId sub box meta)
                            waiting
                    }
                )
            else
                Nothing

        Jobs (SettingSubject _ _ _ _) _ ->
            Nothing

        Jobs (SettingMainBox _ _ _) _ ->
            Nothing

        Jobs (SettingMetadata _ _) _ ->
            Nothing

        Jobs (GitAdding mId) _ ->
            if command == gitAdd (root ready) mId then Just
                ( RunCommandO $ gitCommit (root ready) mId
                , ReadyS $ ready
                    { setMessage =
                        promoteSetMessage $ setMessage ready
                    }
                )
            else
                Nothing

        Jobs (GitCommitting mId) _ ->
            if command == gitCommit (root ready) mId then Just
                ( DoNothingO
                , ReadyS $ ready
                    { setMessage =
                        promoteSetMessage $ setMessage ready
                    , messageLocks = Set.delete mId $ messageLocks ready
                    }
                )
            else
                Nothing


commandResultUpdate
    :: Process.CreateProcess
    -> StdOut
    -> StdErr
    -> Ready
    -> (Output, State)
commandResultUpdate cmd out err ready =
    panicOnStdErr err cmd $
    case setMessageCommandCatcher cmd out ready of
        Just used ->
            used

        Nothing ->
            case sendMessageCommandCatcher cmd out ready of
                Just used ->
                    used

                Nothing ->
                    (DoNothingO, ReadyS ready)


sendMessageCommandCatcher
    :: Process.CreateProcess
    -> StdOut
    -> Ready
    -> Maybe (Output, State)
sendMessageCommandCatcher command _ ready =
    case sendMessage ready of
        Nothing ->
            Nothing

        Just send ->
            sendMessageCatchHelp command send ready


dontIfLocked :: SendMessage -> Ready -> Maybe (Output, State) -> Maybe (Output, State)
dontIfLocked send ready ifNotLocked =
    if Set.member (messageId send) (messageLocks ready) then
        Nothing

    else
        ifNotLocked


sendMessageCatchHelp
    :: Process.CreateProcess
    -> SendMessage
    -> Ready
    -> Maybe (Output, State)
sendMessageCatchHelp command send ready =
    let
        clone = gitCloneBare (root ready) (messageId send)
    in dontIfLocked send ready $ case status send of
        BareCloning ->
            if command == clone then
                 Just
                    ( TarO
                        (tempPath $ root ready)
                        (clonedFilePath $ messageId send)
                    , ReadyS $ ready
                        { sendMessage =
                            Just $ send { status = Reading }
                        , messageLocks =
                            Set.delete
                                (messageId send)
                                (messageLocks ready)
                        }
                    )
            else
                Nothing

        Reading ->
            Nothing

        ToStaticKeyFromDb _ ->
            Nothing

        ToEphemeralFromServer _ _ ->
            Nothing

        MakingMyEphemeral _ _ _ ->
            Nothing

        SendingPointer _ _ ->
            Nothing

        SendingChunks _ _ _ ->
            Nothing

        PuttingSentInDb ->
            Nothing


promoteSetMessage
    :: Jobs SetMessage SetMessageWait
    -> Jobs SetMessage SetMessageWait
promoteSetMessage job =
    case job of
        NoJobs ->
            NoJobs

        Jobs _ [] ->
            NoJobs

        Jobs _ (SetMessageWait mId sub box meta:aiting) ->
            Jobs
                (DoesItExist mId sub box meta)
                aiting


gitCloneBare :: RootPath -> MessageId -> Process.CreateProcess
gitCloneBare root mId =
    (Process.proc "git"
        [ "clone"
        , "--bare"
        , "-o"
        , clonedFilePath mId
        , messagePath root mId
        ])
    { Process.cwd = Just $ tempPath root }


clonedFilePath :: MessageId -> FilePath
clonedFilePath mId =
    show mId <> "clone"


gitInit :: RootPath -> MessageId -> Process.CreateProcess
gitInit root mId =
        (Process.proc "git" ["init", show mId])
        { Process.cwd = Just $ messagesDirPath root }


gitCommit :: RootPath -> MessageId -> Process.CreateProcess
gitCommit root mId =
        (Process.proc "git" ["commit", "-m", "x"])
        {Process.cwd = Just $ messagePath root mId}


gitAdd :: RootPath -> MessageId -> Process.CreateProcess
gitAdd root mId =
        (Process.proc "git" ["add", "."])
        { Process.cwd = Just $ messagePath root mId }


subjectPath :: RootPath -> MessageId -> FilePath
subjectPath root mId =
    messagePath root mId </> "subject.txt"


dirExistsUpdate :: FilePath -> Bool -> Ready -> (Output, State)
dirExistsUpdate path exists ready =
    case setMessage ready of
        NoJobs ->
            pass

        Jobs (DoesItExist mId sub@(Subject s) box meta) waiting ->
            if exists && path == messagePath (root ready) mId then
                ( WriteFileO (subjectPath (root ready) mId) s
                , ReadyS $ ready
                    { setMessage =
                        Jobs
                            (SettingSubject mId sub box meta)
                            waiting
                    }
                )
            else
                ( RunCommandO $ gitInit (root ready) mId
                , ReadyS $ ready
                    { setMessage =
                        Jobs
                            (Initializing mId sub box meta)
                            waiting
                    }
                )

        Jobs (Initializing _ _ _ _) _ ->
            pass

        Jobs (SettingSubject _ _ _ _) _ ->
            pass

        Jobs (SettingMainBox _ _ _) _ ->
            pass

        Jobs (SettingMetadata _ _) _ ->
            pass

        Jobs (GitAdding _) _ ->
            pass

        Jobs (GitCommitting _) _ ->
            pass

  where
    pass = (DoNothingO, ReadyS ready)


newtype Handshake
    = Handshake B.ByteString


newtype Pointer
    = Pointer B.ByteString


newtype TheirEphemeral
    = TheirEphemeral Bl.ByteString


newtype EncryptedBlob
    = EncryptedBlob B.ByteString


newtype BlobId
    = BlobId B.ByteString


data TcpMsg
    = GiveMeEphemeral
    | NewMessageT MyPublicEphemeral Handshake Pointer
    | NewUserId UserId
    | PowInfoT Int B.ByteString
    | Price Int
    | Ephemeral TheirEphemeral UserId
    | NoEphemeral UserId
    | BlobT BlobId EncryptedBlob


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


parseApiInput :: Bl.ByteString -> Either String ApiInput
parseApiInput raw =
    P.eitherResult $ P.parse apiInputP raw


newtype UserId =
    UserId Integer


instance Eq UserId where
    (==) (UserId a) (UserId b) =
        a == b


userIdP :: P.Parser UserId
userIdP = do
    size <- P.anyWord8
    userId <- P.take $ fromIntegral size
    return $ UserId $ decodeInt userId


decodeInt :: B.ByteString -> Integer
decodeInt raw =
    decodeIntHelp (B.unpack raw) 0 0


-- Little Endian
decodeIntHelp :: [Word8] -> Integer -> Integer -> Integer
decodeIntHelp raw counter accum =
    case raw of
        [] ->
            accum

        r : aw ->
            decodeIntHelp
                aw
                (counter + 1)
                (accum + (fromIntegral r) * (256 ^ counter))


blobHashP :: P.Parser Hash32
blobHashP = do
    hash <- hash32P
    P.endOfInput
    return hash


hash32P :: P.Parser Hash32
hash32P = do
    hash <- P.take 32
    return $ Hash32 hash


data ApiInput
    = SetMessageA MessageId Subject MainBox Metadata
    | SendMessageA MessageId UserId Commit
    | AddToWhitelistA UserId
    | RemoveFromWhitelistA UserId
    | GetMessageA MessageId
    | GetWhitelistA
    | GetMyIdA
    | GetDraftsSummaryA
    | GetSentSummaryA
    | GetInboxSummaryA
    | GetMergeCandidatesA MessageId
    | MergeA MessageId MessageId
    | GetHistoryA MessageId
    | RevertA MessageId Commit
    | GetCommitA MessageId Commit
    | GetUniqueA


newtype Subject
    = Subject B.ByteString


newtype MainBox
    = MainBox B.ByteString


newtype Metadata
    = Metadata B.ByteString


newtype MessageId
    = MessageId Int


instance Ord MessageId where
    compare (MessageId a) (MessageId b) =
        compare a b


instance Eq MessageId where
    (==) (MessageId a) (MessageId b) =
        a == b


newtype Commit
    = Commit B.ByteString


stringP :: P.Parser B.ByteString
stringP = do
    size <- uint32P
    P.take size


commitP :: P.Parser Commit
commitP = do
    commit <- P.take 40
    return $ Commit commit


messageIdP :: P.Parser MessageId
messageIdP = do
    id_ <- uint32P
    return $ MessageId id_


apiInputP :: P.Parser ApiInput
apiInputP = do
    P.choice
        [ do
            _ <- P.word8 0
            messageId <- messageIdP
            subject <- fmap Subject stringP
            mainBox <- fmap MainBox stringP
            metadata <- fmap Metadata stringP
            return $ SetMessageA messageId subject mainBox metadata
        , do
            _ <- P.word8 1
            messageId <- messageIdP
            userId <- userIdP
            commit <- commitP
            return $ SendMessageA messageId userId commit
        , do
            _ <- P.word8 2
            userId <- userIdP
            return $ AddToWhitelistA userId
        , do
            _ <- P.word8 3
            userId <- userIdP
            return $ RemoveFromWhitelistA userId
        , do
            _ <- P.word8 4
            messageId <- messageIdP
            return $ GetMessageA messageId
        , do
            _ <- P.word8 5
            return GetWhitelistA
        , do
            _ <- P.word8 6
            return GetMyIdA
        , do
            _ <- P.word8 7
            return GetDraftsSummaryA
        , do
            _ <- P.word8 8
            return GetSentSummaryA
        , do
            _ <- P.word8 9
            return GetInboxSummaryA
        , do
            _ <- P.word8 10
            messageId <- messageIdP
            return $ GetMergeCandidatesA messageId
        , do
            _ <- P.word8 11
            from <- messageIdP
            to <- fmap MessageId uint32P
            return $ MergeA from to
        , do
            _ <- P.word8 12
            messageId <- messageIdP
            return $ GetHistoryA messageId
        , do
            _ <- P.word8 13
            messageId <- messageIdP
            commit <- commitP
            return $ RevertA messageId commit
        , do
            _ <- P.word8 14
            messageId <- messageIdP
            commit <- commitP
            return $ GetCommitA messageId commit
        , do
            _ <- P.word8 15
            return $ GetUniqueA
        ]


setMessageUpdate
    :: MessageId
    -> Subject
    -> MainBox
    -> Metadata
    -> Ready
    -> (Output, State)
setMessageUpdate messageId subject mainBox metadata ready =
    case setMessage ready of
        NoJobs ->
            if Set.member messageId (messageLocks ready) then
                (DoNothingO, ReadyS ready)
            else
                ( DoesDirExistO $ messagePath (root ready) messageId
                , ReadyS $ ready
                    { setMessage = Jobs
                        (DoesItExist messageId subject mainBox metadata)
                        []
                    , messageLocks = Set.insert
                        messageId
                        (messageLocks ready)
                    }
                )

        Jobs current waiting ->
            ( DoNothingO
            , ReadyS $ ready { setMessage =
                Jobs
                    current
                    (SetMessageWait
                        messageId
                        subject
                        mainBox
                        metadata
                            : waiting)
                }
            )


messagesDirPath :: RootPath -> FilePath
messagesDirPath (RootPath root) =
    root </> "messages"


messagePath :: RootPath -> MessageId -> FilePath
messagePath root (MessageId messageId) =
    messagesDirPath root </> show messageId


data SendingStatus
    = Sending
    | Waiting


instance ToField MessageId where
    toField (MessageId mId) =
        toField mId


instance ToField UserId where
    toField (UserId uId) =
        toField uId


instance ToField Commit where
    toField (Commit c) =
        toField c


instance ToField SendingStatus where
    toField =
        toField . sendingToInt


sendingToInt :: SendingStatus -> Int
sendingToInt s =
    case s of
        Sending ->
            0

        Waiting ->
            1


dbInsertSend :: MessageId -> UserId -> Commit -> Clock.UTCTime -> SendingStatus -> DbQuery
dbInsertSend mId uId commit t status =
    DbQuery "INSERT INTO sent (message_id, commit_hash, time, to, status) VALUES (?, ?, ?, ?, ?);" (mId, uId, commit, t, status)


sendMessageUpdate
    :: MessageId
    -> UserId
    -> Commit
    -> Ready
    -> (Output, State)
sendMessageUpdate messageId userId commit ready =
    let
        now = head $ time ready
        sending = dbInsertSend messageId userId commit now Sending
        waiting = dbInsertSend messageId userId commit now Waiting
        clone = gitCloneBare (root ready) messageId
        path = dbPath $ root ready
        justRecordIt = ( DbExecuteO path waiting, ReadyS ready)
        recordAndStart =
            ( BatchO
                [ DbExecuteO path sending
                , RunCommandO clone
                ]
            , ReadyS $ ready
                { sendMessage = Just $ SendMessage
                    { messageId
                    , userId
                    , commit
                    , status = BareCloning
                    }
                , messageLocks =
                    Set.insert messageId $ messageLocks ready
                }
            )
    in
        case sendMessage ready of
            Nothing ->
                if Set.member messageId (messageLocks ready) then
                    justRecordIt
                else
                    recordAndStart

            Just _ ->
                justRecordIt


uiApiUpdate :: ApiInput -> State -> (Output, State)
uiApiUpdate apiInput model =
    case apiInput of
        SetMessageA messageId subject mainBox metadata ->
            updateReady
                model
                (setMessageUpdate messageId subject mainBox metadata)

        SendMessageA messageId userId commit ->
            updateReady model $
                sendMessageUpdate messageId userId commit

        AddToWhitelistA _ ->
            undefined

        RemoveFromWhitelistA _ ->
            undefined

        GetMessageA _ ->
            undefined

        GetWhitelistA ->
            undefined

        GetMyIdA ->
            undefined

        GetDraftsSummaryA ->
            undefined

        GetSentSummaryA ->
            undefined

        GetInboxSummaryA ->
            undefined

        GetMergeCandidatesA _ ->
            undefined

        MergeA _ _ ->
            undefined

        GetHistoryA _ ->
            undefined

        RevertA _ _ ->
            undefined

        GetCommitA _ _ ->
            undefined

        GetUniqueA ->
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
                            { blobsUp = promoteBlobsUp $ blobsUp ready
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

        GettingTimesI _ ->
            pass

        GettingRandomGenI _ _ ->
            pass

        DoKeysExistI _ _ _ ->
            pass

        GettingKeysFromFileI root times gen ->
            if path == keysPath root then
                rawKeysUpdate contents root times gen
            else
                pass

  where
    pass = (DoNothingO, InitS init_)
        


errToText :: Either String a -> Either T.Text a
errToText e =
    case e of
        Left err ->
            Left $ T.pack err

        Right ok ->
            Right ok


parseKeys :: Bl.ByteString -> Either T.Text StaticKeys
parseKeys raw =
    errToText $ P.eitherResult $ P.parse myKeysP raw


uint16P :: P.Parser Int
uint16P = do
    b0 <- uint8P
    b1 <- uint8P
    return $ b0 + b1 * 256


parseLength :: Bl.ByteString -> Either String Int
parseLength bytes2 =
    P.eitherResult $ P.parse uint16P bytes2


uint32P :: P.Parser Int
uint32P = do
    b0 <- uint8P
    b1 <- uint8P
    b2 <- uint8P
    b3 <- uint8P
    return $ b0 + b1 * 256 + b2 * 256 * 256 + b3 * 256 * 256 * 256


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
    rawDhKey <- P.takeByteString
    case Dh.dhBytesToPair $ Noise.convert rawDhKey of
        Nothing ->
            fail "could not parse secret key from file"

        Just dhKeys ->
            return $
                StaticKeys
                    (MyStatic dhKeys)
                    (SessionKey $ Bl.fromStrict sessionKey)
                    userId


rawKeysUpdate
    :: Bl.ByteString
    -> RootPath
    -> [Clock.UTCTime]
    -> CryptoRand.ChaChaDRG
    -> (Output, State)
rawKeysUpdate rawKeys root time drg =
    case parseKeys rawKeys of
        Left err ->
            ( ErrorO $ T.unpack $
                "could not parse static keys: " <> err
            , FailedS
            )

        Right keys@(StaticKeys _ session uId) ->
            ( BatchO
                [ BytesInQO toWebsocketQ (Bl.singleton 11)
                , BytesInQO toServerQ $ logIn session uId
                ]
            , ReadyS $ Ready
                { root
                , blobsUp = NoJobs
                , getBlob = NoJobs
                , setMessage = NoJobs
                , sendMessage = Nothing
                , messageLocks = Set.empty
                , drg
                , time
                , sendingEphemeral = False
                , authStatus = LoggedIn keys
                }
            )


dbPath :: RootPath -> FilePath
dbPath (RootPath root) =
    root </> "database.sqlite"


setupDatabase :: RootPath -> Output
setupDatabase root =
    BatchO $ map (DbExecute_O (dbPath root))
        [ "CREATE TABLE IF NOT EXISTS sent (\n\
          \    message_id INTEGER NOT NULL,\n\
          \    time TEXT NOT NULL,\n\
          \    to INTEGER NOT NULL,\n\
          \    PRIMARY KEY (message_id, time, to)\n\
          \);"
        , "CREATE TABLE IF NOT EXISTS received (\n\
          \    message_id BLOB NOT NULL,\n\
          \    time TEXT NOT NULL,\n\
          \    from INTEGER NOT NULL,\n\
          \    PRIMARY KEY (message_id, time, from)\n\
          \);"
        , "CREATE TABLE IF NOT EXISTS fingerprints (\n\
          \    username INTEGER NOT NULL PRIMARY KEY,\n\
          \    fingerprint INTEGER NOT NULL,\n\
          \);"
        , "CREATE TABLE IF NOT EXISTS my_ephemeral_keys (\n\
          \    public BLOB NOT NULL PRIMARY KEY,\n\
          \    secret BLOB NOT NULL UNIQUE,\n\
          \);"
        ]


dirCreatedUpdate :: FilePath -> Init -> (Output, State)
dirCreatedUpdate path initModel =
    case initModel of
        EmptyI ->
            pass

        GettingTimesI _ ->
            pass

        MakingRootDirI rootPath@(RootPath r) ->
            if path == r then
                ( BatchO
                    [ setupDatabase rootPath
                    , StartUiServerO
                    , StartUiO
                    , GetTimesO
                    ]
                , InitS $ GettingTimesI $ RootPath path
                )
            else
                pass

        GettingRandomGenI _ _ ->
            pass

        DoKeysExistI _ _ _ ->
            pass

        GettingKeysFromFileI _ _ _ ->
            pass

  where
    pass = (DoNothingO, InitS initModel)

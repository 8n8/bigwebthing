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
import qualified Crypto.Noise.DH as Dh
import qualified Crypto.Noise as Noise
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified System.Directory as Dir
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
import qualified Codec.Archive.Tar as Tar
import qualified Network.Simple.TCP as Tcp
import qualified Data.Bits as Bits
import qualified Data.Map as Map


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

        TarO base dirToTar -> do
            tarred <- Tar.pack base [dirToTar]
            return $ Just $ TarredM (base, dirToTar) tarred

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


data Output
    = GetAppDataDirO
    | MakeDirIfMissingO FilePath
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
    | TarredM (FilePath, FilePath) [Tar.Entry]
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
    | DoKeysExistI RootPath
    | GettingKeysFromFileI RootPath


data Ready = Ready
    { root :: RootPath
    , blobsUp :: Jobs BlobUp BlobUpWait
    , getBlob :: Jobs GetBlob GetBlobWait
    , setMessage :: Jobs SetMessage SetMessageWait
    , messageLocks :: Set.Set MessageId
    , sendingEphemeral :: Bool
    , authStatus :: AuthStatus
    , handshakes :: Map.Map HandshakeId Handshake
    }


data HandshakeId
    = HandshakeId UserId PublicEphemeral


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
    | Responder ResponderHandshake


data InitiatorHandshake
    = SentPlainE
    | ReceivedEncryptedE EncryptedEphemeral


data ResponderHandshake
    = ReceivedPlainE
    | SentEncryptedE EncryptedEphemeral


newtype EncryptedEphemeral
    = EncryptedEphemeral Bl.ByteString


data PowInfo
    = PowInfo Word8 Bl.ByteString


data AuthStatus
    = GettingPowInfoA
    | GeneratingStaticKeys PowInfo
    | AwaitingUsername (Dh.KeyPair Curve25519) SessionKey
    | LoggedIn StaticKeys


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

        TcpMsgM raw ->
            updateReady model $ tcpMessageUpdate raw

        RestartingTcpM ->
            updateReady model restartingTcpUpdate

        FileExistenceM path exists ->
            updateInit model $ fileExistenceUpdate path exists

        NewDhKeysM _ ->
            undefined


newtype MyStatic
    = MyStatic (Dh.KeyPair Curve25519)


fileExistenceUpdate :: FilePath -> Bool -> Init -> (Output, State)
fileExistenceUpdate path exists init_ =
    case init_ of
        EmptyI ->
            pass

        MakingRootDirI _ ->
            pass

        DoKeysExistI root ->
            case (exists, path == keysPath root) of
                (True, True) ->
                    ( ReadFileO $ keysPath root
                    , InitS $ GettingKeysFromFileI root
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
                        , messageLocks = Set.empty
                        , sendingEphemeral = False
                        , authStatus = GettingPowInfoA
                        , handshakes = Map.empty
                        }
                    )

                (False, False) ->
                    pass

        GettingKeysFromFileI _ ->
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


tcpMessageUpdate :: Bl.ByteString -> Ready -> (Output, State)
tcpMessageUpdate raw _ =
    case P.eitherResult $ P.parse tcpP raw of
        Left err ->
            ( ErrorO $ "could not parse TCP message: " ++ err
            , FailedS
            )

        Right (NewMessageT _ _) ->
            undefined

        Right (NewUserId _) ->
            undefined

        Right (PowInfoT _ _) ->
            undefined

        Right (Price _) ->
            undefined

        Right (TheirStaticKeyT _ _) ->
            undefined


toServerQ :: Q
toServerQ =
    Q.newTQueue


messageInLength =
    15991


tcpP :: P.Parser TcpMsg
tcpP =
    P.choice
        [ do
            _ <- P.word8 0
            userId <- userIdP
            messageIn <- fmap
                (MessageIn . Bl.fromStrict)
                (P.take messageInLength)
            P.endOfInput
            return $ NewMessageT userId messageIn
        , do
            _ <- P.word8 1
            userId <- userIdP
            return $ NewUserId userId
        , do
            _ <- P.word8 2
            difficulty <- uint8P
            unique <- P.take 16
            return $ PowInfoT
                (fromIntegral difficulty)
                (Bl.fromStrict unique)
        , do
            _ <- P.word8 3
            price <- uint32P
            return $ Price price
        , do
            _ <- P.word8 4
            owner <- userIdP
            key <- fmap (TheirStaticKey . Bl.fromStrict) $ P.take 32
            return $ TheirStaticKeyT owner key
        ]


instance Show UserId where
    show (UserId uId) =
        "UserId: " ++ show uId


tarUpdate
    :: (FilePath, FilePath)
    -> [Tar.Entry]
    -> Ready
    -> (Output, State)
tarUpdate _ _ _ =
    undefined


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
            (DoNothingO, ReadyS ready)


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


data TcpMsg
    = NewMessageT UserId MessageIn
    | NewUserId UserId
    | PowInfoT Word8 Bl.ByteString
    | Price Int
    | TheirStaticKeyT UserId TheirStaticKey


newtype TheirStaticKey
    = TheirStaticKey Bl.ByteString


newtype MessageIn
    = MessageIn Bl.ByteString


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


instance Ord UserId where
    compare (UserId u1) (UserId u2) =
        compare u1 u2


instance Eq UserId where
    (==) (UserId a) (UserId b) =
        a == b


userIdP :: P.Parser UserId
userIdP =
    fmap UserId uint64P


blobHashP :: P.Parser Hash32
blobHashP = do
    hash <- hash32P
    P.endOfInput
    return hash


hash32P :: P.Parser Hash32
hash32P = do
    hash <- P.take 32
    return $ Hash32 hash


data NewMessage
    = NewMessage
        { messageId :: Int
        , subject :: Bl.ByteString
        , mainBox :: Bl.ByteString
        , blobs :: Bl.ByteString
        , wasm :: Bl.ByteString
        , members :: Bl.ByteString
        }


data ApiInput
    = SetMessageA NewMessage
    | AddToWhitelistA UserId
    | RemoveFromWhitelistA UserId
    | GetMessageA MessageId
    | GetWhitelistA
    | GetMyIdA
    | GetHistoryA MessageId
    | RevertA MessageId Commit
    | GetCommitA MessageId Commit
    | GetUniqueA
    | GetPaymentsA
    | GetPriceA
    | GetMessageSummariesA
    | GetMembershipA


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


uiApiUpdate :: ApiInput -> State -> (Output, State)
uiApiUpdate apiInput model =
    case apiInput of
        SetMessageA _ ->
            undefined

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

        DoKeysExistI _ ->
            pass

        GettingKeysFromFileI root ->
            if path == keysPath root then
                rawKeysUpdate contents root
            else
                pass

  where
    pass = (DoNothingO, InitS init_)


type Handshakes
    = Map.Map HandshakeId Handshake


parseCrypto :: Bl.ByteString -> Either String (StaticKeys, Handshakes)
parseCrypto raw =
    P.eitherResult $ P.parse cryptoP raw


cryptoP :: P.Parser (StaticKeys, Handshakes)
cryptoP = do
    keys <- myKeysP
    handshakes <- handshakesP
    return (keys, handshakes)


handshakesP :: P.Parser (Map.Map HandshakeId Handshake)
handshakesP = do
    asList <- handshakesHelpP []
    return $ Map.fromList asList


handshakesHelpP
    :: [(HandshakeId, Handshake)]
    -> P.Parser [(HandshakeId, Handshake)]
handshakesHelpP accum =
    P.choice
        [ do
            handshake <- handshakeP
            handshakesHelpP (handshake : accum)
        , do
            P.endOfInput
            return accum
        ]


handshakeP :: P.Parser (HandshakeId, Handshake)
handshakeP = do
    handshakeId <- handshakeIdP
    handshake <- P.choice
        [ fmap Initiator initiatorP
        , fmap Responder responderP
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


responderP :: P.Parser ResponderHandshake
responderP = do
    _ <- P.word8 1
    P.choice
        [ do
            _ <- P.word8 0
            return ReceivedPlainE
        , do
            _ <- P.word8 1
            ephemeral <- encryptedEphemeralP
            return $ SentEncryptedE ephemeral
        ]


encryptedEphemeralP :: P.Parser EncryptedEphemeral
encryptedEphemeralP =
    fmap EncryptedEphemeral $ fmap Bl.fromStrict $ P.take $ 16 + 32 + 16


handshakeIdP :: P.Parser HandshakeId
handshakeIdP = do
    userId <- userIdP
    publicEphemeral <- fmap PublicEphemeral $
        fmap Bl.fromStrict $ P.take 32
    return $ HandshakeId userId publicEphemeral


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


uint64P :: P.Parser Integer
uint64P = do
    bytes <- mapM (\_ -> P.anyWord8) (take 8 ([1..] :: [Int]))
    let powered = map uint64Help (zip [0..] bytes)
    return $ sum powered


uint64Help :: (Int, Word8) -> Integer
uint64Help (i, w) =
    (fromIntegral w) * (256 ^ i)


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
    -> (Output, State)
rawKeysUpdate rawCrypto root =
    case parseCrypto rawCrypto of
        Left err ->
            ( ErrorO $ "could not parse static keys: " <> err
            , FailedS
            )

        Right (keys@(StaticKeys _ session uId), handshakes) ->
            ( BatchO
                [ BytesInQO toWebsocketQ (Bl.singleton 11)
                , BytesInQO toServerQ $ logIn session uId
                ]
            , ReadyS $ Ready
                { root
                , blobsUp = NoJobs
                , getBlob = NoJobs
                , setMessage = NoJobs
                , messageLocks = Set.empty
                , sendingEphemeral = False
                , authStatus = LoggedIn keys
                , handshakes = handshakes
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
                    , DoesFileExistO $ keysPath root
                    ]
                , InitS $ DoKeysExistI root
                )
            else
                pass

        DoKeysExistI _ ->
            pass

        GettingKeysFromFileI _ ->
            pass

  where
    pass = (DoNothingO, InitS initModel)

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
import qualified Crypto.Noise.DH.Curve25519 as Curve
import qualified Crypto.Noise.DH as Dh
import qualified Crypto.Noise as Noise
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified System.Directory as Dir
import qualified Database.SQLite.Simple as Sql
import qualified Data.Text as T
import System.FilePath ((</>))
import qualified Data.Attoparsec.ByteString as P
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
import Control.Concurrent (forkIO)
import qualified System.Process as Process


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
            bytes <- B.readFile path
            return $ Just $ FileContentsM path bytes

        DbExecute_O path query -> do
            Sql.withConnection path $ \conn ->
                Sql.execute_ conn query
            return Nothing

        DbExecuteO path query params -> do
            Sql.withConnection path $ \conn ->
                Sql.execute conn query params
            return Nothing

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


data Output where
    GetAppDataDirO :: Output
    MakeDirIfMissingO :: FilePath -> Output
    DoNothingO :: Output
    ReadFileO :: FilePath -> Output
    DbExecute_O :: FilePath -> Sql.Query -> Output
    DbExecuteO :: Sql.ToRow q => FilePath -> Sql.Query -> q -> Output
    BatchO :: [Output] -> Output
    StartUiO :: Output
    StartUiServerO :: Output
    ErrorO :: String -> Output
    GetTmpFileHandleO :: FilePath -> Output
    WriteToHandleO :: Io.Handle -> Bl.ByteString -> FilePath -> Output
    MoveFileO :: FilePath -> FilePath -> Output
    BytesInQO :: Q -> Bl.ByteString -> Output
    CloseFileO :: Io.Handle -> Output
    ReadFileLazyO :: FilePath -> Output
    DoesDirExistO :: FilePath -> Output
    RunCommandO :: Process.CreateProcess -> Output
    WriteFileO :: FilePath -> B.ByteString -> Output


data Msg
    = StartM
    | AppDataDirM FilePath
    | DirCreatedIfMissingM FilePath
    | FileContentsM FilePath B.ByteString
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
    = SessionKey B.ByteString

newtype RootPath
    = RootPath FilePath

data StaticKeys
    = StaticKeys
        (Dh.KeyPair Curve.Curve25519)
        SessionKey
        UserId


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
    | NoKeysI RootPath


data Ready = Ready
    { root :: RootPath
    , keys :: StaticKeys
    , blobsUp :: Jobs BlobUp BlobUpWait
    , getBlob :: Jobs GetBlob GetBlobWait
    , setMessage :: Jobs SetMessage SetMessageWait
    }


data SetMessageWait
    = SetMessageWait MessageId Subject MainBox Metadata


data SetMessage
    = DoesItExist MessageId Subject MainBox Metadata
    | Initializing MessageId Subject MainBox Metadata
    | SettingSubject MessageId Subject MainBox Metadata
    | SettingMainBox MessageId MainBox Metadata
    | SettingMetadata MessageId Metadata
    | GitAdding MessageId


data Jobs a b
    = Jobs a [b]
    | NoJobs


data GetBlobWait
    = GetBlobWait Bl.ByteString Q


data GetBlob
    = GetBlob Q Hash32


type Q = Stm.STM (Q.TQueue Bl.ByteString)


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
            (GetAppDataDirO, InitS EmptyI)

        LazyFileContentsM path bytes ->
            updateReady model $ updateOnLazyFileContents path bytes

        AppDataDirM path ->
            ( MakeDirIfMissingO $ makeRootPath path
            , InitS $ MakingRootDirI $ RootPath $ makeRootPath path
            )

        DirCreatedIfMissingM path ->
            updateInit model $ dirCreatedUpdate path

        FileContentsM path contents ->
            fileContentsUpdate path contents model

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


commandResultUpdate
    :: Process.CreateProcess
    -> StdOut
    -> StdErr
    -> Ready
    -> (Output, State)
commandResultUpdate command _ stdErr ready =
    panicOnStdErr stdErr command $
    case setMessage ready of
        NoJobs ->
            pass

        Jobs (DoesItExist _ _ _ _) _ ->
            pass

        Jobs (Initializing mId sub@(Subject s) box meta) waiting ->
            if command == gitInit (root ready) mId then
                ( WriteFileO (subjectPath (root ready) mId) s
                , ReadyS $ ready
                    { setMessage =
                        Jobs
                            (SettingSubject mId sub box meta)
                            waiting
                    }
                )
            else
                pass

        Jobs (SettingSubject _ _ _ _) _ ->
            pass

        Jobs (SettingMainBox _ _ _) _ ->
            pass

        Jobs (SettingMetadata _ _) _ ->
            pass

        Jobs (GitAdding mId) _ ->
            if command == gitAdd (root ready) mId then
                ( RunCommandO $ gitCommit (root ready) mId
                , ReadyS $ ready
                    { setMessage =
                        promoteSetMessage $ setMessage ready
                    }
                )
            else
                pass

  where
    pass = (DoNothingO, ReadyS ready)


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

  where
    pass = (DoNothingO, ReadyS ready)




onGetBlobUpdate :: Bl.ByteString -> Q -> Ready -> (Output, State)
onGetBlobUpdate body q ready =
    case P.eitherResult $ P.parse blobHashP $ Bl.toStrict body of
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
    P.eitherResult $ P.parse apiInputP $ Bl.toStrict raw


newtype UserId =
    UserId Integer


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
    | SendMessageA MessageId UserId
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
    | RevertA MessageId CommitHash
    | GetCommitA MessageId CommitHash
    | GetUniqueA


newtype Subject
    = Subject B.ByteString


newtype MainBox
    = MainBox B.ByteString


newtype Metadata
    = Metadata B.ByteString


newtype MessageId
    = MessageId Int


newtype CommitHash
    = CommitHash B.ByteString


stringP :: P.Parser B.ByteString
stringP = do
    size <- uint32P
    P.take size


commitP :: P.Parser CommitHash
commitP = do
    commit <- P.take 40
    return $ CommitHash commit


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
            return $ SendMessageA messageId userId
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
            ( DoesDirExistO $ messagePath (root ready) messageId
            , ReadyS $ ready { setMessage =
                Jobs
                    (DoesItExist messageId subject mainBox metadata)
                    []
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


uiApiUpdate :: ApiInput -> State -> (Output, State)
uiApiUpdate apiInput model =
    case apiInput of
        SetMessageA messageId subject mainBox metadata ->
            updateReady
                model
                (setMessageUpdate messageId subject mainBox metadata)

        SendMessageA _ _ ->
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


fileContentsUpdate :: FilePath -> B.ByteString -> State -> (Output, State)
fileContentsUpdate path contents model =
    case model of
        ReadyS _ ->
            (DoNothingO, model)

        FailedS ->
            (DoNothingO, model)

        InitS EmptyI ->
            (DoNothingO, model)

        InitS (MakingRootDirI _) ->
            (DoNothingO, model)

        InitS (NoKeysI root) ->
            if path == keysPath root then
                rawKeysUpdate contents root
            else
                (DoNothingO, model)


errToText :: Either String a -> Either T.Text a
errToText e =
    case e of
        Left err ->
            Left $ T.pack err

        Right ok ->
            Right ok


parseKeys :: B.ByteString -> Either T.Text StaticKeys
parseKeys raw =
    errToText $ P.eitherResult $ P.parse myKeysP raw


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
            return $ StaticKeys dhKeys (SessionKey sessionKey) userId


rawKeysUpdate :: B.ByteString -> RootPath ->(Output, State)
rawKeysUpdate rawKeys root =
    case parseKeys rawKeys of
        Left err ->
            ( ErrorO $ T.unpack $
                "could not parse static keys: " <> err
            , FailedS
            )

        Right keys ->
            ( BytesInQO toWebsocketQ (Bl.singleton 11)
            , ReadyS $ Ready
                { root
                , keys
                , blobsUp = NoJobs
                , getBlob = NoJobs
                , setMessage = NoJobs
                }
            )


setupDatabase :: RootPath -> Output
setupDatabase (RootPath rootPath) =
    BatchO $ map (DbExecute_O rootPath)
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

        MakingRootDirI rootPath@(RootPath r) ->
            if path == r then
                ( BatchO
                    [ setupDatabase rootPath
                    , ReadFileO $ keysPath rootPath
                    , StartUiServerO
                    , StartUiO
                    ]
                , InitS $ NoKeysI $ RootPath path
                )
            else
                pass

        NoKeysI _ ->
            pass
  where
    pass = (DoNothingO, InitS initModel)

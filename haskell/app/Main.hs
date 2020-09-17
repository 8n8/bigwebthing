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
import qualified Network.Simple.TCP as Tcp
import qualified Data.Map as Map
import Control.Exception.Base (SomeException)
import qualified Data.Time.Clock as Clock
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import System.Entropy (getEntropy)
import qualified Data.Bits as Bits
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Crypto.Random as CryptoRand
import qualified Database.SQLite.Simple as Sql
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as Ba
import qualified Data.Set as Set


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
    GetRandomGenO -> do
        drg <- CryptoRand.drgNew
        return $ Just $ RandomGenM drg

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

    AppendFileStrictO path toAppend -> do
        _ <- B.appendFile path toAppend
        return $ Just AppendedStrictM

    MakeSessionKeyO -> do
        key <- getEntropy sessionKeyLength
        return $ Just $ NewSessionKeyM key

    ReadMessageO dbPath -> do
        result <- Sql.withConnection
            dbPath
            (\conn -> Sql.query_ conn "SELECT * FROM edits")
        return $ Just $ MessageFromDb dbPath result

    ReadFileStrictO path -> do
        contents <- B.readFile path
        return $ Just $ StrictFileContentsM path contents

    DbInsertDiffO path newRow -> do
        Sql.withConnection path (\conn ->
            Sql.execute conn insertDiff newRow)
        return Nothing


insertDiff =
    "INSERT INTO edits (start, end, insert, integrity) \
    \VALUES (?, ?, ?, ?);"


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
        Nothing ->
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
                        Nothing ->
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
    | GetRandomGenO
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
    | AppendFileStrictO FilePath B.ByteString
    | ReadMessageO FilePath
    | ReadFileStrictO FilePath
    | DbInsertDiffO FilePath (Int, Int, B.ByteString, B.ByteString)


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
    | StrictFileContentsM FilePath B.ByteString
    | FromServerM Bl.ByteString
    | RestartingTcpM
    | FileExistenceM FilePath Bool
    | NewDhKeysM [Dh.KeyPair Curve25519]
    | TimesM [Clock.UTCTime]
    | NewSessionKeyM B.ByteString
    | RandomGenM CryptoRand.ChaChaDRG
    | AppendedStrictM
    | MessageFromDb FilePath [MessageDbRow]


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
        , username :: Username
        , fingerprint :: Fingerprint
        }


newtype Hash32
    = Hash32 Bl.ByteString
    deriving (Eq, Ord, Show)


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
    | GettingRandomGen
        RootPath
        [Dh.KeyPair Curve25519]
        [Clock.UTCTime]
    | DoKeysExistI
        RootPath
        [Dh.KeyPair Curve25519]
        [Clock.UTCTime]
        CryptoRand.ChaChaDRG
    | GettingKeysFromFileI
        RootPath
        [Dh.KeyPair Curve25519]
        [Clock.UTCTime]
        CryptoRand.ChaChaDRG


data Header
    = Header
        { time :: PosixMillis
        , shares :: [Username]
        , subject :: T.Text
        , mainBox :: T.Text
        , blobs :: [Blob]
        , wasm :: Hash32
        }


data Ready =
    Ready
        { root :: RootPath
        , blobsUp :: Jobs BlobUp BlobUpWait
        , getBlob :: Jobs GetBlob GetBlobWait
        , authStatus :: AuthStatus
        , handshakes :: Map.Map HandshakeId Handshake
        , newDhKeys :: [Dh.KeyPair Curve25519]
        , theTime :: [Clock.UTCTime]
        , whitelist ::
            Map.Map Username (Fingerprint, Maybe TheirStatic)
        , waitForAcknowledge :: [AcknowledgementCode]
        , summaries :: Map.Map MessageId Summary
        , randomGen :: CryptoRand.ChaChaDRG
        , counter :: Int
        , assemblingFile :: Maybe AssemblingFile
        , gettingMessage :: Maybe Hash32
        , extractingMessage :: Maybe (MessageId, Username, Header)
        , extractingReferences :: Maybe (MessageId, Hash32)
        , readingToSendToServer :: Maybe AcknowledgementCode
        , page :: Page
        }


data Page
    = Inbox
    deriving Eq


data AssemblingFile
    = Assembling TmpBase Blake.BLAKE2bState
    | AddingFinal TmpBase Hash32
    | Moving TmpBase Hash32


type TmpBase
    = Int


data Summary
    = Summary
        { subjectS :: T.Text
        , timeS :: PosixMillis
        , authorS :: Username
        }


summarize :: Username -> Header -> Summary
summarize from header =
    Summary
        { subjectS = subject header
        , timeS = time header
        , authorS = from
        }


newtype EditId
    = EditId Int
    deriving (Eq, Ord)


data ShareName
    = ShareName MessageId Username
    deriving (Eq, Ord)


data HandshakeId
    = HandshakeId Username FirstMessage


newtype FirstMessage
    = FirstMessage Bl.ByteString


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


instance Eq FirstMessage where
    (==) (FirstMessage p1) (FirstMessage p2) =
        p1 == p2


instance Eq HandshakeId where
    (==) (HandshakeId u1 p1) (HandshakeId u2 p2) =
        u1 == u2 && p1 == p2


instance Ord FirstMessage where
    compare (FirstMessage a) (FirstMessage b) =
        compare a b


data Handshake
    = Initiator InitiatorHandshake
    | ResponderSentEncryptedES MyEphemeral


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
            case parseFromFrontend bin of
            Left err ->
                ( ErrorO $
                    "couldn't parse API input: " <> err
                , FailedS
                )

            Right apiInput ->
                updateReady model $ uiApiUpdate apiInput

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

    RandomGenM gen ->
        updateInit model $ updateOnRandomGen gen

    AppendedStrictM ->
        updateReady model $ fileAssembledUpdate

    MessageFromDb path rows ->
        updateReady model $ messageFromDbUpdate path rows

    StrictFileContentsM path contents ->
        updateReady model $ strictFileContentsUpdate path contents


strictFileContentsUpdate
    :: FilePath
    -> B.ByteString
    -> Ready
    -> (Output, State)
strictFileContentsUpdate path contents ready =
    case readingToSendToServer ready of
    Nothing ->
        (DoNothingO, ReadyS ready)

    Just code ->
        if path == makeChunkPath (root ready) code then
        let
        newReady = ready { readingToSendToServer = Nothing }
        in
        ( BytesInQO toServerQ $ Bl.fromStrict contents
        , ReadyS newReady
        )
        else
        (DoNothingO, ReadyS ready)


type MessageDbRow
    = (Int, Int, Int, B.ByteString, B.ByteString)


messageFromDbUpdate
    :: FilePath
    -> [MessageDbRow]
    -> Ready
    -> (Output, State)
messageFromDbUpdate path rows ready =
    case extractingMessageHelp path rows ready of
    Nothing ->
        case extractingReferencesHelp path rows ready of
        Nothing ->
            (DoNothingO, ReadyS ready)

        Just done ->
            done

    Just done ->
        done


extractingReferencesHelp path rows ready =
    case extractingReferences ready of
    Nothing ->
        Nothing

    Just (messageId, hash) ->
        if path == makeMessagePath (root ready) messageId then
        case constructReferences rows of
        Nothing ->
            Just
                ( ErrorO $ "corruped message " ++ show messageId
                , FailedS
                )

        Just refs ->
            if Set.member hash refs then
            let
            move =
                MoveFileO
                    (downloadPath (root ready) (counter ready))
                    (makeBlobPath (root ready) hash)
            newReady = ready { counter = counter ready + 1 }
            in
            Just
                ( BatchO [move, dumpCache newReady]
                , ReadyS newReady
                )

            else
            Nothing

        else
        Nothing


downloadPath :: RootPath -> Int -> FilePath
downloadPath root unique =
    tempPath root </> show unique


constructReferences :: [MessageDbRow] -> Maybe (Set.Set Hash32)
constructReferences rows =
    constructReferencesHelp Set.empty "" rows


constructReferencesHelp
    :: Set.Set Hash32
    -> B.ByteString
    -> [MessageDbRow]
    -> Maybe (Set.Set Hash32)
constructReferencesHelp accum lastEncoded remaining =
    case remaining of
    [] ->
        Just accum

    r@(_, _, _, _, integrity):emaining ->
        let
        encoded = constructMessageHelp r lastEncoded
        actualHash = hash8 encoded
        parsed = P.parse headerP $ Bl.fromStrict encoded
        in
        if actualHash == Hash8 integrity then
        case P.eitherResult parsed of
        Left _ ->
            Nothing

        Right header ->
            let
            refs = Set.fromList $ map hashB $ blobs header
            allRefs = Set.union refs accum
            in
            constructReferencesHelp allRefs encoded emaining
        else
        Nothing


extractingMessageHelp path rows ready =
    case extractingMessage ready of
    Just (messageId, from, header) ->
        if path == makeMessagePath (root ready) messageId then
        case constructMessage rows of
        Nothing ->
            Just
                ( ErrorO $ "corrupted message " ++ show messageId
                , FailedS
                )

        Just oldEncoded ->
            let
            diff = makeDiff oldEncoded (encodeHeader header)
            summary = summarize from header
            saveDiff =
                DbInsertDiffO
                    (makeMessagePath (root ready) messageId)
                    (diffToTuple diff)
            newReady =
                ready
                    { summaries =
                        Map.insert messageId summary (summaries ready)
                    }
            in
            Just
                ( BatchO [saveDiff, dumpCache newReady]
                , ReadyS newReady
                )
            else
            Nothing

    Nothing ->
        Nothing


constructMessage :: [MessageDbRow] -> Maybe B.ByteString
constructMessage rows =
    let
    candidate = foldr constructMessageHelp "" rows
    actualHash = hash8 candidate
    in
    case getLastHash rows of
    Nothing ->
        Nothing

    Just expectedHash ->
        if actualHash == expectedHash then
            Just candidate

        else
            Nothing


getLastHash :: [MessageDbRow] -> Maybe Hash8
getLastHash rows =
    case reverse rows of
        [] ->
            Nothing

        (_, _, _, _, hash):_ ->
            Just $ Hash8 hash


constructMessageHelp :: MessageDbRow -> B.ByteString -> B.ByteString
constructMessageHelp (_, start, end, insert, _) old =
    let
    firstBit = B.take start old
    lastBit = B.reverse $ B.take end $ B.reverse old
    in
    firstBit <> insert <> lastBit


makeMessagePath :: RootPath -> MessageId -> FilePath
makeMessagePath root (MessageId messageId) =
    makeMessagesPath root </> show messageId


makeMessagesPath :: RootPath -> FilePath
makeMessagesPath (RootPath root) =
    root </> "messages"


fileAssembledUpdate :: Ready -> (Output, State)
fileAssembledUpdate ready =
    case assemblingFile ready of
    Nothing ->
        (DoNothingO, ReadyS ready)

    Just (Assembling pathBase hashState) ->
        let
        hash =
            Hash32 $ Bl.fromStrict $
            Blake.finalize 32 hashState
        newReady =
            ready
                { assemblingFile =
                    Just $ Moving pathBase hash
                }
        finalPath = makeBlobPath (root ready) hash
        in
        ( MoveFileO
            (tempFile (root ready) pathBase)
            finalPath
        , ReadyS newReady
        )

    Just (AddingFinal _ _) ->
        (DoNothingO, ReadyS ready)

    Just (Moving _ _) ->
        (DoNothingO, ReadyS ready)


updateOnRandomGen :: CryptoRand.ChaChaDRG -> Init -> (Output, State)
updateOnRandomGen gen init_ =
    let
    pass = (DoNothingO, InitS init_)
    in
    case init_ of
    EmptyI ->
        pass

    MakingRootDirI _ ->
        pass

    MakingDhKeysI _ ->
        pass

    GettingTimes _ _ ->
        pass

    GettingRandomGen root dhKeys times ->
        ( DoesFileExistO $ keysPath root
        , InitS $ DoKeysExistI root dhKeys times gen
        )

    DoKeysExistI _ _ _ _ ->
        pass

    GettingKeysFromFileI _ _ _ _ ->
        pass


updateOnNewSessionKey :: B.ByteString -> Ready -> (Output, State)
updateOnNewSessionKey raw ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case authStatus ready of
    GettingPowInfoA ->
        pass

    GeneratingSessionKey difficulty unique ->
        signUpHelp difficulty unique raw ready

    AwaitingUsername _ _ ->
        pass

    LoggedIn _ ->
        pass


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
            { authStatus = AwaitingUsername myStatic session
            , newDhKeys = keys
            }
        eitherSignUp =
            makeSignUp difficulty unique session myStatic
        in
        case eitherSignUp of
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


fingerprintSalt =
    B.pack
        [121, 42, 53, 200, 120, 148, 151, 77, 190, 181, 194, 24, 139, 196, 215, 179]


fingerprintLength =
    8


makeFingerprint :: Username -> Dh.PublicKey Curve25519 -> Either String Fingerprint
makeFingerprint (Username usernameLazy) key =
    let
    asBytes = Noise.convert $ Dh.dhPubToBytes key
    username = Bl.toStrict usernameLazy
    hashE = Argon2.hash
        argonFingerprint
        (asBytes <> username)
        fingerprintSalt
        fingerprintLength
    in
    case hashE of
    CryptoPassed hash ->
        Right $ Fingerprint $ Bl.fromStrict hash

    CryptoFailed err ->
        Left $ show err


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


argonFingerprint :: Argon2.Options
argonFingerprint =
    Argon2.Options
        { iterations = 1000
        , memory = 256 * 1024
        , parallelism = 4
        , variant = Argon2.Argon2id
        , version = Argon2.Version13
        }



powSalt =
    B.pack
        [220, 55, 235, 37, 39, 106, 232, 132, 98, 40, 144, 227, 96, 33, 238, 202]



powHelp
    :: Word8
    -> Bl.ByteString
    -> Integer
    -> Either String Bl.ByteString
powHelp difficulty unique counter =
    let
    candidate = Bl.fromStrict $ encodeUint64 counter
    hashE = Argon2.hash
        argonOptions
        (Bl.toStrict $ unique <> candidate)
        powSalt
        powHashLength
    in
    case hashE of
    CryptoPassed hash ->
        if isDifficult hash difficulty then
        Right candidate

        else
        powHelp difficulty unique (counter + 1)

    CryptoFailed err ->
        Left $ show err


encodeUint64 :: Integer -> B.ByteString
encodeUint64 i =
    B.pack $ map (encodeUint64Help i) (take 8 [0..])


encodeUint64Help :: Integer -> Int -> Word8
encodeUint64Help int counter =
    fromIntegral $ (int `Bits.shiftR` (counter * 8)) Bits..&. 0xFF


encodeUint32 :: Int -> B.ByteString
encodeUint32 i =
    B.pack $ map (encodeUint32Help i) (take 4 [0..])


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
    let
    pass = (DoNothingO, InitS init_)
    in
    case init_ of
    EmptyI ->
        pass

    MakingRootDirI _ ->
        pass

    MakingDhKeysI _ ->
        pass

    GettingTimes root dhKeys ->
        ( GetRandomGenO
        , InitS $ GettingRandomGen root dhKeys times
        )

    GettingRandomGen _ _ _ ->
        pass

    DoKeysExistI _ _ _ _ ->
        pass

    GettingKeysFromFileI _ _ _ _ ->
        pass


newDhKeysUpdate :: [Dh.KeyPair Curve25519] -> Init -> (Output, State)
newDhKeysUpdate newKeys init_ =
    let
    pass = (DoNothingO, InitS init_)
    in
    case init_ of
    EmptyI ->
        pass

    MakingRootDirI _ ->
        pass

    MakingDhKeysI root ->
        (GetTimesO, InitS $ GettingTimes root newKeys)

    GettingTimes _ _ ->
        pass

    GettingRandomGen _ _ _ ->
        pass

    DoKeysExistI _ _ _ _ ->
        pass

    GettingKeysFromFileI _ _ _ _ ->
        pass


newtype MyStatic
    = MyStatic (Dh.KeyPair Curve25519)


fileExistenceUpdate :: FilePath -> Bool -> Init -> (Output, State)
fileExistenceUpdate path exists init_ =
    let
    pass = (DoNothingO, InitS init_)
    in
    case init_ of
    EmptyI ->
        pass

    MakingRootDirI _ ->
        pass

    MakingDhKeysI _ ->
        pass

    GettingTimes _ _ ->
        pass

    GettingRandomGen _ _ _ ->
        pass

    DoKeysExistI root newKeys times gen ->
        case (exists, path == keysPath root) of
        (True, True) ->
            ( ReadFileO $ keysPath root
            , InitS $
                GettingKeysFromFileI root newKeys times gen
            )

        (True, False) ->
            pass

        (False, True) ->
            let
            ready =
                Ready
                    { root
                    , blobsUp = NoJobs
                    , getBlob = NoJobs
                    , authStatus = GettingPowInfoA
                    , handshakes = Map.empty
                    , newDhKeys = newKeys
                    , theTime = times
                    , whitelist = Map.empty
                    , summaries = Map.empty
                    , randomGen = gen
                    , counter = 0
                    , assemblingFile = Nothing
                    , gettingMessage = Nothing
                    , waitForAcknowledge = []
                    , extractingMessage = Nothing
                    , readingToSendToServer = Nothing
                    , extractingReferences = Nothing
                    , page = Inbox
                    }
            in
            ( BatchO
                [ BytesInQO toServerQ $ Bl.singleton 0
                , dumpCache ready
                ]
            , ReadyS ready
            )

        (False, False) ->
            pass

    GettingKeysFromFileI _ _ _ _ ->
        pass


restartingTcpUpdate :: Ready -> (Output, State)
restartingTcpUpdate ready =
    let
    startAuth =
        ( BytesInQO toServerQ $ Bl.singleton 0
        , ReadyS $ ready { authStatus = GettingPowInfoA }
        )
    in
    case authStatus ready of
    GettingPowInfoA ->
        startAuth

    GeneratingSessionKey _ _ ->
        startAuth

    AwaitingUsername _ _ ->
        startAuth

    LoggedIn (StaticKeys _ sessionKey username _) ->
        ( BytesInQO toServerQ $ logIn sessionKey username
        , ReadyS ready
        )


logIn :: SessionKey -> Username -> Bl.ByteString
logIn (SessionKey sessionKey) (Username username) =
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

    Right (Acknowledgement code) ->
        case waitForAcknowledge ready of
        [] ->
            (DoNothingO, ReadyS ready)

        [waiting] ->
            if code == waiting then
            let
            newReady = ready { waitForAcknowledge = [] }
            in
            (dumpCache ready, ReadyS newReady)
            else
            (DoNothingO, ReadyS ready)

        w:a:iting ->
            if code == w then
            let
            newReady =
                ready
                    { waitForAcknowledge = a:iting
                    , readingToSendToServer = Just a
                    }
            in
            ( ReadFileStrictO $ makeChunkPath (root ready) a
            , ReadyS newReady
            )
            else
            (DoNothingO, ReadyS ready)

    Right (NewMessageT username message) ->
        messageInUpdate username message ready

    Right (NewUsername username) ->
        newUserIdUpdate username ready

    Right (PowInfoT difficulty unique) ->
        newPowInfoUpdate difficulty unique ready

    Right (Price price) ->
        ( BytesInQO toFrontendQ $ Bl.singleton 9 <> price
        , ReadyS ready
        )


makeChunkPath :: RootPath -> AcknowledgementCode -> FilePath
makeChunkPath root (AcknowledgementCode code) =
    makeChunksPath root </> show code


makeChunksPath :: RootPath -> FilePath
makeChunksPath (RootPath root) =
    root </> "chunks"


newPowInfoUpdate
    :: Word8
    -> Bl.ByteString
    -> Ready
    -> (Output, State)
newPowInfoUpdate difficulty unique ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
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


newUserIdUpdate :: Username -> Ready -> (Output, State)
newUserIdUpdate username ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case authStatus ready of
    GettingPowInfoA ->
        pass

    GeneratingSessionKey _ _ ->
        pass

    AwaitingUsername staticDh@(MyStatic (_, pub)) sessionKey ->
        case makeFingerprint username pub of
        Left err ->
            ( ErrorO $ "couldn't make fingerprint: " ++ err
            , FailedS
            )

        Right fingerprint ->
            let
            newReady =
                ready
                    { authStatus = LoggedIn $
                        StaticKeys
                            { staticDh
                            , sessionKey
                            , username
                            , fingerprint
                            }
                    }
            in
            ( BatchO
                [ BytesInQO
                    toServerQ
                    (logIn sessionKey username)
                , dumpCache newReady
                ]
            , ReadyS newReady
            )

    LoggedIn _ ->
        logErr
            ready
            "received new username but authstatus is LoggedIn"


num1stShakes =
    333


ciphertextP :: P.Parser Ciphertext
ciphertextP = do
    P.choice
        [ fmap FirstHandshakes firstShakesP
        , fmap SecondHandshakes secondShakesP
        , transportP
        ]


secondShakeLen =
    64


secondShakesP :: P.Parser [SecondHandshake]
secondShakesP = do
    _ <- P.word8 1
    P.many1 secondShakeP


secondShakeP :: P.Parser SecondHandshake
secondShakeP = do
    msg1 <- firstMessageP
    msg <- P.take secondShakeLen
    return $ SecondHandshake msg1 $ Bl.fromStrict msg


noiseTransportLen =
    15958


transportP :: P.Parser Ciphertext
transportP = do
    _ <- P.word8 2
    key <- firstMessageP
    noise <- P.take noiseTransportLen
    return $ TransportC key (Bl.fromStrict noise)


firstShakesP :: P.Parser [FirstMessage]
firstShakesP = do
    _ <- P.word8 0
    P.count num1stShakes firstMessageP


publicKeyLen =
    32


firstMessageP :: P.Parser FirstMessage
firstMessageP = do
    fmap (FirstMessage . Bl.fromStrict) $ P.take publicKeyLen


data Plaintext
    = AllInOne Bl.ByteString
    | NotLastInSequence Bl.ByteString
    | FinalChunk Hash32 Bl.ByteString


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

    Right (TransportC firstMessage thisMessage) ->
        transportUpdate from ready firstMessage thisMessage


transportUpdate
    :: Username
    -> Ready
    -> FirstMessage
    -> Bl.ByteString
    -> (Output, State)
transportUpdate from ready firstMessage thisMessage =
    let
        handshake = HandshakeId from firstMessage
    in
    case Map.lookup handshake (handshakes ready) of
    Nothing ->
        (DoNothingO, ReadyS ready)

    Just (Initiator _ ) ->
        (DoNothingO, ReadyS ready)

    Just (ResponderSentEncryptedES myEphemeral) ->
        case authStatus ready of
        GettingPowInfoA ->
            (DoNothingO, ReadyS ready)

        GeneratingSessionKey _ _ ->
            (DoNothingO, ReadyS ready)

        AwaitingUsername _ _ ->
            (DoNothingO, ReadyS ready)

        LoggedIn staticKeys ->
            transportUpdateHelp
                from
                ready
                firstMessage
                thisMessage
                myEphemeral
                staticKeys


transportUpdateHelp
    :: Username
    -> Ready
    -> FirstMessage
    -> Bl.ByteString
    -> MyEphemeral
    -> StaticKeys
    -> (Output, State)
transportUpdateHelp
    from
    ready
    (FirstMessage first)
    encrypted
    myEphemeral
    myStatic =

    let
    options = responderOptions (staticDh myStatic) myEphemeral
    noise0 = Noise.noiseState options noiseXX :: NoiseState
    in
    case
        Noise.readMessage
        (Noise.convert $ Bl.toStrict first)
        noise0
    of
    Noise.NoiseResultNeedPSK _ ->
        (DoNothingO, ReadyS ready) -- shouldn't happen with XX

    Noise.NoiseResultException err ->
        logErr
            ready
            (noiseException err)

    Noise.NoiseResultMessage _ noise1 ->
        case Noise.writeMessage "" noise1 of
        Noise.NoiseResultNeedPSK _ ->
            (DoNothingO, ReadyS ready)

        Noise.NoiseResultException err ->
            logErr
                ready
                (noiseException err)

        Noise.NoiseResultMessage _ noise2 ->
            case Noise.readMessage
                    (Noise.convert $ Bl.toStrict encrypted)
                    noise2
            of
            Noise.NoiseResultNeedPSK _ ->
                (DoNothingO, ReadyS ready)

            Noise.NoiseResultException err ->
                logErr
                    ready
                    (noiseException err)

            Noise.NoiseResultMessage plain noise3 ->
                untrustedPlain
                    (Bl.fromStrict $ Noise.convert plain)
                    noise3
                    from
                    ready



untrustedPlain
    :: Bl.ByteString
    -> NoiseState
    -> Username
    -> Ready
    -> (Output, State)
untrustedPlain plain noise from ready =
    case
    ( Noise.remoteStaticKey noise
    , Map.lookup from (whitelist ready)) of

    (Just _, Nothing) ->
        unsolicitedMessage ready from

    (Nothing, Nothing) ->
        unsolicitedMessage ready from

    (Nothing, (Just _)) ->
        logErr
            ready
            (mconcat
                [ "could not get static key for message from "
                , T.pack $ show from
                ])

    (Just untrusted, Just (fingerprint, Nothing)) ->
        case makeFingerprint from untrusted of
        Left err ->
            logErr
                ready
                (mconcat
                    [ "could not make fingerprint for "
                    , T.pack $ show from
                    , ": "
                    , T.pack err
                    ])

        Right untrustedFingerprint ->
            if fingerprint == untrustedFingerprint then
            gotValidPlaintextUpdate from plain $
                ready
                    { whitelist =
                        Map.insert
                            from
                            ( fingerprint
                            , Just $
                                TheirStatic untrusted)
                            (whitelist ready)
                    }

            else
            unsolicitedMessage ready from

    ( Just untrusted, Just (_, Just (TheirStatic trusted))) ->
        if untrusted == trusted then
        gotValidPlaintextUpdate from plain ready

        else
        unsolicitedMessage ready from


unsolicitedMessage :: Ready -> Username -> (Output, State)
unsolicitedMessage ready from =
    logErr
        ready
        (mconcat
            [ "received untrusted message from "
            , T.pack $ show from
            ])


plainLen =
    15910


plainP :: P.Parser Plaintext
plainP = do
    P.choice
        [ do
            _ <- P.word8 0
            len <- uint16P
            msg <- P.take len
            _ <- P.take (plainLen - 1 - 2 - len) -- padding
            P.endOfInput
            return $ AllInOne $ Bl.fromStrict msg

        , do
            _ <- P.word8 1
            chunk <- P.take (plainLen - 1)
            P.endOfInput
            return $
                NotLastInSequence $ Bl.fromStrict chunk

        , do
            _ <- P.word8 2
            hash <- hash32P
            len <- uint16P
            chunk <- P.take len
            _ <- P.take (plainLen - 1 - 32 - 2 - len) -- padding
            return $ FinalChunk hash $ Bl.fromStrict chunk
        ]


gotValidPlaintextUpdate
    :: Username
    -> Bl.ByteString
    -> Ready
    -> (Output, State)
gotValidPlaintextUpdate from plain ready =
    case P.eitherResult $ P.parse plainP plain of
    Left err ->
        logErr ready $ mconcat
            [ "could not parse plaintext from "
            , T.pack $ show from
            , ": "
            , T.pack err
            ]

    Right parsed ->
        parsedPlainUpdate from parsed ready


allInOneP :: P.Parser Assembled
allInOneP = do
            _ <- P.word8 1
            messageId <- messageIdP
            header <- headerP
            return $ HeaderA messageId header


parsedPlainUpdate :: Username -> Plaintext -> Ready -> (Output, State)
parsedPlainUpdate from plain ready =
    case plain of
    AllInOne p ->
        case P.eitherResult $ P.parse allInOneP p of
        Left err ->
            logErr
                ready
                ("could not parse AllInOne plaintext: " <> T.pack err)

        Right assembled ->
            assembledUpdate from assembled ready

    NotLastInSequence chunk ->
        case assemblingFile ready of
        Nothing ->
            let
            unique = counter ready
            hashState =
                Blake.update
                    (Bl.toStrict chunk)
                    (Blake.initialize 32)
            temp = tempFile (root ready) unique
            assemble = Assembling unique hashState
            newReady =
                ready
                    { assemblingFile = Just assemble
                    , counter = (counter ready) + 1
                    }
            in
            (AppendFileO temp chunk, ReadyS newReady)

        Just (Assembling tmpBase oldHashState) ->
            let
            hashState =
                Blake.update
                (Bl.toStrict chunk)
                oldHashState
            temp = tempFile (root ready) tmpBase
            assemble = Assembling tmpBase hashState
            newReady =
                ready { assemblingFile = Just assemble }
            in
            (AppendFileO temp chunk, ReadyS newReady)

        Just (AddingFinal _ _) ->
            (DoNothingO, ReadyS ready)

        Just (Moving _ _) ->
            (DoNothingO, ReadyS ready)

    FinalChunk expectedHash chunk ->
        case assemblingFile ready of
        Nothing ->
            (DoNothingO, ReadyS ready)

        Just (Assembling tmpBase oldHashState) ->
            let
            hash =
                Hash32 $
                Bl.fromStrict $
                Blake.finalize 32 $
                Blake.update
                    (Bl.toStrict chunk)
                    oldHashState
            temp = tempFile (root ready) tmpBase
            assemble = AddingFinal tmpBase hash
            newReady =
                ready { assemblingFile = Just assemble }
            in
            if expectedHash == hash then
            (AppendFileO temp chunk, ReadyS newReady)

            else
            (DoNothingO, ReadyS newReady)

        Just (AddingFinal _ _) ->
            (DoNothingO, ReadyS ready)

        Just (Moving _ _) ->
            (DoNothingO, ReadyS ready)


data Assembled
    = HeaderA MessageId Header
    | Referenced MessageId Hash32


headerP :: P.Parser Header
headerP = do
    time <- timeP
    shares <- listP usernameP
    subject <- sizedStringP
    mainBox <- sizedStringP
    blobs <- listP blobP
    wasm <- hash32P
    return
        Header{time, shares, subject, mainBox, blobs, wasm}


assembledUpdate
    :: Username
    -> Assembled
    -> Ready
    -> (Output, State)
assembledUpdate from assembled ready =
    case assembled of
    HeaderA messageId header ->
        case Map.lookup messageId (summaries ready) of
        Nothing ->
            let
            summary = summarize from header
            encoded = encodeHeader header
            diff = diffToTuple $ makeDiff "" encoded
            path = makeMessagePath (root ready) messageId
            newReady =
                ready
                    { summaries =
                        Map.insert
                            messageId
                            summary
                            (summaries ready)
                    }
            in
            (DbInsertDiffO path diff, ReadyS newReady)

        Just _ ->
            ( ReadMessageO (makeMessagePath (root ready) messageId)
            , ReadyS $
                ready
                    { extractingMessage =
                        Just (messageId, from, header)
                    }
            )

    Referenced messageId hash ->
        referencedHelp messageId hash ready


referencedHelp
    :: MessageId
    -> Hash32
    -> Ready
    -> (Output, State)
referencedHelp messageId blob ready =
    ( ReadMessageO (makeMessagePath (root ready) messageId)
    , ReadyS $ ready
        { extractingReferences = Just (messageId, blob) }
    )


encodeHeader :: Header -> B.ByteString
encodeHeader header =
    mconcat
        [ encodeTime $ time header
        , encodeList (Bl.toStrict . encodeUsername) (shares header)
        , encodeSizedString $ subject header
        , encodeSizedString $ mainBox header
        , encodeList encodeBlob $ blobs header
        , encodeHash32 $ wasm header
        ]


encodeHash32 :: Hash32 -> B.ByteString
encodeHash32 (Hash32 hash) =
    Bl.toStrict hash


encodeBlob :: Blob -> B.ByteString
encodeBlob blob =
    mconcat
        [ encodeHash32 $ hashB blob
        , encodeSizedString $ filename blob
        , encodeUint64 $ size blob
        , encodeSizedString $ mime blob
        ]


encodeTime :: PosixMillis -> B.ByteString
encodeTime (PosixMillis time) =
    encodeUint64 time


encodeSizedString :: T.Text -> B.ByteString
encodeSizedString string =
    let
    encoded = encodeUtf8 string
    in
    encodeUint32 (B.length encoded) <> encoded


encodeList :: (a -> B.ByteString) -> [a] -> B.ByteString
encodeList encoder list =
    encodeUint32 (length list) <> mconcat (map encoder list)


data Diff
    = Diff
        { start :: Int
        , end :: Int
        , insert :: B.ByteString
        , integrity :: Hash8
        }


makeDiff :: B.ByteString -> B.ByteString -> Diff
makeDiff old new =
    let
    start = countIdentical old new
    end = countIdentical (B.reverse old) (B.reverse new)
    insert = B.drop start $ B.reverse $ B.drop end $ B.reverse new
    integrity = hash8 insert
    in
    Diff { start, end, insert, integrity }


countIdentical :: B.ByteString -> B.ByteString -> Int
countIdentical a b =
     countIdenticalHelp (B.zip a b) 0


diffToTuple :: Diff -> (Int, Int, B.ByteString, B.ByteString)
diffToTuple (Diff start end insert (Hash8 integrity)) =
    ( start, end, insert, integrity )


countIdenticalHelp :: [(Word8, Word8)] -> Int -> Int
countIdenticalHelp zipped accum =
    case zipped of
    [] ->
        accum

    (z1, z2):ipped ->
        if z1 == z2 then
        countIdenticalHelp ipped (accum + 1)
        else
        accum


hash8 :: B.ByteString -> Hash8
hash8 raw =
    Hash8 $ B.take 8 $ Ba.convert (Hash.hash raw :: Hash.Digest Hash.Blake2b_160)


newtype Hash8
    = Hash8 B.ByteString
    deriving (Eq, Ord)


messageIdLen =
    24


timeP :: P.Parser PosixMillis
timeP = do
    t <- uint64P
    return $ PosixMillis t


newtype PosixMillis
    = PosixMillis Integer


blobP :: P.Parser Blob
blobP = do
    hashB <- hash32P
    filename <- sizedStringP
    size <- uint64P
    mime <- sizedStringP
    return $ Blob{hashB, filename, size, mime}



sizedStringP :: P.Parser T.Text
sizedStringP = do
    size <- uint32P
    raw <- P.take size
    case decodeUtf8' raw of
        Left err ->
            fail $ "error parsing UTF-8: " <> show err

        Right str ->
            return str


stringP :: P.Parser T.Text
stringP = do
    raw <- P.takeByteString
    case decodeUtf8' raw of
        Left err ->
            fail $ "error parsing UTF-8: " <> show err

        Right s ->
            return s


data Blob
    = Blob
        { hashB :: Hash32
        , filename :: T.Text
        , size :: Integer
        , mime :: T.Text
        }
        deriving (Eq, Ord)


makeBlobPath :: RootPath -> Hash32 -> FilePath
makeBlobPath root hash =
    blobsPath root </> showHash hash


blobsPath :: RootPath -> FilePath
blobsPath (RootPath root) =
    root </> "blobs"


secondHandshakesUpdate
    :: Username
    -> Ready
    -> [SecondHandshake]
    -> (Output, State)
secondHandshakesUpdate from ready shakes =
    ( DoNothingO
    , let
      shakeMap = make2ndShakeMap shakes from
      newShakes =
        Map.mapWithKey
            (add2ndShakes shakeMap)
            (handshakes ready)
      in
      ReadyS $ ready { handshakes = newShakes }
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
        ResponderSentEncryptedES _ ->
            oldShake

        Initiator SentPlainE ->
            Initiator $ ReceivedEncryptedE $
                EncryptedEphemeral newMsg

        Initiator (ReceivedEncryptedE _) ->
            oldShake


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
    -> [FirstMessage]
    -> (Output, State)
firstHandshakesUpdate from ready msgs =
    let
    pass = (DoNothingO, ReadyS ready)
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
    in
    case authStatus ready of
    GettingPowInfoA ->
        pass

    GeneratingSessionKey _ _ ->
        pass

    AwaitingUsername _ _ ->
        pass

    LoggedIn (StaticKeys myStatic _ _ _) ->
        case sendNoiseXX2s myEphemerals msgs myStatic from of
        Left err ->
            logErr newReady err

        Right kk2s ->
            ( BatchO [kk2s, dumpCache ready]
            , ReadyS newReady
            )


logPath :: RootPath -> FilePath
logPath (RootPath root) =
    root </> "log"


makeResponderShakes
    :: [MyEphemeral]
    -> [FirstMessage]
    -> Username
    -> Map.Map HandshakeId Handshake
makeResponderShakes myNewEphemerals firstShakes from =
    Map.fromList $ map (makeResponderShake from) $
        zip myNewEphemerals firstShakes


makeResponderShake
    :: Username
    -> (MyEphemeral, FirstMessage)
    -> (HandshakeId, Handshake)
makeResponderShake from (myEphemeral, theirEphemeral) =
    ( HandshakeId from theirEphemeral
    , ResponderSentEncryptedES myEphemeral
    )


sendNoiseXX2s
    :: [MyEphemeral]
    -> [FirstMessage]
    -> MyStatic
    -> Username
    -> Either T.Text Output
sendNoiseXX2s myEphemerals firsts myStatic from =
    let
    max2nd = 166
    zipped = zip myEphemerals firsts
    first166 = take max2nd zipped
    second166 = take max2nd $ drop max2nd zipped
    third = drop (2 * max2nd) zipped
    eitherChunks =
        map
            (make2ndNoise from myStatic)
            [first166, second166, third]
    in
    case allRight eitherChunks of
    Left err ->
        Left err

    Right chunks ->
        Right $ BatchO $ map (BytesInQO toServerQ) chunks


make2ndNoise
    :: Username
    -> MyStatic
    -> [(MyEphemeral, FirstMessage)]
    -> Either T.Text Bl.ByteString
make2ndNoise (Username username) myStatic noises1 =
    let
    eitherNoises = map (makeOne2ndNoise myStatic) noises1
    in
    case allRight eitherNoises of
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
    :: MyStatic
    -> MyEphemeral
    -> Noise.HandshakeOpts Curve25519
responderOptions
    (MyStatic myStatic)
    (MyEphemeral myEphemeral) =

    Noise.setLocalStatic (Just myStatic) .
    Noise.setLocalEphemeral (Just myEphemeral) $
    Noise.defaultHandshakeOpts Noise.ResponderRole ""


type NoiseState
    = Noise.NoiseState ChaChaPoly1305 Curve25519 BLAKE2s


makeOne2ndNoise
    :: MyStatic
    -> (MyEphemeral, FirstMessage)
    -> Either T.Text Bl.ByteString
makeOne2ndNoise myStatic (myEphemeral, FirstMessage refKey) =
    let
    options = responderOptions myStatic myEphemeral
    noise0 = Noise.noiseState options noiseXX :: NoiseState
    msg0 = Noise.convert $ Bl.toStrict refKey
    in
    case Noise.readMessage msg0 noise0 of
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
        WriteFileO
            (cachePath $ root ready)
            (encodeCache cache)


encodeCache :: MemCache -> Bl.ByteString
encodeCache memCache =
     mconcat
        [ encodeCryptoKeys $ keys memCache
        , encodeHandshakes $ handshakesM memCache
        , encodeWhitelist $ whitelistM memCache
        ]


encodeWhitelist
    :: Map.Map Username (Fingerprint, Maybe TheirStatic)
    -> Bl.ByteString
encodeWhitelist whitelist =
    let
    asList = Map.toList whitelist
    in
    (Bl.fromStrict $ encodeUint32 (length asList)) <>
    mconcat (map encodeOneWhite asList)


encodeOneWhite
    :: (Username, (Fingerprint, Maybe TheirStatic))
    -> Bl.ByteString
encodeOneWhite (username, (fingerprint, maybeKey)) =
    mconcat
        [ encodeUsername username
        , encodeFingerprint fingerprint
        , encodeMaybe maybeKey encodeTheirStatic
        ]


encodeMaybe :: Maybe a -> (a -> Bl.ByteString) -> Bl.ByteString
encodeMaybe a encoder =
    case a of
    Nothing ->
        Bl.singleton 0

    Just justa ->
        Bl.singleton 1 <> encoder justa


encodeCryptoKeys :: StaticKeys -> Bl.ByteString
encodeCryptoKeys keys =
    mconcat
        [ encodeKeyPair $ staticDh keys
        , encodeSessionKey $ sessionKey keys
        , encodeUsername $ username keys
        , encodeFingerprint $ fingerprint keys
        ]


encodeFingerprint :: Fingerprint -> Bl.ByteString
encodeFingerprint (Fingerprint fingerprint) =
    fingerprint


encodeUsername :: Username -> Bl.ByteString
encodeUsername (Username username) =
    username


encodeSessionKey :: SessionKey -> Bl.ByteString
encodeSessionKey (SessionKey key) =
    key


encodeKeyPair :: MyStatic -> Bl.ByteString
encodeKeyPair (MyStatic (secret, _)) =
    Bl.fromStrict $ Noise.convert $ Dh.dhSecToBytes secret


cachePath :: RootPath -> FilePath
cachePath (RootPath root) =
    root </> "memCache"


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
            , summariesM = summaries ready
            , counterM = counter ready
            , waitForAcknowledgeM = waitForAcknowledge ready
            }


encodeHandshakes :: Map.Map HandshakeId Handshake -> Bl.ByteString
encodeHandshakes =
    mconcat . map encodeHandshake . Map.toList


encodeHandshake :: (HandshakeId, Handshake) -> Bl.ByteString
encodeHandshake (id_, shake) =
    encodeHandshakeId id_ <> encodeHandshakeShake shake


encodeHandshakeId :: HandshakeId -> Bl.ByteString
encodeHandshakeId (HandshakeId (Username u) (FirstMessage e)) =
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

    ResponderSentEncryptedES (MyEphemeral (secret, _)) ->
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
        , do
            _ <- P.word8 4
            code <- acknowledgementP
            return $ Acknowledgement code
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
    | Acknowledgement AcknowledgementCode


newtype AcknowledgementCode
    = AcknowledgementCode Int
    deriving (Eq, Ord)


data Ciphertext
    = FirstHandshakes [FirstMessage]
    | SecondHandshakes [SecondHandshake]
    | TransportC FirstMessage Bl.ByteString


data SecondHandshake
    = SecondHandshake FirstMessage Bl.ByteString


newtype TheirStatic
    = TheirStatic (Dh.PublicKey Curve25519)


newtype ClientToClient
    = ClientToClient Bl.ByteString


encodeTheirStatic :: TheirStatic -> Bl.ByteString
encodeTheirStatic (TheirStatic key) =
    Bl.fromStrict $ Noise.convert $ Dh.dhPubToBytes key


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
    return $ Hash32 $ Bl.fromStrict hash


data FromFrontend
    = InboxClick
    | DraftsClick
    | WriterClick
    | ContactsClick
    | AccountClick
    | NewMain T.Text
    | NewSubject T.Text
    | NewShares [UserId]
    | NewWasm Hash32
    | NewBlobs [Blob]


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
    id_ <- P.take messageIdLen
    return $ MessageId $ Bl.fromStrict id_


fromFrontendP :: P.Parser FromFrontend
fromFrontendP = do
    input <- P.choice
        [ P.word8 0 >> return InboxClick
        , P.word8 1 >> return DraftsClick
        , P.word8 2 >> return WriterClick
        , P.word8 3 >> return ContactsClick
        , P.word8 4 >> return AccountClick
        , do
            _ <- P.word8 5
            newMain <- stringP
            return $ NewMain newMain
        , do
            _ <- P.word8 6
            newSubject <- stringP
            return $ NewSubject newSubject
        , do
            _ <- P.word8 7
            newShares <- P.many1 userIdP
            return $ NewShares newShares
        , do
            _ <- P.word8 8
            hash <- hash32P
            return $ NewWasm hash
        , do
            _ <- P.word8 9
            blobs <- P.many1 blobP
            return $ NewBlobs blobs
        ]
    P.endOfInput
    return input


dumpView :: Ready -> Output
dumpView =
    BytesInQO toFrontendQ . encodeView . makeView


encodeView :: View -> Bl.ByteString
encodeView view =
    mconcat
    [ encodeMaybe (myId view) encodeUserId
    , encodePage $ pageV view
    ]


encodePage :: ViewPage -> Bl.ByteString
encodePage viewPage =
    case viewPage of
        InboxView summaries ->
            Bl.singleton 0 <>
            Bl.fromStrict
                (encodeList
                    (Bl.toStrict . encodeSummary)
                    (Map.toList summaries))


encodeSummary :: (MessageId, Summary) -> Bl.ByteString
encodeSummary (MessageId messageId, summary) =
    mconcat
    [ messageId
    , Bl.fromStrict $ encodeSizedString $ subjectS summary
    , Bl.fromStrict $ encodeTime $ timeS summary
    , encodeUsername $ authorS summary
    ]


encodeUserId :: UserId -> Bl.ByteString
encodeUserId (UserId username fingerprint) =
    mconcat
    [ encodeUsername username
    , encodeFingerprint fingerprint
    ]


makeView :: Ready -> View
makeView ready =
    View
        { myId = myIdView ready
        , pageV = pageView ready
        }


pageView :: Ready -> ViewPage
pageView ready =
    case page ready of
    Inbox ->
        InboxView $ summaries ready


myIdView :: Ready -> Maybe UserId
myIdView ready =
    case authStatus ready of
    GettingPowInfoA ->
        Nothing

    GeneratingSessionKey _ _ ->
        Nothing

    AwaitingUsername _ _ ->
        Nothing

    LoggedIn staticKeys ->
        Just $ UserId (username staticKeys) (fingerprint staticKeys)


data View
    = View
        { myId :: Maybe UserId
        , pageV :: ViewPage
        }


data ViewPage
    = InboxView (Map.Map MessageId Summary)


uiApiUpdate :: FromFrontend -> Ready -> (Output, State)
uiApiUpdate fromFrontend ready =
    case fromFrontend of
        InboxClick ->
            if page ready == Inbox then
            (DoNothingO, ReadyS ready)
            else
            let
            newReady = ready { page = Inbox }
            in
            (dumpView newReady , ReadyS newReady)

        DraftsClick ->
            undefined

        WriterClick ->
            undefined

        ContactsClick ->
            undefined

        AccountClick ->
            undefined

        NewMain _ ->
            undefined

        NewSubject _ ->
            undefined

        NewShares _ ->
            undefined

        NewWasm _ ->
            undefined

        NewBlobs _ ->
            undefined


onMovedFile :: FilePath -> Ready -> (Output, State)
onMovedFile new ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case blobsUp ready of
    NoJobs ->
        pass

    Jobs (AwaitingMoveB q (Hash32 hash) toPath) [] ->
        if toPath == new then
        ( BytesInQO q (Bl.singleton 1 <> hash)
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
            [ BytesInQO q (Bl.singleton 1 <> hash)
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


onWrittenToTmp :: FilePath -> Io.Handle -> Ready -> (Output, State)
onWrittenToTmp writtenPath handle ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
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


showHash :: Hash32 -> String
showHash (Hash32 hash) =
    Bc.unpack $ B64.encodeUnpadded $ Bl.toStrict hash


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
    Bl.fromStrict .
    Blake.finalize 32 .
    Bl.foldrChunks Blake.update (Blake.initialize 32)


tempFile :: RootPath -> Int -> FilePath
tempFile root unique =
    tempPath root </> show unique


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
    -> State
    -> (Output, State)
fileContentsUpdate path contents model =
    let
    pass = (DoNothingO, model)
    in
    case model of
    FailedS ->
        pass

    InitS EmptyI ->
        pass

    InitS (MakingRootDirI _) ->
        pass

    InitS (MakingDhKeysI _) ->
        pass

    InitS (GettingTimes _ _) ->
        pass

    InitS (GettingRandomGen _ _ _) ->
        pass

    InitS (DoKeysExistI _ _ _ _) ->
        pass

    InitS (GettingKeysFromFileI root dhKeys times gen) ->
        if path == keysPath root then
        rawKeysUpdate contents root dhKeys times gen
        else
        pass

    ReadyS ready ->
        case gettingMessage ready of
        Nothing ->
            pass

        Just expectingHash ->
            if (makeBlobPath (root ready) expectingHash) == path then
            ( BytesInQO toFrontendQ contents
            , ReadyS $ ready { gettingMessage = Nothing }
            )
            else
            pass


parseCrypto
    :: Bl.ByteString
    -> Either String MemCache
parseCrypto raw =
    P.eitherResult $ P.parse memCacheP raw


data MemCache
    = MemCache
        { keys :: StaticKeys
        , handshakesM :: Map.Map HandshakeId Handshake
        , whitelistM ::
            Map.Map Username (Fingerprint, Maybe TheirStatic)
        , summariesM :: Map.Map MessageId Summary
        , counterM :: Int
        , waitForAcknowledgeM :: [AcknowledgementCode]
        }


memCacheP :: P.Parser MemCache
memCacheP = do
    keys <- myKeysP
    handshakesM <- handshakesP
    whitelistM <- whitelistP
    summariesM <- summariesP
    counterM <- uint32P
    waitForAcknowledgeM <- listP acknowledgementP
    P.endOfInput
    return $
        MemCache
            { keys
            , handshakesM
            , whitelistM
            , summariesM
            , counterM
            , waitForAcknowledgeM
            }


acknowledgementP :: P.Parser AcknowledgementCode
acknowledgementP = do
    code <- uint32P
    return $ AcknowledgementCode code


listP :: P.Parser a -> P.Parser [a]
listP parser = do
    num <- uint32P
    elements <- P.count num parser
    return elements


summariesP :: P.Parser (Map.Map MessageId Summary)
summariesP = do
    numSums <- uint32P
    asList <- P.count numSums summaryP
    return $ Map.fromList asList


summaryP :: P.Parser (MessageId, Summary)
summaryP = do
    messageId <- messageIdP
    subjectS <- sizedStringP
    timeS <- timeP
    authorS <- usernameP
    return (messageId, Summary {subjectS, timeS, authorS})


whitelistP
    :: P.Parser (Map.Map Username (Fingerprint, Maybe TheirStatic))
whitelistP = do
    numWhites <- uint32P
    asList <- P.count numWhites oneWhitelistP
    return $ Map.fromList asList


oneWhitelistP :: P.Parser (Username, (Fingerprint, Maybe TheirStatic))
oneWhitelistP = do
    username <- usernameP
    fingerprint <- fingerprintP
    maybeStatic <- maybeP theirStaticP
    return (username, (fingerprint, maybeStatic))


maybeP :: P.Parser a -> P.Parser (Maybe a)
maybeP parser =
    P.choice
        [ do
            _ <- P.word8 0
            return Nothing

        , do
            _ <- P.word8 1
            parsed <- parser
            return $ Just parsed
        ]


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


uint64P :: P.Parser Integer
uint64P =
    uint64HelpP 0 0


uint64HelpP :: Integer -> Integer -> P.Parser Integer
uint64HelpP sofar counter =
    if counter == 8 then
    return sofar

    else do
        word <- P.anyWord8
        uint64HelpP
            (sofar + (fromIntegral word) * (256 ^ counter))
            (counter + 1)


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
    return $ ResponderSentEncryptedES myEphemeral


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
    firstMessage <- fmap FirstMessage $
        fmap Bl.fromStrict $ P.take 32
    return $ HandshakeId username firstMessage


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
    username <- usernameP
    fingerprint <- fingerprintP
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
                    , username
                    , fingerprint
                    }


rawKeysUpdate
    :: Bl.ByteString
    -> RootPath
    -> [Dh.KeyPair Curve25519]
    -> [Clock.UTCTime]
    -> CryptoRand.ChaChaDRG
    -> (Output, State)
rawKeysUpdate rawCrypto root newDhKeys times gen =
    case parseCrypto rawCrypto of
    Left err ->
        ( ErrorO $ "could not parse static keys: " <> err
        , FailedS
        )

    Right memCache ->
        ( BatchO
            [ BytesInQO toServerQ $
              logIn
                (sessionKey $ keys memCache)
                (username $ keys $ memCache)
            ]
        , ReadyS $ Ready
            { root
            , blobsUp = NoJobs
            , getBlob = NoJobs
            , authStatus = LoggedIn $ keys memCache
            , handshakes = handshakesM memCache
            , newDhKeys
            , theTime = times
            , whitelist = whitelistM memCache
            , randomGen = gen
            , summaries = summariesM memCache
            , counter = counterM memCache
            , assemblingFile = Nothing
            , gettingMessage = Nothing
            , extractingMessage = Nothing
            , waitForAcknowledge = waitForAcknowledgeM memCache
            , readingToSendToServer = Nothing
            , extractingReferences = Nothing
            , page = Inbox
            }
        )


dirCreatedUpdate :: FilePath -> Init -> (Output, State)
dirCreatedUpdate path initModel =
    let
    pass = (DoNothingO, InitS initModel)
    in
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

    GettingRandomGen _ _ _ ->
        pass

    DoKeysExistI _ _ _ _ ->
        pass

    GettingKeysFromFileI _ _ _ _ ->
        pass

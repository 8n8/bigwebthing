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
import Crypto.Noise.HandshakePatterns (noiseKK)
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
import qualified Text.Hex
import qualified Network.Simple.TCP as Tcp
import qualified Data.Map as Map
import Control.Exception.Base (SomeException)
import qualified Data.Time.Clock as Clock
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import qualified Data.Bits as Bits
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Crypto.Random as CryptoRand
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as Ba
import qualified Data.Set as Set
import qualified Crypto.PubKey.Ed25519 as Ed


newtype Recipient
    = Recipient Ed.PublicKey


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
            mapM (\_ -> Clock.getCurrentTime) ([1..] :: [Integer])
        return $ Just $ TimesM times

    AppendFileO path toAppend -> do
        _ <- Bl.appendFile path toAppend
        return Nothing

    AppendFileStrictO path toAppend -> do
        _ <- B.appendFile path toAppend
        return $ Just AppendedStrictM

    ReadFileStrictO path -> do
        contents <- B.readFile path
        return $ Just $ StrictFileContentsM path contents

    WriteFileStrictO path contents -> do
        B.writeFile path contents
        return Nothing

    GetFileHandleO path -> do
        handle <- Io.openFile path Io.ReadMode
        return $ Just $ FileHandleM path handle

    ReadFromHandleO path n handle -> do
        bytes <- B.hGet handle n
        return $ Just $ ChunkFromHandleM path bytes


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

        Sc.post "/setcode" $
            httpPost SetCodeM

        Sc.post "/setblob/:metadata" $ do
            body <- Sc.body
            metadata <- Sc.param "metadata"
            liftIO $ Stm.atomically $ do
                q <- msgQ
                Q.writeTQueue q (SetBlobM body metadata)

        Sc.post "/getblob" $
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
            Q.writeTQueue q $ msg body responseQ
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
    | ReadFileStrictO FilePath
    | WriteFileStrictO FilePath B.ByteString
    | GetFileHandleO FilePath
    | ReadFromHandleO FilePath Int Io.Handle


data Msg
    = StartM
    | SetCodeM Bl.ByteString Q
    | AppDataDirM FilePath
    | DirCreatedIfMissingM FilePath
    | FileContentsM FilePath Bl.ByteString
    | BatchM [Maybe Msg]
    | SetBlobM Bl.ByteString T.Text
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
    | RandomGenM CryptoRand.ChaChaDRG
    | AppendedStrictM
    | FileHandleM FilePath Io.Handle
    | ChunkFromHandleM FilePath B.ByteString


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


newtype RootPath
    = RootPath FilePath


newtype Hash32
    = Hash32 Bl.ByteString
    deriving (Eq, Ord, Show)


data BlobUpWait
    = BlobUpWait Bl.ByteString T.Text


data BlobUp
    = AwaitingHandleB Bl.ByteString T.Text Hash32
    | AwaitingTmpWriteB T.Text Hash32 FilePath
    | AwaitingMoveB T.Text Hash32 FilePath


promoteBlobsUp :: Jobs BlobUp BlobUpWait -> Jobs BlobUp BlobUpWait
promoteBlobsUp jobs =
    case jobs of
    NoJobs ->
        NoJobs

    Jobs _ [] ->
        NoJobs

    Jobs _ (BlobUpWait body metadata : aiting) ->
        Jobs (AwaitingHandleB body metadata (hash32 body)) aiting


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
        { time :: PosixSeconds
        , shares :: [Sender]
        , subject :: T.Text
        , mainBox :: T.Text
        , blobs :: [Blob]
        , wasm :: Hash32
        }


plainChunkLen =
    15894


encodeAndChunkHeader :: MessageId -> Header -> [B.ByteString]
encodeAndChunkHeader (MessageId messageId) header =
    let
    encoded = encodeHeader header
    withId = messageId <> B.singleton 0 <> encoded
    len = B.length encoded
    padding = B.replicate (plainChunkLen - 3 - len) 0
    in
    if len <= plainChunkLen - 3 then
    [mconcat [B.singleton 0, encodeUint16 len, withId, padding]]

    else
    encodeAndChunkHeaderHelp
        (hash32 $ Bl.fromStrict encoded)
        encoded
        []


encodeAndChunkHeaderHelp
    :: Hash32
    -> B.ByteString
    -> [B.ByteString]
    -> [B.ByteString]
encodeAndChunkHeaderHelp h@(Hash32 hash) encoded chunked =
    let
    len = B.length encoded
    thisLenMax = plainChunkLen - 1 - 32 - 2
    in
    if len > thisLenMax then

    let
    chunk =
        mconcat
            [ B.singleton 1
            , B.replicate 34 0
            , B.take thisLenMax encoded
            ]
    remaining = B.drop thisLenMax encoded
    in
    encodeAndChunkHeaderHelp h remaining (chunk : chunked)

    else
    let
    chunk =
        mconcat
            [ B.singleton 2
            , Bl.toStrict hash
            , encodeUint16 len
            , encoded
            , B.replicate (thisLenMax - len) 0
            ]
    in
    reverse (chunk : chunked)


data Ready
    = Ready
        { root :: RootPath
        , blobsUp :: Jobs BlobUp BlobUpWait
        , getBlob :: Jobs GetBlob GetBlobWait
        , sendingBlob :: Jobs SendingBlob SendingBlobWait
        , authStatus :: AuthStatus
        , handshakes :: Map.Map HandshakeId Handshake
        , newDhKeys :: [Dh.KeyPair Curve25519]
        , theTime :: [Clock.UTCTime]
        , whitelist :: Whitelist
        , waitForAcknowledge :: [Hash32]
        , summaries :: Map.Map MessageId Summary
        , randomGen :: CryptoRand.ChaChaDRG
        , counter :: Integer
        , assemblingFile :: Maybe AssemblingFile
        , gettingMessage :: Maybe Hash32
        , extractingMessage :: Maybe (MessageId, Sender, Header)
        , extractingReferences :: Maybe (MessageId, Hash32)
        , readingToSendToServer :: Maybe Hash32
        , pageR :: Page
        , awaitingCrypto :: [(Hash32, Sender)]
        , readingMessageOnClick :: Maybe MessageId
        , staticNoiseKeys :: Maybe MyStaticNoise
        , secretSigningKey :: Maybe Ed.SecretKey
        , userId :: Maybe (Fingerprint, Shortened)
        , lookingUpShortened :: Map.Map Shortened Fingerprint
        }


data SendingBlob
    = AwaitingHandle Hash32 [Sender]
    | Chunking Io.Handle Hash32 [Sender]


data SendingBlobWait
    = SendingBlobWait MessageId Hash32 [Sender]


data Page
    = Messages
    | Writer MessageId [Diff]
    | Contacts
    | Account
    deriving Eq


decodeHeader :: Bl.ByteString -> Either String Header
decodeHeader raw =
    P.eitherResult (P.parse headerP raw)


data AssemblingFile
    = Assembling TmpBase Blake.BLAKE2bState
    | AddingFinal TmpBase Hash32
    | Moving TmpBase Hash32


type TmpBase
    = Integer


data Summary
    = Summary
        { subjectS :: T.Text
        , timeS :: PosixSeconds
        , authorS :: Sender
        }


summarize :: Sender -> Header -> Summary
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
    = ShareName MessageId Shortened
    deriving (Eq, Ord)


data HandshakeId
    = HandshakeId Sender FirstMessage


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
    = Initiator MyEphemeral InitiatorHandshake
    | ResponderSentEncryptedES MyEphemeral


data SecondShake
    = SecondShake MyEphemeral EncryptedEphemeral


data InitiatorHandshake
    = SentPlainE
    | ReceivedEncryptedE EncryptedEphemeral


newtype MyEphemeral
    = MyEphemeral (Dh.KeyPair Curve25519)


newtype EncryptedEphemeral
    = EncryptedEphemeral Bl.ByteString


data AuthStatus
    = AwaitingAuthCode


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

    SetBlobM body metadata ->
        updateReady model $ setBlobUpdate body metadata

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

    RandomGenM gen ->
        updateInit model $ updateOnRandomGen gen

    AppendedStrictM ->
        updateReady model $ fileAssembledUpdate

    StrictFileContentsM path contents ->
        updateReady model $ strictFileContentsUpdate path contents

    FileHandleM path handle ->
        updateReady model $ updateOnFileHandle path handle

    ChunkFromHandleM path raw ->
        updateReady model $ updateOnChunkFromHandle path raw

    SetCodeM body q ->
        updateReady model $ updateOnNewRawWasm body q


updateOnNewRawWasm
    :: Bl.ByteString
    -> Q
    -> Ready
    -> (Output, State)
updateOnNewRawWasm body q ready =
    let
    hash = hash32 body
    path = makeBlobPath (root ready) hash
    (wasmO, wasmS) = updateOnNewWasm hash ready
    in
    ( BatchO
        [ wasmO
        , WriteFileStrictO path $ Bl.toStrict body
        , BytesInQO q Bl.empty
        ] 
    , wasmS
    )


updateOnChunkFromHandle
    :: FilePath
    -> B.ByteString
    -> Ready
    -> (Output, State)
updateOnChunkFromHandle path raw ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case sendingBlob ready of
    NoJobs ->
        pass

    Jobs (AwaitingHandle _ _) _ ->
        pass

    Jobs (Chunking handle hash tos) _ ->
        if makeBlobPath (root ready) hash == path then

        let
        finalChunk = rawLen == plainChunkLen - 3
        rawLen = B.length raw
        chunk =
            if finalChunk then
            mconcat
            [ B.singleton 0
            , B.replicate 2 0
            , raw
            ]
            else
            mconcat
            [ B.singleton 1
            , encodeUint16 rawLen
            , raw
            , B.replicate (plainChunkLen - 3 - rawLen) 0
            ]
        (sendO, sendModel) =
            foldr
                (sendBlobChunk chunk)
                (DoNothingO, ReadyS ready)
                (map (\(Sender s) -> s) tos)
        in
        ( BatchO
            [ sendO
            , if finalChunk then
              CloseFileO handle
              else
              ReadFromHandleO path (plainChunkLen - 3) handle
            ]
        , sendModel
        )

        else
        pass


updateOnFileHandle
    :: FilePath
    -> Io.Handle
    -> Ready
    -> (Output, State)
updateOnFileHandle path handle ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case sendingBlob ready of
    NoJobs ->
        pass

    Jobs (AwaitingHandle hash tos) waiting ->
        if makeBlobPath (root ready) hash == path then
        ( ReadFromHandleO path (plainChunkLen - 3) handle
        , ReadyS $ ready
            { sendingBlob =
                Jobs (Chunking handle hash tos) waiting
            }
        )

        else
        pass

    Jobs (Chunking _ _ _) _ ->
        pass


strictFileContentsUpdate
    :: FilePath
    -> B.ByteString
    -> Ready
    -> (Output, State)
strictFileContentsUpdate path contents ready =
    case fileToSendToServerUpdate path contents ready of
    Just r1 ->
        r1

    Nothing ->
        messageFileUpdate path (Bl.fromStrict contents) ready
            

fileToSendToServerUpdate
    :: FilePath
    -> B.ByteString
    -> Ready
    -> Maybe (Output, State)
fileToSendToServerUpdate path contents ready =
    case readingToSendToServer ready of
    Nothing ->
        Nothing

    Just code ->
        if path == makeBlobPath (root ready) code then
        let
        newReady = ready { readingToSendToServer = Nothing }
        in
        Just
        ( BytesInQO toServerQ $ Bl.fromStrict contents
        , ReadyS newReady
        )
        else
        Nothing


messageForClick
    :: FilePath
    -> Bl.ByteString
    -> Ready
    -> Maybe (Output, State)
messageForClick path raw ready =
    case readingMessageOnClick ready of
    Nothing ->
        Nothing

    Just messageId ->
        if path == makeMessagePath (root ready) messageId then
        case P.eitherResult $ P.parse (listP diffP) raw of
        Left err ->
            Just $ corruptMessage err messageId

        Right diffs ->
            let
            newReady =
                ready
                    { pageR = Writer messageId diffs
                    , readingMessageOnClick = Nothing
                    }
            in
            Just (dumpView newReady, ReadyS newReady)

        else
        Nothing


messageFileUpdate
    :: FilePath
    -> Bl.ByteString
    -> Ready
    -> (Output, State)
messageFileUpdate path raw ready =
    case extractingMessageHelp path raw ready of
    Nothing ->
        case extractingReferencesHelp path raw ready of
        Nothing ->
            case messageForClick path raw ready of
            Nothing ->
                (DoNothingO, ReadyS ready)

            Just done ->
                done

        Just done ->
            done

    Just done ->
        done


extractingReferencesHelp path contents ready =
    case extractingReferences ready of
    Nothing ->
        Nothing

    Just (messageId, hash) ->
        if path == makeMessagePath (root ready) messageId then
        case P.eitherResult $ P.parse (listP diffP) contents of
        Left err ->
            Just $ corruptMessage err messageId

        Right diffs ->
            case constructReferences diffs of
            Nothing ->
                Just $
                    corruptMessage
                        "couldn't make references from diffs"
                        messageId

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


downloadPath :: RootPath -> Integer -> FilePath
downloadPath root unique =
    tempPath root </> show unique


constructReferences :: [Diff] -> Maybe (Set.Set Hash32)
constructReferences diffs =
    constructReferencesHelp Set.empty "" diffs


constructReferencesHelp
    :: Set.Set Hash32
    -> B.ByteString
    -> [Diff]
    -> Maybe (Set.Set Hash32)
constructReferencesHelp accum lastEncoded remaining =
    case remaining of
    [] ->
        Just accum

    r:emaining ->
        let
        encoded = constructMessageHelp r lastEncoded
        actualHash = hash8 encoded
        parsed = P.parse headerP $ Bl.fromStrict encoded
        in
        if actualHash == integrity r then
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

corruptMessage :: String -> MessageId -> (Output, State)
corruptMessage err messageId =
            ( ErrorO $
                mconcat
                [ "corrupt message file:\n"
                , "messageId: "
                , show messageId
                , "\nparse error: "
                , err
                ]
            , FailedS
            )


diffP :: P.Parser Diff
diffP = do
    start <- uint32P
    end <- uint32P
    insert <- sizedBytesP 
    integrity <- hash8P
    author <- diffAuthorP
    return $ Diff{start, end, insert, integrity, author}


hash8P :: P.Parser Hash8
hash8P = do
    raw <- P.take 8
    return $ Hash8 raw


sizedBytesP :: P.Parser B.ByteString
sizedBytesP = do
    len <- uint32P
    bytes <- P.take len
    return bytes


extractingMessageHelp path raw ready =
    case extractingMessage ready of
    Just (messageId, from, header) ->
        if path == makeMessagePath (root ready) messageId then
        case P.eitherResult $ P.parse (listP diffP) raw of
        Left err ->
            Just $ corruptMessage err messageId

        Right diffs ->
            case constructMessage diffs of
            Nothing ->
                Just $
                    corruptMessage
                        "couldn't construct from diffs"
                        messageId

            Just oldEncoded ->
                let
                diff =
                    makeDiff
                        (NotMe from)
                        oldEncoded
                        (encodeHeader header)
                summary = summarize from header
                saveDiff =
                    WriteFileStrictO
                        (makeMessagePath (root ready) messageId)
                        (encodeDiffs $ diff : diffs)
                newReady =
                    ready
                        { summaries =
                            Map.insert
                                messageId
                                summary
                                (summaries ready)
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


constructMessage :: [Diff] -> Maybe B.ByteString
constructMessage diffs =
    let
    candidate = foldr constructMessageHelp "" diffs
    actualHash = hash8 candidate
    in
    case getLastHash diffs of
    Nothing ->
        Nothing

    Just expectedHash ->
        if actualHash == expectedHash then
            Just candidate

        else
            Nothing


getLastHash :: [Diff] -> Maybe Hash8
getLastHash diffs =
    case reverse diffs of
        [] ->
            Nothing

        diff:_ ->
            Just $ integrity diff


constructMessageHelp :: Diff -> B.ByteString -> B.ByteString
constructMessageHelp diff old =
    let
    firstBit = B.take (start diff) old
    lastBit = B.reverse $ B.take (end diff) $ B.reverse old
    in
    firstBit <> insert diff <> lastBit


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



-- Assocated data
authCodeA :: B.ByteString
authCodeA =
    B.pack [197, 154, 22, 2, 21, 159, 38, 105, 240, 15, 236, 142, 31, 124, 100, 71, 22, 117, 69, 163, 39, 221, 135, 100, 193, 244, 134, 63, 28, 226, 89, 31]


fingerprintSalt =
    B.pack
        [121, 42, 53, 200, 120, 148, 151, 77, 190, 181, 194, 24, 139, 196, 215, 179]


fingerprintLength =
    8


makeFingerprint
    :: Shortened
    -> Dh.PublicKey Curve25519
    -> Either String Fingerprint
makeFingerprint (Shortened shortenedLazy) key =
    let
    asBytes = Noise.convert $ Dh.dhPubToBytes key
    shortened = Bl.toStrict shortenedLazy
    hashE = Argon2.hash
        argonFingerprint
        (asBytes <> shortened)
        fingerprintSalt
        fingerprintLength
    in
    case hashE of
    CryptoPassed hash ->
        Right $ Fingerprint $ Bl.fromStrict hash

    CryptoFailed err ->
        Left $ show err


argonFingerprint :: Argon2.Options
argonFingerprint =
    Argon2.Options
        { iterations = 1000
        , memory = 256 * 1024
        , parallelism = 4
        , variant = Argon2.Argon2id
        , version = Argon2.Version13
        }


encodeUint32 :: Int -> B.ByteString
encodeUint32 i =
    B.pack $ map (encodeUintHelp i) (take 4 [0..])


encodeUintHelp :: Int -> Int -> Word8
encodeUintHelp int counter =
    fromIntegral $ int `Bits.shiftR` (counter * 8) Bits..&. 0xFF


encodeUint16 :: Int -> B.ByteString
encodeUint16 i =
    B.pack $ map (encodeUintHelp i) [0, 1]


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


newtype MyStaticNoise
    = MyStaticNoise (Dh.KeyPair Curve25519)


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
                    , authStatus = AwaitingAuthCode
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
                    , pageR = Messages
                    , awaitingCrypto = []
                    , sendingBlob = NoJobs
                    , readingMessageOnClick = Nothing
                    , staticNoiseKeys = Nothing
                    , userId = Nothing
                    , lookingUpShortened = Map.empty
                    , secretSigningKey = Nothing
                    }
            in
            ( BatchO
                [ BytesInQO toServerQ $ Bl.singleton 1
                , dumpCache ready
                ]
            , ReadyS ready
            )

        (False, False) ->
            pass

    GettingKeysFromFileI _ _ _ _ ->
        pass


sendToServer :: ToServer -> Output
sendToServer msg =
    BytesInQO toServerQ $ encodeToServer msg


type PublicNoise
    = Dh.PublicKey Curve25519


encodePublicNoise :: PublicNoise -> Bl.ByteString
encodePublicNoise pub =
    Bl.fromStrict $ Ba.convert $ Dh.dhPubToBytes pub


data ToServer
    = SignedAuthCodeT Ed.PublicKey Ed.Signature
    | GetPricesT
    | ShortenIdT PaymentDetails PublicNoise Argon2.Options
    | UploadBlobT PaymentDetails BlobId Blob
    | DownloadBlobT BlobId
    | SendMessageT PaymentDetails Recipient InboxMessage
    | DeleteMessageT Sender InboxMessage
    | LookupShortenedT Shortened


signPubKeyP :: P.Parser Ed.PublicKey
signPubKeyP = do
    raw <- P.take Ed.publicKeySize
    case Ed.publicKey raw of
        CryptoFailed err ->
            fail $ show err

        CryptoPassed key ->
            return key


shortenableLength :: Int
shortenableLength =
    42


shortenableP :: P.Parser Shortenable
shortenableP = do
    raw <- P.take shortenableLength
    return $ Shortenable $ Bl.fromStrict raw


newtype Shortenable
    = Shortenable Bl.ByteString


encodeToServer :: ToServer -> Bl.ByteString
encodeToServer toServer =
    case toServer of
    SignedAuthCodeT publicKey signature ->
        mconcat
        [ Bl.singleton 0
        , Bl.fromStrict $ Ba.convert publicKey
        , Bl.fromStrict $ Ba.convert signature
        ]

    GetPricesT ->
        Bl.singleton 1

    ShortenIdT paymentDetails publicNoise argonOptions ->
        mconcat
        [ Bl.singleton 2
        , encodePaymentDetails paymentDetails
        , encodePublicNoise publicNoise
        , encodeArgonOptions argonOptions
        ]

    UploadBlobT paymentDetails blobId blob ->
        mconcat
        [ Bl.singleton 3
        , encodePaymentDetails paymentDetails
        , encodeBlobId blobId
        , Bl.fromStrict $ encodeBlob blob
        ]

    DownloadBlobT blobId ->
        Bl.singleton 4 <> encodeBlobId blobId

    SendMessageT paymentDetails recipient inboxMessage ->
        mconcat
        [ Bl.singleton 5
        , encodePaymentDetails paymentDetails
        , encodeRecipient recipient
        , encodeInboxMessage inboxMessage
        ]

    DeleteMessageT sender inboxMessage ->
        mconcat
        [ Bl.singleton 6
        , encodeSender sender
        , encodeInboxMessage inboxMessage
        ]

    LookupShortenedT (Shortened shortened) ->
        Bl.singleton 7 <> shortened


encodeInboxMessage :: InboxMessage -> Bl.ByteString
encodeInboxMessage (InboxMessage i) =
    i


encodeSender :: Sender -> Bl.ByteString
encodeSender (Sender pubKey) =
    Bl.fromStrict $ Ba.convert pubKey


encodeRecipient :: Recipient -> Bl.ByteString
encodeRecipient (Recipient pubKey) =
    Bl.fromStrict $ Ba.convert pubKey


encodePaymentDetails :: PaymentDetails -> Bl.ByteString
encodePaymentDetails
    (PaymentDetails
        (OldTransaction old)
        (NewTransaction new)
        (AccountsSignature sig)) =

    mconcat
    [ encodeTransaction old
    , encodeTransaction new
    , encodeSignature sig
    ]


encodeTransaction :: Transaction -> Bl.ByteString
encodeTransaction (Transaction amount time balance payment hash) =
    mconcat
    [ encodeAmount amount
    , Bl.fromStrict $ encodeTime time
    , encodeBalance balance
    , encodePayment payment
    , encodeHash hash
    ]


encodeBlobId :: BlobId -> Bl.ByteString
encodeBlobId (BlobId blobId) =
    blobId


encodeArgonOptions :: Argon2.Options -> Bl.ByteString
encodeArgonOptions argon =
    let
    convert = Bl.fromStrict . encodeUint32 . fromIntegral
    in
    mconcat
    [ Bl.singleton 0
    , convert $ Argon2.iterations argon
    , convert $ Argon2.memory argon
    , convert $ Argon2.parallelism argon
    ]


encodeSignature :: Ed.Signature -> Bl.ByteString
encodeSignature sig =
    Bl.fromStrict $ Ba.convert sig


encodeAmount :: Amount -> Bl.ByteString
encodeAmount (Amount m) =
    encodeMoney m


encodeBalance :: Balance -> Bl.ByteString
encodeBalance (Balance m) =
    encodeMoney m


encodeMoney :: Money -> Bl.ByteString
encodeMoney (Money m) =
    Bl.fromStrict $ encodeUint32 m


encodePayment :: Payment -> Bl.ByteString
encodePayment payment =
    case payment of
    PaymentToServer hash ->
        Bl.singleton 0 <> encodeHash hash

    AccountTopUp (TopUpId t) ->
        Bl.singleton 1 <> t


encodeHash :: Hash32 -> Bl.ByteString
encodeHash (Hash32 h) =
    h


newtype InboxMessage
    = InboxMessage Bl.ByteString


data PaymentDetails
    = PaymentDetails OldTransaction NewTransaction AccountsSignature


data Transaction
    = Transaction Amount PosixSeconds Balance Payment Hash32


newtype TopUpId
    = TopUpId Bl.ByteString


newtype Balance
    = Balance Money


newtype Money
    = Money Int
    deriving Eq


newtype BlobId
    = BlobId Bl.ByteString


newtype Amount
    = Amount Money
    deriving Eq


data Payment
    = PaymentToServer Hash32
    | AccountTopUp TopUpId


newtype OldTransaction
    = OldTransaction Transaction


newtype NewTransaction
    = NewTransaction Transaction


newtype AccountsSignature
    = AccountsSignature Ed.Signature


newtype Sender
    = Sender Ed.PublicKey
    deriving (Eq, Show)


instance Ord Sender where
    compare a b =
        compare (encodeSender a) (encodeSender b)


restartingTcpUpdate :: Ready -> (Output, State)
restartingTcpUpdate ready =
    ( DoNothingO
    , ReadyS $ ready { authStatus = AwaitingAuthCode }
    )


fromServerUpdate :: Bl.ByteString -> Ready -> (Output, State)
fromServerUpdate raw ready =
    case P.eitherResult $ P.parse fromServerP raw of
    Left err ->
        logErr ready $ T.concat
            [ "could not parse server message: "
            , T.pack err
            ]

    Right (AcknowledgementF code) ->
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
            ( ReadFileStrictO $ makeBlobPath (root ready) a
            , ReadyS newReady
            )
            else
            (DoNothingO, ReadyS ready)

    Right (InboxMessageF sender message) ->
        messageInUpdate sender message ready

    Right (ShortenedF shortened) ->
        newUserIdUpdate shortened ready

    Right (PricesF shortening blob message) ->
        ( BytesInQO toFrontendQ $
            mconcat
            [ Bl.singleton 9
            , encodeShorteningPrice shortening
            , encodeBlobPrice blob
            , encodeMessagePrice message
            ]
        , ReadyS ready
        )

    Right (AuthCodeToSignF (AuthCode authCode)) ->
        case secretSigningKey ready of
        Nothing ->
            (DoNothingO, ReadyS ready)

        Just secretSign ->
            let
            publicKey = Ed.toPublic secretSign
            toSign = authCodeA <> authCode
            signature = Ed.sign secretSign publicKey toSign
            in
            ( sendToServer $ SignedAuthCodeT publicKey signature
            , ReadyS ready
            )


encodeShorteningPrice :: ShorteningPrice -> Bl.ByteString
encodeShorteningPrice (ShorteningPrice s) =
    encodeMoney s


encodeBlobPrice :: BlobPrice -> Bl.ByteString
encodeBlobPrice (BlobPrice b) =
    encodeMoney b


encodeMessagePrice :: MessagePrice -> Bl.ByteString
encodeMessagePrice (MessagePrice p) =
    encodeMoney p


newUserIdUpdate :: Shortened -> Ready -> (Output, State)
newUserIdUpdate shortened ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case staticNoiseKeys ready of
    Nothing ->
        pass

    Just (MyStaticNoise (_, pub)) ->
        case makeFingerprint shortened pub of
        Left err ->
            ( ErrorO $ "couldn't make fingerprint: " ++ err
            , FailedS
            )

        Right fingerprint ->
            let
            newReady =
                ready { userId = Just (fingerprint, shortened) }
            in
            ( dumpCache newReady
            , ReadyS newReady
            )


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
    :: Sender
    -> InboxMessage
    -> Ready
    -> (Output, State)
messageInUpdate from (InboxMessage raw) ready =
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
    :: Sender
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

    Just (Initiator _ _ ) ->
        (DoNothingO, ReadyS ready)

    Just (ResponderSentEncryptedES myEphemeral) ->
        case staticNoiseKeys ready of
        Nothing ->
            (DoNothingO, ReadyS ready)

        Just staticKeys ->
            transportUpdateHelp
                from
                ready
                firstMessage
                thisMessage
                myEphemeral
                staticKeys


transportUpdateHelp
    :: Sender
    -> Ready
    -> FirstMessage
    -> Bl.ByteString
    -> MyEphemeral
    -> MyStaticNoise
    -> (Output, State)
transportUpdateHelp
    from
    ready
    (FirstMessage first)
    encrypted
    myEphemeral
    myStatic =

    case Map.lookup from $ whitelist ready of
    Nothing ->
        (DoNothingO, ReadyS ready)

    Just (_, _, theirStatic) ->
        let
        options = responderOptions myStatic theirStatic myEphemeral
        noise0 = Noise.noiseState options noiseKK :: NoiseState
        in
        case
            Noise.readMessage
            (Noise.convert $ Bl.toStrict first)
            noise0
        of
        Noise.NoiseResultNeedPSK _ ->
            (DoNothingO, ReadyS ready) -- shouldn't happen with KK

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
    -> Sender
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


    ( Just untrusted, Just (_, _, TheirStaticNoise trusted)) ->
        if untrusted == trusted then
        gotValidPlaintextUpdate from plain ready

        else
        unsolicitedMessage ready from


unsolicitedMessage :: Ready -> Sender -> (Output, State)
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
    :: Sender
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


parsedPlainUpdate :: Sender -> Plaintext -> Ready -> (Output, State)
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
    shares <- listP senderP
    subject <- sizedStringP
    mainBox <- sizedStringP
    blobs <- listP blobP
    wasm <- hash32P
    return
        Header{time, shares, subject, mainBox, blobs, wasm}


assembledUpdate
    :: Sender
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
            diff = makeDiff (NotMe from) "" encoded
            newReady =
                ready
                    { summaries =
                        Map.insert
                            messageId
                            summary
                            (summaries ready)
                    }
            in
            ( WriteFileStrictO
                (makeMessagePath (root ready) messageId)
                (encodeDiffs [diff])
            , ReadyS newReady
            )

        Just _ ->
            ( ReadFileStrictO (makeMessagePath (root ready) messageId)
            , ReadyS $
                ready
                    { extractingMessage =
                        Just (messageId, from, header)
                    }
            )

    Referenced messageId hash ->
        referencedHelp messageId hash ready


encodeDiffs :: [Diff] -> B.ByteString
encodeDiffs =
    mconcat . map encodeDiff


encodeDiff :: Diff -> B.ByteString
encodeDiff diff =
    mconcat
    [ encodeUint32 $ start diff
    , encodeUint32 $ end diff
    , insert diff
    , encodeHash8 $ integrity diff
    , encodeDiffAuthor $ author diff
    ]


encodeShortened :: Shortened -> Bl.ByteString
encodeShortened (Shortened s) =
    s


encodeDiffAuthor :: DiffAuthor -> B.ByteString
encodeDiffAuthor author =
    case author of
    NotMe sender ->
        B.singleton 0 <> Bl.toStrict (encodeSender sender)

    Me ->
        B.singleton 1


diffAuthorP :: P.Parser DiffAuthor
diffAuthorP =
    P.choice
        [ do
            _ <- P.word8 0
            sender <- senderP
            return $ NotMe sender
        , do
            _ <- P.word8 1
            return Me
        ]


encodeHash8 :: Hash8 -> B.ByteString
encodeHash8 (Hash8 hash) =
    hash


referencedHelp
    :: MessageId
    -> Hash32
    -> Ready
    -> (Output, State)
referencedHelp messageId blob ready =
    ( ReadFileStrictO (makeMessagePath (root ready) messageId)
    , ReadyS $ ready
        { extractingReferences = Just (messageId, blob) }
    )


encodeHeader :: Header -> B.ByteString
encodeHeader header =
    mconcat
        [ encodeTime $ time header
        , encodeList (Bl.toStrict . encodeSender) (shares header)
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
        , encodeSizedString $ filenameB blob
        , encodeUint32 $ size blob
        , encodeSizedString $ mime blob
        ]


encodeTime :: PosixSeconds -> B.ByteString
encodeTime (PosixSeconds time) =
    encodeUint32 time


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
        , author :: DiffAuthor
        }
        deriving (Eq)


data DiffAuthor
    = NotMe Sender
    | Me
    deriving (Eq)


makeDiff :: DiffAuthor -> B.ByteString -> B.ByteString -> Diff
makeDiff author old new =
    let
    start = countIdentical old new
    end = countIdentical (B.reverse old) (B.reverse new)
    insert = B.drop start $ B.reverse $ B.drop end $ B.reverse new
    integrity = hash8 insert
    in
    Diff { start, end, insert, integrity, author }


countIdentical :: B.ByteString -> B.ByteString -> Int
countIdentical a b =
     countIdenticalHelp (B.zip a b) 0


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


timeP :: P.Parser PosixSeconds
timeP = do
    t <- uint32P
    return $ PosixSeconds t


newtype PosixSeconds
    = PosixSeconds Int


blobP :: P.Parser Blob
blobP = do
    hashB <- hash32P
    filenameB <- sizedStringP
    size <- uint32P
    mime <- sizedStringP
    return $ Blob{hashB, filenameB, size, mime}



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
        , filenameB :: T.Text
        , size :: Int
        , mime :: T.Text
        }
        deriving (Eq, Ord)


data Wasm
    = Wasm
        Hash32
        Integer -- size
        T.Text  -- file name
        deriving (Eq)


makeBlobPath :: RootPath -> Hash32 -> FilePath
makeBlobPath root hash =
    blobsPath root </> showHash hash


blobsPath :: RootPath -> FilePath
blobsPath (RootPath root) =
    root </> "blobs"


secondHandshakesUpdate
    :: Sender
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

        Initiator myEphemeral SentPlainE ->
            Initiator myEphemeral $ ReceivedEncryptedE $
                EncryptedEphemeral newMsg

        Initiator _ (ReceivedEncryptedE _) ->
            oldShake


make2ndShakeMap
    :: [SecondHandshake]
    -> Sender
    -> Map.Map HandshakeId Bl.ByteString
make2ndShakeMap shakes sender =
    Map.fromList $ map (make2ndShakeHelp sender) shakes


make2ndShakeHelp
    :: Sender
    -> SecondHandshake
    -> (HandshakeId, Bl.ByteString)
make2ndShakeHelp sender (SecondHandshake publicEphemeral msg) =
    (HandshakeId sender publicEphemeral, msg)


firstHandshakesUpdate
    :: Sender
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
    case staticNoiseKeys ready of
    Nothing ->
        pass

    Just myStatic ->
        case Map.lookup from $ whitelist ready of
        Nothing ->
            pass

        Just (_, _, theirs) ->
            case sendNoiseKK2s myEphemerals msgs myStatic theirs of
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
    -> Sender
    -> Map.Map HandshakeId Handshake
makeResponderShakes myNewEphemerals firstShakes from =
    Map.fromList $ map (makeResponderShake from) $
        zip myNewEphemerals firstShakes


makeResponderShake
    :: Sender
    -> (MyEphemeral, FirstMessage)
    -> (HandshakeId, Handshake)
makeResponderShake from (myEphemeral, theirEphemeral) =
    ( HandshakeId from theirEphemeral
    , ResponderSentEncryptedES myEphemeral
    )


sendNoiseKK2s
    :: [MyEphemeral]
    -> [FirstMessage]
    -> MyStaticNoise
    -> TheirStaticNoise
    -> Either T.Text Output
sendNoiseKK2s myEphemerals firsts myStatic theirStatic =
    let
    max2nd = 166
    zipped = zip myEphemerals firsts
    first166 = take max2nd zipped
    second166 = take max2nd $ drop max2nd zipped
    third = drop (2 * max2nd) zipped
    eitherChunks =
        map
            (make2ndNoise myStatic theirStatic)
            [first166, second166, third]
    in
    case allRight eitherChunks of
    Left err ->
        Left err

    Right chunks ->
        Right $ BatchO $ map (BytesInQO toServerQ) chunks


make2ndNoise
    :: MyStaticNoise
    -> TheirStaticNoise
    -> [(MyEphemeral, FirstMessage)]
    -> Either T.Text Bl.ByteString
make2ndNoise myStatic theirStatic noises1 =
    let
    eitherNoises = map (makeOne2ndNoise myStatic theirStatic) noises1
    in
    case allRight eitherNoises of
    Left err ->
        Left err

    Right noises2 ->
        Right $ mconcat
            [ Bl.singleton 3
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
    :: MyStaticNoise
    -> TheirStaticNoise
    -> MyEphemeral
    -> Noise.HandshakeOpts Curve25519
responderOptions
    (MyStaticNoise myStatic)
    (TheirStaticNoise theirStatic)
    (MyEphemeral myEphemeral) =

    Noise.setLocalStatic (Just myStatic) .
    Noise.setRemoteStatic (Just theirStatic) .
    Noise.setLocalEphemeral (Just myEphemeral) $
    Noise.defaultHandshakeOpts Noise.ResponderRole ""


initiatorOptions
    :: MyStaticNoise
    -> TheirStaticNoise
    -> MyEphemeral
    -> Noise.HandshakeOpts Curve25519
initiatorOptions
    (MyStaticNoise myStatic)
    (TheirStaticNoise theirStatic)
    (MyEphemeral myEphemeral) =

    Noise.setLocalStatic (Just myStatic) .
    Noise.setRemoteStatic (Just theirStatic) .
    Noise.setLocalEphemeral (Just myEphemeral) $
    Noise.defaultHandshakeOpts Noise.InitiatorRole ""


newtype TheirStaticNoise
    = TheirStaticNoise (Dh.PublicKey Curve25519)


type NoiseState
    = Noise.NoiseState ChaChaPoly1305 Curve25519 BLAKE2s


makeOne2ndNoise
    :: MyStaticNoise
    -> TheirStaticNoise
    -> (MyEphemeral, FirstMessage)
    -> Either T.Text Bl.ByteString
makeOne2ndNoise
    myStatic
    theirStatic
    (myEphemeral, FirstMessage refKey) =

    let
    options = responderOptions myStatic theirStatic myEphemeral
    noise0 = Noise.noiseState options noiseKK :: NoiseState
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
    WriteFileO
        (cachePath $ root ready)
        (encodeCache $ getCache ready)


encodeCache :: MemCache -> Bl.ByteString
encodeCache memCache =
     mconcat
        [ encodeCryptoKeys $ keys memCache
        , encodeHandshakes $ handshakesM memCache
        , encodeWhitelist $ whitelistM memCache
        , Bl.fromStrict $
            encodeList
                (Bl.toStrict . encodeSummary)
                (Map.toList $ summariesM memCache)
        , encodeVarInt $ counterM memCache
        , Bl.fromStrict $
            encodeList encodeHash32 $
            waitForAcknowledgeM memCache
        ]


encodeSummary :: (MessageId, Summary) -> Bl.ByteString
encodeSummary (messageId, summary) =
    mconcat
    [ encodeMessageId messageId
    , Bl.fromStrict $ encodeSizedString $ subjectS summary
    , Bl.fromStrict $ encodeTime $ timeS summary
    , encodeSender $ authorS summary
    ]


encodeMessageId :: MessageId -> Bl.ByteString
encodeMessageId (MessageId id_) =
    Bl.fromStrict id_


encodeWhitelist :: Whitelist -> Bl.ByteString
encodeWhitelist whitelist =
    let
    asList = Map.toList whitelist
    in
    (Bl.fromStrict $ encodeUint32 (length asList)) <>
    mconcat (map encodeOneWhite asList)


encodeOneWhite
    :: (Sender, (Fingerprint, Shortened, TheirStaticNoise))
    -> Bl.ByteString
encodeOneWhite (sender, (fingerprint, shortened, noiseStatic)) =
    mconcat
        [ encodeSender sender
        , encodeFingerprint fingerprint
        , encodeShortened shortened
        , encodeTheirStatic noiseStatic
        ]


encodeMaybe :: Maybe a -> (a -> Bl.ByteString) -> Bl.ByteString
encodeMaybe a encoder =
    case a of
    Nothing ->
        Bl.singleton 0

    Just justa ->
        Bl.singleton 1 <> encoder justa


encodeCryptoKeys :: Maybe MyStaticNoise -> Bl.ByteString
encodeCryptoKeys maybeKeys =
    encodeMaybe maybeKeys encodeMyStatic


encodeMyStatic :: MyStaticNoise -> Bl.ByteString
encodeMyStatic (MyStaticNoise (sec, _)) =
    Bl.fromStrict $ Ba.convert $ Dh.dhSecToBytes sec


encodeFingerprint :: Fingerprint -> Bl.ByteString
encodeFingerprint (Fingerprint fingerprint) =
    fingerprint


cachePath :: RootPath -> FilePath
cachePath (RootPath root) =
    root </> "memCache"


getCache :: Ready -> MemCache
getCache ready =
    MemCache
        { keys = staticNoiseKeys ready
        , handshakesM = handshakes ready
        , whitelistM = whitelist ready
        , summariesM = summaries ready
        , counterM = counter ready
        , waitForAcknowledgeM = waitForAcknowledge ready
        , awaitingCryptoM = awaitingCrypto ready
        , secretSigningKeyM = secretSigningKey ready
        }


encodeHandshakes :: Map.Map HandshakeId Handshake -> Bl.ByteString
encodeHandshakes =
    mconcat . map encodeHandshake . Map.toList


encodeHandshake :: (HandshakeId, Handshake) -> Bl.ByteString
encodeHandshake (id_, shake) =
    encodeHandshakeId id_ <> encodeHandshakeShake shake


encodeHandshakeId :: HandshakeId -> Bl.ByteString
encodeHandshakeId (HandshakeId (Sender u) (FirstMessage e)) =
    Bl.fromStrict (Ba.convert u) <> e


encodeHandshakeShake :: Handshake -> Bl.ByteString
encodeHandshakeShake h =
    case h of
    Initiator myEphemeral i ->
        mconcat
            [ Bl.singleton 0
            , encodeMyEphemeral myEphemeral
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


encodeMyEphemeral :: MyEphemeral -> Bl.ByteString
encodeMyEphemeral (MyEphemeral (secret, _)) =
    Bl.fromStrict $ Ba.convert $ Dh.dhSecToBytes secret


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
            _ <- P.word8 1
            shorteningPrice <- shorteningPriceP
            blobPrice <- blobPriceP
            messagePrice <- messagePriceP
            return $
                PricesF shorteningPrice blobPrice messagePrice
        , do
            _ <- P.word8 2
            shortened <- shortenedP
            return $ ShortenedF shortened
        , do
            _ <- P.word8 3
            blobId <- blobIdP
            blob <- blobP
            return $ RequestedBlobF blobId blob
        , do
            _ <- P.word8 4
            hash <- hash32P
            return $ AcknowledgementF hash
        , do
            _ <- P.word8 5
            sender <- senderP
            inboxMessage <- inboxMessageP
            return $ InboxMessageF sender inboxMessage
        , do
            _ <- P.word8 6
            transaction <- transactionP
            return $ NewTransactionF transaction
        , do
            _ <- P.word8 7
            shortened <- shortenedP
            signPubKey <- signPubKeyP
            shortenable <- shortenableP
            return $ LongIdForShortIdF shortened signPubKey shortenable
        ]
    P.endOfInput
    return msg


messagePriceP :: P.Parser MessagePrice
messagePriceP =
    fmap MessagePrice moneyP


authCodeLength :: Int
authCodeLength =
    32


shorteningPriceP :: P.Parser ShorteningPrice
shorteningPriceP =
    fmap ShorteningPrice moneyP


authCodeP :: P.Parser AuthCode
authCodeP = do
    raw <- P.take authCodeLength
    return $ AuthCode raw


transactionP :: P.Parser Transaction
transactionP = do
    amount <- amountP
    time <- timeP
    balance <- balanceP
    payment <- paymentP
    hash <- hash32P
    return $ Transaction amount time balance payment hash


amountP :: P.Parser Amount
amountP = do
    raw <- moneyP
    return $ Amount raw


inboxMessageLength :: Int
inboxMessageLength =
    32


inboxMessageP :: P.Parser InboxMessage
inboxMessageP = do
    raw <- P.take inboxMessageLength
    return $ InboxMessage $ Bl.fromStrict raw


senderP :: P.Parser Sender
senderP = do
    raw <- pubEdKeyP
    return $ Sender raw


pubEdKeyP :: P.Parser Ed.PublicKey
pubEdKeyP = do
    raw <- P.take Ed.publicKeySize
    case Ed.publicKey raw of
        CryptoFailed err ->
            fail $ show err

        CryptoPassed key ->
            return key


blobIdLength :: Int
blobIdLength =
    32


blobIdP :: P.Parser BlobId
blobIdP = do
    raw <- P.take blobIdLength
    return $ BlobId $ Bl.fromStrict raw


balanceP :: P.Parser Balance
balanceP = do
    raw <- moneyP
    return $ Balance raw


moneyP :: P.Parser Money
moneyP = do
    raw <- uint32P
    return $ Money raw


paymentP :: P.Parser Payment
paymentP =
    P.choice
    [ do
        _ <- P.word8 0
        hash <- hash32P
        return $ PaymentToServer hash
    , do
        _ <- P.word8 1
        topUpId <- topUpIdP
        return $ AccountTopUp topUpId
    ]


topUpIdLength :: Int
topUpIdLength =
    32


topUpIdP :: P.Parser TopUpId
topUpIdP = do
    raw <- P.take topUpIdLength
    return $ TopUpId $ Bl.fromStrict raw


theirStaticP :: P.Parser TheirStaticNoise
theirStaticP = do
    raw <- P.take 32
    case Dh.dhBytesToPub $ Noise.convert raw of
        Nothing ->
            fail "could not parse their static key"

        Just dhKey ->
            return $ TheirStaticNoise dhKey


blobPriceP :: P.Parser BlobPrice
blobPriceP =
    fmap BlobPrice moneyP


data FromServer
    = AuthCodeToSignF AuthCode
    | PricesF ShorteningPrice BlobPrice MessagePrice
    | ShortenedF Shortened
    | RequestedBlobF BlobId Blob
    | AcknowledgementF Hash32
    | InboxMessageF Sender InboxMessage
    | NewTransactionF Transaction
    | LongIdForShortIdF Shortened Ed.PublicKey Shortenable


newtype AuthCode
    = AuthCode B.ByteString


newtype MessagePrice
    = MessagePrice Money


newtype BlobPrice
    = BlobPrice Money


newtype ShorteningPrice
    = ShorteningPrice Money


newtype AcknowledgementCode
    = AcknowledgementCode Integer
    deriving (Eq, Ord)


data Ciphertext
    = FirstHandshakes [FirstMessage]
    | SecondHandshakes [SecondHandshake]
    | TransportC FirstMessage Bl.ByteString


data SecondHandshake
    = SecondHandshake FirstMessage Bl.ByteString


encodeTheirStatic :: TheirStaticNoise -> Bl.ByteString
encodeTheirStatic (TheirStaticNoise key) =
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
    = UserId Shortened Fingerprint
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


newtype Shortened
    = Shortened Bl.ByteString
    deriving (Eq, Ord, Show)


shortenedLength :: Int
shortenedLength =
    32


shortenedP :: P.Parser Shortened
shortenedP = do
    raw <- P.take shortenedLength
    return $ Shortened $ Bl.fromStrict raw


instance Eq UserId where
    (==) (UserId u1 f1) (UserId u2 f2) =
        u1 == u2 && f1 == f2


instance Eq Fingerprint where
    (==) (Fingerprint a) (Fingerprint b) =
        a == b


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
    = NewMain T.Text
    | NewTo Shortened
    | MessagesClick
    | WriterClick
    | ContactsClick
    | AccountClick
    | DeleteBlob Hash32
    | DeleteContact Shortened
    | SummaryClick MessageId
    | NewContact UserId


newtype MessageId
    = MessageId B.ByteString


instance Ord MessageId where
    compare (MessageId a) (MessageId b) =
        compare a b


instance Eq MessageId where
    (==) (MessageId a) (MessageId b) =
        a == b


messageIdP :: P.Parser MessageId
messageIdP = do
    id_ <- P.take messageIdLen
    return $ MessageId id_


fromFrontendP :: P.Parser FromFrontend
fromFrontendP = do
    input <- P.choice
        [ P.word8 0 >> return MessagesClick
        , P.word8 1 >> return WriterClick
        , P.word8 2 >> return ContactsClick
        , P.word8 3 >> return AccountClick
        , do
            _ <- P.word8 4
            newMain <- stringP
            return $ NewMain newMain
        , do
            _ <- P.word8 5
            shortened <- shortenedP
            return $ NewTo shortened
        , do
            _ <- P.word8 6
            hash <- hash32P
            return $ DeleteBlob hash
        , do
            _ <- P.word8 7
            shortened <- shortenedP
            return $ DeleteContact shortened
        , do
            _ <- P.word8 8
            messageId <- messageIdP
            return $ SummaryClick messageId
        , do
            _ <- P.word8 9
            userId <- userIdP
            return $ NewContact userId
        ]
    P.endOfInput
    return input


userIdP :: P.Parser UserId
userIdP = do
    shortened <- shortenedP
    fingerprint <- fingerprintP
    return $ UserId shortened fingerprint


dumpView :: Ready -> Output
dumpView =
    BytesInQO toFrontendQ . encodeReady


encodeReady :: Ready -> Bl.ByteString
encodeReady =
    encodeReadyView . readyToReadyView


encodeReadyView :: ReadyView -> Bl.ByteString
encodeReadyView readyView =
    mconcat
    [ Bl.fromStrict $
        encodeList encodeWhiteView $ whitelistV readyView
    , Bl.fromStrict $
      encodeList (Bl.toStrict . encodeSummary) $
      Map.toList $
      summariesV readyView
    , encodePage $ pageV readyView
    ]


encodeWhiteView :: (Shortened, Fingerprint) -> B.ByteString
encodeWhiteView (shortened, fingerprint) =
    Bl.toStrict (encodeShortened shortened) <>
    Bl.toStrict (encodeFingerprint fingerprint)


encodePage :: Page -> Bl.ByteString
encodePage page =
    case page of
    Messages ->
        Bl.singleton 0

    Writer messageId diffs ->
        mconcat
        [ Bl.singleton 1
        , encodeMessageId messageId
        , Bl.fromStrict $ encodeDiffs diffs
        ]

    Contacts ->
        Bl.singleton 2

    Account ->
        Bl.singleton 3


readyToReadyView :: Ready -> ReadyView
readyToReadyView ready =
    let
    whites =
        map (\(_, (f, s, _)) -> (s, f)) $ Map.toList $ whitelist ready
    in
    ReadyView
        { whitelistV = whites
        , summariesV = summaries ready
        , pageV = pageR ready
        }


data ReadyView
    = ReadyView
        { whitelistV :: [(Shortened, Fingerprint)]
        , summariesV :: Map.Map MessageId Summary
        , pageV :: Page
        }


pageClick :: Ready -> Page -> (Output, State)
pageClick ready page =
    if pageR ready == page then
    (DoNothingO, ReadyS ready)
    else
    let
    newReady = ready { pageR = page }
    in
    (dumpView newReady, ReadyS newReady)


toWriterHelp :: Ready -> (Output, State)
toWriterHelp ready =
    let
    (messageId, newGen) = makeMessageId (randomGen ready)
    newReady =
        ready
            { pageR = Writer messageId []
            , counter = counter ready + 1
            , randomGen = newGen
            }
    in
    ( BatchO [ dumpView newReady, dumpCache ready ]
    , ReadyS newReady)


type Gen
    = CryptoRand.ChaChaDRG


makeMessageId :: Gen -> (MessageId, Gen)
makeMessageId gen =
    let
    new :: (B.ByteString, Gen)
    new = CryptoRand.randomBytesGenerate messageIdLen gen
    in
    (MessageId (fst new), snd new)


messageIdLen =
    32


removeBlob :: Hash32 -> [Blob] -> [Blob]
removeBlob hash blobs =
    filter ((/= hash) . hashB) blobs


updateOnDeleteBlob :: Hash32 -> Ready -> (Output, State)
updateOnDeleteBlob hash ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case pageR ready of
    Messages ->
        pass

    Writer messageId diffs ->
        case constructMessage diffs of
        Nothing ->
            (ErrorO "could not construct message", FailedS)

        Just oldEncoded ->
            case decodeHeader (Bl.fromStrict oldEncoded) of
            Left err ->
                ( ErrorO $ "could not decode header: " <> err
                , FailedS
                )

            Right oldHeader ->
                let
                newHeader =
                    oldHeader
                        { blobs =
                            removeBlob hash (blobs oldHeader)
                        }
                newEncoded = encodeHeader newHeader
                newDiff = makeDiff Me oldEncoded newEncoded
                newDiffs = newDiff : diffs
                newReady =
                    ready { pageR = Writer messageId newDiffs }
                (sendO, sendState) =
                    shareHeader (ReadyS newReady) messageId newHeader
                in
                updateReady sendState $ \newerReady ->
                ( BatchO
                    [ dumpCache newerReady
                    , dumpView newerReady
                    , dumpMessage (root newerReady) messageId newDiffs
                    , sendO
                    ]
                , sendState
                )

    Contacts ->
        pass

    Account ->
        pass


updateOnDeleteContact :: Shortened -> Ready -> (Output, State)
updateOnDeleteContact shortened ready =
    case getSender (whitelist ready) shortened of
    Nothing ->
        (DoNothingO, ReadyS ready)

    Just sender ->
        let
        newWhite = Map.delete sender (whitelist ready)
        newReady = ready { whitelist = newWhite }
        in
        ( BatchO
            [ dumpView newReady
            , dumpCache newReady
            ]
        , ReadyS newReady
        )


getSender :: Whitelist -> Shortened -> Maybe Sender
getSender whitelist shortened =
    Map.lookup shortened $ whitelistToShortMap whitelist


whitelistToShortMap :: Whitelist -> Map.Map Shortened Sender
whitelistToShortMap =
    Map.fromList . map toShortMapHelp . Map.toList


toShortMapHelp
    :: (Sender, (Fingerprint, Shortened, TheirStaticNoise))
    -> (Shortened, Sender)
toShortMapHelp (sender, (_, shortened, _)) =
    (shortened, sender)


updateOnSummaryClick :: MessageId -> Ready -> (Output, State)
updateOnSummaryClick messageId ready =
    ( ReadFileStrictO $ makeMessagePath (root ready) messageId
    , ReadyS $ ready { readingMessageOnClick = Just messageId }
    )


uiApiUpdate :: FromFrontend -> Ready -> (Output, State)
uiApiUpdate fromFrontend ready =
    case fromFrontend of
    SummaryClick messageId ->
        updateOnSummaryClick messageId ready

    DeleteContact shortened ->
        updateOnDeleteContact shortened ready

    NewTo userId ->
        updateOnNewShares [userId] ready

    DeleteBlob id_ ->
        updateOnDeleteBlob id_ ready

    ContactsClick ->
        pageClick ready Contacts

    MessagesClick ->
        pageClick ready Messages

    AccountClick ->
        pageClick ready Account

    WriterClick ->
        case pageR ready of
        Messages ->
            toWriterHelp ready

        Writer _ _ ->
            (DoNothingO, ReadyS ready)

        Contacts ->
            toWriterHelp ready

        Account ->
            toWriterHelp ready

    NewMain newMain ->
        updateOnNewMain newMain ready

    NewContact userId ->
        updateOnNewContact userId ready


updateOnNewContact :: UserId -> Ready -> (Output, State)
updateOnNewContact (UserId shortened fingerprint) ready =
    ( lookupShortened shortened
    , ReadyS $
        ready
            { lookingUpShortened =
                Map.insert
                    shortened
                    fingerprint
                    (lookingUpShortened ready)
            }
    )


lookupShortened :: Shortened -> Output
lookupShortened shortened =
    BytesInQO toServerQ $ encodeToServer $
    LookupShortenedT shortened


updateOnNewBlobs :: [Blob] -> Ready -> (Output, State)
updateOnNewBlobs newBlobs ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case pageR ready of
    Messages ->
        pass

    Writer messageId diffs ->
        case constructMessage diffs of
        Nothing ->
            (ErrorO "could not construct message", FailedS)

        Just oldEncoded ->
            case decodeHeader $ Bl.fromStrict oldEncoded of
            Left err ->
                ( ErrorO $ "could not decode header: " <> err
                , FailedS
                )

            Right oldHeader ->
                let
                newHeader = oldHeader { blobs = newBlobs }
                newEncoded = encodeHeader newHeader
                newDiff = makeDiff Me oldEncoded newEncoded
                newDiffs = newDiff : diffs
                newReady =
                    ready { pageR = Writer messageId newDiffs }
                onlyNew =
                    map (\b -> hashB b) $
                    Set.toList $
                    Set.difference
                    (Set.fromList $ blobs oldHeader)
                    (Set.fromList $ newBlobs)
                (sendO, sendState) =
                    shareBlobs
                        (ReadyS newReady)
                        messageId
                        onlyNew
                        (shares newHeader)
                write =
                    WriteFileStrictO
                        (makeMessagePath (root ready) messageId)
                        (encodeDiffs newDiffs)
                in
                updateReady sendState $ \newerReady ->
                ( BatchO
                    [ sendO
                    , write
                    , dumpView newerReady
                    ]
                , ReadyS newerReady
                )

    Contacts ->
        pass

    Account ->
        pass


updateOnNewWasm :: Hash32 -> Ready -> (Output, State)
updateOnNewWasm hash ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case pageR ready of
    Messages ->
        pass

    Writer messageId diffs ->
        case constructMessage diffs of
        Nothing ->
            (ErrorO "could not construct message", FailedS)

        Just oldEncoded ->
            case decodeHeader $ Bl.fromStrict oldEncoded of
            Left err ->
                ( ErrorO $ "could not decode header: " <> err
                , FailedS
                )

            Right oldHeader ->
                let
                newHeader = oldHeader { wasm = hash }
                newEncoded = encodeHeader newHeader
                newDiff = makeDiff Me oldEncoded newEncoded
                newDiffs = newDiff : diffs
                newReady = ready { pageR = Writer messageId newDiffs }
                (sendO, sendState) =
                    shareBlobsHelp
                        messageId
                        (shares newHeader)
                        hash
                        (DoNothingO, ReadyS newReady)
                write =
                    WriteFileStrictO
                        (makeMessagePath (root ready) messageId)
                        (encodeDiffs newDiffs)
                in
                updateReady sendState $ \newerReady ->
                ( BatchO
                    [ sendO
                    , write
                    , dumpView newerReady
                    ]
                , ReadyS newerReady
                )

    Contacts ->
        pass

    Account ->
        pass


unshortenOne :: Ready -> Shortened -> Maybe Sender
unshortenOne ready shortened =
    getSender (whitelist ready) shortened


allJusts :: [Maybe a] -> Maybe [a]
allJusts maybes =
    allJustsHelp maybes []


allJustsHelp :: [Maybe a ] -> [a] -> Maybe [a]
allJustsHelp maybes justs =
    case maybes of
    [] ->
        Just justs

    Nothing:_ ->
        Nothing

    Just m:aybes ->
        allJustsHelp aybes (m:justs)


unshorten :: [Shortened] -> Ready -> Maybe [Sender]
unshorten shortened ready =
    allJusts $ map (unshortenOne ready) shortened


updateOnNewShares :: [Shortened] -> Ready -> (Output, State)
updateOnNewShares newShortened ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case unshorten newShortened ready of
    Nothing ->
        pass

    Just newShares ->
        case pageR ready of
        Messages ->
            pass
    
        Writer messageId diffs ->
            case constructMessage diffs of
            Nothing ->
                (ErrorO "could not construct message", FailedS)
    
            Just oldEncoded ->
                case decodeHeader (Bl.fromStrict oldEncoded) of
                Left err ->
                    (ErrorO $ "could not decode header: " <> err
                    , FailedS
                    )
    
                Right oldHeader ->
                    let
                    newHeader = oldHeader { shares = newShares }
                    diffShares :: [Sender]
                    diffShares =
                        Set.toList $ Set.difference
                            (Set.fromList newShares)
                            (Set.fromList $ shares oldHeader)
                    newEncoded = encodeHeader newHeader
                    newDiff = makeDiff Me oldEncoded newEncoded
                    newDiffs = newDiff : diffs
    
                    newReady =
                        ready { pageR = Writer messageId newDiffs }
                    (sendO, sendState) =
                        shareMessage
                            diffShares
                            (ReadyS newReady)
                            messageId
                            newHeader
                    in
                    updateReady sendState $ \newerReady ->
                    ( BatchO
                        [ dumpCache newerReady
                        , dumpView newerReady
                        , dumpMessage
                            (root newerReady)
                            messageId
                            newDiffs
                        , sendO
                        ]
                    , sendState
                    )
    
        Contacts ->
            pass
    
        Account ->
            pass


type Whitelist
    = Map.Map Sender (Fingerprint, Shortened, TheirStaticNoise)


shareMessage
    :: [Sender]
    -> State
    -> MessageId
    -> Header
    -> (Output, State)
shareMessage newShares model messageId header =
    let
    (headerO, headerState) = shareHeader model messageId header
    hashes :: [Hash32]
    hashes = wasm header : map hashB (blobs header)
    (blobsO, blobsState) =
        shareBlobs headerState messageId hashes newShares
    in
    (BatchO [headerO, blobsO], blobsState)


shareBlobs
    :: State
    -> MessageId
    -> [Hash32]
    -> [Sender]
    -> (Output, State)
shareBlobs model messageId hashes tos =
    foldr (shareBlobsHelp messageId tos) (DoNothingO, model) hashes


shareBlobsHelp
    :: MessageId
    -> [Sender]
    -> Hash32
    -> (Output, State)
    -> (Output, State)
shareBlobsHelp messageId tos hash (oldO, model) =
    updateReady model $ \ready ->
    ( BatchO
        [ oldO
        , GetFileHandleO (makeBlobPath (root ready) hash)
        ]
    , case sendingBlob ready of
        NoJobs ->
            ReadyS $ ready
                { sendingBlob =
                    Jobs (AwaitingHandle hash tos) []
                }

        Jobs current waiting ->
            let
            newWait = SendingBlobWait messageId hash tos
            jobs = Jobs current (reverse $ newWait : waiting)
            in
            ReadyS $ ready { sendingBlob = jobs }
    )


dumpMessage :: RootPath -> MessageId -> [Diff] -> Output
dumpMessage root messageId diffs =
    let
    encoded = encodeDiffs diffs
    path = makeMessagePath root messageId
    in
    WriteFileStrictO path encoded


updateOnNewMain :: T.Text -> Ready -> (Output, State)
updateOnNewMain newMain ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case pageR ready of
    Messages ->
        pass

    Writer messageId diffs ->
        case constructMessage diffs of
        Nothing ->
            (ErrorO "could not contruct message", FailedS)

        Just oldEncoded ->
            case decodeHeader (Bl.fromStrict oldEncoded) of
            Left err ->
                ( ErrorO $ "could not decode header: " <> err
                , FailedS
                )

            Right oldHeader ->
                let
                newHeader = oldHeader { mainBox = newMain }
                newEncoded = encodeHeader newHeader
                newDiff = makeDiff Me oldEncoded newEncoded
                newDiffs = newDiff : diffs
                newReady =
                    ready { pageR = Writer messageId newDiffs }
                (sendO, sendState) =
                    shareHeader (ReadyS newReady) messageId newHeader
                in
                updateReady sendState $ \newerReady ->
                ( BatchO
                    [ dumpCache newerReady
                    , dumpView newerReady
                    , dumpMessage (root newerReady) messageId newDiffs
                    , sendO
                    ]
                , sendState
                )

    Contacts ->
        pass

    Account ->
        pass


shareHeader :: State -> MessageId -> Header -> (Output, State)
shareHeader model messageId header =
     let
     chunks = encodeAndChunkHeader messageId header
     in
     encodeAndEncrypt chunks (shares header) model


encodeAndEncrypt
    :: [B.ByteString]
    -> [Sender]
    -> State
    -> (Output, State)
encodeAndEncrypt chunks shares model =
    foldr
        (encodeAndEncryptHelp chunks)
        (DoNothingO, model)
        shares


encryptChunks
    :: MyStaticNoise
    -> TheirStaticNoise
    -> [(B.ByteString, SecondShake)]
    -> [Either String Bl.ByteString]
encryptChunks myStatic theirStaticNoise chunkShakes =
    map (encryptChunk myStatic theirStaticNoise) chunkShakes


encryptChunk
    :: MyStaticNoise
    -> TheirStaticNoise
    -> (B.ByteString, SecondShake)
    -> Either String Bl.ByteString
encryptChunk
    myStatic
    theirStatic
    (plain, SecondShake myEphemeral (EncryptedEphemeral response)) =

    let
    options = initiatorOptions myStatic theirStatic myEphemeral
    noise0 = Noise.noiseState options noiseKK :: NoiseState
    in
    case Noise.writeMessage "" noise0 of
    Noise.NoiseResultMessage _ noise1 ->
        case
            Noise.readMessage
                (Ba.convert $ Bl.toStrict response)
                noise1
        of
        Noise.NoiseResultMessage _ noise2 ->
            case Noise.writeMessage (Ba.convert plain) noise2 of
            Noise.NoiseResultMessage ciphertext _ ->
                Right $ Bl.fromStrict $ mconcat
                [ B.singleton 3
                , Ba.convert ciphertext
                ]

            Noise.NoiseResultNeedPSK _ ->
                Left "NoiseResultNeedPSK"

            Noise.NoiseResultException err ->
                Left $ "NoiseResultException: " ++ show err

        Noise.NoiseResultNeedPSK _ ->
            Left "NoiseResultNeedPSK"

        Noise.NoiseResultException err ->
            Left $ "NoiseResultException: " ++ show err

    Noise.NoiseResultNeedPSK _ ->
        Left "NoiseResultNeedPSK"

    Noise.NoiseResultException err ->
        Left $ "NoiseResultException: " ++ show err


type Handshakes
    = Map.Map HandshakeId Handshake


chooseShakes
    :: Sender
    -> Int
    -> Handshakes
    -> Maybe ([SecondShake], Handshakes)
chooseShakes to num handshakes =
    chooseShakesHelp
        to
        num
        (Map.toList handshakes)
        ([], Map.empty)


chooseShakesHelp
    :: Sender
    -> Int
    -> [(HandshakeId, Handshake)]
    -> ([SecondShake], Handshakes)
    -> Maybe ([SecondShake], Handshakes)
chooseShakesHelp to num handshakes (relevant, irrelevant) =
    if length relevant == num then
    Just (relevant, irrelevant)
    else
    case handshakes of
    [] ->
        Nothing

    (h@(HandshakeId shortened _), handshake):andshakes ->
        chooseShakesHelp
            to
            num
            andshakes
            (
            if to == shortened then
            case handshake of
            Initiator myEphemeral (ReceivedEncryptedE response) ->
                let
                secondShake = SecondShake myEphemeral response
                in
                (secondShake : relevant, irrelevant)

            Initiator _ SentPlainE ->
                (relevant, Map.insert h handshake irrelevant)

            ResponderSentEncryptedES _ ->
                (relevant, Map.insert h handshake irrelevant)

            else
            (relevant, Map.insert h handshake irrelevant))


encodeAndEncryptHelp
    :: [B.ByteString]
    -> Sender
    -> (Output, State)
    -> (Output, State)
encodeAndEncryptHelp chunks to (oldO, model) =
    let
    pass = (DoNothingO, model)
    in
    updateReady model $ \oldReady ->
    (\f ->
        case staticNoiseKeys oldReady of
        Nothing ->
            pass

        Just keys ->
            f keys) $ \myStatic ->

    case chooseShakes to (length chunks) (handshakes oldReady) of
    Nothing ->
        let
        writes =
            map (writeToFile (root oldReady) . Bl.fromStrict) chunks
        awaitings = map (\b -> (hash32 $ Bl.fromStrict b, to)) chunks
        (cryptO, cryptoModel) = bumpCrypto oldReady
        in
        updateReady cryptoModel $ \cryptoReady ->
            let
            newReady =
                cryptoReady
                    { awaitingCrypto =
                        awaitings ++ awaitingCrypto cryptoReady
                    }
            in
            ( BatchO
                [ oldO
                , BatchO writes
                , cryptO
                , dumpCache newReady
                , dumpView newReady
                ]
            , ReadyS $
                cryptoReady
                    { awaitingCrypto = awaitings
                    }
            )

    Just (relevant, irrelevant) ->
        case Map.lookup to $ whitelist oldReady of
        Nothing ->
            pass

        Just (_, _, theirNoise) ->
            let
            eitherEncrypted =
                encryptChunks myStatic theirNoise (zip chunks relevant)
            in
            case allOk eitherEncrypted of
            Left err ->
                ( ErrorO $ "failed to encrypt chunks: " <> err
                , FailedS
                )

            Right encrypted ->
                let
                writes = map (writeToFile (root oldReady)) encrypted
                sends = map (BytesInQO toServerQ) encrypted
                waitForAcknowledge = map hash32 encrypted
                newReady =
                    oldReady
                        { waitForAcknowledge
                        , handshakes = irrelevant
                        }
                in
                ( BatchO [oldO, BatchO writes, BatchO sends]
                , ReadyS newReady
                )


sendBlobChunk
    :: B.ByteString
    -> Ed.PublicKey
    -> (Output, State)
    -> (Output, State)
sendBlobChunk chunk toKey (oldO, oldModel) =
    let
    pass = (DoNothingO, oldModel)
    in
    updateReady oldModel $ \ready ->
    case chooseShakes (Sender toKey) 1 (handshakes ready) of
    Nothing ->
        let
        write = writeToFile (root ready) $ Bl.fromStrict chunk
        awaiting = (hash32 $ Bl.fromStrict chunk, Sender toKey)
        (cryptO, cryptoModel) = bumpCrypto ready
        in
        updateReady cryptoModel $ \cryptoReady ->
            let
            newReady =
                cryptoReady
                    { awaitingCrypto =
                        awaiting : awaitingCrypto cryptoReady
                    }
            in
            ( BatchO
                [ oldO
                , write
                , cryptO
                , dumpCache newReady
                , dumpView newReady
                ]
            , ReadyS newReady
            )

    Just ([shake], remainder) ->
        (\f ->
            case staticNoiseKeys ready of
            Nothing ->
                pass

            Just keys ->
                f keys) $ \myStatic ->
        case Map.lookup (Sender toKey) $ whitelist ready of
        Nothing ->
            pass

        Just (_, _, theirStatic) ->
            let
            eitherEncrypted =
                encryptChunk myStatic theirStatic (chunk, shake)
            newReady =
                ready { handshakes = remainder }
            in
            case eitherEncrypted of
            Left err ->
                (ErrorO $ "could not encrypt chunk: " ++ err, FailedS)

            Right encrypted ->
                let
                write = writeToFile (root ready) encrypted
                send = BytesInQO toServerQ encrypted
                in
                ( BatchO [oldO, write, send]
                , ReadyS newReady
                )

    Just _ ->
        -- It should never happen.
        pass


bumpCrypto :: Ready -> (Output, State)
bumpCrypto ready =
    foldr
        bumpCryptoHelp
        (DoNothingO, ReadyS ready)
        (Map.keys $ whitelist ready)


numFirstShakes =
    499


getStaticKeys
    :: Ready
    -> (MyStaticNoise -> (Output, State))
    -> (Output, State)
getStaticKeys ready okFunc =
    case staticNoiseKeys ready of
    Nothing ->
        (DoNothingO, ReadyS ready)

    Just keys ->
        okFunc keys


bumpCryptoHelp :: Sender -> (Output, State) -> (Output, State)
bumpCryptoHelp contact (oldO, model) =
    updateReady model $ \oldReady ->
    getStaticKeys oldReady $ \myStatic ->
    let
    ephemerals =
        map MyEphemeral $ take numFirstShakes (newDhKeys oldReady)
    remaining = drop numFirstShakes (newDhKeys oldReady)
    newReady =
        oldReady
            { newDhKeys = remaining
            }
    in
    case Map.lookup contact $ whitelist oldReady of
    Nothing ->
        (oldO, model)

    Just (_, _, theirStatic) ->
        let
        eitherCryptos =
            map (makeFirstShake myStatic theirStatic) ephemerals
        in
        case allOk eitherCryptos of
        Left err ->
            ( ErrorO $ "could not make Noise first handshakes: " ++ err
            , FailedS
            )

        Right cryptos ->
            let
            shakes =
                mconcat
                [ Bl.singleton 3
                , encodeSender contact
                , Bl.singleton 0
                , mconcat cryptos
                ]
            toServer = BytesInQO toServerQ shakes
            hash = hash32 shakes
            write = writeToFile (root oldReady) shakes
            in
            ( BatchO [oldO, toServer, write]
            , ReadyS newReady
                { waitForAcknowledge =
                    hash : waitForAcknowledge newReady
                }
            )


makeFirstShake
    :: MyStaticNoise
    -> TheirStaticNoise
    -> MyEphemeral
    -> Either String Bl.ByteString
makeFirstShake myStatic theirStatic myEphemeral =
    let
    options = initiatorOptions myStatic theirStatic myEphemeral
    noise0 :: NoiseState
    noise0 = Noise.noiseState options noiseKK
    in
    case Noise.writeMessage "" noise0 of
    Noise.NoiseResultMessage ciphertext _ ->
        Right $ Bl.fromStrict $ Ba.convert ciphertext

    Noise.NoiseResultNeedPSK _ ->
        Left "NoiseResultNeedPSK"

    Noise.NoiseResultException err ->
        Left $ "NoiseResultException: " ++ show err


writeToFile :: RootPath -> Bl.ByteString -> Output
writeToFile root bytes =
    let
    path = makeBlobPath root (hash32 bytes)
    in
    WriteFileO path bytes


allOk :: [Either a b] -> Either a [b]
allOk eithers =
    allOkHelp eithers []


allOkHelp :: [Either a b] -> [b] -> Either a [b]
allOkHelp eithers accum =
    case eithers of
    [] ->
        Right accum

    Left a : _ ->
        Left a

    Right b : ithers ->
        allOkHelp ithers (b:accum)


data FileMetadata =
    FileMetadata
        { fileSizeM :: Int
        , mimeM :: T.Text
        , filenameM :: T.Text
        }


decodeMetadata :: T.Text -> Either String FileMetadata
decodeMetadata raw =
    case Text.Hex.decodeHex raw of
        Nothing ->
            Left "hexadecimal decode error"

        Just bytes ->
            P.eitherResult $ P.parse metadataP $ Bl.fromStrict bytes


metadataP :: P.Parser FileMetadata
metadataP = do
    filename <- sizedStringP
    mime <- sizedStringP
    size <- uint32P
    return $ FileMetadata size mime filename


onMovedFile :: FilePath -> Ready -> (Output, State)
onMovedFile new ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case blobsUp ready of
    NoJobs ->
        pass

    j@(Jobs (AwaitingMoveB rawMetadata (Hash32 hash) toPath) _) ->
        case decodeMetadata rawMetadata of
        Left err ->
            (ErrorO err, FailedS)

        Right metadata ->
            if toPath == new then
            let
            blob =
                Blob
                    { hashB = Hash32 hash
                    , filenameB = filenameM metadata
                    , size = fileSizeM metadata
                    , mime = mimeM metadata
                    }
            in
            updateOnNewBlobs
                [blob]
                (ready { blobsUp = promoteBlobsUp j })
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

    Jobs (AwaitingTmpWriteB rawMeta hash expectPath) waiting ->
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
                    (AwaitingMoveB rawMeta hash blobPath)
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

    Jobs (AwaitingHandleB body rawMetadata hash) waiting ->
        ( WriteToHandleO handle body path
        , ReadyS $ ready {
            blobsUp = Jobs
                (AwaitingTmpWriteB rawMetadata hash path)
                waiting
            }
        )

    Jobs _ _ ->
        (DoNothingO, ReadyS ready)


setBlobUpdate ::
    Bl.ByteString ->
    T.Text ->
    Ready ->
    (Output, State)
setBlobUpdate body rawMetadata ready =
    case blobsUp ready of
    NoJobs ->
        ( GetTmpFileHandleO $ tempPath $ root ready
        , ReadyS $ ready
            { blobsUp =
                Jobs (AwaitingHandleB body rawMetadata (hash32 body)) []
            }
        )

    Jobs current waiting ->
        ( DoNothingO
        , ReadyS $
            ready
                { blobsUp =
                    Jobs current (BlobUpWait body rawMetadata : waiting)
                }
        )


hash32 :: Bl.ByteString -> Hash32
hash32 =
    Hash32 .
    Bl.fromStrict .
    Blake.finalize 32 .
    Bl.foldrChunks Blake.update (Blake.initialize 32)


tempFile :: RootPath -> Integer -> FilePath
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
        { keys :: Maybe MyStaticNoise
        , handshakesM :: Map.Map HandshakeId Handshake
        , whitelistM :: Whitelist
        , summariesM :: Map.Map MessageId Summary
        , counterM :: Integer
        , waitForAcknowledgeM :: [Hash32]
        , awaitingCryptoM :: [(Hash32, Sender)]
        , secretSigningKeyM :: Maybe Ed.SecretKey
        }


encodeVarInt :: Integer -> Bl.ByteString
encodeVarInt i =
    let
    wds = encodeVarIntHelp i []
    len = fromIntegral $ length wds
    in
    Bl.singleton len <> Bl.pack wds


encodeVarIntHelp :: Integer -> [Word8] -> [Word8]
encodeVarIntHelp remaining accum =
    if remaining == 0 then
    accum
    else
    encodeVarIntHelp
        (remaining `Bits.shiftR` 8)
        (fromIntegral (remaining Bits..&. 0xFF) : accum)


varIntP :: P.Parser Integer
varIntP = do
    len <- P.anyWord8
    varIntHelpP len 0 0

varIntHelpP :: Word8 -> Integer -> Integer -> P.Parser Integer
varIntHelpP len sofar counter =
    if fromIntegral counter == len then
    return sofar

    else do
        word <- P.anyWord8
        varIntHelpP
            len
            (sofar + (fromIntegral word) * (256 ^ counter))
            (counter + 1)


memCacheP :: P.Parser MemCache
memCacheP = do
    keys <- myKeysP
    handshakesM <- handshakesP
    whitelistM <- whitelistP
    summariesM <- summariesP
    counterM <- varIntP
    waitForAcknowledgeM <- listP hash32P
    awaitingCryptoM <- listP awaitingCryptoP
    secretSigningKeyM <- maybeP secretSigningP
    P.endOfInput
    return $
        MemCache
            { keys
            , handshakesM
            , whitelistM
            , summariesM
            , counterM
            , waitForAcknowledgeM
            , awaitingCryptoM
            , secretSigningKeyM
            }


secretSigningP :: P.Parser Ed.SecretKey
secretSigningP = do
    raw <- P.take Ed.secretKeySize
    case Ed.secretKey raw of
        CryptoFailed err ->
            fail $ show err

        CryptoPassed key ->
            return key


awaitingCryptoP :: P.Parser (Hash32, Sender)
awaitingCryptoP = do
    hash <- hash32P
    sender <- senderP
    return (hash, sender)


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
    authorS <- senderP
    return (messageId, Summary {subjectS, timeS, authorS})


whitelistP :: P.Parser Whitelist
whitelistP = do
    numWhites <- uint32P
    asList <- P.count numWhites oneWhitelistP
    return $ Map.fromList asList


oneWhitelistP
    :: P.Parser (Sender, (Fingerprint, Shortened, TheirStaticNoise))
oneWhitelistP = do
    sender <- senderP
    fingerprint <- fingerprintP
    shortened <- shortenedP
    maybeStatic <- theirStaticP
    return (sender, (fingerprint, shortened, maybeStatic))


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


handshakeP :: P.Parser (HandshakeId, Handshake)
handshakeP = do
    handshakeId <- handshakeIdP
    handshake <- P.choice [initiatorP, responderP]
    return (handshakeId, handshake)


initiatorP :: P.Parser Handshake
initiatorP = do
    _ <- P.word8 0
    myEphemeral <- myEphemeralP
    P.choice
        [ do
            _ <- P.word8 0
            return $ Initiator myEphemeral SentPlainE
        , do
            _ <- P.word8 1
            ephemeral <- encryptedEphemeralP
            return $
                Initiator myEphemeral $
                    ReceivedEncryptedE ephemeral
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
    sender <- senderP
    firstMessage <- fmap FirstMessage $
        fmap Bl.fromStrict $ P.take 32
    return $ HandshakeId sender firstMessage


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


myStaticP :: P.Parser MyStaticNoise
myStaticP = do
    rawDhKey <- P.take secretKeyLen
    case Dh.dhBytesToPair $ Noise.convert rawDhKey of
        Nothing ->
            fail "could not parse secret key from file"

        Just dhKeys ->
            return $ MyStaticNoise dhKeys


myKeysP :: P.Parser (Maybe MyStaticNoise)
myKeysP =
    maybeP myStaticP


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
        ( DoNothingO
        , ReadyS $ Ready
            { root
            , blobsUp = NoJobs
            , getBlob = NoJobs
            , authStatus = AwaitingAuthCode
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
            , pageR = Messages
            , awaitingCrypto = awaitingCryptoM memCache
            , sendingBlob = NoJobs
            , readingMessageOnClick = Nothing
            , staticNoiseKeys = Nothing
            , userId = Nothing
            , secretSigningKey = secretSigningKeyM memCache
            , lookingUpShortened = Map.empty
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

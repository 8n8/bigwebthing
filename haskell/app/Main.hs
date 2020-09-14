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
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import System.Entropy (getEntropy)
import qualified Data.Bits as Bits
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Crypto.Random as CryptoRand


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
    | RandomGenM CryptoRand.ChaChaDRG
    | AppendedStrictM


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
    deriving (Eq, Ord)


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


data Ready =
    Ready
        { root :: RootPath
        , blobsUp :: Jobs BlobUp BlobUpWait
        , getBlob :: Jobs GetBlob GetBlobWait
        , messageLocks :: Set.Set MessageId
        , blobLocks :: Set.Set Hash32
        , authStatus :: AuthStatus
        , handshakes :: Map.Map HandshakeId Handshake
        , newDhKeys :: [Dh.KeyPair Curve25519]
        , theTime :: [Clock.UTCTime]
        , whitelist ::
            Map.Map Username (Fingerprint, Maybe TheirStatic)
        , sharePairs :: Map.Map ShareName SharePair
        , sendingBlob :: Jobs Sending SendWait
        , summaries :: Map.Map MessageId Summary
        , randomGen :: CryptoRand.ChaChaDRG
        , counter :: Int
        , assemblingFile :: Maybe AssemblingFile
        , gettingMessage :: Maybe Hash32
        }


summarize :: Header -> Maybe Summary -> Int -> Hash32 -> Summary
summarize header old unique hash =
    case old of
    Nothing ->
        Summary
            { headerBlob = hash
            , referenced =
                Set.insert
                    (wasm header)
                    (Set.map hashB $ blobs header)
            , subjectS = subject header
            , sharersS = sharers header
            , lastEditS = lastEdit header
            , historyId = unique
            }

    Just oldSummary ->
        Summary
            { headerBlob = hash
            , referenced =
                Set.insert
                    (wasm header)
                    (Set.map hashB $ blobs header)
            , subjectS = subject header
            , sharersS = sharers header
            , lastEditS = lastEdit header
            , historyId = historyId oldSummary
            }


data AssemblingFile
    = Assembling TmpBase Blake.BLAKE2bState
    | AddingFinal TmpBase Hash32
    | Moving TmpBase Hash32


type TmpBase
    = Int


data Summary
    = Summary
        { headerBlob :: Hash32
        , referenced :: Set.Set Hash32
        , subjectS :: T.Text
        , sharersS :: Set.Set UserId
        , lastEditS :: PosixMillis
        , historyId :: Int
        }


data Sending
    = ReadingS Hash32 ShareName


data SendWait
    = SendWait Hash32 ShareName


data SharePair
    = SharePair TheirSet MySet


newtype MySet
    = MySet (Set.Set Hash32)


newtype TheirSet
    = TheirSet (Set.Set Hash32)


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


cacheSummary :: RootPath -> Summary -> Output
cacheSummary root summary =
    AppendFileO
        (makeHistoryPath root $ historyId summary)
        (encodeSummary summary)


makeHistoryPath :: RootPath -> Int -> FilePath
makeHistoryPath root id_ =
    blobsPath root </> show id_


encodeSummary :: Summary -> Bl.ByteString
encodeSummary summary =
    mconcat
        [ encodeHash $ headerBlob summary
        , encodeString $ subjectS summary
        , encodeSharers $ sharersS summary
        , encodeTime $ lastEditS summary
        ]


encodeHash :: Hash32 -> Bl.ByteString
encodeHash (Hash32 hash) =
    hash


encodeString :: T.Text -> Bl.ByteString
encodeString s =
    let
    asBytes = Bl.fromStrict $ encodeUtf8 s
    len = Bl.length asBytes
    in
    encodeUint32 (fromIntegral len) <> asBytes


encodeSharers :: Set.Set UserId -> Bl.ByteString
encodeSharers userIds =
    let
    asList = Set.toList userIds
    num = length asList
    in
    encodeUint32 num <> mconcat (map encodeUserId asList)


encodeUserId :: UserId -> Bl.ByteString
encodeUserId (UserId username fingerprint) =
    encodeUsername username <> encodeFingerprint fingerprint


isSharer :: Username -> Set.Set (UserId) -> Bool
isSharer candidate sharers =
    Set.member candidate $ Set.map (\(UserId u _) -> u) sharers


encodeTime :: PosixMillis -> Bl.ByteString
encodeTime (PosixMillis t) =
    encodeUint64 t


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

    RandomGenM gen ->
        updateInit model $ updateOnRandomGen gen

    AppendedStrictM ->
        updateReady model $ fileAssembledUpdate


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
    candidate = encodeUint64 counter
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
                    , messageLocks = Set.empty
                    , authStatus = GettingPowInfoA
                    , handshakes = Map.empty
                    , newDhKeys = newKeys
                    , theTime = times
                    , whitelist = Map.empty
                    , blobLocks = Set.empty
                    , sharePairs = Map.empty
                    , sendingBlob = NoJobs
                    , summaries = Map.empty
                    , randomGen = gen
                    , counter = 0
                    , assemblingFile = Nothing
                    , gettingMessage = Nothing
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


parsedPlainUpdate :: Username -> Plaintext -> Ready -> (Output, State)
parsedPlainUpdate from plain ready =
    case plain of
    AllInOne p ->
        assembledUpdate from p ready

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
    = ShareSetA MessageId TheirSet
    | HeaderA MessageId Bl.ByteString
    | Referenced MessageId Bl.ByteString


assembledP :: P.Parser Assembled
assembledP = do
    messageId <- messageIdP
    P.choice
        [ do
            _ <- P.word8 0
            hashes <- P.many1 hash32P
            P.endOfInput
            return $
                ShareSetA
                    messageId
                    (TheirSet (Set.fromList hashes))
        , do
            _ <- P.word8 1
            header <- P.takeLazyByteString
            return $ HeaderA messageId header
        , do
            _ <- P.word8 2
            referenced <- P.takeLazyByteString
            return $ Referenced messageId referenced
        ]


assembledUpdate
    :: Username
    -> Bl.ByteString
    -> Ready
    -> (Output, State)
assembledUpdate from assembled ready =
    case P.eitherResult $ P.parse assembledP assembled of
    Right (ShareSetA messageId theirs) ->
        sharePairUpdate from messageId theirs ready

    Right (HeaderA messageId header) ->
        newHeaderUpdate from messageId header ready

    Right (Referenced messageId body) ->
        referencedUpdate from messageId body ready

    Left err ->
        logErr
            ready
            (mconcat
                [ "couldn't parse assembled blob from "
                , T.pack $ show from
                , ": "
                , T.pack err
                ])


referencedUpdate
    :: Username
    -> MessageId
    -> Bl.ByteString
    -> Ready
    -> (Output, State)
referencedUpdate from messageId body ready =
    let
    hash = hash32 body
    writeBlob =
        WriteFileO
            (makeBlobPath (root ready) hash)
            body
    shareName = ShareName messageId from
    in
    case Map.lookup messageId (summaries ready) of
    Nothing ->
        unsolicitedBlob ready from hash

    Just summary ->
        case Map.lookup shareName (sharePairs ready) of
        Nothing ->
            unsolicitedBlob ready from hash

        Just (SharePair theirs (MySet mine)) ->
            if Set.member hash (referenced summary) then
            ( BatchO [writeBlob, dumpCache ready]
            , ReadyS $
                ready
                    { sharePairs =
                        Map.insert
                            shareName
                            (SharePair
                                theirs
                                (MySet $
                                    Set.insert hash mine
                                ))
                            (sharePairs ready)
                    }
            )

            else
            unsolicitedBlob ready from hash


unsolicitedBlob :: Ready -> Username -> Hash32 -> (Output, State)
unsolicitedBlob ready from hash =
     logErr
         ready
         (mconcat
             [ "received unsolicited blob from "
             , T.pack $ show from
             , " with hash "
             , T.pack $ showHash hash
             ])


badHeaderErr :: Ready -> Username -> String -> (Output, State)
badHeaderErr ready from err =
    logErr
        ready
        (mconcat
            [ "could not parse new header from "
            , T.pack $ show from
            , ": "
            , T.pack $ show err
            ])


newHeaderUpdate from messageId header ready =
    case
        ( Map.lookup messageId (summaries ready)
        , P.eitherResult $ P.parse headerP header
        )
    of
    (Nothing, Left err) ->
        badHeaderErr ready from err

    (Just _, Left err) ->
        badHeaderErr ready from err

    (Just summary, Right parsed) ->
        let
        hash = hash32 header

        newSummary =
            Summary
                { headerBlob = hash
                , subjectS = subject parsed
                , sharersS = sharersS summary
                , lastEditS = lastEdit parsed
                , historyId = historyId summary
                , referenced = makeRefs parsed
                }

        newSummaries =
            Map.insert messageId newSummary (summaries ready)

        newReady = ready { summaries = newSummaries }

        headerPath = makeBlobPath (root ready) hash
        in
        if isSharer from (sharersS summary) then
        ( BatchO
            [ cacheSummary (root ready) summary
            , WriteFileO headerPath header
            , dumpCache newReady
            ]
        , ReadyS newReady
        )

        else
        (DoNothingO, ReadyS ready)

    (Nothing, Right parsed) ->
        let
        hash = hash32 header
        newSummary =
            Summary
                { headerBlob = hash
                , subjectS = subject parsed
                , sharersS = sharers parsed
                , lastEditS = lastEdit parsed
                , historyId = counter ready
                , referenced = makeRefs parsed
                }

        newReady = ready
            { summaries =
                Map.insert
                    messageId
                    newSummary
                    (summaries ready)
            , counter = (counter ready) + 1
            }
        in
        ( BatchO
            [ WriteFileO
                (makeBlobPath (root ready) hash)
                header
            , cacheSummary (root ready) newSummary
            , dumpCache newReady
            ]
        , ReadyS ready
        )


messageIdLen =
    24


headerP :: P.Parser Header
headerP = do
    mainBox <- sizedStringP
    subject <- sizedStringP
    sharers <- sharersP
    lastEdit <- timeP
    blobs <- blobsP
    wasm <- hash32P
    return $
        Header {mainBox, subject, sharers, lastEdit, blobs, wasm}


sharersP :: P.Parser (Set.Set UserId)
sharersP = do
    size <- uint32P
    sharers <- P.count size userIdP
    return $ Set.fromList sharers


timeP :: P.Parser PosixMillis
timeP = do
    t <- uint64P
    return $ PosixMillis t


newtype PosixMillis
    = PosixMillis Integer


blobsP :: P.Parser (Set.Set Blob)
blobsP = do
    size <- uint32P
    blobs <- P.count size blobP
    return $ Set.fromList blobs


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


makeRefs :: Header -> Set.Set Hash32
makeRefs header =
    let
    blobRefs = Set.map hashB $ blobs header
    in
    Set.insert (wasm header) blobRefs


data Header
    = Header
        { mainBox :: T.Text
        , subject :: T.Text
        , sharers :: Set.Set UserId
        , lastEdit :: PosixMillis
        , blobs :: Set.Set Blob
        , wasm :: Hash32
        }


data Blob
    = Blob
        { hashB :: Hash32
        , filename :: T.Text
        , size :: Integer
        , mime :: T.Text
        }
        deriving (Eq, Ord)


sharePairUpdate
    :: Username
    -> MessageId
    -> TheirSet
    -> Ready
    -> (Output, State)
sharePairUpdate from messageId theirs@(TheirSet theirSet) ready =
    let
    name = ShareName messageId from
    oldPairs = sharePairs ready
    in
    case Map.lookup name oldPairs of
    Nothing ->
        let
        pair = SharePair theirs (MySet Set.empty)
        pairs = Map.insert name pair oldPairs
        newReady = ready { sharePairs = pairs }
        in
        wrapReady $ bumpShare name newReady

    Just (SharePair (TheirSet theirOld) m) ->
        let
        newTheirs = TheirSet $ Set.union theirSet theirOld
        pair = SharePair newTheirs m
        pairs = Map.insert name pair oldPairs
        newReady = ready {sharePairs = pairs}
        in
        wrapReady $ bumpShare name newReady


wrapReady :: (Output, Ready) -> (Output, State)
wrapReady (output, ready) =
    (output, ReadyS ready)


bumpShares :: Ready -> (Output, Ready)
bumpShares ready =
    let
    names = Map.keys $ sharePairs ready
    (outputs, newReady) = foldr bumpHelp ([], ready) names
    in
    (BatchO outputs, newReady)


bumpHelp :: ShareName -> ([Output], Ready) -> ([Output], Ready)
bumpHelp name (oldOutputs, oldReady) =
    let
    (newOutput, newReady) = bumpShare name oldReady
    in
    (newOutput : oldOutputs, newReady)


bumpShare :: ShareName -> Ready -> (Output, Ready)
bumpShare name ready =
    case Map.lookup name $ sharePairs ready of
    Nothing ->
        (DoNothingO, ready)

    Just (SharePair (TheirSet theirs) (MySet mine)) ->
        case Set.toList $ Set.difference mine theirs of
        [] ->
            (dumpCache ready, ready)

        d : _ ->
            sendBlob name d ready


sendBlob :: ShareName -> Hash32 -> Ready -> (Output, Ready)
sendBlob name hash ready =
    case sendingBlob ready of
    NoJobs ->
        ( BatchO
            [ dumpCache ready
            , ReadFileO $ makeBlobPath (root ready) hash
            ]
        , ready { sendingBlob = Jobs (ReadingS hash name) [] }
        )

    Jobs current pending ->
        ( dumpCache ready
        , ready
            { sendingBlob =
                Jobs current $
                append (SendWait hash name) pending
            }
        )

makeBlobPath :: RootPath -> Hash32 -> FilePath
makeBlobPath root hash =
    blobsPath root </> showHash hash


blobsPath :: RootPath -> FilePath
blobsPath (RootPath root) =
    root </> "blobs"


append :: a -> [a] -> [a]
append item to =
    reverse $ item : (reverse to)


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
    encodeUint32 (length asList) <>
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
            , sharePairsM = sharePairs ready
            , summariesM = summaries ready
            , counterM = counter ready
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
    = NewMessageA MessageId Header
    | GetSummary
    | AddToWhitelist UserId
    | RemoveFromWhitelist UserId
    | GetMessage Hash32
    | GetWhitelist
    | GetMyId
    | GetMessageHistory MessageId
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
    id_ <- P.take messageIdLen
    return $ MessageId $ Bl.fromStrict id_


fromFrontendP :: P.Parser FromFrontend
fromFrontendP = do
    input <- P.choice
        [ do
            _ <- P.word8 0
            messageId <- messageIdP
            header <- headerP
            return $ NewMessageA messageId header
        , do
            _ <- P.word8 1
            return GetSummary
        , do
            _ <- P.word8 2
            userId <- userIdP
            return $ AddToWhitelist userId
        , do
            _ <- P.word8 3
            userId <- userIdP
            return $ RemoveFromWhitelist userId
        , do
            _ <- P.word8 4
            hash <- hashP
            return $ GetMessage hash
        , do
            _ <- P.word8 5
            return GetWhitelist
        , do
            _ <- P.word8 6
            return GetMyId
        , do
            _ <- P.word8 7
            messageId <- messageIdP
            return $ GetMessageHistory messageId
        , do
            _ <- P.word8 8
            return GetPayments
        , do
            _ <- P.word8 9
            return GetPrice
        , do
            _ <- P.word8 10
            return GetMembership
        ]
    P.endOfInput
    return input


hashP :: P.Parser Hash32
hashP = do
    hash <- P.take 32
    return $ Hash32 $ Bl.fromStrict hash


addToWhitelistUpdate :: UserId -> Ready -> (Output, State)
addToWhitelistUpdate (UserId username fingerprint) ready =
    let
    newWhitelist =
        Map.insert username (fingerprint, Nothing) (whitelist ready)
    newReady = ready { whitelist = newWhitelist }
    encoded = encodeUsernames (Map.keys newWhitelist)
    in
    ( BatchO [BytesInQO toServerQ encoded, dumpCache newReady]
    , ReadyS newReady
    )


encodeUsernames :: [Username] -> Bl.ByteString
encodeUsernames =
    mconcat . map (\(Username u) -> u)


removeFromWhitelistUpdate :: UserId -> Ready -> (Output, State)
removeFromWhitelistUpdate (UserId username fingerprint) ready =
    let
    newWhitelist =
        Map.insert username (fingerprint, Nothing) (whitelist ready)
    newReady = ready { whitelist = newWhitelist }
    encoded = encodeUsernames (Map.keys newWhitelist)
    in
    ( BatchO [BytesInQO toServerQ encoded, dumpCache newReady]
    , ReadyS newReady
    )


getMessageUpdate :: Hash32 -> Ready -> (Output, State)
getMessageUpdate hash ready =
    ( ReadFileO (makeBlobPath (root ready) hash)
    , ReadyS $ ready { gettingMessage = Just hash }
    )


getWhitelistUpdate :: Ready -> (Output, State)
getWhitelistUpdate ready =
    let
    userIds = map makeUserId $ Map.toList $ whitelist ready
    encoded = mconcat $ map encodeUserId userIds
    in
    ( BytesInQO toFrontendQ encoded, ReadyS ready)


makeUserId :: (Username, (Fingerprint, Maybe TheirStatic)) -> UserId
makeUserId (username, (fingerprint, _)) =
    UserId username fingerprint


getMyIdUpdate :: Ready -> (Output, State)
getMyIdUpdate ready =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case authStatus ready of
    GettingPowInfoA ->
        pass

    GeneratingSessionKey _ _ ->
        pass

    AwaitingUsername _ _ ->
        pass

    LoggedIn (StaticKeys _ _ username fingerprint) ->
        let
        userId = UserId username fingerprint
        encoded = Bl.singleton 5 <> encodeUserId userId
        in
        (BytesInQO toFrontendQ encoded, ReadyS ready)


uiApiUpdate :: FromFrontend -> State -> (Output, State)
uiApiUpdate apiInput model =
    case apiInput of
    NewMessageA messageId header ->
        updateReady model $ newMessageUpdate messageId header

    AddToWhitelist userId ->
        updateReady model $ addToWhitelistUpdate userId

    RemoveFromWhitelist userId ->
        updateReady model $ removeFromWhitelistUpdate userId

    GetMessage hash ->
        updateReady model $ getMessageUpdate hash

    GetWhitelist ->
        updateReady model $ getWhitelistUpdate

    GetMyId ->
        updateReady model $ getMyIdUpdate

    GetSummary ->
        undefined

    GetMessageHistory _ ->
        undefined

    GetPayments ->
        undefined

    GetPrice ->
        undefined

    GetMembership ->
        undefined


newMessageUpdate :: MessageId -> Header -> Ready -> (Output, State)
newMessageUpdate messageId header ready =
    let
    encoded = encodeHeader header
    hash = hash32 encoded
    summary =
        summarize
            header
            (Map.lookup messageId $ summaries ready)
            (counter ready)
            hash
    historyPath =
        makeHistoryPath (root ready) (historyId summary)
    newReady =
        ready
            { summaries =
                Map.insert
                    messageId
                    summary
                    (summaries ready)
            , counter = (counter ready) + 1
            , sharePairs =
                newSharePairs
                    (sharePairs ready)
                    messageId
                    summary
                    hash
            }
    (bumpedOut, bumpedReady) = bumpShares newReady
    in
    ( BatchO
        [ bumpedOut
        , dumpCache bumpedReady
        , WriteFileO
            (makeBlobPath (root ready) hash)
            encoded
        , AppendFileO historyPath (encodeSummary summary)
        ]
    , ReadyS bumpedReady
    )


newSharePairs
    :: Map.Map ShareName SharePair
    -> MessageId
    -> Summary
    -> Hash32
    -> Map.Map ShareName SharePair
newSharePairs oldShares messageId summary hash =
    Set.foldr (newSharePairsHelp messageId hash summary) oldShares $
    Set.map (\(UserId u _) -> u) $ sharersS summary


newSharePairsHelp
    :: MessageId
    -> Hash32
    -> Summary
    -> Username
    -> Map.Map ShareName SharePair
    -> Map.Map ShareName SharePair
newSharePairsHelp messageId hash summary from oldShares =
    let
    name = ShareName messageId from
    in
    case Map.lookup name oldShares of
    Nothing ->
        Map.insert
            name
            (SharePair
                (TheirSet Set.empty)
                (MySet $ Set.union
                    (Set.fromList [hash])
                    (referenced summary)))
            oldShares

    Just (SharePair (TheirSet theirs) (MySet mine)) ->
        Map.insert
            name
            (SharePair
                (TheirSet theirs)
                (MySet $ Set.insert hash mine))
            oldShares


encodeHeader :: Header -> Bl.ByteString
encodeHeader header =
    mconcat
        [ encodeString $ mainBox header
        , encodeString $ subject header
        , encodeSharers $ sharers header
        , encodeTime $ lastEdit header
        , encodeBlobs $ blobs header
        , encodeHash $ wasm header
        ]


encodeBlobs :: Set.Set Blob -> Bl.ByteString
encodeBlobs blobs =
    let
    asList = Set.toList blobs
    len = length asList
    in
    encodeUint32 len <> mconcat (map encodeBlob asList)


encodeBlob :: Blob -> Bl.ByteString
encodeBlob blob =
    mconcat
        [ encodeHash $ hashB blob
        , encodeString $ filename blob
        , encodeUint64 $ size blob
        , encodeString $ mime blob
        ]


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
        , sharePairsM :: Map.Map ShareName SharePair
        , summariesM :: Map.Map MessageId Summary
        , counterM :: Int
        }


memCacheP :: P.Parser MemCache
memCacheP = do
    keys <- myKeysP
    handshakesM <- handshakesP
    whitelistM <- whitelistP
    sharePairsM <- sharePairsP
    summariesM <- summariesP
    counterM <- uint32P
    P.endOfInput
    return $
        MemCache
            { keys
            , handshakesM
            , whitelistM
            , sharePairsM
            , summariesM
            , counterM
            }


summariesP :: P.Parser (Map.Map MessageId Summary)
summariesP = do
    numSums <- uint32P
    asList <- P.count numSums summaryP
    return $ Map.fromList asList


summaryP :: P.Parser (MessageId, Summary)
summaryP = do
    messageId <- messageIdP
    headerBlob <- hash32P
    subjectS <- sizedStringP
    sharersS <- sharersP
    lastEditS <- timeP
    historyId <- uint32P
    referenced <- referencedP
    return
        ( messageId
        , Summary
            { headerBlob
            , subjectS
            , sharersS
            , lastEditS
            , historyId
            , referenced
            }
        )


referencedP :: P.Parser (Set.Set Hash32)
referencedP = do
    num <- uint32P
    asList <- P.count num hash32P
    return $ Set.fromList asList


sharePairsP :: P.Parser (Map.Map ShareName SharePair)
sharePairsP = do
    numShares <- uint32P
    asList <- P.count numShares oneShareP
    return $ Map.fromList asList


oneShareP :: P.Parser (ShareName, SharePair)
oneShareP = do
    shareName <- shareNameP
    sharePair <- sharePairP
    return (shareName, sharePair)


shareNameP :: P.Parser ShareName
shareNameP = do
    messageId <- messageIdP
    username <- usernameP
    return $ ShareName messageId username


sharePairP :: P.Parser SharePair
sharePairP = do
    theirSet <- theirSetP
    mySet <- mySetP
    return $ SharePair theirSet mySet


theirSetP :: P.Parser TheirSet
theirSetP = do
    len <- uint32P
    asList <- P.count len hash32P
    return $ TheirSet $ Set.fromList asList


mySetP :: P.Parser MySet
mySetP = do
    len <- uint32P
    asList <- P.count len hash32P
    return $ MySet $ Set.fromList asList


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
            , messageLocks = Set.empty
            , authStatus = LoggedIn $ keys memCache
            , handshakes = handshakesM memCache
            , newDhKeys
            , theTime = times
            , whitelist = whitelistM memCache
            , blobLocks = Set.empty
            , sharePairs = sharePairsM memCache
            , sendingBlob = NoJobs
            , randomGen = gen
            , summaries = summariesM memCache
            , counter = counterM memCache
            , assemblingFile = Nothing
            , gettingMessage = Nothing
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

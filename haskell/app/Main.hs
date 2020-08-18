{-# LANGUAGE OverloadedStrings #-}
module Main where

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
import qualified Data.Time.Clock as Time


main :: IO ()
main =
    mainHelp EmptyS StartM
            

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
            return $ Just $ DirExistsM path

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

        GetTimeO -> do
            time <- Time.getCurrentTime
            return $ Just $ TheTimeM time

        ReadFileLazyO path -> do
            bytes <- Bl.readFile path
            return $ Just $ LazyFileContentsM path bytes


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
    :: (Bl.ByteString -> Stm.STM (Q.TQueue Bl.ByteString) -> Msg)
    -> Sc.ActionM ()
httpPost msg =
    let
        responseQ :: Stm.STM (Q.TQueue Bl.ByteString)
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
    | DbExecute_O FilePath Sql.Query
    | DbExecuteO FilePath Sql.Query Diff
    | BatchO [Output]
    | StartUiO
    | StartUiServerO
    | ErrorO String
    | GetTmpFileHandleO FilePath
    | WriteToHandleO Io.Handle Bl.ByteString FilePath
    | MoveFileO FilePath FilePath
    | BytesInQO (Stm.STM (Q.TQueue Bl.ByteString)) Bl.ByteString
    | CloseFileO Io.Handle
    | GetTimeO
    | ReadFileLazyO FilePath


data Msg
    = StartM
    | AppDataDirM FilePath
    | DirExistsM FilePath
    | FileContentsM FilePath B.ByteString
    | BatchM [Maybe Msg]
    | SetBlobM Bl.ByteString (Stm.STM (Q.TQueue Bl.ByteString))
    | TmpFileHandleM FilePath Io.Handle
    | WrittenToHandleM FilePath Io.Handle
    | MovedFileM FilePath
    | GetBlobM Bl.ByteString (Stm.STM (Q.TQueue Bl.ByteString))
    | FromWebsocketM Ws.DataMessage
    | TheTimeM Time.UTCTime
    | LazyFileContentsM FilePath Bl.ByteString


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


toWebsocketQ :: Stm.STM (Q.TQueue Bl.ByteString)
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


data BlobUploading
    = AwaitingHandleB
        (Stm.STM (Q.TQueue Bl.ByteString))
        Bl.ByteString
        Hash32
    | AwaitingTmpWriteB
        (Stm.STM (Q.TQueue Bl.ByteString))
        Hash32
        FilePath
    | AwaitingMoveB
        (Stm.STM (Q.TQueue Bl.ByteString))
        Hash32
        FilePath


newtype Iota
    = Iota Integer


data State
    = Ready
        { rootS :: RootPath
        , keysS :: StaticKeys
        , blobsUpS :: [BlobUploading]
        , setSnapsS :: [SetSnapshot]
        , getBlobS :: [GetBlob]
        }
    | EmptyS
    | FailedS
    | MakingRootDirS RootPath
    | NoKeysS RootPath



data GetBlob
    = GetBlob Q Hash32


data SetSnapshot
    = TimeS B.ByteString B.ByteString


type Q = Stm.STM (Q.TQueue Bl.ByteString)


keysPath :: RootPath -> FilePath
keysPath (RootPath root) =
    root </> "staticKeys"


dbPath :: RootPath -> FilePath
dbPath (RootPath root) =
    root </> "database.sqlite"


makeRootPath :: FilePath -> FilePath
makeRootPath appDataDir =
    appDataDir </> "bigwebthing"


updateOnLazyFileContents
    :: FilePath
    -> Bl.ByteString
    -> State
    -> (Output, State)
updateOnLazyFileContents path bytes model =
    case model of
        EmptyS ->
            (DoNothingO, model)

        FailedS ->
            (DoNothingO, model)

        MakingRootDirS _ ->
            (DoNothingO, model)

        NoKeysS _ ->
            (DoNothingO, model)

        Ready _ _ _ _ [] ->
            (DoNothingO, model)

        Ready r k b s (GetBlob q hash : etBlobs) ->
            if path == makeBlobPath r hash then
                ( BytesInQO q bytes, Ready r k b s etBlobs)
            else
                (DoNothingO, model)


update :: State -> Msg -> (Output, State)
update model msg =
    case msg of
        StartM ->
            (GetAppDataDirO, EmptyS)

        LazyFileContentsM path bytes ->
            updateOnLazyFileContents path bytes model

        AppDataDirM path ->
            ( MakeDirIfMissingO $ makeRootPath path
            , MakingRootDirS $ RootPath $ makeRootPath path
            )

        DirExistsM path ->
            dirExistsUpdate path model

        FileContentsM path contents ->
            fileContentsUpdate path contents model

        BatchM msgs ->
            let
                (outputs, newModel) = batchUpdate model msgs []
            in
                (BatchO outputs, newModel)

        SetBlobM blob responseQ ->
            setBlobUpdate blob responseQ model

        TmpFileHandleM path handle ->
            onTmpFileHandleUpdate path handle model

        WrittenToHandleM path handle ->
            onWrittenToTmp path handle model

        MovedFileM new ->
            onMovedFile new model

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

        TheTimeM t ->
            onTimeUpdate t model

        GetBlobM body q ->
            onGetBlobUpdate model body q


onGetBlobUpdate :: State -> Bl.ByteString -> Q -> (Output, State)
onGetBlobUpdate model body q =
    case P.eitherResult $ P.parse blobHashP $ Bl.toStrict body of
        Left err ->
            ( ErrorO $ "could not parse GetBlob: " <> err
            , FailedS
            )

        Right hash ->
            case model of
                EmptyS ->
                    (DoNothingO, model)

                FailedS ->
                    (DoNothingO, model)

                MakingRootDirS _ ->
                    (DoNothingO, model)

                NoKeysS _ ->
                    (DoNothingO, model)

                Ready r k b s getBlobs ->
                    ( ReadFileLazyO $ makeBlobPath r hash
                    , Ready r k b s (consEnd (GetBlob q hash) getBlobs)
                    )


consEnd :: a -> [a] -> [a]
consEnd new old =
    reverse $ new : reverse old


onTimeUpdate :: Time.UTCTime -> State -> (Output, State)
onTimeUpdate t model =
    case model of
        EmptyS ->
            (DoNothingO, model)

        FailedS ->
            (DoNothingO, model)

        MakingRootDirS _ ->
            (DoNothingO, model)

        NoKeysS _ ->
            (DoNothingO, model)

        Ready _ _ _ [] _ ->
            (DoNothingO, model)

        Ready root keys@(StaticKeys _ _ userId) blobs (s:naps) getBlobs ->
            let
                (query, params) = makeDiffDbInsert userId s t
            in
                ( DbExecuteO (dbPath root) query params
                , Ready root keys blobs naps getBlobs
                )


makeDiffDbInsert
    :: UserId
    -> SetSnapshot
    -> Time.UTCTime
    -> (Sql.Query, Diff)
makeDiffDbInsert (UserId userId) (TimeS previous next) t =
    let
        (start, end, insert) = makeDiff previous next
        (Hash20 previousHash) = hash20 previous
        (Hash20 hash) = hash20 next
    in
        ( "INSERT INTO diffs (\n\
          \    hash,\n\
          \    previous_hash,\n\
          \    start,\n\
          \    end,\n\
          \    insert,\n\
          \    time,\n\
          \    author)\n\
          \VALUES (?, ?, ?, ?, ?, ?, ?);"
        , (hash, previousHash, start, end, insert, t, userId)
        )


type Diff =
    ( B.ByteString
    , B.ByteString
    , Int
    , Int
    , B.ByteString
    , Time.UTCTime
    , Integer
    )


makeDiff :: B.ByteString -> B.ByteString -> (Int, Int, B.ByteString)
makeDiff previous next =
    let
        prevUnpack = B.unpack previous
        nextUnpack = B.unpack next
        zipForward = zip prevUnpack nextUnpack
        zipBackward = zip (reverse prevUnpack) (reverse nextUnpack)
        start = countIdentical zipForward
        end = length zipBackward - (countIdentical zipBackward) 
        newEnd = end + length prevUnpack - length nextUnpack
        insert = B.pack $ drop start $ drop newEnd nextUnpack
    in
        (start, end, insert)


countIdentical :: [(Word8, Word8)] -> Int
countIdentical zipped =
    countIdenticalHelp zipped 0


countIdenticalHelp :: [(Word8, Word8)] -> Int -> Int
countIdenticalHelp zipped count =
    case zipped of
        [] ->
            count

        (z1, z2) : ipped ->
            if z1 == z2 then
                countIdenticalHelp ipped $ count + 1
            else
                count
            

hash20 :: B.ByteString -> Hash20
hash20 bytes =
    (Hash20 . Blake.finalize 20 . Blake.update bytes)
     (Blake.initialize 20)


parseApiInput :: Bl.ByteString -> Either String ApiInput 
parseApiInput raw =
    P.eitherResult $ P.parse apiInputP $ Bl.toStrict raw


newtype UserId =
    UserId Integer


newtype Hash20 =
    Hash20 B.ByteString


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


hash20P :: P.Parser Hash20
hash20P = do
    hash <- P.take 20
    return $ Hash20 hash


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
    = SetSnapshotA B.ByteString B.ByteString
    | SendMessageA Hash20 Hash20 UserId
    | AddToWhitelistA UserId
    | RemoveFromWhitelistA UserId
    | GetMessageA Hash20 Hash20
    | GetWhitelistA
    | GetMyIdA
    | GetDraftsSummaryA
    | GetSentSummaryA
    | GetInboxSummaryA


apiInputP :: P.Parser ApiInput
apiInputP = do
    P.choice
        [ do
            _ <- P.word8 0
            previous <- bytesP
            new <- P.takeByteString
            return $ SetSnapshotA previous new
        , do
            _ <- P.word8 1
            previous <- hash20P
            new <- hash20P
            userId <- userIdP
            return $ SendMessageA previous new userId
        , do
            _ <- P.word8 2
            userId <- userIdP
            return $ AddToWhitelistA userId
        , do
            _ <- P.word8 3
            userId <- userIdP
            return $ RemoveFromWhitelistA userId
        , do
            _ <- P.word8 5
            previous <- hash20P
            new <- hash20P
            return $ GetMessageA previous new
        , do
            _ <- P.word8 6
            return GetWhitelistA
        , do
            _ <- P.word8 7
            return GetMyIdA
        , do
            _ <- P.word8 8
            return GetDraftsSummaryA
        , do
            _ <- P.word8 9 
            return GetSentSummaryA
        , do
            _ <- P.word8 10
            return GetInboxSummaryA
        ]


uiApiUpdate
    :: ApiInput
    -> State
    -> (Output, State)
uiApiUpdate apiInput model =
    case apiInput of
        SetSnapshotA before after ->
            onSetSnapshot before after model

        SendMessageA _ _ _ ->
            undefined

        AddToWhitelistA _ ->
            undefined

        RemoveFromWhitelistA _ ->
            undefined

        GetMessageA _ _ ->
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


onSetSnapshot
    :: B.ByteString
    -> B.ByteString
    -> State
    -> (Output, State)
onSetSnapshot before after model =
    case model of
        EmptyS ->
            (DoNothingO, model)

        FailedS ->
            (DoNothingO, model)

        MakingRootDirS _ ->
            (DoNothingO, model)

        NoKeysS _ ->
            (DoNothingO, model)

        Ready root keys blobs setSnaps getBlobs ->
            ( GetTimeO
            , Ready root keys blobs (TimeS before after : setSnaps) getBlobs
            )


onMovedFile :: FilePath -> State -> (Output, State)
onMovedFile new model =
    case model of
        EmptyS ->
            (DoNothingO, model)

        FailedS ->
            (DoNothingO, model)

        MakingRootDirS _ ->
            (DoNothingO, model)

        NoKeysS _ ->
            (DoNothingO, model)

        Ready _ _ [] _ _ ->
            (DoNothingO, model)

        Ready rootPath keys (AwaitingMoveB q (Hash32 hash) toPath : lobs) snaps getBlobs ->
            if toPath == new then
                ( BytesInQO q (Bl.singleton 1 <> Bl.fromStrict hash) 
                , Ready rootPath keys lobs snaps getBlobs
                )

            else
                ( DoNothingO, model)

        Ready _ _ (AwaitingTmpWriteB _ _ _ : _) _ _ ->
            (DoNothingO, model)

        Ready _ _ (AwaitingHandleB _ _ _ : _) _ _ ->
            (DoNothingO, model)


onWrittenToTmp :: FilePath -> Io.Handle -> State -> (Output, State)
onWrittenToTmp writtenPath handle model =
    case model of
        EmptyS ->
            (DoNothingO, model)

        FailedS ->
            (DoNothingO, model)

        MakingRootDirS _ ->
            (DoNothingO, model)

        NoKeysS _ ->
            (DoNothingO, model)

        Ready _ _ [] _ _ ->
            (DoNothingO, model)

        Ready rootPath keys (AwaitingTmpWriteB q hash expectPath : lobs) snaps getBlobs ->
            if writtenPath == expectPath then
                let
                    blobPath = makeBlobPath rootPath hash
                in
                    ( BatchO
                        [ MoveFileO writtenPath blobPath
                        , CloseFileO handle
                        ]
                    , Ready
                        rootPath
                        keys
                        (AwaitingMoveB q hash blobPath : lobs)
                        snaps
                        getBlobs
                    )
            else
                (DoNothingO, model)

        Ready _ _ (AwaitingHandleB _ _ _ : _) _ _ ->
            (DoNothingO, model)

        Ready _ _ (AwaitingMoveB _ _ _ : _) _ _ ->
            (DoNothingO, model)


encodeHash :: Hash32 -> String
encodeHash (Hash32 hash) =
    Bc.unpack $ B64.encodeUnpadded hash


makeBlobPath :: RootPath -> Hash32 -> FilePath
makeBlobPath (RootPath rootPath) hash =
    rootPath </> encodeHash hash


onTmpFileHandleUpdate
    :: FilePath
    -> Io.Handle
    -> State
    -> (Output, State)
onTmpFileHandleUpdate path handle model =
    case model of
        EmptyS ->
            (DoNothingO, model)

        FailedS ->
            (DoNothingO, model)

        MakingRootDirS _ ->
            (DoNothingO, model)

        NoKeysS _ ->
            (DoNothingO, model)

        Ready _ _ [] _ _ ->
            (DoNothingO, model)

        Ready root keys (AwaitingHandleB q blob hash : lobs) snaps getBlobs ->
            ( WriteToHandleO handle blob path
            , Ready
                root
                keys
                (AwaitingTmpWriteB q hash path : lobs)
                snaps
                getBlobs
            )

        Ready _ _ (AwaitingTmpWriteB _ _ _ : _) _ _ ->
            (DoNothingO, model)

        Ready _ _ (AwaitingMoveB _ _ _ : _) _ _ ->
            (DoNothingO, model)


setBlobUpdate ::
    Bl.ByteString ->
    Stm.STM (Q.TQueue Bl.ByteString) ->
    State ->
    (Output, State)
setBlobUpdate blob responseQ model =
    case model of
        EmptyS ->
            (DoNothingO, model)

        FailedS ->
            (DoNothingO, model)

        MakingRootDirS _ ->
            (DoNothingO, model)

        NoKeysS _ -> 
            (DoNothingO, model)

        Ready rootPath keys blobs snaps getBlobs ->
            ( GetTmpFileHandleO $ tempPath rootPath
            , Ready
                rootPath
                keys
                (AwaitingHandleB
                    responseQ
                    blob
                    (hash32 blob) :
                blobs)
                snaps
                getBlobs
            )


hash32 :: Bl.ByteString -> Hash32
hash32 =
    Hash32 .
    Blake.finalize 32 .
    Bl.foldrChunks Blake.update (Blake.initialize 32)
                        

tempPath :: RootPath -> FilePath
tempPath (RootPath root) =
    root </> "temporary"

                    
iotaPlusOne :: Iota -> Iota            
iotaPlusOne (Iota i) =
    Iota (i + 1)


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
        FailedS ->
            (DoNothingO, model)

        EmptyS ->
            (DoNothingO, model)

        MakingRootDirS _ ->
            (DoNothingO, model)

        NoKeysS root ->
            if path == keysPath root then
                rawKeysUpdate contents root
            else
                (DoNothingO, model)

        Ready _ _ _ _ _ ->
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


bytesP :: P.Parser B.ByteString
bytesP = do
    n <- uint32P
    P.take n


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
            , Ready root keys [] [] []
            )


setupDatabase :: RootPath -> Output
setupDatabase (RootPath root) =
    BatchO $ map (DbExecute_O root)
        [ "CREATE TABLE IF NOT EXISTS diffs (\n\
          \    hash BLOB NOT NULL,\n\
          \    previous_hash BLOB NOT NULL,\n\
          \    start INTEGER NOT NULL,\n\
          \    end INTEGER NOT NULL,\n\
          \    insert BLOB NOT NULL,\n\
          \    time TEXT NOT NULL,\n\
          \    author INTEGER NOT NULL,\n\
          \    PRIMARY KEY (hash, previous_hash)\n\
          \);"
        , "CREATE TABLE IF NOT EXISTS sent (\n\
          \    hash BLOB NOT NULL,\n\
          \    time TEXT NOT NULL,\n\
          \    to INTEGER NOT NULL,\n\
          \    PRIMARY KEY (hash, time, to)\n\
          \);"
        , "CREATE TABLE IF NOT EXISTS received (\n\
          \    from INTEGER NOT NULL,\n\
          \    hash BLOB NOT NULL,\n\
          \    time TEXT NOT NULL,\n\
          \    PRIMARY KEY (from, hash, time)\n\
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


dirExistsUpdate :: FilePath -> State -> (Output, State)
dirExistsUpdate path model =
    case model of
        FailedS ->
            (DoNothingO, model)

        EmptyS ->
            (DoNothingO, model)

        MakingRootDirS root@(RootPath r) ->
            if path == r then
                ( BatchO
                    [ setupDatabase root
                    , ReadFileO $ keysPath root
                    , StartUiServerO
                    , StartUiO
                    ]
                , NoKeysS $ RootPath path
                )
            else
                (DoNothingO, model)

        NoKeysS _ ->
            (DoNothingO, model)

        Ready _ _ _ _ _ ->
            (DoNothingO, model)

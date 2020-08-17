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
            return $ Just $ WrittenToTmpM path

        MoveFileO oldPath newPath -> do
            Dir.renameFile oldPath newPath
            return $ Just $ MovedFileM newPath

        BytesInQO q bytes -> do
            Stm.atomically $ do
                q_ <- q
                Q.writeTQueue q_ bytes
            return Nothing
            

httpApi :: IO Wai.Application
httpApi =
    Sc.scottyApp $ do

        Sc.get "/static/:requested" $ do
            requested <- Sc.param "requested"
            Sc.file $ "static/" ++ requested

        Sc.post "/setblob" $
            let
                responseQ :: Stm.STM (Q.TQueue Bl.ByteString)
                responseQ =
                    Q.newTQueue
            in do
                body <- Sc.body
                liftIO $ Stm.atomically $ do
                    q <- msgQ
                    Q.writeTQueue q $ SetBlobM body Q.newTQueue
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
    | BatchO [Output]
    | StartUiO
    | StartUiServerO
    | ErrorO String
    | GetTmpFileHandleO FilePath
    | WriteToHandleO Io.Handle Bl.ByteString FilePath
    | MoveFileO FilePath FilePath
    | BytesInQO (Stm.STM (Q.TQueue Bl.ByteString)) Bl.ByteString


data Msg
    = StartM
    | AppDataDirM FilePath
    | DirExistsM FilePath
    | FileContentsM FilePath B.ByteString
    | BatchM [Maybe Msg]
    | SetBlobM Bl.ByteString (Stm.STM (Q.TQueue Bl.ByteString))
    | TmpFileHandleM FilePath Io.Handle
    | WrittenToTmpM FilePath
    | MovedFileM FilePath


websocket :: Ws.ServerApp
websocket pending = do
    conn <- Ws.acceptRequest pending
    websocketSend conn


websocketSend :: Ws.Connection -> IO ()
websocketSend conn = do
    out <- Stm.atomically $ do
        q <- toWebsocketQ
        Q.readTQueue q
    Ws.sendDataMessage conn $ Ws.Binary out
    websocketSend conn


toWebsocketQ :: Stm.STM (Q.TQueue Bl.ByteString)
toWebsocketQ =
    Q.newTQueue


newtype SessionKey
    = SessionKey B.ByteString

newtype RootPath
    = RootPath FilePath

data StaticKeys
    = StaticKeys (Dh.KeyPair Curve.Curve25519) SessionKey


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
    = EmptyS
    | FailedS
    | MakingRootDirS RootPath
    | MainS
        RootPath
        (Maybe StaticKeys)
        [BlobUploading]


keysPath :: RootPath -> FilePath
keysPath (RootPath root) =
    root </> "staticKeys"


dbPath :: RootPath -> FilePath
dbPath (RootPath root) =
    root </> "database.sqlite"


makeRootPath :: FilePath -> FilePath
makeRootPath appDataDir =
    appDataDir </> "bigwebthing"


update :: State -> Msg -> (Output, State)
update model msg =
    case msg of
        StartM ->
            (GetAppDataDirO, EmptyS)

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

        WrittenToTmpM path ->
            onWrittenToTmp path model

        MovedFileM new ->
            onMovedFile new model


onMovedFile :: FilePath -> State -> (Output, State)
onMovedFile new model =
    case model of
        EmptyS ->
            (DoNothingO, model)

        FailedS ->
            (DoNothingO, model)

        MakingRootDirS _ ->
            (DoNothingO, model)

        MainS _ _ [] ->
            (DoNothingO, model)

        MainS rootPath keys (AwaitingMoveB q (Hash32 hash) toPath : lobs) ->
            if toPath == new then
                ( BytesInQO q (Bl.singleton 1 <> Bl.fromStrict hash) 
                , MainS rootPath keys lobs
                )

            else
                ( DoNothingO, model)

        MainS _ _ (AwaitingTmpWriteB _ _ _ : _) ->
            (DoNothingO, model)

        MainS _ _ (AwaitingHandleB _ _ _ : _) ->
            (DoNothingO, model)


onWrittenToTmp :: FilePath -> State -> (Output, State)
onWrittenToTmp writtenPath model =
    case model of
        EmptyS ->
            (DoNothingO, model)

        FailedS ->
            (DoNothingO, model)

        MakingRootDirS _ ->
            (DoNothingO, model)

        MainS _ _ [] ->
            (DoNothingO, model)

        MainS rootPath keys (AwaitingTmpWriteB q hash expectPath : lobs) ->
            if writtenPath == expectPath then
                let
                    blobPath = makeBlobPath rootPath hash
                in
                    ( MoveFileO writtenPath blobPath
                    , MainS
                        rootPath
                        keys
                        (AwaitingMoveB q hash blobPath : lobs)
                    )
            else
                (DoNothingO, model)

        MainS _ _ (AwaitingHandleB _ _ _ : _) ->
            (DoNothingO, model)

        MainS _ _ (AwaitingMoveB _ _ _ : _) ->
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

        MainS _ _ [] ->
            (DoNothingO, model)

        MainS rootPath keys (AwaitingHandleB q blob hash : lobs) ->
            ( WriteToHandleO handle blob path
            , MainS
                rootPath
                keys
                (AwaitingTmpWriteB q hash path : lobs)
            )

        MainS _ _ (AwaitingTmpWriteB _ _ _ : _) ->
            (DoNothingO, model)

        MainS _ _ (AwaitingMoveB _ _ _ : _) ->
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

        MainS rootPath keys blobs ->
            ( GetTmpFileHandleO $ tempPath rootPath
            , MainS
                rootPath
                keys
                (AwaitingHandleB
                    responseQ
                    blob
                    (hash32 blob) :
                blobs)
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

        MainS root Nothing blobs ->
            if path == keysPath root then
                rawKeysUpdate contents root blobs
            else
                (DoNothingO, model)

        MainS _ (Just _) _ ->
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


sessionKeyLength :: Int
sessionKeyLength =
    16


myKeysP :: P.Parser StaticKeys
myKeysP = do
    sessionKey <- P.take sessionKeyLength
    rawDhKey <- P.takeByteString
    case Dh.dhBytesToPair $ Noise.convert rawDhKey of
        Nothing ->
            fail "could not parse secret key from file"

        Just dhKeys ->
            return $ StaticKeys dhKeys (SessionKey sessionKey)

        
rawKeysUpdate :: B.ByteString -> RootPath -> [BlobUploading] -> (Output, State)
rawKeysUpdate rawKeys root blobs =
    case parseKeys rawKeys of
        Left err ->
            ( ErrorO $ T.unpack $
                "could not parse static keys: " <> err
            , FailedS
            )

        Right keys ->
            ( DoNothingO, MainS root (Just keys) blobs)


setupDatabase :: RootPath -> Output
setupDatabase (RootPath root) =
    BatchO $ map (DbExecute_O root)
        [ "CREATE TABLE IF NOT EXISTS diffs (\n\
          \    hash BLOB NOT NULL PRIMARY KEY,\n\
          \    start INTEGER NOT NULL,\n\
          \    end INTEGER NOT NULL,\n\
          \    insert BLOB NOT NULL,\n\
          \    previous_hash BLOB NOT NULL,\n\
          \    time TEXT NOT NULL,\n\
          \    author INTEGER NOT NULL,\n\
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
                , MainS (RootPath path) Nothing []
                )
            else
                (DoNothingO, model)

        MainS _ _ _ ->
            (DoNothingO, model)

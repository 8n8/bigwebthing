{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Concurrent.STM as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.ByteString as B
import System.FilePath ((</>))
import qualified Network.Simple.TCP as Tcp
import qualified Control.Exception as E
import qualified Crypto.Random as CryptoRand
import qualified Database.SQLite.Simple as Db
import qualified Data.Text.IO as Tio
import Update


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


io :: TVar.TVar State -> Output -> IO ()
io mainState output =
    case output of
    CloseSocketO socket -> do
        _ <- (E.try $ Tcp.closeSock socket)
                :: IO (Either E.IOException ())
        return ()

    BatchO b ->
        mapM_ (io mainState) b

    TcpRecvO address socket len -> do
        result <- E.try $ Tcp.recv socket len
        updateIo mainState $ TcpMsgInM address result

    MsgInSocketO address socket msg -> do
        result <- E.try $ Tcp.send socket msg
        updateIo mainState $ TcpSendResultM address result

    PrintO msg ->
        Tio.putStrLn msg

    DoNothingO ->
        return ()

    ReadAccessListO -> do
        result <- E.try $ B.readFile accessListPath
        updateIo mainState $ AccessListM result

    StartTcpServerO ->
        Tcp.serve (Tcp.Host "127.0.0.1") "11453" $ \(sock, addr) ->
        updateIo mainState $ NewTcpConnM sock addr

    DeleteInboxMessageDbO sender recipient message ->
        Db.withConnection dbPath $ \conn ->
            Db.execute
                conn
                deleteMessageSql
                (sender, recipient, message)

    GetRandomGenO -> do
        drg <- CryptoRand.drgNew
        updateIo mainState $ RandomGenM drg

    SaveMessageToDbO sender recipient inboxMessage ->
        Db.withConnection dbPath $ \conn ->
            Db.execute
                conn
                saveMessageSql
                (sender, recipient, inboxMessage)

    GetMessageFromDbO recipient -> do
        result <- Db.withConnection dbPath $ \conn ->
            Db.query
                conn
                getMessageSql
                (Db.Only recipient)
        updateIo mainState $ MessagesFromDbM recipient result

    SetupDbO ->
        Db.withConnection dbPath $ \conn ->
            Db.execute_ conn makeMessagesTableSql


makeMessagesTableSql :: Db.Query
makeMessagesTableSql =
    "CREATE TABLE IF NOT EXISTS messages \
    \(sender BLOB NOT NULL,\
    \ recipient BLOB NOT NULL,\
    \ message BLOB NOT NULL,\
    \ PRIMARY KEY (sender, recipient, message));"


saveMessageSql :: Db.Query
saveMessageSql =
    "INSERT INTO messages (sender, recipient, message) \
    \VALUES (?, ?, ?);"


getMessageSql :: Db.Query
getMessageSql =
    "SELECT sender, message FROM messages WHERE recipient=?;"


deleteMessageSql :: Db.Query
deleteMessageSql =
    "DELETE FROM messages \
    \WHERE sender = ? AND recipient = ? and message = ?;"


rootPath :: FilePath
rootPath =
    "bwtdata"


dbPath :: FilePath
dbPath =
    rootPath </> "database.sqlite"


accessListPath :: FilePath
accessListPath =
    rootPath </> "accessList.txt"

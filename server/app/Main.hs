{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main (main) where

import qualified System.Directory as Dir
import qualified Data.Attoparsec.ByteString as P
import qualified Control.Concurrent.STM as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.ByteString as B
import System.FilePath ((</>))
import qualified Data.Text as T
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
io mainState output = do
  case output of
    MsgInSocketO address socket msg -> do
        result <- E.try $ Tcp.send socket msg
        updateIo mainState $ TcpSendResultM address result

    CloseSocketO socket ->
        Tcp.closeSock socket

    PrintO msg ->
        Tio.putStrLn msg

    DoNothingO ->
        return ()

    ReadAccessListO -> do
        result <- E.try $ B.readFile accessListPath
        updateIo mainState $ AccessListM result

    StartTcpServerO ->
        tcpServer mainState

    BatchO outputs ->
        mapM_ (io mainState) outputs

    MakeDirIfNotThereO path ->
        Dir.createDirectoryIfMissing True path

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


tcpServer :: TVar.TVar State -> IO ()
tcpServer mainState =
    Tcp.serve (Tcp.Host "127.0.0.1") "11453" (tcpServerHelp mainState)


tcpServerHelp :: TVar.TVar State -> (Tcp.Socket, Tcp.SockAddr) -> IO ()
tcpServerHelp mainState (socket, address) = do
    updateIo mainState $ NewTcpConnM socket address
    tcpReceiver mainState socket address


tcpRecv
    :: Tcp.Socket
    -> Int
    -> IO (Either E.IOException (Maybe B.ByteString))
tcpRecv socket len =
    E.try $ Tcp.recv socket len


uint16P :: P.Parser Int
uint16P = do
    b0 <- uint8P
    b1 <- uint8P
    return $ b0 + b1 * 256


tcpReceiver :: TVar.TVar State -> Tcp.Socket -> Tcp.SockAddr -> IO ()
tcpReceiver mainState socket address = do
    eitherMsgLen <- tcpRecv socket 2
    case eitherMsgLen of
        Left err -> do
            Tio.putStrLn "1"
            Tio.putStrLn $ "tcpRecv error: " <> T.pack (show err)
            updateIo mainState $ DeadTcpM address

        Right Nothing -> do
            Tio.putStrLn "2"
            updateIo mainState $ DeadTcpM address

        Right (Just rawLen) ->
            case P.parseOnly uint16P rawLen of
            Left _ -> do
                Tio.putStrLn "3"
                updateIo mainState $ DeadTcpM address

            Right len -> do
                if len < maxMessageLength then do
                    eitherMsg <- tcpRecv socket len
                    case eitherMsg of
                        Left _ -> do
                            Tio.putStrLn "4"
                            updateIo mainState $ DeadTcpM address

                        Right Nothing -> do
                            Tio.putStrLn "5"
                            updateIo mainState $ DeadTcpM address

                        Right (Just msg) -> do
                            updateIo mainState $ TcpMsgInM address msg
                            tcpReceiver mainState socket address

                else do
                    Tio.putStrLn "6"
                    Tio.putStrLn $
                        "length: " <> T.pack (show len) <> "\n====="
                    updateIo mainState $ DeadTcpM address


uint8P :: P.Parser Int
uint8P =
    fromIntegral <$> P.anyWord8


rootPath :: FilePath
rootPath =
    "bwtdata"


dbPath :: FilePath
dbPath =
    rootPath </> "database.sqlite"


accessListPath =
    rootPath </> "accessList.txt"


maxMessageLength :: Int
maxMessageLength =
    16000

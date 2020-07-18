{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Streamly.Prelude as S
import Streamly
import qualified Control.Concurrent as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import qualified Network.Simple.TCP as Tcp
import qualified Control.Monad.STM as Stm
import qualified Control.Concurrent.STM.TQueue as Q
import qualified Data.Attoparsec.ByteString as P
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Handler.Warp (run)
import qualified Network.WebSockets as Ws
import qualified Network.Wai as Wai
import qualified Web.Scotty as Sc
import Data.Text.Encoding (decodeUtf8')


newtype TcpMsg
    = AuthCodeT B.ByteString
    deriving Eq


newtype UiMsg
    = SendMsg T.Text


tcpClient :: IO ()
tcpClient =
    Tcp.connect "localhost" "3001" $ \(socket, _) -> do
        _ <- C.forkIO $ tcpSend socket
        tcpReceive socket


maxServerMsg :: Int
maxServerMsg =
    16000


tcpDelay :: Int
tcpDelay =
    10000000


onTcpReceiveErr :: T.Text -> Tcp.Socket -> IO ()
onTcpReceiveErr err socket = do
    Tio.putStrLn err
    C.threadDelay tcpDelay
    tcpReceive socket


tcpReceive ::  Tcp.Socket -> IO ()
tcpReceive socket = do
    rawLen <- Tcp.recv socket 4
    case decodeRawLen rawLen of
        Left err ->
            onTcpReceiveErr err socket

        Right len -> do
            rawMsg <- Tcp.recv socket len
            case decodeTcpMsg rawMsg of
                Left err ->
                    onTcpReceiveErr err socket

                Right msg -> do
                    Stm.atomically $ do
                        q <- inQ
                        Q.writeTQueue q (FromServerI msg)
                    tcpReceive socket


outQ :: Stm.STM (Q.TQueue B.ByteString)
outQ =
    Q.newTQueue


inQ :: Stm.STM (Q.TQueue In)
inQ =
    Q.newTQueue
        

data In
    = FromServerI FromServer
    | FromWebsocketI FromWebsocket


data FromWebsocket
    = ToServerF B.ByteString
    | GetPowF PowInfo
    | CacheGetF T.Text
    | CacheSetF T.Text B.ByteString
    | CacheDeleteF T.Text
    | SendMessageF B.ByteString T.Text


fromWebsocketP :: P.Parser FromWebsocket
fromWebsocketP = do
    P.choice
        [ toServerP
        , GetPowF <$> powInfoP
        , cacheGetP
        , cacheSetP
        , cacheDeleteP
        , sendMessageP
        ]


cacheDeleteP :: P.Parser FromWebsocket
cacheDeleteP = do
    _ <- P.word8 4
    key <- stringP
    return $ CacheDeleteF key


cacheSetP :: P.Parser FromWebsocket
cacheSetP = do
    _ <- P.word8 3
    key <- stringP
    value <- P.takeByteString
    return $ CacheSetF key value


cacheGetP :: P.Parser FromWebsocket
cacheGetP = do
    _ <- P.word8 2
    CacheGetF <$> stringP


toServerP :: P.Parser FromWebsocket
toServerP = do
    _ <- P.word8 0
    ToServerF <$> P.takeByteString


stringP :: P.Parser T.Text
stringP = do
    len <- uint32P
    raw <- P.take len
    case decodeUtf8' raw of
        Left err ->
            fail $ show err
        Right str ->
            return str


sendMessageP :: P.Parser FromWebsocket
sendMessageP = do
    _ <- P.word8 5
    toId <- P.take 10
    draftId <- stringP
    return $ SendMessageF toId draftId


uint32P :: P.Parser Int
uint32P = do
    l0 <- fmap fromIntegral P.anyWord8
    l1 <- fmap fromIntegral P.anyWord8
    l2 <- fmap fromIntegral P.anyWord8
    l3 <- fmap fromIntegral P.anyWord8
    return $ l0 + l1*256 + l2 * 256*256 + l3 * 256*256*256


data FromServer
    = KeysForIdF TheirKeys
    | PowInfoF PowInfo


data TheirKeys = TheirKeys SignKey EncryptKey


newtype SignKey = SignKey B.ByteString
    

newtype EncryptKey = EncryptKey B.ByteString


data PowInfo
    = PowInfo Int B.ByteString


newtype AuthCode = AuthCode B.ByteString


theirKeysP :: P.Parser FromServer
theirKeysP = do
    sign <- P.take 32
    encrypt <- P.take 32
    P.endOfInput
    return $ KeysForIdF $
        TheirKeys (SignKey sign) (EncryptKey encrypt)


powInfoP :: P.Parser PowInfo
powInfoP = do
    difficulty <- fmap fromIntegral P.anyWord8
    unique <- P.take 16
    P.endOfInput
    return $ PowInfo difficulty unique


fromServerP :: P.Parser FromServer
fromServerP =
    P.choice
        [ theirKeysP
        , PowInfoF <$> powInfoP
        ]


decodeTcpMsg :: Maybe B.ByteString -> Either T.Text FromServer
decodeTcpMsg maybeRaw =
    case maybeRaw of
        Nothing ->
            Left "maybeRaw in decodeTcpMsg was Nothing"

        Just bytes ->
            case P.eitherResult $ P.parse fromServerP bytes of
                Left err ->
                    Left $ T.pack err

                Right ok ->
                    Right ok


decodeRawLen :: Maybe B.ByteString -> Either T.Text Int
decodeRawLen maybeRaw =
    case maybeRaw of
        Nothing ->
            Left "maybeRaw in decodeRawLen was Nothing"

        Just bytes ->
            case P.eitherResult $ P.parse uint32P bytes of
                Left err ->
                    Left $ T.pack err

                Right ok ->
                    Right ok


tcpSend :: Tcp.Socket -> IO ()
tcpSend socket = do
    out <- Stm.atomically $ do
        q <- toServerQ 
        Q.readTQueue q
    Tcp.send socket out
    tcpSend socket


toServerQ :: Stm.STM (Q.TQueue B.ByteString)
toServerQ =
    Q.newTQueue


websocketPort :: Int
websocketPort =
    17448


uiServer :: IO ()
uiServer = do
    app <- scottyApp
    run websocketPort $ websocketsOr
        Ws.defaultConnectionOptions websocket app


websocket :: Ws.ServerApp
websocket pending = do
    conn <- Ws.acceptRequest pending
    websocketReceive conn
    websocketSend conn


scottyApp :: IO Wai.Application
scottyApp =
    Sc.scottyApp $
        Sc.get "/" $ Sc.file "static/index.html"


websocketReceive :: Ws.Connection -> IO ()
websocketReceive conn = do
    raw <- Ws.receiveData conn
    case P.eitherResult $ P.parse fromWebsocketP raw of
        Left err -> do
            Tio.putStrLn $ "FATAL ERROR: " <> T.pack err
            return ()

        Right msg -> do
            Stm.atomically $ do
                q <- inQ
                Q.writeTQueue q $ FromWebsocketI msg
            websocketReceive conn


toWebsocketQ :: Stm.STM (Q.TQueue B.ByteString)
toWebsocketQ =
    Q.newTQueue


websocketSend :: Ws.Connection -> IO ()
websocketSend conn = do
    out <- Stm.atomically $ do
        q <- toWebsocketQ
        Q.readTQueue q
    Ws.sendDataMessage conn $ Ws.Binary $ Bl.fromStrict out
    websocketSend conn


data CacheMsg
    = CacheMsg


inStream :: Serial In
inStream =
    S.repeatM $ Stm.atomically $ do
        q <- inQ
        Q.readTQueue q





main :: IO ()
main = do
    _ <- C.forkIO tcpClient
    _ <- C.forkIO uiServer
    return ()

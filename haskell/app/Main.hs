module Main where

import qualified Streamly.Prelude as S
import Streamly
import qualified Control.Concurrent as C
import qualified Data.ByteString as B
import qualified Data.Text as T


data TcpMsg
    = AuthCodeT B.ByteString
    deriving Eq


getTcpMsg :: IO TcpMsg
getTcpMsg =
    undefined


data UiMsg
    = SendMsg T.Text


getUiMsg :: IO UiMsg
getUiMsg =
    undefined


tcpServer :: IO ()
tcpServer =
    undefined


uiServer :: IO ()
uiServer =
    undefined


cacheServer :: IO ()
cacheServer =
    undefined


data CacheMsg
    = CacheMsg


getCacheMsg :: IO CacheMsg
getCacheMsg =
    undefined


tcpMsgs :: Serial TcpMsg
tcpMsgs =
    S.repeatM getTcpMsg


uiMsgs :: Serial UiMsg
uiMsgs =
    S.repeatM getUiMsg


cacheMsgs :: Serial CacheMsg
cacheMsgs =
    S.repeatM getCacheMsg


isAuth :: TcpMsg -> Bool
isAuth t =
    case t of
        AuthCodeT _ -> True


authCodes :: Serial TcpMsg
authCodes =
    S.filter isAuth tcpMsgs


toServer :: Serial B.ByteString
toServer = do
    
    



main :: IO ()
main = do
    _ <- C.forkIO tcpServer
    _ <- C.forkIO uiServer
    _ <- C.forkIO cacheServer
    return ()

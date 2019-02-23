{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T
import qualified Network.WebSockets as W
import Control.Monad (forever)

main :: IO ()
main = W.runServer "127.0.0.1" 3000 application

application :: W.ServerApp
application pending = do
    conn <- W.acceptRequest pending
    W.forkPingThread conn 30
    forever $ do
        msg <- W.receiveData conn
        W.sendTextData conn $ T.concat [msg, " world!"]
-               

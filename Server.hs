{-# LANGUAGE OverloadedStrings #-}

module Server
  ( main
  ) where

import qualified Control.Concurrent.STM as Stm
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as Bl
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as De
import qualified Web.Scotty as Scot
import qualified Uninvitation as U
import qualified Invitation as I
import qualified Message as Msg
import qualified Crypto as C
import qualified Data.Aeson as J


dataDir :: String
dataDir = "serverData"

hashStr :: C.Hash -> T.Text
hashStr (C.Hash bs) = De.decodeUtf8 bs

main :: IO ()
main = do
  expected <- Stm.atomically $ Stm.newTVar S.empty
  Scot.scotty 4000 $ do
    Scot.post "/blob" $ do
      body <- fmap Bl.toStrict Scot.body
      let hash = C.hash body
      let hashTxt = hashStr hash
      expectedNow <- liftIO $ Stm.readTVarIO expected
      let allowed = S.member hash expectedNow
      let filepath = dataDir ++ '/' : T.unpack hashTxt
      when allowed $ liftIO $ B.writeFile filepath body
    Scot.post "/invite" $ do
      invite <- Scot.jsonData 
      liftIO $ print (invite :: Maybe I.Invitation)

{-# LANGUAGE OverloadedStrings #-}

module Server
  ( main
  ) where

import qualified Control.Concurrent.STM as Stm
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Saltine.Core.Hash as Hash
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as De
import qualified Web.Scotty as Scot
import GHC.Generics (Generic)
import qualified Uninvitation Uninvite
import qualified Invitation as Invite
import qualified Message as Msg

dataDir :: String
dataDir = "serverData"

data RawMsg = RawMsg
    { author :: B.ByteString
    , msg :: RawMsgContent
    } deriving (Generic, Show)

data RawMsgContent
    = Invite
        { invitee :: B.ByteString
        , inviter :: B.ByteString
        , signature :: B.ByteString
        }
    | Uninvite
        { uninvitee :: B.ByteString
        , 
        }
    | FileUpload
        { fileHash :: B.ByteString
        , recipient :: B.ByteString
        } deriving (Generic, Show)

main :: IO ()
main = do
  expected <- Stm.atomically $ Stm.newTVar S.empty
  Scot.scotty 4000 $
    Scot.post "/blob" $ do
      body <- Scot.body
      let hash = B.fromStrict $ Hash.hash $ B.toStrict body
      let hashTxt = De.decodeUtf8 hash
      expectedNow <- liftIO $ Stm.readTVarIO expected
      let allowed = S.member hash expectedNow
      let filepath = dataDir ++ '/' : T.unpack hashTxt
      when allowed $ liftIO $ B.writeFile filepath body
    Scot.post "/invite" $ do

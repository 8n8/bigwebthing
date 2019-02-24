{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Message where

import CommonJson
import qualified Data.Aeson as J
import GHC.Generics (Generic)
import Crypto.Saltine.Core.Sign (PublicKey(..))

data Message = Message
    { hash :: Hash
    , author :: PublicKey
    , signature :: Signature
    } deriving (Generic, Show)

instance ToJSON Hash where
    toEncoding = J.genericToEncoding J.defaultOptions

instance FromJSON Hash

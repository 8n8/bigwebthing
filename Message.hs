{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Message where

import qualified Data.Aeson as J
import GHC.Generics (Generic)
import Crypto (PSign, Sig, Hash)

data Message = Message
    { hash :: Hash
    , author :: PSign
    , signature :: Sig
    } deriving (Generic, Show)

instance J.ToJSON Message where
    toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Message

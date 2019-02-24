{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Uninvitation where

import CommonJson
import qualified Data.Aeson as J
import GHC.Generics (Generic)
import Crypto.Saltine.Core.Sign (PublicKey(..))

data Uninvitation = Invitation
    { uninvitee :: PublicKey
    , author :: PublicKey
    , signature :: Signature
    } deriving (Generic, Show)

instance ToJSON Uninvitation where
    toEncoding = J.genericToEncoding J.defaultOptions

instance FromJSON Uninvitation

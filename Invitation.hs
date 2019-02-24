{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Invitation where

import CommonJson
import qualified Data.Aeson as J
import GHC.Generics (Generic)
import Crypto.Saltine.Core.Sign (PublicKey(..))

data Invitation = Invitation
    { invitee :: PublicKey
    , author :: PublicKey
    , signature :: Signature
    } deriving (Generic)

instance J.ToJSON Invitation where
    toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Invitation

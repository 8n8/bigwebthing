{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Invitation where

import CommonJson
import Crypto (PSign, Sig)
import qualified Data.Aeson as J
import GHC.Generics (Generic)

data Invitation = Invitation
    { invitee :: PSign
    , author :: PSign
    , signature :: Sig
    } deriving (Generic)

instance J.ToJSON Invitation where
    toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Invitation

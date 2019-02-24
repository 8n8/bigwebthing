{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Uninvitation where

import qualified Data.Aeson as J
import GHC.Generics (Generic)
import Crypto (PSign, Sig)

data Uninvitation = Invitation
    { uninvitee :: PSign
    , author :: PSign
    , signature :: Sig
    } deriving (Generic, Show)

instance J.ToJSON Uninvitation where
    toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Uninvitation

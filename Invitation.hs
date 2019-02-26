{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Invitation where

import Crypto (PSign(..), Sig(..))
import qualified Data.Aeson as J
import GHC.Generics (Generic)

data Invitation = Invitation
    { invitee :: PSign
    , author :: PSign
    , signature :: Sig
    } deriving (Generic, Show)

instance J.ToJSON Invitation where
    toEncoding = J.genericToEncoding J.defaultOptions

instance J.FromJSON Invitation where
    parseJSON (J.Object v) = do
        inviteeF <- v J..: "invitee"
        authorF <- v J..: "author"
        signatureF <- v J..: "signature"
        return $ Invitation inviteeF authorF signatureF

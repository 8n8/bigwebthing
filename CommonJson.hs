module CommonJson () where

import qualified Data.ByteString as B
import Crypto (Sig (..), PSign (..), Hash (..))
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Aeson as J
import Control.Monad (MonadPlus, mzero)

textToByteString :: MonadPlus m =>  T.Text -> m B.ByteString
textToByteString x = case B64.decode (E.encodeUtf8 x) of
                     Left _ -> mzero
                     Right bs -> pure bs   

byteStringToText :: B.ByteString -> T.Text
byteStringToText = E.decodeUtf8 . B64.encode

instance J.FromJSON PSign where
    parseJSON (J.String x) = fmap PSign (textToByteString x)

instance J.ToJSON PSign where
    toJSON (PSign bs) = J.toJSON $ byteStringToText bs

instance J.FromJSON Sig where
    parseJSON (J.String x) = fmap Sig (textToByteString x)
    parseJSON _ = mzero

instance J.ToJSON Sig where
    toJSON (Sig bs) = J.toJSON $ byteStringToText bs

instance J.FromJSON Hash where
    parseJSON (J.String x) = fmap Hash (textToByteString x)
    parseJSON _ = mzero

instance J.ToJSON Hash where
    toJSON (Hash bs) = J.toJSON $ byteStringToText bs

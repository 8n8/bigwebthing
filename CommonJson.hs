module CommonJson (Signature) where

-- import Crypto.Saltine.Core.Sign (PublicKey)
import qualified Data.ByteString as B
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

--instance J.FromJSOn PublicKey where
--    parseJSON (J.String x) = fmap PublicKey (textToByteString x)
--
--instance J.ToJSON PublicKey where
--    toJSON (PK bs) = J.toJSON $ byteStringToText bs

newtype Signature = Signature B.ByteString

instance J.FromJSON Signature where
    parseJSON (J.String x) = fmap Signature (textToByteString x)
    parseJSON _ = mzero

instance J.ToJSON Signature where
    toJSON (Signature bs) = J.toJSON $ byteStringToText bs

newtype Hash = Hash B.ByteString

instance J.FromJSON Hash where
    parseJSON (J.String x) = fmap Hash (textToByteString x)
    parseJSON _ = mzero

instance J.ToJSON Hash where
    toJSON (Hash bs) = J.toJSON $ byteStringToText bs

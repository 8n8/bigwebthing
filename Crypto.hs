{-# LANGUAGE ForeignFunctionInterface #-}

module Crypto
  ( eKeyPair
  , hash
  , pDecrypt
  , pEncrypt
  , randomBytes
  , sKeyPair
  , sDecrypt
  , sEncrypt
  , sealEncrypt
  , sealDecrypt
  , sign
  , sodiumInit
  , verify
  , EKeyPair(..)
  , SKeyPair(..)
  , crypto_secretbox_NONCEBYTES
  , crypto_secretbox_KEYBYTES
  , crypto_box_NONCEBYTES
  , PSign (..)
  , SSign (..)
  , PEnc (..)
  , SEnc (..)
  , Sig (..)
  , Nonce (..)
  , Hash (..)
  , SymmetricKey (..)
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as Bi
import qualified Foreign as F
import qualified Foreign.C.Types as T
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Aeson as J
import Control.Monad (MonadPlus, mzero)

newtype SSign = SSign B.ByteString deriving (Show, Ord, Eq)
newtype PSign = PSign B.ByteString deriving (Show, Ord, Eq)
newtype PEnc = PEnc B.ByteString deriving (Show, Ord, Eq)
newtype SEnc = SEnc B.ByteString deriving (Show, Ord, Eq)
newtype Sig = Sig B.ByteString deriving (Show, Ord, Eq)
newtype Nonce = Nonce B.ByteString deriving (Show, Ord, Eq)
newtype Hash = Hash B.ByteString deriving (Show, Ord, Eq)
newtype SymmetricKey = SymmetricKey B.ByteString deriving (Show, Ord, Eq)

textToByteString :: MonadPlus m =>  T.Text -> m B.ByteString
textToByteString x = case B64.decode (E.encodeUtf8 x) of
                     Left _ -> mzero
                     Right bs -> pure bs   

byteStringToText :: B.ByteString -> T.Text
byteStringToText = E.decodeUtf8 . B64.encode

instance J.FromJSON PSign where
    parseJSON (J.String x) = fmap PSign (textToByteString x)
    parseJSON _ = mzero

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

crypto_box_MACBYTES :: Int
crypto_box_MACBYTES = 16

crypto_box_SECRETKEYBYTES :: Int
crypto_box_SECRETKEYBYTES = 32

crypto_generichash_BYTES :: Int
crypto_generichash_BYTES = 32

crypto_box_SEALBYTES :: Int
crypto_box_SEALBYTES = 48

crypto_box_NONCEBYTES :: Int
crypto_box_NONCEBYTES = 24

crypto_secretbox_NONCEBYTES :: Int
crypto_secretbox_NONCEBYTES = 24

crypto_secretbox_KEYBYTES :: Int
crypto_secretbox_KEYBYTES = 32

crypto_sign_BYTES :: Int
crypto_sign_BYTES = 64

-- crypto_sign_SEEDBYTES :: Int
-- crypto_sign_SEEDBYTES = 32

-- crypto_box_SEEDBYTES :: Int
-- crypto_box_SEEDBYTES = 32

packCString :: Int -> F.Ptr F.Word8 -> IO B.ByteString
packCString len cstr = create len $ \p -> Bi.memcpy p cstr len

create :: Int -> (F.Ptr F.Word8 -> IO ()) -> IO B.ByteString
create l f = do
    fp <- mallocByteString l
    F.withForeignPtr fp f
    return $! Bi.PS fp 0 l

mallocByteString :: Int -> IO (F.ForeignPtr a)
mallocByteString = Bi.mallocByteString

foreign import ccall unsafe "sodium.h crypto_generichash"
    c_crypto_generichash
        :: CUString
        -> T.CSize
        -> CUString
        -> T.CULLong
        -> CUString
        -> T.CSize
        -> IO T.CInt

foreign import ccall unsafe "sodium.h crypto_sign_detached"
    c_crypto_sign_detached
        :: CUString
        -> F.Ptr T.CULLong
        -> CUString
        -> T.CULLong
        -> CUString
        -> IO T.CInt

foreign import ccall unsafe "sodium.h randombytes_buf"
    c_randombytes_buf :: F.Ptr T.CUChar -> T.CSize -> IO ()

sign :: B.ByteString -> SSign -> Sig
sign message (SSign secretKey) = unsafePerformIO $
  useAsCString message $ \messagePtr ->
  useAsCString secretKey $ \secretKeyPtr ->
  F.allocaBytes crypto_sign_BYTES $ \sigPtr -> do
    status <- c_crypto_sign_detached
      sigPtr
      F.nullPtr
      messagePtr
      (fromIntegral $ B.length message)
      secretKeyPtr
    case status of
      0 -> fmap Sig $ packCString crypto_sign_BYTES $ F.castPtr sigPtr
      errCode -> error $ "Could not sign. Error code: " ++
                         (show errCode)

hash :: B.ByteString -> Hash
hash x = unsafePerformIO $
  useAsCString x $ \contentPtr ->
  F.allocaBytes crypto_generichash_BYTES $ \hashPtr -> do
    status <- c_crypto_generichash
                  hashPtr
                  (fromIntegral crypto_generichash_BYTES)
                  contentPtr
                  (fromIntegral $ B.length x)
                  F.nullPtr
                  0
    case status of
      0 -> fmap Hash $ packCString crypto_generichash_BYTES $ F.castPtr hashPtr
      errCode -> error $ "Could not hash. Error code: " ++
                         (show errCode)

randomBytes :: Int -> IO B.ByteString
randomBytes len = do
  F.allocaBytes len $ \resultsbuffer -> do
    c_randombytes_buf resultsbuffer (fromIntegral len)
    packCString len $ F.castPtr resultsbuffer

pDecrypt
    :: B.ByteString
    -> Nonce
    -> PEnc
    -> SEnc
    -> Either Int B.ByteString
pDecrypt ciphertext (Nonce nonce) (PEnc publicKey) (SEnc privateKey) =
  let
    ciphertextLength = B.length ciphertext
    msgLen = ciphertextLength - 16
  in
    unsafePerformIO $
      useAsCString ciphertext $ \ciphertextPtr ->
      useAsCString nonce $ \noncePtr ->
      useAsCString publicKey $ \publicKeyPtr ->
      useAsCString privateKey $ \privateKeyPtr ->
      F.allocaBytes msgLen $ \messagePtr -> do
        status <- c_crypto_box_open_easy
                      messagePtr
                      ciphertextPtr
                      (fromIntegral ciphertextLength)
                      noncePtr
                      publicKeyPtr
                      privateKeyPtr
        case status of
          0 -> do
            packed <- packCString msgLen $ F.castPtr messagePtr
            return $ Right packed
          errCode -> return $ Left $ fromIntegral errCode

sDecrypt
    :: B.ByteString
    -> Nonce
    -> SymmetricKey
    -> Either Int B.ByteString
sDecrypt ciphertext (Nonce nonce) (SymmetricKey secretKey) =
  let
    ciphertextLength = B.length ciphertext
    messageLength = ciphertextLength - crypto_box_MACBYTES
  in unsafePerformIO $
    useAsCString ciphertext $ \ciphertextPtr ->
    useAsCString nonce $ \noncePtr ->
    useAsCString secretKey $ \secretKeyPtr ->
    F.allocaBytes messageLength $ \messagePtr -> do
      status <- c_crypto_secretbox_open_easy
                   messagePtr
                   ciphertextPtr
                   (fromIntegral ciphertextLength)
                   noncePtr
                   secretKeyPtr
      case status of
        0 -> do
          packed <- packCString messageLength $ F.castPtr messagePtr
          return $ Right packed
        errCode -> return $ Left $ fromIntegral errCode

sEncrypt
    :: B.ByteString
    -> Nonce
    -> SymmetricKey
    -> Either Int B.ByteString
sEncrypt message (Nonce nonce) (SymmetricKey secretKey) = unsafePerformIO $
  let
    messageLength = B.length message
    ciphertextLength = crypto_box_MACBYTES + messageLength
  in
    useAsCString message $ \messagePtr ->
    useAsCString nonce $ \noncePtr ->
    useAsCString secretKey $ \secretKeyPtr ->
    F.allocaBytes ciphertextLength $ \ciphertextPtr -> do
        status <- c_crypto_secretbox_easy
                      ciphertextPtr
                      messagePtr
                      (fromIntegral $ B.length message)
                      noncePtr
                      secretKeyPtr
        case status of
          0 -> do
            packed <- packCString ciphertextLength $
                         F.castPtr ciphertextPtr
            return $ Right packed
          errCode -> return $ Left $ fromIntegral errCode

pEncrypt
    :: PEnc
    -> SEnc
    -> B.ByteString
    -> Nonce
    -> Either Int B.ByteString
pEncrypt (PEnc publicKey) (SEnc secretKey) message (Nonce nonce) =
  let
    messageLength = B.length message
    cryptoLen = crypto_box_MACBYTES + messageLength
  in unsafePerformIO $ do
    useAsCString publicKey $ \publicKeyPtr ->
      useAsCString secretKey $ \secretKeyPtr ->
      useAsCString message $ \messagePtr ->
      useAsCString nonce $ \noncePtr ->
      F.allocaBytes cryptoLen $ \ciphertextPtr -> do
        status <- c_crypto_box_easy
                       ciphertextPtr
                       messagePtr
                       (fromIntegral messageLength)
                       noncePtr
                       publicKeyPtr
                       secretKeyPtr
        case status of
          0 -> do
            packed <- packCString cryptoLen $ F.castPtr ciphertextPtr
            return $ Right packed
          errCode -> return $ Left $ fromIntegral errCode

sealDecrypt
  :: B.ByteString
  -> PEnc
  -> SEnc
  -> Either Int B.ByteString
sealDecrypt ciphertext (PEnc publicKey) (SEnc secretKey) =
  let ciphertextLength = B.length ciphertext
      messageLength = ciphertextLength - crypto_box_SEALBYTES
  in unsafePerformIO $
     useAsCString ciphertext $ \ciphertextPtr ->
     useAsCString publicKey $ \publicKeyPtr ->
     useAsCString secretKey $ \secretKeyPtr ->
     F.allocaBytes messageLength $ \messagePtr -> do
       status <- c_crypto_box_seal_open
                   messagePtr
                   ciphertextPtr
                   (fromIntegral ciphertextLength)
                   publicKeyPtr
                   secretKeyPtr
       case status of
         0 -> do
           packed <- packCString messageLength $ F.castPtr messagePtr
           return $ Right packed
         errCode -> return $ Left $ fromIntegral errCode

sealEncrypt
  :: B.ByteString -> PEnc -> Either Int B.ByteString
sealEncrypt message (PEnc publicKey) =
  let
    messageLength = B.length message
    cryptoLen = crypto_box_SEALBYTES + messageLength
  in
    unsafePerformIO $
      useAsCString message $ \messagePtr ->
      useAsCString publicKey $ \publicKeyPtr ->
      F.allocaBytes cryptoLen $ \ciphertextPtr -> do
        status <- c_crypto_box_seal
                     ciphertextPtr
                     messagePtr
                     (fromIntegral messageLength)
                     publicKeyPtr
        case status of
          0 -> do
            packed <- packCString cryptoLen $ F.castPtr ciphertextPtr
            return $ Right packed
          errCode -> return $ Left $ fromIntegral errCode

verify :: Sig -> B.ByteString -> PSign -> Bool
verify (Sig sig) message (PSign publicKey) = unsafePerformIO $
  useAsCString sig $ \sigPtr ->
  useAsCString message $ \messagePtr ->
  useAsCString publicKey $ \publicKeyPtr -> do
    status <- c_crypto_sign_verify_detached
                  sigPtr
                  messagePtr
                  (fromIntegral $ B.length message)
                  publicKeyPtr
    return $ status == 0

foreign import ccall unsafe "sodium.h crypto_sign_verify_detached"
  c_crypto_sign_verify_detached :: CUString
                                -> CUString
                                -> T.CULLong
                                -> CUString
                                -> IO T.CInt

foreign import ccall unsafe "sodium.h crypto_secretbox_easy"
  c_crypto_secretbox_easy :: CUString
                          -> CUString
                          -> T.CULLong
                          -> CUString
                          -> CUString
                          -> IO T.CInt

foreign import ccall unsafe "sodium.h crypto_secretbox_open_easy"
  c_crypto_secretbox_open_easy :: CUString
                               -> CUString
                               -> T.CULLong
                               -> CUString
                               -> CUString
                               -> IO T.CInt

foreign import ccall unsafe "sodium.h crypto_box_open_easy"
  c_crypto_box_open_easy :: CUString
                         -> CUString
                         -> T.CULLong
                         -> CUString
                         -> CUString
                         -> CUString
                         -> IO T.CInt

foreign import ccall unsafe "sodium.h crypto_box_easy"
  c_crypto_box_easy :: F.Ptr T.CUChar
                    -> F.Ptr T.CUChar
                    -> T.CULLong
                    -> F.Ptr T.CUChar
                    -> F.Ptr T.CUChar
                    -> F.Ptr T.CUChar
                    -> IO T.CInt

foreign import ccall unsafe "sodium.h crypto_box_seal_open"
  c_crypto_box_seal_open :: CUString
                         -> CUString
                         -> T.CULLong
                         -> CUString
                         -> CUString
                         -> IO T.CInt

foreign import ccall unsafe "sodium.h crypto_box_seal"
  c_crypto_box_seal :: CUString
                    -> CUString
                    -> T.CULLong
                    -> CUString
                    -> IO T.CInt

foreign import ccall unsafe "sodium.h sodium_init"
  c_sodium_init :: IO T.CInt

foreign import ccall unsafe "sodium.h crypto_sign_seed_keypair"
  c_crypto_sign_seed_keypair :: GenKeyFun

foreign import ccall unsafe "sodium.h crypto_box_seed_keypair"
  c_crypto_box_seed_keypair :: GenKeyFun

data EKeyPair = EKeyPair
  { ePublic :: PEnc
  , eSecret :: SEnc
  } deriving Show

data SKeyPair = SKeyPair
  { sPublic :: PSign
  , sSecret :: SSign
  } deriving Show

type CUString = F.Ptr T.CUChar

newtype CSecretKey = CSecretKey CUString deriving Show

type GenKeyFun = CUString -> CUString -> CUString -> IO T.CInt

useAsCString :: B.ByteString -> (F.Ptr T.CUChar -> IO a) -> IO a
useAsCString (Bi.PS fp o l) action =
 F.allocaBytes (l+1) $ \buf ->
   F.withForeignPtr fp $ \p -> do
     Bi.memcpy buf (p `F.plusPtr` o) (fromIntegral l)
     F.pokeByteOff buf l (0::F.Word8)
     action (F.castPtr buf)

makeKeyPair
  :: GenKeyFun
  -> Int
  -> B.ByteString
  -> IO (Either Int (B.ByteString, B.ByteString))
makeKeyPair cfunc secretKeyLength seed =
  useAsCString seed $ \seedPtr -> F.allocaBytes 32 $ \publickey -> do
    F.allocaBytes secretKeyLength $ \secretkey -> do
      status <- cfunc publickey secretkey seedPtr
      case status of
        0 -> do
          bsPublickey <- packCString 32 $ F.castPtr publickey
          bsSecretkey <- packCString secretKeyLength $
            F.castPtr secretkey
          return $ Right $ (bsPublickey, bsSecretkey)
        errcode -> return $ Left $ fromIntegral errcode

eKeyPair :: B.ByteString -> Either Int EKeyPair
eKeyPair seed = unsafePerformIO $ do
  eitherps <- makeKeyPair c_crypto_box_seed_keypair crypto_box_SECRETKEYBYTES seed
  return $ case eitherps of
    Left err -> Left err
    Right (p, s) -> Right $ EKeyPair (PEnc p) (SEnc s)

sKeyPair :: B.ByteString -> Either Int SKeyPair
sKeyPair seed = unsafePerformIO $ do
  eitherps <- makeKeyPair c_crypto_sign_seed_keypair 64 seed
  return $ case eitherps of
    Left err -> Left err
    Right (p, s) -> Right $ SKeyPair (PSign p) (SSign s)

sodiumInit :: IO ()
sodiumInit = do
  resultCode <- c_sodium_init
  case resultCode of
    0 -> return ()
    1 -> return ()
    _ -> error "Could not initialise libsodium."

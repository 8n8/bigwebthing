{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydrogen (init_, hydroKxKeygen) where

import qualified Language.C.Inline as C
import qualified Foreign.C.Types as Ft
import qualified Foreign.Marshal.Array as Fa
import qualified Data.ByteString as B
import qualified System.IO.Unsafe as Unsafe

C.include "hydrogen.h"

init_ :: IO Ft.CInt
init_ =
    [C.exp| int{ hydro_init() } |]


data StaticKp
    = StaticKp Pk Sk


newtype Pk
    = Pk B.ByteString


newtype Sk
    = Sk B.ByteString


publicKeyBytes :: Int
publicKeyBytes =
    fromIntegral $
    Unsafe.unsafePerformIO
    [C.exp| int{hydro_kx_PUBLICKEYBYTES} |]


secretKeyBytes :: Int
secretKeyBytes =
    fromIntegral $
    Unsafe.unsafePerformIO
    [C.exp| int{hydro_kx_SECRETKEYBYTES} |]


hydroKxKeygen :: IO StaticKp
hydroKxKeygen =
    Fa.allocaArray secretKeyBytes $ \skPtr ->
    Fa.allocaArray publicKeyBytes $ \pkPtr -> do
        [C.block| void{
                hydro_kx_keypair static_kp;
                hydro_kx_keygen(&static_kp);
                for (int i = 0; i < hydro_kx_SECRETKEYBYTES; i++) {
                    $(char* skPtr)[i] = static_kp.sk[i];
                }
                for (int i = 0; i < hydro_kx_PUBLICKEYBYTES; i++) {
                    $(char* pkPtr)[i] = static_kp.pk[i];
                }
            }
        |]
        sk <- B.packCStringLen (skPtr, secretKeyBytes)
        pk <- B.packCStringLen (pkPtr, publicKeyBytes)
        return $ StaticKp (Pk pk) (Sk sk)

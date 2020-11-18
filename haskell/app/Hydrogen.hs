{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydrogen (init_) where

import qualified Language.C.Inline as C
import qualified Foreign.C.Types as Ft
import qualified Foreign.Marshal.Alloc as Fa
import qualified Data.ByteString as B

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


hydroKxKeygen :: IO StaticKp
hydroKxKeygen =
    Fa.alloca $ \skPtr ->
    Fa.alloca $ \pkPtr -> do
        errCode <- [C.block| int{
            hydro_kx_keypair static_kp;
            int err = hydro_kx_keygen(static_kp);
            skPtr = &static_kp->sk

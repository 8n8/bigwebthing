{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Hydrogen
    ( init_
    , hydroKxKeygen
    , hydroKxKk1
    , HydroHashState(..)
    , hydroKxKk2
    ) where

import qualified Language.C.Inline as C
import qualified Foreign.C.Types as Ft
import qualified Foreign.Marshal.Array as Fa
import qualified Foreign.Marshal.Alloc as Alloc
import qualified Data.ByteString as B
import qualified System.IO.Unsafe as Unsafe
import qualified GHC.Word as W
import qualified Foreign.Storable as Storable

C.context (C.baseCtx <> C.bsCtx)
C.include "hydrogen.h"

init_ :: IO Ft.CInt
init_ =
    [C.exp| int{ hydro_init() } |]


data StaticKp
    = StaticKp Pk Sk
    deriving (Show)


newtype Pk
    = Pk B.ByteString
    deriving (Show)


newtype Sk
    = Sk B.ByteString
    deriving (Show)


kx_PUBLICKEYBYTES :: Int
kx_PUBLICKEYBYTES =
    fromIntegral $
    Unsafe.unsafePerformIO
    [C.exp| int{hydro_kx_PUBLICKEYBYTES} |]


kx_SECRETKEYBYTES :: Int
kx_SECRETKEYBYTES =
    fromIntegral $
    Unsafe.unsafePerformIO
    [C.exp| int{hydro_kx_SECRETKEYBYTES} |]


kx_SESSIONKEYBYTES :: Int
kx_SESSIONKEYBYTES =
    fromIntegral $
    Unsafe.unsafePerformIO
    [C.exp| int{hydro_kx_SESSIONKEYBYTES} |]


kx_KK_PACKET2BYTES :: Int
kx_KK_PACKET2BYTES =
    fromIntegral $
    Unsafe.unsafePerformIO
    [C.exp| int{hydro_kx_KK_PACKET2BYTES} |]


packet1bytes :: Int
packet1bytes =
    fromIntegral $
    Unsafe.unsafePerformIO
    [C.exp| int{hydro_kx_KK_PACKET1BYTES} |]


hydroKxKeygen :: IO StaticKp
hydroKxKeygen =
    Fa.allocaArray kx_SECRETKEYBYTES $ \skPtr ->
    Fa.allocaArray kx_PUBLICKEYBYTES $ \pkPtr -> do
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
        sk <- B.packCStringLen (skPtr, kx_SECRETKEYBYTES)
        pk <- B.packCStringLen (pkPtr, kx_PUBLICKEYBYTES)
        return $ StaticKp (Pk pk) (Sk sk)


data HydroKxState
    = HydroKxState EphemeralKeyPair HydroHashState


data HydroHashState
    = HydroHashState
        { state :: B.ByteString
        , bufOff :: W.Word8
        , align :: B.ByteString
        }


newtype EphemeralKeyPair
    = EphemeralKeyPair HydroKxKeyPair


data HydroKxKeyPair
    = HydroKxKeyPair Pk Sk


newtype Packet1
    = Packet1 B.ByteString


-- The things that I want back from this function are:
--
-- 1. the packet to send to the initiator
-- 2. the session secret sending key
-- 3. the session secret receiving key
hydroKxKk2
    :: Pk
    -> StaticKp
    -> Packet1
    -> IO (Maybe (Packet2, SessionKp))
hydroKxKk2
    (Pk theirPk)
    (StaticKp (Pk myPk) (Sk mySk))
    (Packet1 packet1) =

    Fa.allocaArray kx_KK_PACKET2BYTES $ \packet2Ptr ->
    Fa.allocaArray kx_SESSIONKEYBYTES $ \txPtr ->
    Fa.allocaArray kx_SESSIONKEYBYTES $ \rxPtr -> do
        err <- [C.block| int{
                hydro_kx_session_keypair session_kp;
                uint8_t packet2[hydro_kx_KK_PACKET2BYTES];
                hydro_kx_keypair my_static;
                for (int i = 0; i < hydro_kx_PUBLICKEYBYTES; i++) {
                    $bs-ptr:myPk[i] = my_static.pk[i];
                }
                for (int i = 0; i < hydro_kx_SECRETKEYBYTES; i++) {
                    $bs-ptr:mySk[i] = my_static.sk[i];
                }
                int err = hydro_kx_kk_2(
                    &session_kp,
                    $(char* packet2Ptr),
                    $bs-ptr:packet1,
                    $bs-ptr:theirPk,
                    &my_static);
                for (int i = 0; i < hydro_kx_KK_PACKET2BYTES; i++) {
                    $(char* packet2Ptr)[i] = packet2[i];
                }
                for (int i = 0; i < hydro_kx_SESSIONKEYBYTES; i++) {
                    $(char* txPtr)[i] = session_kp.tx[i];
                }
                for (int i = 0; i < hydro_kx_SESSIONKEYBYTES; i++) {
                    $(char* rxPtr)[i] = session_kp.rx[i];
                }
                return err;
            }
        |]
        if err /= 0 then
            return Nothing
        else do
            packet2 <- B.packCStringLen (packet2Ptr, kx_KK_PACKET2BYTES)
            tx <- B.packCStringLen (txPtr, kx_SESSIONKEYBYTES)
            rx <- B.packCStringLen (rxPtr, kx_SESSIONKEYBYTES)
            return $ Just (Packet2 packet2, SessionKp (Tx tx) (Rx rx))


data SessionKp
    = SessionKp Tx Rx


newtype Tx
    = Tx B.ByteString


newtype Rx
    = Rx B.ByteString


newtype Packet2
    = Packet2 B.ByteString


-- So the things that I want back from this function are:
--
-- 1. the packet
-- 2. ephemeral public key
-- 3. ephemeral secret key
-- 4. the hash state
--     4.1 'state'
--     4.2 'bufOff
--     4.3 'align'
hydroKxKk1
    :: Pk
    -> StaticKp
    -> IO (Maybe (HydroKxState, Packet1))
hydroKxKk1 (Pk theirPk) (StaticKp (Pk myPk) (Sk mySk)) =
    Fa.allocaArray packet1bytes $ \packet1Ptr ->
    Fa.allocaArray kx_PUBLICKEYBYTES $ \ephPkPtr ->
    Fa.allocaArray kx_SECRETKEYBYTES $ \ephSkPtr ->
    Fa.allocaArray 12 $ \hsStatePtr ->
    Alloc.alloca $ \hsBufOffPtr ->
    Fa.allocaArray 3 $ \hsAlignPtr -> do
        err <- [C.block| int{
                hydro_kx_state st_client;
                hydro_kx_keypair my_static;
                for (int i = 0; i < hydro_kx_PUBLICKEYBYTES; i++) {
                    $bs-ptr:myPk[i] = my_static.pk[i];
                }
                for (int i = 0; i < hydro_kx_SECRETKEYBYTES; i++) {
                    $bs-ptr:mySk[i] = my_static.sk[i];
                }
                int err = hydro_kx_kk_1(
                    &st_client,
                    $(char* packet1Ptr),
                    $bs-ptr:theirPk,
                    &my_static);
                for (int i = 0; i < hydro_kx_PUBLICKEYBYTES; i++) {
                    $(char* ephPkPtr)[i] = st_client.eph_kp.pk[i];
                }
                for (int i = 0; i < hydro_kx_SECRETKEYBYTES; i++) {
                    $(char* ephSkPtr)[i] = st_client.eph_kp.sk[i];
                }
                for (int i = 0; i < 12; i++) {
                    $(char* hsStatePtr)[i] = st_client.h_st.state[i];
                }
                *$(uint8_t* hsBufOffPtr) = st_client.h_st.buf_off;
                for (int i = 0; i < 3; i++) {
                    $(char* hsAlignPtr)[i] = st_client.h_st.align[i];
                }
                return err;
            }
        |]
        if err /= 0 then
            return Nothing
        else do
            packet1 <- B.packCStringLen (packet1Ptr, packet1bytes)
            ephPk <- B.packCStringLen (ephPkPtr, kx_PUBLICKEYBYTES)
            ephSk <- B.packCStringLen (ephSkPtr, kx_SECRETKEYBYTES)
            hsState <- B.packCStringLen (hsStatePtr, 12)
            hsBufOff <- Storable.peek hsBufOffPtr
            hsAlign <- B.packCStringLen (hsAlignPtr, 3)
            return $ Just
                ( HydroKxState
                    (EphemeralKeyPair $
                     HydroKxKeyPair (Pk ephPk) (Sk ephSk))
                    (HydroHashState hsState hsBufOff hsAlign)
                , Packet1 packet1
                )

{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Crypto.Noise as N
import Crypto.Noise.Hash.BLAKE2s (BLAKE2s)
import Crypto.Noise.DH.Curve25519 (Curve25519)
import Crypto.Noise.Cipher.ChaChaPoly1305 (ChaChaPoly1305)
import qualified Data.ByteString as B
import qualified Crypto.Noise.DH as Dh
import Crypto.Noise.HandshakePatterns (noiseKK)
import Crypto.Random (getSystemDRG, randomBytesGenerate)


main :: IO ()
main =
    do
    tests <- fmap mconcat $ fmap (fmap encodeTest) $ sequence $ take 1000 $ repeat genTest
    B.writeFile "tests" tests


encodeTest :: Test -> B.ByteString
encodeTest (Test a b c d e f g h i j) =
    a <> b <> c <> d <> e <> f <> g <> h <> i <> j


data Test
    -- 32 + 32 + 32 + 32 + 48 + 48 + 10 + 26 + 10 + 26 = 296
    = Test
    -- 32 bytes
    { clientStaticT :: B.ByteString
    -- 32 bytes
    , clientEphemeralT :: B.ByteString
    -- 32 bytes
    , serverStaticT :: B.ByteString
    -- 32 bytes
    , serverEphemeralT :: B.ByteString
    -- 32 + 16 = 48
    , kk1T :: B.ByteString
    -- 32 + 16 + 16 = 64
    , kk2T :: B.ByteString
    -- 10
    , plain1T :: B.ByteString
    -- 10 + 16 = 26
    , cipher1T :: B.ByteString
    -- 10
    , plain2T :: B.ByteString
    -- 10 + 16 = 26
    , cipher2T :: B.ByteString
    }


randomBytes :: IO B.ByteString
randomBytes =
    do
    drg <- getSystemDRG
    let (bs, _) = randomBytesGenerate 10 drg
    return bs


genTest :: IO Test
genTest =
    do
    clientStatic <- genKey
    clientEphemeral <- genKey
    serverStatic <- genKey
    serverEphemeral <- genKey
    plain1 <- randomBytes
    plain2 <- randomBytes
    let
        kk1 =
            makeKk1
                clientStatic
                clientEphemeral
                (makePublic serverStatic)
        (kk2, serverNoise1) =
            makeKk2
                serverStatic
                serverEphemeral
                (makePublic clientStatic)
                kk1
        clientNoise1 =
            readKk2
                clientStatic
                clientEphemeral
                (makePublic serverStatic)
                kk2
        (_, cipher1) = encrypt clientNoise1 plain1
        (serverNoise2, _) = decrypt serverNoise1 cipher1
        (_, cipher2) = encrypt serverNoise2 plain2
    return $
        Test
        { clientStaticT = clientStatic
        , clientEphemeralT = clientEphemeral
        , serverStaticT = serverStatic
        , serverEphemeralT = serverEphemeral
        , kk1T = kk1
        , kk2T = kk2
        , plain1T = plain1
        , cipher1T = cipher1
        , plain2T = plain2
        , cipher2T = cipher2
        }


encrypt :: NoiseState -> B.ByteString -> (NoiseState, B.ByteString)
encrypt noise plaintext =
    case N.writeMessage (N.convert plaintext) noise of
    N.NoiseResultMessage ciphertext noise' ->
        (noise', N.convert ciphertext)

    N.NoiseResultNeedPSK _ ->
        error "encrypt failed: NoiseResultNeedPSK"

    N.NoiseResultException _ ->
        error "encrypt failed: NoiseResultException"


decrypt :: NoiseState -> B.ByteString -> (NoiseState, B.ByteString)
decrypt noise ciphertext =
    case N.readMessage (N.convert ciphertext) noise of
    N.NoiseResultMessage plaintext noise' ->
        (noise', N.convert plaintext)

    N.NoiseResultNeedPSK _ ->
        error "decrypt failed: NoiseResultNeedPSK"

    N.NoiseResultException _ ->
        error "decrypt failed: NoiseResultException"
        

genKey :: IO B.ByteString
genKey =
    do
    (secret, _) <- Dh.dhGenKey :: IO (Dh.KeyPair Curve25519)
    return $ N.convert $ Dh.dhSecToBytes secret


makePublic :: B.ByteString -> B.ByteString
makePublic secret =
    let
    kp :: Maybe (Dh.KeyPair Curve25519)
    kp = Dh.dhBytesToPair (N.convert secret)
    in
    case kp of
    Nothing ->
        error "couldn't make public key"

    Just (_, key) ->
        N.convert $ Dh.dhPubToBytes key


type NoiseState
    = N.NoiseState ChaChaPoly1305 Curve25519 BLAKE2s

readKk2
    :: B.ByteString
    -> B.ByteString
    -> B.ByteString
    -> B.ByteString
    -> NoiseState
readKk2 mySecretStatic mySecretEphemeral theirPublic kk2 =
    let
    localStaticKey =
        case Dh.dhBytesToPair (N.convert mySecretStatic) of
        Nothing ->
            error "couldn't make local static key"

        Just key ->
            key

    remoteStaticKey =
        case Dh.dhBytesToPub (N.convert theirPublic) of
        Nothing ->
            error "couldn't make remote static key"

        Just key ->
            key

    localEphemeralKey =
        case Dh.dhBytesToPair (N.convert mySecretEphemeral) of
        Nothing ->
            error "couldn't make local ephemeral key"

        Just key ->
            key

    dho :: N.HandshakeOpts Curve25519
    dho = N.defaultHandshakeOpts N.InitiatorRole ""

    iho =
        N.setLocalStatic (Just localStaticKey)
        . N.setLocalEphemeral (Just localEphemeralKey)
        . N.setRemoteStatic (Just remoteStaticKey)
        $ dho

    ins :: N.NoiseState ChaChaPoly1305 Curve25519 BLAKE2s
    ins = N.noiseState iho noiseKK
    in
    case N.writeMessage "" ins of
    N.NoiseResultMessage _ noise ->
        case N.readMessage (N.convert kk2) noise of
        N.NoiseResultMessage "" noise' ->
            noise'

        N.NoiseResultMessage _ _ ->
            error "readKK2 failed: NoiseResultMessage"

        N.NoiseResultNeedPSK _ ->
            error "readKK2 failed: NoiseResultNeedPSK"

        N.NoiseResultException _ ->
            error "readKK2 failed: NoiseResultException"

    N.NoiseResultNeedPSK _ ->
        error "readKK2 failed: NoiseResultNeedPSK"

    N.NoiseResultException _ ->
        error "readKK2 failed: NoiseResultException"


makeKk2
    :: B.ByteString
    -> B.ByteString
    -> B.ByteString
    -> B.ByteString
    -> (B.ByteString, NoiseState)
makeKk2 mySecretStatic mySecretEphemeral theirPublic kk1  =
    let
    remoteStaticKey =
        case Dh.dhBytesToPub (N.convert theirPublic) of
        Nothing ->
            error "couldn't make remote static key"

        Just key ->
            key

    localEphemeralKey =
        case Dh.dhBytesToPair (N.convert mySecretEphemeral) of
        Nothing ->
            error "couldn't make local ephemeral key"

        Just key ->
            key

    localStaticKey =
        case Dh.dhBytesToPair (N.convert mySecretStatic) of
        Nothing ->
            error "couldn't make local static key"

        Just key ->
            key

    dho :: N.HandshakeOpts Curve25519
    dho = N.defaultHandshakeOpts N.ResponderRole ""
    rho =
        N.setLocalStatic (Just localStaticKey)
        . N.setLocalEphemeral (Just localEphemeralKey)
        . N.setRemoteStatic (Just remoteStaticKey)
        $ dho
    rns = N.noiseState rho noiseKK :: NoiseState
    in
    case N.readMessage (N.convert kk1) rns of
    N.NoiseResultMessage "" rns' ->
        case N.writeMessage "" rns' of
        N.NoiseResultMessage kk2 rns'' ->
            (N.convert kk2, rns'')

        N.NoiseResultNeedPSK _ ->
            error "make KK2 write failed: NoiseResultNeedPSK"

        N.NoiseResultException _ ->
            error "make KK2 write failed: NoiseResultException"

    N.NoiseResultMessage _ _ ->
        error "make KK2 read failed: NoiseResultMessage not empty"

    N.NoiseResultNeedPSK _ ->
        error "make KK2 read failed: NoiseResultNeedPSK"

    N.NoiseResultException _ ->
        error "make KK2 read failed: NoiseResultException"

makeKk1
    :: B.ByteString
    -> B.ByteString
    -> B.ByteString
    -> B.ByteString
makeKk1 mySecretStatic mySecretEphemeral theirPublic =
    let
    localStaticKey =
        case Dh.dhBytesToPair (N.convert mySecretStatic) of
        Nothing ->
            error "couldn't make local static key"

        Just key ->
            key

    remoteStaticKey =
        case Dh.dhBytesToPub (N.convert theirPublic) of
        Nothing ->
            error "couldn't make remote static key"

        Just key ->
            key

    localEphemeralKey =
        case Dh.dhBytesToPair (N.convert mySecretEphemeral) of
        Nothing ->
            error "couldn't make local ephemeral key"

        Just key ->
            key

    dho :: N.HandshakeOpts Curve25519
    dho = N.defaultHandshakeOpts N.InitiatorRole ""

    iho =
        N.setLocalStatic (Just localStaticKey)
        . N.setLocalEphemeral (Just localEphemeralKey)
        . N.setRemoteStatic (Just remoteStaticKey)
        $ dho

    ins :: N.NoiseState ChaChaPoly1305 Curve25519 BLAKE2s
    ins = N.noiseState iho noiseKK
    in
    case N.writeMessage "" ins of
    N.NoiseResultMessage ciphertext _ ->
        N.convert ciphertext

    N.NoiseResultNeedPSK _ ->
        error "Bad make KK1 NoiseResultNeedPSK"

    N.NoiseResultException _ ->
        error "Bad make KK1 NoiseResultException"

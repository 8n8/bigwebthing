{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Crypto.Noise
import Crypto.Noise.Cipher.ChaChaPoly1305
import Crypto.Noise.DH
import Crypto.Noise.DH.Curve25519
import Crypto.Noise.Hash.BLAKE2s
import Crypto.Noise.HandshakePatterns (noiseKK)


-- So I want to:
-- 1. make my static key pair
-- 2. make their static key pair
-- 3. send them a KK1
-- 4. get a KK2 in response
-- 5. encrypt a payload for them
-- 6. try to decrypt the payload with my NoiseState
main :: IO ()
main =
    do
    myStatic <- dhGenKey :: IO (KeyPair Curve25519)
    theirStatic <- dhGenKey :: IO (KeyPair Curve25519)

    myEphemeral <- dhGenKey :: IO (KeyPair Curve25519)
    theirEphemeral <- dhGenKey :: IO (KeyPair Curve25519)

    (let
        me = makeMyInitNoise myStatic myEphemeral theirStatic
        them = makeTheirInitNoise theirStatic theirEphemeral myStatic
     in
        case writeMessage "" me of
        NoiseResultMessage kk1 me' ->
            case readMessage kk1 them of
            NoiseResultMessage _ them' ->
                case writeMessage "" them' of
                NoiseResultMessage kk2 _ ->
                    case readMessage kk2 me' of
                    NoiseResultMessage _ me'' ->
                        if not $ handshakeComplete me'' then
                            print ("handshake not finished" :: String)
                        else
                        case writeMessage plaintext me'' of
                        NoiseResultMessage ciphertext _ ->
                            case readMessage ciphertext me'' of
                            NoiseResultMessage plaintext' _ ->
                                print $ plaintext' == plaintext

                            _ ->
                                print ("bad final readMessage" :: String)

                        _ ->
                            print ("bad writeMessage for me''" :: String)

                    _ ->
                        print ("bad readMessage me'" :: String)

                _ ->
                    print ("bad writeMessage them'" :: String)

            _ ->
                print ("bad readMessage them" :: String)

        _ ->
            print ("bad writeMessage me" :: String))
                    


makeMyInitNoise :: KeyPair Curve25519 -> KeyPair Curve25519 -> KeyPair Curve25519 -> NoiseState ChaChaPoly1305 Curve25519 BLAKE2s
makeMyInitNoise myStatic myEphemeral (_, theirStatic) =
    let
    dho = defaultHandshakeOpts InitiatorRole "" :: HandshakeOpts Curve25519
    iho =
        setLocalStatic (Just myStatic)
        . setLocalEphemeral (Just myEphemeral)
        . setRemoteStatic (Just theirStatic)
        $ dho
    in
    noiseState iho noiseKK :: NoiseState ChaChaPoly1305 Curve25519 BLAKE2s


makeTheirInitNoise :: KeyPair Curve25519 -> KeyPair Curve25519 -> KeyPair Curve25519 -> NoiseState ChaChaPoly1305 Curve25519 BLAKE2s
makeTheirInitNoise theirStatic theirEphemeral (_, myStatic) =
    let
    dho = defaultHandshakeOpts ResponderRole "" :: HandshakeOpts Curve25519
    rho =
        setLocalStatic (Just theirStatic)
        . setLocalEphemeral (Just theirEphemeral)
        . setRemoteStatic (Just myStatic)
        $ dho
    in
    noiseState rho noiseKK :: NoiseState ChaChaPoly1305 Curve25519 BLAKE2s


plaintext :: ScrubbedBytes
plaintext = "hello"

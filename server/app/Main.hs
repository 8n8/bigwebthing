{-# LANGUAGE DuplicateRecordFields #-}

module Main (main) where

import Lib
import Crypto.Hash (Digest)
import Crypto.Hash.Algorithms (Blake2s_256)
import qualified Data.ByteString as B
import qualified Data.Word as W
import Crypto.Noise.DH.Curve25519 (Curve25519)
import qualified Crypto.Noise.DH as Dh
import qualified Crypto.PubKey.Ed25519 as Ed


data GlobalData
    = GlobalData
        [(ShortId, PublicKey, Salt)]
        (Ed.Signature, PosixSeconds)
        [BadUserKey]
        (Dh.SecretKey Curve25519)


newtype BadUserKey
    = BadUserKey PublicKey


newtype Salt
    = Salt B.ByteString


newtype ShortId
    = ShortId W.Word64


data UserData
    = UserData
        [(From, SentTime, Message)]
        [(From, SentTime, ReadTime, MessageSize)]
        [(WholeGbp, PosixSeconds)]


newtype Message
    = Message B.ByteString


newtype MessageSize
    = MessageSize W.Word16


newtype From
    = From PublicKey


newtype To
    = To PublicKey


newtype ReadTime
    = ReadTime PosixSeconds


newtype SentTime
    = SentTime PosixSeconds


newtype PosixSeconds
    = PosixSeconds W.Word32


newtype WholeGbp
    = WholeGbp W.Word32


type Hash
    = Digest Blake2s_256


type PublicKey
    = Dh.PublicKey Curve25519


main :: IO ()
main =
    putStrLn "hi"

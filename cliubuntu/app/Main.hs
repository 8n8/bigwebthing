module Main (main) where

import qualified Crypto.Noise.DH as Dh
import Crypto.Hash (Digest)
import Crypto.Hash.Algorithms (Blake2s_256)
import qualified Data.Text as T
import Crypto.Noise.DH.Curve25519 (Curve25519)
import qualified Data.Word as W
import qualified Data.ByteString as B
import System.Environment (getArgs)
import qualified Data.Text.IO as Tio
import Control.Exception (try)


data Data
    = Data
        [(FileInfo, CreatedTime)]
        [(SendId, To, FileInfo, PosixSeconds)]
        [(SendId, Error)]
        [(From, FileInfo, PosixSeconds)]
        [(AppInfo, FileInfo)]
        [AppInfo]
        [(PublicKey, Nickname)]
        [(Kk1, From, SessionId)]
        [(Kk2, From, SessionId)]
        [(SessionId, EphemeralSecret, PublicKey)]
        StaticSecret


newtype Kk1
    = Kk1 B.ByteString


newtype Kk2
    = Kk2 B.ByteString


newtype SessionId
    = SessionId B.ByteString


data FileInfo
    = FileInfo (Digest Blake2s_256) Description FileSize


data Nickname
    = Nickname1 NickChar
    | Nickname2 NickChar NickChar
    | Nickname3 NickChar NickChar NickChar
    | Nickname4 NickChar NickChar NickChar NickChar
    | Nickname5 NickChar NickChar NickChar NickChar NickChar
    | Nickname6 NickChar NickChar NickChar NickChar NickChar NickChar
    | Nickname7
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
    | Nickname8
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar
    | Nickname9
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar
    | Nickname10
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar
    | Nickname11
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar
    | Nicname12
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar NickChar
    | Nickname13
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar NickChar NickChar
    | Nickname14
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
    | Nickname15
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar
    | Nickname16
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar
    | Nickname17
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar
    | Nickname18
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar
    | Nickname19
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar NickChar
    | Nickname20
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar NickChar NickChar NickChar
        NickChar NickChar NickChar NickChar NickChar NickChar


data NickChar
    = Nca
    | Ncb
    | Ncc
    | Ncd
    | Nce
    | Ncf
    | Ncg
    | Nch
    | Nci
    | Ncj
    | Nck
    | Ncl
    | Ncm
    | Ncn
    | Nco
    | Ncp
    | Ncq
    | Ncr
    | Ncs
    | Nct
    | Ncu
    | Ncv
    | Ncw
    | Ncx
    | Ncy
    | Ncz
    | Nc2
    | Nc3
    | Nc4
    | Nc5
    | Nc6
    | Nc7
    | Nc8
    | Nc9


newtype FileSize
    = FileSize W.Word64


newtype SendId
    = SendId Int


newtype From
    = From PublicKey


newtype To
    = To PublicKey


newtype PosixSeconds
    = PosixSeconds W.Word32


newtype Error
    = Error T.Text


newtype CreatedTime
    = CreatedTime PosixSeconds


newtype Description
    = Description T.Text


newtype EphemeralSecret
    = EphemeralSecret SecretKey


newtype StaticSecret
    = StaticSecret SecretKey


type PublicKey
    = Dh.PublicKey Curve25519


type SecretKey
    = Dh.SecretKey Curve25519


newtype FileHash
    = FileHash (Digest Blake2s_256)


newtype AppInfo
    = AppInfo FileInfo


data Fingerprint
    = Fingerprint
        W.Word8 W.Word8 W.Word8 W.Word8 W.Word8
        W.Word8 W.Word8 W.Word8 W.Word8 W.Word8


newtype UserId
    = UserId W.Word64


data Args
    = AddContact UserId Fingerprint Nickname 


main :: IO ()
main =
    putStrLn "hi"

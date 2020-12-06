module Main (main) where

import qualified Hedgehog as H
import qualified Test.Tasty as Tasty
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty.Hedgehog as Th
import qualified GHC.IO.Exception as Ge
import qualified Control.Exception as E
import qualified Network.Socket as NS
import qualified Network.Simple.TCP as Tcp
import qualified Update as U
import qualified Crypto.Random as CryptoRand
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Data.Set as Set
import qualified Data.Map as Map
import System.IO.Unsafe (unsafePerformIO)


main :: IO ()
main =
    Tasty.defaultMain properties


properties :: Tasty.TestTree
properties =
    let
    t = Th.testProperty
    in
    Tasty.testGroup ""
    [ t "tcpExceptIn" tcpExceptIn
    , t "tcpNothingIn" tcpNothingIn
    ]


dummyTcpSocket :: Tcp.Socket
dummyTcpSocket =
    unsafePerformIO $ NS.mkSocket 0


dummySocketAddress :: Tcp.SockAddr
dummySocketAddress =
    NS.SockAddrUnix ""


dummyException :: E.IOException
dummyException =
    Ge.IOError Nothing Ge.AlreadyExists "" "" Nothing Nothing


tcpNothingIn :: H.Property
tcpNothingIn =
    let
    msg = U.TcpMsgInM dummySocketAddress $ Right Nothing
    in
    H.property $
    do
    ready <- H.forAll readyG
    let (out, model') = U.update (U.ReadyS ready) msg
    isCloseSocket out
    H.assert $ isReady model'


tcpExceptIn :: H.Property
tcpExceptIn =
    let
    msg = U.TcpMsgInM dummySocketAddress $ Left dummyException
    in
    H.property $ do
    ready <- H.forAll readyG
    let (out, model') = U.update (U.ReadyS ready) msg
    isCloseSocket out
    H.assert $ isReady model'


isCloseSocket :: U.Output -> H.PropertyT IO ()
isCloseSocket o =
    case o of
    U.CloseSocketO _ ->
        return ()

    U.BatchO bs ->
        mapM_ isCloseSocket bs

    _ ->
        H.failure


readyG :: H.Gen U.Ready
readyG = do
    conns <- tcpConnsG
    let randomGen = CryptoRand.drgNewTest (0, 0, 0, 0, 0)
    accessList <- accessListG
    return $ U.Ready conns randomGen accessList


tcpConnsG :: H.Gen (Map.Map Tcp.SockAddr U.TcpConn)
tcpConnsG =
    do
    asList <- Gen.list (Range.linear 1 10) tcpConnG
    return $ Map.fromList asList


tcpConnG :: H.Gen (Tcp.SockAddr, U.TcpConn)
tcpConnG =
    do
    auth <- authStatusG
    tcpGetting <- Gen.element [U.LengthG, U.BodyG]
    return
        ( dummySocketAddress
        , U.TcpConn dummyTcpSocket auth tcpGetting
        )


authStatusG :: H.Gen U.AuthStatus
authStatusG =
    Gen.choice
    [ fmap (U.Authenticated . U.Sender) publicKeyG
    , fmap U.Untrusted authCodeG
    ]


authCodeG :: H.Gen U.AuthCode
authCodeG =
    do
    bytes <- Gen.bytes $ Range.singleton U.authLength
    return $ U.AuthCode bytes


accessListG :: H.Gen (Set.Set U.PublicKey)
accessListG = do
    keys <- fmap (fmap U.PublicKey) $
            Gen.list (Range.linear 0 10) publicKeyG
    return $ Set.fromList keys


publicKeyG :: H.Gen Ed.PublicKey
publicKeyG = do
    raw <- Gen.bytes (Range.singleton Ed.publicKeySize)
    case Ed.publicKey raw of
        CryptoPassed key ->
            return key

        CryptoFailed err ->
            fail $ show err


isReady :: U.State -> Bool
isReady s =
    case s of
    U.InitS _ ->
        False

    U.ReadyS _ ->
        True

    U.FailedS ->
        False

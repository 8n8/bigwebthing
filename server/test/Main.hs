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
import qualified Data.ByteString as B


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
    , t "tcpTooLongIn" $ tcpBadRightIn 200 300
    , t "tcpTooShortIn" $ tcpBadRightIn 0 1
    , t "tcpGoodBadSignedAuth" tcpGoodBadSignedAuth
    , t "tcpSendMessage" tcpSendMessage
    , t "tcpGetMessage" tcpGetMessage
    , t "newTcpConn" newTcpConn
    , t "deadTcp" deadTcp
    ]


deadTcp :: H.Property
deadTcp =
    H.property $ do
    let msg = U.DeadTcpM dummySocketAddress
    ready <- H.forAll readyG
    let (out, _) = U.update (U.ReadyS ready) msg
    out H.=== U.CloseSocketO dummyTcpSocket


newTcpConn :: H.Property
newTcpConn =
    H.property $ do
    let msg = U.NewTcpConnM dummyTcpSocket dummySocketAddress
    ready <- H.forAll readyG
    let (out, _) = U.update (U.ReadyS ready) msg
    H.assert $ isTcpRecv out


isTcpRecv :: U.Output -> Bool
isTcpRecv o =
    case o of
    U.TcpRecvO _ _ _ ->
        True

    U.BatchO bs ->
        any isTcpRecv bs

    _ ->
        False


tcpGetMessage :: H.Property
tcpGetMessage =
    H.property $ do
    let msg = U.TcpMsgInM dummySocketAddress $
            Right $ Just $ B.singleton 2
    ready <- H.forAll readyGetBodyG
    let (out, _) = U.update (U.ReadyS ready) msg
    case out of
        U.GetMessageFromDbO _ ->
            return ()

        _ -> do
            H.failure


tcpSendMessage :: H.Property
tcpSendMessage =
    H.property $ do
    publicKey <- H.forAll $ Gen.bytes $
            Range.singleton Ed.publicKeySize
    inboxMessage <- H.forAll $ Gen.bytes $ Range.linear 1 100
    let raw = B.singleton 1 <> publicKey <> inboxMessage
    let msg = U.TcpMsgInM dummySocketAddress $ Right $ Just raw
    ready <- H.forAll readyG
    let (out, _) = U.update (U.ReadyS ready) msg
    H.assert $ isCloseSocket out


tcpGoodBadSignedAuth :: H.Property
tcpGoodBadSignedAuth =
    H.property $ do
    publicKey <- H.forAll $
            Gen.bytes (Range.singleton Ed.publicKeySize)
    signedAuth <- H.forAll $ Gen.bytes (Range.singleton 64)
    let raw = B.singleton 0 <> publicKey <> signedAuth
    let msg = U.TcpMsgInM dummySocketAddress $ Right $ Just raw
    ready <- H.forAll readyG
    let (out, _) = U.update (U.ReadyS ready) msg
    out H.=== U.CloseSocketO dummyTcpSocket


dummyTcpSocket :: Tcp.Socket
dummyTcpSocket =
    unsafePerformIO $ NS.mkSocket 0


dummySocketAddress :: Tcp.SockAddr
dummySocketAddress =
    NS.SockAddrUnix ""


dummyException :: E.IOException
dummyException =
    Ge.IOError Nothing Ge.AlreadyExists "" "" Nothing Nothing


tcpBadRightIn :: Int -> Int -> H.Property
tcpBadRightIn start stop =
    H.property $
    do
    bytes <- H.forAll $ Gen.bytes $ Range.linear start stop
    let msg = U.TcpMsgInM dummySocketAddress $ Right $ Just bytes
    ready <- H.forAll readyG
    let (out, model') = U.update (U.ReadyS ready) msg
    H.assert $ isCloseSocket out
    H.assert $ socketDeleted model' dummySocketAddress


tcpNothingIn :: H.Property
tcpNothingIn =
    let
    msg = U.TcpMsgInM dummySocketAddress $ Right Nothing
    in
    H.property $
    do
    ready <- H.forAll readyG
    let (out, model') = U.update (U.ReadyS ready) msg
    H.assert $ isCloseSocket out
    H.assert $ socketDeleted model' dummySocketAddress


socketDeleted :: U.State -> Tcp.SockAddr -> Bool
socketDeleted state address =
    case state of
    U.ReadyS ready ->
        Map.lookup address (U.tcpConns ready) == Nothing

    _ ->
        False
    

tcpExceptIn :: H.Property
tcpExceptIn =
    let
    msg = U.TcpMsgInM dummySocketAddress $ Left dummyException
    in
    H.property $ do
    ready <- H.forAll readyG
    let (out, model') = U.update (U.ReadyS ready) msg
    H.assert $ isCloseSocket out
    H.assert $ socketDeleted model' dummySocketAddress


isCloseSocket :: U.Output -> Bool
isCloseSocket o =
    case o of
    U.CloseSocketO _ ->
        True

    U.BatchO bs ->
        any isCloseSocket bs

    _ ->
        False


readyGetBodyG :: H.Gen U.Ready
readyGetBodyG = do
    conns <- tcpConnsGetBodyG
    let randomGen = CryptoRand.drgNewTest (0, 0, 0, 0, 0)
    accessList <- accessListG
    return $ U.Ready conns randomGen accessList


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


tcpConnsGetBodyG :: H.Gen (Map.Map Tcp.SockAddr U.TcpConn)
tcpConnsGetBodyG =
    do
    asList <- Gen.list (Range.linear 1 10) tcpConnGetBodyG
    return $ Map.fromList asList


tcpConnGetBodyG :: H.Gen (Tcp.SockAddr, U.TcpConn)
tcpConnGetBodyG =
    do
    publicKey <- publicKeyG
    let auth = U.Authenticated $ U.Sender publicKey
    return
        ( dummySocketAddress
        , U.TcpConn dummyTcpSocket auth U.BodyG
        )


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

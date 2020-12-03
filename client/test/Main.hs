{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as Th
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog as H
import qualified Update as L
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Control.Exception as E
import qualified GHC.IO.Exception as Ge
import qualified Network.Simple.TCP as Tcp
import qualified Network.Socket as NS
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Text as T


dummyTcpSocket :: Tcp.Socket
dummyTcpSocket =
    unsafePerformIO $ NS.mkSocket 0


dummySocketAddress :: Tcp.SockAddr
dummySocketAddress =
    NS.SockAddrUnix ""


dummyTcp :: (Tcp.Socket, Tcp.SockAddr)
dummyTcp =
    (dummyTcpSocket, dummySocketAddress)


main :: IO ()
main =
    Tasty.defaultMain properties


properties :: Tasty.TestTree
properties =
    Tasty.testGroup ""
    [ Th.testProperty "reverse" propReverse
    , Th.testProperty "updateStartM" updateStartM
    , Th.testProperty "tcpSocketNotClosed" tcpSocketClosed
    , Th.testProperty "finishedSonBad" tcpFinishedOnBad
    , Th.testProperty "dontMessWithSecretKey" dontMessWithSecretKey
    , Th.testProperty "goodTcpConnM" goodTcpConnM
    , Th.testProperty "goodTcpSend" goodTcpSend
    , Th.testProperty "tooLongMessage" tooLongMessage
    , Th.testProperty "emptyMessage" emptyMessage
    , Th.testProperty "noArgs" noArgs
    , Th.testProperty "badArgs" badArgs
    , Th.testProperty "argGet" argGet
    , Th.testProperty "argPrint" argPrint
    , Th.testProperty "goodArgSend" goodArgSend
    , Th.testProperty "sendNoRecipient" sendNoRecipient
    , Th.testProperty "sendBadRecipient" sendBadRecipient
    ]


sendBadRecipient :: H.Property
sendBadRecipient =
    H.property $ do
        ready <- H.forAll readyG
        badRecipient <- H.forAll $ Gen.choice
                [ Gen.string (Range.linear 44 100) Gen.unicodeAll
                , Gen.string (Range.linear 0 42) Gen.unicodeAll
                ]
        let (out, model) = L.update (L.ReadyS ready) $
                L.ArgsM ["send", badRecipient]
        model H.=== L.FinishedS
        H.assert $ isPrintO out


sendNoRecipient :: H.Property
sendNoRecipient =
    H.property $ do
        ready <- H.forAll readyG
        let (out, model) = L.update (L.ReadyS ready) $ L.ArgsM ["send"]
        model H.=== L.FinishedS
        H.assert $ isPrintO out


goodArgSend :: H.Property
goodArgSend =
    H.property $ do
        ready <- H.forAll readyG
        recipient <- H.forAll $
                Gen.string (Range.singleton 43) Gen.alphaNum
        let (out, model) = L.update (L.ReadyS ready) $
                L.ArgsM ["send", recipient]
        model H./== L.FinishedS
        H.assert $ not $ isPrintO out
        H.assert $ isGetStdIn out


isGetStdIn :: L.Output -> Bool
isGetStdIn out =
    case out of
    L.ReadMessageFromStdInO ->
        True

    L.BatchO bs ->
        any isGetStdIn bs

    _ ->
        False


argPrint :: H.Property
argPrint =
    H.property $ do
        ready <- H.forAll readyG
        arg <- H.forAll $ Gen.element ["help", "myid"]
        let (out, model) = L.update (L.ReadyS ready) $ L.ArgsM [arg]
        model H.=== L.FinishedS
        case out of
            L.PrintO _ ->
                return ()

            _ ->
                H.failure


argGet :: H.Property
argGet =
    H.property $ do
        ready <- H.forAll readyG
        let (out, model) = L.update (L.ReadyS ready) $ L.ArgsM ["get"]
        model H./== L.FinishedS
        H.assert $ not $ isPrintO out


badArgs :: H.Property
badArgs =
    H.property $ do
        ready <- H.forAll readyG
        args <- H.forAll $ Gen.list
            (Range.linear 4 10)
            (Gen.string (Range.linear 1 10) Gen.unicode)
        let (out, model') = L.update (L.ReadyS ready) $ L.ArgsM args
        model' H.=== L.FinishedS
        H.assert $ isPrintO out


noArgs :: H.Property
noArgs =
    H.property $ do
        ready <- H.forAll readyG
        let (out, model') = L.update (L.ReadyS ready) $ L.ArgsM []
        model' H.=== L.FinishedS
        H.assert $ isPrintO out


dontMessWithSecretKey :: H.Property
dontMessWithSecretKey =
    H.property $ do
        oldReady <- H.forAll readyG
        msg <- H.forAll msgG
        let (_, model') = L.update (L.ReadyS oldReady) msg
        case model' of
            L.ReadyS ready ->
                L.secretKey ready H.=== L.secretKey oldReady

            _ ->
                return ()


emptyMessage :: H.Property
emptyMessage =
    H.property $ do
        model <- H.forAll $ fmap L.ReadyS readyStdInG
        let (out, model') = L.update model $ L.StdInM ""
        model' H.=== L.FinishedS
        H.assert $ isPrintO out


tooLongMessage :: H.Property
tooLongMessage =
    H.property $ do
        model <- H.forAll $ fmap L.ReadyS readyStdInG
        msg <- H.forAll $ Gen.bytes (Range.constant 101 1000)
        let (out, model') = L.update model $ L.StdInM msg
        model' H.=== L.FinishedS
        H.assert $ isPrintO out



readyStdInG :: H.Gen L.Ready
readyStdInG = do
    secretKey <- secretKeyG
    authStatus <- authStatusG
    stdIn <- fmap Just publicKeyG
    return $ L.Ready secretKey authStatus stdIn


isPrintO :: L.Output -> Bool
isPrintO out =
    case out of
    L.PrintO _ ->
        True

    L.BatchO bs ->
        any isPrintO bs

    _ ->
        False
        

goodTcpSend :: H.Property
goodTcpSend =
    H.property $ do
        model <- H.forAll $ fmap L.ReadyS readyG
        let result = L.update model $ L.TcpSendResultM $ Right ()
        result H.=== (L.DoNothingO, model)


goodTcpConnM :: H.Property
goodTcpConnM =
    H.property $ do
        model <- H.forAll $ fmap L.ReadyS readyG
        let (out, model') =
                L.update model $ L.TcpConnM $ Right dummyTcp
        case model' of
            L.ReadyS _ ->
                return ()

            _ ->
                H.failure

        H.assert $ isTcpListen out


isTcpListen :: L.Output -> Bool
isTcpListen o =
    case o of
        L.TcpListenO _ _ ->
            True

        L.BatchO bs ->
            any isTcpListen bs

        _ ->
            False


tcpFinishedOnBad :: H.Property
tcpFinishedOnBad =
    H.property $ do
        msg <- H.forAll badMsgG
        model <- H.forAll stateG
        let (_, model') = L.update model msg
        model' H.=== L.FinishedS


dummyException :: E.IOException
dummyException =
    Ge.IOError Nothing Ge.AlreadyExists "" "" Nothing Nothing


tcpSocketClosed :: H.Property
tcpSocketClosed =
    H.property $ do
        msg <- H.forAll badServerG
        model <- H.forAll $ fmap L.ReadyS readySocketG
        let (output, _) = L.update model msg
        H.assert $ outputContainsTcpClose output


badServerG :: H.Gen L.Msg
badServerG =
    Gen.choice
    [ return $ L.FromServerM $ Right Nothing
    , return $ L.FromServerM $ Left dummyException
    ]


msgG :: H.Gen L.Msg
msgG =
    Gen.choice
    [ return L.StartM
    , return $ L.TcpConnM $ Left dummyException
    , return $ L.TcpConnM $ Right dummyTcp
    , return $ L.TcpSendResultM $ Left dummyException
    , return $ L.TcpSendResultM $ Right ()
    , do
        bytes <- Gen.bytes (Range.constant 0 500)
        return $ L.StdInM bytes
    , do
        args <- Gen.list
            (Range.linear 0 5)
            (Gen.string (Range.linear 1 10) Gen.unicode)
        return $ L.ArgsM args
    , return $ L.SecretKeyFileM $ Left dummyException
    , do
        bytes <- Gen.bytes (Range.linear 30 50)
        return $ L.SecretKeyFileM $ Right bytes
    , return $ L.FromServerM $ Left dummyException
    , do
        mb <- Gen.maybe $ Gen.bytes $ Range.linear 2 100
        return $ L.FromServerM $ Right mb
    , L.NewSecretKeyM <$> secretKeyG
    ]



badMsgG :: H.Gen L.Msg
badMsgG =
    Gen.choice
    [ do
        args <- Gen.list
            (Range.linear 3 10) -- deliberately making too many args
            (Gen.string (Range.linear 1 10) Gen.unicode)
        return $ L.ArgsM args
    , return $ L.SecretKeyFileM (Left dummyException)
    , do
        msgs <- Gen.list (Range.linear 1 5) (fmap Just badMsgG)
        return $ L.BatchM msgs
    , return $ L.FromServerM $ Right Nothing
    , return $ L.FromServerM $ Left dummyException
    ]


outputContainsTcpClose :: L.Output -> Bool
outputContainsTcpClose output =
    case output of
    L.CloseTcpO _ ->
        True

    L.BatchO outputs ->
        any outputContainsTcpClose outputs

    _ ->
        False


updateStartM :: H.Property
updateStartM =
    H.property $ do
        state <- H.forAll $ fmap L.ReadyS readyG
        let (output, newState) = L.update state L.StartM
        case newState of
            L.InitS _ ->
                output H./== L.DoNothingO

            L.FinishedS ->
                H.failure

            L.ReadyS _ ->
                H.failure


propReverse :: H.Property
propReverse =
    H.property $ do
        xs <- H.forAll $ Gen.list (Range.linear 0 100) Gen.alpha
        reverse (reverse xs) H.=== xs


readySocketG :: H.Gen L.Ready
readySocketG = do
    secretKey <- secretKeyG
    readingStdIn <- readingStdInG
    authStatus <- authSocketG
    return $ L.Ready secretKey authStatus readingStdIn


readyG :: H.Gen L.Ready
readyG = do
    secretKey <- secretKeyG
    readingStdIn <- readingStdInG
    authStatus <- authStatusG
    return $ L.Ready secretKey authStatus readingStdIn


stateG :: H.Gen L.State
stateG =
    Gen.choice
    [ fmap L.ReadyS readyG
    , fmap L.InitS initG
    , return L.FinishedS
    ]


initG :: H.Gen L.Init
initG =
    Gen.element
    [ L.EmptyI
    , L.GettingKeysFromFileI
    , L.GeneratingSecretKeyI
    ]


authSocketG :: H.Gen L.AuthStatus
authSocketG =
    Gen.choice
    [ do
        sockety <- socketyG
        return $ L.LoggedInA sockety
    , do
        toServer <- toServerG
        mb <- fmap Just socketyG
        return $ L.NotLoggedInA $ L.SendWhenLoggedIn mb toServer
    ]


authStatusG :: H.Gen L.AuthStatus
authStatusG =
    Gen.choice
    [ do
        sockety <- socketyG
        return $ L.LoggedInA sockety
    , do
        toServer <- toServerG
        mb <- Gen.maybe socketyG
        return $ L.NotLoggedInA $ L.SendWhenLoggedIn mb toServer
    , return $ L.NotLoggedInA L.JustNotLoggedIn
    ]


socketyG :: H.Gen (Tcp.Socket, L.ListenStatus)
socketyG = do
    listen <- listenStatusG
    return (dummyTcpSocket, listen)


listenStatusG :: H.Gen L.ListenStatus
listenStatusG =
    Gen.element [L.GettingLength, L.GettingBody] 


toServerG :: H.Gen L.ToServer
toServerG =
    Gen.choice
    [ do
        key <- publicKeyG
        sig <- signatureG
        return $ L.SignedAuthCodeT key sig
    , do
        key <- publicKeyG
        txt <- messageG
        return $ L.SendMessageT key txt
    , return L.GetMessageT
    ]


signatureG :: H.Gen Ed.Signature
signatureG = do
    raw <- Gen.bytes (Range.singleton Ed.signatureSize)
    case Ed.signature raw of
        CryptoPassed sig ->
            return sig

        CryptoFailed err ->
            fail $ show err


messageG :: H.Gen T.Text
messageG =
    Gen.text (Range.constant 0 100) Gen.ascii


secretKeyG :: H.Gen Ed.SecretKey
secretKeyG = do
    raw <- Gen.bytes (Range.singleton Ed.secretKeySize)
    case Ed.secretKey raw of
        CryptoPassed key ->
            return key

        CryptoFailed err ->
            fail $ show err


publicKeyG :: H.Gen Ed.PublicKey
publicKeyG = do
    raw <- Gen.bytes (Range.singleton Ed.publicKeySize)
    case Ed.publicKey raw of
        CryptoPassed key ->
            return key

        CryptoFailed err ->
            fail $ show err


readingStdInG :: H.Gen (Maybe Ed.PublicKey)
readingStdInG =
    Gen.maybe publicKeyG

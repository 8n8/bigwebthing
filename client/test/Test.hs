import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as Th
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog as H
import qualified Lib as L
import qualified Crypto.PubKey.Ed25519 as Ed
import Crypto.Error (CryptoFailable(..))

main :: IO ()
main =
    Tasty.defaultMain properties


properties :: Tasty.TestTree
properties =
    Tasty.testGroup ""
    [ Th.testProperty "reverse" propReverse
    ]


propReverse :: H.Property
propReverse =
    H.property $ do
        xs <- H.forAll $ Gen.list (Range.linear 0 100) Gen.alpha
        reverse (reverse xs) H.=== xs


readyG :: H.Gen L.Ready
readyG = do
    secretKey <- secretKeyG
    readingStdIn <- readingStdInG
    return $
        L.Ready
            secretKey (L.NotLoggedInA L.JustNotLoggedIn) readingStdIn


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


msgG :: H.Gen L.Msg
msgG =
    Gen.choice
    [ return L.StartM
    , return $ L.TcpSendResultM (Right ())
    , return $ L.BadTcpRecvM "bad"
    , do
        bytes <- Gen.bytes (Range.linear 0 150)
        return $ L.StdInM bytes
    , do
        args <- Gen.list
            (Range.linear 0 5)
            (Gen.string (Range.linear 1 10) Gen.unicode)
        return $ L.ArgsM args
    , do
        bytes <- Gen.bytes (Range.linear 0 100)
        return $ L.SecretKeyFileM (Right bytes)
    , do
        msgs <- Gen.list (Range.linear 0 5) (Gen.maybe msgG)
        return $ L.BatchM msgs
    , do
        fromServer <- Gen.bytes (Range.linear 0 300)
        return $ L.FromServerM fromServer
    , do
        secret <- secretKeyG
        return $ L.NewSecretKeyM secret
    ]



notBackToInit :: H.Property
notBackToInit =
    H.property $ do
        ready <- H.forAll readyG
        msg <- H.forAll msgG
        let (_, newState) = L.update (L.ReadyS ready) msg
        case newState of
            L.InitS _ ->
                H.failure

            L.ReadyS _ ->
                return ()

            L.FinishedS ->
                return ()

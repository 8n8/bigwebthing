module Main (main) where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as Th
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog as H
import qualified Update as L
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Crypto.PubKey.Ed25519 as Ed


main :: IO ()
main =
    Tasty.defaultMain properties


properties :: Tasty.TestTree
properties =
    Tasty.testGroup ""
    [ Th.testProperty "reverse" propReverse
    , Th.testProperty "updateStartM" updateStartM
    ]


updateStartM :: H.Property
updateStartM =
    H.property $ do
        state <- H.forAll stateG
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

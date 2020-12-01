import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as Th
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog as H
import qualified Lib as L
import qualified Data.Attoparsec.ByteString as P

main :: IO ()
main =
    Tasty.defaultMain properties


properties :: Tasty.TestTree
properties =
    Tasty.testGroup ""
    [ Th.testProperty "reverse" propReverse
    , Th.testProperty "parseRecipient" parseRecipient
    , Th.testProperty "uint16parseDecode" uint16parseDecode
    ]



uint16parseDecode :: H.Property
uint16parseDecode =
    H.property $ do
        n <- H.forAll $ Gen.int (Range.constant 0 65535)
        case P.parseOnly L.uint16P (L.encodeUint16 n) of
            Left _ ->
                H.failure

            Right n1 ->
                n H.=== n1


parseRecipient :: H.Property
parseRecipient =
    H.property $ do
        raw <- H.forAll $ Gen.string (Range.linear 44 100) Gen.unicode
        case L.parseRecipient raw of
            Left _ ->
                return ()

            Right _ ->
                H.failure

        raw2 <- H.forAll $ Gen.string ( Range.linear 0 42) Gen.unicode
        case L.parseRecipient raw2 of
            Left _ ->
                return ()

            Right _ ->
                H.failure


propReverse :: H.Property
propReverse =
    H.property $ do
        xs <- H.forAll $ Gen.list (Range.linear 0 100) Gen.alpha
        reverse (reverse xs) H.=== xs

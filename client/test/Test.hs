import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as Th
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog as H

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

module Main (main) where

import qualified Hedgehog as H
import qualified Test.Tasty as Tasty
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Test.Tasty.Hedgehog as Th

main :: IO ()
main =
    Tasty.defaultMain properties


properties :: Tasty.TestTree
properties =
    let
    t = Th.testProperty
    in
    Tasty.testGroup ""
    [ t "reverse" propReverse
    ]


propReverse :: H.Property
propReverse =
    H.property $ do
        xs <- H.forAll $ Gen.list (Range.linear 0 100) Gen.alpha
        reverse (reverse xs) H.=== xs

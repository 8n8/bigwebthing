module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Testable exposing (..)


suite : Test
suite =
    describe "The Testable module"
        [ test "plus" <|
            \_ -> Expect.equal (plus 2) 7
        ]

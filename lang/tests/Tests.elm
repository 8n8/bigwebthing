module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Compiler exposing (..)


suite : Test
suite =
    describe "The Compiler module"
        [ test "empty program is wrong" <|
            \_ ->
                err (compile "")
        ]

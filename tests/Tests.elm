module Tests exposing (..)

import Bytes.Decode as D
import Bytes.Encode as E
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Hex.Convert
import Parser as P
import Test exposing (..)
import Truelang


suite : Test
suite =
    describe "Truelang"
        [ test "3" <| \_ ->
            Expect.err <| Truelang.compile {main = "3", modules = []}
        ]


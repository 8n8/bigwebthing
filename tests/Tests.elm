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
import Utils


suite : Test
suite =
    describe "Truelang" <| List.map makeTest spec


makeTest : (Utils.Code, Result String String) -> Test
makeTest (input, output) =
    test input.main <| \_ ->
        Expect.equal (Truelang.compile input) output


spec =
    [ ( { main = "0", modules = []}
      , Err <| String.dropLeft 1 """
Bad stack at program end.

Got:

    int 0

Expected:

    i32
"""
      )
    ]

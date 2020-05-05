module Tests exposing (..)

import Bytes.Decode as D
import Bytes.Encode as E
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser as P
import Test exposing (..)
import Truelang
import Utils


suite : Test
suite =
    describe "Truelang" <| List.map makeTest spec


makeTest : ( String, Result String String ) -> Test
makeTest ( input, output ) =
    test input <|
        \_ ->
            Expect.equal (Truelang.compile { main = input, modules = [] }) output


spec =
    [ ( "0"
      , Err <| String.dropLeft 1 """
Bad stack at program end.

Got:

    int 0

Expected:

    i32
"""
      )
    , ( "0 .meta:toI32:;"
      , Ok <| String.dropLeft 1 """
(module
    (import "env" "memory" (memory 1))
    (func $main (result i32)
        (i32.const 0)
    )
    (export "main" (func $main))
)
"""
      )
    , ( "0 .meta:toI64:;"
      , Err <| String.dropLeft 1 """
Bad stack at program end.

Got:

    i64

Expected:

    i32
"""
      )
    , ( "0.0 .meta:toF32:;"
      , Err <| String.dropLeft 1 """
Bad stack at program end.

Got:

    f32

Expected:

    i32
"""
      )
    ]

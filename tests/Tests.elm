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


makeTest : ( String, Utils.Code, Result String String ) -> Test
makeTest ( name, input, output ) =
    test name <|
        \_ ->
            Expect.equal (Truelang.compile input) output


spec =
    [ ( "just one integer"
      , { main = "0", modules = [] }
      , Err <| String.dropLeft 1 """
Bad stack at program end.

Got:

    0

Expected:

    i32
"""
      )
    , ( "very simple int32"
      , { main = "0 .meta:toI32:", modules = [] }
      , simpleOk
      )
    , ( "very simple int64"
      , { main = "0 .meta:toI64:", modules = [] }
      , Err <| String.dropLeft 1 """
Bad stack at program end.

Got:

    i64

Expected:

    i32
"""
      )
    , ( "very simple float32"
      , { main = "0.0 .meta:toF32:", modules = [] }
      , Err <| String.dropLeft 1 """
Bad stack at program end.

Got:

    f32

Expected:

    i32
"""
      )
    , ( "empty program"
      , { main = "", modules = [] }
      , emptyErr
      )
    , ( "basic import"
      , { main = "import " ++ Utils.hash "", modules = [ "" ] }
      , emptyErr
      )
    , ( "simple import"
      , { main = "import " ++ Utils.hash libSimple, modules = [ libSimple ] }
      , simpleOk
      )
    ]


libSimple =
    "0 .meta:toI32:"


simpleOk =
    Ok <| String.dropLeft 1 """
(module
    (import "env" "memory" (memory 1))
    (func $main (result i32)
        (i32.const 0)
    )
    (export "main" (func $main))
)
"""


emptyErr =
    Err <| String.dropLeft 1 """
Bad stack at program end.

Got:

    <empty stack>

Expected:

    i32
"""

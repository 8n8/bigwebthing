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
      , { main = "0 .wasm:toI32:", modules = [] }
      , simpleOk
      )
    , ( "very simple int64"
      , { main = "0 .wasm:toI64:", modules = [] }
      , Err <| String.dropLeft 1 """
Bad stack at program end.

Got:

    i64

Expected:

    i32
"""
      )
    , ( "very simple float32"
      , { main = "0.0 .wasm:toF32:", modules = [] }
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
    , ( "Meta bytes"
      , { main = "Hi .meta:toUtf8:", modules = [] }
      , Err <| String.dropLeft 1 """
Bad stack at program end.

Got:

    bytes 2 0 0 0 72 105

Expected:

    i32
"""
      )
    , ( "hi"
      , { main = hiMain, modules = [ libString ] }
      , hiWasm
      )
    ]


hiMain =
    Debug.log "hiMain" <| "import string " ++ Utils.hash libString ++ """
string.writeByte(0, 1)
string.writeString(1, "hi")
"""


libString =
    """
module string

export fun writeByte(byte: intT, offset: intT) -> () {
    wasm.i32.to(byte)
    RUN_UNSAFE({ wasm.i32.store8(byte, offset) })
    return ()
}

export fun writeBytes(bytes: bytesT, offset: intT) -> (intT) {
    // It expects the top of the stack to be some bytes, and the
    // second item to be the memory offset.
    list.map(
        fun (byte: intT) -> (intT) {
            writeByte(byte, offset)
            return(++(offset))
        },
        bytes)
    return(offset)
}

export fun writeString(string stringT, offset intT) () {
    // The top of the stack should be a string, and the second item
    // should be the memory offset. 
    bytes = stringToUtf8(string)
    writeBytes(bytes, offset)
}
"""


hiWasm =
    Ok """(module
    (import "env" "memory" (memory 1))
    (func $main (result i32)
        (i32.const 0)
        (i32.const 1)
        i32.store8
        (i32.const 1)
        (i32.const 2)
        i32.store
        (i32.const 5)
        (i32.const 72)
        i32.store8
        (i32.const 6)
        (i32.const 105)
        i32.store8
        (i32.const 7)
    )
    (export "main" (func $main))
)
"""


libSimple =
    "0 .wasm:toI32:"


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

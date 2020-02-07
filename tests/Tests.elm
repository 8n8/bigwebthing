module Tests exposing (..)

import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser as P
import Test exposing (..)
import Testable exposing (..)


suite : Test
suite =
    describe "The Testable module"
        [ parsingT
        ]


parsingT : Test
parsingT =
    describe "Parsers"
        [ programPT
        , topProgramPT
        , stringPT
        , defPT
        , variableT
        , programBlockPT
        ]


topProgramPT : Test
topProgramPT =
    describe "topProgramP"
        [ test "multiline" <|
            \_ ->
                Expect.ok <| P.run topProgramP "/**/"
        ]


programBlockPT : Test
programBlockPT =
    describe "programBlockP"
        [ test "empty" <|
            \_ ->
                Expect.err <| P.run (programBlockP { stack = [], defs = Dict.empty }) ""
        , test "simple" <|
            \_ ->
                Expect.ok <| P.run (programBlockP { stack = [], defs = Dict.empty }) "{ \"h\" }"
        ]


variableT : Test
variableT =
    describe "variable"
        [ test "empty" <|
            \_ ->
                Expect.err <| P.run variable ""
        , test "one character" <|
            \_ ->
                Expect.equal (P.run variable "a") (Ok "a")
        , test "bad first character" <|
            \_ ->
                Expect.err <| P.run variable "2a"
        ]


programPT : Test
programPT =
    describe "programP"
        [ test "empty" <|
            \_ ->
                Expect.equal (P.run (programP { stack = [], defs = Dict.empty }) "") (Ok { typeState = { stack = [], defs = Dict.empty }, elfs = [], elts = [] })
        , fuzz string "throws a load of junk at it to check it finishes" <|
            \s ->
                let
                    _ =
                        P.run (programP { stack = [], defs = Dict.empty }) s
                in
                Expect.pass
        , test "block comment" <|
            \_ ->
                Expect.ok <| P.run (programP { stack = [], defs = Dict.empty }) "/**/"
        ]


stringPT : Test
stringPT =
    describe "stringP"
        [ test "empty" <|
            \_ ->
                Expect.equal (P.run stringP "\"\"") (Ok "")
        , test "simple" <|
            \_ ->
                Expect.equal (P.run stringP "\"a\"") (Ok "a")
        , test "only one quote" <|
            \_ ->
                Expect.err <| P.run stringP "\""
        ]


defPT : Test
defPT =
    describe "defP"
        [ test "empty" <|
            \_ ->
                Expect.err <| P.run defP ""
        , test "simple" <|
            \_ ->
                Expect.ok <| P.run defP "= ff"
        , test "spaces between = and name" <|
            \_ ->
                Expect.ok <| P.run defP "=ff"
        ]


oneWhitespacePT : Test
oneWhitespacePT =
    describe "oneWhitespaceP"
        [ test "multiline simple" <|
            \_ ->
                Expect.ok <| P.run oneWhitespaceP "/**/"
        ]

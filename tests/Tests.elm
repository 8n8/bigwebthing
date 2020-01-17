module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Testable exposing (..)
import Parser as P


suite : Test
suite =
    describe "The Testable module"
        [ parsingT
        ]


parsingT : Test
parsingT =
    describe "Parsers"
        [ programPT
        , stringPT
        , defPT
        , variableT
        , programBlockPT
        ]


programBlockPT : Test
programBlockPT =
    describe "programBlockP"
        [ test "empty" <|
            \_ ->
                Expect.err <| P.run programBlockP ""
        , test "simple" <|
            \_ ->
                Expect.ok <| P.run programBlockP "{ \"h\" }"
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
        , test "multiple characters" <|
            \_ ->
                Expect.equal (P.run variable "C2x232") (Ok "c2x232")
        ]


programPT : Test
programPT =
    describe "programP"
        [ test "empty" <|
            \_ ->
                Expect.equal (P.run programP "") (Ok ([], []))

        , fuzz string "throws a load of junk at it to check it finishes" <|
            \s ->
                let _ = P.run programP s
                in Expect.pass
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

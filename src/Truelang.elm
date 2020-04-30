module Truelang exposing (compile)

import Dict
import Parser as P exposing ((|.), (|=))
import Set
import Utils



{-
   Overview:

   The code is parsed into into atoms.

   What's the difference between a ProgVal and an Atom? An atom is a
   piece of the code, whereas ProgVals are the values that the type
   checker uses to represent run-time values when it is checking the
   code.
-}


compile : Utils.Code -> Result String String
compile code =
    case parse code of
        Err parsingError ->
            Err <| "Parsing error: " ++ parsingError

        Ok atoms ->
            secondStage atoms


type Atom
    = Retrieve String
    | Block (List (Located Atom))
    | DefConst String
    | DefMut String
    | MetaSwitch (List ( Type, List (Located Atom) ))
    | MetaLoop
    | Runblock
    | Export String
    | AWasm WasmIn
    | Loop
    | IfElse
    | TypeWrap String
    | TypeUnwrap String
    | StringLiteral String
    | IntLiteral Int
    | FloatLiteral Float


type WasmOut
    = Oi32mul
    | Oloop (List WasmOut) (List WasmOut)
    | OifElse (List WasmOut) (List WasmOut) (List WasmOut)
    | OsetLocal String
    | OgetLocal String


type BasicType
    = Bi32
    | Bi64
    | Bf32
    | Bf64


showBasic : BasicType -> String
showBasic basicType =
    case basicType of
        Bi32 ->
            "i32"

        Bi64 ->
            "i64"

        Bf32 ->
            "f32"

        Bf64 ->
            "f64"


type
    WasmIn
    -- Simple
    = Ii32mul


makeWasmModule : List WasmOut -> Dict.Dict String ( Position, Def ) -> String
makeWasmModule wasms defs =
    String.concat
        [ "(module\n"
        , "  (import \"env\" \"memory\" (memory 1))\n"
        , "  (func $main (result i32)\n"
        , "    "
        , declareLocals defs
        , "\n"
        , wasmsToString wasms ++ "\n"
        , "  )\n"
        , "  (export \"main\" (func $main))\n"
        , ")\n"
        ]


declareLocals : Dict.Dict String ( Position, Def ) -> String
declareLocals defs =
    String.join " " <| Utils.justs <| List.map declareLocal <| Dict.toList defs


declareLocal : ( String, ( Position, Def ) ) -> Maybe String
declareLocal ( name, ( _, def ) ) =
    case def of
        Mutable basic ->
            Just <|
                String.concat
                    [ "(local $"
                    , name
                    , " "
                    , showBasic basic
                    , ")"
                    ]

        Constant (Tbasic basic) ->
            Just <|
                String.concat
                    [ "(local $"
                    , name
                    , " "
                    , showBasic basic
                    , ")"
                    ]

        Constant _ ->
            Nothing


wasmsToString : List WasmOut -> String
wasmsToString wasms =
    String.join " " <| List.map wasmToString wasms


wasmToString : WasmOut -> String
wasmToString wasm =
    case wasm of
        Oi32mul ->
            "i32.mul"

        Oloop break body ->
            String.concat
                [ "(block\n"
                , "  (loop\n"
                , "    " ++ wasmsToString body ++ "\n"
                , "\n"
                , "    " ++ wasmsToString break ++ "\n"
                , "    (br_if 1)\n"
                , "    (br 0)\n"
                , "  )\n"
                , ")\n"
                ]

        OifElse if_ then_ else_ ->
            String.concat
                [ wasmsToString if_ ++ "\n"
                , "(if\n"
                , "  (then " ++ wasmsToString then_ ++ ")\n"
                , "  (else " ++ wasmsToString else_ ++ ")\n"
                , ")\n"
                ]

        OsetLocal name ->
            String.concat
                [ "(set_local $"
                , name
                , ")\n"
                ]

        OgetLocal name ->
            String.concat
                [ "(get_local $"
                , name
                , ")\n"
                ]


secondStage : List (Located Atom) -> Result String String
secondStage atoms =
    runTypeChecks atoms initEltState


parse : Utils.Code -> Result String (List (Located Atom))
parse code =
    case P.run (parseP "main" code.modules) code.main of
        Err err ->
            Err <| deadEndsToString err

        Ok atoms ->
            Ok atoms


parseP : String -> List String -> P.Parser (List (Located Atom))
parseP moduleName modules =
    P.loop [] <| parseHelpP moduleName modules


parseHelpP :
    String
    -> List String
    -> List (Located Atom)
    -> P.Parser (P.Step (List (Located Atom)) (List (Located Atom)))
parseHelpP moduleName modules p =
    P.oneOf
        [ P.map
            (\elements -> P.Loop (List.reverse elements ++ p))
            (importHelpP modules)
        , P.map
            (\element -> P.Loop (element :: p))
            (located moduleName (elementP modules))
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse p))
        ]


initEltState : EltState
initEltState =
    { position = { start = ( 0, 0 ), end = ( 0, 0 ), file = "" }
    , defs = Dict.empty
    , stack = []
    , wasmOut = []
    , defUse = Set.empty
    , isHome = True
    , wrapperTypes = Dict.empty
    , error = Nothing
    }


runTypeChecks : List (Located Atom) -> EltState -> Result String String
runTypeChecks atoms init =
    case runTypeChecksHelp (Just [ Tbasic Bi32 ]) atoms init of
        Ok endState ->
            Ok <| makeWasmModule endState.wasmOut endState.defs

        Err err ->
            Err <| prettyErrorMessage err


deadEndsToString : List P.DeadEnd -> String
deadEndsToString deadEnds =
    String.join "\n\n" <| List.map deadEndToString deadEnds


deadEndToString : P.DeadEnd -> String
deadEndToString deadEnd =
    String.concat
        [ "parsing error:\n"
        , "row: "
        , String.fromInt deadEnd.row
        , "\n"
        , "col: "
        , String.fromInt deadEnd.col
        , "\n"
        , problemToString deadEnd.problem
        ]


type Type
    = Tbasic BasicType
    | Tall
    | Twrapper String Type
    | Tblock (List (Located Atom))
    | Tint Int
    | Tfloat Float
    | Tstring String


metaLoopHelp : List (Located Atom) -> Int -> EltOut -> EltOut
metaLoopHelp block counter stateResult =
    case stateResult of
        Err err ->
            Err { err | message = "error in metaloop iteration " ++ String.fromInt counter ++ ": " ++ err.message }

        Ok state ->
            runTypeChecksHelp Nothing block state


type alias Located a =
    { start : ( Int, Int )
    , value : a
    , file : String
    , end : ( Int, Int )
    }


type Def
    = Mutable BasicType
    | Constant Type


type alias EltState =
    { position : Position
    , defs : Dict.Dict String ( Position, Def )
    , stack : List Type
    , wasmOut : List WasmOut
    , defUse : Set.Set String
    , isHome : Bool
    , wrapperTypes : Dict.Dict String Type
    , error : Maybe String
    }


findPath : Type -> List ( Type, List (Located Atom) ) -> Result String (List (Located Atom))
findPath key paths =
    case List.filter (isSubType key << Tuple.first) paths of
        [] ->
            Err <| "no path matching " ++ showTypeVal key

        [ ( _, matchingPath ) ] ->
            Ok matchingPath

        matchingPaths ->
            Err <| "multiple paths matching " ++ showTypeVal key ++ ": " ++ String.join " " (List.map (showTypeVal << Tuple.first) matchingPaths)


type alias Position =
    { file : String
    , start : ( Int, Int )
    , end : ( Int, Int )
    }


prettyErrorMessage : TypeError -> String
prettyErrorMessage { state, message } =
    String.concat
        [ prettyLocation state.position
        , ":\n"
        , message
        ]


prettyLocation : Position -> String
prettyLocation { file, start, end } =
    String.concat
        [ "file "
        , file
        , "\n"
        , "between "
        , prettyPosition start
        , " and "
        , prettyPosition end
        ]


prettyPosition : ( Int, Int ) -> String
prettyPosition ( row, column ) =
    String.concat
        [ "row "
        , String.fromInt row
        , " column "
        , String.fromInt column
        ]


badStackEnd : List Type -> List Type -> String
badStackEnd expected got =
    String.concat
        [ "bad stack: got "
        , showTypeStack got
        , ", expected "
        , showTypeStack expected
        ]


checkStackEnd : List Type -> List Type -> Bool
checkStackEnd sub master =
    if List.length sub /= List.length master then
        False

    else
        List.all (\( a, b ) -> isSubType a b) <| zip sub master


zip : List a -> List b -> List ( a, b )
zip las lbs =
    List.map2 (\a b -> ( a, b )) las lbs


runTypeChecksHelp : Maybe (List Type) -> List (Located Atom) -> EltState -> EltOut
runTypeChecksHelp stackEnd atoms state =
    case atoms of
        [] ->
            case stackEnd of
                Nothing ->
                    Ok state

                Just se ->
                    if not <| checkStackEnd state.stack se then
                        Err
                            { state = state
                            , message = badStackEnd se state.stack
                            }

                    else
                        Ok state

        a :: toms ->
            case processAtom { state | position = { start = a.start, end = a.end, file = a.file } } a.value of
                Err err ->
                    Err err

                Ok ok ->
                    runTypeChecksHelp stackEnd toms ok


processAtom : EltState -> Atom -> EltOut
processAtom state atom =
    case atom of
        Retrieve v ->
            case Dict.get v state.defs of
                Nothing ->
                    Err
                        { state = state
                        , message = "no definition \"" ++ v ++ "\""
                        }

                Just ( _, Mutable retrieved ) ->
                    Ok
                        { state
                            | stack = Tbasic retrieved :: state.stack
                            , wasmOut = OgetLocal v :: state.wasmOut
                            , defUse = Set.insert v state.defUse
                        }

                Just ( _, Constant (Tbasic retrieved) ) ->
                    Ok
                        { state
                            | stack = Tbasic retrieved :: state.stack
                            , wasmOut = OgetLocal v :: state.wasmOut
                            , defUse = Set.insert v state.defUse
                        }

                Just ( _, Constant nonBasic ) ->
                    Ok
                        { state
                            | stack = nonBasic :: state.stack
                            , defUse = Set.insert v state.defUse
                        }

        Block bs ->
            Ok { state | stack = Tblock bs :: state.stack }

        MetaSwitch metaSwitch ->
            case state.stack of
                [] ->
                    Err { state = state, message = "empty stack" }

                s :: _ ->
                    case findPath s metaSwitch of
                        Err error ->
                            Err { state = state, message = error }

                        Ok path ->
                            Ok { state | stack = Tblock path :: state.stack }

        DefConst newName ->
            case Dict.get newName state.defs of
                Just ( position, _ ) ->
                    Err { state = state, message = "\"" ++ newName ++ "\" is immutable and is already defined at " ++ prettyLocation position }

                Nothing ->
                    case state.stack of
                        [] ->
                            Err { state = state, message = "empty stack" }

                        s :: tack ->
                            Ok
                                { state
                                    | stack = tack
                                    , defs = Dict.insert newName ( state.position, Constant s ) state.defs
                                    , wasmOut =
                                        case s of
                                            Tbasic _ ->
                                                OsetLocal newName :: state.wasmOut

                                            _ ->
                                                state.wasmOut
                                }

        Runblock ->
            case state.stack of
                [] ->
                    Err { state = state, message = "empty stack" }

                (Tblock block) :: tack ->
                    runTypeChecksHelp Nothing block { state | stack = tack }

                other :: _ ->
                    Err { state = state, message = "expecting a block on the stack, but got: " ++ showTypeVal other }

        MetaLoop ->
            case state.stack of
                [] ->
                    Err { state = state, message = "empty stack" }

                [ _ ] ->
                    Err { state = state, message = "only one thing on stack" }

                (Tint i) :: (Tblock block) :: tack ->
                    List.foldr (metaLoopHelp block) (Ok { state | stack = tack }) (List.range 1 i)

                other ->
                    Err { state = state, message = "expecting an int64 and a block on top of the stack, but got " ++ showTypeStack other }

        Export exported ->
            if Dict.member exported state.defs then
                Ok { state | defUse = Set.insert exported state.defUse }

            else
                Err { message = "\"" ++ exported ++ "\" is not defined", state = state }

        Loop ->
            loopHelp state

        IfElse ->
            ifElseTypeHelp state

        DefMut name ->
            case ( Dict.get name state.defs, state.stack ) of
                ( _, [] ) ->
                    Err { state = state, message = "empty stack" }

                ( Nothing, s :: tack ) ->
                    case s of
                        Tbasic basic ->
                            Ok
                                { state
                                    | stack = tack
                                    , wasmOut = OsetLocal name :: state.wasmOut
                                    , defs = Dict.insert name ( state.position, Mutable basic ) state.defs
                                }

                        _ ->
                            Err
                                { state = state
                                , message = "expected basic numeric type, but got " ++ showTypeVal s
                                }

                ( Just ( position, Mutable oldValue ), s :: tack ) ->
                    case s of
                        Tbasic basic ->
                            if oldValue == basic then
                                Ok
                                    { state
                                        | stack = tack
                                        , wasmOut = OsetLocal name :: state.wasmOut
                                    }

                            else
                                Err { state = state, message = "variable \"" ++ name ++ "\" was previously defined at position " ++ prettyLocation position ++ " with type " ++ showBasic oldValue ++ " but type of new value is " ++ showBasic basic }

                        _ ->
                            Err
                                { state = state
                                , message = "expected basic numeric type, but got " ++ showTypeVal s
                                }

                ( Just ( _, Constant _ ), _ ) ->
                    Err { state = state, message = "Can't change value of constant." }

        AWasm Ii32mul ->
            case state.stack of
                [] ->
                    Err { state = state, message = "empty stack" }

                _ :: [] ->
                    Err { state = state, message = "only one thing in stack" }

                i1 :: i2 :: tack ->
                    case ( isSubType i1 (Tbasic Bi32), isSubType i2 (Tbasic Bi32) ) of
                        ( False, _ ) ->
                            Err { state = state, message = "Top item in stack should be a " ++ showTypeVal (Tbasic Bi32) ++ ", but is a " ++ showTypeVal i1 }

                        ( _, False ) ->
                            Err { state = state, message = "Second item instack should be a " ++ showTypeVal (Tbasic Bi32) ++ ", but is a " ++ showTypeVal i2 }

                        ( True, True ) ->
                            Ok
                                { state
                                    | stack = Tbasic Bi32 :: tack
                                    , wasmOut = Oi32mul :: state.wasmOut
                                }

        TypeWrap type_ ->
            case ( state.stack, Dict.get type_ state.wrapperTypes ) of
                ( [], _ ) ->
                    Err { state = state, message = "empty stack" }

                ( contained :: tack, Nothing ) ->
                    Ok
                        { state
                            | wrapperTypes = Dict.insert type_ contained state.wrapperTypes
                            , stack = Twrapper type_ contained :: tack
                        }

                ( contained :: tack, Just previouslyWrapped ) ->
                    if isSubType contained previouslyWrapped then
                        Ok { state | stack = Twrapper type_ contained :: tack }

                    else
                        Err { state = state, message = "type \"" ++ showTypeVal contained ++ "\" is not compatible with \"" ++ type_ ++ "\"" }

        TypeUnwrap type_ ->
            case state.stack of
                [] ->
                    Err { state = state, message = "empty stack" }

                (Twrapper wrapper wrapped) :: tack ->
                    if type_ == wrapper then
                        Ok { state | stack = wrapped :: tack }

                    else
                        Err
                            { state = state
                            , message =
                                String.concat
                                    [ "expected \""
                                    , type_
                                    , "\", but got \""
                                    , wrapper
                                    ]
                            }

                other :: _ ->
                    Err
                        { state = state
                        , message =
                            String.concat
                                [ "expected \""
                                , type_
                                , "\", but got "
                                , showTypeVal other
                                ]
                        }

        StringLiteral string ->
            Ok { state | stack = Tstring string :: state.stack }

        IntLiteral i ->
            Ok { state | stack = Tint i :: state.stack }

        FloatLiteral f ->
            Ok { state | stack = Tfloat f :: state.stack }


loopHelp : EltState -> EltOut
loopHelp state =
    case state.stack of
        [] ->
            Err { state = state, message = "empty stack" }

        (Tblock body) :: (Tblock break) :: tack ->
            let
                cleanStack =
                    { state | stack = [] }

                bodyEndResult =
                    runTypeChecksHelp (Just []) body cleanStack

                breakEndResult =
                    runTypeChecksHelp (Just [ Tbasic Bi32 ]) break cleanStack
            in
            case ( bodyEndResult, breakEndResult ) of
                ( Err err, _ ) ->
                    Err
                        { message =
                            String.concat
                                [ "bad LOOP body: "
                                , prettyErrorMessage err
                                ]
                        , state = state
                        }

                ( _, Err err ) ->
                    Err
                        { message =
                            String.concat
                                [ "bad LOOP break block: "
                                , prettyErrorMessage err
                                ]
                        , state = state
                        }

                ( Ok bodyEnd, Ok breakEnd ) ->
                    let
                        loopWasm =
                            Oloop bodyEnd.wasmOut breakEnd.wasmOut
                    in
                    Ok { state | wasmOut = loopWasm :: state.wasmOut, stack = tack }

        _ ->
            Err { message = "there's nothing to run", state = state }


ifElseTypeHelp : EltState -> EltOut
ifElseTypeHelp state =
    case state.stack of
        (Tblock else_) :: (Tblock then_) :: (Tblock switch) :: tack ->
            let
                cleanStack =
                    { state | stack = [] }

                elseEnd =
                    runTypeChecksHelp (Just []) else_ cleanStack

                thenEnd =
                    runTypeChecksHelp (Just []) then_ cleanStack

                switchEnd =
                    runTypeChecksHelp (Just [ Tbasic Bi32 ]) switch cleanStack
            in
            case ( elseEnd, thenEnd, switchEnd ) of
                ( Err err, _, _ ) ->
                    Err
                        { message =
                            String.concat
                                [ "bad ELSE block: "
                                , prettyErrorMessage err
                                ]
                        , state = state
                        }

                ( _, Err err, _ ) ->
                    Err
                        { message =
                            String.concat
                                [ "bad IF block: "
                                , prettyErrorMessage err
                                ]
                        , state = state
                        }

                ( _, _, Err err ) ->
                    Err
                        { message =
                            String.concat
                                [ "bad switch block in IFELSE: "
                                , prettyErrorMessage err
                                ]
                        , state = state
                        }

                ( Ok elseE, Ok thenE, Ok switchE ) ->
                    let
                        ifElseWasm =
                            OifElse
                                switchE.wasmOut
                                thenE.wasmOut
                                elseE.wasmOut
                    in
                    Ok { state | wasmOut = ifElseWasm :: state.wasmOut, stack = tack }

        bad ->
            Err
                { message =
                    String.concat
                        [ "bad stack: "
                        , showTypeStack bad
                        , ", expecting IF, ELSE, and SWITCH blocks"
                        ]
                , state = state
                }


showAtoms : List Atom -> String
showAtoms atoms =
    String.join " " <| List.map showAtom atoms


problemToString : P.Problem -> String
problemToString problem =
    case problem of
        P.Expecting expecting ->
            "expecting: " ++ expecting

        P.ExpectingInt ->
            "expecting an Int"

        P.ExpectingHex ->
            "expecting some Hex"

        P.ExpectingOctal ->
            "expecting some Octal"

        P.ExpectingBinary ->
            "expecting some Binary"

        P.ExpectingFloat ->
            "expecting a Float"

        P.ExpectingNumber ->
            "expecting a Number"

        P.ExpectingVariable ->
            "expecting a variable"

        P.ExpectingSymbol symbol ->
            "expecting a symbol: \"" ++ symbol ++ "\""

        P.ExpectingKeyword keyword ->
            "expecting a keyword: \"" ++ keyword ++ "\""

        P.ExpectingEnd ->
            "expecting end of input"

        P.UnexpectedChar ->
            "unexpected character"

        P.Problem prob ->
            "error: " ++ prob

        P.BadRepeat ->
            "bad repeat"


type alias TypeError =
    { state : EltState
    , message : String
    }


type alias EltOut =
    Result TypeError EltState


showTypeVal : Type -> String
showTypeVal type_ =
    case type_ of
        Tblock block ->
            "block: " ++ showAtoms (List.map .value block)

        Tall ->
            "all"

        Tbasic Bi32 ->
            "int32"

        Tbasic Bi64 ->
            "int64"

        Tbasic Bf32 ->
            "float32"

        Tbasic Bf64 ->
            "float64"

        Twrapper wrapper wrapped ->
            wrapper ++ "<" ++ showTypeVal wrapped ++ ">"

        Tint i ->
            "int: " ++ String.fromInt i

        Tfloat f ->
            "float: " ++ String.fromFloat f

        Tstring s ->
            "\"" ++ showString s ++ "\""


isSubType : Type -> Type -> Bool
isSubType sub master =
    case ( sub, master ) of
        -- Tall
        ( _, Tall ) ->
            True

        ( Tall, _ ) ->
            False

        -- Tint
        ( Tint _, Tbasic Bi32 ) ->
            True

        ( Tint i1, Tint i2 ) ->
            i1 == i2

        ( Tint _, Tbasic Bi64 ) ->
            True

        ( Tint _, _ ) ->
            False

        ( Tbasic Bi32, Tbasic Bi32 ) ->
            True

        ( Tbasic Bi32, _ ) ->
            False

        ( Tbasic Bi64, Tbasic Bi64 ) ->
            True

        ( Tbasic Bi64, _ ) ->
            False

        ( Tfloat _, Tbasic Bf32 ) ->
            True

        ( Tfloat _, Tbasic Bf64 ) ->
            True

        ( Tfloat f1, Tfloat f2 ) ->
            f1 == f2

        ( Tfloat _, _ ) ->
            False

        ( Tbasic Bf32, Tbasic Bf32 ) ->
            True

        ( Tbasic Bf32, _ ) ->
            False

        ( Tbasic Bf64, Tbasic Bf64 ) ->
            True

        ( Tbasic Bf64, _ ) ->
            False

        ( Twrapper t1 _, Twrapper t2 _ ) ->
            t1 == t2

        ( Twrapper _ _, _ ) ->
            False

        -- Tblock
        ( Tblock b1, Tblock b2 ) ->
            b1 == b2

        ( Tblock _, _ ) ->
            False

        ( Tstring s1, Tstring s2 ) ->
            s1 == s2

        ( Tstring _, _ ) ->
            False


elementP : List String -> String -> P.Parser Atom
elementP modules moduleName =
    P.oneOf
        [ runBlockP
        , exportP
        , plainP
        , setMutLocalP
        , retrieveP
        , defConstP
        , programBlockP moduleName modules
        , intLiteralP
        , floatLiteralP
        , typeWrapP
        , typeUnwrapP
        , metaSwitchP modules moduleName
        , stringLiteralP
        ]


setMutLocalP : P.Parser Atom
setMutLocalP =
    P.succeed DefMut
        |. P.keyword "~="
        |. whiteSpaceP
        |= variable


stringHelp2 : ( List String, Int ) -> P.Parser (P.Step ( List String, Int ) String)
stringHelp2 ( revChunks, offset ) =
    P.succeed (stepHelp offset)
        |= stringHelp revChunks
        |= P.getOffset


stringLiteralP : P.Parser Atom
stringLiteralP =
    P.map StringLiteral stringP


stringHelp : List String -> P.Parser (P.Step (List String) String)
stringHelp revChunks =
    P.oneOf
        [ P.succeed (\chunk -> P.Loop (chunk :: revChunks))
            |. P.token "\\"
            |= P.oneOf
                [ P.map (\_ -> "\n") (P.token "n")
                , P.map (\_ -> "\t") (P.token "t")
                , P.map (\_ -> "\u{000D}") (P.token "r")
                , P.map (\_ -> "\"") (P.token "\"")
                ]
        , P.chompWhile isUninteresting
            |> P.getChompedString
            |> P.map (\chunk -> P.Loop (chunk :: revChunks))
        ]


stepHelp : Int -> P.Step (List String) String -> Int -> P.Step ( List String, Int ) String
stepHelp oldOffset step newOffset =
    case step of
        P.Done str ->
            P.Done str

        P.Loop revChunks ->
            if newOffset > oldOffset then
                P.Loop ( revChunks, newOffset )

            else
                P.Done <| String.join "" <| List.reverse revChunks


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'


{-| Mostly copied from <https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm>
-}
stringP : P.Parser String
stringP =
    P.succeed identity
        |. P.token "\""
        |= P.loop ( [], 0 ) stringHelp2
        |. P.token "\""


metaSwitchP : List String -> String -> P.Parser Atom
metaSwitchP moduleName modules =
    P.map MetaSwitch <|
        P.sequence
            { start = "("
            , separator = ","
            , end = ")"
            , spaces = whiteSpaceP
            , item = metaSwitchBranchP moduleName modules
            , trailing = P.Forbidden
            }


metaSwitchBranchP : List String -> String -> P.Parser ( Type, List (Located Atom) )
metaSwitchBranchP modules moduleName =
    P.succeed (\type_ atom -> ( type_, atom ))
        |= typeP
        |. whiteSpaceP
        |. P.symbol "->"
        |. whiteSpaceP
        |= bareBlockP moduleName modules


typeP : P.Parser Type
typeP =
    P.oneOf
        [ intTypeP
        , floatTypeP
        , intTypeP
        , allInt32typeP
        , allInt64typeP
        , allFloat32typeP
        , allFloat64typeP
        , wrapperTypeP
        , allTypesP
        ]


allTypesP : P.Parser Type
allTypesP =
    P.succeed Tall
        |. P.keyword "allTypes"


intTypeP : P.Parser Type
intTypeP =
    P.succeed Tint
        |. P.keyword "int"
        |= intHelpP


floatTypeP : P.Parser Type
floatTypeP =
    P.succeed Tfloat
        |. P.keyword "float"
        |= P.float


allInt32typeP : P.Parser Type
allInt32typeP =
    P.succeed (Tbasic Bi32)
        |. P.keyword "i32"


allInt64typeP : P.Parser Type
allInt64typeP =
    P.succeed (Tbasic Bi64)
        |. P.keyword "i64"


allFloat32typeP : P.Parser Type
allFloat32typeP =
    P.succeed (Tbasic Bf32)
        |. P.keyword "f32"


allFloat64typeP : P.Parser Type
allFloat64typeP =
    P.succeed (Tbasic Bf64)
        |. P.keyword "f64"


wrapperTypeP : P.Parser Type
wrapperTypeP =
    P.succeed Twrapper
        |= variable
        |. P.symbol "<"
        -- The lazy wrapper is necessary to get round a
        -- "Bad recursion" compiler error.
        |= P.lazy (\_ -> typeP)
        |. P.symbol ">"


typeUnwrapP : P.Parser Atom
typeUnwrapP =
    P.succeed TypeUnwrap
        |. P.token ">"
        |= variable
        |. P.token "<"


typeWrapP : P.Parser Atom
typeWrapP =
    P.succeed TypeWrap
        |. P.token "<"
        |= variable
        |. P.token ">"


intLiteralP : P.Parser Atom
intLiteralP =
    P.map IntLiteral intHelpP


intHelpP : P.Parser Int
intHelpP =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |= P.number
                { int = Just identity
                , hex = Just identity
                , octal = Nothing
                , binary = Nothing
                , float = Nothing
                }
        , P.int
        ]


floatLiteralP : P.Parser Atom
floatLiteralP =
    P.map FloatLiteral floatP


floatP : P.Parser Float
floatP =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |= P.float
        , P.float
        ]


plainP : P.Parser Atom
plainP =
    P.oneOf <|
        List.map plainHelpP <|
            [ ( "loop", Loop )
            , ( "ifElse", IfElse )
            , ( "metaLoop", MetaLoop )

            -- Basic WASM instructions
            , ( "i32.mul", AWasm Ii32mul )
            ]


plainHelpP : ( String, Atom ) -> P.Parser Atom
plainHelpP ( string, atom ) =
    P.succeed atom |. P.keyword string


located : String -> (String -> P.Parser a) -> P.Parser (Located a)
located filename parser =
    P.succeed (\start value end -> { start = start, end = end, value = value, file = filename })
        |. whiteSpaceP
        |= P.getPosition
        |= parser filename
        |= P.getPosition
        |. whiteSpaceP


importHelpP : List String -> P.Parser (List (Located Atom))
importHelpP modules =
    P.andThen (importHelpHelpP modules) importP


showTypeStack : List Type -> String
showTypeStack typestack =
    "[" ++ (String.join ", " <| List.map showTypeVal typestack) ++ "]"


programBlockP : String -> List String -> P.Parser Atom
programBlockP moduleName modules =
    P.map Block <| bareBlockP moduleName modules


bareBlockP : String -> List String -> P.Parser (List (Located Atom))
bareBlockP moduleName modules =
    P.succeed identity
        |. P.token "{"
        |= parseP moduleName modules
        |. P.token "}"


defConstP : P.Parser Atom
defConstP =
    P.succeed DefConst
        |. P.token "="
        |. whiteSpaceP
        |= variable


retrieveP : P.Parser Atom
retrieveP =
    P.map Retrieve variable


exportP : P.Parser Atom
exportP =
    P.succeed Export
        |. P.keyword "export"
        |. whiteSpaceP
        |= variable


runBlockP : P.Parser Atom
runBlockP =
    P.succeed Runblock
        |. P.token "."


whiteSpaceP : P.Parser ()
whiteSpaceP =
    P.loop 0 <| ifProgress oneWhitespaceP


importP : P.Parser String
importP =
    P.succeed identity
        |. P.keyword "import"
        |. whiteSpaceP
        |= hashP
        |. whiteSpaceP


hashP : P.Parser String
hashP =
    P.andThen
        (\s ->
            if String.length s == 44 then
                P.succeed s

            else
                P.problem "hash is not 44 characters"
        )
        hashPhelp


hashPhelp : P.Parser String
hashPhelp =
    P.succeed (\body -> body ++ "=")
        |= hashBody
        |. P.token "="


hashBody : P.Parser String
hashBody =
    P.getChompedString <| P.succeed () |. hashCharsP


importHelpHelpP : List String -> String -> P.Parser (List (Located Atom))
importHelpHelpP modules toImport =
    case findModule modules toImport of
        Nothing ->
            P.problem <| "can't find program with name \"" ++ toImport ++ "\""

        Just module_ ->
            case P.run (parseP toImport modules) module_ of
                Err err ->
                    P.problem <| deadEndsToString err

                Ok atoms ->
                    P.succeed atoms


findModule : List String -> String -> Maybe String
findModule modules hash =
    let
        hashes =
            Dict.fromList <| List.map (\s -> ( Utils.hash s, s )) modules
    in
    Dict.get hash hashes


showAtom : Atom -> String
showAtom atom =
    case atom of
        MetaLoop ->
            "MetaLoop"

        Retrieve string ->
            "Retrieve " ++ showString string

        Block atoms ->
            "Block {" ++ bareShowBlock atoms ++ "}"

        DefConst string ->
            "DefConst " ++ string

        MetaSwitch paths ->
            "MetaSwitch: " ++ showPaths paths

        Runblock ->
            "Runblock"

        Export s ->
            "Export " ++ s

        AWasm wasm ->
            "WASM: " ++ showWasm wasm

        Loop ->
            "loop"

        IfElse ->
            "ifelse"

        TypeWrap t ->
            "<" ++ t ++ ">"

        TypeUnwrap t ->
            ">" ++ t ++ "<"

        StringLiteral s ->
            "\"" ++ showString s ++ "\""

        DefMut name ->
            "DefMut " ++ name

        IntLiteral i ->
            "int: " ++ String.fromInt i

        FloatLiteral f ->
            "float: " ++ String.fromFloat f


bareShowBlock : List (Located Atom) -> String
bareShowBlock block =
    String.join " " <| List.map (showAtom << .value) block


showPaths : List ( Type, List (Located Atom) ) -> String
showPaths paths =
    "(" ++ String.join ", " (List.map showPath paths) ++ ")"


showPath : ( Type, List (Located Atom) ) -> String
showPath ( type_, block ) =
    showTypeVal type_ ++ " -> " ++ bareShowBlock block


showWasm : WasmIn -> String
showWasm wasm =
    case wasm of
        Ii32mul ->
            "i32.mul"


variable : P.Parser String
variable =
    P.variable
        { start = \c -> Char.isAlpha c || Set.member c okVariableStart
        , inner = \c -> Char.isAlphaNum c || Set.member c okVariableInner
        , reserved = reserved
        }


{-| Don't use P.NotNestable for multicomment, as it doesn't consume
the closing \*/
-}
oneWhitespaceP : P.Parser ()
oneWhitespaceP =
    P.oneOf
        [ P.lineComment "//"
        , P.multiComment "/*" "*/" P.Nestable
        , P.spaces
        ]


ifProgress : P.Parser a -> Int -> P.Parser (P.Step Int ())
ifProgress parser offset =
    P.succeed identity
        |. parser
        |= P.getOffset
        |> P.map
            (\newOffset ->
                if offset == newOffset then
                    P.Done ()

                else
                    P.Loop newOffset
            )


hashCharsP : P.Parser ()
hashCharsP =
    P.chompWhile (\c -> Set.member c okHashChars)


okHashChars : Set.Set Char
okHashChars =
    Set.fromList
        [ 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/' ]


showString : String -> String
showString s =
    String.foldr showStringHelp "" s


showStringHelp : Char -> String -> String
showStringHelp char accumulator =
    case char of
        '"' ->
            "\"\\" ++ accumulator

        '\n' ->
            "n\\" ++ accumulator

        '\t' ->
            "t\\" ++ accumulator

        '\u{000D}' ->
            "r\\" ++ accumulator

        c ->
            String.cons c accumulator


okVariableStart : Set.Set Char
okVariableStart =
    Set.fromList
        [ '+'
        , '='
        ]


reserved : Set.Set String
reserved =
    Set.fromList
        [ "."
        , "="
        , "import"
        , "export"
        ]


okVariableInner : Set.Set Char
okVariableInner =
    Set.fromList
        [ '=' ]

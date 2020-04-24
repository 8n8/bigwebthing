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
            case typeCheck atoms of
                Just typeError ->
                    Err <| "Type error: " ++ typeError

                Nothing ->
                    case makeWasm atoms of
                        Err error ->
                            Err <| "WASM generator error: " ++ error

                        Ok wasm ->
                            Ok wasm


type Atom
    = Retrieve String
    | Block (List (Located Atom))
    | MetaDefine String
    | Runblock
    | Export String
    | AWasm WasmIn
    | Loop
    | IfElse
    | TypeWrap String
    | TypeUnwrap String


type alias WasmState =
    { defs : Dict.Dict String Atom
    , wasmStack : List WasmOut
    , error : Maybe String
    , metaStack : List Atom
    }


type WasmOut
    = Oi32mul
    | Oi32const Int
    | Oi32store8
    | Oloop (List WasmOut) (List WasmOut)
    | OifElse (List WasmOut) (List WasmOut) (List WasmOut)
    | OsetLocal BasicType String
    | OupdateLocal String
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


fromBasic : BasicType -> Type
fromBasic basicType =
    case basicType of
        Bi32 ->
            TallInt32

        Bi64 ->
            TallInt64

        Bf32 ->
            TallFloat32

        Bf64 ->
            TallFloat64


type WasmIn
    = Ii32mul
    | Ii32const Int
    | Ii32store8
    | IsetConstLocal BasicType String
    | IsetMutLocal BasicType String
    | IupdateMutLocal String
    | IgetLocal String


initWasmState : WasmState
initWasmState =
    { defs = Dict.empty
    , wasmStack = []
    , error = Nothing
    , metaStack = []
    }


makeWasm : List (Located Atom) -> Result String String
makeWasm atoms =
    let
        result =
            makeWasmHelp initWasmState atoms
    in
    case result.error of
        Nothing ->
            Ok <|
                String.concat
                    [ "(module\n"
                    , "  (import \"env\" \"memory\" (memory 1))\n"
                    , "  (func $main (result i32)\n"
                    , wasmsToString result.wasmStack ++ "\n"
                    , "  )\n"
                    , "  (export \"main\" (func $main))\n"
                    , ")\n"
                    ]

        Just error ->
            Err error


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

        Oi32const i ->
            "i32.const " ++ String.fromInt i

        Oi32store8 ->
            "i32.store8"

        OsetLocal basicType name ->
            String.concat
                [ "(local $"
                , name
                , " "
                , showBasic basicType
                , ")\n"
                , "(set_local $"
                , name
                , ")\n"
                ]

        OupdateLocal name ->
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


makeWasmHelp : WasmState -> List (Located Atom) -> WasmState
makeWasmHelp wasmState atoms =
    List.foldr makeOneWasm wasmState atoms


makeOneWasm : Located Atom -> WasmState -> WasmState
makeOneWasm { value } accum =
    case value of
        AWasm Ii32mul ->
            { accum | wasmStack = Oi32mul :: accum.wasmStack }

        AWasm (Ii32const i) ->
            { accum | wasmStack = Oi32const i :: accum.wasmStack }

        AWasm Ii32store8 ->
            { accum | wasmStack = Oi32store8 :: accum.wasmStack }

        AWasm (IsetConstLocal basicType name) ->
            { accum | wasmStack = OsetLocal basicType name :: accum.wasmStack }

        AWasm (IsetMutLocal basicType name) ->
            { accum | wasmStack = OsetLocal basicType name :: accum.wasmStack }

        AWasm (IupdateMutLocal name) ->
            { accum | wasmStack = OupdateLocal name :: accum.wasmStack }

        AWasm (IgetLocal name) ->
            { accum | wasmStack = OgetLocal name :: accum.wasmStack }

        Retrieve name ->
            case Dict.get name accum.defs of
                Nothing ->
                    { accum
                        | error =
                            Just <|
                                "could not find \""
                                    ++ name
                                    ++ "\""
                    }

                Just retrieved ->
                    { accum
                        | metaStack =
                            retrieved :: accum.metaStack
                    }

        Block block ->
            { accum | metaStack = Block block :: accum.metaStack }

        MetaDefine name ->
            case accum.metaStack of
                [] ->
                    { accum
                        | error =
                            Just <|
                                String.concat
                                    [ "nothing on stack to define as name \""
                                    , name
                                    , "\""
                                    ]
                    }

                s :: tack ->
                    { accum
                        | defs = Dict.insert name s accum.defs
                        , metaStack = tack
                    }

        Runblock ->
            case accum.metaStack of
                [] ->
                    { accum | error = Just "nothing on stack to run" }

                (Block block) :: tack ->
                    makeWasmHelp { accum | metaStack = tack } block

                other :: _ ->
                    { accum | error = Just <| "top of stack is not a block, it is: " ++ showAtom other }

        Export _ ->
            accum

        Loop ->
            case accum.metaStack of
                (Block body) :: (Block break) :: tack ->
                    let
                        clearStack =
                            { accum
                                | wasmStack = []
                                , metaStack = []
                            }

                        bodyWasm =
                            makeWasmHelp clearStack body

                        breakWasm =
                            makeWasmHelp clearStack break

                        loopWasm =
                            Oloop
                                breakWasm.wasmStack
                                bodyWasm.wasmStack
                    in
                    { accum | wasmStack = loopWasm :: accum.wasmStack }

                other ->
                    { accum
                        | error =
                            Just <|
                                String.concat
                                    [ "bad stack: expecting EXIT and BODY blocks "
                                    , "for the loop, but got: "
                                    , showAtoms other
                                    ]
                    }

        IfElse ->
            case accum.metaStack of
                (Block else_) :: (Block then_) :: (Block if_) :: tack ->
                    let
                        clearStack =
                            { accum
                                | wasmStack = []
                                , metaStack = []
                            }

                        ifWasm =
                            makeWasmHelp clearStack if_

                        thenWasm =
                            makeWasmHelp clearStack then_

                        elseWasm =
                            makeWasmHelp clearStack else_

                        ifElseWasm =
                            OifElse
                                ifWasm.wasmStack
                                thenWasm.wasmStack
                                elseWasm.wasmStack
                    in
                    { accum
                        | wasmStack =
                            ifElseWasm :: accum.wasmStack
                    }

                other ->
                    { accum
                        | error =
                            Just <|
                                String.concat
                                    [ "bad stack: expecting IF, THEN and ELSE "
                                    , "blocks, but got: "
                                    , showAtoms other
                                    ]
                    }

        TypeWrap _ ->
            accum

        TypeUnwrap _ ->
            accum


typeCheck : List (Located Atom) -> Maybe String
typeCheck atoms =
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
    , metaStack = []
    , wasmStack = []
    , defUse = Set.empty
    , isHome = True
    , wrapperTypes = Dict.empty
    }


runTypeChecks : List (Located Atom) -> EltState -> Maybe String
runTypeChecks atoms init =
    case runTypeChecksHelp (Just [ TallInt32 ]) atoms init of
        Ok endState ->
            Nothing

        Err err ->
            Just <| prettyErrorMessage err


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
    = Tint32 Int
    | Tint64 Int
    | Tfloat32 Float
    | Tfloat64 Float
    | Tall
    | TallInt32
    | TallInt64
    | TallFloat32
    | TallFloat64
    | Twrapper String Type


type alias Located a =
    { start : ( Int, Int )
    , value : a
    , file : String
    , end : ( Int, Int )
    }


type Mutability
    = Mutable
    | Immutable


type Def
    = WasmMut Type
    | WasmConst Type
    | Meta Atom


type alias EltState =
    { position : Position
    , defs : Dict.Dict String ( Position, Def )
    , metaStack : List Atom
    , wasmStack : List Type
    , defUse : Set.Set String
    , isHome : Bool
    , wrapperTypes : Dict.Dict String Type
    }


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


typeEndChecks : EltState -> Maybe String
typeEndChecks s =
    case endEmpty s of
        Nothing ->
            noUnusedNames s

        Just badEmpty ->
            Just badEmpty


noUnusedNames : EltState -> Maybe String
noUnusedNames s =
    let
        keys =
            Set.fromList <| Dict.keys s.defs

        standardKeys =
            Set.empty

        newKeys =
            Set.diff keys standardKeys

        unused =
            Set.diff newKeys s.defUse
    in
    case namesAndPositions unused s.defs of
        [] ->
            Nothing

        oneOrMore ->
            Just <| prettyUnused oneOrMore


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
                    if not <| checkStackEnd state.wasmStack se then
                        Err
                            { state = state
                            , message = badStackEnd se state.wasmStack
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

                Just ( _, WasmMut retrieved ) ->
                    Ok
                        { state
                            | wasmStack = retrieved :: state.wasmStack
                            , defUse = Set.insert v state.defUse
                        }

                Just ( _, WasmConst retrieved ) ->
                    Ok
                        { state
                            | wasmStack = retrieved :: state.wasmStack
                            , defUse = Set.insert v state.defUse
                        }

                Just ( _, Meta retrieved ) ->
                    Ok
                        { state
                            | metaStack = retrieved :: state.metaStack
                            , defUse = Set.insert v state.defUse
                        }

        Block bs ->
            Ok { state | metaStack = Block bs :: state.metaStack }

        MetaDefine newName ->
            case Dict.get newName state.defs of
                Just ( position, _ ) ->
                    Err { state = state, message = "\"" ++ newName ++ "\" is immutable and is already defined at " ++ prettyLocation position }

                Nothing ->
                    case state.metaStack of
                        [] ->
                            Err { state = state, message = "empty stack" }

                        s :: tack ->
                            Ok
                                { state
                                    | metaStack = tack
                                    , defs = Dict.insert newName ( state.position, Meta s ) state.defs
                                }

        AWasm (IupdateMutLocal name) ->
            case Dict.get name state.defs of
                Just ( position, WasmConst _ ) ->
                    Err { state = state, message = "\"" ++ name ++ "\" is immutable and is already defined at " ++ prettyLocation position }

                Just ( position, Meta _ ) ->
                    Err { state = state, message = "\"" ++ name ++ "\" is immutable and is already defined at " ++ prettyLocation position }

                Just ( position, WasmMut value ) ->
                    case state.wasmStack of
                        [] ->
                            Err { state = state, message = "empty stack" }

                        s :: tack ->
                            if isSubType s value then
                                Ok
                                    { state
                                        | wasmStack = tack
                                        , defs = Dict.insert name ( position, WasmMut s ) state.defs
                                    }

                            else
                                Err { state = state, message = "bad type: got \"" ++ showTypeVal s ++ "\", but expected \"" ++ showTypeVal value ++ "\"" }

                Nothing ->
                    Err { state = state, message = "\"" ++ name ++ "\" is not defined" }

        AWasm (IsetMutLocal basicType name) ->
            case Dict.get name state.defs of
                Just ( position, _ ) ->
                    Err { state = state, message = "\"" ++ name ++ "\" is immutable and is already defined at " ++ prettyLocation position }

                Nothing ->
                    case state.wasmStack of
                        [] ->
                            Err { state = state, message = "empty stack" }

                        s :: tack ->
                            if isSubType (fromBasic basicType) s then
                                Ok { state | wasmStack = tack, defs = Dict.insert name ( state.position, WasmMut s ) state.defs }

                            else
                                Err { state = state, message = "bad type declaration: " ++ " got \"" ++ showBasic basicType ++ "\", expected: \"" ++ showTypeVal s ++ "\"" }

        AWasm Ii32mul ->
            case state.wasmStack of
                [] ->
                    Err { state = state, message = "empty stack" }

                _ :: [] ->
                    Err { state = state, message = "only one thing in stack" }

                i1 :: i2 :: tack ->
                    case ( isSubType i1 TallInt32, isSubType i2 TallInt32 ) of
                        ( False, _ ) ->
                            Err { state = state, message = "Top item in stack should be a " ++ showTypeVal TallInt32 ++ ", but is a " ++ showTypeVal i1 }

                        ( _, False ) ->
                            Err { state = state, message = "Second item instack should be a " ++ showTypeVal TallInt32 ++ ", but is a " ++ showTypeVal i2 }

                        ( True, True ) ->
                            Ok
                                { state
                                    | wasmStack = TallInt32 :: tack
                                }

        AWasm (Ii32const i) ->
            Ok { state | wasmStack = Tint32 i :: state.wasmStack }

        AWasm (IsetConstLocal basicType name) ->
            case state.wasmStack of
                [] ->
                    Err { state = state, message = "empty stack" }

                value :: tack ->
                    if isSubType (fromBasic basicType) value then
                        Ok
                            { state
                                | defs = Dict.insert name ( state.position, WasmConst value ) state.defs
                                , wasmStack = tack
                            }

                    else
                        Err { state = state, message = "bad type declaration: " ++ " got \"" ++ showBasic basicType ++ "\", expected: \"" ++ showTypeVal value ++ "\"" }

        AWasm (IgetLocal name) ->
            case Dict.get name state.defs of
                Nothing ->
                    Err { state = state, message = "\"" ++ name ++ "\" is not defined" }

                Just ( _, WasmMut value ) ->
                    Ok { state | wasmStack = value :: state.wasmStack }

                Just ( _, WasmConst value ) ->
                    Ok { state | wasmStack = value :: state.wasmStack }

                Just ( position, Meta _ ) ->
                    Err { state = state, message = "\"" ++ name ++ "\" is defined at " ++ prettyLocation position ++ ", but it is a meta definition, not a run-time definition" }

        AWasm Ii32store8 ->
            case state.wasmStack of
                [] ->
                    Err { state = state, message = "empty stack" }

                _ :: [] ->
                    Err { state = state, message = "only one thing on stack" }

                i1 :: i2 :: tack ->
                    case ( isSubType i1 TallInt32, isSubType i2 TallInt32 ) of
                        ( False, _ ) ->
                            Err { state = state, message = "Top item in stack should be a " ++ showTypeVal TallInt32 ++ ", but is a " ++ showTypeVal i1 }

                        ( _, False ) ->
                            Err { state = state, message = "Second item instack should be a " ++ showTypeVal TallInt32 ++ ", but is a " ++ showTypeVal i2 }

                        ( True, True ) ->
                            Ok { state | wasmStack = tack }

        Loop ->
            loopHelp state

        Runblock ->
            case state.metaStack of
                [] ->
                    Err { state = state, message = "empty stack" }

                (Block block) :: tack ->
                    runTypeChecksHelp Nothing block { state | metaStack = tack }

                _ ->
                    Err { message = "there's nothing to run", state = state }

        Export exported ->
            if Dict.member exported state.defs then
                Ok { state | defUse = Set.insert exported state.defUse }

            else
                Err { message = "\"" ++ exported ++ "\" is not defined", state = state }

        IfElse ->
            ifElseTypeHelp state

        TypeWrap type_ ->
            case ( state.wasmStack, Dict.get type_ state.wrapperTypes ) of
                ( [], _ ) ->
                    Err { state = state, message = "empty stack" }

                ( contained :: tack, Nothing ) ->
                    Ok
                        { state
                            | wrapperTypes = Dict.insert type_ contained state.wrapperTypes
                            , wasmStack = Twrapper type_ contained :: tack
                        }

                ( contained :: tack, Just previouslyWrapped ) ->
                    if isSubType contained previouslyWrapped then
                        Ok { state | wasmStack = Twrapper type_ contained :: tack }

                    else
                        Err { state = state, message = "type \"" ++ showTypeVal contained ++ "\" is not compatible with \"" ++ type_ ++ "\"" }

        TypeUnwrap type_ ->
            case state.wasmStack of
                [] ->
                    Err { state = state, message = "empty stack" }

                (Twrapper wrapper wrapped) :: tack ->
                    if type_ == wrapper then
                        Ok { state | wasmStack = wrapped :: tack }

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


loopHelp : EltState -> EltOut
loopHelp state =
    case state.metaStack of
        [] ->
            Err { state = state, message = "empty stack" }

        (Block body) :: (Block break) :: tack ->
            let
                cleanStack =
                    { state | wasmStack = [] }

                bodyEnd =
                    runTypeChecksHelp (Just []) body cleanStack

                breakEnd =
                    runTypeChecksHelp (Just [ TallInt32 ]) break cleanStack
            in
            case ( bodyEnd, breakEnd ) of
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

                ( Ok _, Ok _ ) ->
                    Ok state

        _ ->
            Err { message = "there's nothing to run", state = state }


ifElseTypeHelp : EltState -> EltOut
ifElseTypeHelp state =
    case state.metaStack of
        (Block else_) :: (Block if_) :: (Block switch) :: metatack ->
            let
                cleanStack =
                    { state | wasmStack = [] }

                elseEnd =
                    runTypeChecksHelp (Just []) else_ cleanStack

                ifEnd =
                    runTypeChecksHelp (Just []) if_ cleanStack

                switchEnd =
                    runTypeChecksHelp (Just [ TallInt32 ]) switch cleanStack
            in
            case ( elseEnd, ifEnd, switchEnd ) of
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

                ( Ok _, Ok _, Ok _ ) ->
                    Ok state

        bad ->
            Err
                { message =
                    String.concat
                        [ "bad stack: "
                        , showAtoms bad
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


endEmpty : EltState -> Maybe String
endEmpty s =
    case s.wasmStack of
        [] ->
            Nothing

        ts ->
            Just <|
                String.concat
                    [ "typestack should be empty at end of program, but got "
                    , showTypeStack ts
                    ]


prettyUnused : List ( String, Position ) -> String
prettyUnused unused =
    String.join ", " <| List.map onePrettyUnused unused


onePrettyUnused : ( String, Position ) -> String
onePrettyUnused ( name, position ) =
    String.concat
        [ "\""
        , name
        , "\" defined but not used: \n"
        , prettyLocation position
        ]


namesAndPositions : Set.Set String -> Dict.Dict String ( Position, Def ) -> List ( String, Position )
namesAndPositions unused positions =
    Utils.justs <| List.map (makePosition positions) <| Set.toList unused


makePosition : Dict.Dict String ( Position, Def ) -> String -> Maybe ( String, Position )
makePosition positions name =
    case Dict.get name positions of
        Nothing ->
            Nothing

        Just ( position, _ ) ->
            Just ( name, position )


type alias EltOut =
    Result TypeError EltState


showTypeVal : Type -> String
showTypeVal type_ =
    case type_ of
        Tint32 i ->
            "int32: " ++ String.fromInt i

        Tall ->
            "all"

        TallInt32 ->
            "int32"

        Tint64 i ->
            "int64: " ++ String.fromInt i

        Tfloat32 f ->
            "float32: " ++ String.fromFloat f

        Tfloat64 f ->
            "float64: " ++ String.fromFloat f

        TallInt64 ->
            "int64"

        TallFloat32 ->
            "float32"

        TallFloat64 ->
            "float64"

        Twrapper wrapper wrapped ->
            wrapper ++ "<" ++ showTypeVal wrapped ++ ">"


isSubType : Type -> Type -> Bool
isSubType sub master =
    case ( sub, master ) of
        -- Tall
        ( _, Tall ) ->
            True

        ( Tall, _ ) ->
            False

        -- Tint32
        ( Tint32 _, TallInt32 ) ->
            True

        ( Tint32 i1, Tint32 i2 ) ->
            i1 == i2

        ( Tint32 _, _ ) ->
            False

        ( TallInt32, TallInt32 ) ->
            True

        ( TallInt32, _ ) ->
            False

        -- Tint64
        ( Tint64 _, TallInt64 ) ->
            True

        ( Tint64 i1, Tint64 i2 ) ->
            i1 == i2

        ( Tint64 _, _ ) ->
            False

        ( TallInt64, TallInt64 ) ->
            True

        ( TallInt64, _ ) ->
            False

        -- Tfloat32
        ( Tfloat32 _, TallFloat32 ) ->
            True

        ( Tfloat32 f1, Tfloat32 f2 ) ->
            f1 == f2

        ( Tfloat32 _, _ ) ->
            False

        ( TallFloat32, TallFloat32 ) ->
            True

        ( TallFloat32, _ ) ->
            False

        -- Tfloat64
        ( Tfloat64 _, TallFloat64 ) ->
            True

        ( Tfloat64 f1, Tfloat64 f2 ) ->
            f1 == f2

        ( Tfloat64 _, _ ) ->
            False

        ( TallFloat64, TallFloat64 ) ->
            True

        ( TallFloat64, _ ) ->
            False

        ( Twrapper t1 _, Twrapper t2 _ ) ->
            t1 == t2

        ( Twrapper _ _, _ ) ->
            False


elementP : List String -> String -> P.Parser Atom
elementP modules moduleName =
    P.oneOf
        [ runBlockP
        , exportP
        , plainP
        , retrieveP
        , metaDefP
        , programBlockP moduleName modules
        , int32P
        , typeWrapP
        , typeUnwrapP
        ]


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


int32P : P.Parser Atom
int32P =
    P.succeed (AWasm << Ii32const)
        |= intHelpP
        |. P.keyword "i32"


intP : (Int -> Atom) -> P.Parser Atom
intP toAtom =
    P.succeed toAtom
        |= intHelpP


intHelpP : P.Parser Int
intHelpP =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |= P.int
        , P.int
        ]


plainP : P.Parser Atom
plainP =
    P.oneOf <|
        List.map plainHelpP
            [ ( "loop", Loop )
            , ( "ifelse", IfElse )
            , ( "i32mul", AWasm Ii32mul )
            , ( "UNSAFE_i32store8", AWasm Ii32store8 )
            ]


plainHelpP : ( String, Atom ) -> P.Parser Atom
plainHelpP ( string, atom ) =
    P.succeed atom |. P.keyword string


loopP : P.Parser Atom
loopP =
    P.succeed Loop |. P.keyword "loop"


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
    String.concat
        [ "<"
        , String.join ", " <| List.map showTypeVal typestack
        , ">"
        ]



-- showProgVal : ProgVal -> String
-- showProgVal p =
--     case p of
--         Pint32 i ->
--             "int32: " ++ String.fromInt i
--
--         Pall ->
--             "all"
--
--         PallInt32 ->
--             "int32"
--
--         Pint64 i ->
--             "int64: " ++ String.fromInt i
--
--         Pfloat32 f ->
--             "float32: " ++ String.fromFloat f
--
--         Pfloat64 f ->
--             "float64: " ++ String.fromFloat f
--
--         PallInt64 ->
--             "int64"
--
--         PallFloat32 ->
--             "float32"
--
--         PallFloat64 ->
--             "float64"


programBlockP : String -> List String -> P.Parser Atom
programBlockP moduleName modules =
    P.succeed Block
        |. P.token "{"
        |= parseP moduleName modules
        |. P.token "}"


metaDefP : P.Parser Atom
metaDefP =
    P.succeed MetaDefine
        |. P.token "="
        |. whiteSpaceP
        |= variable


wasmP : P.Parser Atom
wasmP =
    P.map AWasm wasmHelpP


wasmHelpP : P.Parser WasmIn
wasmHelpP =
    P.oneOf <| List.map wasmHelpHelpP wasmWords


wasmHelpHelpP : ( String, WasmIn ) -> P.Parser WasmIn
wasmHelpHelpP ( literal, wasm ) =
    P.succeed wasm |. P.keyword literal


wasmWords : List ( String, WasmIn )
wasmWords =
    [ ( "i32.mul", Ii32mul )
    ]


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
        Retrieve string ->
            "Retrieve " ++ showString string

        Block atoms ->
            "Block {" ++ String.join " " (List.map (showAtom << .value) atoms) ++ "}"

        MetaDefine string ->
            "MetaDefine " ++ string

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


showWasm : WasmIn -> String
showWasm wasm =
    case wasm of
        Ii32mul ->
            "i32.mul"

        Ii32const i ->
            "i32.const " ++ String.fromInt i

        Ii32store8 ->
            "i32.store8"

        IsetConstLocal basicType name ->
            String.concat
                [ "local $" ++ name ++ " " ++ showBasic basicType ++ "\n"
                , "set_local $" ++ name ++ "\n"
                ]

        IsetMutLocal basicType name ->
            String.concat
                [ "local $" ++ name ++ " " ++ showBasic basicType ++ "\n"
                , "set_local $" ++ name ++ "\n"
                ]

        IupdateMutLocal name ->
            "set_local $" ++ name

        IgetLocal name ->
            "get_local $" ++ name


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

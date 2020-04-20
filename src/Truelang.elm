module Truelang exposing (compile)

import Dict
import Maybe.Extra
import Parser as P exposing ((|.), (|=))
import Result.Extra
import Set
import Utils


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
    | Define String
    | Runblock
    | Export String
    | AWasm Wasm


type alias WasmState =
    { defs : Dict.Dict String Atom
    , wasmStack : List Wasm
    , error : Maybe String
    , metaStack : List Atom
    }


type Wasm
    = I32mul


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
            Ok <| wasmsToString result.wasmStack

        Just error ->
            Err error


wasmsToString : List Wasm -> String
wasmsToString wasms =
    String.join " " <| List.map wasmToString wasms


wasmToString : Wasm -> String
wasmToString wasm =
    case wasm of
        I32mul ->
            "i32.mul"


makeWasmHelp : WasmState -> List (Located Atom) -> WasmState
makeWasmHelp wasmState atoms =
    List.foldr makeOneWasm wasmState atoms


makeOneWasm : Located Atom -> WasmState -> WasmState
makeOneWasm { value } accum =
    case value of
        AWasm I32mul ->
            { accum | wasmStack = I32mul :: accum.wasmStack }

        Retrieve name ->
            case Dict.get name accum.defs of
                Nothing ->
                    { accum | error = Just <| "could not find \"" ++ name ++ "\"" }

                Just retrieved ->
                    { accum | metaStack = retrieved :: accum.metaStack }

        Block block ->
            { accum | metaStack = Block block :: accum.metaStack }

        Define name ->
            case accum.metaStack of
                [] ->
                    { accum | error = Just <| "nothing on stack to define as name \"" ++ name ++ "\"" }

                s :: tack ->
                    { accum
                        | defs = Dict.insert name s accum.defs
                        , metaStack = tack
                    }

        Runblock ->
            case accum.metaStack of
                [] ->
                    { accum | error = Just <| "nothing on stack to run" }

                (Block block) :: tack ->
                    makeWasmHelp { accum | metaStack = tack } block

                other :: _ ->
                    { accum | error = Just <| "top of stack is not a block, it is: " ++ showAtom other }

        Export _ ->
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
    , stack = []
    , defs = standardTypes
    , defPos = Dict.empty
    , defUse = Set.empty
    , isHome = True
    }


runTypeChecks : List (Located Atom) -> EltState -> Maybe String
runTypeChecks atoms init =
    case runTypeChecksHelp atoms init of
        Ok endState ->
            typeEndChecks endState

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


type ProgVal
    = Pstring String
    | Ptuple (List ProgVal)
    | Pint Int
    | Pblock (List (Located Atom))
    | Plist (List ProgVal)
    | PallLists
    | Ptype Type
    | PallInts
    | PallStrings
    | PallBlocks
    | PallTuples
    | PsomeTuples (List Type)
    | Pall
    | Pprint
    | Pcons
    | PmakeTuple
    | PtypeOf
    | Pswitch
    | Ploop
    | Pplus
    | Ppop
    | Pbool Bool
    | Pequal
    | PcatString
    | PallI32


type alias Type =
    List ProgVal


type alias Located a =
    { start : ( Int, Int )
    , value : a
    , file : String
    , end : ( Int, Int )
    }


standardTypes : Dict.Dict String Type
standardTypes =
    Dict.fromList
        [ ( "[]", [ Plist [] ] )
        , ( "int", [ PallInts ] )
        , ( "string", [ PallStrings ] )
        , ( "testForSwitch", [ Pint 1, Pint 2 ] )
        , ( "print", [ Pprint ] )
        , ( "emptylist", [ Plist [] ] )
        , ( "cons", [ Pcons ] )
        , ( "maketuple", [ PmakeTuple ] )
        , ( "typeof", [ PtypeOf ] )
        , ( "switch", [ Pswitch ] )
        , ( "loop", [ Ploop ] )
        , ( "+", [ Pplus ] )
        , ( "pop", [ Ppop ] )
        , ( "true", [ Pbool True ] )
        , ( "false", [ Pbool False ] )
        , ( "==", [ Pequal ] )
        , ( "counter", [ PallInts ] )
        , ( "runloop", [ Pbool True, Pbool False ] )
        , ( "catstrings", [ PcatString ] )
        ]


type alias EltState =
    { position : Position
    , defs : Dict.Dict String Type
    , stack : List Type
    , defPos : Dict.Dict String Position
    , defUse : Set.Set String
    , isHome : Bool
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
            Set.fromList <| Dict.keys standardTypes

        newKeys =
            Set.diff keys standardKeys

        unused =
            Set.diff newKeys s.defUse
    in
    case namesAndPositions unused s.defPos of
        [] ->
            Nothing

        oneOrMore ->
            Just <| prettyUnused oneOrMore


runTypeChecksHelp : List (Located Atom) -> EltState -> EltOut
runTypeChecksHelp atoms state =
    case atoms of
        [] ->
            Ok state

        a :: toms ->
            case processAtom { state | position = { start = a.start, end = a.end, file = a.file } } a.value of
                Err err ->
                    Err err

                Ok ok ->
                    runTypeChecksHelp toms ok


processAtom : EltState -> Atom -> EltOut
processAtom state atom =
    case atom of
        Retrieve v ->
            case Dict.get v state.defs of
                Nothing ->
                    Err { state = state, message = "no definition \"" ++ v ++ "\"" }

                Just retrieved ->
                    Ok
                        { state
                            | stack = retrieved :: state.stack
                            , defUse = Set.insert v state.defUse
                        }

        Block bs ->
            Ok { state | stack = [ Pblock bs ] :: state.stack }

        Define newName ->
            case Dict.get newName state.defPos of
                Just position ->
                    Err { state = state, message = "\"" ++ newName ++ "\" is already defined at " ++ prettyLocation position }

                Nothing ->
                    case state.stack of
                        [] ->
                            Err { state = state, message = "empty stack" }

                        s :: tack ->
                            Ok
                                { state
                                    | stack = tack
                                    , defs = Dict.insert newName s state.defs
                                    , defPos = Dict.insert newName state.position state.defPos
                                }

        AWasm I32mul ->
            case state.stack of
                [] ->
                    Err { state = state, message = "empty stack" }

                _ :: [] ->
                    Err { state = state, message = "only one thing in stack" }

                i1 :: i2 :: tack ->
                    case ( isSubType i1 [ PallI32 ], isSubType i2 [ PallI32 ] ) of
                        ( False, _ ) ->
                            Err { state = state, message = "Top item instack should be a " ++ showTypeVal [ PallI32 ] ++ ", but is a " ++ showTypeVal i1 }

                        ( _, False ) ->
                            Err { state = state, message = "Second item instack should be a " ++ showTypeVal [ PallI32 ] ++ ", but is a " ++ showTypeVal i2 }

                        ( True, True ) ->
                            Ok { state | stack = tack }

        Runblock ->
            case state.stack of
                [] ->
                    Err { state = state, message = "empty stack" }

                ((Pblock b1) :: bs) :: tack ->
                    case getBlocks (Pblock b1 :: bs) of
                        Nothing ->
                            Err { message = "expecting a block, but got " ++ showTypeVal (Pblock b1 :: bs), state = state }

                        Just blocks ->
                            case Result.Extra.combine <| List.map (\b -> runTypeChecksHelp b { state | stack = tack }) blocks of
                                Err err ->
                                    Err { err | message = "error inside block called at " ++ prettyLocation state.position ++ ": " ++ err.message }

                                Ok results ->
                                    let
                                        allNames =
                                            List.foldr Set.union Set.empty (List.map (Set.fromList << Dict.keys << .defPos) results)

                                        oldNames =
                                            Set.fromList <| Dict.keys state.defPos

                                        newNames =
                                            Set.diff allNames oldNames

                                        newUnused =
                                            Set.diff newNames combinedUsed

                                        stacksAsTuples =
                                            List.map (\s -> [ PsomeTuples s.stack ]) results

                                        combinedType =
                                            typeListUnion stacksAsTuples

                                        combinedPositions =
                                            List.foldr Dict.union Dict.empty (List.map .defPos results)

                                        combinedUsed =
                                            List.foldr Set.union Set.empty (List.map .defUse results)
                                    in
                                    if Set.isEmpty newUnused then
                                        case combinedType of
                                            [ PsomeTuples ts ] ->
                                                Ok { state | stack = ts, defUse = combinedUsed }

                                            _ ->
                                                Err { state = state, message = "internal error in type checker: could not convert stacks to tuples" }

                                    else
                                        Err { state = state, message = prettyUnusedInBlock newUnused combinedPositions }

                [ PcatString ] :: tack ->
                    case tack of
                        stringsCandidate :: ack ->
                            if isSubType stringsCandidate [ Plist [ PallStrings ] ] then
                                Ok { state | stack = [ PallStrings ] :: ack }

                            else
                                Err { message = "expecting a list of strings, but got " ++ showTypeVal stringsCandidate, state = state }

                        [] ->
                            Err { message = "empty stack", state = state }

                [ Pprint ] :: tack ->
                    case tack of
                        stringCandidate :: ack ->
                            if isSubType stringCandidate [ PallStrings ] then
                                Ok { state | stack = ack }

                            else
                                Err { message = "expecting a string but got " ++ showTypeVal stringCandidate, state = state }

                        [] ->
                            Err { message = "empty stack", state = state }

                [ Pcons ] :: tack ->
                    case tack of
                        toAdd :: toAddTo :: remainsOfStack ->
                            case listTypeCons toAdd toAddTo of
                                Nothing ->
                                    Err { message = "the second thing on the stack must be a list", state = state }

                                Just newListType ->
                                    Ok { state | stack = newListType :: remainsOfStack }

                        _ ->
                            Err { message = "the top of the stack should be the thing to add, and the second thing should be the list to add to", state = state }

                [ PmakeTuple ] :: tack ->
                    case tack of
                        [ Pint i ] :: remainsOfStack ->
                            if List.length remainsOfStack < i then
                                Err { state = state, message = "stack too short" }

                            else
                                Ok { state | stack = [ PsomeTuples (List.take i remainsOfStack) ] :: List.drop i remainsOfStack }

                        _ ->
                            Err { state = state, message = "no integer on top of stack" }

                [ PtypeOf ] :: tack ->
                    case tack of
                        t :: ack ->
                            Ok { state | stack = [ Ptype t ] :: ack }

                        _ ->
                            Err { state = state, message = "empty stack" }

                [ Pswitch ] :: tack ->
                    case tack of
                        [] ->
                            Err { message = "empty stack", state = state }

                        _ :: [] ->
                            Err { message = "only one thing in stack", state = state }

                        pathsCandidate :: toSwitchOn :: remainsOfStack ->
                            case extractAndCheckPaths pathsCandidate toSwitchOn of
                                Err err ->
                                    Err { message = err, state = state }

                                Ok routes ->
                                    Ok { state | stack = typeListUnion routes :: remainsOfStack }

                [ Ploop ] :: tack ->
                    case tack of
                        [] ->
                            Err { message = "empty stack", state = state }

                        _ :: [] ->
                            Err { message = "only one thing in stack", state = state }

                        maybeBlocks :: maybeKeepRunning :: restOfStack ->
                            if isSubType maybeKeepRunning [ Pbool True, Pbool False ] then
                                case checkBlocks maybeBlocks { state | stack = restOfStack } of
                                    Ok newUsed ->
                                        Ok { state | stack = restOfStack, defUse = newUsed }

                                    Err err ->
                                        Err err

                            else
                                Err { message = "second item in the stack must be a 1 or 0", state = state }

                [ Ppop ] :: tack ->
                    case tack of
                        _ :: ack ->
                            Ok { state | stack = ack }

                        _ ->
                            Err { message = "empty stack", state = state }

                [ Pplus ] :: tack ->
                    case tack of
                        [] ->
                            Err { message = "empty stack", state = state }

                        _ :: [] ->
                            Err { message = "only one thing on stack", state = state }

                        [ Pint n1 ] :: [ Pint n2 ] :: remainder ->
                            Ok { state | stack = [ Pint (n1 + n2) ] :: remainder }

                        maybeN1 :: maybeN2 :: remainder ->
                            if isSubType maybeN1 [ PallInts ] then
                                if isSubType maybeN2 [ PallInts ] then
                                    Ok { state | stack = [ PallInts ] :: remainder }

                                else
                                    Err { message = "the second thing on the stack is not an integer, it is a " ++ showTypeVal maybeN2, state = state }

                            else
                                Err { message = "the top thing on the stack is not an integer, it is a " ++ showTypeVal maybeN2, state = state }

                [ Pequal ] :: tack ->
                    case tack of
                        [] ->
                            Err { message = "empty stack", state = state }

                        _ :: [] ->
                            Err { message = "only one thing on stack", state = state }

                        _ :: _ :: ck ->
                            Ok { state | stack = [ Pbool True, Pbool False ] :: ck }

                _ ->
                    Err { message = "there's nothing to run", state = state }

        Export exported ->
            if Dict.member exported state.defPos then
                Ok { state | defUse = Set.insert exported state.defUse }

            else
                Err { message = "\"" ++ exported ++ "\" is not defined", state = state }


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
    case s.stack of
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


namesAndPositions : Set.Set String -> Dict.Dict String Position -> List ( String, Position )
namesAndPositions unused positions =
    Utils.justs <| List.map (makePosition positions) <| Set.toList unused


makePosition : Dict.Dict String Position -> String -> Maybe ( String, Position )
makePosition positions name =
    case Dict.get name positions of
        Nothing ->
            Nothing

        Just position ->
            Just ( name, position )


type alias EltOut =
    Result TypeError EltState


showTypeVal : Type -> String
showTypeVal type_ =
    String.concat
        [ "["
        , String.join ", " <| List.map showProgVal type_
        , "]"
        ]


isSubType : Type -> Type -> Bool
isSubType sub master =
    List.all (matchesType master) sub


matchesType : Type -> ProgVal -> Bool
matchesType master sub =
    List.any (isContained sub) master


isContained : ProgVal -> ProgVal -> Bool
isContained sub master =
    sub
        == master
        || master
        == Pall
        || (case ( sub, master ) of
                ( Pstring _, PallStrings ) ->
                    True

                ( Pint _, PallInts ) ->
                    True

                ( Pblock _, PallBlocks ) ->
                    True

                ( Plist _, PallLists ) ->
                    True

                ( Plist subList, Plist masterList ) ->
                    isSubType subList masterList

                _ ->
                    False
           )


checkBlocks : Type -> EltState -> Result TypeError (Set.Set String)
checkBlocks maybeBlocks oldState =
    case getBlocks maybeBlocks of
        Nothing ->
            Err { message = "not only a block", state = oldState }

        Just blocks ->
            case Result.Extra.combine <| List.map (\b -> runTypeChecksHelp b oldState) blocks of
                Err err ->
                    Err err

                Ok results ->
                    let
                        stacksAsTuples =
                            List.map (\s -> [ PsomeTuples s.stack ]) results

                        combinedType =
                            typeListUnion stacksAsTuples

                        combinedUsed =
                            List.foldr Set.union Set.empty (List.map .defUse results)
                    in
                    case combinedType of
                        [ PsomeTuples ts ] ->
                            let
                                oldStack =
                                    [ Pbool True, Pbool False ] :: oldState.stack
                            in
                            if equalStacks ts oldStack then
                                Ok combinedUsed

                            else
                                Err { message = "Bad loop block changed the stack. Old stack: " ++ showTypeStack oldStack ++ "\nNew stack: " ++ showTypeStack ts, state = oldState }

                        _ ->
                            Err { state = oldState, message = "internal error in type checker: could not convert stacks to tuples" }


equalStacks : List Type -> List Type -> Bool
equalStacks s1 s2 =
    if List.length s1 /= List.length s2 then
        False

    else
        List.all equalTypes <| zip s1 s2


equalTypes : ( Type, Type ) -> Bool
equalTypes ( t1, t2 ) =
    isSubType t1 t2 && isSubType t2 t1


zip : List a -> List b -> List ( a, b )
zip l1 l2 =
    List.map2 (\a b -> ( a, b )) l1 l2


typeListUnion : List Type -> Type
typeListUnion types =
    typeListUnionHelp types []


typeListUnionHelp : List Type -> Type -> Type
typeListUnionHelp notLookedAtYet accumulator =
    case notLookedAtYet of
        [] ->
            accumulator

        topType :: remainder ->
            typeListUnionHelp remainder (typeUnion topType accumulator)


deduplicate : Type -> Type
deduplicate t =
    let
        result =
            List.foldr dedupHelp [] t
    in
    result


dedupHelp : ProgVal -> Type -> Type
dedupHelp v accum =
    if List.member v accum then
        accum

    else
        v :: accum


extractAndCheckPaths : Type -> Type -> Result String (List Type)
extractAndCheckPaths pathsCandidate toSwitchOn =
    case pathsCandidate of
        [ Plist pl ] ->
            case Result.Extra.combine <| List.map toPath pl of
                Err err ->
                    Err err

                Ok paths ->
                    let
                        ( missingPaths, surplusPaths ) =
                            checkPathsComplete paths toSwitchOn
                    in
                    if List.isEmpty missingPaths then
                        if List.isEmpty surplusPaths then
                            Ok <| List.map Tuple.second paths

                        else
                            Err <| "too many paths: " ++ showTypeVal surplusPaths

                    else
                        Err <| "not enough paths: " ++ showTypeVal missingPaths

        _ ->
            Err "paths are not a list"


listTypeCons : Type -> Type -> Maybe Type
listTypeCons toAdd toAddTo =
    case toAddTo of
        [ PallLists ] ->
            Just [ PallLists ]

        [ Plist values ] ->
            Just [ Plist <| typeUnion toAdd values ]

        _ ->
            Nothing


prettyUnusedInBlock : Set.Set String -> Dict.Dict String Position -> String
prettyUnusedInBlock names positions =
    let
        help : String -> String
        help name =
            case Dict.get name positions of
                Nothing ->
                    "internal error: cannot find position of name \"" ++ name ++ "\""

                Just position ->
                    onePrettyUnused ( name, position )
    in
    String.join ", " <| List.map help <| Set.toList names


getBlocks : Type -> Maybe (List (List (Located Atom)))
getBlocks type_ =
    Maybe.Extra.combine <| List.map getBlock type_


getBlock : ProgVal -> Maybe (List (Located Atom))
getBlock v =
    case v of
        Pblock bs ->
            Just bs

        _ ->
            Nothing


elementP : List String -> String -> P.Parser Atom
elementP modules moduleName =
    P.oneOf
        [ runBlockP
        , exportP
        , retrieveP
        , defP
        , wasmP
        , programBlockP moduleName modules
        ]


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


showProgVal : ProgVal -> String
showProgVal p =
    case p of
        Pstring s ->
            "string: " ++ s

        Pint i ->
            "int: " ++ String.fromInt i

        Pblock bs ->
            "block: {" ++ String.join ", " (List.map (showAtom << .value) bs) ++ "}"

        Plist l ->
            String.concat
                [ "list ["
                , String.join ", " <| List.map showProgVal l
                , "]"
                ]

        Ptype type_ ->
            "type: " ++ showTypeVal type_

        Ptuple ts ->
            "tuple: " ++ String.join ", " (List.map showProgVal ts)

        PallInts ->
            "int"

        PallStrings ->
            "string"

        PallBlocks ->
            "block"

        Pall ->
            "all"

        PallTuples ->
            "tuple"

        PsomeTuples ts ->
            "tuple: (" ++ String.join ", " (List.map showTypeVal ts) ++ ")"

        Pprint ->
            "print"

        PallLists ->
            "list"

        Pcons ->
            "cons"

        PmakeTuple ->
            "maketuple"

        PtypeOf ->
            "typeof"

        Pswitch ->
            "switch"

        Ploop ->
            "loop"

        Pplus ->
            "plus"

        Ppop ->
            "pop"

        Pbool True ->
            "bool: true"

        Pbool False ->
            "bool: false"

        Pequal ->
            "=="

        PcatString ->
            "catstring"

        PallI32 ->
            "int32"


typeUnion : Type -> Type -> Type
typeUnion t1 t2 =
    case ( t1, t2 ) of
        ( [ PsomeTuples t1s ], [ PsomeTuples t2s ] ) ->
            [ PsomeTuples <| List.map2 typeUnion t1s t2s ]

        ( [ Ptype p1 ], [ Ptype p2 ] ) ->
            [ Ptype (p1 ++ p2) ]

        _ ->
            deduplicate <| t1 ++ t2


checkPathsComplete : List ( Type, Type ) -> Type -> ( Type, Type )
checkPathsComplete paths value =
    let
        combinedPaths =
            typeListUnion <| List.map Tuple.first paths

        missingPaths =
            typeDiff value combinedPaths

        surplusPaths =
            typeDiff combinedPaths value
    in
    ( missingPaths, surplusPaths )


toPath : ProgVal -> Result String ( Type, Type )
toPath candidate =
    case candidate of
        PsomeTuples [ value, [ Ptype toMatch ] ] ->
            Ok ( toMatch, value )

        _ ->
            Err <| "not a path: " ++ showProgVal candidate


programBlockP : String -> List String -> P.Parser Atom
programBlockP moduleName modules =
    P.succeed Block
        |. P.token "{"
        |= parseP moduleName modules
        |. P.token "}"


defP : P.Parser Atom
defP =
    P.succeed Define
        |. P.token "="
        |. whiteSpaceP
        |= variable


wasmP : P.Parser Atom
wasmP =
    P.map AWasm wasmHelpP


wasmHelpP : P.Parser Wasm
wasmHelpP =
    P.oneOf <| List.map wasmHelpHelpP wasmWords


wasmHelpHelpP : ( String, Wasm ) -> P.Parser Wasm
wasmHelpHelpP ( literal, wasm ) =
    P.succeed wasm |. P.keyword literal


wasmWords : List ( String, Wasm )
wasmWords =
    [ ( "i32.mul", I32mul )
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

        Define string ->
            "Define " ++ string

        Runblock ->
            "Runblock"

        Export s ->
            "Export " ++ s

        AWasm wasm ->
            "WASM: " ++ showWasm wasm


showWasm : Wasm -> String
showWasm wasm =
    case wasm of
        I32mul ->
            "i32.mul"


typeDiff : Type -> Type -> Type
typeDiff fromThis subtractThis =
    List.filter (\x -> not <| isSubType [ x ] subtractThis) fromThis


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

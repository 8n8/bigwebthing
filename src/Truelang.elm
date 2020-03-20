module Truelang exposing (runProgram)

import Dict
import Maybe.Extra
import Parser as P exposing ((|.), (|=))
import Result.Extra
import SHA256
import Set
import Utils


runProgram :
    String
    -> Dict.Dict String Utils.Program
    -> Int
    -> ( Maybe Utils.Document, List Utils.HumanMsg )
runProgram programName allPrograms myName =
    case Dict.get programName allPrograms of
        Nothing ->
            ( Just <| Utils.SmallString <| "internal error: no such program \"" ++ programName ++ "\"", [] )

        Just program ->
            case P.run (topProgramP (hash program.code) allPrograms) program.code of
                Err deadEnds ->
                    ( Just <| Utils.SmallString <| deadEndsToString deadEnds
                    , []
                    )

                Ok atoms ->
                    case runTypeChecks atoms { initEltState | position = { start = initEltState.position.start, end = initEltState.position.end, file = hash program.code } } of
                        Just errMsg ->
                            ( Just <| Utils.SmallString ("type error: " ++ errMsg), [] )

                        Nothing ->
                            runElfs allPrograms program atoms [] myName


runElfs :
    Dict.Dict String Utils.Program
    -> Utils.Program
    -> List (Located Atom)
    -> List ProgVal
    -> Int
    -> ( Maybe Utils.Document, List Utils.HumanMsg )
runElfs allPrograms program elfs progStack myName =
    let
        oldP =
            { program = program
            , defs = standardLibrary
            , stack = progStack
            , rightDoc = Nothing
            , outbox = []
            , internalError = Nothing
            , allPrograms = allPrograms
            , myName = myName
            }

        newP =
            runElfsHelp elfs oldP
    in
    case newP.internalError of
        Nothing ->
            ( newP.rightDoc, newP.outbox )

        Just err ->
            ( Just <| Utils.SmallString ("internal error: " ++ err), oldP.outbox )


initEltState =
    { position = { start = ( 0, 0 ), end = ( 0, 0 ), file = "" }
    , stack = []
    , defs = standardTypes
    , defPos = Dict.empty
    , defUse = Set.empty
    , typeDefPos = Dict.empty
    , typeDefUse = Set.empty
    , typeDefs = standardTypeProgramDefs
    , typeStack = []
    , isHome = True
    }


hash : String -> String
hash s =
    SHA256.toBase64 <| SHA256.fromString s


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


topProgramP : String -> Dict.Dict String Utils.Program -> P.Parser (List (Located Atom))
topProgramP filename programs =
    P.succeed identity
        |= programP filename programs
        |. P.end


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
    | PlistPrograms
    | PcatString
    | PsendMessage


type alias Type =
    List ProgVal


type alias Located a =
    { start : ( Int, Int )
    , value : a
    , file : String
    , end : ( Int, Int )
    }


type Atom
    = Retrieve String
    | Block (List (Located Atom))
    | Define String
    | Runblock
    | TypeLanguage (List (Located TlAtom))
    | StringLiteral String
    | IntegerLiteral Int
    | Export String


type TlAtom
    = TlRetrieve String
    | TlBlock (List (Located TlAtom))
    | TlDefine String
    | TlRunblock
    | TlStringLiteral String
    | TlIntegerLiteral Int


standardLibrary : Dict.Dict String ProgVal
standardLibrary =
    Dict.fromList
        [ ( "[]", Plist [] )
        , ( "testForSwitch", Pint 2 )
        , ( "print", Pprint )
        , ( "emptylist", Plist [] )
        , ( "cons", Pcons )
        , ( "maketuple", PmakeTuple )
        , ( "typeof", PtypeOf )
        , ( "switch", Pswitch )
        , ( "loop", Ploop )
        , ( "counter", Pint 0 )
        , ( "runloop", Pbool True )
        , ( "+", Pplus )
        , ( "pop", Ppop )
        , ( "==", Pequal )
        , ( "true", Pbool True )
        , ( "false", Pbool False )
        , ( "listprograms", PlistPrograms )
        , ( "catstrings", PcatString )
        , ( "sendmessage", PsendMessage )
        ]


runElfsHelp :
    List (Located Atom)
    -> ProgramState
    -> ProgramState
runElfsHelp atoms s =
    case atoms of
        [] ->
            s

        a :: toms ->
            runElfsHelp toms (programProcessAtom a s)


standardTypeProgramDefs : Dict.Dict String TypeProgramValue
standardTypeProgramDefs =
    Dict.fromList
        [ ( "string", Ttype [ PallStrings ] )
        , ( "[]", Tlist [] )
        , ( "int", Ttype [ PallInts ] )
        , ( "alltypes", Ttype [ Pall ] )
        ]


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
        , ( "listprograms", [ PlistPrograms ] )
        , ( "catstrings", [ PcatString ] )
        , ( "sendmessage", [ PsendMessage ] )
        ]


type alias EltState =
    { position : Position
    , typeDefs : Dict.Dict String TypeProgramValue
    , typeStack : List TypeProgramValue
    , typeDefPos : Dict.Dict String Position
    , typeDefUse : Set.Set String
    , defs : Dict.Dict String Type
    , stack : List Type
    , defPos : Dict.Dict String Position
    , defUse : Set.Set String
    , isHome : Bool
    }


type TypeProgramValue
    = Ttype Type
    | Tstring String
    | Tblock (List (Located TlAtom))
    | Tlist (List TypeProgramValue)
    | Tinteger Int


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

                [ PsendMessage ] :: tack ->
                    case tack of
                        recipientCandidate :: messageCandidate :: ck ->
                            if isSubType recipientCandidate [ PallInts ] then
                                if isSubType messageCandidate [ PallStrings ] then
                                    Ok { state | stack = ck }

                                else
                                    Err { message = "expecting a string, but got " ++ showTypeVal messageCandidate, state = state }

                            else
                                Err { message = "expecting an integer, but got " ++ showTypeVal recipientCandidate, state = state }

                        _ ->
                            Err { message = "expecting an integer and a string", state = state }

                [ Pprint ] :: tack ->
                    case tack of
                        stringCandidate :: ack ->
                            if isSubType stringCandidate [ PallStrings ] then
                                Ok { state | stack = ack }

                            else
                                Err { message = "expecting a string but got " ++ showTypeVal stringCandidate, state = state }

                        [] ->
                            Err { message = "empty stack", state = state }

                [ PlistPrograms ] :: tack ->
                    if state.isHome then
                        Ok { state | stack = [ Plist [ PallStrings ] ] :: tack }

                    else
                        Err { message = "not home app", state = state }

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
                        t :: ack ->
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

                        t :: a :: ck ->
                            Ok { state | stack = [ Pbool True, Pbool False ] :: ck }

                _ ->
                    Err { message = "there's nothing to run", state = state }

        TypeLanguage typeAtomsLocated ->
            runTypeProgram typeAtomsLocated state

        StringLiteral s ->
            Ok { state | stack = [ Pstring s ] :: state.stack }

        IntegerLiteral i ->
            Ok { state | stack = [ Pint i ] :: state.stack }

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


programP : String -> Dict.Dict String Utils.Program -> P.Parser (List (Located Atom))
programP filename programs =
    P.loop [] (programHelpP filename programs)


programHelpP :
    String
    -> Dict.Dict String Utils.Program
    -> List (Located Atom)
    -> P.Parser (P.Step (List (Located Atom)) (List (Located Atom)))
programHelpP filename programs p =
    P.oneOf
        [ P.map (\elements -> P.Loop (List.reverse elements ++ p)) (importHelpP programs)
        , P.map (\element -> P.Loop (element :: p))
            (located filename (elementP programs))
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse p))
        ]


type alias ProgramState =
    { program : Utils.Program
    , allPrograms : Dict.Dict String Utils.Program
    , defs : Dict.Dict String ProgVal
    , stack : List ProgVal
    , rightDoc : Maybe Utils.Document
    , outbox : List Utils.HumanMsg
    , internalError : Maybe String
    , myName : Int
    }


programProcessAtom : Located Atom -> ProgramState -> ProgramState
programProcessAtom { start, value, end } s =
    case value of
        Retrieve name ->
            case Dict.get name s.defs of
                Nothing ->
                    { s | internalError = Just <| "could not find name \"" ++ name ++ "\"" }

                Just retrieved ->
                    { s | stack = retrieved :: s.stack }

        Block block ->
            { s | stack = Pblock block :: s.stack }

        Define newName ->
            case s.stack of
                [] ->
                    { s | internalError = Just "empty stack" }

                top :: remainsOfStack ->
                    { s
                        | defs = Dict.insert newName top s.defs
                        , stack = remainsOfStack
                    }

        Runblock ->
            case s.stack of
                (Pblock block) :: remainsOfStack ->
                    let
                        newS =
                            runElfsHelp block { s | stack = remainsOfStack }
                    in
                    { newS | defs = s.defs }

                Pprint :: (Pstring string) :: remainsOfStack ->
                    { s | rightDoc = Just <| print s.rightDoc string, stack = remainsOfStack }

                Pcons :: toAdd :: (Plist ls) :: remainsOfStack ->
                    { s | stack = Plist (toAdd :: ls) :: remainsOfStack }

                PmakeTuple :: (Pint tupleLength) :: remainsOfStack ->
                    { s | stack = Ptuple (List.take tupleLength remainsOfStack) :: List.drop tupleLength remainsOfStack }

                PtypeOf :: next :: remainsOfStack ->
                    { s | stack = Ptype [ next ] :: remainsOfStack }

                Pswitch :: paths :: toSwitchOn :: remainsOfStack ->
                    switch paths toSwitchOn { s | stack = remainsOfStack }

                Ploop :: (Pblock blockToRun) :: remainsOfStack ->
                    runLoop blockToRun { s | stack = remainsOfStack }

                Ppop :: toPop :: remainsOfStack ->
                    { s | stack = remainsOfStack }

                Pplus :: (Pint n1) :: (Pint n2) :: rest ->
                    { s | stack = Pint (n1 + n2) :: rest }

                Pequal :: e1 :: e2 :: remainsOfStack ->
                    { s | stack = Pbool (e1 == e2) :: remainsOfStack }

                PlistPrograms :: remainsOfStack ->
                    { s | stack = Plist (List.map Pstring <| Dict.keys s.allPrograms) :: remainsOfStack }

                PcatString :: (Plist stringCandidates) :: tack ->
                    case extractStrings stringCandidates of
                        Nothing ->
                            { s | internalError = Just "not all strings on top of stack" }

                        Just strings ->
                            { s | stack = Pstring (String.concat strings) :: tack }

                PsendMessage :: (Pint recipient) :: (Pstring message) :: tack ->
                    { s | outbox = { from = s.myName, to = recipient, document = Utils.SmallString message } :: s.outbox }

                _ ->
                    { s | internalError = Just "not a block on top of stack, or couldn't run it" }

        TypeLanguage _ ->
            s

        StringLiteral string ->
            { s | stack = Pstring string :: s.stack }

        IntegerLiteral i ->
            { s | stack = Pint i :: s.stack }

        Export _ ->
            s


type alias TypeError =
    { state : EltState, message : String }


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
    justs <| List.map (makePosition positions) <| Set.toList unused


makePosition : Dict.Dict String Position -> String -> Maybe ( String, Position )
makePosition positions name =
    case Dict.get name positions of
        Nothing ->
            Nothing

        Just position ->
            Just ( name, position )


type alias EltOut =
    Result TypeError EltState


runTypeProgram : List (Located TlAtom) -> EltState -> EltOut
runTypeProgram atoms state =
    case atoms of
        [] ->
            Ok state

        a :: toms ->
            case processTypeAtom { state | position = { start = a.start, end = a.end, file = a.file } } a.value of
                Err err ->
                    Err err

                Ok newState ->
                    runTypeProgram toms newState


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
                            if ts /= oldStack then
                                Err { message = "Bad loop block changed the stack. Old stack: " ++ showTypeStack oldStack ++ "\nNew stack: " ++ showTypeStack ts, state = oldState }

                            else
                                Ok combinedUsed

                        _ ->
                            Err { state = oldState, message = "internal error in type checker: could not convert stacks to tuples" }


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


elementP : Dict.Dict String Utils.Program -> String -> P.Parser Atom
elementP programs filename =
    P.oneOf
        [ runBlockP
        , stringPWrap
        , exportP
        , retrieveP
        , intPWrap
        , defP
        , programBlockP filename programs
        , topTypeLangP filename
        ]


located : String -> (String -> P.Parser a) -> P.Parser (Located a)
located filename parser =
    P.succeed (\start value end -> { start = start, end = end, value = value, file = filename })
        |. whiteSpaceP
        |= P.getPosition
        |= parser filename
        |= P.getPosition
        |. whiteSpaceP


importHelpP : Dict.Dict String Utils.Program -> P.Parser (List (Located Atom))
importHelpP programs =
    P.andThen (importHelpHelpP programs) importP


extractStrings : List ProgVal -> Maybe (List String)
extractStrings candidates =
    Maybe.Extra.combine <| List.map extractString candidates


extractString : ProgVal -> Maybe String
extractString candidate =
    case candidate of
        Pstring s ->
            Just s

        _ ->
            Nothing


runLoop : List (Located Atom) -> ProgramState -> ProgramState
runLoop blockBody oldState =
    case oldState.stack of
        (Pbool False) :: remainsOfStack ->
            { oldState | stack = remainsOfStack }

        (Pbool True) :: remainsOfStack ->
            runLoop blockBody (runElfsHelp blockBody { oldState | stack = remainsOfStack })

        _ ->
            { oldState | internalError = Just "need a bool on top of stack" }


switch : ProgVal -> ProgVal -> ProgramState -> ProgramState
switch pathsCandidate toSwitchOn state =
    case matchPath pathsCandidate toSwitchOn of
        Err err ->
            { state | internalError = Just err }

        Ok chosen ->
            { state | stack = chosen :: state.stack }


matchPath : ProgVal -> ProgVal -> Result String ProgVal
matchPath pathsCandidate toSwitchOn =
    case extractPaths pathsCandidate of
        Err err ->
            Err <| "bad paths: got " ++ showProgVal err

        Ok paths ->
            case List.head <| List.filter (\p -> isSubType [ toSwitchOn ] (Tuple.first p)) paths of
                Nothing ->
                    Err "no matching paths"

                Just chosen ->
                    Ok <| Tuple.second chosen


print : Maybe Utils.Document -> String -> Utils.Document
print doc s =
    case doc of
        Just (Utils.Ordering ds) ->
            Utils.Ordering <| ds ++ [ Utils.SmallString s ]

        Just (Utils.SmallString oldS) ->
            Utils.Ordering [ Utils.SmallString oldS, Utils.SmallString s ]

        Nothing ->
            Utils.SmallString s


showTypeStack : List Type -> String
showTypeStack typestack =
    String.concat
        [ "<"
        , String.join ", " <| List.map showTypeVal typestack
        , ">"
        ]


justs : List (Maybe a) -> List a
justs maybes =
    List.foldr justsHelp [] maybes


justsHelp : Maybe a -> List a -> List a
justsHelp maybe accum =
    case maybe of
        Just a ->
            a :: accum

        Nothing ->
            accum


processTypeAtom : EltState -> TlAtom -> EltOut
processTypeAtom state atom =
    case atom of
        TlRetrieve toRetrieve ->
            case Dict.get toRetrieve state.typeDefs of
                Nothing ->
                    Err { state = state, message = "no definition \"" ++ toRetrieve ++ "\"" }

                Just retrieved ->
                    Ok
                        { state
                            | typeStack = retrieved :: state.typeStack
                            , typeDefUse = Set.insert toRetrieve state.typeDefUse
                        }

        TlBlock block ->
            Ok { state | typeStack = Tblock block :: state.typeStack }

        TlDefine newName ->
            case Dict.get newName state.typeDefPos of
                Just position ->
                    Err { state = state, message = "\"" ++ newName ++ "\" is already defined at " ++ prettyLocation position }

                Nothing ->
                    case state.typeStack of
                        [] ->
                            Err { state = state, message = "empty stack" }

                        s :: tack ->
                            Ok
                                { state
                                    | typeStack = tack
                                    , typeDefs = Dict.insert newName s state.typeDefs
                                    , typeDefPos = Dict.insert newName state.position state.typeDefPos
                                }

        TlRunblock ->
            case state.typeStack of
                [] ->
                    Err { state = state, message = "empty stack" }

                (Tblock bs) :: tack ->
                    case runTypeProgram bs state of
                        Err err ->
                            Err { err | message = "error inside block called at " ++ prettyLocation state.position }

                        Ok newState ->
                            Ok { newState | typeDefs = state.typeDefs }

                _ ->
                    Err { state = state, message = "it's not a block" }

        TlStringLiteral s ->
            Ok { state | typeStack = Tstring s :: state.typeStack }

        TlIntegerLiteral i ->
            Ok { state | typeStack = Tinteger i :: state.typeStack }


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

        PlistPrograms ->
            "listprograms"

        PcatString ->
            "catstring"

        PsendMessage ->
            "sendmessage"


typeUnion : Type -> Type -> Type
typeUnion t1 t2 =
    case ( t1, t2 ) of
        ( [ PsomeTuples t1s ], [ PsomeTuples t2s ] ) ->
            [ PsomeTuples <| List.map2 typeUnion t1s t2s ]

        ( [ Ptype p1 ], [ Ptype p2 ] ) ->
            [ Ptype (p1 ++ p2) ]

        _ ->
            t1 ++ t2


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


topTypeLangP : String -> P.Parser Atom
topTypeLangP filename =
    P.succeed TypeLanguage
        |. P.token "<"
        |= typeLangP filename
        |. P.token ">"


typeLangP : String -> P.Parser (List (Located TlAtom))
typeLangP filename =
    P.loop [] (typeLangHelpP filename)


programBlockP : String -> Dict.Dict String Utils.Program -> P.Parser Atom
programBlockP filename programs =
    P.succeed Block
        |. P.token "{"
        |= programP filename programs
        |. P.token "}"


defP : P.Parser Atom
defP =
    P.succeed Define
        |. P.token "="
        |. whiteSpaceP
        |= variable


intPWrap : P.Parser Atom
intPWrap =
    P.succeed IntegerLiteral
        |= intP


intP : P.Parser Int
intP =
    P.oneOf
        [ P.succeed negate
            |. P.symbol "-"
            |= P.int
        , P.int
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


stringPWrap : P.Parser Atom
stringPWrap =
    P.succeed StringLiteral
        |= stringP


{-| Mostly copied from <https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm>
-}
stringP : P.Parser String
stringP =
    P.succeed identity
        |. P.token "\""
        |= P.loop ( [], 0 ) stringHelp2
        |. P.token "\""


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


importHelpHelpP : Dict.Dict String Utils.Program -> String -> P.Parser (List (Located Atom))
importHelpHelpP programs toImport =
    case Dict.get toImport programs of
        Nothing ->
            P.problem <| "can't find program with name \"" ++ toImport ++ "\""

        Just program ->
            case P.run (programP toImport programs) program.code of
                Err err ->
                    P.problem <| deadEndsToString err

                Ok atoms ->
                    P.succeed atoms


extractPaths : ProgVal -> Result ProgVal (List ( Type, ProgVal ))
extractPaths candidates =
    case candidates of
        Plist ls ->
            Result.Extra.combine <| List.map getMatchPath ls

        _ ->
            Err candidates


getMatchPath : ProgVal -> Result ProgVal ( Type, ProgVal )
getMatchPath p =
    case p of
        Ptuple [ path, Ptype t ] ->
            Ok ( t, path )

        bad ->
            Err bad


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

        TypeLanguage tlAtoms ->
            "Typelanguage <" ++ String.join " " (List.map (showTlAtom << .value) tlAtoms) ++ ">"

        StringLiteral string ->
            "String " ++ showString string

        IntegerLiteral integer ->
            "Integer " ++ String.fromInt integer

        Export s ->
            "Export " ++ s


typeDiff : Type -> Type -> Type
typeDiff fromThis subtractThis =
    List.filter (\x -> not <| isSubType [ x ] subtractThis) fromThis


typeLangHelpP :
    String
    -> List (Located TlAtom)
    -> P.Parser (P.Step (List (Located TlAtom)) (List (Located TlAtom)))
typeLangHelpP filename p =
    P.oneOf
        [ P.map (\element -> P.Loop (element :: p))
            (located filename typeElementP)
        , P.succeed () |> P.map (\_ -> P.Done (List.reverse p))
        ]


variable : P.Parser String
variable =
    P.variable
        { start = \c -> Char.isAlpha c || Set.member c okVariableStart
        , inner = \c -> Char.isAlphaNum c || Set.member c okVariableInner
        , reserved = reserved
        }


stringHelp2 : ( List String, Int ) -> P.Parser (P.Step ( List String, Int ) String)
stringHelp2 ( revChunks, offset ) =
    P.succeed (stepHelp offset)
        |= stringHelp revChunks
        |= P.getOffset


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


{-| Don't use P.NotNestable for multicomment, as it doesn't consume
the closing \*
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


showTlAtom : TlAtom -> String
showTlAtom atom =
    case atom of
        TlRetrieve string ->
            "Retrieve " ++ string

        TlBlock atoms ->
            "Block {" ++ String.join " " (List.map (showTlAtom << .value) atoms) ++ "}"

        TlDefine string ->
            "Define " ++ string

        TlRunblock ->
            "Runblock"

        TlStringLiteral string ->
            "String " ++ string

        TlIntegerLiteral integer ->
            "Integer " ++ String.fromInt integer


typeElementP : String -> P.Parser TlAtom
typeElementP filename =
    P.oneOf
        [ typeRunBlockP
        , typeStringP
        , typeDefP
        , typeBlockP filename
        , typeRetrieveP
        ]


typeRetrieveP : P.Parser TlAtom
typeRetrieveP =
    P.succeed TlRetrieve
        |= variable


typeBlockP : String -> P.Parser TlAtom
typeBlockP filename =
    P.succeed TlBlock
        |. P.token "{"
        |= typeLangP filename
        |. P.token "}"


typeDefP : P.Parser TlAtom
typeDefP =
    P.succeed TlDefine
        |. P.token "="
        |. whiteSpaceP
        |= variable


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


typeStringP : P.Parser TlAtom
typeStringP =
    P.succeed TlStringLiteral
        |= stringP


typeRunBlockP : P.Parser TlAtom
typeRunBlockP =
    P.succeed TlRunblock
        |. P.token "."


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'

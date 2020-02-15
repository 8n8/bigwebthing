module Testable exposing (..)

import Base64
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Dict
import Element
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Html
import Html.Attributes
import Json.Decode as Jd
import Json.Encode as Je
import List.Nonempty as N
import Maybe.Extra
import Parser as P exposing ((|.), (|=))
import Result.Extra
import Set


type Msg
    = RetrievedHome String
    | RetrievedHash String
    | UpdatedLeft String
    | UpdatedEditor String
    | LookupRaw (N.Nonempty String)
    | LaunchProgram String
    | NewRawKeys Je.Value
    | ShowProgramCheckBox Bool


type alias Model =
    { home : Home
    , openProgram : Maybe ( Program, Maybe Document )
    , lookedUpBlob : Maybe ( Bytes.Bytes, Set.Set String )
    , toLookUp : List String
    , accumBlob : Maybe Bytes.Bytes
    , internalErr : Maybe String
    , editProgram : Bool
    }


type alias Home =
    { biggestNonceBase : Int
    , myKeys : Maybe MySecretKeys
    , outbox : List HumanMsg
    , programs : Dict.Dict String Program
    , pubKeys : Dict.Dict String PubKeys
    }


type alias MySecretKeys =
    { encrypt : Bytes.Bytes
    , sign : Bytes.Bytes
    }


type alias PubKeys =
    { encrypt : Bytes.Bytes
    , sign : Bytes.Bytes
    }


initHome : Home
initHome =
    { outbox = []
    , programs = Dict.fromList [ ( "home", defaultHome ) ]
    , pubKeys = Dict.empty
    , biggestNonceBase = 0
    , myKeys = Nothing
    }


defaultHome : Program
defaultHome =
    { code = defaultHomeCode
    , name = "home"
    , description = "The main home app."
    , inbox = []
    , blobs = []
    , typedIn = ""
    }


defaultHomeCode : String
defaultHomeCode =
    """[] "hi" cons. "there" cons."""


view : Model -> Html.Html Msg
view model =
    Element.layout [] (viewHelp model)


viewHelp : Model -> Element.Element Msg
viewHelp model =
    Element.column
        [ Element.width <| Element.fill
        , Element.padding 6
        , Element.spacing 20
        , sansSerif
        , Font.size 25
        ]
        [ homeButton
        , leftInput model
        , Element.text "The program output goes here:"
        , showRightDoc model
        , Element.text "End of program output."
        , editor model
        ]


css : String -> String -> Element.Attribute Msg
css key value =
    Element.htmlAttribute <| Html.Attributes.style key value


sansSerif : Element.Attribute Msg
sansSerif =
    Font.family [ Font.typeface "Ubuntu" ]


monospace : Element.Attribute Msg
monospace =
    Font.family [ Font.typeface "Ubuntu Mono" ]


editor : Model -> Element.Element Msg
editor model =
    case model.openProgram of
        Nothing ->
            Element.text <| "internal err: can't find program"

        Just ( program, _ ) ->
            Element.Input.multiline
                [ monospace
                ]
                { onChange = UpdatedEditor
                , text = program.code
                , placeholder =
                    Just <|
                        Element.Input.placeholder [] <|
                            Element.text "Type program here"
                , label =
                    Element.Input.labelAbove [ sansSerif ] <|
                        Element.text "This box contains the program:"
                , spellcheck = False
                }


homeButton : Element.Element Msg
homeButton =
    Element.Input.button [ Element.padding 10, Border.width 1, Border.color <| Element.rgb255 0 0 0 ]
        { onPress = Just <| LaunchProgram "home"
        , label = Element.text "Launch home app"
        }


standardTypeProgramDefs : Dict.Dict String TypeProgramValue
standardTypeProgramDefs =
    Dict.fromList
        [ ( "string", Ttype [ PallStrings ] )
        , ( "[]", Tlist [] )
        , ( "int", Ttype [ PallInts ] )
        , ( "alltypes", Ttype [ Pall ] )
        ]


initTypeProgramState : TypeState
initTypeProgramState =
    { defs = standardTypeProgramDefs
    , stack = []
    , runTimeNames = Set.fromList <| Dict.keys standardTypes
    }


type alias Located a =
    { start : ( Int, Int )
    , value : a
    , end : ( Int, Int )
    }


topProgramP : P.Parser (List (Located Atom))
topProgramP =
    P.succeed identity
        |= programP
        |. P.end


runProgram : Program -> ( Program, Maybe Document, List HumanMsg )
runProgram program =
    case P.run topProgramP program.code of
        Err deadEnds ->
            ( program
            , Just <| SmallString <| deadEndsToString deadEnds
            , []
            )

        Ok atoms ->
            case runTypeChecks atoms initEltState of
                Just errMsg ->
                    ( program, Just <| SmallString ("type error: " ++ errMsg), [] )

                Nothing ->
                    runElfs program atoms []


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


type alias ParserOut =
    { typeState : TypeState
    , elfs : List Elf
    , elts : List Elt
    }


programP : P.Parser (List (Located Atom))
programP =
    P.loop [] programHelpP


programHelpP :
    List (Located Atom)
    -> P.Parser (P.Step (List (Located Atom)) (List (Located Atom)))
programHelpP p =
    P.oneOf
        [ P.map (\element -> P.Loop (element :: p))
            (located elementP)
        , P.succeed ()
            |> P.map (\_ -> P.Done (List.reverse p))
        ]


programLoopHelp :
    List (Located Atom)
    -> Int
    -> List (Located Atom)
    -> Int
    -> P.Step ( List (Located Atom), Int ) ( List (Located Atom), Int )
programLoopHelp oldP oldOffset newP newOffset =
    if newOffset == oldOffset then
        P.Done ( oldP, oldOffset )

    else
        P.Loop
            ( oldP ++ newP
            , newOffset
            )


elementP : P.Parser Atom
elementP =
    P.oneOf
        [ runBlockP
        , stringPWrap
        , retrieveP
        , intPWrap
        , defP
        , programBlockP
        , topTypeLangP
        ]


topTypeLangP : P.Parser Atom
topTypeLangP =
    P.succeed TypeLanguage
        |. P.token "<"
        |= typeLangP
        |. P.token ">"


typeLangP : P.Parser (List (Located TlAtom))
typeLangP =
    P.loop [] typeLangHelpP


type alias Ety =
    TypeState -> Result String TypeProgramOut


typeLangHelpP :
    List (Located TlAtom)
    -> P.Parser (P.Step (List (Located TlAtom)) (List (Located TlAtom)))
typeLangHelpP p =
    P.oneOf
        [ P.map (\element -> P.Loop (element :: p))
            (located typeElementP)
        , P.succeed () |> P.map (\_ -> P.Done (List.reverse p))
        ]


typeElementP : P.Parser TlAtom
typeElementP =
    P.oneOf
        [ typeRunBlockP
        , typeStringP
        , typeDefP
        , typeBlockP
        , typeRetrieveP
        ]


typeRetrieveP : P.Parser TlAtom
typeRetrieveP =
    P.succeed TlRetrieve
        |= variable


typeBlockP : P.Parser TlAtom
typeBlockP =
    P.succeed TlBlock
        |. P.token "{"
        |= typeLangP
        |. P.token "}"


typeDefP : P.Parser TlAtom
typeDefP =
    P.succeed TlDefine
        |. P.token "="
        |. whiteSpaceP
        |= variable


typeDefEty : String -> Ety
typeDefEty var t =
    case t.stack of
        [] ->
            Err "stack should contain at least one item"

        s :: tack ->
            if Dict.member var t.defs then
                Err <| "multiple definitions of \"" ++ var ++ "\""

            else
                Ok
                    { state =
                        { defs = Dict.insert var s t.defs
                        , stack = tack
                        , runTimeNames = t.runTimeNames
                        }
                    , elts = []
                    }


typeStringP : P.Parser TlAtom
typeStringP =
    P.succeed TlStringLiteral
        |= stringP


stringEty : String -> Ety
stringEty s t =
    Ok { state = { t | stack = Tstring s :: t.stack }, elts = [] }


typeRunBlockP : P.Parser TlAtom
typeRunBlockP =
    P.succeed TlRunblock
        |. P.token "."


runTypeBlock : TypeState -> List (TypeState -> Result String TypeProgramOut) -> Result String TypeProgramOut
runTypeBlock t elements =
    let
        perhaps =
            runTypeBlockHelp elements { state = t, elts = [] }
    in
    case perhaps of
        Ok result ->
            Ok
                { state =
                    { defs = t.defs
                    , stack = result.state.stack
                    , runTimeNames = t.runTimeNames
                    }
                , elts = result.elts
                }

        Err err ->
            Err err


runTypeBlockHelp :
    List (TypeState -> Result String TypeProgramOut)
    -> TypeProgramOut
    -> Result String TypeProgramOut
runTypeBlockHelp elements t =
    case elements of
        [] ->
            Ok t

        e :: lements ->
            case e t.state of
                Ok newOut ->
                    let
                        addElts =
                            { newOut | elts = t.elts ++ newOut.elts }
                    in
                    runTypeBlockHelp lements addElts

                Err err ->
                    Err err


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


variable : P.Parser String
variable =
    P.variable
        { start = \c -> Char.isAlpha c || Set.member c okVariableStart
        , inner = \c -> Char.isAlphaNum c || Set.member c okVariableInner
        , reserved = reserved
        }


reserved : Set.Set String
reserved =
    Set.fromList
        [ "." ]


okVariableStart : Set.Set Char
okVariableStart =
    Set.fromList
        [ '['
        , ']'
        , ','
        ]


okVariableInner : Set.Set Char
okVariableInner =
    Set.fromList
        [ '['
        , ']'
        ]


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'


located : P.Parser a -> P.Parser (Located a)
located parser =
    P.succeed Located
        |. whiteSpaceP
        |= P.getPosition
        |= parser
        |= P.getPosition
        |. whiteSpaceP


defP : P.Parser Atom
defP =
    P.succeed Define
        |. P.token "="
        |. whiteSpaceP
        |= variable


defElf : String -> ProgramState -> ProgramState
defElf var p =
    case p.stack of
        [] ->
            { p
                | internalError =
                    Just
                        "stack should contain at least one item"
            }

        s :: tack ->
            if Dict.member var p.defs then
                { p
                    | internalError =
                        Just <|
                            String.concat
                                [ "multiple definitions of \""
                                , var
                                , "\""
                                ]
                }

            else
                { p
                    | defs = Dict.insert var s p.defs
                    , stack = tack
                }


defElt : String -> ( Int, Int ) -> ( Int, Int ) -> Elt
defElt var start end state =
    case state.stack of
        [] ->
            Err
                { message = "you need to put something on the stack before a definition"
                , state = newPos state start end
                }

        s :: tack ->
            if Dict.member var state.defs then
                Err
                    { message =
                        String.concat
                            [ "mutiple definitions of \""
                            , var
                            , "\""
                            ]
                    , state = newPos state start end
                    }

            else
                Ok
                    { state
                        | defs = Dict.insert var s state.defs
                        , stack = tack
                        , startPosition = start
                        , endPosition = end
                        , defPos = Dict.insert var ( start, end ) state.defPos
                    }


runBlockP : P.Parser Atom
runBlockP =
    P.succeed Runblock
        |. P.token "."


programBlockP : P.Parser Atom
programBlockP =
    P.succeed Block
        |. P.token "{"
        |= programP
        |. P.token "}"


runBlockElf : ProgramState -> ProgramState
runBlockElf s =
    case s.stack of
        [] ->
            { s | internalError = Just ". but empty stack" }

        (Pblock block) :: xs ->
            runElfsHelp block { s | stack = xs }

        x :: _ ->
            { s
                | internalError =
                    Just <|
                        String.concat
                            [ "expecting a block on top of the "
                            , "stack, but got "
                            , showProgVal x
                            ]
            }


blockUnusedNames : EltState -> EltState -> Maybe TypeError
blockUnusedNames new old =
    let
        newNames =
            Dict.diff new.defPos old.defPos

        newNameSet =
            Set.fromList <| Dict.keys newNames

        newUnused =
            Set.diff newNameSet new.defUse
    in
    case namesAndPositions newUnused new.defPos of
        [] ->
            Nothing

        oneOrMore ->
            Just { message = prettyUnused oneOrMore, state = new }


newPos : EltState -> ( Int, Int ) -> ( Int, Int ) -> EltState
newPos state start end =
    { state
        | startPosition = start
        , endPosition = end
    }


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


showTypeAtom : TypeAtom -> String
showTypeAtom typeAtom =
    case typeAtom of
        Astring s ->
            "string: \"" ++ showString s ++ "\""

        Aint i ->
            "int: " ++ String.fromInt i

        Atype t ->
            "type: " ++ showTypeVal t

        Ablock block ->
            "block: " ++ String.join ", " (List.map (showAtom << .value) block)


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


showTypeVal : Type -> String
showTypeVal type_ =
    String.concat
        [ "["
        , String.join ", " <| List.map showProgVal type_
        , "]"
        ]


showStandardType : StandardType -> String
showStandardType standard =
    case standard of
        Sstring ->
            "string"

        Sblock ->
            "block"

        Slist type_ ->
            "list: " ++ showTypeVal type_

        Sint ->
            "int"

        Stuple types ->
            "tuple: "
                ++ (String.join ", " <| List.map showTypeVal types)

        Stype ->
            "type"

        Sall ->
            "all"


showTypeStack : List Type -> String
showTypeStack typestack =
    String.concat
        [ "<"
        , String.join ", " <| List.map showTypeVal typestack
        , ">"
        ]


retrieveP : P.Parser Atom
retrieveP =
    P.map Retrieve variable


whiteSpaceP : P.Parser ()
whiteSpaceP =
    P.loop 0 <| ifProgress oneWhitespaceP


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


initDoc : Document
initDoc =
    SmallString "this program produces no output"


runElfs :
    Program
    -> List (Located Atom)
    -> List ProgVal
    -> ( Program, Maybe Document, List HumanMsg )
runElfs program elfs progStack =
    let
        oldP =
            { program = program
            , defs = standardLibrary
            , stack = progStack
            , rightDoc = Nothing
            , outbox = []
            , blobs = []
            , internalError = Nothing
            }

        newP =
            runElfsHelp elfs oldP
    in
    case newP.internalError of
        Nothing ->
            ( newP.program, newP.rightDoc, newP.outbox )

        Just err ->
            ( oldP.program, Just <| SmallString ("internal error: " ++ err), oldP.outbox )


type alias ProgramState =
    { program : Program
    , defs : Dict.Dict String ProgVal
    , stack : List ProgVal
    , rightDoc : Maybe Document
    , outbox : List HumanMsg
    , blobs : List Blob
    , internalError : Maybe String
    }


typeOf : ProgVal -> Type
typeOf value =
    [ value ]


type alias Elf =
    ProgramState -> ProgramState


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

                _ ->
                    { s | internalError = Just "not a block on top of stack, or couldn't run it" }

        TypeLanguage _ ->
            s

        StringLiteral string ->
            { s | stack = Pstring string :: s.stack }

        IntegerLiteral i ->
            { s | stack = Pint i :: s.stack }


type alias TypeState =
    { defs : Dict.Dict String TypeProgramValue
    , stack : List TypeProgramValue
    , runTimeNames : Set.Set String
    }


type alias TypeProgramOut =
    { state : TypeState
    , elts : List Elt
    }


type TypeProgramValue
    = Ttype Type
    | Tstring String
    | Tblock (List (Located TlAtom))
    | Tlist (List TypeProgramValue)
    | Tinteger Int


showTypeProgramValue : TypeProgramValue -> String
showTypeProgramValue typeProgramValue =
    case typeProgramValue of
        Ttype t ->
            showTypeVal t

        Tstring s ->
            "\"" ++ showString s ++ "\""

        Tblock _ ->
            "block"

        Tlist ts ->
            String.concat
                [ "list: ["
                , String.join ", " <| List.map showTypeProgramValue ts
                , "]"
                ]

        Tinteger i ->
            String.fromInt i


showTypeProgramType : TypeProgramValue -> String
showTypeProgramType t =
    case t of
        Ttype _ ->
            "type"

        Tstring _ ->
            "string"

        Tblock _ ->
            "block"

        Tlist _ ->
            "list"

        Tinteger _ ->
            "int"


type alias Type =
    List ProgVal


type TypeAtom
    = Astring String
    | Aint Int
    | Atype Type
    | Ablock (List (Located Atom))


type StandardType
    = Sblock
    | Sstring
    | Sint
    | Stype
    | Slist Type
    | Stuple (List Type)
    | Sall


type Atom
    = Retrieve String
    | Block (List (Located Atom))
    | Define String
    | Runblock
    | TypeLanguage (List (Located TlAtom))
    | StringLiteral String
    | IntegerLiteral Int


type TlAtom
    = TlRetrieve String
    | TlBlock (List (Located TlAtom))
    | TlDefine String
    | TlRunblock
    | TlStringLiteral String
    | TlIntegerLiteral Int


isSubStack : List Type -> List Type -> Bool
isSubStack sub master =
    (List.length sub == List.length master)
        && (List.all identity <| List.map2 isSubType sub master)


type alias Elt =
    EltState -> EltOut


type alias TypeError =
    { state : EltState, message : String }


type alias EltOut =
    Result TypeError EltState


type alias EltState =
    { startPosition : ( Int, Int )
    , endPosition : ( Int, Int )
    , typeDefs : Dict.Dict String TypeProgramValue
    , typeStack : List TypeProgramValue
    , typeDefPos : Dict.Dict String ( ( Int, Int ), ( Int, Int ) )
    , typeDefUse : Set.Set String
    , defs : Dict.Dict String Type
    , stack : List Type
    , defPos : Dict.Dict String ( ( Int, Int ), ( Int, Int ) )
    , defUse : Set.Set String
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
        ]


swapInfo : String
swapInfo =
    """"swap" swaps the top two things on the stack"""


swapElt : Elt
swapElt state =
    case state.stack of
        [] ->
            Err { state = state, message = "empty stack: " ++ swapInfo }

        _ :: [] ->
            Err { state = state, message = "only one thing in stack: " ++ swapInfo }

        s :: t :: ack ->
            Ok { state | stack = t :: s :: ack }


typeUnionInfo : String
typeUnionInfo =
    """typeUnion needs the top two items in the stack to be types"""


typeUnionElt : Elt
typeUnionElt s =
    case s.stack of
        [] ->
            Err { state = s, message = "empty stack: " ++ typeUnionInfo }

        _ :: [] ->
            Err { state = s, message = "only one thing in stack: " ++ typeUnionInfo }

        top :: next :: remainsOfStack ->
            Ok { s | stack = typeUnion top next :: remainsOfStack }


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


listTypeCons : Type -> Type -> Maybe Type
listTypeCons toAdd toAddTo =
    case toAddTo of
        [ PallLists ] ->
            Just [ PallLists ]

        [ Plist values ] ->
            Just [ Plist <| typeUnion toAdd values ]

        _ ->
            Nothing


typeUnion : Type -> Type -> Type
typeUnion t1 t2 =
    t1 ++ t2


customUnion : List TypeAtom -> List TypeAtom -> List TypeAtom
customUnion l1 l2 =
    l1 ++ l2


standardUnion : List StandardType -> List StandardType -> List StandardType
standardUnion l1 l2 =
    l1 ++ l2


standardLibrary : Dict.Dict String ProgVal
standardLibrary =
    Dict.fromList
        [ ( "[]", Plist [] )
        , ( "testForSwitch", Pint 2 )
        , ( "print", Pprint )
        , ( "emptylist", Plist [] )
        , ( "cons", Pcons )
        , ( "maketuple", PmakeTuple )
        ]


swapElf : Elf
swapElf p =
    case p.stack of
        s :: t :: ack ->
            { p | stack = t :: s :: ack }

        _ ->
            { p | internalError = Just "swap needs at least two things on the stack" }


makeTupleElf : Elf
makeTupleElf p =
    case p.stack of
        (Pint length) :: remainsOfStack ->
            let
                tuple =
                    Ptuple <| List.take length remainsOfStack

                remainsOfRemains =
                    List.drop length remainsOfStack
            in
            { p | stack = tuple :: remainsOfRemains }

        _ ->
            { p | internalError = Just "expecting an int" }


typeofElf : Elf
typeofElf p =
    case p.stack of
        [] ->
            { p | internalError = Just "there should be something on the stack" }

        s :: tack ->
            { p | stack = Ptype (typeOf s) :: tack }


switchElf : Elf
switchElf p =
    case p.stack of
        (Plist paths) :: value :: remainsOfStack ->
            case extractPaths paths of
                Err err ->
                    { p | internalError = Just <| "bad paths 1: " ++ showProgVal err }

                Ok goodPaths ->
                    case findPath (typeOf value) goodPaths of
                        Nothing ->
                            { p | internalError = Just <| "no path for value: " ++ showProgVal value }

                        Just path ->
                            { p | stack = path :: remainsOfStack }

        _ ->
            { p | internalError = Just "bad stack" }


extractPaths : List ProgVal -> Result ProgVal (List ( Type, ProgVal ))
extractPaths candidates =
    Result.Extra.combine <| List.map getMatchPath candidates


getMatchPath : ProgVal -> Result ProgVal ( Type, ProgVal )
getMatchPath p =
    case p of
        Ptuple [ Ptype t, path ] ->
            Ok ( t, path )

        bad ->
            Err bad


findPath : Type -> List ( Type, ProgVal ) -> Maybe ProgVal
findPath value paths =
    let
        matching =
            List.filter
                (isSubType value << Tuple.first)
                paths
    in
    Maybe.map Tuple.second <| List.head matching


consInfo =
    "\"cons\" adds something to the front of a list. The top item on the stack should be the new list element.  The second item on the stack should be the list that will be added to."


consElf : Elf
consElf p =
    case p.stack of
        toPrepend :: (Plist ps) :: remainsOfStack ->
            { p | stack = Plist (toPrepend :: ps) :: remainsOfStack }

        _ ->
            { p | internalError = Just <| "bad stack: " ++ consInfo }


printElf : ProgramState -> ProgramState
printElf p =
    case p.stack of
        (Pstring s) :: tack ->
            { p
                | rightDoc = Just <| print p.rightDoc s
                , stack = tack
            }

        _ ->
            { p
                | internalError =
                    Just <|
                        String.concat
                            [ "runtime error:\n"
                            , "\"print\" expects the top of the stack to "
                            , "be a string"
                            ]
            }


print : Maybe Document -> String -> Document
print doc s =
    case doc of
        Just (Ordering ds) ->
            Ordering <| ds ++ [ SmallString s ]

        Just (SmallString oldS) ->
            Ordering [ SmallString oldS, SmallString s ]

        Just (Named n b) ->
            Ordering [ Named n b, SmallString s ]

        Just (Anon b) ->
            Ordering [ Anon b, SmallString s ]

        Nothing ->
            SmallString s


printElt : Elt
printElt state =
    case state.stack of
        [] ->
            let
                message =
                    String.concat
                        [ "\"print\" needs there to be a something "
                        , "on the stack, but it is empty"
                        ]
            in
            Err { state = state, message = message }

        s :: tack ->
            if isSubType s [ PallStrings ] then
                Ok { state | stack = tack }

            else
                let
                    message =
                        String.concat
                            [ "\"print\" needs the top value on the "
                            , "stack to be a string, but it is "
                            , showTypeVal s
                            ]
                in
                Err { state = state, message = message }


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


standardOfCustom : List StandardType -> List TypeAtom -> Bool
standardOfCustom _ _ =
    False


standardOfStandard : List StandardType -> List StandardType -> Bool
standardOfStandard sub master =
    List.all (standardOfStandardHelp master) sub


standardOfStandardHelp : List StandardType -> StandardType -> Bool
standardOfStandardHelp master candidate =
    List.member Sall master
        || (case candidate of
                Sblock ->
                    List.any ((==) Sblock) master

                Slist lt ->
                    listMatch lt (getLists master [])

                Stuple tupleTypes ->
                    tupleMatch tupleTypes (getTuples master [])

                _ ->
                    List.member candidate master
           )


tupleMatch : List Type -> List (List Type) -> Bool
tupleMatch sub master =
    List.any (isSubStack sub) master


getLists : List StandardType -> List Type -> List Type
getLists notReadYet accum =
    case notReadYet of
        [] ->
            accum

        (Slist t) :: otReadYet ->
            getLists otReadYet (t :: accum)

        _ :: otReadYet ->
            getLists otReadYet accum


getTuples : List StandardType -> List (List Type) -> List (List Type)
getTuples notReadYet accum =
    case notReadYet of
        [] ->
            accum

        (Stuple tu) :: otReadYet ->
            getTuples otReadYet (tu :: accum)

        _ :: otReadYet ->
            getTuples otReadYet accum


listMatch : Type -> List Type -> Bool
listMatch candidate master =
    List.any (isSubType candidate) master


customOfCustom : List TypeAtom -> List TypeAtom -> Bool
customOfCustom sub master =
    List.all (\s -> List.member s master) sub


customOfStandard : List TypeAtom -> List StandardType -> Bool
customOfStandard sub master =
    List.all (customOfStandardHelp master) sub


customOfStandardHelp : List StandardType -> TypeAtom -> Bool
customOfStandardHelp bs t =
    List.member Sall bs
        || (case t of
                Astring _ ->
                    List.member Sstring bs

                Aint _ ->
                    List.member Sint bs

                Atype _ ->
                    List.member Stype bs

                Ablock block ->
                    List.member Sblock bs
           )


initEltState =
    { startPosition = ( 0, 0 )
    , endPosition = ( 0, 0 )
    , stack = []
    , defs = standardTypes
    , defPos = Dict.empty
    , defUse = Set.empty
    , typeDefPos = Dict.empty
    , typeDefUse = Set.empty
    , typeDefs = standardTypeProgramDefs
    , typeStack = []
    }


runTypeChecks : List (Located Atom) -> EltState -> Maybe String
runTypeChecks atoms init =
    case runTypeChecksHelp atoms init of
        Ok endState ->
            typeEndChecks endState

        Err err ->
            Just <| prettyErrorMessage err


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


prettyUnused : List ( String, ( Int, Int ), ( Int, Int ) ) -> String
prettyUnused unused =
    String.join ", " <| List.map onePrettyUnused unused


onePrettyUnused : ( String, ( Int, Int ), ( Int, Int ) ) -> String
onePrettyUnused ( name, start, end ) =
    String.concat
        [ "\""
        , name
        , "\" defined but not used: between "
        , prettyPosition start
        , " and "
        , prettyPosition end
        ]


namesAndPositions : Set.Set String -> Dict.Dict String ( ( Int, Int ), ( Int, Int ) ) -> List ( String, ( Int, Int ), ( Int, Int ) )
namesAndPositions unused positions =
    justs <| List.map (makePosition positions) <| Set.toList unused


makePosition : Dict.Dict String ( ( Int, Int ), ( Int, Int ) ) -> String -> Maybe ( String, ( Int, Int ), ( Int, Int ) )
makePosition positions name =
    case Dict.get name positions of
        Nothing ->
            Nothing

        Just ( start, end ) ->
            Just ( name, start, end )


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


prettyErrorMessage : TypeError -> String
prettyErrorMessage { state, message } =
    String.concat
        [ "between "
        , prettyPosition state.startPosition
        , " and "
        , prettyPosition state.endPosition
        , ":\n"
        , message
        ]


prettyPosition : ( Int, Int ) -> String
prettyPosition ( row, column ) =
    String.concat
        [ "row "
        , String.fromInt row
        , " column "
        , String.fromInt column
        ]


runTypeChecksHelp : List (Located Atom) -> EltState -> EltOut
runTypeChecksHelp atoms state =
    case atoms of
        [] ->
            Ok state

        a :: toms ->
            case processAtom { state | startPosition = a.start, endPosition = a.end } a.value of
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
                Just ( position, _ ) ->
                    Err { state = state, message = "\"" ++ newName ++ "\" is already defined at " ++ prettyPosition position }

                Nothing ->
                    case state.stack of
                        [] ->
                            Err { state = state, message = "empty stack" }

                        s :: tack ->
                            Ok
                                { state
                                    | stack = tack
                                    , defs = Dict.insert newName s state.defs
                                    , defPos = Dict.insert newName ( state.startPosition, state.endPosition ) state.defPos
                                }

        Runblock ->
            case state.stack of
                [] ->
                    Err { state = state, message = "empty stack" }

                [ Pblock bs ] :: tack ->
                    case runTypeChecksHelp bs state of
                        Err err ->
                            Err { err | message = "error inside block called at " ++ prettyPosition state.startPosition }

                        Ok newState ->
                            Ok { newState | defs = state.defs }

                [ Pprint ] :: tack ->
                    case tack of
                        [ Pstring _ ] :: ack ->
                            Ok { state | stack = ack }

                        _ ->
                            Err { message = "expecting a string", state = state }

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
                                Err { state = state, message = "stack not long enough" }

                            else
                                Ok { state | stack = [ PsomeTuples (List.take i remainsOfStack) ] :: List.drop i remainsOfStack }

                        _ ->
                            Err { state = state, message = "no integer on top of stack" }

                _ ->
                    Err { message = "there's nothing to run", state = state }

        TypeLanguage typeAtomsLocated ->
            runTypeProgram typeAtomsLocated state

        StringLiteral s ->
            Ok { state | stack = [ Pstring s ] :: state.stack }

        IntegerLiteral i ->
            Ok { state | stack = [ Pint i ] :: state.stack }


runTypeProgram : List (Located TlAtom) -> EltState -> EltOut
runTypeProgram atoms state =
    case atoms of
        [] ->
            Ok state

        a :: toms ->
            case processTypeAtom { state | startPosition = a.start, endPosition = a.end } a.value of
                Err err ->
                    Err err

                Ok newState ->
                    runTypeProgram toms newState


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
                Just ( startPosition, endPosition ) ->
                    Err { state = state, message = "\"" ++ newName ++ "\" is already defined at " ++ prettyPosition startPosition }

                Nothing ->
                    case state.typeStack of
                        [] ->
                            Err { state = state, message = "empty stack" }

                        s :: tack ->
                            Ok
                                { state
                                    | typeStack = tack
                                    , typeDefs = Dict.insert newName s state.typeDefs
                                    , typeDefPos = Dict.insert newName ( state.startPosition, state.endPosition ) state.typeDefPos
                                }

        TlRunblock ->
            case state.typeStack of
                [] ->
                    Err { state = state, message = "empty stack" }

                (Tblock bs) :: tack ->
                    case runTypeProgram bs state of
                        Err err ->
                            Err { err | message = "error inside block called at " ++ prettyPosition state.startPosition }

                        Ok newState ->
                            Ok { newState | typeDefs = state.typeDefs }

                _ ->
                    Err { state = state, message = "it's not a block" }

        TlStringLiteral s ->
            Ok { state | typeStack = Tstring s :: state.typeStack }

        TlIntegerLiteral i ->
            Ok { state | typeStack = Tinteger i :: state.typeStack }


type alias HumanMsg =
    { from : String
    , to : String
    , document : Document
    }


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


leftInput : Model -> Element.Element Msg
leftInput model =
    Element.Input.multiline [ monospace ]
        { onChange = UpdatedLeft
        , text = leftText model
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    Element.text "Type here"
        , label =
            Element.Input.labelAbove [ sansSerif ] <|
                Element.text <|
                    "Your input goes here:"
        , spellcheck = True
        }


leftText : Model -> String
leftText model =
    case model.openProgram of
        Nothing ->
            "internal error:: can't find program"

        Just ( program, _ ) ->
            program.typedIn


showRightDoc : Model -> Element.Element Msg
showRightDoc model =
    Element.el [ monospace ] <|
        case model.openProgram of
            Nothing ->
                Element.text <| "internal error: can't find program"

            Just ( _, doc ) ->
                displayDoc model.lookedUpBlob doc


displayDoc :
    Maybe ( Bytes.Bytes, Set.Set String )
    -> Maybe Document
    -> Element.Element Msg
displayDoc lookedUpBlob doc =
    case doc of
        Just (Anon blob) ->
            displayBlob "•" blob lookedUpBlob

        Just (Named name blob) ->
            displayBlob name blob lookedUpBlob

        Just (Ordering docs) ->
            Element.column [] <|
                List.map (displayDoc lookedUpBlob) (List.map Just docs)

        Just (SmallString s) ->
            Element.text s

        Nothing ->
            Element.text "this program produces no output"


displayBlob :
    String
    -> Blob
    -> Maybe ( Bytes.Bytes, Set.Set String )
    -> Element.Element Msg
displayBlob name (Blob mime hashes) lookedUpBlob =
    case lookedUpBlob of
        Nothing ->
            Element.Input.button []
                { onPress = Just <| LookupRaw hashes
                , label = Element.text name
                }

        Just ( bytes, lookedUpHashes ) ->
            if
                lookedUpHashes
                    /= (Set.fromList <| N.toList hashes)
            then
                Element.Input.button []
                    { onPress = Just <| LookupRaw hashes
                    , label = Element.text name
                    }

            else
                case mime of
                    Text ->
                        Element.text <|
                            case decodeString bytes of
                                Nothing ->
                                    "internal error: corrupted text"

                                Just text ->
                                    text


decodeString : Bytes.Bytes -> Maybe String
decodeString bytes =
    D.decode (D.string (Bytes.width bytes)) bytes


jsonHumanMsg :
    { pubKeys : Dict.Dict String PubKeys
    , nonceBase : Int
    , myKeys : MySecretKeys
    }
    -> Int
    -> HumanMsg
    -> Result String Je.Value
jsonHumanMsg { pubKeys, nonceBase, myKeys } msgCounter { to, document } =
    case Dict.get to pubKeys of
        Nothing ->
            Err <| "bad recipient: \"" ++ to ++ "\""

        Just toKeys ->
            let
                b =
                    Base64.fromBytes

                docBytes =
                    E.encode <| encodeDocument document

                bad64 bad =
                    String.concat
                        [ "could not convert "
                        , bad
                        , " to base64"
                        ]
            in
            case
                ( ( b docBytes
                  , b myKeys.sign
                  , b <| makeNonce nonceBase msgCounter
                  )
                , ( b myKeys.encrypt
                  , b toKeys.encrypt
                  )
                )
            of
                ( ( Nothing, _, _ ), _ ) ->
                    Err <| bad64 "document"

                ( ( _, Nothing, _ ), _ ) ->
                    Err <| bad64 "my secret signing key"

                ( ( _, _, Nothing ), _ ) ->
                    Err <| bad64 "nonce"

                ( _, ( Nothing, _ ) ) ->
                    Err <| bad64 "my secret encryption key"

                ( _, ( _, Nothing ) ) ->
                    Err <| bad64 "recipient key"

                ( ( Just d, Just s, Just n ), ( Just e, Just t ) ) ->
                    Ok <|
                        Je.object
                            [ ( "document", Je.string d )
                            , ( "mySecretSign", Je.string s )
                            , ( "mySecretEncrypt", Je.string e )
                            , ( "toPublicEncrypt", Je.string t )
                            , ( "nonce", Je.string n )
                            ]


makeNonce : Int -> Int -> Bytes.Bytes
makeNonce nonceBase msgCounter =
    E.encode <| makeNonceHelp nonceBase msgCounter


makeNonceHelp : Int -> Int -> E.Encoder
makeNonceHelp nonceBase msgCounter =
    E.sequence
        [ E.unsignedInt32 Bytes.BE nonceBase
        , E.unsignedInt32 Bytes.BE msgCounter
        ]


encodeMsgs : MsgsToEncode -> Result String (List Je.Value)
encodeMsgs m =
    combineResults <|
        List.indexedMap
            (jsonHumanMsg
                { pubKeys = m.pubKeys
                , nonceBase = m.nonceBase
                , myKeys = m.myKeys
                }
            )
            m.msgs


combineResults : List (Result a b) -> Result a (List b)
combineResults results =
    combineResultsHelp results []


combineResultsHelp : List (Result a b) -> List b -> Result a (List b)
combineResultsHelp results accum =
    case results of
        [] ->
            Ok <| List.reverse accum

        (Ok o) :: rs ->
            combineResultsHelp rs (o :: accum)

        (Err err) :: _ ->
            Err err


type alias MsgsToEncode =
    { msgs : List HumanMsg
    , pubKeys : Dict.Dict String PubKeys
    , myKeys : MySecretKeys
    , nonceBase : Int
    }


decodeSecretKeys : Jd.Decoder MySecretKeys
decodeSecretKeys =
    Jd.map2 MySecretKeys
        (Jd.field "encrypt" decodeKey)
        (Jd.field "sign" decodeKey)


decodeKey : Jd.Decoder Bytes.Bytes
decodeKey =
    Jd.andThen decodeKeyHelp Jd.string


decodeKeyHelp : String -> Jd.Decoder Bytes.Bytes
decodeKeyHelp k =
    case Base64.toBytes k of
        Nothing ->
            Jd.fail "could not convert base64 string to bytes"

        Just bs ->
            Jd.succeed bs


decodeHome : D.Decoder Home
decodeHome =
    D.map5 Home
        (D.unsignedInt32 Bytes.BE)
        decodeMyKeys
        (list decodeHumanMsg)
        (D.map programsToDict <| list decodeProgram)
        decodePubKeys


decodeMyKeys : D.Decoder (Maybe MySecretKeys)
decodeMyKeys =
    D.andThen decodeMyKeysHelp D.unsignedInt8


decodeMyKeysHelp : Int -> D.Decoder (Maybe MySecretKeys)
decodeMyKeysHelp i =
    case i of
        0 ->
            D.succeed Nothing

        _ ->
            D.map Just <|
                D.map2 MySecretKeys
                    (D.bytes 32)
                    (D.bytes 64)


decodePubKeys : D.Decoder (Dict.Dict String PubKeys)
decodePubKeys =
    D.map Dict.fromList <| list decodePubKeysHelp


decodePubKeysHelp : D.Decoder ( String, PubKeys )
decodePubKeysHelp =
    D.map2 (\a b -> ( a, b ))
        sizedString
        decodePubKey


decodePubKey : D.Decoder PubKeys
decodePubKey =
    D.map2 PubKeys
        (D.bytes 32)
        (D.bytes 32)


encodeHome : Home -> E.Encoder
encodeHome h =
    E.sequence
        [ encodePrograms h.programs
        , encodeHumanMsgs h.outbox
        ]


encodeBytes : Bytes.Bytes -> E.Encoder
encodeBytes bs =
    E.sequence
        [ E.unsignedInt32 Bytes.BE <| Bytes.width bs
        , E.bytes bs
        ]


{-| Programs are encoded as a uint32 containing the number of
programs, followed by the programs, one after another.
-}
encodePrograms : Dict.Dict String Program -> E.Encoder
encodePrograms programs =
    let
        asList =
            Dict.values programs
    in
    E.sequence <|
        (E.unsignedInt32 Bytes.BE <| List.length asList)
            :: List.map encodeProgram asList


encodeHumanMsgs : List HumanMsg -> E.Encoder
encodeHumanMsgs msgs =
    E.sequence <|
        (E.unsignedInt32 Bytes.BE <| List.length msgs)
            :: List.map encodeHumanMsg msgs


encodeProgram : Program -> E.Encoder
encodeProgram program =
    E.sequence
        [ encodeSizedString program.code
        , encodeSizedString program.name
        , encodeSizedString program.description
        , encodeInbox program.inbox
        , encodeBlobs program.blobs
        , encodeSizedString program.typedIn
        ]


encodeInbox : List HumanMsg -> E.Encoder
encodeInbox msgs =
    E.sequence <|
        (E.unsignedInt32 Bytes.BE <| List.length msgs)
            :: List.map encodeHumanMsg msgs


encodeHumanMsg : HumanMsg -> E.Encoder
encodeHumanMsg { from, to, document } =
    E.sequence
        [ encodeSizedString from
        , encodeSizedString to
        , encodeDocument document
        ]


encodeDocument : Document -> E.Encoder
encodeDocument doc =
    case doc of
        Anon blob ->
            E.sequence
                [ E.unsignedInt8 0
                , encodeBlob blob
                ]

        Named name blob ->
            E.sequence
                [ E.unsignedInt8 1
                , encodeSizedString name
                , encodeBlob blob
                ]

        Ordering docs ->
            E.sequence <|
                [ E.unsignedInt8 2
                , E.unsignedInt32 Bytes.BE <| List.length docs
                ]
                    ++ List.map encodeDocument docs

        SmallString s ->
            E.sequence <|
                [ E.unsignedInt8 3
                , encodeSizedString s
                ]


encodeBlobs : List Blob -> E.Encoder
encodeBlobs blobs =
    E.sequence <|
        (E.unsignedInt32 Bytes.BE <| List.length blobs)
            :: List.map encodeBlob blobs


encodeBlob : Blob -> E.Encoder
encodeBlob (Blob mime hashes) =
    E.sequence <|
        encodeMime mime
            :: (N.toList <| N.map encodeSizedString hashes)


encodeMime : Mime -> E.Encoder
encodeMime mime =
    E.unsignedInt8 <|
        case mime of
            Text ->
                0


encodeSizedString : String -> E.Encoder
encodeSizedString str =
    E.sequence
        [ E.unsignedInt32 Bytes.BE (E.getStringWidth str)
        , E.string str
        ]


programsToDict : List Program -> Dict.Dict String Program
programsToDict programs =
    Dict.fromList <| List.map (\p -> ( p.name, p )) programs


{-| Pinched from the Bytes documentation.
-}
list : D.Decoder a -> D.Decoder (List a)
list decoder =
    D.unsignedInt32 Bytes.BE
        |> D.andThen
            (\len -> D.loop ( len, [] ) (listStep decoder))


{-| Pinched from the Bytes documentation.
-}
listStep :
    D.Decoder a
    -> ( Int, List a )
    -> D.Decoder (D.Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        D.succeed (D.Done xs)

    else
        D.map (\x -> D.Loop ( n - 1, x :: xs )) decoder


sizedString : D.Decoder String
sizedString =
    D.unsignedInt32 Bytes.BE
        |> D.andThen D.string


decodeProgram : D.Decoder Program
decodeProgram =
    map6 Program
        sizedString
        sizedString
        sizedString
        (list decodeHumanMsg)
        (list decodeBlob)
        sizedString


map6 :
    (a -> b -> c -> d -> e -> f -> result)
    -> D.Decoder a
    -> D.Decoder b
    -> D.Decoder c
    -> D.Decoder d
    -> D.Decoder e
    -> D.Decoder f
    -> D.Decoder result
map6 func decoderA decoderB decoderC decoderD decoderE decoderF =
    D.map func decoderA
        |> D.andThen (dmap decoderB)
        |> D.andThen (dmap decoderC)
        |> D.andThen (dmap decoderD)
        |> D.andThen (dmap decoderE)
        |> D.andThen (dmap decoderF)


dmap : D.Decoder a -> (a -> b) -> D.Decoder b
dmap a b =
    D.map b a


decodeHumanMsg : D.Decoder HumanMsg
decodeHumanMsg =
    D.map3 HumanMsg sizedString sizedString decodeDocument


decodeDocument : D.Decoder Document
decodeDocument =
    D.andThen decodeDocumentHelp D.unsignedInt8


decodeDocumentHelp : Int -> D.Decoder Document
decodeDocumentHelp typeNum =
    case typeNum of
        0 ->
            decodeAnon

        1 ->
            decodeNamed

        2 ->
            decodeOrdering

        _ ->
            D.fail


decodeNamed : D.Decoder Document
decodeNamed =
    D.map2 Named sizedString decodeBlob


decodeAnon : D.Decoder Document
decodeAnon =
    D.map Anon decodeBlob


decodeBlob : D.Decoder Blob
decodeBlob =
    D.map2 Blob decodeMime (decodeNonEmpty sizedString)


decodeNonEmpty : D.Decoder a -> D.Decoder (N.Nonempty a)
decodeNonEmpty decoder =
    D.andThen
        (\a ->
            case a of
                [] ->
                    D.fail

                x :: xs ->
                    D.succeed <| N.Nonempty x xs
        )
        (list decoder)


decodeMime : D.Decoder Mime
decodeMime =
    D.andThen decodeMimeHelp D.unsignedInt8


decodeMimeHelp : Int -> D.Decoder Mime
decodeMimeHelp i =
    case i of
        0 ->
            D.succeed Text

        _ ->
            D.fail


decodeOrdering : D.Decoder Document
decodeOrdering =
    D.map Ordering (list decodeDocument)


type alias Program =
    { code : String
    , name : String
    , description : String
    , inbox : List HumanMsg
    , blobs : List Blob
    , typedIn : String
    }


type Document
    = Anon Blob
    | Named String Blob
    | Ordering (List Document)
    | SmallString String


type Blob
    = Blob Mime (N.Nonempty String)


type Mime
    = Text

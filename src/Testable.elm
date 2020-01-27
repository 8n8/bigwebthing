module Testable exposing (..)

import Base64
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Dict
import Element
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Html
import Html.Attributes
import Json.Decode as Jd
import Json.Encode as Je
import List.Nonempty as N
import Parser as P exposing ((|.), (|=))
import Set


plus : Int -> Int
plus i =
    i + 5


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


serializeBlob : Blob -> Maybe String
serializeBlob blob =
    Base64.fromBytes <| E.encode (encodeBlob blob)


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


defaultHomeCode =
    """"Hello, this is the placeholder home app." print !"""


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
        , editorCheckbox model.editProgram
        , editor model
        ]


css : String -> String -> Element.Attribute Msg
css key value =
    Element.htmlAttribute <| Html.Attributes.style key value


sansSerif =
    Font.family [ Font.typeface "Ubuntu" ]


monospace =
    Font.family [ Font.typeface "Ubuntu Mono" ]



-- The wrapping is still not working nicely. See this Ellie for
-- an example: https://ellie-app.com/7PscK58sWRba1


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


editorCheckbox : Bool -> Element.Element Msg
editorCheckbox checked =
    Element.Input.checkbox []
        { onChange = ShowProgramCheckBox
        , icon = Element.Input.defaultCheckbox
        , checked = checked
        , label =
            Element.Input.labelRight [] <|
                Element.text "Tick this box if you're sure you want to edit the program."
        }


homeButton : Element.Element Msg
homeButton =
    Element.Input.button [ Element.padding 10, Border.width 1, Border.color <| Element.rgb255 0 0 0 ]
        { onPress = Just <| LaunchProgram "home"
        , label = Element.text "Launch home app"
        }


type alias ParserState =
    { mirror : MirrorState
    , types : TypeProgramState
    }


type alias MirrorState =
    { defs : Dict.Dict String TypeT
    , stack : List TypeT
    }


type alias TypeProgramState =
    { defs : Dict.Dict String TypeProgramValue
    , stack : List TypeProgramValue
    }


type TypeProgramValue
    = TType TypeT
    | TString String
    | Tblock (List (TypeProgramState -> TypeProgramState))


type BuiltInType
    = Bstring
    | Bblock (List (MirrorState -> Result String MirrorState))


type alias BuiltInTypeComp =
    Int


initParserState : ParserState
initParserState =
    { mirror =
        { defs = standardMirrorDefs
        , stack = []
        }
    , types =
        { defs = standardTypeDefs
        , stack = []
        }
    }


topProgramP : P.Parser Actions
topProgramP =
    P.succeed Tuple.second
        |= programP initParserState
        |. P.end


runProgram : Program -> ( Program, Maybe Document, List HumanMsg )
runProgram program =
    case P.run topProgramP program.code of
        Err deadEnds ->
            ( program
            , Just <| SmallString <| deadEndsToString deadEnds
            , []
            )

        Ok actions ->
            runElfs program actions []


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


programP : ParserState -> P.Parser ( ParserState, Actions )
programP p =
    P.loop ( p, [] ) programHelpP


programHelpP :
    ( ParserState, Actions )
    -> P.Parser (P.Step ( ParserState, Actions ) ( ParserState, Actions ))
programHelpP ( oldState, oldActions ) =
    P.oneOf
        [ P.succeed (\( p, a ) -> P.Loop ( p, a ++ oldActions ))
            |. whiteSpaceP
            |= elementP oldState
            |. whiteSpaceP
        , P.succeed () |> P.map (\_ -> P.Done ( oldState, oldActions ))
        ]


elementP : ParserState -> P.Parser ( ParserState, Actions )
elementP p =
    P.oneOf
        [ runBlockP p
        , stringPWrap p
        , defP p
        , programBlockP p

        -- , partialTypeCheckP p
        -- , fullTypeCheckP p
        , retrieveP p

        -- , switchP p
        ]



-- switchP : ParserState -> P.Parser ParserState
-- switchP =
--     P.map makeSwitchElfsElts <|
--         P.sequence
--             { start = "switch"
--             , separator = ","
--             , end = "endswitch"
--             , spaces = whiteSpaceP
--             , item = switchPartP
--             , trailing = P.Mandatory
--             }
-- makeSwitchElfsElts = Debug.todo "todo"
-- type alias SwitchPart =
--     { pattern : Pattern
--     , block : (List Elf, List Elt)
--     }


type Pattern
    = PString String
    | PVariable



-- switchPartP : P.Parser SwitchPart
-- switchPartP =
--     P.succeed SwitchPart
--         |= patternP
--         |. whiteSpaceP
--         |. P.token "->"
--         |. whiteSpaceP
--         |= programBlockP


patternP =
    Debug.todo ""


type alias Actions =
    List (ProgramState -> ProgramState)


stringPWrap : ParserState -> P.Parser ( ParserState, Actions )
stringPWrap p =
    P.succeed (stringPWrapHelp p)
        |= stringP


stringPWrapHelp : ParserState -> String -> ( ParserState, Actions )
stringPWrapHelp oldP s =
    let
        oldMirror =
            oldP.mirror

        newT =
            { builtIn = [], custom = [ Astring s ] }

        newMirror =
            { oldMirror | stack = newT :: oldMirror.stack }
    in
    ( { oldP | mirror = newMirror }, [ stringAction s ] )


stringAction : String -> ProgramState -> ProgramState
stringAction s p =
    { p | stack = Pstring s :: p.stack }



-- stringElt : String -> Dict.Dict String TypeT -> List TypeT -> Result String ( Dict.Dict String TypeT, List TypeT )
-- stringElt s dets stack =
--     Ok ( dets, {custom = [Pstring s], standard = []} :: stack )


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
        { start = Char.isAlpha
        , inner = \c -> Char.isAlphaNum c || c == '_'
        , reserved = Set.empty
        }


isUninteresting : Char -> Bool
isUninteresting char =
    char /= '\\' && char /= '"'


defP : ParserState -> P.Parser ( ParserState, Actions )
defP p =
    -- P.succeed (\var -> ( [ defElf var ], [ defElt var ] ))
    P.succeed identity
        |. P.token "="
        |. whiteSpaceP
        |= variable
        |> P.andThen (defPHelp p)


defPHelp : ParserState -> String -> P.Parser ( ParserState, Actions )
defPHelp p var =
    case p.mirror.stack of
        [] ->
            P.problem "a definition requires something on the stack"

        s :: tack ->
            if Dict.member var p.mirror.defs then
                P.problem <|
                    "multiple definitions of \""
                        ++ var
                        ++ "\""

            else
                let
                    oldMirror =
                        p.mirror

                    newDefs =
                        Dict.insert var s oldMirror.defs

                    newMirror =
                        { oldMirror
                            | defs = newDefs
                            , stack = tack
                        }
                in
                P.succeed
                    ( { p | mirror = newMirror }, [ defElf var ] )


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


defElt :
    String
    -> Dict.Dict String TypeT
    -> List TypeT
    -> Result String ( Dict.Dict String TypeT, List TypeT )
defElt var dets typestack =
    case typestack of
        [] ->
            Err "you need to put something on the stack before a definition"

        s :: tack ->
            if Dict.member var dets then
                Err <|
                    String.concat
                        [ "mutiple definitions of \""
                        , var
                        , "\""
                        ]

            else
                Ok ( Dict.insert var s dets, tack )


runBlockP : ParserState -> P.Parser ( ParserState, Actions )
runBlockP oldP =
    -- P.succeed ( [ runBlockElf ], [ runBlockElt ] )
    P.andThen (runBlockPHelp oldP) (P.keyword "!")


runBlockPHelp : ParserState -> () -> P.Parser ( ParserState, Actions )
runBlockPHelp oldP _ =
    case oldP.mirror.stack of
        [] ->
            P.problem "got \"!\" but the stack is empty"

        topType :: remainingTypes ->
            if isSubType topType { builtIn = [ Bblock [] ], custom = [] } then
                P.succeed ( oldP, [] )

            else
                P.problem <|
                    String.concat
                        [ "got \"!\": the stack should have a string "
                        , "on the top but it was a "
                        , showType topType
                        ]


showType : TypeT -> String
showType { custom, builtIn } =
    case ( custom, builtIn ) of
        ( [], [] ) ->
            "empty"

        ( [], bs ) ->
            String.concat
                [ "built in: "
                , String.join " + " <| List.map showBuiltIn bs
                ]

        ( cs, [] ) ->
            String.concat
                [ "custom: {"
                , String.join ", " <| List.map showCustom cs
                , "}"
                ]

        ( cs, bs ) ->
            String.concat
                [ "custom: {"
                , String.join ", " <| List.map showCustom cs
                , "} + "
                , "built in: "
                , String.join " + " <| List.map showBuiltIn bs
                ]


showCustom : TypeAtom -> String
showCustom t =
    case t of
        Astring s ->
            "string: \"" ++ showString s ++ "\""


showBuiltIn : BuiltInType -> String
showBuiltIn b =
    case b of
        Bstring ->
            "string"

        Bblock _ ->
            "block"


programBlockP : ParserState -> P.Parser ( ParserState, Actions )
programBlockP oldP =
    P.succeed (\( newP, actions ) -> ( blockUpdate newP, actions ))
        |. P.keyword "{"
        |= programP oldP
        |. P.keyword "}"


blockUpdate : ParserState -> ParserState
blockUpdate oldP =
    let
        oldMirror =
            oldP.mirror

        newMirror =
            { oldMirror
                | stack = { builtIn = [ Bblock [] ], custom = [] } :: oldMirror.stack
            }
    in
    { oldP | mirror = newMirror }



-- (\( elfs, elts ) -> ( [ blockElf elfs ], [ blockElt elts ] ))
-- blockElf : List Elf -> ProgramState -> ProgramState
-- blockElf elfs p =
--     { p | stack = Pblock elfs :: p.stack }
-- blockElt : List Elt -> Dict.Dict String TypeT -> List TypeT -> Result String ( Dict.Dict String TypeT, List TypeT )
-- blockElt elts dets stack =
--     Ok ( dets, {custom = [], standard = [Sblock elts]} :: stack )


runBlockElf : ProgramState -> ProgramState
runBlockElf s =
    case s.stack of
        [] ->
            { s | internalError = Just "! but empty stack" }

        (Pblock block) :: xs ->
            runElfsHelp block { s | stack = xs }

        x :: _ ->
            { s
                | internalError =
                    Just <|
                        String.concat
                            [ "expecting a block on top of the "
                            , "stack, but got"
                            , showProgVal x
                            ]
            }



-- runBlockElt :
--     Dict.Dict String TypeT
--     -> List TypeT
--     -> Result String ( Dict.Dict String TypeT, List TypeT )
-- runBlockElt dets typeStack =
--     case typeStack of
--         [] ->
--             Err "empty stack"
--
--         (TypeT [] (Sblock block)) :: xs ->
--             case runTypeChecksHelp block dets xs of
--                 Ok newStack ->
--                     Ok ( dets, newStack )
--
--                 Err errMsg ->
--                     Err errMsg
--
--         x :: _ ->
--             Err <|
--                 String.concat
--                     [ "bad stack: expecting block, but got "
--                     , showTypeVal x
--                     ]


showString : String -> String
showString s =
    String.reverse <| String.foldr showStringHelp "" s


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


showTypeVal : TypeT -> String
showTypeVal { custom, builtIn } =
    case ( custom, builtIn ) of
        ( [], [] ) ->
            "empty"

        ( [], b :: [] ) ->
            showBuiltInType b

        ( [], bs ) ->
            String.concat
                [ "{"
                , String.join ", " <|
                    List.map showBuiltInType bs
                , "}"
                ]

        ( cs, [] ) ->
            String.concat
                [ "{"
                , String.join ", " <| List.map showTypeAtom cs
                , "}"
                ]

        ( cs, bs ) ->
            String.concat
                [ "{"
                , String.join ", " <|
                    List.map showBuiltInType bs
                , ", "
                , String.join ", " <| List.map showTypeAtom cs
                , "}"
                ]


showBuiltInType : BuiltInType -> String
showBuiltInType builtIn =
    case builtIn of
        Bstring ->
            "string"

        Bblock _ ->
            "block"


showTypeStack : List TypeT -> String
showTypeStack typestack =
    String.concat
        [ "<"
        , String.join ", " <| List.map showTypeVal typestack
        , ">"
        ]



-- showTypeCheck : List TypeLiteral -> String
-- showTypeCheck typeCheck =
--     String.concat
--         [ "<"
--         , String.join ", " <| List.map showTypeLit typeCheck
--         , ">"
--         ]
-- showTypeLit : TypeLiteral -> String
-- showTypeLit typeLit =
--     case typeLit of
--         Tlstring ->
--             "string"
--
--         Tlblock ->
--             "block"


retrieveP : ParserState -> P.Parser ( ParserState, Actions )
retrieveP p =
    P.andThen (retrievePHelp p) variable


retrievePHelp : ParserState -> String -> P.Parser ( ParserState, Actions )
retrievePHelp p var =
    case Dict.get var p.mirror.defs of
        Nothing ->
            P.problem <| "no definition \"" ++ var ++ "\""

        Just lookedUp ->
            P.succeed ( p, [ makeRetrieveElf var ] )


makeRetrieveElf : String -> ProgramState -> ProgramState
makeRetrieveElf var p =
    case Dict.get var p.defs of
        Nothing ->
            { p
                | internalError =
                    Just <|
                        "no definition \""
                            ++ var
                            ++ "\""
            }

        Just f ->
            { p | stack = f :: p.stack }



-- makeRetrieveElt : String -> Dict.Dict String TypeT -> List TypeT -> Result String ( Dict.Dict String TypeT, List TypeT )
-- makeRetrieveElt var dets typestack =
--     case Dict.get var dets of
--         Nothing ->
--             Err <| String.concat [ "no definition \"", var, "\"" ]
--
--         Just t ->
--             Ok ( dets, t :: typestack )
-- fullTypeCheckP : P.Parser ( List Elf, List Elt )
-- fullTypeCheckP =
--     P.map (\ts -> ( [], [ makeFullTypeCheck ts ] )) <|
--         P.map List.reverse <|
--             P.sequence
--                 { start = "<"
--                 , separator = ","
--                 , end = ">"
--                 , spaces = whiteSpaceP
--                 , item = typeLiteralP
--                 , trailing = P.Mandatory
--                 }
-- typeCheckErr : List TypeLiteral -> List TypeT -> String
-- typeCheckErr expected got =
--     String.concat
--         [ "type stack does not match type declaration:\n"
--         , "expecting "
--         , showTypeCheck <| List.reverse expected
--         , "\n"
--         , "but got "
--         , showTypeStack <| List.reverse got
--         , "\n"
--         ]
-- makeFullTypeCheck : List TypeLiteral -> Dict.Dict String TypeT -> List TypeT -> Result String ( Dict.Dict String TypeT, List TypeT )
-- makeFullTypeCheck typeVals dets typeStack =
--     if equalT typeVals typeStack then
--         Ok ( dets, typeStack )
--
--     else
--         Err <| typeCheckErr typeVals typeStack
-- equalT : List TypeLiteral -> List TypeT -> Bool
-- equalT lits vals =
--     if List.length lits /= List.length vals then
--         False
--
--     else
--         List.all identity <| List.map2 equalThelp lits vals
-- isSubTypeOfLit : List ProgVal -> TypeLiteral -> Bool
-- isSubTypeOfLit values lit =
--     List.all (isSubTypeOfHelp lit) values


isSubType : TypeT -> TypeT -> Bool
isSubType sub master =
    List.any identity
        [ sub == master
        , List.all identity
            [ List.any identity
                [ customOfCustom sub.custom master.custom
                , customOfBuiltIn sub.custom master.builtIn
                ]
            , List.any identity
                [ builtInOfCustom sub.builtIn master.custom
                , builtInOfBuiltIn sub.builtIn master.builtIn
                ]
            ]
        ]


builtInOfCustom : List BuiltInType -> List TypeAtom -> Bool
builtInOfCustom sub master =
    False


builtInOfBuiltIn : List BuiltInType -> List BuiltInType -> Bool
builtInOfBuiltIn sub master =
    List.all (\s -> List.member s master) sub


customOfCustom : List TypeAtom -> List TypeAtom -> Bool
customOfCustom sub master =
    List.all (\s -> List.member s master) sub


customOfBuiltIn : List TypeAtom -> List BuiltInType -> Bool
customOfBuiltIn sub master =
    List.all (customOfBuiltInHelp master) sub


customOfBuiltInHelp : List BuiltInType -> TypeAtom -> Bool
customOfBuiltInHelp bs t =
    case t of
        Astring _ ->
            List.member Bstring bs



-- atomOfBuiltIn : BuiltInTypeSet -> TypeAtom -> Bool
-- atomOfBuiltIn master t =
--     case t of
--         Astring _ ->
--             containsString master
-- containsString : List BuiltInType -> Bool
-- containsString bs =
--     Set.map isString bs == Set.singleton True


isString : BuiltInType -> Bool
isString b =
    b == Bstring



-- isSubTypeOfHelp : TypeLiteral -> ProgVal -> Bool
-- isSubTypeOfHelp lit value =
--     case (lit, value) of
--         (Tlstring, Pstring _) ->
--             True
--
--         (Tlblock, Pblock _) ->
--             True
--
--         _ ->
--             False
--
--
-- equalThelp : TypeLiteral -> TypeT -> Bool
-- equalThelp lit val =
--     case ( lit, val ) of
--         ( Tlstring, Tcustom ts ) ->
--             isSubTypeOfLit ts Tlstring
--
--         ( Tlblock, Tstandard (Sblock _ )) ->
--             True
--
--         _ ->
--             False
--
--
-- makePartialTypeCheck : List TypeLiteral -> Dict.Dict String TypeT -> List TypeT -> Result String ( Dict.Dict String TypeT, List TypeT )
-- makePartialTypeCheck typeVals dets typeStack =
--     let
--         lenExpected =
--             List.length typeVals
--
--         lenActual =
--             List.length typeStack
--
--         candidate =
--             List.take lenExpected typeStack
--     in
--     if lenActual < lenExpected then
--         Err <|
--             String.concat
--                 [ "expecting "
--                 , String.fromInt lenExpected
--                 , " items on the stack, but only got "
--                 , String.fromInt lenActual
--                 ]
--
--     else if equalT typeVals candidate then
--         Ok ( dets, typeStack )
--
--     else
--         Err <| typeCheckErr typeVals candidate


{-| Some examples of type literals:

1.  string

2.  block

3.  { "hello", "hi", "hey" }

4.  { "aa" } + block

5.  block + string

6.  string - { "a very bad string", "a terrible string" }

7.  {}

-}



-- typeLiteralP : P.Parser TypeT
-- typeLiteralP =
--     P.oneOf [simpleStandard,
--     -- P.oneOf [ stringTypeP, blockTypeP ]
-- blockTypeP : P.Parser TypeLiteral
-- blockTypeP =
--     P.succeed Tlblock
--         |. P.keyword "block"
--
--
-- stringTypeP : P.Parser TypeLiteral
-- stringTypeP =
--     P.succeed Tlstring
--         |. P.keyword "string"
-- partialTypeCheckP : P.Parser ( List Elf, List Elt )
-- partialTypeCheckP =
--     P.map (\elt -> ( [], [ makePartialTypeCheck elt ] )) <|
--         P.map List.reverse <|
--             P.sequence
--                 { start = "<.."
--                 , separator = ","
--                 , end = ">"
--                 , spaces = whiteSpaceP
--                 , item = typeLiteralP
--                 , trailing = P.Optional
--                 }


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


oneWhitespaceP : P.Parser ()
oneWhitespaceP =
    P.oneOf
        [ P.lineComment "//"
        , P.multiComment "/*" "*/" P.NotNestable
        , P.spaces
        ]


initDoc =
    SmallString "this program produces no output"


runElfs :
    Program
    -> Actions
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
    ( newP.program, newP.rightDoc, newP.outbox )


type alias ProgramState =
    { program : Program
    , defs : Dict.Dict String ProgVal
    , stack : List ProgVal
    , rightDoc : Maybe Document
    , outbox : List HumanMsg
    , blobs : List Blob
    , internalError : Maybe String
    }



-- type alias Elf =
--     ProgramState -> ProgramState


runElfsHelp :
    Actions
    -> ProgramState
    -> ProgramState
runElfsHelp elfs s =
    case elfs of
        [] ->
            s

        e :: lfs ->
            let
                newS =
                    e s
            in
            runElfsHelp lfs newS


type alias TypeT =
    { builtIn : List BuiltInType
    , custom : List TypeAtom
    }


type TypeAtom
    = Astring String


showTypeAtom : TypeAtom -> String
showTypeAtom t =
    case t of
        Astring s ->
            "string: \"" ++ showString s ++ "\""



-- type alias Elt =
--     Dict.Dict String TypeT -> List TypeT -> Result String ( Dict.Dict String TypeT, List TypeT )


standardTypeDefs : Dict.Dict String TypeProgramValue
standardTypeDefs =
    Dict.fromList
        []


standardMirrorDefs : Dict.Dict String TypeT
standardMirrorDefs =
    Dict.fromList
        [ ( "print"
          , { builtIn = [ Bblock [ printMirror ] ]
            , custom = []
            }
          )
        ]


standardLibrary : Dict.Dict String ProgVal
standardLibrary =
    Dict.fromList
        [ ( "print", Pblock [ printElf ] )
        ]


printElf : ProgramState -> ProgramState
printElf p =
    case p.stack of
        (Pstring s) :: tack ->
            { p
                | rightDoc = Just <| print p.rightDoc s
                , stack = tack
            }

        stack ->
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


printNeeds =
    "\"print\" needs there to be a string on the stack, but it is "



-- isSubTypeOf : TypeT -> TypeT -> Bool
-- isSubTypeOf subset bigset =
--     customMatches subset.custom bigset &&
--     standardMatches subset.standard bigset
--
--
-- standardMatches : List StandardType -> TypeT -> Bool
-- standardMatches standard bigset =
--     List.all (standardMatchesHelp bigset) standard
--
--
-- standardMatchesHelp : TypeT -> StandardType -> Bool
-- standardMatchesHelp bigset standard =
--     List.any ((==) standard) bigset.standard
-- customMatches : List ProgVal -> TypeT -> Bool
-- customMatches custom bigset =
--     isSubList custom bigset.custom ||
--     valsMatchType custom bigset.standard
-- (Tcustom subValues, Tcustom bigValues) ->
--     isSubList subValues bigValues
-- (Tcustom subValues, Tstandard Sstring) ->
--     List.all isProgString subValues
-- (Tcustom subValues, Tstandard (Sblock _)) ->
--     List.all isProgBlock subValues
-- (Tstandard Sstring, Tcustom _) ->
--     False
-- (Tstandard Sstring, Tstandard Sstring) ->
--     True
-- (Tstandard Sstring, Tstandard (Sblock _)) ->
--     False
-- (Tstandard Sstring, Tcombined _ standards) ->
--     List.any ((==) Sstring) standards
-- (Tcombined _ (_::_), Tcustom _) ->
--     False
-- (Tcombined subCustom [], Tcustom bigCustom) ->
--     isSubList subCustom bigCustom
-- (Tcustom subCustom, Tcombined bigCustom bigStandards) ->
--     List.any identity
--         [ isSubList subCustom bigCustom
--         , List.any (valsMatchType subCustom) bigStandards
--         ]
-- (Tstandard (Sblock _), Tcustom bigCustom) ->
--     List.any isProgBlock bigCustom
-- (Tstandard (Sblock _), Tstandard (Sblock _)) ->
--     True
-- (Tstandard (Sblock _), Tcombined bigCustom bigStandard) ->
--     List.any identity
--         [ List.any isProgBlock bigCustom
--         , List.any isStandardBlock bigStandard
--         ]
-- (Tstandard (Sblock _), Tstandard Sstring) ->
--     False
-- (Tcombined _ [], Tstandard _) ->
--     False
-- (Tcombined subValues [], Tcombined bigValues []) ->
--     isSubList subValues bigValues
-- isStandardBlock : StandardType -> Bool
-- isStandardBlock s =
--     case s of
--         Sblock _ ->
--             True
--
--         _ ->
--             False
-- valsMatchType : List ProgVal -> StandardType -> Bool
-- valsMatchType values t =
--     List.all (valMatchesType t) values
--
--
-- valMatchesType : StandardType -> ProgVal -> Bool
-- valMatchesType t value =
--     case (t, value) of
--         (Sstring, Pstring _) ->
--             True
--
--         (Sblock _, Pblock _) ->
--             True
--
--         _ ->
--             False
--
--
-- isProgBlock : ProgVal -> Bool
-- isProgBlock t =
--     case t of
--         Pblock _ ->
--             True
--
--         _ ->
--             False
--
--
-- isProgString : ProgVal -> Bool
-- isProgString t =
--     case t of
--         Pstring _ ->
--             True
--
--         _ ->
--             False
--
--
-- isSubList : List ProgVal -> List ProgVal -> Bool
-- isSubList sub big =
--     List.all (\s -> List.member s big) sub


printMirror : MirrorState -> Result String MirrorState
printMirror { defs, stack } =
    case stack of
        [] ->
            Err <| printNeeds ++ "empty"

        s :: tack ->
            if isSubType s { builtIn = [ Bstring ], custom = [] } then
                Ok { defs = defs, stack = tack }

            else
                Err <| printNeeds ++ showTypeStack stack



-- runTypeChecks : List Elt -> Maybe String
-- runTypeChecks elts =
--     case runTypeChecksHelp elts standardTypes [] of
--         Ok [] ->
--             Nothing
--
--         Ok ts ->
--             Just <|
--                 String.concat
--                     [ "typestack should be empty at end of program, but "
--                     , "got "
--                     , showTypeStack ts
--                     ]
--
--         Err err ->
--             Just err
-- runTypeChecksHelp :
--     List Elt
--     -> Dict.Dict String TypeT
--     -> List TypeT
--     -> Result String (List TypeT)
-- runTypeChecksHelp elts dets typeStack =
--     case elts of
--         [] ->
--             Ok typeStack
--
--         e :: lts ->
--             case e dets typeStack of
--                 Err errMsg ->
--                     Err errMsg
--
--                 Ok ( newDets, newTypeStack ) ->
--                     runTypeChecksHelp lts newDets newTypeStack


type alias HumanMsg =
    { from : String
    , to : String
    , document : Document
    }


type ProgVal
    = Pstring String
    | Pblock (List (ProgramState -> ProgramState))


showProgVal : ProgVal -> String
showProgVal p =
    case p of
        Pstring s ->
            "string: " ++ s

        Pblock bs ->
            String.concat
                [ "block of length "
                , String.fromInt <| List.length bs
                ]


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

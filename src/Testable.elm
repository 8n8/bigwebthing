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
    """"Hello, this is the placeholder home app." print ."""


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
        --, editorCheckbox model.editProgram
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


standardTypeProgramDefs : Dict.Dict String TypeProgramValue
standardTypeProgramDefs =
    Dict.fromList
        [ ( "block", Ttype { custom = [], standard = [ Sblock [] ] } )
        , ( "string", Ttype { custom = [], standard = [ Sstring ] } )
        , ( "topcheck", Tblock [ topcheck ] )
        , ( "[]", Tlist [] )
        , ( ",", Tblock [ typeCons ] )
        ]


typeConsHelp : String
typeConsHelp =
    "to prepend to a list the top of the stack should be the new element, and the second item in the stack should be the list"


typeCons : TypeState -> Result String TypeProgramOut
typeCons t =
    case t.stack of
        [] ->
            Err <| "empty stack, but " ++ typeConsHelp

        _ :: [] ->
            Err <| "only one thing in stack, but " ++ typeConsHelp

        toPrepend :: (Tlist ts) :: remainsOfStack ->
            Ok
                { state = { t | stack = Tlist (toPrepend :: ts) :: remainsOfStack }
                , elts = []
                }

        _ ->
            Err <| "bad stack: " ++ typeConsHelp


topcheck : TypeState -> Result String TypeProgramOut
topcheck t =
    case t.stack of
        [] ->
            Err "a partial type check needs a list on top of the stack, but it is empty"

        (Tlist ts) :: tack ->
            case getTypes ts of
                Nothing ->
                    Err "a partial type check needs a list of types only, but this list has other things in it"

                Just types ->
                    Ok
                        { state = { t | stack = tack }
                        , elts = [ partialTypeCheckElt types ]
                        }

        topItem :: _ ->
            Err <|
                String.concat
                    [ "a partial type check needs the top item on the "
                    , "stack to be a list of types, but it is a "
                    , showTypeProgramType topItem
                    ]


initTypeProgramState : TypeState
initTypeProgramState =
    { defs = standardTypeProgramDefs
    , stack = []
    }


topProgramP : P.Parser ParserOut
topProgramP =
    P.succeed identity
        |= programP initTypeProgramState
        |. P.end


runProgram : Program -> ( Program, Maybe Document, List HumanMsg )
runProgram program =
    case P.run topProgramP program.code of
        Err deadEnds ->
            ( program
            , Just <| SmallString <| deadEndsToString deadEnds
            , []
            )

        Ok { elfs, elts } ->
            case runTypeChecks elts of
                Just errMsg ->
                    ( program, Just <| SmallString ("type error: " ++ errMsg), [] )

                Nothing ->
                    runElfs program elfs []


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


programP : TypeState -> P.Parser ParserOut
programP t =
    P.loop { typeState = t, elfs = [], elts = [] } programHelpP


programHelpP : ParserOut -> P.Parser (P.Step ParserOut ParserOut)
programHelpP p =
    P.oneOf
        [ P.succeed (\{ typeState, elfs, elts } -> P.Loop { elfs = p.elfs ++ elfs, elts = p.elts ++ elts, typeState = typeState })
            |. whiteSpaceP
            |= elementP p.typeState
            |. whiteSpaceP
        , P.succeed () |> P.map (\_ -> P.Done p)
        ]


elementP : TypeState -> P.Parser ParserOut
elementP t =
    let
        w p =
            P.map (\( elfs, elts ) -> { elfs = elfs, elts = elts, typeState = t }) p
    in
    P.oneOf
        [ w runBlockP
        , w stringPWrap
        , w defP
        , programBlockP t
        , topTypeLangP
        , w retrieveP
        ]


topTypeLangP : P.Parser ParserOut
topTypeLangP =
    P.succeed identity
        |. P.token "<"
        |= typeLangP
        |. P.token ">"
        |> P.andThen topTypeLangPhelp


initialTypeState =
    { defs = standardTypeProgramDefs
    , stack = []
    }


topTypeLangPhelp : List (TypeState -> Result String TypeProgramOut) -> P.Parser ParserOut
topTypeLangPhelp actions =
    case runTypeBlock initialTypeState actions of
        Err err ->
            P.problem err

        Ok { state, elts } ->
            P.succeed { typeState = state, elfs = [], elts = elts }


typeLangP : P.Parser (List Ety)
typeLangP =
    P.loop [] typeLangHelpP


type alias Ety =
    TypeState -> Result String TypeProgramOut


typeLangHelpP : List Ety -> P.Parser (P.Step (List Ety) (List Ety))
typeLangHelpP oldEtys =
    P.oneOf
        [ P.succeed (\etys -> P.Loop (oldEtys ++ etys))
            |. whiteSpaceP
            |= typeElementP
            |. whiteSpaceP
        , P.succeed () |> P.map (\_ -> P.Done oldEtys)
        ]


typeElementP : P.Parser (List Ety)
typeElementP =
    P.oneOf
        [ typeRunBlockP
        , typeStringP
        , typeDefP
        , typeBlockP
        -- , partialTypeCheckP
        , fullTypeCheckP
        , customTypeWrapP
        , typeRetrieveP
        ]


fullTypeCheckP : P.Parser (List Ety)
fullTypeCheckP =
    P.succeed [ fullTypeCheckEty ]
        |. P.keyword "fullcheck"


fullTypeCheckEty : Ety
fullTypeCheckEty t =
    case t.stack of
        [] ->
            Err "stack is empty but should be a list"

        (Tlist ts) :: remainsOfStack ->
            case getTypes ts of
                Nothing ->
                    Err "list on top of stack contains things that are not types"

                Just types ->
                    Ok
                        { state = { t | stack = remainsOfStack }
                        , elts = [ fullTypeCheckElt types ]
                        }

        topItem :: _ ->
            Err <|
                String.concat
                    [ "a full type check needs the top item on the stack to be a list of types, but it is a "
                    , showTypeProgramType topItem
                    ]


fullTypeCheckElt : List Type -> Elt
fullTypeCheckElt t state =
    if equalTypeStacks t state.stack then
        Ok state

    else
        Err
            { state = state
            , message = failedFullMessage t state.stack
            }


failedFullMessage : List Type -> List Type -> String
failedFullMessage expected actual =
    String.concat
        [ "failed full type check:\n"
        , "expecting: "
        , showTypeStack expected
        , "\n"
        , "but got: "
        , showTypeStack actual
        ]


partialTypeCheckP : P.Parser (List Ety)
partialTypeCheckP =
    P.succeed [ partialTypeCheckEty ]
        |. P.keyword "topcheck"


partialTypeCheckEty : Ety
partialTypeCheckEty t =
    case t.stack of
        [] ->
            Err "stack is empty but should have a list on the top"

        (Tlist ts) :: remainsOfStack ->
            case getTypes ts of
                Nothing ->
                    Err "list on top of stack has things other than types in it"

                Just types ->
                    Ok
                        { state = { t | stack = remainsOfStack }
                        , elts = [ partialTypeCheckElt types ]
                        }

        topItem :: _ ->
            Err <| "top item on stack is not a list, it is a " ++ showTypeProgramType topItem


getTypes : List TypeProgramValue -> Maybe (List Type)
getTypes values =
    Maybe.Extra.traverse getType values


getType : TypeProgramValue -> Maybe Type
getType typeProgramValue =
    case typeProgramValue of
        Ttype t ->
            Just t

        Tlist ts ->
            listToType ts

        _ ->
            Nothing


listToType : List TypeProgramValue -> Maybe Type
listToType l =
    case Maybe.Extra.traverse valueToAtom l of
        Nothing ->
            Nothing

        Just ts ->
            Just { custom = ts, standard = [] }


valueToAtom : TypeProgramValue -> Maybe TypeAtom
valueToAtom value =
    case value of
        Tstring s ->
            Just <| Astring s

        _ ->
            Nothing


partialTypeCheckElt : List Type -> Elt
partialTypeCheckElt t state =
    let
        lengthExpected =
            List.length t

        lengthActual =
            List.length state.stack

        relevant =
            List.take lengthExpected state.stack
    in
    if lengthExpected == 0 then
        Err
            { message =
                "there's no point in an empty partial type check"
            , state = state
            }

    else if lengthExpected > lengthActual then
        Err
            { message = failedPartialMessage t state.stack
            , state = state
            }

    else if equalTypeStacks relevant t then
        Ok state

    else
        Err
            { message = failedPartialMessage t state.stack
            , state = state
            }


failedPartialMessage : List Type -> List Type -> String
failedPartialMessage expected actual =
    String.concat
        [ "failed partial type check:\n"
        , "expecting: "
        , showTypeStack expected
        , "\n"
        , "but got: "
        , showTypeStack actual
        ]


typeRetrieveP : P.Parser (List Ety)
typeRetrieveP =
    P.succeed (\v -> [ typeRetrieveEty v ])
        |= variable


typeRetrieveEty : String -> Ety
typeRetrieveEty var t =
    case Dict.get var t.defs of
        Nothing ->
            Err <| "no definition \"" ++ var ++ "\""

        Just definition ->
            Ok <|
                { state = { t | stack = definition :: t.stack }
                , elts = []
                }


typeBlockP : P.Parser (List Ety)
typeBlockP =
    P.succeed (\etys -> [ typeBlockEty etys ])
        |. P.token "{"
        |= typeLangP
        |. P.token "}"


typeBlockEty : List Ety -> Ety
typeBlockEty etys t =
    Ok
        { state = { t | stack = Tblock etys :: t.stack }
        , elts = []
        }


typeDefP : P.Parser (List Ety)
typeDefP =
    P.succeed (\v -> [ typeDefEty v ])
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
                        }
                    , elts = []
                    }


typeStringP : P.Parser (List Ety)
typeStringP =
    P.succeed (\s -> [ stringEty s ])
        |= stringP


stringEty : String -> Ety
stringEty s t =
    Ok { state = { t | stack = Tstring s :: t.stack }, elts = [] }


typeRunBlockP : P.Parser (List Ety)
typeRunBlockP =
    P.succeed [ typeRunBlockEty ]
        |. P.token "."


typeRunBlockEty : TypeState -> Result String TypeProgramOut
typeRunBlockEty t =
    case t.stack of
        [] ->
            Err "got \".\" but stack is empty"

        (Tblock b) :: tack ->
            runTypeBlock { t | stack = tack } b

        top :: _ ->
            Err <|
                String.concat
                    [ "expecting a block on top of the stack, but "
                    , "got "
                    , showTypeProgramValue top
                    ]


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


stringPWrap : P.Parser ( List Elf, List Elt )
stringPWrap =
    P.succeed (\start s end -> ( [ stringElf s ], [ stringElt start end s ] ))
        |= P.getPosition
        |= stringP
        |= P.getPosition


stringElf : String -> ProgramState -> ProgramState
stringElf s p =
    { p | stack = Pstring s :: p.stack }


stringElt : ( Int, Int ) -> ( Int, Int ) -> String -> Elt
stringElt start end string s =
    Ok
        { s
            | stack = { standard = [], custom = [Astring string] } :: s.stack
            , startPosition = start
            , endPosition = end
        }


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


defP : P.Parser ( List Elf, List Elt )
defP =
    P.succeed (\start var end -> ( [ defElf var ], [ defElt var start end ] ))
        |= P.getPosition
        |. P.token "="
        |. whiteSpaceP
        |= variable
        |= P.getPosition


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
                    }


runBlockP : P.Parser ( List Elf, List Elt )
runBlockP =
    P.succeed (\b e -> ( [ runBlockElf ], [ runBlockElt b e ] ))
        |= P.getPosition
        |. P.token "."
        |= P.getPosition


programBlockP : TypeState -> P.Parser ParserOut
programBlockP t =
    P.succeed (\start { elfs, elts, typeState } end -> { elfs = [ blockElf elfs ], elts = [ blockElt elts start end ], typeState = typeState })
        |= P.getPosition
        |. P.keyword "{"
        |= programP t
        |. P.keyword "}"
        |= P.getPosition


blockElf : List Elf -> ProgramState -> ProgramState
blockElf elfs p =
    { p | stack = Pblock elfs :: p.stack }


blockElt : List Elt -> ( Int, Int ) -> ( Int, Int ) -> Elt
blockElt elts start end s =
    Ok
        { s
            | stack = { standard = [ Sblock elts ], custom = [] } :: s.stack
            , startPosition = start
            , endPosition = end
        }


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
                            , "stack, but got"
                            , showProgVal x
                            ]
            }


runBlockElt : ( Int, Int ) -> ( Int, Int ) -> Elt
runBlockElt start end state =
    case state.stack of
        [] ->
            Err
                { state = newPos state start end, message = "empty stack" }

        t :: xs ->
            case getBlock t.standard of
                Nothing ->
                    Err
                        { state = newPos state start end
                        , message =
                            String.concat
                                [ "bad stack: expecting block, but "
                                , "got "
                                , showTypeVal t
                                ]
                        }

                Just block ->
                    case runTypeChecksHelp block { state | stack = xs, startPosition = start, endPosition = end } of
                        Err err -> Err err
                        Ok ok ->
                            Ok { ok | defs = state.defs }


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


getBlock : List StandardType -> Maybe (List Elt)
getBlock standards =
    List.head <| justs <| List.map getBlockHelp standards


getBlockHelp : StandardType -> Maybe (List Elt)
getBlockHelp s =
    case s of
        Sblock b ->
            Just b

        _ ->
            Nothing


showTypeAtom : TypeAtom -> String
showTypeAtom t =
    case t of
        Astring s ->
            "string: \"" ++ showString s ++ "\""


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
showTypeVal { custom, standard } =
    case ( custom, standard ) of
        ( [], [] ) ->
            "empty"

        ( [], b :: [] ) ->
            showStandardType b

        ( [], bs ) ->
            String.concat
                [ "{"
                , String.join ", " <|
                    List.map showStandardType bs
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
                    List.map showStandardType bs
                , ", "
                , String.join ", " <| List.map showTypeAtom cs
                , "}"
                ]


showStandardType : StandardType -> String
showStandardType standard =
    case standard of
        Sstring ->
            "string"

        Sblock _ ->
            "block"


showTypeStack : List Type -> String
showTypeStack typestack =
    String.concat
        [ "<"
        , String.join ", " <| List.map showTypeVal typestack
        , ">"
        ]


retrieveP : P.Parser ( List Elf, List Elt )
retrieveP =
    P.succeed retrievePhelp
        |= P.getPosition
        |= variable
        |= P.getPosition


retrievePhelp : ( Int, Int ) -> String -> ( Int, Int ) -> ( List Elf, List Elt )
retrievePhelp start var end =
    ( [ makeRetrieveElf var ], [ makeRetrieveElt var start end ] )


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


makeRetrieveElt : String -> ( Int, Int ) -> ( Int, Int ) -> Elt
makeRetrieveElt var start end state =
    case Dict.get var state.defs of
        Nothing ->
            Err
                { message = "no definition \"" ++ var ++ "\""
                , state = newPos state start end
                }

        Just t ->
            Ok <| newPos { state | stack = t :: state.stack } start end


customTypeWrapP : P.Parser (List Ety)
customTypeWrapP =
    P.succeed [ customTypeEty ]
        |. P.keyword "totype"


customTypeEty : Ety
customTypeEty t =
    case t.stack of
        [] ->
            Err "empty stack, but it should have a list of types atoms"

        (Tlist candidates) :: tack ->
            case toTypeAtoms candidates of
                Err badElements ->
                    Err <|
                        String.concat
                            [ "cannot convert these elements in the list to a type: "
                            , String.join ", " <| List.map showTypeProgramValue badElements
                            ]

                Ok typeAtoms ->
                    let
                        type_ =
                            { custom = typeAtoms, standard = [] }

                        newStack =
                            Ttype type_ :: tack
                    in
                    Ok
                        { state = { t | stack = newStack }
                        , elts = []
                        }

        bad :: _ ->
            Err <|
                String.concat
                    [ "totype needs the top of the stack to be a list, but it is "
                    , showTypeProgramValue bad
                    ]


customTypeT : TypeState -> P.Parser TypeState
customTypeT t =
    case t.stack of
        [] ->
            P.problem "totype needs something on the stack, but it is empty"

        (Tlist candidates) :: tack ->
            case toTypeAtoms candidates of
                Err badElements ->
                    P.problem <|
                        String.concat
                            [ "cannot convert these elements in the list to a type: "
                            , String.join ", " <| List.map showTypeProgramValue badElements
                            ]

                Ok typeAtoms ->
                    let
                        type_ =
                            { custom = typeAtoms, standard = [] }

                        newStack =
                            Ttype type_ :: tack
                    in
                    P.succeed { t | stack = newStack }

        bad :: _ ->
            P.problem <|
                String.concat
                    [ "totype needs the top of the stack to be a list, "
                    , "but it is "
                    , showTypeProgramValue bad
                    ]


toTypeAtoms : List TypeProgramValue -> Result (List TypeProgramValue) (List TypeAtom)
toTypeAtoms candidates =
    let
        ( good, bad ) =
            toTypeAtomsHelp candidates [] []
    in
    if List.length bad == 0 then
        Ok good

    else
        Err bad


toTypeAtomsHelp : List TypeProgramValue -> List TypeAtom -> List TypeProgramValue -> ( List TypeAtom, List TypeProgramValue )
toTypeAtomsHelp remaining goodAccum badAccum =
    case remaining of
        [] ->
            ( goodAccum, badAccum )

        r :: emaining ->
            case r of
                Ttype _ ->
                    toTypeAtomsHelp emaining goodAccum (r :: badAccum)

                Tstring s ->
                    toTypeAtomsHelp
                        emaining
                        (Astring s :: goodAccum)
                        badAccum

                Tblock _ ->
                    toTypeAtomsHelp emaining goodAccum (r :: badAccum)

                Tlist _ ->
                    toTypeAtomsHelp emaining goodAccum (r :: badAccum)


blockTypeP : P.Parser Type
blockTypeP =
    P.succeed { standard = [ Sblock [] ], custom = [] }
        |. P.keyword "block"


stringTypeP : P.Parser Type
stringTypeP =
    P.succeed { standard = [ Sstring ], custom = [] }
        |. P.keyword "string"


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


initDoc : Document
initDoc =
    SmallString "this program produces no output"


runElfs :
    Program
    -> List Elf
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


type alias Elf =
    ProgramState -> ProgramState


runElfsHelp :
    List Elf
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


type alias TypeState =
    { defs : Dict.Dict String TypeProgramValue
    , stack : List TypeProgramValue
    }


type alias TypeProgramOut =
    { state : TypeState
    , elts : List Elt
    }


type TypeProgramValue
    = Ttype Type
    | Tstring String
    | Tblock (List (TypeState -> Result String TypeProgramOut))
    | Tlist (List TypeProgramValue)


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


type alias Type =
    { custom : List TypeAtom
    , standard : List StandardType
    }


type TypeAtom
    = Astring String


type StandardType
    = Sblock (List Elt)
    | Sstring


equalTypeStacks : List Type -> List Type -> Bool
equalTypeStacks t1 t2 =
    (List.length t1 == List.length t2)
        && (List.all identity <| List.map2 isSubType t1 t2)


equalType : Type -> Type -> Bool
equalType t1 t2 =
    equalStandards t1.standard t2.standard
        && equalCustoms t1.custom t2.custom


equalCustoms : List TypeAtom -> List TypeAtom -> Bool
equalCustoms t1 t2 =
    List.all identity <| List.map2 equalCustom t1 t2


equalCustom : TypeAtom -> TypeAtom -> Bool
equalCustom t1 t2 =
    t1 == t2


equalStandards : List StandardType -> List StandardType -> Bool
equalStandards b1 b2 =
    List.all identity <| List.map2 equalStandard b1 b2


equalStandard : StandardType -> StandardType -> Bool
equalStandard b1 b2 =
    case ( b1, b2 ) of
        ( Sstring, Sstring ) ->
            True

        ( Sblock _, Sblock _ ) ->
            True

        _ ->
            False


type alias Elt =
    EltState -> EltOut


type alias TypeError =
    { state : EltState, message : String }


type alias EltOut =
    Result TypeError EltState


type alias EltState =
    { startPosition : ( Int, Int )
    , endPosition : ( Int, Int )
    , stack : List Type
    , defs : Dict.Dict String Type
    }


standardTypes : Dict.Dict String Type
standardTypes =
    Dict.fromList
        [ ( "print", { standard = [ Sblock [ printElt ] ], custom = [] } )
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
            if isSubType s { standard = [ Sstring ], custom = [] } then
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
    List.any identity
        [ sub == master
        , List.all identity
            [ List.any identity
                [ customOfCustom sub.custom master.custom
                , customOfStandard sub.custom master.standard
                ]
            , List.any identity
                [ standardOfCustom sub.standard master.custom
                , standardOfStandard sub.standard master.standard
                ]
            ]
        ]


standardOfCustom : List StandardType -> List TypeAtom -> Bool
standardOfCustom _ _ =
    False


standardOfStandard : List StandardType -> List StandardType -> Bool
standardOfStandard sub master =
    List.all (\s -> List.member s master) sub


customOfCustom : List TypeAtom -> List TypeAtom -> Bool
customOfCustom sub master =
    List.all (\s -> List.member s master) sub


customOfStandard : List TypeAtom -> List StandardType -> Bool
customOfStandard sub master =
    List.all (customOfStandardHelp master) sub


customOfStandardHelp : List StandardType -> TypeAtom -> Bool
customOfStandardHelp bs t =
    case t of
        Astring _ ->
            List.member Sstring bs


initEltState =
    { startPosition = ( 0, 0 )
    , endPosition = ( 0, 0 )
    , stack = []
    , defs = standardTypes
    }


runTypeChecks : List Elt -> Maybe String
runTypeChecks elts =
    case runTypeChecksHelp elts initEltState of
        Ok { stack } ->
            case stack of
                [] ->
                    Nothing

                ts ->
                    Just <|
                        String.concat
                            [ "typestack should be empty at end of program, but "
                            , "got "
                            , showTypeStack ts
                            ]

        Err err ->
            Just <| errorToString err


errorToString : TypeError -> String
errorToString { state, message } =
    message


runTypeChecksHelp : List Elt -> EltState -> EltOut
runTypeChecksHelp elts state =
    case elts of
        [] ->
            Ok state

        e :: lts ->
            case e state of
                Err errMsg ->
                    Err errMsg

                Ok newState ->
                    runTypeChecksHelp lts newState


type alias HumanMsg =
    { from : String
    , to : String
    , document : Document
    }


type ProgVal
    = Pstring String
    | Pblock (List Elf)


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

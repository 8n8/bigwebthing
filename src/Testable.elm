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
import Maybe.Extra
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


sansSerif = Font.family [ Font.typeface "Ubuntu" ]
monospace = Font.family [ Font.typeface "Ubuntu Mono" ]


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
                    Element.Input.labelAbove [sansSerif] <|
                        Element.text "This box contains the program:"
                , spellcheck = False
                }


editorCheckbox : Bool -> Element.Element Msg
editorCheckbox checked =
    Element.Input.checkbox []
        { onChange = ShowProgramCheckBox
        , icon = Element.Input.defaultCheckbox
        , checked = checked
        , label = Element.Input.labelRight [] <|
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
        [
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

        Ok {elfs, elts} ->
            case runTypeChecks elts of
                Just errMsg ->
                    ( program, Just <| SmallString errMsg, [] )

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
    P.loop {typeState = t, elfs = [], elts = []} programHelpP


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
        w p = P.map (\(elfs, elts) -> {elfs = elfs, elts = elts, typeState = t}) p
    in
        P.oneOf
            [ w runBlockP
            , w stringPWrap
            , w defP
            , programBlockP t
            -- , partialTypeCheckP
            -- , fullTypeCheckP
            , topTypeLangP t
            , w retrieveP
            -- , switchP
            ]


topTypeLangP : TypeState -> P.Parser ParserOut
topTypeLangP t =
    P.succeed identity
        |. P.token "<"
        |= typeLangP t
        |. P.token ">"


typeLangP : TypeState -> P.Parser ParserOut
typeLangP t =
    P.loop {typeState = t, elfs = [], elts = []} typeLangHelpP


typeLangHelpP : ParserOut -> P.Parser (P.Step ParserOut ParserOut)
typeLangHelpP p =
    P.oneOf
        [ P.succeed (\{ typeState, elfs, elts } -> P.Loop {elfs=p.elfs ++ elfs, elts = p.elts ++ elts, typeState = typeState})
            |. whiteSpaceP
            |= typeElementP p.typeState
            |. whiteSpaceP
        , P.succeed () |> P.map (\_ -> P.Done p)
        ]


typeElementP : TypeState -> P.Parser ParserOut
typeElementP t =
    let
        w p = P.map (\newT -> {typeState = newT, elfs = [], elts = []}) (p t)
    in P.oneOf
        [ typeRunBlockP t
        , w typeStringP
        , w typeDefP
        , w typeBlockP
        , partialTypeCheckP t
        , w typeRetrieveP
        , w typeListP
        ]


typeListP : TypeState -> P.Parser TypeState
typeListP t =
    P.sequence
        { start = "["
        , separator = ","
        , end = "]"
        , spaces = whiteSpaceP
        , item = typeListLiteralElementP t
        , trailing = P.Mandatory
        }
    |> P.map (\l -> {t|stack = Tlist l :: t.stack})


typeListLiteralElementP : TypeState -> P.Parser TypeProgramValue
typeListLiteralElementP t =
    P.oneOf
        [ P.map Tstring stringP
        , P.map Tblock (typeBlockHelpP t)
        , P.map Ttype (typeLiteralP t)
        ]


partialTypeCheckP : TypeState -> P.Parser ParserOut
partialTypeCheckP t =
    P.succeed identity
        |. P.keyword "topcheck"
        |= partialTypeCheckHelp t


partialTypeCheckHelp : TypeState -> P.Parser ParserOut
partialTypeCheckHelp t =
    case t.stack of
        [] ->
            P.problem <| "a partial type check needs a list on the top of the stack, but it is empty"
        (Tlist ts) :: remainsOfStack ->
            case getTypes ts of
                Nothing ->
                    P.problem <| "a partial type check needs a list of types only, but this list has other things in it"

                Just types -> P.succeed
                    { typeState = { t | stack = remainsOfStack }
                    , elfs = []
                    , elts = [partialTypeCheckElt types]
                    }

        topItem :: _ ->
            P.problem <| String.concat
                [ "a partial type check needs the top item on the "
                , "stack to be a list of types, but it is a "
                , showTypeProgramType topItem
                ]
                

getTypes : List TypeProgramValue -> Maybe (List Type)
getTypes values =
    Maybe.Extra.traverse getType values


getType : TypeProgramValue -> Maybe Type
getType typeProgramValue =
    case typeProgramValue of
        Ttype t ->
            Just t

        _ ->
            Nothing

          
partialTypeCheckElt : List Type -> Dict.Dict String Type -> List Type -> Result String (Dict.Dict String Type, List Type)
partialTypeCheckElt t dets stack =
    let
        lengthExpected = List.length t
        lengthActual = List.length stack
        relevant = List.take lengthExpected stack
    in
        if lengthExpected > lengthActual then
            Err <| failedPartialMessage t stack 
        else
            if equalTypeStacks t relevant then
                Ok (dets, stack)

            else
                Err <| failedPartialMessage t stack


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


typeRetrieveP : TypeState -> P.Parser TypeState
typeRetrieveP t =
    P.succeed identity
        |= variable
        |> P.andThen (typeRetrievePhelp t)


typeRetrievePhelp : TypeState -> String -> P.Parser TypeState
typeRetrievePhelp t var =
    case Dict.get var t.defs of
        Nothing ->
            P.problem <| "no definition \"" ++ var ++ "\""

        Just definition ->
            P.succeed { t | stack = definition :: t.stack }


typeBlockP : TypeState -> P.Parser TypeState
typeBlockP t =
    P.map
        (\elts -> { t | stack = Tblock elts :: t.stack })
        (typeBlockHelpP t)


typeBlockHelpP : TypeState -> P.Parser (List Elt)
typeBlockHelpP t =
    P.succeed .elts
        |. P.token "{"
        |= typeLangP t
        |. P.token "}"


typeDefP : TypeState -> P.Parser TypeState
typeDefP t =
    P.succeed identity
        |. P.token "="
        |. whiteSpaceP
        |= variable
        |> P.andThen (typeDefPhelp t)


typeDefPhelp : TypeState -> String -> P.Parser TypeState
typeDefPhelp t var =
    case t.stack of
        [] ->
            P.problem "stack should contain at least one item"

        s :: tack ->
            if Dict.member var t.defs then
                P.problem <|
                    "multiple definitions of \"" ++ var ++ "\""

            else
                P.succeed
                    { defs = Dict.insert var s t.defs
                    , stack = tack
                    }


typeStringP : TypeState -> P.Parser TypeState
typeStringP t =
    P.succeed (\s -> {t | stack = Tstring s :: t.stack })
        |= stringP


typeStringPHelp : TypeState -> String -> TypeState
typeStringPHelp {defs, stack} s =
    let
        newStack = Tstring s :: stack
    in
        {stack = newStack, defs = defs }


typeRunBlockP : TypeState -> P.Parser ParserOut
typeRunBlockP t =
    case t.stack of
        [] ->
            P.problem "got \"!\" but stack is empty"

        (Tblock b) :: tack ->
            P.succeed {typeState = {defs = t.defs, stack = tack}
                      , elfs = []
                      , elts = b
                      }

        top :: _ ->
            P.problem <| String.concat
                [ "expecting a block on top of the stack, but got "
                , showTypeProgramValue top
                ]
            

runTypeBlockHelp :
    TypeState
    -> List (TypeState -> TypeState)
    -> TypeState
runTypeBlockHelp t block =
    case block of
        [] ->
            t

        b :: lock ->
            runTypeBlockHelp (b t) lock



-- switchP : P.Parser ( List Elf, List Elt )
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
-- 
-- 
-- makeSwitchElfsElts = Debug.todo "todo"


type alias SwitchPart =
    { pattern : Pattern
    , block : (List Elf, List Elt)
    }


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


patternP = Debug.todo ""


stringPWrap : P.Parser ( List Elf, List Elt )
stringPWrap =
    P.succeed (\s -> ( [ stringElf s ], [ stringElt ] ))
        |= stringP


stringElf : String -> ProgramState -> ProgramState
stringElf s p =
    { p | stack = Pstring s :: p.stack }


stringElt : Dict.Dict String Type -> List Type -> Result String ( Dict.Dict String Type, List Type )
stringElt dets stack =
    Ok ( dets, {standard = [Sstring], custom = []}  :: stack )


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
                , P.map (\_ -> "\r") (P.token "r")
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


defP : P.Parser ( List Elf, List Elt )
defP =
    P.succeed (\var -> ( [ defElf var ], [ defElt var ] ))
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


defElt :
    String
    -> Dict.Dict String Type
    -> List Type
    -> Result String ( Dict.Dict String Type, List Type )
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


runBlockP : P.Parser ( List Elf, List Elt )
runBlockP =
    P.succeed ( [ runBlockElf ], [ runBlockElt ] )
        |. P.keyword "!"


programBlockP : TypeState -> P.Parser ParserOut
programBlockP t =
    P.succeed (\{elfs, elts, typeState} -> {elfs = [ blockElf elfs ], elts = [ blockElt elts ], typeState = typeState })
        |. P.keyword "{"
        |= programP t
        |. P.keyword "}"


blockElf : List Elf -> ProgramState -> ProgramState
blockElf elfs p =
    { p | stack = Pblock elfs :: p.stack }


blockElt : List Elt -> Dict.Dict String Type -> List Type -> Result String ( Dict.Dict String Type, List Type )
blockElt elts dets stack =
    Ok ( dets, {standard = [Sblock elts], custom = []} :: stack )


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


runBlockElt :
    Dict.Dict String Type
    -> List Type
    -> Result String ( Dict.Dict String Type, List Type )
runBlockElt dets typeStack =
    case typeStack of
        [] ->
            Err "empty stack"
    
        t :: xs ->
            case getBlock t.standard of
                Nothing ->
                    Err <|
                        String.concat
                            [ "bad stack: expecting block, but got "
                            , showTypeVal t
                            ]
                Just block ->
                    case runTypeChecksHelp block dets xs of
                        Ok newStack ->
                            Ok ( dets, newStack )

                        Err errMsg ->
                            Err errMsg


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
    P.succeed retrievePhelp |= variable


retrievePhelp : String -> ( List Elf, List Elt )
retrievePhelp var =
    ( [ makeRetrieveElf var ], [ makeRetrieveElt var ] )


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


makeRetrieveElt : String -> Dict.Dict String Type -> List Type -> Result String ( Dict.Dict String Type, List Type )
makeRetrieveElt var dets typestack =
    case Dict.get var dets of
        Nothing ->
            Err <| String.concat [ "no definition \"", var, "\"" ]

        Just t ->
            Ok ( dets, t :: typestack )


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


-- makeFullTypeCheck : List TypeLiteral -> Dict.Dict String Type -> List Type -> Result String ( Dict.Dict String Type, List Type )
-- makeFullTypeCheck typeVals dets typeStack =
--     if equalT typeVals typeStack then
--         Ok ( dets, typeStack )
-- 
--     else
--         Err <| typeCheckErr typeVals typeStack


-- equalT : List TypeLiteral -> List Type -> Bool
-- equalT lits vals =
--     if List.length lits /= List.length vals then
--         False
-- 
--     else
--         List.all identity <| List.map2 equalThelp lits vals
-- 
-- 
-- equalThelp : TypeLiteral -> Type -> Bool
-- equalThelp lit val =
--     case ( lit, val ) of
--         ( Tlstring, Tstring ) ->
--             True
-- 
--         ( Tlblock, Tblock _ ) ->
--             True
-- 
--         _ ->
--             False


-- makePartialTypeCheck : List TypeLiteral -> Dict.Dict String Type -> List Type -> Result String ( Dict.Dict String Type, List Type )
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


typeLiteralP : TypeState -> P.Parser Type
typeLiteralP t =
    P.oneOf [ stringTypeP, blockTypeP ]


blockTypeP : P.Parser Type
blockTypeP =
    P.succeed {standard = [Sblock []], custom = []}
        |. P.keyword "block"


stringTypeP : P.Parser Type
stringTypeP =
    P.succeed {standard = [Sstring], custom = []}
        |. P.keyword "string"


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


type TypeProgramValue
    = Ttype Type
    | Tstring String
    | Tblock (List Elt)
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
    { standard : List StandardType
    , custom : List TypeAtom
    }


type TypeAtom
    = Astring String


type StandardType
    = Sblock (List Elt)
    | Sstring


equalTypeStacks : List Type -> List Type -> Bool
equalTypeStacks t1 t2 =
    List.all identity <| List.map2 equalType t1 t2


equalType : Type -> Type -> Bool
equalType t1 t2 =
    equalStandards t1.standard t2.standard &&
    equalCustoms t1.custom t2.custom


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
    case (b1, b2) of
        (Sstring, Sstring) ->
            True

        (Sblock _, Sblock _) ->
            True

        _ ->
            False


type alias Elt =
    Dict.Dict String Type -> List Type -> Result String ( Dict.Dict String Type, List Type )


standardTypes : Dict.Dict String Type
standardTypes =
    Dict.fromList
        [ ( "print", { standard = [Sblock [ printElt ]], custom = []} )
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


printElt : Elt
printElt dets stack =
    case stack of
        [] ->
            Err <| String.concat
                [ "\"print\" needs there to be a something on the "
                , "stack, but it is empty"
                ]
        s :: tack ->
            if isSubType s ({standard = [Sstring], custom = []}) then
                Ok ( dets, tack )
            else
                Err <|
                    String.concat
                        [ "\"print\" needs the top value on the "
                        , "stack to be a string, but it is "
                        , showTypeVal s
                        ]


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
standardOfCustom sub master =
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



runTypeChecks : List Elt -> Maybe String
runTypeChecks elts =
    case runTypeChecksHelp elts standardTypes [] of
        Ok [] ->
            Nothing

        Ok ts ->
            Just <|
                String.concat
                    [ "typestack should be empty at end of program, but "
                    , "got "
                    , showTypeStack ts
                    ]

        Err err ->
            Just err


runTypeChecksHelp :
    List Elt
    -> Dict.Dict String Type
    -> List Type
    -> Result String (List Type)
runTypeChecksHelp elts dets typeStack =
    case elts of
        [] ->
            Ok typeStack

        e :: lts ->
            case e dets typeStack of
                Err errMsg ->
                    Err errMsg

                Ok ( newDets, newTypeStack ) ->
                    runTypeChecksHelp lts newDets newTypeStack


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
    Element.Input.multiline [monospace]
        { onChange = UpdatedLeft
        , text = leftText model
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    Element.text "Type here"
        , label =
            Element.Input.labelAbove [sansSerif] <|
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
    Element.el [monospace] <|
    case model.openProgram of
        Nothing ->
            Element.text <| "internal error: can't find program"

        Just (_, doc) ->
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

port module Main exposing (main)

import Base64
import Browser
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Dict
import Element
import Element.Input
import Html
import Json.Decode as Jd
import Json.Encode as Je
import List.Nonempty as N
import Parser as P exposing ((|.), (|=))
import Set
import Debug exposing (log)


port requestHome : () -> Cmd msg


port retrievedHome : (String -> msg) -> Sub msg


port requestHash : String -> Cmd msg


port retrievedHash : (String -> msg) -> Sub msg


port cacheHome : String -> Cmd msg


port cacheHash : String -> Cmd msg


port sendMsg : Je.Value -> Cmd msg


port gotSecretKeys : (Je.Value -> msg) -> Sub msg


port getSecretKeys : () -> Cmd msg


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = RetrievedHome String
    | RetrievedHash String
    | UpdatedLeft String
    | UpdatedEditor String
    | LookupRaw (N.Nonempty String)
    | LaunchProgram String
    | NewRawKeys Je.Value


type alias Model =
    { home : Home
    , openProgram : Maybe ( Program, Document )
    , lookedUpBlob : Maybe ( Bytes.Bytes, Set.Set String )
    , toLookUp : List String
    , accumBlob : Maybe Bytes.Bytes
    , internalErr : Maybe String
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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( newProgram, rightDoc, outMsgs ) =
            runProgram defaultHome
    in
    ( { home = initHome
      , openProgram = Just ( defaultHome, rightDoc )
      , lookedUpBlob = Nothing
      , toLookUp = []
      , accumBlob = Nothing
      , internalErr = Nothing
      }
    , requestHome ()
    )


cacheBlobs : List Blob -> Cmd msg
cacheBlobs blobs =
    Cmd.batch <| List.map cacheBlobHelp blobs


cacheBlobHelp : Blob -> Cmd msg
cacheBlobHelp blob =
    case serializeBlob blob of
        Nothing ->
            Cmd.none

        Just blobStr ->
            cacheHash blobStr


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
    """"Hello, this is the placeholder home app." print"""


view : Model -> Html.Html Msg
view model =
    Element.layout [] (viewHelp model)


viewHelp : Model -> Element.Element Msg
viewHelp model =
    Element.column []
        [ homeButton
        , leftRight model
        , editor model
        ]


editor : Model -> Element.Element Msg
editor model =
    case model.openProgram of
        Nothing ->
            Element.text <| "internal err: can't find program"

        Just ( program, _ ) ->
            Element.Input.multiline []
                { onChange = UpdatedEditor
                , text = program.code
                , placeholder =
                    Just <|
                        Element.Input.placeholder [] <|
                            Element.text "Type program here"
                , label =
                    Element.Input.labelAbove [] <|
                        Element.text "Type program here"
                , spellcheck = False
                }


homeButton : Element.Element Msg
homeButton =
    Element.Input.button []
        { onPress = Just <| LaunchProgram "home"
        , label = Element.text "Home"
        }


leftRight : Model -> Element.Element Msg
leftRight model =
    Element.row []
        [ leftInput model
        , showRightDoc model
        ]


runProgram : Program -> ( Program, Document, List HumanMsg )
runProgram program =
    case P.run programP program.code of
        Err deadEnds ->
            ( program
            , SmallString <| deadEndsToString deadEnds
            , []
            )

        Ok ( elfs, elts ) ->
            case runTypeChecks elts of
                Just errMsg ->
                    ( program, SmallString errMsg, [] )

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


programP : P.Parser ( List Elf, List Elt )
programP =
    P.loop ( [], [] ) programHelpP


programHelpP :
    ( List Elf, List Elt )
    -> P.Parser (P.Step ( List Elf, List Elt ) ( List Elf, List Elt ))
programHelpP ( oldElfs, oldElts ) =
    P.oneOf
        [ P.succeed (\( elfs, elts ) -> P.Loop ( oldElfs ++ elfs, oldElts ++ elts ))
            |. whiteSpaceP
            |= elementP
        , P.succeed () |> P.map (\_ -> P.Done ( oldElfs, oldElts ))
        ]


elementP : P.Parser ( List Elf, List Elt )
elementP =
    P.oneOf
        [ runBlockP
        , stringPWrap
        , defP
        , programBlockP
        , partialTypeCheckP
        , fullTypeCheckP
        , retrieveP
        ]


stringPWrap : P.Parser ( List Elf, List Elt )
stringPWrap =
    P.succeed (\s -> ( [ stringElf s ], [ stringElt ] ))
        |= stringP


stringElf : String -> ProgramState -> ProgramState
stringElf s p =
    { p | stack = Pstring s :: p.stack }


stringElt : Dict.Dict String TypeVal -> List TypeVal -> Result String ( Dict.Dict String TypeVal, List TypeVal )
stringElt dets stack =
    Ok ( dets, Tstring :: stack )


{-| Mostly copied from <https://github.com/elm/parser/blob/master/examples/DoubleQuoteString.elm>
-}
stringP : P.Parser String
stringP =
    P.succeed identity
        |. P.token "\""
        |= P.loop [] stringHelp


stringHelp : List String -> P.Parser (P.Step (List String) String)
stringHelp revChunks =
    P.oneOf
        [ P.succeed (\chunk -> P.Loop (chunk :: revChunks))
            |. P.token "\\"
            |= P.oneOf
                [ P.map (\_ -> "\n") (P.token "n")
                , P.map (\_ -> "\t") (P.token "t")
                , P.map (\_ -> "\u{000D}") (P.token "r")
                ]
        , P.token "\""
            |> P.map (\_ -> P.Done (String.join "" (List.reverse revChunks)))
        , P.chompWhile isUninteresting
            |> P.getChompedString
            |> P.map (\chunk -> P.Loop (chunk :: revChunks))
        ]


variable : P.Parser String
variable =
    P.map (String.map Char.toLower) <|
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
        |. P.keyword "="
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
    -> Dict.Dict String TypeVal
    -> List TypeVal
    -> Result String ( Dict.Dict String TypeVal, List TypeVal )
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


programBlockP : P.Parser ( List Elf, List Elt )
programBlockP =
    P.succeed identity
        |. P.keyword "{"
        |= programP
        |. P.keyword "}"


runBlockElf : ProgramState -> ProgramState
runBlockElf s =
    case s.stack of
        [] ->
            { s | internalError = Just "! but empty stack" }

        (Pblock block) :: xs ->
            let
                ( newP, rightDoc, msgs ) =
                    runElfs s.program block s.stack
            in
            { s
                | program = newP
                , stack = xs
                , outbox = s.outbox ++ msgs
                , rightDoc = rightDoc
            }

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
    Dict.Dict String TypeVal
    -> List TypeVal
    -> Result String ( Dict.Dict String TypeVal, List TypeVal )
runBlockElt dets typeStack =
    case typeStack of
        [] ->
            Err "empty stack"

        (Tblock block) :: xs ->
            case runTypeChecksHelp block dets xs of
                Nothing ->
                    Ok ( dets, xs )

                Just errMsg ->
                    Err errMsg

        x :: _ ->
            Err <|
                String.concat
                    [ "bad stack: expecting block, but got "
                    , showTypeVal x
                    ]


showTypeVal : TypeVal -> String
showTypeVal typeVal =
    case typeVal of
        Tstring ->
            "string"

        Tblock atoms ->
            "block"


showTypeStack : List TypeVal -> String
showTypeStack typestack =
    String.concat
        [ "< "
        , String.join ", " <| List.map showTypeVal typestack
        , " >"
        ]


showTypeCheck : List TypeLiteral -> String
showTypeCheck typeCheck =
    String.concat
        [ "< "
        , String.join ", " <| List.map showTypeLit typeCheck
        , " >"
        ]


showTypeLit : TypeLiteral -> String
showTypeLit typeLit =
    case typeLit of
        Tlstring ->
            "string"

        Tlblock ->
            "block"


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


makeRetrieveElt : String -> Dict.Dict String TypeVal -> List TypeVal -> Result String ( Dict.Dict String TypeVal, List TypeVal )
makeRetrieveElt var dets typestack =
    case Dict.get var dets of
        Nothing ->
            Err <| String.concat [ "no definition \"", var, "\"" ]

        Just t ->
            Ok ( dets, t :: typestack )


fullTypeCheckP : P.Parser ( List Elf, List Elt )
fullTypeCheckP =
    P.map (\ts -> ( [], [ makeFullTypeCheck ts ] )) <|
        P.sequence
            { start = "<"
            , separator = ","
            , end = ">"
            , spaces = whiteSpaceP
            , item = typeLiteralP
            , trailing = P.Optional
            }


typeCheckErr : List TypeLiteral -> List TypeVal -> String
typeCheckErr expected got =
    String.concat
        [ "type stack does not match type declaration:\n"
        , "expecting "
        , showTypeCheck expected
        , "\n"
        , "but got "
        , showTypeStack got
        , "\n"
        ]


makeFullTypeCheck : List TypeLiteral -> Dict.Dict String TypeVal -> List TypeVal -> Result String ( Dict.Dict String TypeVal, List TypeVal )
makeFullTypeCheck typeVals dets typeStack =
    if equalT typeVals typeStack then
        Err <| typeCheckErr typeVals typeStack

    else
        Ok ( dets, typeStack )


equalT : List TypeLiteral -> List TypeVal -> Bool
equalT lits vals =
    if List.length lits /= List.length vals then
        False

    else
        List.all identity <| List.map2 equalThelp lits vals


equalThelp : TypeLiteral -> TypeVal -> Bool
equalThelp lit val =
    case ( lit, val ) of
        ( Tlstring, Tstring ) ->
            True

        ( Tlblock, Tblock _ ) ->
            True

        _ ->
            False


makePartialTypeCheck : List TypeLiteral -> Dict.Dict String TypeVal -> List TypeVal -> Result String ( Dict.Dict String TypeVal, List TypeVal )
makePartialTypeCheck typeVals dets typeStack =
    let
        lenExpected =
            List.length typeVals

        lenActual =
            List.length typeStack

        candidate =
            List.take lenExpected typeStack
    in
    if lenActual < lenExpected then
        Err <|
            String.concat
                [ "expecting "
                , String.fromInt lenExpected
                , " items on the stack, but only got "
                , String.fromInt lenActual
                ]

    else if equalT typeVals candidate then
        Ok ( dets, typeStack )

    else
        Err <| typeCheckErr typeVals candidate


typeLiteralP : P.Parser TypeLiteral
typeLiteralP =
    P.oneOf [ stringTypeP, blockTypeP ]


blockTypeP : P.Parser TypeLiteral
blockTypeP =
    P.succeed Tlblock
        |. P.keyword "block"



-- typeP : P.Parser TypeVal
-- typeP =
--     P.oneOf [ stringTypeP, blockTypeP ]


type TypeLiteral
    = Tlstring
    | Tlblock


stringTypeP : P.Parser TypeLiteral
stringTypeP =
    P.succeed Tlstring
        |. P.keyword "str"



-- blockTypeP : P.Parser TypeVal
-- blockTypeP =
--     P.map Tblock <|
--         P.sequence
--             { start = "["
--             , separator = ","
--             , end = "]"
--             , spaces = whiteSpaceP
--             , item = (\() -> typeP) ()
--             , trailing = P.Optional
--             }


partialTypeCheckP : P.Parser ( List Elf, List Elt )
partialTypeCheckP =
    P.map (\elt -> ( [], [ makePartialTypeCheck elt ] )) <|
        P.sequence
            { start = "<.."
            , separator = ","
            , end = ">"
            , spaces = whiteSpaceP
            , item = typeLiteralP
            , trailing = P.Optional
            }


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
    -> ( Program, Document, List HumanMsg )
runElfs program elfs progStack =
    let
        oldP =
            { program = program
            , defs = standardLibrary
            , stack = progStack
            , rightDoc = initDoc
            , outbox = []
            , blobs = []
            , internalError = Nothing
            }
    in
        runElfsHelp elfs oldP


type alias ProgramState =
    { program : Program
    , defs : Dict.Dict String ProgVal
    , stack : List ProgVal
    , rightDoc : Document
    , outbox : List HumanMsg
    , blobs : List Blob
    , internalError : Maybe String
    }


type alias Elf =
    ProgramState -> ProgramState


runElfsHelp :
    List Elf
    -> ProgramState
    -> ( Program, Document, List HumanMsg )
runElfsHelp elfs s =
    case elfs of
        [] ->
            ( s.program, s.rightDoc, s.outbox )

        e :: lfs ->
            let
                newS =
                    e s
            in
            runElfsHelp lfs newS


type TypeVal
    = Tstring
    | Tblock (List Elt)


type alias Elt =
    Dict.Dict String TypeVal -> List TypeVal -> Result String ( Dict.Dict String TypeVal, List TypeVal )


standardTypes : Dict.Dict String TypeVal
standardTypes =
    Dict.fromList
        [ ( "print", Tblock [ printElt ] )
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
                | rightDoc = print p.rightDoc s
                , stack = tack
            }

        stack ->
            { p
                | internalError = Just <| String.concat
                    [ "runtime error:\n"
                    , "\"print\" expects the top of the stack to "
                    , "be a string"
                    ]
            }

         
print : Document -> String -> Document
print doc s =
    case doc of
        Ordering ds ->
            Ordering <| ds ++ [ SmallString s ]

        SmallString oldS ->
            Ordering [ SmallString oldS, SmallString s ]

        Named n b ->
            Ordering [ Named n b, SmallString s ]

        Anon b ->
            Ordering [ Anon b, SmallString s ]


printElt : Elt
printElt dets stack =
    case stack of
        Tstring :: tack ->
            Ok ( dets, tack )

        s ->
            Err <| String.concat
                [ "\"print\" needs there to be a string on the stack"
                , " but the stack is "
                , showTypeStack s
                ]


runTypeChecks : List Elt -> Maybe String
runTypeChecks elts =
    runTypeChecksHelp elts standardTypes []


runTypeChecksHelp :
    List Elt
    -> Dict.Dict String TypeVal
    -> List TypeVal
    -> Maybe String
runTypeChecksHelp elts dets typeStack =
    case elts of
        [] ->
            case log "typestack: (elts empty)" typeStack of
                [] ->
                    Nothing

                x ->
                    Just <| String.concat
                        [ "typestack should be empty at end of "
                        , "program, but got "
                        , showTypeStack x
                        ]

        e :: lts ->
            case e dets (log "typestack (elts full)" typeStack) of
                Err errMsg ->
                    Just errMsg

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
    Element.Input.multiline []
        { onChange = UpdatedLeft
        , text = leftText model
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    Element.text "Type stuff here"
        , label =
            Element.Input.labelAbove [] <|
                Element.text <|
                    "Type here, using instructions on right."
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
    case model.openProgram of
        Nothing ->
            Element.text <| "internal error: can't find program"

        Just program ->
            case model.openProgram of
                Nothing ->
                    Element.text <| "internal error: no open program"

                Just ( _, doc ) ->
                    displayDoc model.lookedUpBlob doc


displayDoc :
    Maybe ( Bytes.Bytes, Set.Set String )
    -> Document
    -> Element.Element Msg
displayDoc lookedUpBlob doc =
    case doc of
        Anon blob ->
            displayBlob "•" blob lookedUpBlob

        Named name blob ->
            displayBlob name blob lookedUpBlob

        Ordering docs ->
            Element.column [] <|
                List.map (displayDoc lookedUpBlob) docs

        SmallString s ->
            Element.text s


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


cacheHomeHelp : Home -> Cmd msg
cacheHomeHelp home =
    case Base64.fromBytes <| E.encode <| encodeHome home of
        Nothing ->
            Cmd.none

        Just base64str ->
            cacheHome base64str


sendMsgs : List Je.Value -> Cmd Msg
sendMsgs msgs =
    Cmd.batch <| List.map sendMsg msgs


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


reRunProgram : Model -> Program -> ( Model, Cmd Msg )
reRunProgram model program =
    let
        ( p, doc, msgs ) =
            runProgram program

        oldHome =
            model.home

        newOutbox =
            model.home.outbox ++ msgs
    in
    case model.home.myKeys of
        Nothing ->
            ( { model | home = { oldHome | outbox = newOutbox }, openProgram = Just ( p, doc ) }
            , getSecretKeys ()
            )

        Just myKeys ->
            case
                encodeMsgs
                    { msgs = newOutbox
                    , pubKeys = model.home.pubKeys
                    , myKeys = myKeys
                    , nonceBase = model.home.biggestNonceBase
                    }
            of
                Err err ->
                    ( { model
                        | internalErr = Just <| "error encoding messages: " ++ err
                        , home = { oldHome | outbox = newOutbox }
                      }
                    , Cmd.none
                    )

                Ok encodedMsgs ->
                    let
                        newHome =
                            { oldHome
                                | outbox = newOutbox
                                , biggestNonceBase = oldHome.biggestNonceBase + List.length msgs
                                , programs =
                                    Dict.insert
                                        program.name
                                        p
                                        oldHome.programs
                            }
                    in
                    ( { model
                        | home = newHome
                        , openProgram = Just ( p, doc )
                      }
                    , Cmd.batch
                        [ sendMsgs encodedMsgs
                        , cacheHomeHelp newHome
                        ]
                    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LaunchProgram programName ->
            case Dict.get programName model.home.programs of
                Nothing ->
                    ( { model | openProgram = Nothing }, Cmd.none )

                Just program ->
                    reRunProgram model program

        LookupRaw hashes ->
            ( { model | toLookUp = N.toList hashes }
            , Cmd.batch <| List.map requestHash <| N.toList hashes
            )

        NewRawKeys rawKeys ->
            case Jd.decodeValue decodeSecretKeys rawKeys of
                Err err ->
                    ( { model
                        | internalErr = Just <| Jd.errorToString err
                      }
                    , Cmd.none
                    )

                Ok keys ->
                    let
                        oldHome =
                            model.home

                        newHome =
                            { oldHome | myKeys = Just keys }

                        newModel =
                            { model | home = newHome }
                    in
                    case model.openProgram of
                        Nothing ->
                            ( model, Cmd.none )

                        Just ( program, _ ) ->
                            reRunProgram newModel program

        RetrievedHome rawHome ->
            case Base64.toBytes rawHome of
                Just bytes ->
                    case D.decode decodeHome bytes of
                        Nothing ->
                            ( model, Cmd.none )

                        Just home ->
                            ( { model | home = home }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RetrievedHash raw ->
            ( model, Cmd.none )

        UpdatedLeft newLeftText ->
            case model.openProgram of
                Nothing ->
                    ( model, Cmd.none )

                Just ( program, _ ) ->
                    let
                        newProg =
                            { program | typedIn = newLeftText }
                    in
                    reRunProgram model newProg

        UpdatedEditor newCode ->
            case model.openProgram of
                Nothing ->
                    ( model, Cmd.none )

                Just ( program, _ ) ->
                    let
                        newProg =
                            { program | code = newCode }
                    in
                    reRunProgram model newProg


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ retrievedHome RetrievedHome
        , retrievedHash RetrievedHash
        , gotSecretKeys NewRawKeys
        ]

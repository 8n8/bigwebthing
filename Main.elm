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
import List.Nonempty as N
import Set


port requestHome : () -> Cmd msg


port retrievedHome : (String -> msg) -> Sub msg


port requestHash : String -> Cmd msg


port retrievedHash : (String -> msg) -> Sub msg


port cacheHome : String -> Cmd msg


port cacheHash : String -> Cmd msg


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


type alias Model =
    { home : Home
    , openProgram : Maybe Program
    , lookedUpBlob : Maybe ( Bytes.Bytes, Set.Set String )
    , rightDoc : Maybe Document
    , toLookUp : List String
    , accumBlob : Maybe Bytes.Bytes
    }


type alias Home =
    { programs : Dict.Dict String Program
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { home = { programs = Dict.empty }
      , openProgram = Nothing
      , lookedUpBlob = Nothing
      , rightDoc = Nothing
      , toLookUp = []
      , accumBlob = Nothing
      }
    , requestHome ()
    )


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

        Just program ->
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
        , rightDoc model
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
            "internal error: can't find program"

        Just program ->
            program.typedIn


rightDoc : Model -> Element.Element Msg
rightDoc model =
    case model.openProgram of
        Nothing ->
            Element.text <| "internal error: can't find program"

        Just program ->
            case model.rightDoc of
                Nothing ->
                    Element.text <| "internal error: no right doc"

                Just doc ->
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LaunchProgram programName ->
            ( { model
                | openProgram =
                    Dict.get programName model.home.programs
              }
            , Cmd.none
            )

        LookupRaw hashes ->
            ( { model | toLookUp = N.toList hashes }
            , Cmd.batch <| List.map requestHash <| N.toList hashes
            )

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

                Just program ->
                    ( { model
                        | openProgram =
                            Just
                                { program | typedIn = newLeftText }
                      }
                    , Cmd.none
                    )

        UpdatedEditor newCode ->
            case model.openProgram of
                Nothing ->
                    ( model, Cmd.none )

                Just program ->
                    ( { model
                        | openProgram =
                            Just
                                { program | code = newCode }
                      }
                    , Cmd.none
                    )


decodeHome : D.Decoder Home
decodeHome =
    D.map (Home << programsToDict) (list decodeProgram)


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
        (list decodeDocument)
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
    , inbox : List Document
    , blobs : List Blob
    , typedIn : String
    }


type Document
    = Anon Blob
    | Named String Blob
    | Ordering (List Document)


type Blob
    = Blob Mime (N.Nonempty String)


type Mime
    = Text


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ retrievedHome RetrievedHome
        , retrievedHash RetrievedHash
        ]

port module Main exposing (main)

import Base64
import Browser
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Element
import Html
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


type alias Model =
    { home : Home
    }


type alias Home =
    { programs : List Program
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { home = { programs = [] }
      }
    , Cmd.none
    )


view : Model -> Html.Html Msg
view model =
    Element.layout [] (viewHelp model)


viewHelp : Model -> Element.Element Msg
viewHelp model =
    Element.text "Hello, you stylish developer!"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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


decodeHome : D.Decoder Home
decodeHome =
    D.map Home (list decodeProgram)


list : D.Decoder a -> D.Decoder (List a)
list decoder =
    D.unsignedInt32 Bytes.BE
        |> D.andThen
            (\len -> D.loop ( len, [] ) (listStep decoder))


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
    D.map4 Program
        sizedString
        sizedString
        (list decodeDocument)
        decodeUserInput


decodeUserInput : D.Decoder UserInput
decodeUserInput =
    D.map2 UserInput (list decodeBlob) sizedString


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


decodeHash : D.Decoder Hash
decodeHash =
    D.map Hash sizedString


decodeBlob : D.Decoder Blob
decodeBlob =
    D.map2 Blob decodeMime (list decodeHash)


decodeMime : D.Decoder Mime
decodeMime =
    D.andThen decodeMimeHelp D.unsignedInt8


decodeMimeHelp : Int -> D.Decoder Mime
decodeMimeHelp i =
    case i of
        0 ->
            D.succeed Video

        1 ->
            D.succeed Audio

        2 ->
            D.succeed Image

        3 ->
            D.succeed Text

        _ ->
            D.fail


decodeOrdering : D.Decoder Document
decodeOrdering =
    D.map Ordering (list decodeDocument)


type alias Program =
    { code : String
    , description : String
    , inbox : List Document
    , userInput : UserInput
    }


type alias UserInput =
    { blobs : List Blob
    , typedIn : String
    }


type Document
    = Anon Blob
    | Named String Blob
    | Ordering (List Document)


type Blob
    = Blob Mime (List Hash)


type Mime
    = Video
    | Audio
    | Image
    | Text


type Hash
    = Hash String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ retrievedHome RetrievedHome
        , retrievedHash RetrievedHash
        ]

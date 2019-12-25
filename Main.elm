port module Main exposing (main)

import Browser
import Element
import Html
import Json.Decode
import Json.Encode
import Set


port request : Json.Encode.Value -> Cmd a


port retrieved : (Json.Encode.Value -> msg) -> Sub msg


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = Retrieve Json.Encode.Value


type alias Model =
    { programs : Result Json.Decode.Error (List Program)
    , badDbRead : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { programs = Ok []
      , badDbRead = Nothing
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
        Retrieve json ->
            decodeRetrieve model json


decodeRetrieve : Model -> Json.Encode.Value -> ( Model, Cmd Msg )
decodeRetrieve model raw =
    case Json.Decode.decodeValue decodeKvString raw of
        Ok [ ( "programs", rawprog ) ] ->
            ( { model | programs = decodePrograms rawprog }
            , Cmd.none
            )

        Ok _ ->
            ( { model
                | badDbRead =
                    Just <|
                        "bad DB response"
              }
            , Cmd.none
            )

        Err err ->
            ( { model
                | badDbRead =
                    Just <|
                        "bad DB response:\n"
                            ++ "parsing error:\n"
                            ++ Json.Decode.errorToString err
              }
            , Cmd.none
            )


decodePrograms : String -> Result Json.Decode.Error (List Program)
decodePrograms raw =
    Json.Decode.decodeString (Json.Decode.list decodeProgram) raw


decodeProgram : Json.Decode.Decoder Program
decodeProgram =
    Json.Decode.map4 Program
        (Json.Decode.field "code" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "inbox" decodeInbox)
        (Json.Decode.field "input" decodeInput)


decodeInput : Json.Decode.Decoder UserInput
decodeInput =
    Json.Decode.map2 UserInput
        (Json.Decode.field "typedin" Json.Decode.string)
        (Json.Decode.field "blobs" <| Json.Decode.list decodeBlob)


decodeInbox : Json.Decode.Decoder (List Document)
decodeInbox =
    Json.Decode.list decodeDocument


decodeDocument : Json.Decode.Decoder Document
decodeDocument =
    Json.Decode.andThen
        decodeDocHelp
        (Json.Decode.field "type" Json.Decode.string)


decodeDocHelp : String -> Json.Decode.Decoder Document
decodeDocHelp docType =
    case docType of
        "anon" ->
            Json.Decode.map Anon decodeBlob

        "named" ->
            decodeNamed

        "ordering" ->
            decodeOrdering

        _ ->
            Json.Decode.fail <|
                "Trying to decode document, but type \""
                    ++ docType
                    ++ "\" is not supported."


decodeOrdering : Json.Decode.Decoder Document
decodeOrdering =
    Json.Decode.map Ordering <| Json.Decode.list decodeDocument


decodeNamed : Json.Decode.Decoder Document
decodeNamed =
    Json.Decode.map2 Named
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "blob" decodeBlob)


decodeHash : Json.Decode.Decoder Hash
decodeHash =
    Json.Decode.map Hash Json.Decode.string


decodeBlob : Json.Decode.Decoder Blob
decodeBlob =
    Json.Decode.map2 Blob
        (Json.Decode.field "mime" decodeMime)
        (Json.Decode.field "hashes" <| Json.Decode.list decodeHash)


decodeMime : Json.Decode.Decoder Mime
decodeMime =
    Json.Decode.andThen
        decodeMimeHelp
        Json.Decode.string


decodeMimeHelp : String -> Json.Decode.Decoder Mime
decodeMimeHelp raw =
    case raw of
        "video" ->
            Json.Decode.succeed Video

        "audio" ->
            Json.Decode.succeed Audio

        "image" ->
            Json.Decode.succeed Image

        "text" ->
            Json.Decode.succeed Text

        _ ->
            Json.Decode.fail <| "Bad mime: " ++ raw


type alias Program =
    { code : String
    , description : String
    , inbox : List Document
    , userInput : UserInput
    }


type alias UserInput =
    { typedIn : String
    , blobs : List Blob
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


decodeKvString : Json.Decode.Decoder (List ( String, String ))
decodeKvString =
    Json.Decode.keyValuePairs Json.Decode.string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

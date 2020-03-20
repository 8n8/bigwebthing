module Main exposing (main)

import Base64
import Base64.Decode
import Base64.Encode
import Browser
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Dict
import Editor
import Element
import Generator
import Hex.Convert
import Html
import Http
import Json.Decode as Jd
import Json.Encode as Je
import List.Nonempty as N



-- Client-to-client API
-- ====================
--
-- Top-level API
-- -------------
--
-- These are the message formats that are acceptable between clients.
-- Remember (from the server API spec set out in the README), that
-- the server already requires that messages between clients are
-- signed by the sender, so extra authentication is not necessary.
--
-- 1. Request public encryption key:
-- + 0x01
--
-- 2. Public encryption key:
-- + 0x02
-- + 32 bytes: public encryption key
--
-- 3. Encrypted blob:
-- + 0x03
-- + 8 bytes: nonce
-- + the blob
--
--
-- Sub-level API
-- -------------
--
-- These are the message formats that are acceptable inside the
-- encrypted blobs (see 3 above).
-- ...


type Msg
    = Editor Editor.Msg
    | Generator Generator.Msg



-- | Retriever Retriever.Msg
-- | Sender Sender.Msg
-- | Importer Importer.Msg


type alias Model =
    { editor : Editor.Model
    , generator : Generator.Model

    -- , retriever : Retriever.Model
    -- , sender : Sender.Model
    -- , importer : Importer.Model
    }


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


view : Model -> Html.Html Msg
view model =
    Element.layout [] <|
        Element.column [ Element.padding 12 ]
            [ Element.map Editor <| Editor.view model.editor
            , Element.map Generator <| Generator.view model.generator

            -- , Retriever.view model.retriever
            -- , Sender.view model.sender
            -- , Importer.view model.impporter
            ]


showB64Err : Base64.Decode.Error -> String
showB64Err err =
    case err of
        Base64.Decode.ValidationError ->
            "validation error"

        Base64.Decode.InvalidByteSequence ->
            "invalid byte sequence"


showHttpError : Http.Error -> String
showHttpError err =
    case err of
        Http.BadUrl url ->
            "bad url: " ++ url

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus code ->
            "bad status code: " ++ String.fromInt code

        Http.BadBody b ->
            "bad body: " ++ b


update : Msg -> Model -> ( Model, Cmd Msg )
update globalMsg globalModel =
    case globalMsg of
        Editor msg ->
            let
                ( newModel, command ) =
                    Editor.update msg globalModel.editor
            in
            ( { globalModel | editor = newModel }, Cmd.map Editor command )

        Generator msg ->
            let
                ( newModel, command ) =
                    Generator.update msg globalModel.generator
            in
            ( { globalModel | generator = newModel }, Cmd.map Generator command )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { editor = Editor.initModel, generator = Generator.initModel }, Cmd.batch [ Cmd.map Editor Editor.initCmd, Cmd.map Generator Generator.initCmd ] )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map Editor Editor.subscriptions
        , Sub.map Generator Generator.subscriptions
        ]

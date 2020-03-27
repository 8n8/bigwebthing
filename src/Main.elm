module Main exposing (main)

import Base64
import Base64.Decode
import Base64.Encode
import Browser
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Communicator
import Dict
import Editor
import Element
import Generator
import Hex.Convert
import Html
import Http
import Importer
import Json.Decode as Jd
import Json.Encode as Je
import List.Nonempty as N


type Msg
    = Editor Editor.Msg
    | Generator Generator.Msg
    | Communicator Communicator.Msg
    | Importer Importer.Msg


type alias Model =
    { editor : Editor.Model
    , generator : Generator.Model
    , communicator : Communicator.Model
    , importer : Importer.Model
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
            [ Element.map Editor <|
                Editor.view model.editor
            , Element.map Generator <|
                Generator.view model.generator
            , Element.map Communicator <|
                Communicator.view model.communicator
            , Element.map Importer <|
                Importer.view model.importer
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update globalMsg globalModel =
    case globalMsg of
        Editor msg ->
            let
                ( newModel, command ) =
                    Editor.update msg globalModel.editor
            in
            ( { globalModel | editor = newModel }
            , Cmd.map Editor command
            )

        Generator msg ->
            let
                ( newModel, command ) =
                    Generator.update msg globalModel.generator
            in
            ( { globalModel | generator = newModel }
            , Cmd.map Generator command
            )

        Communicator msg ->
            let
                ( newModel, command ) =
                    Communicator.update msg globalModel.communicator
            in
            ( { globalModel | communicator = newModel }
            , Cmd.map Communicator command
            )

        Importer msg ->
            let
                ( newModel, command ) =
                    Importer.update msg globalModel.importer
            in
            ( { globalModel | importer = newModel }
            , Cmd.map Importer command
            )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { editor = Editor.initModel
      , generator = Generator.initModel
      , communicator = Communicator.initModel
      , importer = Importer.initModel
      }
    , Cmd.batch
        [ Cmd.map Editor Editor.initCmd
        , Cmd.map Generator Generator.initCmd
        , Cmd.map Communicator Communicator.initCmd
        , Cmd.map Importer Importer.initCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map Editor Editor.subscriptions
        , Sub.map Generator Generator.subscriptions
        , Sub.map Communicator Communicator.subscriptions
        , Sub.map Importer Importer.subscriptions
        ]

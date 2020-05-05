module Main exposing (main)

import Browser
import Communicator
import Editor
import Element
import Html


type Msg
    = Editor Editor.Msg
    | Communicator Communicator.Msg


type alias Model =
    { editor : Editor.Model
    , communicator : Communicator.Model
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
            , Element.map Communicator <|
                Communicator.view model.communicator
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

        Communicator msg ->
            let
                ( newModel, command ) =
                    Communicator.update msg globalModel.communicator
            in
            ( { globalModel | communicator = newModel }
            , Cmd.map Communicator command
            )


init : () -> ( Model, Cmd Msg )
init _ =
    ( { editor = Editor.initModel
      , communicator = Communicator.initModel
      }
    , Cmd.batch
        [ Cmd.map Editor Editor.initCmd
        , Cmd.map Communicator Communicator.initCmd
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map Editor Editor.subscriptions
        , Sub.map Communicator Communicator.subscriptions
        ]

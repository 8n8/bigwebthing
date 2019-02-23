module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Hat
import Html.Events exposing (onClick, onInput)
import Http
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> DoNothing
        , onUrlChange = \_ -> DoNothing
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { boxStr = "", displayStr = "" }, Cmd.none )


type Msg
    = TypedIn String
    | FromServer (Result Http.Error String)
    | DoNothing


type alias Model =
    { boxStr : String
    , displayStr : String
    }


postMsg : String -> Cmd Msg
postMsg txt =
    Http.post
        { url = "http://localhost:3000"
        , expect = Http.expectString FromServer
        , body = Http.stringBody "text/plain; charset=utf-8" txt
        }


update msg model =
    case msg of
        TypedIn txt ->
            ( { model | boxStr = txt }, postMsg txt )

        FromServer (Err err) ->
            ( model, Cmd.none )

        FromServer (Ok str) ->
            ( { model | displayStr = str }, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


view model =
    { title = "BigWebThing"
    , body =
        [ div []
            [ input
                [ Hat.placeholder "Text to send"
                , Hat.value model.boxStr
                , onInput TypedIn
                ]
                []
            ]
        , text model.displayStr
        ]
    }

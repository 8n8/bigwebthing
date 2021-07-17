module Testable exposing (Model, Msg, add, init, subscriptions, update, view)

import Html


add : Int -> Int -> Int
add a b =
    a + b


type alias Model =
    Int


type alias Msg =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )


view : Model -> Html.Html Msg
view _ =
    Html.text "hi"


update : Msg -> Model -> ( Model, Cmd Msg )
update _ _ =
    ( 0, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

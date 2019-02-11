import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Url

main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    , onUrlRequest = \_ -> Increment
    , onUrlChange = \_ -> Increment
    }

init : () -> Url.Url -> Nav.Key -> (Int, Cmd Msg)
init _ _ _ = (0, Cmd.none)

type Msg = Increment | Decrement

update msg model =
  case msg of
    Increment ->
      (model + 1, Cmd.none)

    Decrement ->
      (model - 1, Cmd.none)

view model =
  { title = "BigWebThing"
  , body = [
      div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]
    ]
  }

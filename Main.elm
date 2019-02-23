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
    , subscriptions = subscriptions
    , onUrlRequest = \_ -> Increment
    , onUrlChange = \_ -> Increment
    }

subscriptions _ =
    listen "ws://localhost:3000" FromServer

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ _ _ = ({boxStr = "", displayStr = ""}, Cmd.none)

type Msg
    = TypedIn String
    | FromServer String
 
type alias Model = 
    { boxStr : String
    , displayStr : String
    }

update msg model =
  case msg of
    TypeIn txt ->
      ( { model | boxStr = txt }, send "ws://localhost:3000" txt )
    FromServer str ->
      { model | displayStr = str }

view model =
  { title = "BigWebThing"
  , body = [
      div []
        [ input
            [ placeholder "Text to send"
            , value model.boxStr
            , onInput TypedIn
            ] []
        ]
    ]
  }

module Main exposing (main)
import Browser
import Browser.Navigation
import Html
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> TestMsg
        , onUrlChange = \_ -> TestMsg
        }


init : Int -> Url.Url -> Browser.Navigation.Key -> (Model, Cmd Msg)
init _ _ _ =
    (initModel, Cmd.none)


type alias Model = String


initModel : Model
initModel =
    "hello" 


type Msg
    = TestMsg


view : Model -> Browser.Document Msg
view model =
    { title = "BigWebThing"
    , body = [ Html.text model ]
    }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    (model, Cmd.none)

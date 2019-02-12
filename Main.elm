import Browser
import Element as E
import Element.Border as Eb
import Element.Input as Ei
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
  , body = [ E.layout [] (gui model) ]
    --   div []
    --     [ button [ onClick Decrement ] [ text "-" ]
    --     , div [] [ text (String.fromInt model) ]
    --     , button [ onClick Increment ] [ text "+" ]
    --     ]
    -- ]
  }

gui model =
    E.el
        []
        (E.column
            [ E.spacing 20 ]
            [ Ei.button
                [ Eb.width 2
                , Eb.color <| E.rgb 0 0 0
                , E.padding 5
                ]
                { onPress = Just Decrement
                , label = E.text "-"
                }
            , E.text <| String.fromInt model
            , Ei.button
                [ Eb.width 2
                , Eb.color <| E.rgb 0 0 0
                , E.padding 5
                ]
                { onPress = Just Increment
                , label = E.text "+"
                }
            ]
        )

module Main exposing (main)

import Browser
import Testable as T


main : Program () T.Model T.Msg
main =
    Browser.element
        { init = T.init
        , view = T.view
        , update = T.update
        , subscriptions = T.subscriptions
        }

module Main exposing (main)

import Compiler

main =
    Browser.document
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

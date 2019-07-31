module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element as E
import Element.Font as Font
import Http
import Time
import Url
import Url.Parser as Up exposing ((</>))


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> DoNothing
        , onUrlChange = \_ -> DoNothing
        }


parseRoute : Up.Parser (String -> a) a
parseRoute =
    Up.s "getapp" </> Up.string </> Up.s "index.html"


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { securityCode = Up.parse parseRoute url }, Cmd.none )


type Msg
    = LaunchApp String
    | AppLaunched (Result Http.Error ())
    | DoNothing


type alias Model =
    { securityCode : Maybe String
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        AppLaunched _ ->
            ( model, Cmd.none )

        LaunchApp str ->
            case model.securityCode of
                Nothing ->
                    ( model, Cmd.none )

                Just code ->
                    ( model
                    , Http.get
                        { url = "/makeapproute/" ++ code ++ "/" ++ str
                        , expect = Http.expectWhatever AppLaunched
                        }
                    )


view : Model -> Browser.Document Msg
view model =
    { title = "BigWebThing"
    , body = [ E.layout [] (homePage model) ]
    }


homePage : Model -> E.Element Msg
homePage model =
    E.column
        [ E.width E.fill
        , E.padding 5
        , E.spacing 5
        ]
        [ E.text "hello"
        ]

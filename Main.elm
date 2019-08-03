module Main exposing (main)

import Base64
import Browser
import Browser.Navigation as Nav
import Element as E
import Element.Font as Font
import Element.Input as Ei
import Http
import Json.Decode as Jd
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
    case Up.parse parseRoute url of
        Nothing ->
            ( { securityCode = Nothing
              , installBox = ""
              , installErr = Nothing
              , metadata = Ok []
              }
            , Cmd.none
            )

        Just code ->
            ( { securityCode = Just code
              , installBox = ""
              , installErr = Nothing
              , metadata = Ok []
              }
            , getMetadata code
            )


getMetadata : String -> Cmd.Cmd Msg
getMetadata code =
    Http.get
        { url = "/metadata/" ++ code
        , expect = Http.expectJson GotMetadata (Jd.list metadataDec)
        }


type Msg
    = LaunchApp String
    | AppLaunched (Result Http.Error ())
    | DoNothing
    | InstallBox String
    | PressInstallButton
    | Installed (Result Http.Error ())
    | GotMetadata (Result Http.Error (List Metadata))


type alias Metadata =
    { name : String
    , description : String
    , iconUrl : String
    }


metadataDec : Jd.Decoder Metadata
metadataDec =
    Jd.map3 Metadata
        (Jd.field "Name" Jd.string)
        (Jd.field "Description" Jd.string)
        (Jd.field "IconFile" Jd.string)


type alias Model =
    { securityCode : Maybe String
    , installBox : String
    , installErr : Maybe Http.Error
    , metadata : Result Http.Error (List Metadata)
    }


installApp : String -> String -> Cmd Msg
installApp code url =
    Http.post
        { url = "http://localhost:3000/install/" ++ code
        , body = Http.stringBody "text/plain" url
        , expect = Http.expectWhatever Installed
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMetadata metadata ->
            ( { model | metadata = metadata }, Cmd.none )

        Installed (Ok ()) ->
            ( model, Cmd.none )

        Installed (Err err) ->
            ( { model | installErr = Just err }, Cmd.none )

        PressInstallButton ->
            ( model
            , case model.securityCode of
                Nothing ->
                    Cmd.none

                Just code ->
                    installApp code model.installBox
            )

        InstallBox txt ->
            ( { model | installBox = txt }, Cmd.none )

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
    case model.securityCode of
        Nothing ->
            badSecurityCode

        Just _ ->
            goodSecurityCode model


badSecurityCode : E.Element Msg
badSecurityCode =
    E.text <|
        "BigWebThing can't run at the moment because it could "
            ++ "not read the security code from the URL."


prettyHttpError : Http.Error -> String
prettyHttpError err =
    case err of
        Http.BadUrl str ->
            "bad url: " ++ str

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus code ->
            "bad status code: " ++ String.fromInt code

        Http.BadBody str ->
            "bad response body: " ++ str


goodSecurityCode : Model -> E.Element Msg
goodSecurityCode model =
    E.column
        [ E.width E.fill
        , E.padding 5
        , E.spacing 5
        ]
    <|
        [ installBox model.installBox
        , installButton
        ]
            ++ (case model.installErr of
                    Nothing ->
                        []

                    Just err ->
                        [ E.text <|
                            "Error installing app: "
                                ++ prettyHttpError err
                        ]
               )


installButton : E.Element Msg
installButton =
    Ei.button []
        { onPress = Just PressInstallButton
        , label = E.text "Install app"
        }


installBox : String -> E.Element Msg
installBox txt =
    Ei.text []
        { onChange = InstallBox
        , text = txt
        , placeholder =
            Just <|
                Ei.placeholder [] <|
                    E.text "Enter address of app to install"
        , label =
            Ei.labelHidden "Enter address of app to install:"
        }

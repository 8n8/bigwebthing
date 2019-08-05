module Main exposing (main)

import Base64
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Debug
import Dict
import Element as E
import Element.Background as Eb
import Element.Border as Border
import Element.Font as Font
import Element.Input as Ei
import Http
import Json.Decode as Jd
import Task
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


paleBlue =
    E.rgb255 52 164 235


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
              , metadata = Ok Dict.empty
              }
            , Cmd.none
            )

        Just code ->
            ( { securityCode = Just code
              , installBox = ""
              , installErr = Nothing
              , metadata = Ok Dict.empty
              }
            , getMetadata code
            )


initViewport : Browser.Dom.Viewport
initViewport =
    { scene =
        { width = 0
        , height = 0
        }
    , viewport =
        { x = 0
        , y = 0
        , width = 0
        , height = 0
        }
    }


getMetadata : String -> Cmd.Cmd Msg
getMetadata code =
    Http.get
        { url = "/metadata/" ++ code
        , expect = Http.expectJson GotMetadata (Jd.dict metadataDec)
        }


type Msg
    = LaunchApp String
    | AppLaunched (Result Http.Error ())
    | DoNothing
    | InstallBox String
    | PressInstallButton
    | Installed (Result Http.Error ())
    | GotMetadata (Result Http.Error (Dict.Dict String Metadata))


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
    , metadata : Result Http.Error (Dict.Dict String Metadata)
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
            ( { model | installErr = Nothing }, Cmd.none )

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
                        { url = "/makeapp/" ++ code ++ "/" ++ str
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

        Just code ->
            goodSecurityCode code model


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


goodSecurityCode : String -> Model -> E.Element Msg
goodSecurityCode code model =
    E.column
        [ E.width E.fill
        , E.padding 5
        , E.spacing 20
        ]
    <|
        [ E.row [ E.width E.fill, E.spacing 5 ]
            [ installBox model.installBox
            , installButton
            ]
        , E.el [ homeFont, homeFontSize ] <| E.text "List of installed apps:"
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
            ++ apps code model


apps : String -> Model -> List (E.Element Msg)
apps code model =
    case model.metadata of
        Err err ->
            [ badMetadata err ]

        Ok mds ->
            goodMetadata code mds


goodMetadata : String -> Dict.Dict String Metadata -> List (E.Element Msg)
goodMetadata code mds =
    List.map (showMetadata code) (Dict.toList mds)


showMetadata : String -> ( String, Metadata ) -> E.Element Msg
showMetadata code ( appHash, md ) =
    Ei.button []
        { onPress = Just <| LaunchApp appHash
        , label = showMetadataContent code ( appHash, md )
        }


showMetadataContent : String -> ( String, Metadata ) -> E.Element Msg
showMetadataContent code ( _, md ) =
    E.paragraph [ E.alignTop, E.alignLeft, E.width E.fill, homeFont, homeFontSize ]
        [ E.el [ E.alignLeft ] <|
            E.image [ E.width <| E.px 130 ]
                { src = "/icons/" ++ code ++ "/" ++ md.iconUrl
                , description =
                    "Icon image for app \""
                        ++ md.name
                        ++ "\"."
                }
        , E.el [ Font.bold ] <| E.text md.name
        , E.text md.description
        ]


badMetadata : Http.Error -> E.Element Msg
badMetadata err =
    E.text <| prettyHttpError err


homeFont =
    Font.family [ Font.typeface "Ubuntu", Font.sansSerif ]


homeFontSize =
    Font.size 30


installButton : E.Element Msg
installButton =
    Ei.button
        [ Eb.color paleBlue
        , Border.rounded 5
        , E.padding 10
        , E.height <| E.px 60
        ]
        { onPress = Just PressInstallButton
        , label =
            E.el
                [ homeFont
                , homeFontSize
                , Font.color <| E.rgb 1 1 1
                ]
            <|
                E.text "Install"
        }


installBox : String -> E.Element Msg
installBox txt =
    Ei.text [ E.height <| E.px 60, homeFont, homeFontSize ]
        { onChange = InstallBox
        , text = txt
        , placeholder =
            Just <|
                Ei.placeholder [] <|
                    E.el [ homeFont, homeFontSize ] <|
                        E.text "Enter address of app to install"
        , label =
            Ei.labelHidden "Enter address of app to install:"
        }

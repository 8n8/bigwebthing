module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Element as E
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Ei
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Hat
import Html.Events exposing (onClick, onInput)
import Http
import List as L
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> DoNothing
        , onUrlChange = \_ -> DoNothing
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { boxStr = "", displayStr = "", page = Home }, Cmd.none )


type Msg
    = TypedIn String
    | FromServer (Result Http.Error String)
    | NewDocumentButtonClick
    | MembershipButtonClick
    | HomeButtonClick
    | DoNothing


type Page
    = Home
    | NewDoc


type alias Model =
    { page : Page
    , boxStr : String
    , displayStr : String
    }


postMsg : String -> Cmd Msg
postMsg txt =
    Http.post
        { url = "http://localhost:3000"
        , expect = Http.expectString FromServer
        , body = Http.stringBody "text/plain; charset=utf-8" txt
        }


update msg model =
    case msg of
        TypedIn txt ->
            ( { model | boxStr = txt }, postMsg txt )

        FromServer (Err err) ->
            ( model, Cmd.none )

        FromServer (Ok str) ->
            ( { model | displayStr = str }, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )

        MembershipButtonClick ->
            ( model, Cmd.none )

        NewDocumentButtonClick ->
            ( { model | page = NewDoc }, Cmd.none )

        HomeButtonClick ->
            ( { model | page = Home }, Cmd.none )


view model =
    { title = "BigWebThing"
    , body = [ E.layout [] (mainEl model) ]
    }


placeholder : Ei.Placeholder Msg
placeholder =
    Ei.placeholder [] <| E.text "Type here to search"


black =
    E.rgb 0 0 0


searchStyle =
    [ Border.width 1
    , Border.color black
    , Border.solid
    , Border.rounded 0
    , E.width <| E.px 600
    ]


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }


idPadding =
    { top = 5
    , right = 20
    , bottom = 0
    , left = 0
    }


idtxt =
    [ "8XKTUnNufTH"
    , "mkEqNv0zxI7"
    , "z/+npv+RPpD"
    , "vm7HyN7zc0="
    ]


myId =
    E.column
        [ Font.family [ Font.typeface "Courier" ]
        , Font.size 20
        , E.alignRight
        , E.alignTop
        ]
    <|
        (E.el [ Font.bold ] <| E.text "Public ID:")
            :: L.map E.text idtxt


searchBoxStyle =
    [ Font.family [ Font.typeface "Courier", Font.monospace ]
    , E.alignTop
    ]


topButtonStyle =
    [ E.alignLeft
    , E.alignTop
    ]


makeTopButton : ( Msg, String ) -> E.Element Msg
makeTopButton ( msg, label ) =
    E.el topButtonStyle <|
        Ei.button []
            { onPress = Just msg
            , label = E.text label
            }


topButtons =
    E.row [ E.spacing 30 ] <|
        L.map makeTopButton
            [ ( HomeButtonClick, "Home" )
            , ( NewDocumentButtonClick, "New document" )
            , ( MembershipButtonClick, "Members" )
            ]


searchBox txt =
    E.el searchBoxStyle <|
        Ei.text
            searchStyle
            { onChange = TypedIn
            , text = txt
            , placeholder = Just placeholder
            , label = Ei.labelAbove [] E.none
            }


topButtonsAndSearch txt =
    E.column [ E.spacing 20, E.alignTop ]
        [ topButtons
        , searchBox txt
        ]


topSection txt =
    E.row [ E.width E.fill, E.spacing 20 ]
        [ topButtonsAndSearch txt
        , myId
        ]


mainEl : Model -> E.Element Msg
mainEl model =
    E.column
        [ Font.family [ Font.typeface "Georgia", Font.serif ]
        , Font.size 28
        , E.width E.fill
        , E.padding 20
        ]
        [ topSection model.displayStr
        ]

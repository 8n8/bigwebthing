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
    ( { boxStr = "", displayStr = "" }, Cmd.none )


type Msg
    = TypedIn String
    | FromServer (Result Http.Error String)
    | NewDocumentButtonClick
    | MembershipButtonClick
    | DoNothing


type alias Model =
    { boxStr : String
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
            ( model, Cmd.none )


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

    --, E.padding 0
    ]


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }


createNowStyle =
    [ E.moveDown 1
    , E.alignLeft
    , E.alignTop
    , E.paddingEach { edges | top = 33 }
    ]


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
        , E.paddingEach idPadding
        ]
    <|
        (E.el [ Font.bold ] <| E.text "Public ID:")
            :: L.map E.text idtxt


searchBoxStyle =
    [ Font.family [ Font.typeface "Courier", Font.monospace ]
    , E.paddingEach
        { top = 20
        , left = 20
        , right = 0
        , bottom = 0
        }
    , E.alignTop
    ]


mainEl : Model -> E.Element Msg
mainEl model =
    E.column
        [ Font.family [ Font.typeface "Georgia", Font.serif ]
        , Font.size 28
        , E.width E.fill
        ]
        [ E.row [ E.width E.fill, E.spacing 30 ]
            [ E.el searchBoxStyle <|
                Ei.text
                    searchStyle
                    { onChange = TypedIn
                    , text = model.boxStr
                    , placeholder = Just placeholder
                    , label = Ei.labelAbove [] E.none
                    }
            , E.el createNowStyle <|
                Ei.button
                    []
                    { onPress = Just NewDocumentButtonClick
                    , label = E.text "New document"
                    }
            , E.el createNowStyle <|
                Ei.button
                    []
                    { onPress = Just MembershipButtonClick
                    , label = E.text "Members"
                    }
            , myId
            ]
        ]

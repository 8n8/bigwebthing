module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug
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
        , onUrlChange = \url -> NewUrl url.path
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( { boxStr = "", displayStr = "", page = Home, key = key }, Cmd.none )


type Msg
    = TypedIn String
    | FromServer (Result Http.Error String)
    | NewDocumentButtonClick
    | MembershipButtonClick
    | HomeButtonClick
    | DoNothing
    | NewUrl String
    | UploadDoc


type Page
    = Home
    | NewDoc
    | Members
    | Unknown


type alias Model =
    { page : Page
    , boxStr : String
    , displayStr : String
    , key : Nav.Key
    }


postMsg : String -> Cmd Msg
postMsg txt =
    Http.post
        { url = "http://localhost:3000"
        , expect = Http.expectString FromServer
        , body = Http.stringBody "text/plain; charset=utf-8" txt
        }


setNewDocUrl key = Nav.pushUrl key "/newdocument"

update msg model =
    case msg of
        NewUrl "/" ->
            ( { model | page = Home }, Cmd.none ) 

        NewUrl "/newdocument" ->
            ( { model | page = NewDoc }, Cmd.none )

        NewUrl "/members" ->
            ( { model | page = Members }, Cmd.none )

        NewUrl _ -> 
            ( { model | page = Unknown }, Cmd.none )

        TypedIn txt ->
            ( { model | boxStr = txt }, postMsg txt )

        FromServer (Err err) ->
            ( model, Cmd.none )

        FromServer (Ok str) ->
            ( { model | displayStr = str }, Cmd.none )

        UploadDoc ->
            ( model, Cmd.none )
 
        DoNothing ->
            ( model, Cmd.none )

        MembershipButtonClick ->
            ( { model | page = Members }
            , Nav.pushUrl model.key "/members"
            )

        NewDocumentButtonClick ->
            ( { model | page = NewDoc }
            , Nav.pushUrl model.key "/newdocument"
            )

        HomeButtonClick ->
            ( { model | page = Home }, Cmd.none )


view model =
    { title = "BigWebThing"
    , body = [ E.layout [] (mainEl <| Debug.log "model" model) ]
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
    , Font.size 28
    , E.alignTop
    ]


topButtonStyle =
    [ E.alignLeft
    , E.alignTop
    , Font.size 28
    , Font.family [ Font.typeface "Georgia", Font.serif ]
    ]

buttonColor p1 p2 =
    if p1 == p2 then
        Bg.color (E.rgb255 201 221 255)
    else
        Bg.color (E.rgb255 255 255 255)

makeTopButton : Page -> ( Msg, Page, String ) -> E.Element Msg
makeTopButton page ( msg, buttonPage, label ) =
    E.el ((buttonColor page buttonPage) :: topButtonStyle) <|
        Ei.button []
            { onPress = Just msg
            , label = E.text label
            }


topButtons page =
    E.row [ E.spacing 20, E.alignTop ] <|
        L.map (makeTopButton page)
            [ ( HomeButtonClick, Home, "Home" )
            , ( NewDocumentButtonClick, NewDoc, "New document" )
            , ( MembershipButtonClick, Members, "Members" )
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


topButtonsAndSearch page txt =
    E.column [ E.spacing 20, E.alignTop ]
        [ topButtons page
        , searchBox txt
        ]


homeTopSection txt =
    E.row [ E.width E.fill, E.spacing 20 ]
        [ topButtonsAndSearch Home txt
        , myId
        ]

newDocTopSection =
    E.row [ E.width E.fill, E.spacing 20 ]
        [ topButtons NewDoc
        , myId
        ]

membersTopSection =
    E.row [ E.width E.fill, E.spacing 20 ]
        [ topButtons Members
        , myId
        ]

mainEl : Model -> E.Element Msg
mainEl model = case model.page of
    Home -> homePage model
    NewDoc -> newDocPage model
    Members -> memberPage model
    Unknown -> E.none

memberPage model =
    E.column
        [ E.width E.fill
        , E.padding 20
        ]
        [ membersTopSection
        ]

homePage model =
    E.column
        [ E.width E.fill
        , E.padding 20
        ]
        [ homeTopSection model.boxStr
        ]

newDocPage model = 
    E.column
         [ E.width E.fill
         , E.padding 20
         ]
         [ newDocTopSection
         , Ei.button []
             { onPress = Just UploadDoc
             , label = E.text "Upload document"
             }
         ]

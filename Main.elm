module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug
import Element as E
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Ei
import File
import File.Select as Fs
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Hat
import Html.Events exposing (onClick, onInput)
import Http
import List as L
import Url
import Url.Parser as Up exposing ((</>))


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = \_ -> DoNothing
        , onUrlChange = \url -> NewUrl url
        }


parseRoute : Up.Parser (( Page, Maybe String ) -> a) a
parseRoute =
    Up.oneOf
        [ Up.map (\x -> ( Home, Just x )) Up.string
        , Up.map (\x -> ( NewDoc, Just x )) <|
            Up.string
                </> Up.s "newdocument"
        , Up.map (\x -> ( Members, Just x )) <|
            Up.string
                </> Up.s "members"
        ]


urlToPage : Url.Url -> ( Page, Maybe String )
urlToPage url =
        case Up.parse parseRoute url of
            Nothing ->
                ( Unknown, Nothing )

            Just result ->
                result


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    case urlToPage url of
        ( page, securityCode ) ->
            ( { page = page
              , securityCode = securityCode
              , boxStr = ""
              , displayStr = ""
              , key = key
              , tags = ""
              , uploadStatus = Ok ()
              }
            , Cmd.none
            )


type Msg
    = TypedIn String
    | TagBox String
    | FromServer (Result Http.Error String)
    | NewDocumentButtonClick
    | MembershipButtonClick
    | HomeButtonClick
    | DoNothing
    | NewUrl Url.Url
    | UploadDoc
    | DocLoaded File.File
    | AppHash (Result Http.Error ())


type Page
    = Home
    | NewDoc
    | Members
    | Unknown


type alias Model =
    { page : Page
    , securityCode : Maybe String
    , boxStr : String
    , displayStr : String
    , key : Nav.Key
    , tags : String
    , uploadStatus : Result Http.Error ()
    }


postMsg : String -> Cmd Msg
postMsg txt =
    Http.post
        { url = "http://localhost:3000"
        , expect = Http.expectString FromServer
        , body = Http.stringBody "text/plain; charset=utf-8" txt
        }


setNewDocUrl key =
    Nav.pushUrl key "/newdocument"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TagBox tags ->
            ( { model | tags = tags }, Cmd.none)
        DocLoaded file ->
            case model.securityCode of
                Nothing ->
                    ( model, Cmd.none )

                Just code ->
                    ( model
                    , Http.post
                        { url = "/saveapp/" ++ code
                        , body = Debug.log "body" <|
                            Http.multipartBody
                                [ Http.filePart "file" file
                                , Http.stringPart "tags" model.tags
                                ]
                        , expect = Http.expectWhatever AppHash
                        }
                    )

        AppHash status ->
            ( { model | uploadStatus = status }
            , Cmd.none
            )

        NewUrl path ->
            case urlToPage path of
                ( page, _ ) ->
                    ( { model | page = page }, Cmd.none )

        TypedIn txt ->
            ( { model | boxStr = txt }, postMsg txt )

        FromServer (Err err) ->
            ( model, Cmd.none )

        FromServer (Ok str) ->
            ( { model | displayStr = str }, Cmd.none )

        UploadDoc ->
            ( model, Fs.file [ "application/octet-stream" ] DocLoaded )

        DoNothing ->
            ( model, Cmd.none )

        MembershipButtonClick ->
            case model.securityCode of
                Nothing ->
                    ( { model | page = Unknown }, Cmd.none )

                Just code ->
                    ( { model | page = Members }
                    , Nav.pushUrl model.key <|
                        "/"
                            ++ code
                            ++ "/members"
                    )

        NewDocumentButtonClick ->
            case model.securityCode of
                Nothing ->
                    ( { model | page = Unknown }, Cmd.none )

                Just code ->
                    ( { model | page = NewDoc }
                    , Nav.pushUrl model.key <|
                        "/"
                            ++ code
                            ++ "/newdocument"
                    )

        HomeButtonClick ->
            case model.securityCode of
                Nothing ->
                    ( { model | page = Unknown }, Cmd.none )

                Just code ->
                    ( { model | page = Home }
                    , Nav.pushUrl model.key <| "/" ++ code
                    )


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
    E.el (buttonColor page buttonPage :: topButtonStyle) <|
        Ei.button [ E.padding 5 ]
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


newDocTopSection tagText =
    E.column [ E.width E.fill ]
        [ E.row [ E.width E.fill, E.spacing 20 ]
            [ E.column
                [ E.alignTop, E.height E.fill ]
                [ topButtons NewDoc ]
            , myId
            ]
        , E.row [ E.spacing 20 ]
            [ E.el [] <| Ei.text
                [ E.width <| E.px 600, E.height <| E.px 50 ]
                { onChange = TagBox
                , text = tagText
                , placeholder = Just <| Ei.placeholder [] <|
                    E.text "Type tags separated by commas"
                , label = Ei.labelAbove [] E.none
                }                     
            , Ei.button
                [ E.alignBottom
                , E.padding 5
                , Border.width 1
                , E.height <| E.px 50
                ]
                { onPress = Just UploadDoc
                , label =
                    E.el [ E.padding 3 ] <|
                        E.text "Choose local file"
                }
            ]
        ]


membersTopSection =
    E.row [ E.width E.fill, E.spacing 20 ]
        [ topButtons Members
        , myId
        ]


mainEl : Model -> E.Element Msg
mainEl model =
    case model.page of
        Home ->
            homePage model

        NewDoc ->
            newDocPage model

        Members ->
            memberPage model

        Unknown ->
            unknownPage


unknownPage =
    E.el [] <| E.text "Page doesn't exist"


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
        [ newDocTopSection model.tags
        ]

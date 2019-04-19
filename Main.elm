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
import Json.Decode as Jd
import Json.Encode as Je
import Set


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

initModel page securityCode key =
    { page = page
    , securityCode = securityCode
    , searchStr = ""
    , fileUpload = Nothing
    , searchResults = Ok []
    , key = key
    , newTagsBox = ""
    , selectedTags = []
    , uploadStatus = Ok ()
    }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    case urlToPage url of
        ( page, securityCode ) ->
            ( initModel page securityCode key
            , case securityCode of
                Nothing -> Cmd.none
                Just code -> postMsg "" [] code
            )


type Msg
    = SearchBox String
    | TagBox String
    | NewDocumentButtonClick
    | SearchResults (Result Http.Error (List SearchResult))
    | MembershipButtonClick
    | HomeButtonClick
    | DoNothing
    | NewUrl Url.Url
    | UploadDoc
    | DocLoaded File.File
    | AppHash (Result Http.Error ())
    | UnchooseTag String
    | ChooseTag String
    | ChooseDoc


type Page
    = Home
    | NewDoc
    | Members
    | Unknown

type alias SearchResult =
    { author : String
    , tags : List String
    , hash : String
    , posixTime : Int
    }

decodeSearchResult : Jd.Decoder SearchResult
decodeSearchResult =
    Jd.map4 SearchResult
        (Jd.field "Author" Jd.string)
        (Jd.field "Tags" (Jd.list Jd.string))
        (Jd.field "Hash" Jd.string)
        (Jd.field "Posixtime" Jd.int)

decodeSearchResults : Jd.Decoder (List SearchResult)
decodeSearchResults = Jd.list decodeSearchResult

type alias Model =
    { page : Page
    , securityCode : Maybe String
    , searchStr : String
    , searchResults : Result Http.Error (List SearchResult)
    , key : Nav.Key
    , newTagsBox : String
    , selectedTags : List String
    , fileUpload : Maybe File.File
    , uploadStatus : Result Http.Error ()
    }

encodeSearchQuery tags searchString =
    Je.object
        [ ("Tags", (Je.list Je.string) tags)
        , ("SearchString", Je.string searchString)
        ]

postMsg : String -> List String -> String -> Cmd Msg
postMsg searchString tags securityCode =
    Http.post
        { url = "http://localhost:3000/searchapps/" ++ securityCode
        , expect = Http.expectJson SearchResults decodeSearchResults
        , body = Http.jsonBody <| encodeSearchQuery tags searchString
        }


setNewDocUrl key =
    Nav.pushUrl key "/newdocument"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChooseTag tag ->
            let newTags = tag :: model.selectedTags
            in case model.securityCode of
                Nothing -> ( model, Cmd.none )
                Just code ->
                    ( { model | selectedTags = newTags }
                    , postMsg model.searchStr newTags code
                    )
        UnchooseTag tag ->
            let
                newTags = List.filter ((/=) tag) model.selectedTags
            in case model.securityCode of
                Nothing -> ( model, Cmd.none)
                Just code ->
                    ( { model | selectedTags = newTags }
                    , postMsg model.searchStr newTags code
                    )
        SearchResults results ->
            ( { model | searchResults = results }, Cmd.none )
        TagBox tags ->
            ( { model | newTagsBox = tags }, Cmd.none)
        DocLoaded file ->
            ( { model | fileUpload = Just file }, Cmd.none)
        UploadDoc ->
            case (model.securityCode, model.fileUpload) of
                (Just code, Just file) ->
                    ( { model | fileUpload = Nothing }
                    , Http.post
                        { url = "/saveapp/" ++ code
                        , body = Debug.log "body" <|
                            Http.multipartBody
                                [ Http.filePart "file" file
                                , Http.stringPart "tags" model.newTagsBox
                                ]
                        , expect = Http.expectWhatever AppHash
                        }
                    )

                _ ->
                    ( model, Cmd.none )

        AppHash status ->
            ( { model | uploadStatus = status }
            , Cmd.none
            )

        NewUrl path ->
            case urlToPage path of
                ( page, _ ) ->
                    ( { model | page = page }, Cmd.none )

        SearchBox txt ->
            case model.securityCode of
                Nothing -> ( { model | page = Unknown }, Cmd.none )
                Just code ->
                    ( { model | searchStr = txt }, postMsg txt model.selectedTags code )

        -- FromServer (Err err) ->
        --     ( model, Cmd.none )

        -- FromServer (Ok searchResults) ->
        --     ( { model | displayStr = str }, Cmd.none )

        ChooseDoc ->
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
                    , Cmd.batch
                        [ Nav.pushUrl model.key <| "/" ++ code
                        , postMsg
                            model.searchStr
                            model.selectedTags
                            code
                        ]
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
        [ Font.family [ Font.typeface "Courier", Font.monospace ]
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
    , Font.size 33 
    , Font.family [ Font.typeface "Georgia", Font.serif ]
    ]



-- blue = E.rgb255 201 221 255
blue = E.rgb255 132 179 255
paleBlue = E.rgb255 201 221 255
grey = E.rgb255 169 169 169
white = E.rgb255 255 255 255

buttonColor p1 p2 =
    if p1 == p2 then
        Bg.color blue

    else
        Bg.color paleBlue


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
            { onChange = SearchBox
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

newDocButtonStyle =
                [ E.alignBottom
                , E.padding 5
                , Border.width 1
                , E.height <| E.px 50
                ]


greyNewDocButtonStyle =
        newDocButtonStyle ++
        [ Border.color grey
        , Font.color grey
        , E.htmlAttribute <| Hat.style "box-shadow" "none"
        ]

newDocTopSection tagText fileUpload =
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
            , Ei.button newDocButtonStyle
                { onPress = Just ChooseDoc
                , label =
                    E.el [ E.padding 3 ] <|
                        E.text "Choose local file"
                }
            , Ei.button
                (case fileUpload of
                    Nothing -> greyNewDocButtonStyle
                    Just _ -> newDocButtonStyle)
                { onPress = case fileUpload of
                      Nothing -> Nothing
                      Just _ -> Just UploadDoc
                , label = E.el [ E.padding 3 ] <|
                    E.text "Upload file"
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


tagStyle color =
    [ Bg.color color
    , E.alignLeft
    , Font.size 24 
    , Font.family [ Font.typeface "Courier", Font.monospace ]
    , E.paddingXY 0 5
    ]

showTag msg color tag =
   Ei.button (tagStyle color) {onPress = Just (msg tag), label = E.text tag}


homeShowTags msgFun tags color =
    case tags of
        [] -> E.none
        _ -> E.wrappedRow [E.spacing 15, E.paddingXY 0 10] <|
                List.map (showTag msgFun color) tags

addToSet : List String -> Set.Set String -> Set.Set String
addToSet ls accum = Set.union accum (Set.fromList ls)

choosableTags : Result Http.Error (List SearchResult) -> List String
choosableTags searchResults = case searchResults of
    Err _ -> []
    Ok results ->
        let
            tags = List.map .tags results
        in Set.toList <| List.foldl addToSet Set.empty tags
        
viewSearchResult result =
    E.column []
        [ E.text result.author
        , homeShowTags ChooseTag result.tags paleBlue
        , E.text result.hash
        , E.text <| String.fromInt result.posixTime
        ]

homeSearchResults results =
    case results of
        Err _ -> E.text "No results"
        Ok r -> E.column [] <| List.map viewSearchResult r

homePage : Model -> E.Element Msg
homePage model =
    E.column
        [ E.width E.fill
        , E.padding 20
        ]
        [ homeTopSection model.searchStr
        , homeShowTags UnchooseTag model.selectedTags blue
        , homeShowTags ChooseTag (choosableTags model.searchResults) paleBlue
        , homeSearchResults model.searchResults
        ]


newDocPage model =
    E.column
        [ E.width E.fill
        , E.padding 20
        ]
        [ newDocTopSection model.newTagsBox model.fileUpload
        ]

module Main exposing (main)

import Browser
import Browser.Navigation as Nav
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
import Json.Decode as Jd
import Json.Encode as Je
import List as L
import Set
import Task
import Time
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
    (</>) (Up.s "getapp") <|
        Up.oneOf
            [ Up.map (\x -> ( Home, Just x )) <|
                Up.string
                    </> Up.s "index.html"
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
    , searchResults = Ok { apps = [], tags = [] }
    , key = key
    , newTagsBox = ""
    , selectedTags = Set.empty
    , unselectedTags = Set.empty
    , uploadStatus = Ok ()
    , zone = Time.utc
    , checkedBoxes = Set.empty
    , selectAll = False
    , sendDrawOpen = Nothing
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    case urlToPage url of
        ( page, securityCode ) ->
            ( initModel page securityCode key
            , case securityCode of
                Nothing ->
                    Cmd.none

                Just code ->
                    Cmd.batch
                        [ postMsg "" [] code, Task.perform Zone Time.here ]
            )


type Msg
    = SearchBox String
    | TagBox String
    | NewDocumentButtonClick
    | NewSearchResults (Result Http.Error SearchResults)
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
    | Zone Time.Zone
    | FileSelected String Bool
    | TickAll Bool
    | LaunchApp String
    | AppLaunched (Result Http.Error ())
    | AppSent (Result Http.Error ())
    | OpenSendDrawer String
    | UpdateSendDrawer SendDrawer
    | SendApp


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


type alias SearchResults =
    { apps : List SearchResult
    , tags : List String
    }


decodeSearchResults : Jd.Decoder SearchResults
decodeSearchResults =
    Jd.map2 SearchResults
        (Jd.field "Apps" (Jd.list decodeSearchResult))
        (Jd.field "Tags" (Jd.list Jd.string))

type alias Model =
    { page : Page
    , securityCode : Maybe String
    , searchStr : String
    , searchResults : Result Http.Error SearchResults
    , key : Nav.Key
    , sendDrawOpen : Maybe SendDrawer
    , newTagsBox : String
    , unselectedTags : Set.Set String
    , selectedTags : Set.Set String
    , fileUpload : Maybe File.File
    , uploadStatus : Result Http.Error ()
    , zone : Time.Zone
    , checkedBoxes : Set.Set String
    , selectAll : Bool
    }

type alias SendDrawer =
    { recipient : String
    , appHash : String
    }

encodeSearchQuery tags searchString =
    Je.object
        [ ( "Tags", Je.list Je.string tags )
        , ( "SearchString", Je.string searchString )
        ]

encodeSendApp sendDrawer = -- apphash recipients =
    Je.object
        [ ( "AppHash", Je.string sendDrawer.appHash )
        , ( "Recipients", Je.list Je.string [sendDrawer.recipient] )
        ]

postSendApp sendDrawer securityCode =
    Http.post
        { url = "/sendapp/" ++ securityCode
        , expect = Http.expectWhatever AppSent
        , body = Http.jsonBody <| encodeSendApp sendDrawer
        }


postMsg : String -> List String -> String -> Cmd Msg
postMsg searchString tags securityCode =
    Http.post
        { url = "/searchapps/" ++ securityCode
        , expect = Http.expectJson NewSearchResults decodeSearchResults
        , body = Http.jsonBody <| encodeSearchQuery tags searchString
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OpenSendDrawer hash ->
            ( { model | sendDrawOpen = Just
                    { recipient = ""
                    , appHash = hash
                    }
              }
            , Cmd.none
            )

        UpdateSendDrawer newDrawer ->
            ( { model | sendDrawOpen = Just newDrawer }, Cmd.none )
            
        SendApp -> case (model.sendDrawOpen, model.securityCode) of
            (Just drawer, Just code) ->
                ( { model | sendDrawOpen = Nothing }
                , postSendApp drawer code
                )
            _ -> ( model, Cmd.none )

        AppSent _ ->
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

        TickAll True ->
            case model.searchResults of
                Err _ ->
                    ( { model | selectAll = True }, Cmd.none )

                Ok results ->
                    ( { model
                        | checkedBoxes = Set.fromList (List.map .hash results.apps)
                        , selectAll = True
                      }
                    , Cmd.none
                    )

        TickAll False ->
            ( { model
                | checkedBoxes = Set.empty
                , selectAll = False
              }
            , Cmd.none
            )

        FileSelected hash False ->
            ( { model
                | checkedBoxes = Set.remove hash model.checkedBoxes
              }
            , Cmd.none
            )

        FileSelected hash True ->
            ( { model
                | checkedBoxes = Set.insert hash model.checkedBoxes
              }
            , Cmd.none
            )

        Zone zone ->
            ( { model | zone = zone }, Cmd.none )

        ChooseTag tag ->
            let
                newTags =
                    Set.insert tag model.selectedTags
            in
            case model.securityCode of
                Nothing ->
                    ( model, Cmd.none )

                Just code ->
                    ( { model | selectedTags = newTags }
                    , postMsg model.searchStr (Set.toList newTags) code
                    )

        UnchooseTag tag ->
            let
                newTags =
                    Set.remove tag model.selectedTags
            in
            case model.securityCode of
                Nothing ->
                    ( model, Cmd.none )

                Just code ->
                    ( { model | selectedTags = newTags }
                    , postMsg model.searchStr (Set.toList newTags) code
                    )

        NewSearchResults results ->
            ( { model
                | searchResults = results
                , unselectedTags =
                    case results of
                        Err _ ->
                            model.unselectedTags

                        Ok r ->
                            let
                                newTags =
                                    Set.fromList r.tags
                            in
                            Set.diff newTags model.selectedTags
              }
            , Cmd.none
            )

        TagBox tags ->
            ( { model | newTagsBox = tags }, Cmd.none )

        DocLoaded file ->
            ( { model | fileUpload = Just file }, Cmd.none )

        UploadDoc ->
            case ( model.securityCode, model.fileUpload ) of
                ( Just code, Just file ) ->
                    ( { model | fileUpload = Nothing }
                    , Http.post
                        { url = "/saveapp/" ++ code
                        , body =
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
                Nothing ->
                    ( { model | page = Unknown }, Cmd.none )

                Just code ->
                    ( { model | searchStr = txt }, postMsg txt (Set.toList model.selectedTags) code )

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
                        "/getapp/"
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
                        "/getapp/"
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
                        [ Nav.pushUrl model.key <| "/getapp/" ++ code ++ "/index.html"
                        , postMsg
                            model.searchStr
                            (Set.toList model.selectedTags)
                            code
                        ]
                    )


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
        (idStyle
            ++ [ E.alignRight
               , E.alignTop
               ]
        )
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


blue =
    E.rgb255 132 179 255


paleBlue =
    E.rgb255 214 229 255


grey =
    E.rgb255 169 169 169


white =
    E.rgb255 255 255 255


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
    newDocButtonStyle
        ++ [ Border.color grey
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
            [ E.el [] <|
                Ei.text
                    [ E.width <| E.px 600, E.height <| E.px 50 ]
                    { onChange = TagBox
                    , text = tagText
                    , placeholder =
                        Just <|
                            Ei.placeholder [] <|
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
                    Nothing ->
                        greyNewDocButtonStyle

                    Just _ ->
                        newDocButtonStyle
                )
                { onPress =
                    case fileUpload of
                        Nothing ->
                            Nothing

                        Just _ ->
                            Just UploadDoc
                , label =
                    E.el [ E.padding 3 ] <|
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
    Ei.button (tagStyle color) { onPress = Just (msg tag), label = E.text tag }


tagSort =
    L.sortWith tagCompare


tagCompare a b =
    case compare (String.length a) (String.length b) of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            compare a b


homeShowTags : (String -> Msg) -> List String -> E.Color -> E.Attribute Msg -> E.Element Msg
homeShowTags msgFun tags color padding =
    case tags of
        [] ->
            E.none

        _ ->
            E.wrappedRow [ E.spacing 15, padding ] <|
                List.map (showTag msgFun color) (tagSort tags)


addToSet : List String -> Set.Set String -> Set.Set String
addToSet ls accum =
    Set.union accum (Set.fromList ls)


choosableTags : Result Http.Error SearchResults -> List String
choosableTags searchResults =
    case searchResults of
        Err _ ->
            []

        Ok results ->
            results.tags


idStyle =
    [ Font.family [ Font.typeface "Courier", Font.monospace ]
    , Font.size 20
    ]


normalText =
    [ Font.family [ Font.typeface "Georgia", Font.serif ]
    , Font.size 20
    ]


dayToStr : Time.Weekday -> String
dayToStr weekday =
    case weekday of
        Time.Mon ->
            "Monday"

        Time.Tue ->
            "Tuesday"

        Time.Wed ->
            "Wednesday"

        Time.Thu ->
            "Thursday"

        Time.Fri ->
            "Friday"

        Time.Sat ->
            "Saturday"

        Time.Sun ->
            "Sunday"


monthToStr : Time.Month -> String
monthToStr month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


prettyTime posix zone =
    let
        year =
            Time.toYear zone posix

        month =
            Time.toMonth zone posix

        day =
            Time.toDay zone posix

        dayName =
            Time.toWeekday zone posix

        hour =
            Time.toHour zone posix

        minute =
            Time.toMinute zone posix
    in
    dayToStr dayName
        ++ " "
        ++ String.fromInt day
        ++ " "
        ++ monthToStr month
        ++ " "
        ++ String.fromInt year
        ++ ", "
        ++ String.fromInt hour
        ++ ":"
        ++ (let
                strMin =
                    String.fromInt minute
            in
            case String.length strMin of
                1 ->
                    "0" ++ strMin

                _ ->
                    strMin
           )


bigCheckbox onChange checked =
    Ei.checkbox
        [ E.alignTop
        , E.alignLeft
        ]
        { onChange = onChange
        , icon = defaultCheckbox
        , checked = checked
        , label = Ei.labelHidden "Select document"
        }


viewSearchResult : Time.Zone -> Set.Set String -> String -> Maybe SendDrawer -> SearchResult -> E.Element Msg
viewSearchResult zone checkedBoxes securityCode sendDrawOpen result =
    E.row [ E.spacing 30 ]
        [ bigCheckbox (FileSelected result.hash) (Set.member result.hash checkedBoxes)
        , E.column [ E.spacing 10 ]
            [ homeShowTags ChooseTag result.tags paleBlue (E.padding 0)
            , E.row []
                [ E.el normalText <| E.text "Author ID: "
                , E.el idStyle <| E.text result.author
                ]
            , E.row []
                [ E.el normalText <| E.text "Document ID: "
                , E.el idStyle <| Ei.button [] { onPress = Just (LaunchApp result.hash), label = E.text result.hash }
                ]
            , E.row []
                [ E.el normalText <| E.text "Date: "
                , E.el normalText <|
                    E.text <|
                        prettyTime
                            (Time.millisToPosix <| result.posixTime * 1000)
                            zone
                ]
            , let button = Ei.button [] { onPress = Just (OpenSendDrawer result.hash), label = E.text "Send this app" }
                  drawer recipStr = E.row []
                      [ Ei.text []
                          { onChange = \txt -> UpdateSendDrawer
                              { recipient = txt
                              , appHash = result.hash
                              }
                          , text = recipStr
                          , placeholder = Just <| Ei.placeholder [] <| E.text "Type recipient ID code"
                          , label = Ei.labelAbove [] E.none
                          }
                      , Ei.button []
                          { onPress = Just SendApp
                          , label = E.text "Send"
                          }
                      ]
              in case sendDrawOpen of
                  Nothing -> button
                  Just draw ->
                      if draw.appHash == result.hash then
                          drawer draw.recipient
                      else
                          button
            ]
        ]


defaultCheckbox : Bool -> E.Element msg
defaultCheckbox checked =
    E.el
        [ E.width (E.px 35)
        , E.height (E.px 35)
        , Font.color white
        , E.centerY
        , Font.size 9
        , Font.center
        , Border.rounded 3
        , Border.color <|
            if checked then
                E.rgb255 59 153 252

            else
                E.rgb255 211 211 211
        , Border.shadow <|
            { offset = ( 0, 0 )
            , blur = 1
            , size = 1
            , color =
                if checked then
                    E.rgba255 238 238 238 0

                else
                    E.rgb255 238 238 238
            }
        , Bg.color <|
            if checked then
                E.rgb255 59 153 252

            else
                white
        , Border.width <|
            if checked then
                0

            else
                1
        ]
        (if checked then
            E.el
                [ Border.color white
                , E.height (E.px 12)
                , E.width (E.px 18)
                , E.rotate (degrees -45)
                , E.centerX
                , E.centerY
                , E.moveUp 2
                , Border.widthEach
                    { top = 0
                    , left = 4
                    , bottom = 4
                    , right = 0
                    }
                ]
                E.none

         else
            E.none
        )


noResults =
    E.text "No results."


homeSearchResults : Result Http.Error SearchResults -> Time.Zone -> Set.Set String -> Bool -> String -> Maybe SendDrawer -> E.Element Msg
homeSearchResults results zone checkedBoxes selectAll securityCode sendDrawOpen =
    case results of
        Err _ ->
            noResults

        Ok r ->
            case r.apps of
                [] ->
                    noResults

                _ ->
                    E.column
                        [ E.spacing 30, E.paddingXY 0 10 ]
                    <|
                        (E.el [] <| bigCheckbox TickAll selectAll)
                            :: List.map (viewSearchResult zone checkedBoxes securityCode sendDrawOpen) r.apps


allChecked searchResults checkedBoxes =
    checkedBoxes == Set.fromList (List.map .hash searchResults)


homePage : Model -> E.Element Msg
homePage model =
    E.column
        [ E.width E.fill
        , E.padding 20
        , E.spacing 20
        ]
        [ homeTopSection model.searchStr
        , homeShowTags UnchooseTag (Set.toList model.selectedTags) blue (E.padding 0)
        , homeShowTags
            ChooseTag
            (Set.toList model.unselectedTags)
            paleBlue
            (E.padding 0)
        , case model.securityCode of
            Nothing ->
                E.text "Internal error: no security code."

            Just code ->
                homeSearchResults model.searchResults model.zone model.checkedBoxes model.selectAll code model.sendDrawOpen
        ]


newDocPage model =
    E.column
        [ E.width E.fill
        , E.padding 20
        ]
        [ newDocTopSection model.newTagsBox model.fileUpload
        ]

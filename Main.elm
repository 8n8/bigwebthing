module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug exposing (log)
import Dict exposing (Dict)
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
import Parser as P exposing ((|.), (|=))
import Set exposing (Set)
import String.Extra as Se
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


parseRoute : Up.Parser (String -> a) a
parseRoute =
    Up.s "getapp" </> Up.string </> Up.s "index.html"


initModel : Maybe String -> Nav.Key -> Model
initModel securityCode key =
    { securityCode = securityCode
    }


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


type alias Invite =
    { to : String
    , from : String
    , sig : String
    , posixTime : Int
    }


type alias MsgMaker =
    { chosenTags : Set String
    , currentTag : String
    , chosenRecipients : Set String
    , recipientSearchStr : String
    , recipChosenTags : Set String
    , msgHash : Maybe String
    }


type alias SendDrawer =
    { recipient : String
    , appHash : String
    }


encodeSearchQuery : List String -> String -> Je.Value
encodeSearchQuery tags searchString =
    Je.object
        [ ( "Tags", Je.list Je.string tags )
        , ( "SearchString", Je.string searchString )
        ]


encodeSendApp : SendDrawer -> Je.Value
encodeSendApp sendDrawer =
    Je.object
        [ ( "AppHash", Je.string sendDrawer.appHash )
        , ( "Recipient", Je.string sendDrawer.recipient )
        ]


invitesDec : Jd.Decoder (List Invite)
invitesDec =
    Jd.list inviteDec


accessDec : Jd.Decoder (Dict String (Set String))
accessDec =
    Jd.dict <| Jd.map Set.fromList <| Jd.list Jd.string


tagsDec : Jd.Decoder (Dict String (Set String))
tagsDec =
    Jd.dict <| Jd.map Set.fromList <| Jd.list Jd.string


byte32Dec : Jd.Decoder (Dict String String)
byte32Dec =
    Jd.dict Jd.string


inviteDec : Jd.Decoder Invite
inviteDec =
    Jd.map4 Invite
        (Jd.field "t" Jd.string)
        (Jd.field "f" Jd.string)
        (Jd.field "s" Jd.string)
        (Jd.field "d" Jd.int)


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


placeholder : Ei.Placeholder Msg
placeholder =
    Ei.placeholder [] <| E.text "Type here to search"


black : E.Color
black =
    E.rgb 0 0 0


searchStyle : List (E.Attribute Msg)
searchStyle =
    [ E.width <| E.px 600 ]


myId : String -> E.Element Msg
myId id =
    E.row
        (E.spacing 20 :: idStyle)
    <|
        [ E.el [ Font.bold ] <| E.text "Public ID:"
        , E.text id
        ]


monoFont : E.Attribute Msg
monoFont =
    Font.family [ Font.typeface "Source Code Pro", Font.monospace ]


monoSize : E.Attribute Msg
monoSize =
    Font.size 37


searchBoxStyle : List (E.Attribute Msg)
searchBoxStyle =
    [ monoFont
    , Font.size 37
    , E.alignTop
    ]


topButtonStyle : List (E.Attribute Msg)
topButtonStyle =
    [ E.alignLeft
    , E.alignTop
    , Font.size 28
    , Font.family [ Font.typeface "Georgia", Font.serif ]
    ]


blue : E.Color
blue =
    E.rgb255 132 179 255


paleBlue : E.Color
paleBlue =
    E.rgb255 214 229 255


grey : E.Color
grey =
    E.rgb255 169 169 169


white : E.Color
white =
    E.rgb255 255 255 255


addToSet : List String -> Set.Set String -> Set.Set String
addToSet ls accum =
    Set.union accum (Set.fromList ls)


idStyle : List (E.Attribute Msg)
idStyle =
    [ Font.family [ Font.typeface "Source Code Pro", Font.monospace ]
    , Font.size 37
    ]


normalText : List (E.Attribute Msg)
normalText =
    [ Font.family [ Font.typeface "Georgia", Font.serif ]
    , Font.size 28
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


prettyTime : Time.Posix -> Time.Zone -> String
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



{-
   bigCheckbox : (Bool -> Msg) -> Bool -> E.Element Msg
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
-}
{-
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
               , let
                   button =
                       Ei.button [] { onPress = Just (OpenSendDrawer result.hash), label = E.text "Send this app" }

                   drawer recipStr =
                       E.row []
                           [ Ei.text []
                               { onChange =
                                   \txt ->
                                       UpdateSendDrawer
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
                 in
                 case sendDrawOpen of
                   Nothing ->
                       button

                   Just draw ->
                       if draw.appHash == result.hash then
                           drawer draw.recipient

                       else
                           button
               ]
           ]
-}
{-
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
-}


noResults : E.Element Msg
noResults =
    E.text "No results."



{-
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
-}


allChecked searchResults checkedBoxes =
    checkedBoxes == Set.fromList (List.map .hash searchResults)


homePage : Model -> E.Element Msg
homePage model =
    E.column
        [ E.width E.fill
        , E.padding 5
        , E.spacing 5
        ]
        [ E.text "hello"
        ]

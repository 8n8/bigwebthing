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
import Parser as P exposing ((|.), (|=))
import Set
import String.Extra as Se
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
    , inviteeBox = ""
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
    , publicId = "Loading..."
    , rawData = "Loading..."
    , parsedData = []
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
                        [ postMsg "" [] code
                        , Task.perform Zone Time.here
                        , Http.get { url = "/getmyid/" ++ code, expect = Http.expectString MyPublicId }
                        , Http.get { url = "/loadmaster/" ++ code, expect = Http.expectString LoadedMaster }
                        ]
            )


type Msg
    = SearchBox String
    | LoadedMaster (Result Http.Error String)
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
    | SendInvite
    | InviteeBox String
    | InviteSent (Result Http.Error ())
    | MyPublicId (Result Http.Error String)


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


type PublicSign
    = String


type Message
    = Human (Set.Set String) (Set.Set String) (List HumanMsgPart)
    | Invite InviteLike
    | Uninvite InviteLike


type Id
    = Id String


type Tag
    = Tag String



{-
   The fields are
   1. recipients
   2. invitee
   3. author
   4. signature
-}


type InviteLike
    = InviteLike (Set.Set String) String String Signature


type alias Signature =
    String


type alias FileHash =
    String


type HumanMsgPart
    = Text (List Paragraph)
    | File FileHash


type Paragraph
    = Paragraph (List Line)


type Line
    = Line String


{-| The syntax for the messages file is like this:

messages {
human
to { JXQ3EM8ulCcl9YfLPCI1OqiESWccBrchHKi8lcyMRTo= }
tags { `the` `tags` `for my first message example` }
body {
\`The time now approached for Lady Russell's return; the
day was even fixed, and Anne, being engaged to join her as
soon as she was resettled, was looking forward to an early
removal to Kellynch, and beginning to think how her own
comfort was likely to be affected by it.

It would place here in the same village with Captain Went-
worth, within half a mile of him; they would have to fre-
quent the same church, and there must be intercourse
between the two families.

`file aGpub1w63ntWPeIDOi3SIwDSsLx5sOdzZIbZkcioon0=`

This was much against her; but, on
the other hand, he spent so much of his time at Upper-
cross, that in removing thence she might be considered
rather as leaving him behind, than as going towards him;
\`
}
invite
to { }
invitee ffzeJbaGlXZ-t10NWtuhUUdwi0rm5eqI030j9hOEjWk=
author hurxrCzrSyhOt6g66nno5QeCH71QeMWvpaPtoXuxcLI=
signature 21jFWq5NzZfa-vzb6hC8DcleS74W2jhvGWfR33Cv5UIaGpub1w63ntWPeIDOi3SIwDSsLx5sOdzZIbZkcioon0ffzeJbaGlXZ-t10NWtuhUUdwi0rm5eqI030j9hOEjWk=
}

-}
masterParser : P.Parser (List Message)
masterParser =
    listP messageP


isSpace : Char -> Bool
isSpace c =
    c == ' ' || c == '\n' || c == '\u{000D}'


oneSpaceP : P.Parser ()
oneSpaceP =
    P.oneOf <| L.map P.symbol [ " ", "\n", "\u{000D}" ]


spacesP : P.Parser ()
spacesP =
    oneSpaceP |. P.chompWhile isSpace


messageP : P.Parser Message
messageP =
    P.succeed identity
        |= P.oneOf [ humanP, inviteP, uninviteP ]
        |. P.oneOf [ P.end, spacesP ]


inviteP : P.Parser Message
inviteP =
    P.succeed Invite
        |. P.token "invite"
        |. spacesP
        |= inviteLikeP


uninviteP : P.Parser Message
uninviteP =
    P.succeed Invite
        |. P.token "uninvite"
        |. spacesP
        |= inviteLikeP


inviteLikeP : P.Parser InviteLike
inviteLikeP =
    P.succeed InviteLike
        |. P.token "to"
        |. spacesP
        |= toListP
        |. spacesP
        |. P.token "invitee"
        |. spacesP
        |= hashP
        |. spacesP
        |. P.token "author"
        |. spacesP
        |= hashP
        |. P.token "signature"
        |. spacesP
        |= sigP


humanP : P.Parser Message
humanP =
    P.succeed Human
        |. P.token "human"
        |. spacesP
        |. P.token "to"
        |. spacesP
        |= toListP
        |. spacesP
        |. P.token "tags"
        |. spacesP
        |= tagListP
        |. spacesP
        |. P.token "body"
        |. spacesP
        |= listP humanPartP


tagListP : P.Parser (Set.Set String)
tagListP =
    P.map Set.fromList <| listP tagP


toListP : P.Parser (Set.Set String)
toListP =
    P.map Set.fromList <| listP hashP


humanPartP : P.Parser HumanMsgPart
humanPartP =
    P.oneOf [ humanTextP, humanFileP ]


humanTextP : P.Parser HumanMsgPart
humanTextP =
    P.map Text <| listP paragraphP


humanFileP : P.Parser HumanMsgPart
humanFileP =
    P.succeed File
        |. P.token "file"
        |. spacesP
        |= hashP


paragraphP : P.Parser Paragraph
paragraphP =
    P.map Paragraph <| listP lineP


lineP : P.Parser Line
lineP =
    P.succeed Line
        |= P.getChompedString (P.chompWhile isTagChar)
        |. P.oneOf
            [ P.token "\n"
            , P.token "\u{000D}"
            , P.end
            ]


idP : P.Parser Id
idP =
    P.map Id hashP


tagP : P.Parser String
tagP =
    P.succeed identity
        |. P.symbol "`"
        |= P.getChompedString (P.chompWhile isTagChar)
        |. P.symbol "`"


isTagChar : Char -> Bool
isTagChar c =
    Set.member c tagSet


tagSet : Set.Set Char
tagSet =
    Set.fromList <| String.toList tagChars


tagChars : String
tagChars =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
        ++ "!\"£$%^&*()-_=+#~{[}];:'@<,>.?/\\| "


{-| Go Nacl adds 64 bytes to a signed message. So the signature is
a 32-byte hash plus the 64 byte overhead, which is 96 bytes.
In Go, base64.URLEncoding.EncodeToString(<some 96 byte slice>)
gives a string of length 128, with no padding (i.e. no "=" on the
end).
-}
sigP : P.Parser String
sigP =
    P.getChompedString (P.chompWhile isBase64Char)
        |> P.andThen checkSig


checkSig : String -> P.Parser String
checkSig sig =
    if String.length sig == 128 then
        P.succeed sig

    else
        P.problem "base64 signature has 128 characters and no padding"


hashP : P.Parser String
hashP =
    P.getChompedString (P.chompWhile isBase64Char)
        |> P.andThen checkBase64


checkBase64 : String -> P.Parser String
checkBase64 base64 =
    if String.length base64 == 43 then
        P.succeed (base64 ++ "=") |. P.token "="

    else
        P.problem "base64 hash has 43 characters followed by ="


base64Set : Set.Set Char
base64Set =
    Set.fromList <| String.toList base64String


base64String : String
base64String =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"


isBase64Char : Char -> Bool
isBase64Char c =
    Set.member c base64Set


listP : P.Parser a -> P.Parser (List a)
listP itemP =
    P.succeed identity
        |. P.symbol "{"
        |= P.loop [] (buildListP itemP)


buildListP : P.Parser a -> List a -> P.Parser (P.Step (List a) (List a))
buildListP itemP revItems =
    P.oneOf
        [ P.symbol "}"
            |> P.map (\_ -> P.Done (L.reverse revItems))
        , P.map (\item -> P.Loop (item :: revItems)) itemP
        , P.map (\_ -> P.Loop revItems) spacesP
        ]


type alias Model =
    { rawData : String
    , parsedData : List Message
    , searchStr : String
    , page : Page
    , inviteeBox : String
    , securityCode : Maybe String
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
    , publicId : String
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


encodeSendApp sendDrawer =
    Je.object
        [ ( "AppHash", Je.string sendDrawer.appHash )
        , ( "Recipient", Je.string sendDrawer.recipient )
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
        MyPublicId (Err _) ->
            ( model, Cmd.none )

        MyPublicId (Ok id) ->
            ( { model | publicId = id }, Cmd.none )

        LoadedMaster (Err _) ->
            ( model, Cmd.none )

        LoadedMaster (Ok newMaster) ->
            case P.run masterParser newMaster of
                Err _ ->
                    ( model, Cmd.none )

                Ok parsed ->
                    ( { model | parsedData = parsed }, Cmd.none )

        InviteeBox str ->
            ( { model | inviteeBox = str }, Cmd.none )

        InviteSent _ ->
            ( model, Cmd.none )

        SendInvite ->
            case model.securityCode of
                Nothing ->
                    ( model, Cmd.none )

                Just code ->
                    ( { model | inviteeBox = "" }
                    , Http.post
                        { url = "/invite/" ++ code ++ "/" ++ model.inviteeBox
                        , expect = Http.expectWhatever InviteSent
                        , body = Http.emptyBody
                        }
                    )

        OpenSendDrawer hash ->
            ( { model
                | sendDrawOpen =
                    Just
                        { recipient = ""
                        , appHash = hash
                        }
              }
            , Cmd.none
            )

        UpdateSendDrawer newDrawer ->
            ( { model | sendDrawOpen = Just newDrawer }, Cmd.none )

        SendApp ->
            case ( model.sendDrawOpen, model.securityCode ) of
                ( Just drawer, Just code ) ->
                    ( { model | sendDrawOpen = Nothing }
                    , postSendApp drawer code
                    )

                _ ->
                    ( model, Cmd.none )

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
    , body = [ E.layout [] (homePage model) ]
    }


placeholder : Ei.Placeholder Msg
placeholder =
    Ei.placeholder [] <| E.text "Type here to search"


black =
    E.rgb 0 0 0


searchStyle =
    [ E.width <| E.px 600 ]


edges =
    { top = 0, left = 0, right = 0, bottom = 0 }


idPadding =
    { top = 5
    , right = 20
    , bottom = 0
    , left = 0
    }


idtxt id =
    Se.break 11 id


myId id =
    E.row
        (E.spacing 20 :: idStyle)
    <|
        [ E.el [ Font.bold ] <| E.text "Public ID:"
        , E.text id
        ]


searchBoxStyle =
    [ Font.family [ Font.typeface "Source Code Pro", Font.monospace ]
    , Font.size 37
    , E.alignTop
    ]


topButtonStyle =
    [ E.alignLeft
    , E.alignTop
    , Font.size 28
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
    E.column [ E.spacing 20, E.alignTop ] [ searchBox txt ]


homeTopSection txt id =
    E.row [ E.width E.fill, E.spacing 20 ]
        [ topButtonsAndSearch Home txt
        , myId id
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


newDocTopSection tagText fileUpload id =
    E.column []
        [ myId id
        , topButtons NewDoc
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


membersTopSection id =
    E.row [ E.width E.fill, E.spacing 20 ]
        [ topButtons Members
        , myId id
        ]


unknownPage =
    E.el [] <| E.text "Page doesn't exist"


memberPage model =
    E.column
        [ E.width E.fill
        , E.padding 20
        , E.spacing 20
        ]
        [ myId model.publicId
        , topButtons Members
        , E.row []
            [ Ei.text []
                { onChange = InviteeBox
                , text = model.inviteeBox
                , placeholder =
                    Just <|
                        Ei.placeholder [] <|
                            E.text "Type ID of person to invite."
                , label = Ei.labelAbove [] E.none
                }
            , Ei.button []
                { onPress = Just SendInvite
                , label = E.text "Send invite"
                }
            ]
        ]


tagStyle color =
    [ Bg.color color
    , E.alignLeft
    , Font.size 28
    , Font.family [ Font.typeface "Source Code Pro", Font.monospace ]
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
    [ Font.family [ Font.typeface "Source Code Pro", Font.monospace ]
    , Font.size 37
    ]


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
        , E.padding 5
        , E.spacing 5
        ]
        [ myId model.publicId
        , searchBox model.searchStr
        , homeShowTags
            UnchooseTag
            (Set.toList model.selectedTags)
            blue
            (E.padding 0)
        , homeShowTags
            ChooseTag
            (Set.toList model.unselectedTags)
            paleBlue
            (E.padding 0)
        , txtEditor model.rawData
        ]


txtEditor : String -> E.Element Msg
txtEditor rawData =
    E.el idStyle <| E.text rawData


newDocPage model =
    E.column
        [ E.width E.fill
        , E.padding 20
        , E.spacing 20
        ]
        [ myId model.publicId
        , topButtons NewDoc
        , E.row [ E.spacing 20 ]
            [ E.el [] <|
                Ei.text
                    [ E.width <| E.px 600, E.height <| E.px 50 ]
                    { onChange = TagBox
                    , text = model.newTagsBox
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
                (case model.fileUpload of
                    Nothing ->
                        greyNewDocButtonStyle

                    Just _ ->
                        newDocButtonStyle
                )
                { onPress =
                    case model.fileUpload of
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

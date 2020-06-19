port module Main exposing (main)

import Base64.Decode as B64d
import Base64.Encode as B64e
import Browser
import Browser.Events
import Bytes
import Bytes.Encode as Be
import Dict
import Element as E
import Element.Background as Background
import Element.Font as Font
import Element.Input as Ei
import Html
import Json.Decode as Jd
import Json.Encode as Je


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type GetMe
    = KeysFromCacheG
    | GeneratedKeysG
    | PowInfoG MyKeys
    | PowG MyKeys
    | NameFromServerG MyKeys


type Page
    = AdminP AdminPage
    | MessagingP MessagingPage


type alias Blob =
    { id : Int
    , mime : String
    , filename : String
    , size : Int
    }


type AdminPage
    = PricingA
    | AccountA
    | AboutA
    | HelpA


type MessagingPage
    = WriteE Draft
    | ContactsE
    | InboxE
    | DraftsE
    | SentE
    | SendingE


type alias Draft =
    { id : Maybe Int
    , subject : String
    , to : Maybe Int
    , userInput : String
    , code : Maybe Code
    , blobs : List Blob
    }


type alias Code =
    { contents : Bytes.Bytes
    , mime : String
    , filename : String
    }


type AuthCode
    = AuthCode Bytes.Bytes


type alias Model =
    { myId : Maybe ( MyName, MyKeys )
    , processes : List Process
    , fatal : Maybe String
    , page : Page
    , windowWidth : Int
    , inboxSummary : Maybe (List InboxMessageSummary)
    , draftsSummary : Maybe (List DraftSummary)
    , sentSummary : Maybe (List SentSummary)
    , sendingSummary : Maybe (List SendingSummary)
    , contacts : Maybe (Dict.Dict Int Contact)
    }


type alias Contact =
    { name : String
    , keys : TheirKeys
    }


type alias TheirKeys =
    { signing : Bytes.Bytes
    , encryption : Bytes.Bytes
    }


type alias SendingSummary =
    { subject : String
    , to : Int
    , time : Int
    , id : Int
    }


type alias SentSummary =
    { subject : String
    , to : Int
    , sentTime : Int
    , receivedTime : Int
    , id : Int
    }


type alias DraftSummary =
    { subject : String
    , to : Int
    , time : Int
    , id : Int
    }


type alias InboxMessageSummary =
    { subject : String
    , from : String
    , time : Int
    , id : Int
    }


type alias MyKeys =
    { encryption : KeyPair
    , signing : KeyPair
    }


type alias KeyPair =
    { public : Bytes.Bytes
    , secret : Bytes.Bytes
    }


type MyName
    = MyName Int


type Process
    = GetMeP GetMe


initModel : Int -> Model
initModel windowWidth =
    { myId = Nothing
    , processes = [ GetMeP KeysFromCacheG ]
    , fatal = Nothing
    , page = MessagingP InboxE
    , windowWidth = windowWidth
    , inboxSummary = Nothing
    , draftsSummary = Nothing
    , sentSummary = Nothing
    , sendingSummary = Nothing
    , contacts = Nothing
    }


init : Int -> ( Model, Cmd Msg )
init windowWidth =
    ( initModel windowWidth, Cmd.none )


view : Model -> Html.Html Msg
view model =
    E.layout [ E.padding 5 ] <| viewE model


viewE : Model -> E.Element Msg
viewE model =
    E.column
        [ E.width E.fill
        , E.spacingXY 0 20
        ]
        [ title model.windowWidth
        , adminButtons model.windowWidth model.page
        , messagingButtons model.windowWidth model.page
        , messagingPage model
        ]


messagingPage : Model -> E.Element Msg
messagingPage model =
    case model.page of
        AdminP PricingA ->
            E.text "Pricing goes here"

        AdminP AccountA ->
            E.text "Account info goes here"

        AdminP AboutA ->
            E.text "About info goes here"

        AdminP HelpA ->
            E.text "Contact details for support go here"

        MessagingP (WriteE _) ->
            E.text "Write page goes here"

        MessagingP ContactsE ->
            E.text "Contacts page goes here"

        MessagingP InboxE ->
            E.text "Inbox page goes here"

        MessagingP DraftsE ->
            E.text "Drafts page goes here"

        MessagingP SentE ->
            E.text "Sent page goes here"

        MessagingP SendingE ->
            E.text "Sending page goes here"


title : Int -> E.Element Msg
title windowWidth =
    E.el
        [ E.centerX
        , Font.size <| titleSize windowWidth
        , Font.color blue
        , Font.family [ Font.typeface "Ubuntu" ]
        ]
    <|
        E.text "BigWebThing"


blue : E.Color
blue =
    E.rgb255 69 143 255


titleSize : Int -> Int
titleSize w =
    let
        min =
            39

        max =
            120

        div =
            w // 7
    in
    if div > max then
        max

    else if div < min then
        min

    else
        div


adminButtons : Int -> Page -> E.Element Msg
adminButtons windowWidth page =
    E.wrappedRow
        [ E.centerX
        , E.spacingXY 5 0
        ]
    <|
        List.map
            (adminButton windowWidth page)
            [ PricingA, AccountA, AboutA, HelpA ]


adminButton : Int -> Page -> AdminPage -> E.Element Msg
adminButton windowWidth page adminPage =
    Ei.button
        []
        { onPress = Just <| SimpleM <| PageClickS <| AdminP adminPage
        , label = adminButtonLabel windowWidth page adminPage
        }


adminButtonLabel : Int -> Page -> AdminPage -> E.Element Msg
adminButtonLabel windowWidth page adminPage =
    E.el
        (adminLabelStyle windowWidth page adminPage)
    <|
        E.text <|
            adminLabelText adminPage


adminLabelStyle : Int -> Page -> AdminPage -> List (E.Attribute Msg)
adminLabelStyle windowWidth page adminPage =
    [ E.centerX
    , Font.family [ Font.typeface "Ubuntu" ]
    , Font.size <| adminButtonFontSize windowWidth
    , E.paddingXY 8 16
    , Background.color <|
        if adminPageOn page adminPage then
            blue

        else
            E.rgb255 255 255 255
    ]


adminButtonFontSize : Int -> Int
adminButtonFontSize w =
    let
        min =
            18

        max =
            28

        div =
            w // 20
    in
    if div > max then
        max

    else if div < min then
        min

    else
        div


adminPageOn : Page -> AdminPage -> Bool
adminPageOn page adminPage =
    case ( page, adminPage ) of
        ( AdminP p1, p2 ) ->
            p1 == p2

        _ ->
            False


emptyDraft : Draft
emptyDraft =
    { id = Nothing
    , subject = ""
    , to = Nothing
    , userInput = ""
    , code = Nothing
    , blobs = []
    }


messagingButtons : Int -> Page -> E.Element Msg
messagingButtons windowWidth page =
    E.wrappedRow
        [ E.width E.fill
        , E.spacingXY 5 10
        ]
    <|
        List.map
            (messagingButton windowWidth page)
            [ InboxE
            , DraftsE
            , SentE
            , SendingE
            , WriteE emptyDraft
            , ContactsE
            ]


messagingButton : Int -> Page -> MessagingPage -> E.Element Msg
messagingButton windowWidth page subPage =
    Ei.button
        [ E.width <| E.minimum 150 E.fill
        , Background.color <|
            if messagingPageOn page subPage then
                blue

            else
                E.rgb 1 1 1
        ]
        { onPress =
            Just <| SimpleM <| PageClickS <| MessagingP subPage
        , label = messagingButtonLabel windowWidth page subPage
        }


messagingButtonLabel : Int -> Page -> MessagingPage -> E.Element Msg
messagingButtonLabel windowWidth page subPage =
    E.el
        (messagingLabelStyle windowWidth page subPage)
    <|
        E.text <|
            messagingLabelText subPage


messagingLabelText : MessagingPage -> String
messagingLabelText page =
    case page of
        WriteE _ ->
            "Write"

        ContactsE ->
            "Contacts"

        InboxE ->
            "Inbox"

        DraftsE ->
            "Drafts"

        SentE ->
            "Sent"

        SendingE ->
            "Sending"


messagingLabelStyle :
    Int
    -> Page
    -> MessagingPage
    -> List (E.Attribute Msg)
messagingLabelStyle windowWidth page subPage =
    [ E.centerX
    , Font.family [ Font.typeface "Ubuntu" ]
    , E.paddingXY 0 20
    , Font.size <| messagingButtonFontSize windowWidth
    , Background.color <|
        if messagingPageOn page subPage then
            blue

        else
            E.rgb255 255 255 255
    ]


messagingButtonFontSize : Int -> Int
messagingButtonFontSize w =
    let
        min =
            30

        max =
            45

        div =
            w // 13
    in
    if div > max then
        max

    else if div < min then
        min

    else
        div


messagingPageOn : Page -> MessagingPage -> Bool
messagingPageOn page subPage =
    case ( page, subPage ) of
        ( MessagingP m1, m2 ) ->
            m1 == m2

        _ ->
            False


adminLabelText : AdminPage -> String
adminLabelText adminPage =
    case adminPage of
        PricingA ->
            "Pricing"

        AccountA ->
            "Account"

        HelpA ->
            "Help"

        AboutA ->
            "About"


type Simple
    = PageClickS Page
    | NewWindowWidthS Int


type ForProcess
    = KeysFromCacheF MyKeys
    | GeneratedKeysF MyKeys
    | AuthCodeF AuthCode
    | PowInfoF PowInfo
    | PowF Pow
    | NameFromServerF MyName


port elmToJs : Je.Value -> Cmd msg


port jsToElm : (Je.Value -> msg) -> Sub msg


type Pow
    = Pow Bytes.Bytes


type alias PowInfo =
    { unique : Bytes.Bytes
    , difficulty : Int
    }


jsKeyVal : String -> Je.Value -> Je.Value
jsKeyVal key value =
    Je.object [ ( "key", Je.string key ), ( "value", value ) ]


cacheGet : String -> Je.Value
cacheGet key =
    jsKeyVal "cacheGet" <| Je.string key


encodeToJs : ElmToJs -> Je.Value
encodeToJs value =
    case value of
        WebsocketE bytes ->
            jsKeyVal
                "toWebsocket"
                (Je.string <| B64e.encode <| B64e.bytes bytes)

        GetPowE powInfo ->
            jsKeyVal "getPow" (encodePowInfo powInfo)

        GetInboxSummaryE ->
            cacheGet "inboxSummary"

        GetContactsE ->
            cacheGet "contacts"

        GetSendingSummaryE ->
            cacheGet "sendingSummary"

        GetDraftsSummaryE ->
            cacheGet "draftsSummary"

        GetSentSummaryE ->
            cacheGet "sentSummary"


encodePowInfo : PowInfo -> Je.Value
encodePowInfo { unique, difficulty } =
    Je.object
        [ ( "unique", Je.string <| B64e.encode <| B64e.bytes unique )
        , ( "difficulty", Je.int difficulty )
        ]


type ElmToJs
    = WebsocketE Bytes.Bytes
    | GetPowE PowInfo
    | GetInboxSummaryE
    | GetContactsE
    | GetSendingSummaryE
    | GetDraftsSummaryE
    | GetSentSummaryE


type JsToElm
    = MyKeysFromCacheJ MyKeys


type Msg
    = ForProcessM ForProcess
    | SimpleM Simple
    | JsonFromJsM Je.Value


{-| Each piece of data from Javascript is a piece of JSON with fields
'key' and 'value'.
-}
decodeFromJs : Je.Value -> Result String JsToElm
decodeFromJs json =
    case Jd.decodeValue fromJsDecoder json of
        Err err ->
            Err <| Jd.errorToString err

        Ok ok ->
            Ok ok


fromJsDecoder : Jd.Decoder JsToElm
fromJsDecoder =
    Jd.andThen fromJsDecoderHelp <| Jd.field "key" Jd.string


fromJsDecoderHelp : String -> Jd.Decoder JsToElm
fromJsDecoderHelp key =
    case key of
        "fromCache" ->
            fromCacheDecoder

        badKey ->
            Jd.fail <| "bad key: \"" ++ badKey ++ "\""


fromCacheDecoder : Jd.Decoder JsToElm
fromCacheDecoder =
    Jd.andThen fromCacheDecoderHelp <| Jd.field "key" Jd.string


fromCacheDecoderHelp : String -> Jd.Decoder JsToElm
fromCacheDecoderHelp key =
    case key of
        "myKeys" ->
            myKeysFromCacheDecoder

        badKey ->
            Jd.fail <| "bad cache response key \"" ++ badKey ++ "\""


myKeysFromCacheDecoder : Jd.Decoder JsToElm
myKeysFromCacheDecoder =
    Jd.map2 (\e s -> MyKeysFromCacheJ <| MyKeys e s)
        (Jd.field "encryption" encryptionKeysDecoder)
        (Jd.field "signing" signingKeysDecoder)


signingKeysDecoder : Jd.Decoder KeyPair
signingKeysDecoder =
    Jd.map2 KeyPair
        (Jd.field "secretKey" key64)
        (Jd.field "publicKey" key32)


key64 : Jd.Decoder Bytes.Bytes
key64 =
    Jd.andThen (bytesDecoder 64) Jd.string


key32 : Jd.Decoder Bytes.Bytes
key32 =
    Jd.andThen (bytesDecoder 32) Jd.string


encryptionKeysDecoder : Jd.Decoder KeyPair
encryptionKeysDecoder =
    Jd.map2 KeyPair
        (Jd.field "secretKey" key32)
        (Jd.field "publicKey" key32)


showB64Error : B64d.Error -> String
showB64Error error =
    case error of
        B64d.ValidationError ->
            "Validation error"

        B64d.InvalidByteSequence ->
            "Invalid byte sequence"


bytesDecoder : Int -> String -> Jd.Decoder Bytes.Bytes
bytesDecoder length raw =
    case B64d.decode B64d.bytes raw of
        Err err ->
            Jd.fail <| showB64Error err

        Ok bytes ->
            let
                bytesLen =
                    Bytes.width bytes
            in
            if bytesLen == length then
                Jd.succeed bytes

            else
                Jd.fail <|
                    "expecting "
                        ++ String.fromInt length
                        ++ " bytes, but got "
                        ++ String.fromInt bytesLen


toMsg : JsToElm -> Msg
toMsg j =
    case j of
        MyKeysFromCacheJ myKeys ->
            ForProcessM <| KeysFromCacheF myKeys


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JsonFromJsM json ->
            case decodeFromJs json of
                Err err ->
                    ( { model | fatal = Just err }, Cmd.none )

                Ok fromJs ->
                    update (toMsg fromJs) model

        ForProcessM forProcess ->
            router [] forProcess model

        SimpleM simple ->
            updateSimple simple model


updateSimple : Simple -> Model -> ( Model, Cmd Msg )
updateSimple msg model =
    case msg of
        PageClickS (MessagingP InboxE) ->
            ( { model | page = MessagingP InboxE }
            , case model.inboxSummary of
                Nothing ->
                    elmToJs <| encodeToJs GetInboxSummaryE

                Just _ ->
                    Cmd.none
            )

        PageClickS (MessagingP DraftsE) ->
            ( { model | page = MessagingP DraftsE }
            , case model.draftsSummary of
                Nothing ->
                    elmToJs <| encodeToJs GetDraftsSummaryE

                Just _ ->
                    Cmd.none
            )

        PageClickS (MessagingP SentE) ->
            ( { model | page = MessagingP SentE }
            , case model.sentSummary of
                Nothing ->
                    elmToJs <| encodeToJs GetSentSummaryE

                Just _ ->
                    Cmd.none
            )

        PageClickS (MessagingP SendingE) ->
            ( { model | page = MessagingP SendingE }
            , case model.sendingSummary of
                Nothing ->
                    elmToJs <| encodeToJs GetSendingSummaryE

                Just _ ->
                    Cmd.none
            )

        PageClickS (MessagingP (WriteE draft)) ->
            ( { model | page = MessagingP <| WriteE draft }
            , Cmd.none
            )

        PageClickS (MessagingP ContactsE) ->
            ( { model | page = MessagingP ContactsE }
            , case model.contacts of
                Nothing ->
                    elmToJs <| encodeToJs GetContactsE

                Just _ ->
                    Cmd.none
            )

        PageClickS (AdminP PricingA) ->
            ( { model | page = AdminP PricingA }
            , Cmd.none
            )

        PageClickS (AdminP AccountA) ->
            ( { model | page = AdminP AccountA }
            , Cmd.none
            )

        PageClickS (AdminP AboutA) ->
            ( { model | page = AdminP AboutA }
            , Cmd.none
            )

        PageClickS (AdminP HelpA) ->
            ( { model | page = AdminP HelpA }
            , Cmd.none
            )

        NewWindowWidthS width ->
            ( { model | windowWidth = width }, Cmd.none )


type ProcessTick
    = FinishedT Model (Cmd Msg)
    | ContinuingT Model (Cmd Msg) Process
    | NotUsedMessageT


updateGetMe : GetMe -> ForProcess -> Model -> ProcessTick
updateGetMe getMe msg model =
    case ( getMe, msg ) of
        ( KeysFromCacheG, KeysFromCacheF myKeys ) ->
            ContinuingT
                model
                getPowInfo
                (GetMeP <| PowInfoG myKeys)

        ( KeysFromCacheG, _ ) ->
            NotUsedMessageT

        ( GeneratedKeysG, GeneratedKeysF myKeys ) ->
            ContinuingT
                model
                getPowInfo
                (GetMeP <| PowInfoG myKeys)

        ( GeneratedKeysG, _ ) ->
            NotUsedMessageT

        ( PowInfoG myKeys, PowInfoF powInfo ) ->
            ContinuingT
                model
                (getPow powInfo)
                (GetMeP <| PowG myKeys)

        ( PowInfoG _, _ ) ->
            NotUsedMessageT

        ( PowG myKeys, PowF pow ) ->
            ContinuingT
                model
                (elmToJs <| makeNameRequest pow myKeys)
                (GetMeP <| NameFromServerG myKeys)

        ( PowG _, _ ) ->
            NotUsedMessageT

        ( NameFromServerG myKeys, NameFromServerF myName ) ->
            FinishedT
                { model | myId = Just ( myName, myKeys ) }
                Cmd.none

        ( NameFromServerG _, _ ) ->
            NotUsedMessageT


getPowInfo : Cmd Msg
getPowInfo =
    elmToJs <|
        encodeToJs <|
            WebsocketE <|
                Be.encode <|
                    Be.unsignedInt8 3


getPow : PowInfo -> Cmd Msg
getPow powInfo =
    elmToJs <| encodeToJs <| GetPowE powInfo


makeNameRequest : Pow -> MyKeys -> Je.Value
makeNameRequest (Pow pow) { encryption, signing } =
    encodeToJs <|
        WebsocketE <|
            Be.encode <|
                Be.sequence
                    [ Be.unsignedInt8 1
                    , Be.bytes pow
                    , Be.bytes signing.public
                    , Be.bytes encryption.public
                    ]


processTick : Process -> ForProcess -> Model -> ProcessTick
processTick p msg model =
    case p of
        GetMeP getMe ->
            updateGetMe getMe msg model


router : List Process -> ForProcess -> Model -> ( Model, Cmd Msg )
router notRelevant msg model =
    case model.processes of
        [] ->
            ( model, Cmd.none )

        p :: rocesses ->
            case processTick p msg model of
                FinishedT newModel cmd ->
                    ( { newModel
                        | processes = rocesses ++ notRelevant
                      }
                    , cmd
                    )

                ContinuingT newModel cmd newP ->
                    ( { newModel
                        | processes = newP :: rocesses ++ notRelevant
                      }
                    , cmd
                    )

                NotUsedMessageT ->
                    router (p :: notRelevant) msg model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ jsToElm JsonFromJsM
        , Browser.Events.onResize <|
            \w _ -> SimpleM <| NewWindowWidthS w
        ]

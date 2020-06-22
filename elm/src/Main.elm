port module Main exposing (main)

import Base64.Decode as B64d
import Base64.Encode as B64e
import Bitwise
import Browser
import Browser.Events
import Bytes
import Bytes.Decode as Bd
import Bytes.Encode as Be
import Dict
import Element as E
import Element.Background as Background
import Element.Font as Font
import Element.Input as Ei
import File.Download as Download
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
    { id : String
    , mime : String
    , filename : String
    , size : Int
    }


type AdminPage
    = PricingA
    | AccountA
    | AboutA
    | HelpA


type Wasm
    = SmallString String
    | Ordering (List Wasm)


wasmOutputBytesDecoder : Bd.Decoder Wasm
wasmOutputBytesDecoder =
    Bd.unsignedInt8
        |> Bd.andThen
            (\indicator ->
                case indicator of
                    0 ->
                        Bd.map Ordering <| list wasmOutputBytesDecoder

                    1 ->
                        Bd.map SmallString stringDecoder

                    _ ->
                        Bd.fail
            )


type MessagingButton
    = ContactsB
    | InboxB
    | DraftsB
    | SentB
    | WriteB


type AdminButton
    = PricingB
    | AccountB
    | AboutB
    | HelpB


type MessagingPage
    = ContactsE
    | WriteE ( Draft, Maybe Wasm )
    | InboxE (Maybe ( InboxMessage, Wasm ))
    | DraftsE
    | SentE (Maybe ( Sent, Wasm ))


type alias Sent =
    { id : Int
    , subject : String
    , to : String
    , userInput : String
    , code : Code
    , blobs : List Blob
    }


type alias InboxMessage =
    { subject : String
    , fromId : String
    , userInput : String
    , blobs : List Blob
    , timeSent : Int
    , timeReceived : Int
    , id : String
    , code : Code
    }


type alias Draft =
    { id : Maybe Int
    , subject : String
    , to : Maybe String
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
    , contacts : Maybe (Dict.Dict String Contact)
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
    , to : String
    , time : Int
    , id : Int
    }


type alias SentSummary =
    { subject : String
    , toId : String
    , sentTime : Int
    , receivedTime : Int
    , id : String
    }


type alias DraftSummary =
    { subject : String
    , toId : String
    , time : Int
    , id : String
    }


type alias InboxMessageSummary =
    { subject : String
    , fromId : String
    , time : Int
    , id : String
    }


type alias MyKeys =
    { encrypt : KeyPair
    , sign : KeyPair
    }


type alias KeyPair =
    { public : Bytes.Bytes
    , secret : Bytes.Bytes
    }


type MyName
    = MyName String


type Process
    = GetMeP GetMe
    | GetInboxMessageP GetInboxMessage
    | BlobForDownloadP Blob


type GetInboxMessage
    = FromCacheG String
    | WasmOutputG InboxMessage


initModel : Int -> Model
initModel windowWidth =
    { myId = Nothing
    , processes = [ GetMeP KeysFromCacheG ]
    , fatal = Nothing
    , page = MessagingP (InboxE Nothing)
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
        , mainPage model
        ]


mainPage : Model -> E.Element Msg
mainPage model =
    case model.page of
        AdminP PricingA ->
            E.text "Pricing goes here"

        AdminP AccountA ->
            E.text "Account info goes here"

        AdminP AboutA ->
            E.text "About info goes here"

        AdminP HelpA ->
            E.text "Contact details for support go here"

        MessagingP ContactsE ->
            E.text "Contacts page goes here"

        MessagingP (InboxE Nothing) ->
            case model.inboxSummary of
                Nothing ->
                    noMessages

                Just inboxSummary ->
                    inboxPage inboxSummary

        MessagingP (InboxE (Just message)) ->
            inboxMessageView message model.contacts

        MessagingP (WriteE _) ->
            E.text "Writer page goes here"

        MessagingP DraftsE ->
            E.text "Drafts page goes here"

        MessagingP (SentE _) ->
            E.text "Sent page goes here"


inboxMessageView :
    ( InboxMessage, Wasm )
    -> Maybe (Dict.Dict String Contact)
    -> E.Element Msg
inboxMessageView ( msg, wasm ) contacts =
    E.column []
        [ prettyTime msg.timeReceived
        , E.text msg.fromId
        , wasmView wasm
        , userInputView msg.userInput
        , blobsView msg.blobs
        , codeView msg.code
        ]


userInputView : String -> E.Element Msg
userInputView userInput =
    E.el [ Font.family [ Font.typeface "Ubuntu Mono" ] ] <|
        E.text userInput


blobsView : List Blob -> E.Element Msg
blobsView blobs =
    E.column [ E.spacing 10 ] <| List.map blobView blobs


blobView : Blob -> E.Element Msg
blobView blob =
    E.row [ E.spacing 10 ]
        [ E.text blob.filename
        , E.text blob.mime
        , E.text <| prettySize blob.size
        , Ei.button []
            { onPress = Just <| DownloadBlobM blob
            , label = E.text "Download"
            }
        ]


wasmView : Wasm -> E.Element Msg
wasmView wasm =
    case wasm of
        SmallString s ->
            showSmallString s

        Ordering os ->
            E.column []
                (List.map wasmView os)


showSmallString : String -> E.Element Msg
showSmallString s =
    E.el [ Font.family [ Font.typeface "Ubuntu Mono" ] ] <|
        E.text s


codeView : Code -> E.Element Msg
codeView code =
    E.row [ E.spacing 10 ]
        [ E.text code.filename
        , E.text <| prettySize <| Bytes.width code.contents
        , Ei.button []
            { onPress = Just <| DownloadCodeM code
            , label = E.text "Download"
            }
        ]


prettySize : Int -> String
prettySize s =
    if s < 1000 then
        String.fromInt s ++ " B"

    else if s < 1000000 then
        String.fromInt (s // 1000) ++ " KB"

    else if s < 1000000000 then
        String.fromInt (s // 1000000) ++ " MB"

    else
        "Bad size"


noMessages : E.Element Msg
noMessages =
    E.text "No messages"


inboxPage : List InboxMessageSummary -> E.Element Msg
inboxPage summary =
    E.column
        []
        (List.map inboxMenuItem summary)


inboxMenuItem : InboxMessageSummary -> E.Element Msg
inboxMenuItem { subject, fromId, time, id } =
    Ei.button
        []
        { onPress = Just <| InboxMenuClickM id
        , label =
            E.row
                [ E.spacing 10
                ]
                [ prettyTime time
                , E.text fromId
                , E.text subject
                ]
        }


prettyTime : Int -> E.Element Msg
prettyTime =
    E.text << String.fromInt


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
            [ PricingB, AccountB, AboutB, HelpB ]


adminButton : Int -> Page -> AdminButton -> E.Element Msg
adminButton windowWidth page button =
    Ei.button
        []
        { onPress = Just <| adminButtonMsg button
        , label = adminButtonLabel windowWidth page button
        }


adminButtonMsg : AdminButton -> Msg
adminButtonMsg button =
    case button of
        PricingB ->
            PricingM

        AccountB ->
            AccountM

        AboutB ->
            AboutM

        HelpB ->
            HelpM


adminButtonLabel : Int -> Page -> AdminButton -> E.Element Msg
adminButtonLabel windowWidth page button =
    E.el
        (adminLabelStyle windowWidth page button)
    <|
        E.text <|
            adminLabelText button


adminLabelStyle : Int -> Page -> AdminButton -> List (E.Attribute Msg)
adminLabelStyle windowWidth page button =
    [ E.centerX
    , Font.family [ Font.typeface "Ubuntu" ]
    , Font.size <| adminButtonFontSize windowWidth
    , E.paddingXY 8 16
    , Background.color <|
        if adminPageOn page button then
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


adminPageOn : Page -> AdminButton -> Bool
adminPageOn page button =
    case ( page, button ) of
        ( AdminP PricingA, PricingB ) ->
            True

        ( AdminP PricingA, _ ) ->
            False

        ( AdminP AccountA, AccountB ) ->
            True

        ( AdminP AccountA, _ ) ->
            False

        ( AdminP AboutA, AboutB ) ->
            True

        ( AdminP AboutA, _ ) ->
            False

        ( AdminP HelpA, HelpB ) ->
            True

        ( AdminP HelpA, _ ) ->
            False

        ( MessagingP _, _ ) ->
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
            [ InboxB, WriteB, DraftsB, SentB, ContactsB ]


messagingButton : Int -> Page -> MessagingButton -> E.Element Msg
messagingButton windowWidth page button =
    Ei.button
        [ E.width <| E.minimum 150 E.fill
        , Background.color <|
            if messagingPageOn page button then
                blue

            else
                E.rgb 1 1 1
        ]
        { onPress =
            Just <| messagingButtonMsg button
        , label = messagingButtonLabel windowWidth page button
        }


messagingButtonMsg : MessagingButton -> Msg
messagingButtonMsg button =
    case button of
        ContactsB ->
            ContactsM

        InboxB ->
            InboxM

        DraftsB ->
            DraftsM

        SentB ->
            SentM

        WriteB ->
            WriteM


messagingButtonLabel : Int -> Page -> MessagingButton -> E.Element Msg
messagingButtonLabel windowWidth page button =
    E.el
        (messagingLabelStyle windowWidth page button)
    <|
        E.text <|
            messagingLabelText button


messagingLabelText : MessagingButton -> String
messagingLabelText button =
    case button of
        WriteB ->
            "Write"

        ContactsB ->
            "Contacts"

        InboxB ->
            "Inbox"

        DraftsB ->
            "Drafts"

        SentB ->
            "Sent"


messagingLabelStyle :
    Int
    -> Page
    -> MessagingButton
    -> List (E.Attribute Msg)
messagingLabelStyle windowWidth page button =
    [ E.centerX
    , Font.family [ Font.typeface "Ubuntu" ]
    , E.paddingXY 0 20
    , Font.size <| messagingButtonFontSize windowWidth
    , Background.color <|
        if messagingPageOn page button then
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


messagingPageOn : Page -> MessagingButton -> Bool
messagingPageOn page msg =
    case ( page, msg ) of
        ( MessagingP ContactsE, ContactsB ) ->
            True

        ( MessagingP ContactsE, _ ) ->
            False

        ( MessagingP (InboxE _), InboxB ) ->
            True

        ( MessagingP (InboxE _), _ ) ->
            False

        ( MessagingP DraftsE, DraftsB ) ->
            True

        ( MessagingP DraftsE, _ ) ->
            False

        ( MessagingP (SentE _), SentB ) ->
            True

        ( MessagingP (SentE _), _ ) ->
            False

        ( MessagingP (WriteE _), WriteB ) ->
            True

        ( MessagingP (WriteE _), _ ) ->
            False

        ( AdminP _, _ ) ->
            False


adminLabelText : AdminButton -> String
adminLabelText button =
    case button of
        PricingB ->
            "Pricing"

        AccountB ->
            "Account"

        HelpB ->
            "Help"

        AboutB ->
            "About"


port elmToJs : Je.Value -> Cmd msg


port jsToElm : (Je.Value -> msg) -> Sub msg


type Pow
    = Pow Bytes.Bytes


type alias PowInfo =
    { difficulty : Int
    , unique : Bytes.Bytes
    }


jsKeyVal : String -> Je.Value -> Je.Value
jsKeyVal key value =
    Je.object [ ( "key", Je.string key ), ( "value", value ) ]


encodeToJs : ElmToJs -> Je.Value
encodeToJs value =
    case value of
        WebsocketE bytes ->
            jsKeyVal
                "toWebsocket"
                (Je.string <| B64e.encode <| B64e.bytes bytes)

        CacheGetE key ->
            jsKeyVal "cacheGet" <| Je.string key

        CacheSetE key bytes ->
            jsKeyVal
                "cacheSet"
            <|
                Je.string <|
                    B64e.encode <|
                        B64e.bytes bytes

        GetPowE powInfo ->
            jsKeyVal "getPow" <| encodePowInfo powInfo

        RunWasmE { userInput, wasmCode, msgId } ->
            jsKeyVal "runWasm" <|
                Je.object
                    [ ( "userInput"
                      , Je.string userInput
                      )
                    , ( "wasmCode"
                      , Je.string <|
                            B64e.encode <|
                                B64e.bytes wasmCode
                      )
                    , ( "msgId", Je.string msgId )
                    ]


encodePowInfo : PowInfo -> Je.Value
encodePowInfo { unique, difficulty } =
    Je.object
        [ ( "unique", Je.string <| B64e.encode <| B64e.bytes unique )
        , ( "difficulty", Je.int difficulty )
        ]


type ElmToJs
    = WebsocketE Bytes.Bytes
    | GetPowE PowInfo
    | CacheGetE String
    | CacheSetE String Bytes.Bytes
    | RunWasmE
        { userInput : String
        , wasmCode : Bytes.Bytes
        , msgId : String
        }


type Msg
    = FromCacheM String Bytes.Bytes
    | GeneratedKeysM MyKeys
    | PowM Pow
    | WebsocketB64M String
    | WasmOutputM String Wasm
    | JsonFromJsM Je.Value
    | PricingM
    | AccountM
    | AboutM
    | HelpM
    | InboxM
    | DraftsM
    | SentM
    | ContactsM
    | WriteM
    | NewWindowWidthM Int
    | InboxMenuClickM String
    | DownloadCodeM Code
    | DownloadBlobM Blob
    | WebsocketM FromWebsocket


fromJsDecoder : Jd.Decoder Msg
fromJsDecoder =
    Jd.andThen fromJsDecoderHelp <| Jd.field "key" Jd.string


fromJsDecoderHelp : String -> Jd.Decoder Msg
fromJsDecoderHelp key =
    case key of
        "fromCache" ->
            fromCacheDecoder

        "wasmOutput" ->
            wasmOutputDecoder

        "websocket" ->
            Jd.map WebsocketB64M Jd.string

        "pow" ->
            powDecoder

        badKey ->
            Jd.fail <| "bad key: \"" ++ badKey ++ "\""


powDecoder : Jd.Decoder Msg
powDecoder =
    Jd.string
        |> Jd.andThen
            (\b64 ->
                case B64d.decode B64d.bytes b64 of
                    Err err ->
                        Jd.fail <| showB64Error err

                    Ok bytes ->
                        Jd.succeed <| PowM <| Pow bytes
            )


wasmOutputDecoder : Jd.Decoder Msg
wasmOutputDecoder =
    Jd.map2 (\id wasmB64 -> ( id, wasmB64 ))
        (Jd.field "id" Jd.string)
        (Jd.field "wasm" Jd.string)
        |> Jd.andThen
            (\( id, wasmB64 ) ->
                case B64d.decode B64d.bytes wasmB64 of
                    Err err ->
                        Jd.fail <| showB64Error err

                    Ok bytes ->
                        case Bd.decode wasmOutputBytesDecoder bytes of
                            Just wasm ->
                                Jd.succeed <| WasmOutputM id wasm

                            Nothing ->
                                Jd.fail "could not decode WASM output bytes"
            )


fromCacheDecoder : Jd.Decoder Msg
fromCacheDecoder =
    Jd.map2 (\key value -> ( key, value ))
        (Jd.field "key" Jd.string)
        (Jd.field "value" Jd.string)
        |> Jd.andThen
            (\( key, value ) ->
                case B64d.decode B64d.bytes value of
                    Err err ->
                        Jd.fail <| showB64Error err

                    Ok bytes ->
                        Jd.succeed <| FromCacheM key bytes
            )


myKeysDecoder : Bd.Decoder MyKeys
myKeysDecoder =
    Bd.map2 MyKeys
        encryptKeysDecoder
        signKeysDecoder


encryptKeysDecoder : Bd.Decoder KeyPair
encryptKeysDecoder =
    Bd.map2 KeyPair (Bd.bytes 32) (Bd.bytes 32)


signKeysDecoder : Bd.Decoder KeyPair
signKeysDecoder =
    Bd.map2 KeyPair (Bd.bytes 32) (Bd.bytes 64)


showB64Error : B64d.Error -> String
showB64Error error =
    case error of
        B64d.ValidationError ->
            "Validation error"

        B64d.InvalidByteSequence ->
            "Invalid byte sequence"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    updateHelp [] msg model


clickCmd : Maybe a -> Cmd Msg -> Cmd Msg
clickCmd summary cmd =
    case summary of
        Nothing ->
            cmd

        Just _ ->
            Cmd.none


updateSimple : Msg -> Model -> ( Model, Cmd Msg )
updateSimple msg model =
    case msg of
        InboxM ->
            ( { model | page = MessagingP <| InboxE Nothing }
            , clickCmd model.inboxSummary <| cacheGet "inboxSummary"
            )

        DraftsM ->
            ( { model | page = MessagingP DraftsE }
            , clickCmd model.draftsSummary <|
                cacheGet "draftsSummary"
            )

        SentM ->
            ( { model | page = MessagingP <| SentE Nothing }
            , clickCmd model.sentSummary <| cacheGet "sentSummary"
            )

        WriteM ->
            ( { model | page = MessagingP <| WriteE ( emptyDraft, Nothing ) }
            , Cmd.none
            )

        ContactsM ->
            ( { model | page = MessagingP ContactsE }
            , case model.contacts of
                Nothing ->
                    cacheGet "contacts"

                Just _ ->
                    Cmd.none
            )

        PricingM ->
            ( { model | page = AdminP PricingA }
            , Cmd.none
            )

        AccountM ->
            ( { model | page = AdminP AccountA }
            , Cmd.none
            )

        AboutM ->
            ( { model | page = AdminP AboutA }
            , Cmd.none
            )

        HelpM ->
            ( { model | page = AdminP HelpA }
            , Cmd.none
            )

        NewWindowWidthM width ->
            ( { model | windowWidth = width }, Cmd.none )

        InboxMenuClickM id ->
            ( { model
                | processes =
                    GetInboxMessageP (FromCacheG id)
                        :: model.processes
              }
            , cacheGet id
            )

        DownloadCodeM { contents, mime, filename } ->
            ( model, Download.bytes filename mime contents )

        DownloadBlobM blob ->
            ( { model
                | processes =
                    BlobForDownloadP blob :: model.processes
              }
            , cacheGet blob.id
            )

        FromCacheM _ _ ->
            ( model, Cmd.none )

        GeneratedKeysM _ ->
            ( model, Cmd.none )

        WasmOutputM _ _ ->
            ( model, Cmd.none )

        JsonFromJsM json ->
            case Jd.decodeValue fromJsDecoder json of
                Err err ->
                    ( { model | fatal = Just <| Jd.errorToString err }
                    , Cmd.none
                    )

                Ok fromJsMsg ->
                    update fromJsMsg model

        PowM _ ->
            ( model, Cmd.none )

        WebsocketB64M b64 ->
            case B64d.decode B64d.bytes b64 of
                Err err ->
                    ( { model | fatal = Just <| showB64Error err }
                    , Cmd.none
                    )

                Ok bytes ->
                    case Bd.decode fromWebsocketDecoder bytes of
                        Nothing ->
                            ( { model
                                | fatal = Just "bad bytes from server"
                              }
                            , Cmd.none
                            )

                        Just fromWebsocket ->
                            update (WebsocketM fromWebsocket) model

        WebsocketM (MyNameW _) ->
            ( model, Cmd.none )

        WebsocketM (KeysForNameW _ _) ->
            ( model, Cmd.none )

        WebsocketM (PowInfoW _) ->
            ( model, Cmd.none )

        WebsocketM (AuthCodeW _) ->
            ( model, Cmd.none )


cacheMyName : String -> Cmd Msg
cacheMyName =
    elmToJs
        << encodeToJs
        << CacheSetE "myName"
        << Be.encode
        << stringEncoder


stringEncoder : String -> Be.Encoder
stringEncoder s =
    Be.sequence
        [ Be.unsignedInt32 Bytes.LE <| Be.getStringWidth s
        , Be.string s
        ]


cacheMyKeys : MyKeys -> Cmd Msg
cacheMyKeys =
    elmToJs
        << encodeToJs
        << CacheSetE "myKeys"
        << Be.encode
        << myKeysEncoder


myKeysEncoder : MyKeys -> Be.Encoder
myKeysEncoder { encrypt, sign } =
    Be.sequence <|
        List.map Be.bytes
            [ encrypt.public
            , encrypt.secret
            , sign.public
            , sign.secret
            ]


type FromWebsocket
    = MyNameW String
    | KeysForNameW String TheirKeys
    | PowInfoW PowInfo
    | AuthCodeW Bytes.Bytes


fromWebsocketDecoder : Bd.Decoder FromWebsocket
fromWebsocketDecoder =
    Bd.unsignedInt8
        |> Bd.andThen
            (\indicator ->
                case indicator of
                    1 ->
                        Bd.map MyNameW stringDecoder

                    2 ->
                        keysForNameDecoder

                    3 ->
                        Bd.map PowInfoW powInfoDecoder

                    4 ->
                        Bd.map AuthCodeW <| Bd.bytes 8

                    _ ->
                        Bd.fail
            )


keysForNameDecoder : Bd.Decoder FromWebsocket
keysForNameDecoder =
    Bd.map2 KeysForNameW
        stringDecoder
        theirKeysDecoder


theirKeysDecoder : Bd.Decoder TheirKeys
theirKeysDecoder =
    Bd.map2 TheirKeys
        (Bd.bytes 32)
        (Bd.bytes 32)


powInfoDecoder : Bd.Decoder PowInfo
powInfoDecoder =
    Bd.map2 PowInfo Bd.unsignedInt8 (Bd.bytes 8)


cacheGet : String -> Cmd Msg
cacheGet =
    elmToJs << encodeToJs << CacheGetE


type ProcessTick
    = FinishedT Model (Cmd Msg)
    | ContinuingT Model (Cmd Msg) Process
    | NotUsedMessageT


updateGetMe : GetMe -> Msg -> Model -> ProcessTick
updateGetMe getMe msg model =
    case ( getMe, msg ) of
        ( KeysFromCacheG, FromCacheM "myKeys" myKeysBytes ) ->
            case Bd.decode myKeysDecoder myKeysBytes of
                Nothing ->
                    FinishedT
                        { model
                            | fatal = Just "bad bytes in \"myKeys\""
                        }
                        Cmd.none

                Just myKeys ->
                    ContinuingT
                        model
                        getPowInfo
                        (GetMeP <| PowInfoG myKeys)

        ( KeysFromCacheG, _ ) ->
            NotUsedMessageT

        ( GeneratedKeysG, GeneratedKeysM myKeys ) ->
            ContinuingT
                model
                (Cmd.batch [ getPowInfo, cacheMyKeys myKeys ])
                (GetMeP <| PowInfoG myKeys)

        ( GeneratedKeysG, _ ) ->
            NotUsedMessageT

        ( PowInfoG myKeys, WebsocketM (PowInfoW powInfo) ) ->
            ContinuingT
                model
                (getPow powInfo)
                (GetMeP <| PowG myKeys)

        ( PowInfoG _, _ ) ->
            NotUsedMessageT

        ( PowG myKeys, PowM pow ) ->
            ContinuingT
                model
                (elmToJs <| makeNameRequest pow myKeys)
                (GetMeP <| NameFromServerG myKeys)

        ( PowG _, _ ) ->
            NotUsedMessageT

        ( NameFromServerG myKeys, WebsocketM (MyNameW myName) ) ->
            FinishedT
                { model | myId = Just ( MyName myName, myKeys ) }
                (cacheMyName myName)

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
makeNameRequest (Pow pow) { encrypt, sign } =
    encodeToJs <|
        WebsocketE <|
            Be.encode <|
                Be.sequence
                    [ Be.unsignedInt8 1
                    , Be.bytes pow
                    , Be.bytes sign.public
                    , Be.bytes encrypt.public
                    ]


processTick : Process -> Msg -> Model -> ProcessTick
processTick p msg model =
    case p of
        GetMeP getMe ->
            updateGetMe getMe msg model

        GetInboxMessageP getInbox ->
            updateGetInboxMsg getInbox msg model

        BlobForDownloadP blob ->
            case msg of
                FromCacheM id blobBytes ->
                    if blob.id == id then
                        FinishedT
                            model
                            (Download.bytes
                                blob.filename
                                blob.mime
                                blobBytes
                            )

                    else
                        NotUsedMessageT

                _ ->
                    NotUsedMessageT


runWasm : InboxMessage -> Cmd Msg
runWasm msg =
    elmToJs <|
        encodeToJs <|
            RunWasmE
                { userInput = msg.userInput
                , wasmCode = msg.code.contents
                , msgId = msg.id
                }


inboxMsgDecoder : Bd.Decoder InboxMessage
inboxMsgDecoder =
    map8 InboxMessage
        stringDecoder
        stringDecoder
        stringDecoder
        (list blobDecoder)
        int64Decoder
        int64Decoder
        stringDecoder
        codeDecoder


int64Decoder : Bd.Decoder Int
int64Decoder =
    Bd.map2 (\a b -> Bitwise.shiftLeftBy 32 b + a)
        (Bd.unsignedInt32 Bytes.LE)
        (Bd.unsignedInt32 Bytes.LE)


codeDecoder : Bd.Decoder Code
codeDecoder =
    Bd.map3 Code
        bytesDecoder
        stringDecoder
        stringDecoder


blobDecoder : Bd.Decoder Blob
blobDecoder =
    Bd.map4 Blob
        stringDecoder
        stringDecoder
        stringDecoder
        (Bd.unsignedInt32 Bytes.LE)


list : Bd.Decoder a -> Bd.Decoder (List a)
list decoder =
    Bd.unsignedInt32 Bytes.LE
        |> Bd.andThen
            (\len -> Bd.loop ( len, [] ) (listStep decoder))


listStep :
    Bd.Decoder a
    -> ( Int, List a )
    -> Bd.Decoder (Bd.Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        Bd.succeed <| Bd.Done xs

    else
        Bd.map (\x -> Bd.Loop ( n - 1, x :: xs )) decoder


stringDecoder : Bd.Decoder String
stringDecoder =
    Bd.unsignedInt32 Bytes.LE |> Bd.andThen Bd.string


bytesDecoder : Bd.Decoder Bytes.Bytes
bytesDecoder =
    Bd.unsignedInt32 Bytes.LE |> Bd.andThen Bd.bytes


map8 :
    (a -> b -> c -> d -> e -> f -> g -> h -> result)
    -> Bd.Decoder a
    -> Bd.Decoder b
    -> Bd.Decoder c
    -> Bd.Decoder d
    -> Bd.Decoder e
    -> Bd.Decoder f
    -> Bd.Decoder g
    -> Bd.Decoder h
    -> Bd.Decoder result
map8 func decA decB decC decD decE decF decG decH =
    Bd.map func decA
        |> Bd.andThen (dmap decB)
        |> Bd.andThen (dmap decC)
        |> Bd.andThen (dmap decD)
        |> Bd.andThen (dmap decE)
        |> Bd.andThen (dmap decF)
        |> Bd.andThen (dmap decG)
        |> Bd.andThen (dmap decH)


dmap : Bd.Decoder a -> (a -> b) -> Bd.Decoder b
dmap a b =
    Bd.map b a


updateGetInboxMsg :
    GetInboxMessage
    -> Msg
    -> Model
    -> ProcessTick
updateGetInboxMsg getting msg model =
    case ( getting, msg ) of
        ( FromCacheG idWant, FromCacheM idGot inboxMsgBytes ) ->
            if idWant == idGot then
                case Bd.decode inboxMsgDecoder inboxMsgBytes of
                    Nothing ->
                        FinishedT
                            { model | fatal = Just "bad bytes in inbox message from cache" }
                            Cmd.none

                    Just inboxMsg ->
                        ContinuingT
                            model
                            (runWasm inboxMsg)
                            (GetInboxMessageP <| WasmOutputG inboxMsg)

            else
                NotUsedMessageT

        ( FromCacheG _, _ ) ->
            NotUsedMessageT

        ( WasmOutputG msg1, WasmOutputM msgId wasm ) ->
            if msg1.id == msgId then
                FinishedT
                    { model
                        | page =
                            MessagingP <|
                                InboxE <|
                                    Just ( msg1, wasm )
                    }
                    Cmd.none

            else
                NotUsedMessageT

        ( WasmOutputG _, _ ) ->
            NotUsedMessageT


updateHelp :
    List Process
    -> Msg
    -> Model
    -> ( Model, Cmd Msg )
updateHelp notRelevant msg model =
    case model.processes of
        [] ->
            updateSimple msg model

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
                    updateHelp
                        (p :: notRelevant)
                        msg
                        { model | processes = rocesses }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ jsToElm JsonFromJsM
        , Browser.Events.onResize <|
            \w _ -> NewWindowWidthM w
        ]

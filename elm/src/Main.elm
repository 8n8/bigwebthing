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
import File
import File.Download as Download
import File.Select as Select
import Html
import Json.Decode as Jd
import Json.Encode as Je
import Task
import Time


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type GetMe
    = KeysFromJsG
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
    { id : String
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
    { id : String
    , subject : String
    , to : String
    , time : Int
    , userInput : String
    , code : Maybe Code
    , blobs : List Blob
    }


type alias Code =
    { contents : Bytes.Bytes
    , mime : String
    , filename : String
    }


type alias Model =
    { myId : Maybe ( MyName, MyKeys )
    , nonFatal : Maybe String
    , processes : List Process
    , fatal : Maybe String
    , page : Page
    , windowWidth : Int
    , inboxSummary : Maybe (List InboxMessageSummary)
    , draftsSummary : Maybe (List DraftSummary)
    , sentSummary : Maybe (List SentSummary)
    , sendingSummary : Maybe (List SendingSummary)
    , contacts : Maybe (Dict.Dict String Contact)
    , iota : Maybe Int
    , lastWasmRun : Maybe String
    }


type alias Contact =
    { name : String
    , keys : TheirKeys
    }


contactsDecoder : Bd.Decoder (Dict.Dict String Contact)
contactsDecoder =
    Bd.map Dict.fromList <| list contactDecoderHelp


contactDecoderHelp : Bd.Decoder ( String, Contact )
contactDecoderHelp =
    Bd.map2 (\a b -> ( a, b ))
        stringDecoder
        contactDecoder


contactDecoder : Bd.Decoder Contact
contactDecoder =
    Bd.map2 Contact
        stringDecoder
        theirKeysDecoder


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


sentSummaryDecoder : Bd.Decoder SentSummary
sentSummaryDecoder =
    Bd.map5 SentSummary
        stringDecoder
        stringDecoder
        int64Decoder
        int64Decoder
        stringDecoder


type alias DraftSummary =
    { subject : String
    , to : String
    , time : Int
    , id : String
    }


draftSummaryDecoder : Bd.Decoder DraftSummary
draftSummaryDecoder =
    Bd.map4 DraftSummary
        stringDecoder
        stringDecoder
        int64Decoder
        stringDecoder


type alias InboxMessageSummary =
    { subject : String
    , fromId : String
    , time : Int
    , id : String
    }


inboxMessageSummaryDecoder : Bd.Decoder InboxMessageSummary
inboxMessageSummaryDecoder =
    Bd.map4 InboxMessageSummary
        stringDecoder
        stringDecoder
        int64Decoder
        stringDecoder


type alias MyKeys =
    { encrypt : Bytes.Bytes
    , sign : EcdsaKey
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
    , processes = [ GetMeP KeysFromJsG ]
    , fatal = Nothing
    , page = MessagingP (InboxE Nothing)
    , windowWidth = windowWidth
    , nonFatal = Nothing
    , inboxSummary = Nothing
    , draftsSummary = Nothing
    , sentSummary = Nothing
    , sendingSummary = Nothing
    , contacts = Nothing
    , iota = Nothing
    , lastWasmRun = Nothing
    }


init : Int -> ( Model, Cmd Msg )
init windowWidth =
    ( initModel windowWidth, initCmd )


initCmd : Cmd Msg
initCmd =
    cacheGet "iota"


view : Model -> Html.Html Msg
view model =
    E.layout [ E.padding 20 ] <| viewE model


viewE : Model -> E.Element Msg
viewE model =
    case model.fatal of
        Just err ->
            E.text err

        Nothing ->
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

        MessagingP (WriteE w) ->
            writerView w

        MessagingP DraftsE ->
            E.text "Drafts page goes here"

        MessagingP (SentE _) ->
            E.text "Sent page goes here"


writerView : ( Draft, Maybe Wasm ) -> E.Element Msg
writerView ( draft, maybeWasm ) =
    E.column [ E.spacing 30, E.paddingXY 0 20, E.width E.fill ]
        [ toBox draft.to draft.id
        , subjectBox draft.subject draft.id
        , userInputBox draft.userInput draft.id
        , case maybeWasm of
            Nothing ->
                E.none

            Just wasm ->
                wasmView wasm
        , editCode draft.code draft.id
        , editBlobs draft.blobs draft.id
        ]


normalTextSize : Int
normalTextSize =
    25


toBox : String -> String -> E.Element Msg
toBox to draftId =
    Ei.text
        [ Font.size normalTextSize
        , ubuntuMono
        , E.width <| E.maximum 400 <| E.fill
        ]
        { onChange =
            \newTo ->
                NewToM { draftId = draftId, to = newTo }
        , text = to
        , placeholder = Nothing
        , label =
            Ei.labelLeft
                [ Font.size normalTextSize
                , ubuntu
                , E.centerY
                , E.paddingEach { left = 0, right = 7, bottom = 0, top = 0 }
                ]
            <|
                E.text "To"
        }


updateDraft : Draft -> Maybe Wasm -> Model -> ( Model, Cmd Msg )
updateDraft newDraft maybeWasm model =
    let
        newDraftsSummary =
            updateDraftsSummary newDraft model.draftsSummary
    in
    ( { model
        | page = MessagingP (WriteE ( newDraft, maybeWasm ))
        , draftsSummary = newDraftsSummary
      }
    , Cmd.batch
        [ cacheDraft newDraft
        , case newDraftsSummary of
            Nothing ->
                Cmd.none

            Just summary ->
                cacheDraftsSummary summary
        ]
    )


subjectBox : String -> String -> E.Element Msg
subjectBox subject draftId =
    Ei.text
        [ Font.size normalTextSize
        , ubuntuMono
        , E.width <| E.maximum 900 <| E.fill
        ]
        { onChange =
            \s -> NewSubjectM { draftId = draftId, subject = s }
        , text = subject
        , placeholder = Nothing
        , label =
            Ei.labelLeft
                [ ubuntu
                , E.centerY
                , Font.size normalTextSize
                , E.paddingEach { left = 0, right = 7, bottom = 0, top = 0 }
                ]
            <|
                E.text "Subject"
        }


userInputBox : String -> String -> E.Element Msg
userInputBox userInput draftId =
    Ei.multiline
        [ Font.size normalTextSize
        , ubuntuMono
        , E.height <| E.minimum 100 <| E.shrink
        ]
        { onChange =
            \u -> NewUserInputM { draftId = draftId, userInput = u }
        , text = userInput
        , placeholder = Nothing
        , label =
            Ei.labelAbove
                [ ubuntu
                , Font.size normalTextSize
                , E.paddingEach { left = 0, right = 0, top = 0, bottom = 5 }
                ]
            <|
                E.text "Message"
        , spellcheck = True
        }


editCode : Maybe Code -> String -> E.Element Msg
editCode maybeCode draftId =
    case maybeCode of
        Nothing ->
            Ei.button []
                { onPress = Just <| UploadCodeM draftId
                , label =
                    E.el
                        [ ubuntu, Font.size normalTextSize ]
                    <|
                        E.text "Upload code"
                }

        Just code ->
            E.row []
                [ E.text code.filename
                , E.text <| prettySize <| Bytes.width code.contents
                , Ei.button []
                    { onPress = Just <| DownloadCodeM code
                    , label = E.text "Download"
                    }
                ]


editBlobs : List Blob -> String -> E.Element Msg
editBlobs blobs draftId =
    E.column [] <|
        List.map (editBlob draftId) blobs
            ++ [ uploadBlob draftId ]


uploadBlob : String -> E.Element Msg
uploadBlob draftId =
    Ei.button []
        { onPress = Just <| UploadBlobM draftId
        , label =
            E.el
                [ ubuntu, Font.size normalTextSize ]
            <|
                E.text "Upload a file"
        }


editBlob : String -> Blob -> E.Element Msg
editBlob draftId blob =
    E.row []
        [ E.text blob.filename
        , E.text blob.mime
        , E.text <| prettySize blob.size
        , Ei.button []
            { onPress =
                Just <|
                    DeleteBlobM
                        { draftId = draftId, blobId = blob.id }
            , label = E.text "Delete"
            }
        , Ei.button []
            { onPress = Just <| DownloadBlobM blob
            , label = E.text "Download"
            }
        ]


inboxMessageView :
    ( InboxMessage, Wasm )
    -> Maybe (Dict.Dict String Contact)
    -> E.Element Msg
inboxMessageView ( msg, wasm ) _ =
    E.column []
        [ prettyTime msg.timeReceived
        , E.text msg.fromId
        , wasmView wasm
        , userInputView msg.userInput
        , blobsView msg.blobs
        , codeView msg.code
        ]


ubuntu : E.Attribute Msg
ubuntu =
    Font.family [ Font.typeface "Ubuntu" ]


ubuntuMono : E.Attribute Msg
ubuntuMono =
    Font.family [ Font.typeface "Ubuntu Mono" ]


userInputView : String -> E.Element Msg
userInputView userInput =
    E.el [ ubuntuMono ] <|
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
    E.el [ ubuntuMono ] <|
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
        , ubuntu
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
    E.row
        [ E.centerX
        , E.spacingXY 13 5
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
    , ubuntu
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


emptyDraft : String -> Int -> Draft
emptyDraft id time =
    { id = id
    , subject = ""
    , time = time
    , to = ""
    , userInput = ""
    , code = Nothing
    , blobs = []
    }


messagingButtons : Int -> Page -> E.Element Msg
messagingButtons windowWidth page =
    E.wrappedRow
        [ E.width E.fill
        , E.spacingXY 15 10
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


messagingButtonLabel :
    Int
    -> Page
    -> MessagingButton
    -> E.Element Msg
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
    , ubuntu
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
                Je.object
                    [ ( "key", Je.string key )
                    , ( "value"
                      , Je.string <| B64e.encode <| B64e.bytes bytes
                      )
                    ]

        CacheDeleteE key ->
            jsKeyVal "cacheDelete" <| Je.string key

        GetPowE powInfo ->
            jsKeyVal "getPow" <| encodePowInfo powInfo

        RerunWasmE { userInput, msgId } ->
            jsKeyVal "rerunWasm" <|
                Je.object
                    [ ( "userInput", Je.string userInput )
                    , ( "msgId", Je.string msgId )
                    ]

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


type alias RunWasm =
    { userInput : String
    , wasmCode : Bytes.Bytes
    , msgId : String
    }


type ElmToJs
    = WebsocketE Bytes.Bytes
    | GetPowE PowInfo
    | CacheGetE String
    | CacheSetE String Bytes.Bytes
    | CacheDeleteE String
    | RunWasmE RunWasm
    | RerunWasmE { userInput : String, msgId : String }


type Msg
    = FromCacheM String Bytes.Bytes
    | BadCacheM String String
    | TimeForWriteM Time.Posix
    | BlobSelectedM String File.File
    | BlobUploadedM String File.File Bytes.Bytes
    | CodeSelectedM String File.File
    | CodeUploadedM String File.File Bytes.Bytes
    | MyPublicKeysM MyKeys
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
    | NewToM { draftId : String, to : String }
    | NewSubjectM { draftId : String, subject : String }
    | NewUserInputM { draftId : String, userInput : String }
    | UploadCodeM String
    | UploadBlobM String
    | DeleteBlobM { draftId : String, blobId : String }
    | NullCacheM String
    | BadWebsocketM


fromJsDecoder : Jd.Decoder Msg
fromJsDecoder =
    Jd.andThen fromJsDecoderHelp <|
        Jd.field "key" Jd.string


fromJsDecoderHelp : String -> Jd.Decoder Msg
fromJsDecoderHelp key =
    Jd.field "value" <|
        case key of
            "badWebsocket" ->
                Jd.succeed BadWebsocketM

            "fromCache" ->
                fromCacheDecoder

            "wasmOutput" ->
                wasmOutputDecoder

            "websocket" ->
                Jd.map WebsocketB64M Jd.string

            "pow" ->
                Jd.map (PowM << Pow) b64Json

            "publicKeys" ->
                Jd.map MyPublicKeysM myKeysDecoderJson

            "badCache" ->
                Jd.map2 BadCacheM
                    (Jd.field "key" Jd.string)
                    (Jd.field "error" Jd.string)

            "nullCache" ->
                Jd.map NullCacheM Jd.string

            badKey ->
                Jd.fail <| "bad key: \"" ++ badKey ++ "\""


myKeysDecoderJson : Jd.Decoder MyKeys
myKeysDecoderJson =
    Jd.map2 MyKeys
        (Jd.field "encrypt" b64Json)
        (Jd.field "sign" ecdsaDecoder)


type alias EcdsaKey =
    { x : Bytes.Bytes
    , y : Bytes.Bytes
    }


ecdsaDecoder : Jd.Decoder EcdsaKey
ecdsaDecoder =
    Jd.map2 EcdsaKey
        (Jd.field "x" b64Json)
        (Jd.field "y" b64Json)


b64Json : Jd.Decoder Bytes.Bytes
b64Json =
    Jd.string
        |> Jd.andThen
            (\b64 ->
                case B64d.decode B64d.bytes b64 of
                    Err err ->
                        Jd.fail <| showB64Error err

                    Ok bytes ->
                        Jd.succeed bytes
            )


badWasmBytes : String
badWasmBytes =
    "could not decode WASM output bytes"


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
                                Jd.fail badWasmBytes
            )


badSummary : String -> Maybe String
badSummary box =
    Just <| "could not decode " ++ box ++ " summary bytes"


blobUploaded :
    Model
    -> String
    -> File.File
    -> Bytes.Bytes
    -> ( Model, Cmd Msg )
blobUploaded model draftId file bytes =
    case ( model.page, model.iota ) of
        ( MessagingP (WriteE ( draft, maybeWasm )), Just iota ) ->
            if draft.id /= draftId then
                ( model, Cmd.none )

            else
                let
                    blobId =
                        String.fromInt iota

                    newBlob =
                        { id = blobId
                        , mime = File.mime file
                        , filename = File.name file
                        , size = Bytes.width bytes
                        }

                    newDraft =
                        { draft | blobs = newBlob :: draft.blobs }
                in
                ( { model
                    | page =
                        MessagingP (WriteE ( newDraft, maybeWasm ))
                    , iota = Just <| iota + 1
                  }
                , Cmd.batch
                    [ cacheDraft newDraft
                    , cacheBlob blobId bytes
                    , cacheIota iota
                    ]
                )

        _ ->
            ( model, Cmd.none )


updateDeleteBlob : Model -> String -> String -> ( Model, Cmd Msg )
updateDeleteBlob model draftId blobId =
    case model.page of
        MessagingP (WriteE ( draft, maybeWasm )) ->
            if draft.id /= draftId then
                ( model, Cmd.none )

            else
                let
                    newDraft =
                        { draft
                            | blobs =
                                deleteBlob blobId draft.blobs
                        }
                in
                ( { model
                    | page =
                        MessagingP (WriteE ( newDraft, maybeWasm ))
                  }
                , deleteBlobFromCache blobId
                )

        _ ->
            ( model, Cmd.none )


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
            ( model, Task.perform TimeForWriteM Time.now )

        TimeForWriteM posix ->
            case model.iota of
                Nothing ->
                    ( model, Cmd.none )

                Just iota ->
                    ( { model
                        | page =
                            MessagingP <|
                                WriteE
                                    ( emptyDraft
                                        (String.fromInt iota)
                                        (Time.posixToMillis posix)
                                    , Nothing
                                    )
                        , iota = Just <| iota + 1
                      }
                    , cacheIota iota
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

        FromCacheM "inboxSummary" bytes ->
            case Bd.decode (list inboxMessageSummaryDecoder) bytes of
                Nothing ->
                    ( { model | fatal = badSummary "inbox" }
                    , Cmd.none
                    )

                Just inboxSummary ->
                    ( { model | inboxSummary = Just inboxSummary }
                    , Cmd.none
                    )

        NullCacheM "inboxSummary" ->
            ( { model | inboxSummary = Just [] }, Cmd.none )

        FromCacheM "draftsSummary" bytes ->
            case Bd.decode (list draftSummaryDecoder) bytes of
                Nothing ->
                    ( { model | fatal = badSummary "drafts" }
                    , Cmd.none
                    )

                Just draftsSummary ->
                    ( { model | draftsSummary = Just draftsSummary }
                    , Cmd.none
                    )

        NullCacheM "draftsSummary" ->
            ( { model | draftsSummary = Just [] }, Cmd.none )

        FromCacheM "sentSummary" bytes ->
            case Bd.decode (list sentSummaryDecoder) bytes of
                Nothing ->
                    ( { model | fatal = badSummary "sent" }
                    , Cmd.none
                    )

                Just sentSummary ->
                    ( { model | sentSummary = Just sentSummary }
                    , Cmd.none
                    )

        NullCacheM "sentSummary" ->
            ( { model | sentSummary = Just [] }, Cmd.none )

        FromCacheM "contacts" bytes ->
            case Bd.decode contactsDecoder bytes of
                Nothing ->
                    ( { model
                        | fatal =
                            Just
                                "could not decode contacts bytes"
                      }
                    , Cmd.none
                    )

                Just contacts ->
                    ( { model | contacts = Just contacts }, Cmd.none )

        NullCacheM "contacts" ->
            ( { model | contacts = Just Dict.empty }, Cmd.none )

        FromCacheM "iota" iotaBs ->
            case Bd.decode (Bd.unsignedInt32 Bytes.LE) iotaBs of
                Nothing ->
                    ( { model
                        | fatal =
                            Just
                                "could not decode iota bytes"
                      }
                    , Cmd.none
                    )

                Just iota ->
                    ( { model | iota = Just iota }, Cmd.none )

        NullCacheM "iota" ->
            ( { model | iota = Just 0 }, Cmd.none )

        FromCacheM _ _ ->
            ( model, Cmd.none )

        WasmOutputM msgId wasm ->
            case model.page of
                MessagingP (WriteE ( draft, _ )) ->
                    if draft.id /= msgId then
                        ( model, Cmd.none )

                    else
                        ( { model
                            | page =
                                MessagingP <|
                                    WriteE ( draft, Just wasm )
                            , lastWasmRun = Just msgId
                          }
                        , Cmd.none
                        )

                _ ->
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

        BadCacheM key err ->
            ( { model
                | fatal =
                    Just <|
                        "bad cache: "
                            ++ key
                            ++ ": "
                            ++ err
              }
            , Cmd.none
            )

        NewToM { draftId, to } ->
            case model.page of
                MessagingP (WriteE ( draft, maybeWasm )) ->
                    if draft.id /= draftId then
                        ( model, Cmd.none )

                    else
                        updateDraft
                            { draft | to = to }
                            maybeWasm
                            model

                _ ->
                    ( model, Cmd.none )

        NewSubjectM { draftId, subject } ->
            case model.page of
                MessagingP (WriteE ( draft, maybeWasm )) ->
                    if draft.id /= draftId then
                        ( model, Cmd.none )

                    else
                        updateDraft
                            { draft | subject = subject }
                            maybeWasm
                            model

                _ ->
                    ( model, Cmd.none )

        NewUserInputM i ->
            updateOnUserInput i model

        UploadCodeM draftId ->
            ( model
            , Select.file
                [ "application/wasm" ]
                (CodeSelectedM draftId)
            )

        CodeSelectedM draftId file ->
            ( model
            , Task.perform
                (CodeUploadedM draftId file)
              <|
                File.toBytes file
            )

        CodeUploadedM draftId file bytes ->
            case model.page of
                MessagingP (WriteE ( draft, maybeWasm )) ->
                    if draft.id /= draftId then
                        ( model, Cmd.none )

                    else
                        let
                            newCode =
                                { contents = bytes
                                , mime = File.mime file
                                , filename = File.name file
                                }

                            newDraft =
                                { draft | code = Just newCode }
                        in
                        ( { model
                            | page =
                                MessagingP
                                    (WriteE ( newDraft, maybeWasm ))
                          }
                        , Cmd.batch
                            [ cacheDraft newDraft
                            , elmToJs
                                << encodeToJs
                                << RunWasmE
                              <|
                                { userInput = draft.userInput
                                , wasmCode = newCode.contents
                                , msgId = draft.id
                                }
                            ]
                        )

                _ ->
                    ( model, Cmd.none )

        UploadBlobM draftId ->
            ( model
            , Select.file
                [ "application/octet-stream" ]
                (BlobSelectedM draftId)
            )

        BlobSelectedM draftId file ->
            ( model
            , Task.perform
                (BlobUploadedM draftId file)
              <|
                File.toBytes file
            )

        BlobUploadedM draftId file bytes ->
            blobUploaded model draftId file bytes

        DeleteBlobM { draftId, blobId } ->
            updateDeleteBlob model draftId blobId

        NullCacheM unknown ->
            ( { model
                | fatal =
                    Just <| "unknown null cache: " ++ unknown
              }
            , Cmd.none
            )

        BadWebsocketM ->
            ( { model | nonFatal = Just "No network connection" }, Cmd.none )

        MyPublicKeysM _ ->
            ( model, Cmd.none )


deleteBlob : String -> List Blob -> List Blob
deleteBlob id blobs =
    List.filter (\blob -> blob.id /= id) blobs


deleteBlobFromCache : String -> Cmd Msg
deleteBlobFromCache =
    elmToJs << encodeToJs << CacheDeleteE


cacheBlob : String -> Bytes.Bytes -> Cmd Msg
cacheBlob id bytes =
    CacheSetE id bytes |> encodeToJs |> elmToJs


updateOnUserInput :
    { draftId : String, userInput : String }
    -> Model
    -> ( Model, Cmd Msg )
updateOnUserInput { draftId, userInput } model =
    case model.page of
        MessagingP (WriteE ( draft, maybeWasm )) ->
            if draft.id /= draftId then
                ( model, Cmd.none )

            else
                case model.lastWasmRun of
                    Nothing ->
                        onUserInputHelp
                            model
                            draft
                            userInput
                            maybeWasm

                    Just lastRunId ->
                        if lastRunId /= draft.id then
                            onUserInputHelp
                                model
                                draft
                                userInput
                                maybeWasm

                        else
                            let
                                newDraft =
                                    { draft | userInput = userInput }
                            in
                            ( { model
                                | page =
                                    MessagingP <|
                                        WriteE ( newDraft, maybeWasm )
                              }
                            , Cmd.batch
                                [ cacheDraft newDraft
                                , case draft.code of
                                    Nothing ->
                                        Cmd.none

                                    Just _ ->
                                        { userInput = userInput
                                        , msgId = draft.id
                                        }
                                            |> elmToJs
                                            << encodeToJs
                                            << RerunWasmE
                                ]
                            )

        _ ->
            ( model, Cmd.none )


onUserInputHelp :
    Model
    -> Draft
    -> String
    -> Maybe Wasm
    -> ( Model, Cmd Msg )
onUserInputHelp model draft userInput maybeWasm =
    let
        newDraft =
            { draft | userInput = userInput }
    in
    ( { model | page = MessagingP (WriteE ( newDraft, maybeWasm )) }
    , Cmd.batch
        [ cacheDraft newDraft
        , case draft.code of
            Nothing ->
                Cmd.none

            Just { contents } ->
                runDraftWasm
                    { userInput = userInput
                    , wasmCode = contents
                    , msgId = draft.id
                    }
        ]
    )


runDraftWasm : RunWasm -> Cmd Msg
runDraftWasm =
    elmToJs << encodeToJs << RunWasmE


cacheDraftsSummary : List DraftSummary -> Cmd Msg
cacheDraftsSummary =
    elmToJs
        << encodeToJs
        << CacheSetE "draftsSummary"
        << Be.encode
        << listEncoder draftSummaryEncoder


draftSummaryEncoder : DraftSummary -> Be.Encoder
draftSummaryEncoder { subject, to, time, id } =
    Be.sequence
        [ stringEncoder subject
        , stringEncoder to
        , Be.unsignedInt32 Bytes.LE time
        , stringEncoder id
        ]


listEncoder : (a -> Be.Encoder) -> List a -> Be.Encoder
listEncoder encoder items =
    Be.sequence <|
        (Be.unsignedInt32 Bytes.LE <| List.length items)
            :: List.map encoder items


updateDraftsSummary :
    Draft
    -> Maybe (List DraftSummary)
    -> Maybe (List DraftSummary)
updateDraftsSummary draft maybeOldSummary =
    case maybeOldSummary of
        Nothing ->
            maybeOldSummary

        Just oldSummary ->
            Just <| List.map (updateDraftSummary draft) oldSummary


updateDraftSummary : Draft -> DraftSummary -> DraftSummary
updateDraftSummary draft summary =
    if draft.id == summary.id then
        { subject = draft.subject
        , to = draft.to
        , time = draft.time
        , id = draft.id
        }

    else
        summary


cacheDraft : Draft -> Cmd Msg
cacheDraft draft =
    draft
        |> elmToJs
        << encodeToJs
        << CacheSetE draft.id
        << Be.encode
        << draftEncoder


draftEncoder : Draft -> Be.Encoder
draftEncoder { id, subject, to, userInput, code, blobs } =
    Be.sequence
        [ stringEncoder id
        , stringEncoder subject
        , stringEncoder to
        , stringEncoder userInput
        , maybeCodeEncoder code
        , listEncoder blobEncoder blobs
        ]


maybeCodeEncoder : Maybe Code -> Be.Encoder
maybeCodeEncoder maybeCode =
    case maybeCode of
        Nothing ->
            Be.unsignedInt8 0

        Just { contents, mime, filename } ->
            Be.sequence
                [ Be.unsignedInt8 1
                , bytesEncoder contents
                , stringEncoder mime
                , stringEncoder filename
                ]


bytesEncoder : Bytes.Bytes -> Be.Encoder
bytesEncoder bytes =
    Be.sequence
        [ Be.unsignedInt32 Bytes.LE <| Bytes.width bytes
        , Be.bytes bytes
        ]


blobEncoder : Blob -> Be.Encoder
blobEncoder { id, mime, filename, size } =
    Be.sequence
        [ stringEncoder id
        , stringEncoder mime
        , stringEncoder filename
        , Be.unsignedInt32 Bytes.LE size
        ]


cacheIota : Int -> Cmd Msg
cacheIota =
    elmToJs
        << encodeToJs
        << CacheSetE "iota"
        << Be.encode
        << Be.unsignedInt32 Bytes.LE


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
        ( KeysFromJsG, MyPublicKeysM myKeys ) ->
            ContinuingT
                model
                getPowInfo
                (GetMeP <| PowInfoG myKeys)

        ( KeysFromJsG, _ ) ->
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
                    , Be.bytes sign.x
                    , Be.bytes sign.y
                    , Be.bytes encrypt
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


badInboxMsg : Maybe String
badInboxMsg =
    Just "bad bytes in inbox message from cache"


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
                            { model | fatal = badInboxMsg }
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

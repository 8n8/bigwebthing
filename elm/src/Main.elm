port module Main exposing (main)

import Base64.Decode as B64d
import Base64.Encode as B64e
import Browser
import Browser.Events
import Bytes
import Bytes.Decode as Bd
import Bytes.Encode as Be
import Dict
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Ei
import File
import File.Download as Download
import File.Select as Select
import Hex.Convert
import Html
import Http
import Json.Decode as Jd
import Json.Encode as Je
import Task
import Time
import MessageIdMap as Mid
import UserIdMap as Uid
import SHA256


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
                        Bd.map Ordering <|
                            list wasmOutputBytesDecoder

                    1 ->
                        Bd.map SmallString stringDecoder

                    _ ->
                        Bd.fail
            )


type MessagingButton
    = ContactsB
    | MessagesB
    | AccountB
    | WriterB


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
    , filename : String
    }


type alias Model =
    { messagingHover : Maybe MessagingButton
    , page : PageV
    , windowWidth : Int
    , fatal : Maybe String
    , badWasm : Maybe String
    , timeZone : Maybe Time.Zone
    , summaries : Mid.MessageIdMap Summary
    , whitelist : Uid.UserIdMap
    , zone : Maybe Time.Zone
    , lastWasmId : Maybe String
    , counter : Int
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
    , sign : Bytes.Bytes
    }


type MyName
    = MyName String


type Process
    = GetMeP GetMe
    | GetInboxMessageP GetInboxMessage
    | GetDraftP GetDraft
    | BlobForDownloadP Blob


type GetDraft
    = FromCacheD String
    | WasmOutputD Draft


type GetInboxMessage
    = FromCacheG String
    | WasmOutputG InboxMessage


initModel : Int -> Model
initModel windowWidth =
    { fatal = Nothing
    , windowWidth = windowWidth
    , lastWasmId = Nothing
    , messagingHover = Nothing
    , badWasm = Nothing
    , timeZone = Nothing
    , summaries = Mid.empty
    , whitelist = Uid.empty
    , page = Messages
    , zone = Nothing
    , counter = 0
    }


init : Int -> ( Model, Cmd Msg )
init windowWidth =
    ( initModel windowWidth, initCmd )


initCmd : Cmd Msg
initCmd =
        Task.perform TimeZoneM Time.here


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
                , messagingButtons model.windowWidth model.page
                , mainPage model
                ]


mainPage : Model -> E.Element Msg
mainPage model =
    case (model.timeZone, model.page) of
        (Nothing, _) ->
            E.text "error: no time zone"

        (Just zone, Messages) ->
                    summaryView zone model.whitelist model.summaries

        ( Just zone, Writer {wasm, snapshot, summaries, shareSearch}) ->
            E.column []
                [ viewSnapshot
                    zone
                    snapshot
                    model.whitelist
                    shareSearch
                , case wasm of
                    Nothing ->
                        E.none

                    Just justWasm ->
                        wasmView justWasm
                , viewSummaries zone model.whitelist summaries
                ]

        (_, Contacts) ->
            case Uid.toList model.whitelist of
                Nothing ->
                    E.text "could not decode contacts"

                Just whitelist ->
                    E.column [] <|
                        List.map contactView whitelist


contactView : (Uid.Username, Uid.Fingerprint) -> E.Element Msg
contactView (username, fingerprint) =
    E.row []
        [ E.text <| prettyUserId fingerprint username
        , deleteContact username
        ]


deleteContact : Uid.Username -> E.Element Msg
deleteContact username =
    Ei.button []
        { onPress = Just <| DeleteContactM username
        , label = E.text "Delete"
        }


viewSummaries :
    Time.Zone ->
    Uid.UserIdMap ->
    List SnapSummary ->
    E.Element Msg
viewSummaries zone userIds summaries =
    E.column [] <| List.map (viewSnapSummary zone userIds) summaries


viewSnapSummary :
    Time.Zone ->
    Uid.UserIdMap ->
    SnapSummary ->
    E.Element Msg
viewSnapSummary zone whitelist {time, edit, author} =
    E.row []
        [ prettyTime time zone
        , E.text edit
        , E.text <| prettyAuthor whitelist author
        ]


summaryView :
    Time.Zone ->
    Uid.UserIdMap ->
    Mid.MessageIdMap Summary ->
    E.Element Msg
summaryView zone whitelist summaries =
    case Mid.toList summaries of
        Nothing ->
            E.text "could not convert summaries to list"

        Just summariesList ->
            E.column [] <|
            List.map (oneSummaryLine zone whitelist) <|
            summariesList


oneSummaryLine :
    Time.Zone ->
    Uid.UserIdMap ->
    (Mid.MessageId, Summary) ->
    E.Element Msg
oneSummaryLine zone whitelist (messageId, summary) =
    E.row []
        [ prettyTime summary.time zone
        , E.text summary.subject
        , E.text <| prettyAuthor whitelist summary.author
        ]


prettyAuthor : Uid.UserIdMap -> Uid.Username -> String
prettyAuthor map username =
    case Uid.get username map of
        Nothing ->
            "could not display username"

        Just f ->
            prettyUserId f username
                    

prettyUserId : Uid.Fingerprint -> Uid.Username -> String
prettyUserId (Uid.Fingerprint f) (Uid.Username u) =
    bytesToString u ++ bytesToString f



bytesToString : Bytes.Bytes -> String
bytesToString =
    B64e.encode << B64e.bytes


-- writerView : (Mid.MessageId, List Diff, Maybe Wasm) -> E.Element Msg
-- writerView (messageId, diffs, maybeWasm) =
--     E.column [ E.spacing 30, E.paddingXY 0 20, E.width E.fill ] <|
--         case constructMessage diffs of
--             Err err ->
--                 E.text <| "Internal error: " ++ err
-- 
--             Ok snapshot ->
--                 [ toBox <| showShares <| snapshot.shares messageId
--                 , userInputBox snapshot.mainBox messageId
--                 , case maybeWasm of
--                     Nothing ->
--                         E.none
-- 
--                     Just wasm ->
--                         wasmView wasm
--                 , editCode snapshot.wasm messageId
--                 , editBlobs snapshot.blobs messageId
--                 ]


showShares : List Uid.UserId -> String
showShares userIds =
    String.concat <| List.map showUserId userIds


showUserId : Uid.UserId -> String
showUserId
    (Uid.UserId
        (Uid.Username username)
        (Uid.Fingerprint fingerprint)) =

    bytesToString fingerprint ++ bytesToString username
    


sendButtonLabel : E.Element Msg
sendButtonLabel =
    E.el writerButtons <|
        E.text "Send"


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
        { onChange = NewToM
        , text = to
        , placeholder = Nothing
        , label =
            Ei.labelLeft
                [ Font.size normalTextSize
                , ubuntu
                , E.centerY
                , E.paddingEach
                    { left = 0, right = 7, bottom = 0, top = 0 }
                ]
            <|
                E.text "To"
        }


userInputBox : String -> String -> E.Element Msg
userInputBox userInput draftId =
    Ei.multiline
        [ Font.size normalTextSize
        , ubuntuMono
        , E.height <| E.minimum 100 <| E.shrink
        ]
        { onChange = NewUserInputM
        , text = userInput
        , placeholder = Nothing
        , label =
            Ei.labelAbove
                [ ubuntu
                , Font.size normalTextSize
                , E.paddingEach
                    { left = 0, right = 0, top = 0, bottom = 5 }
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
                    E.el writerButtons <|
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


editBlobs : List Blob -> Mid.MessageId -> E.Element Msg
editBlobs blobs messageId =
    E.column [] <|
        List.map (editBlob messageId) blobs
            ++ [ uploadBlob ]


writerButtons : List (E.Attribute Msg)
writerButtons =
    [ ubuntu
    , Font.size normalTextSize
    , E.paddingXY 8 16
    , Border.rounded buttonCorner
    , E.mouseOver [ Background.color veryLightBlue ]
    ]


uploadBlob : E.Element Msg
uploadBlob =
    Ei.button []
        { onPress = Just UploadBlobM
        , label =
            E.el writerButtons <|
                E.text "Upload a file"
        }


editBlob : Mid.MessageId -> Blob -> E.Element Msg
editBlob messageId blob =
    E.row []
        [ E.text blob.filename
        , E.text blob.mime
        , E.text <| prettySize blob.size
        , Ei.button []
            { onPress = Just <| DownloadBlobM blob
            , label = E.text "Download"
            }
        , Ei.button []
            { onPress = Just <| DeleteBlobM blob
            , label = E.text "Delete"
            }
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


prettyWeekday : Time.Weekday -> String
prettyWeekday weekday =
    case weekday of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


prettyMonth : Time.Month -> String
prettyMonth month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


prettyTime : Time.Posix -> Time.Zone -> E.Element Msg
prettyTime posix zone =
    let
        year =
            String.fromInt <| Time.toYear zone posix

        month =
            prettyMonth <| Time.toMonth zone posix

        day =
            String.padLeft 2 ' ' <|
                String.fromInt <|
                    Time.toDay zone posix

        weekday =
            prettyWeekday <| Time.toWeekday zone posix

        hour =
            String.padLeft 2 ' ' <|
                String.fromInt <|
                    Time.toHour zone posix

        minute =
            String.padLeft 2 '0' <|
                String.fromInt <|
                    Time.toMinute zone posix
    in
    E.text <|
        String.concat
            [ year
            , " "
            , weekday
            , " "
            , month
            , " "
            , day
            , " "
            , hour
            , ":"
            , minute
            ]


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


lightBlue : E.Color
lightBlue =
    E.rgb255 153 194 255


veryLightBlue : E.Color
veryLightBlue =
    E.rgb255 204 224 255


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


adminLabelStyle : Int -> List (E.Attribute Msg)
adminLabelStyle windowWidth =
    [ E.centerX
    , ubuntu
    , Font.size <| adminButtonFontSize windowWidth
    , E.paddingXY 8 16
    , Border.rounded buttonCorner
    ]


white : E.Color
white =
    E.rgb255 255 255 255


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


messagingButtons :
    Int
    -> PageV
    -> E.Element Msg
messagingButtons windowWidth page =
    E.wrappedRow
        [ E.width E.fill
        , E.spacingXY 15 10
        ]
    <|
        List.map
            (messagingButton windowWidth page)
            [ MessagesB, WriterB, ContactsB, AccountB ]


messagingButton :
    Int
    -> PageV
    -> MessagingButton
    -> E.Element Msg
messagingButton windowWidth page button =
    Ei.button
        ([ E.width <| E.minimum 150 E.fill
         , Border.rounded buttonCorner
         ]
            ++ (if messagingPageOn page button then
                    [ Background.color lightBlue ]

                else
                    [ Background.color white
                    , E.mouseOver [ Background.color veryLightBlue ]
                    ]
               )
        )
        { onPress =
            Just <| messagingButtonMsg button
        , label = messagingButtonLabel windowWidth button
        }


messagingButtonMsg : MessagingButton -> Msg
messagingButtonMsg button =
    case button of
        ContactsB ->
            ContactsM

        WriterB ->
            WriteM


messagingButtonLabel :
    Int
    -> MessagingButton
    -> E.Element Msg
messagingButtonLabel windowWidth button =
    E.el
        (messagingLabelStyle windowWidth)
    <|
        E.text <|
            messagingLabelText button


messagingLabelText : MessagingButton -> String
messagingLabelText button =
    case button of
        ContactsB ->
            "Contacts"


buttonCorner : Int
buttonCorner =
    3


messagingLabelStyle :
    Int
    -> List (E.Attribute Msg)
messagingLabelStyle windowWidth =
    [ E.centerX
    , ubuntu
    , Border.rounded buttonCorner
    , E.paddingXY 0 20
    , Font.size <| messagingButtonFontSize windowWidth
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


messagingPageOn : PageV -> MessagingButton -> Bool
messagingPageOn page msg =
    case ( page, msg ) of
        (Messages, ContactsB) ->
            False


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


hash32ToString : Hash32 -> String
hash32ToString (Hash32 hash) =
    bytesToString hash


encodeToJs : ElmToJs -> Je.Value
encodeToJs value =
    case value of
        ToBackendE toBackend ->
            jsKeyVal "toBackend" <|
                Je.string <|
                    B64e.encode <|
                        B64e.bytes <|
                            Be.encode <|
                                encodeToBackend toBackend

        RerunWasmE { userInput, id } ->
            jsKeyVal "rerunWasm" <|
                Je.object
                    [ ( "userInput", Je.string <| bytesToString userInput )
                    , ( "msgId", Je.string id)
                    ]

        RunWasmE { input, wasmCode, id } ->
            jsKeyVal "runWasm" <|
                Je.object
                    [ ( "userInput"
                      , Je.string <| B64e.encode <| B64e.bytes input
                      )
                    , ( "wasmCode"
                      , Je.string <|
                            B64e.encode <|
                                B64e.bytes wasmCode
                      )
                    , ( "id", Je.string id)
                    ]


type ToBackend
    = NewMain String
    | NewTo String
    | NewSubject String
    | NewShares (List Uid.UserId)
    | NewWasm Code
    | NewBlobs (List Blob)
    | MessagesClick
    | WriterClick
    | ContactsClick
    | AccountClick
    | DeleteBlob String


encodeToBackend : ToBackend -> Be.Encoder
encodeToBackend toBackend =
    case toBackend of
        MessagesClick ->
            Be.unsignedInt8 0

        WriterClick ->
            Be.unsignedInt8 1

        ContactsClick ->
            Be.unsignedInt8 2

        AccountClick ->
            Be.unsignedInt8 3

        NewMain m ->
            Be.sequence [Be.unsignedInt8 4, Be.string m]

        NewShares shares ->
            Be.sequence <|
                Be.unsignedInt8 6 ::
                    (List.map encodeUserId shares)

        NewWasm code ->
            Be.sequence [Be.unsignedInt8 7, encodeCode code]

        NewBlobs blobs ->
            Be.sequence <|
                Be.unsignedInt8 8 ::
                    List.map blobEncoder blobs

        NewTo to ->
            Be.sequence [Be.unsignedInt8 9, Be.string to]

        NewSubject subject ->
            Be.sequence [Be.unsignedInt8 10, Be.string subject]

        DeleteBlob id ->
            Be.sequence [Be.unsignedInt8 11, Be.string id]


encodeCode : Code -> Be.Encoder
encodeCode {contents, filename} =
    Be.sequence [Be.bytes contents, Be.string filename]


encodeUserId : Uid.UserId -> Be.Encoder
encodeUserId (Uid.UserId username fingerprint) =
    Be.sequence
        [encodeUsername username, encodeFingerprint fingerprint]


encodeFingerprint : Uid.Fingerprint -> Be.Encoder
encodeFingerprint (Uid.Fingerprint f) =
    Be.bytes f


encodeUsername : Uid.Username -> Be.Encoder
encodeUsername (Uid.Username u) =
    Be.bytes u


type Hash32
    = Hash32 Bytes.Bytes


powInfoEncoder : PowInfo -> Be.Encoder
powInfoEncoder { difficulty, unique } =
    Be.sequence [ Be.unsignedInt8 difficulty, Be.bytes unique ]


type alias RunWasm =
    { input : Bytes.Bytes
    , wasmCode : Bytes.Bytes
    , id : String
    }


type ElmToJs
    = ToBackendE ToBackend
    | RunWasmE RunWasm
    | RerunWasmE { userInput : Bytes.Bytes, id : String }


type Msg
    = TimeZoneM Time.Zone
    | DeleteContactM Uid.Username
    | UploadedM (Result Http.Error ())
    | FromBackendM Bytes.Bytes
    | BadWasmM String
    | NoBackendM
    | BlobSelectedM File.File
    | CodeSelectedM String File.File
    | WasmOutputM String Wasm
    | JsonFromJsM Je.Value
    | ContactsM
    | WriteM
    | NewWindowWidthM Int
    | DownloadCodeM Code
    | DownloadBlobM Blob
    | NewToM String
    | NewSubjectM String
    | NewUserInputM String
    | UploadCodeM String
    | UploadBlobM
    | NewViewM ReadyView
    | NonsenseFromBackendM Bytes.Bytes
    | DeleteBlobM Blob
    | ShareBoxContentsM String


fromJsDecoder : Jd.Decoder Msg
fromJsDecoder =
    Jd.andThen fromJsDecoderHelp <|
        Jd.field "key" Jd.string


fromJsDecoderHelp : String -> Jd.Decoder Msg
fromJsDecoderHelp key =
    Jd.field "value" <|
        case key of
            "fromBackend" ->
                Jd.map FromBackendM b64Json

            "noBackend" ->
                Jd.succeed NoBackendM

            "wasmOutput" ->
                wasmOutputDecoder

            "badWasm" ->
                Jd.map BadWasmM Jd.string

            _ ->
                Jd.fail "Bad data from Javascript"


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
                        case
                            Bd.decode wasmOutputBytesDecoder bytes
                        of
                            Just wasm ->
                                Jd.succeed <| WasmOutputM id wasm

                            Nothing ->
                                Jd.fail badWasmBytes
            )


badSummary : String -> Maybe String
badSummary box =
    Just <| "could not decode " ++ box ++ " summary bytes"


showB64Error : B64d.Error -> String
showB64Error error =
    case error of
        B64d.ValidationError ->
            "Validation error"

        B64d.InvalidByteSequence ->
            "Invalid byte sequence"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeZoneM zone ->
            ( { model | zone = Just zone }, Cmd.none )


clickCmd : Maybe a -> Cmd Msg -> Cmd Msg
clickCmd summary cmd =
    case summary of
        Nothing ->
            cmd

        Just _ ->
            Cmd.none


localUrl : String
localUrl =
    "http://localhost:17448"


updateSimple : Msg -> Model -> ( Model, Cmd Msg )
updateSimple msg model =
    case msg of
        WriteM ->
            ( model
            , WriterClick |>
              ToBackendE |>
              encodeToJs |>
              elmToJs
            )

        ContactsM ->
            ( model
            , ContactsClick |>
              ToBackendE |>
              encodeToJs |>
              elmToJs
            )

        NewWindowWidthM width ->
            ( { model | windowWidth = width }, Cmd.none )

        DownloadCodeM { contents, filename } ->
            ( model
            , Download.bytes filename "application/wasm" contents
            )

        WasmOutputM newWasmId wasm ->
            case (model.page, model.lastWasmId) of
                (Writer _, Nothing) ->
                    (model, Cmd.none)

                (Writer w, Just lastWasmId) ->
                    if newWasmId /= lastWasmId then
                        ( model, Cmd.none )

                    else
                        ( { model
                            | page = Writer {w | wasm = Just wasm}
                            , badWasm = Nothing
                          }
                        , Cmd.none
                        )

                _ ->
                    ( model, Cmd.none )

        JsonFromJsM json ->
            case Jd.decodeValue fromJsDecoder json of
                Err err ->
                    ( { model
                        | fatal =
                            Just <| Jd.errorToString err
                      }
                    , Cmd.none
                    )

                Ok fromJsMsg ->
                    update fromJsMsg model

        NewUserInputM i ->
            updateOnUserInput i model

        UploadCodeM draftId ->
            ( model
            , Select.file
                [ "application/wasm" ]
                (CodeSelectedM draftId)
            )

        CodeSelectedM draftId file ->
            (model, cacheCode file)

        FromBackendM bytes ->
            update (decodeFromBackend bytes) model

        BadWasmM err ->
            ( { model | badWasm = Just err }, Cmd.none )

        NoBackendM ->
            ( { model | fatal = Just "No backend" }, Cmd.none )

        TimeZoneM zone ->
            ( { model | timeZone = Just zone }, Cmd.none )

        UploadedM (Ok ()) ->
            ( model, Cmd.none )

        UploadedM (Err err) ->
            ( { model | fatal = Just <| httpErrToString err }
            , Cmd.none
            )

        NewViewM newView ->
            ( { model
                | whitelist = newView.whitelist
                , summaries = newView.summaries
                , page = newView.page
              }
            , Cmd.none
            )

        BlobSelectedM file ->
            (model, cacheBlob file)

        DownloadBlobM {id} ->
            (model, Download.url <| localUrl ++ "/" ++ id)

        NewToM to ->
            (model, NewTo to |> ToBackendE |> encodeToJs |> elmToJs)

        NewSubjectM subject ->
            ( model
            , NewSubject subject |>
              ToBackendE |>
              encodeToJs |>
              elmToJs
            )

        UploadBlobM ->
            ( model
            , Select.file
                ["application/octet-stream"]
                BlobSelectedM
            )

        NonsenseFromBackendM rawNonsense ->
            ( { model | fatal = Just <|
                "bad messeage from backend: " ++
                    bytesToString rawNonsense
              }
            , Cmd.none
            )

        DeleteBlobM {id} ->
            ( model
            , DeleteBlob id |>
              ToBackendE |>
              encodeToJs |>
              elmToJs
            )


type Mime
    = Mime String


findDraft : String -> Maybe (List DraftSummary) -> Maybe DraftSummary
findDraft draftId maybeDraftsSummary =
    maybeDraftsSummary
        |> Maybe.andThen
            (\draftsSummary ->
                case
                    List.filter
                        (\d -> d.id == draftId)
                        draftsSummary
                of
                    [] ->
                        Nothing

                    d :: _ ->
                        Just d
            )


httpErrToString : Http.Error -> String
httpErrToString err =
    case err of
        Http.BadUrl u ->
            "bad url: " ++ u

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus code ->
            "bad status: " ++ String.fromInt code

        Http.BadBody e ->
            "bad body: " ++ e


nonsenseFromServer : Bytes.Bytes -> Maybe String
nonsenseFromServer bytes =
    Just <| "nonsense from server: " ++ Hex.Convert.toString bytes


nonsenseFromBackend : Bytes.Bytes -> Maybe String
nonsenseFromBackend bytes =
    Just <| "nonsense from backend: " ++ Hex.Convert.toString bytes


decodeFromBackend : Bytes.Bytes -> Msg
decodeFromBackend bytes =
    case Bd.decode fromBackendDecoder bytes of
        Nothing ->
            NonsenseFromBackendM bytes

        Just fromBackend ->
            fromBackend


viewDecoder : Bd.Decoder ReadyView
viewDecoder =
    Bd.map3 ReadyView
        whitelistD
        summariesD
        pageD


fingerprintD : Bd.Decoder Uid.Fingerprint
fingerprintD =
    Bd.map Uid.Fingerprint (Bd.bytes 8)


usernameD : Bd.Decoder Uid.Username
usernameD =
    Bd.map Uid.Username (Bd.bytes 8)


summaryD : Bd.Decoder (Mid.MessageId, Summary)
summaryD =
    Bd.map2 (\m s -> (m, s))
        messageIdD
        (Bd.map3 Summary
            stringDecoder
            timeD
            usernameD)


diffsD : Bd.Decoder Diff
diffsD =
    Bd.map4 Diff
        (Bd.unsignedInt32 Bytes.LE)
        (Bd.unsignedInt32 Bytes.LE)
        bytesDecoder
        hash8D


hash8D : Bd.Decoder Hash8
hash8D =
    Bd.map Hash8 <| Bd.bytes 8


timeD : Bd.Decoder Time.Posix
timeD =
    Bd.map
        (Time.millisToPosix << ((*) 1000))
        (Bd.unsignedInt32 Bytes.LE)


whitelistD : Bd.Decoder Uid.UserIdMap
whitelistD =
    Bd.map Uid.fromList (list userIdD)


userIdD : Bd.Decoder Uid.UserId
userIdD =
    Bd.map2 Uid.UserId usernameD fingerprintD


summariesD : Bd.Decoder (Mid.MessageIdMap Summary)
summariesD =
    Bd.map Mid.fromList <| list summaryD


type alias ReadyView =
    { whitelist : Uid.UserIdMap
    , summaries : Mid.MessageIdMap Summary
    , page : PageV
    }


pageD : Bd.Decoder PageV
pageD =
    Bd.andThen pageHelpD Bd.unsignedInt8


snapSummaryD : Bd.Decoder SnapSummary
snapSummaryD =
    Bd.map3 SnapSummary
        timeD
        stringDecoder
        usernameD



pageHelpD : Int -> Bd.Decoder PageV
pageHelpD indicator =
    case indicator of
        0 ->
            Bd.succeed Messages

        1 ->
            Bd.map2 (\n u -> Writer <| WriterPage Nothing n u "")
                snapshotD
                (list snapSummaryD)

        2 ->
            Bd.succeed Contacts

        3 ->
            Bd.succeed Account

        _ ->
            Bd.fail


messageIdD : Bd.Decoder Mid.MessageId
messageIdD =
    Bd.map Mid.MessageId (Bd.bytes 24)


type PageV
    = Messages
    | Writer WriterPage
    | Contacts
    | Account


type alias WriterPage = 
    { wasm : Maybe Wasm
    , snapshot : Snapshot
    , summaries : List SnapSummary
    , shareSearch : String
    }


type alias SnapSummary =
    { time : Time.Posix
    , edit : String
    , author : Uid.Username
    }


type alias Diff =
    { start : Int
    , end : Int
    , insert : Bytes.Bytes
    , integrity : Hash8
    }


type alias Summary =
    { subject : String
    , time : Time.Posix
    , author : Uid.Username
    }


fromBackendDecoder : Bd.Decoder Msg
fromBackendDecoder =
    Bd.map NewViewM viewDecoder


cacheCode : File.File -> Cmd Msg
cacheCode file =
    Http.post
        { url = localUrl ++ "/uploadcode"
        , body = Http.fileBody file
        , expect = Http.expectWhatever UploadedM
        }


cacheBlob : File.File -> Cmd Msg
cacheBlob file =
    Http.post
        { url = localUrl ++ "/uploadfile"
        , body = Http.fileBody file
        , expect = Http.expectWhatever UploadedM
        }


emptyBytes =
    Be.encode <| Be.sequence []


type Hash8 =
    Hash8 Bytes.Bytes


sha256 : Bytes.Bytes -> Bytes.Bytes
sha256 bytes =
    SHA256.fromBytes bytes |> SHA256.toBytes


hash8 : Bytes.Bytes -> Maybe Hash8
hash8 bytes =
    case Bd.decode (Bd.bytes 8) (sha256 bytes) of
        Nothing ->
            Nothing

        Just bytes8 ->
            Just <| Hash8 bytes8


hash32 : Bytes.Bytes -> Hash32
hash32 bytes =
    Hash32 <| sha256 bytes


reverseBytes : Bytes.Bytes -> Maybe Bytes.Bytes
reverseBytes bytes =
    let
        decoder =
            Bd.loop (emptyBytes, Bytes.width bytes) reverseBytesHelp
    in
    Bd.decode decoder bytes


reverseBytesHelp :
    (Bytes.Bytes, Int) ->
    Bd.Decoder (Bd.Step (Bytes.Bytes, Int) Bytes.Bytes)
reverseBytesHelp (setyb, i) =
    if i == 0 then
        Bd.succeed <| Bd.Done setyb

    else
        Bd.map
            (\b ->
                Bd.Loop
                    ( Be.encode <|
                        Be.sequence
                            [ Be.unsignedInt8 b
                            , Be.bytes setyb
                            ]
                    , i - 1))
            Bd.unsignedInt8


type alias Snapshot =
    { time : Time.Posix
    , shares : List Uid.Username
    , mainBox : String
    , blobs : List Blob
    , wasm : Maybe Code
    }


viewSnapshot : Time.Zone -> Snapshot -> Uid.UserIdMap -> String -> E.Element Msg
viewSnapshot
    zone
    {time, shares, mainBox, blobs, wasm}
    userIds
    shareSearch =

    E.column []
        [ prettyTime time zone
        , addShareBox shareSearch
        , viewShares shares userIds
        , userInputView mainBox
        , blobsView blobs
        , case wasm of
            Just justWasm ->
                codeView justWasm

            Nothing ->
                E.none
        ]


viewShares : List Uid.Username -> Uid.UserIdMap -> E.Element Msg
viewShares usernames whitelist =
    E.column [] <|
        List.map (viewShare whitelist) usernames


viewShare : Uid.UserIdMap -> Uid.Username -> E.Element Msg
viewShare whitelist username =
    case Uid.get username whitelist of
        Nothing ->
            E.text "could not display username"

        Just f ->
            E.text <| prettyUserId f username
            

addShareBox : String -> E.Element Msg
addShareBox contents =
    Ei.text
        [ Font.size normalTextSize
        , ubuntuMono
        , E.width <| E.maximum 400 <| E.fill
        ]
        { onChange = ShareBoxContentsM
        , text = contents
        , placeholder = Nothing
        , label =
            Ei.labelLeft
                [ Font.size normalTextSize
                , ubuntu
                , E.centerY
                , E.paddingEach
                    { left = 0, right = 7, bottom = 0, top = 0}
                ]
            <|
                E.text "To"
        }


snapshotD : Bd.Decoder Snapshot
snapshotD =
    Bd.map5 Snapshot
        timeD
        (list usernameD)
        stringDecoder
        (list blobD)
        (maybeD codeD)


maybeD : Bd.Decoder a -> Bd.Decoder (Maybe a)
maybeD decoder =
    Bd.unsignedInt8
        |> Bd.andThen (maybeHelp decoder)


maybeHelp : Bd.Decoder a -> Int -> Bd.Decoder (Maybe a)
maybeHelp decoder indicator =
    case indicator of
        0 ->
            Bd.succeed Nothing

        1 ->
            Bd.map Just decoder

        _ ->
            Bd.fail


blobD : Bd.Decoder Blob
blobD =
    Bd.map4 Blob
        stringDecoder
        stringDecoder
        stringDecoder
        (Bd.unsignedInt32 Bytes.LE)


codeD : Bd.Decoder Code
codeD =
    Bd.map2 Code
       bytesDecoder
       stringDecoder


updateOnUserInput : String -> Model -> ( Model, Cmd Msg )
updateOnUserInput userInput model =
    case model.page of
        Writer w ->
            case w.snapshot.wasm of
                Nothing ->
                    (model, Cmd.none)

                Just code ->
                    case model.lastWasmId of
                        Nothing ->
                            ( { model |
                                    lastWasmId =
                                        Just <|
                                        String.fromInt
                                        model.counter
                              , counter = model.counter + 1
                              }
                            , Cmd.batch
                                [ RunWasmE
                                    { input =
                                        Be.encode
                                            (Be.string
                                                userInput)
                                    , wasmCode = code.contents
                                    , id = String.fromInt model.counter
                                    } |>
                                    encodeToJs |>
                                    elmToJs
                                , NewMain userInput |>
                                  ToBackendE |>
                                  encodeToJs |>
                                  elmToJs
                                ]
                            )

                        Just lastWasmId ->
                                ( model
                                , Cmd.batch
                                    [ RerunWasmE
                                        { userInput =
                                            Be.encode (Be.string userInput)
                                        , id = lastWasmId
                                        } |>
                                      encodeToJs |>
                                      elmToJs
                                    , NewMain userInput |>
                                      ToBackendE |>
                                      encodeToJs |>
                                      elmToJs
                                    ]
                                )

        _ ->
            ( model, Cmd.none )


runDraftWasm : RunWasm -> Cmd Msg
runDraftWasm =
    elmToJs << encodeToJs << RunWasmE


draftSummaryEncoder : DraftSummary -> Be.Encoder
draftSummaryEncoder { subject, to, time, id } =
    Be.sequence
        [ stringEncoder subject
        , stringEncoder to
        , int64Encoder time
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
    -> List DraftSummary
updateDraftsSummary draft maybeOldSummary =
    case maybeOldSummary of
        Nothing ->
            [ summarizeDraft draft ]

        Just oldSummary ->
            case List.partition (\d -> d.id == draft.id) oldSummary of
                ( [], _ ) ->
                    summarizeDraft draft :: oldSummary

                ( _ :: _, remainder ) ->
                    summarizeDraft draft :: remainder


summarizeDraft : Draft -> DraftSummary
summarizeDraft { subject, to, time, id } =
    { subject = subject
    , to = to
    , time = time
    , id = id
    }


draftDecoder : Bd.Decoder Draft
draftDecoder =
    map7 Draft
        stringDecoder
        stringDecoder
        stringDecoder
        timeDecoder
        stringDecoder
        (maybeDecoder codeDecoder)
        (list blobDecoder)


maybeDecoder : Bd.Decoder a -> Bd.Decoder (Maybe a)
maybeDecoder dec =
    Bd.andThen
        (\indicator ->
            case indicator of
                0 ->
                    Bd.succeed Nothing

                1 ->
                    Bd.map Just dec

                _ ->
                    Bd.fail
        )
        Bd.unsignedInt8


draftEncoder : Draft -> Be.Encoder
draftEncoder { id, subject, to, time, userInput, code, blobs } =
    Be.sequence
        [ stringEncoder id
        , stringEncoder subject
        , stringEncoder to
        , timeEncoder time
        , stringEncoder userInput
        , maybeCodeEncoder code
        , listEncoder blobEncoder blobs
        ]


timeEncoder : Int -> Be.Encoder
timeEncoder t =
    stringEncoder <| String.fromInt t


timeDecoder : Bd.Decoder Int
timeDecoder =
    Bd.andThen timeDecoderHelp stringDecoder


timeDecoderHelp : String -> Bd.Decoder Int
timeDecoderHelp s =
    case String.toInt s of
        Nothing ->
            Bd.fail

        Just i ->
            Bd.succeed i


maybeCodeEncoder : Maybe Code -> Be.Encoder
maybeCodeEncoder maybeCode =
    case maybeCode of
        Nothing ->
            Be.unsignedInt8 0

        Just { contents, filename } ->
            Be.sequence
                [ Be.unsignedInt8 1
                , bytesEncoder contents
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


stringEncoder : String -> Be.Encoder
stringEncoder s =
    Be.sequence
        [ Be.unsignedInt32 Bytes.LE <| Be.getStringWidth s
        , Be.string s
        ]


type FromServer
    = MyNameW String
    | KeysForNameW String TheirKeys
    | PowInfoW PowInfo
    | AuthCodeW Bytes.Bytes
    | NoInternetW String


fromServerDecoder : Bd.Decoder FromServer
fromServerDecoder =
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

                    5 ->
                        Bd.map NoInternetW stringDecoder

                    _ ->
                        Bd.fail
            )


keysForNameDecoder : Bd.Decoder FromServer
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


type ProcessTick
    = FinishedT Model (Cmd Msg)
    | ContinuingT Model (Cmd Msg) Process
    | NotUsedMessageT


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


int64Encoder : Int -> Be.Encoder
int64Encoder i =
    stringEncoder <| String.fromInt i


int64Decoder : Bd.Decoder Int
int64Decoder =
    Bd.andThen int64DecoderHelp stringDecoder


int64DecoderHelp : String -> Bd.Decoder Int
int64DecoderHelp s =
    case String.toInt s of
        Nothing ->
            Bd.fail

        Just i ->
            if i < 0 then
                Bd.fail

            else
                Bd.succeed i


codeDecoder : Bd.Decoder Code
codeDecoder =
    Bd.map2 Code
        bytesDecoder
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


map7 :
    (a -> b -> c -> d -> e -> f -> g -> result)
    -> Bd.Decoder a
    -> Bd.Decoder b
    -> Bd.Decoder c
    -> Bd.Decoder d
    -> Bd.Decoder e
    -> Bd.Decoder f
    -> Bd.Decoder g
    -> Bd.Decoder result
map7 func decA decB decC decD decE decF decG =
    Bd.map func decA
        |> Bd.andThen (dmap decB)
        |> Bd.andThen (dmap decC)
        |> Bd.andThen (dmap decD)
        |> Bd.andThen (dmap decE)
        |> Bd.andThen (dmap decF)
        |> Bd.andThen (dmap decG)


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


badDraft : Maybe String
badDraft =
    Just "bad bytes in draft from cache"


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ jsToElm JsonFromJsM
        , Browser.Events.onResize <|
            \w _ -> NewWindowWidthM w
        ]

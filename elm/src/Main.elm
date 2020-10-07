port module Main exposing (main)

import Base64.Decode as B64d
import Base64.Encode as B64e
import Browser
import Browser.Events
import Bytes
import Bytes.Decode as Bd
import Bytes.Encode as Be
import Element as E
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Ei
import File
import File.Download as Download
import File.Select as Select
import Html
import Http
import Json.Decode as Jd
import Json.Encode as Je
import MessageIdMap as Mid
import Task
import Time
import UserIdMap as Uid


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Blob =
    { id : String
    , mime : String
    , filename : String
    , size : Int
    }


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
    , shareBox : String
    }


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
    , shareBox = ""
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
    case ( model.timeZone, model.page ) of
        ( Nothing, _ ) ->
            E.text "error: no time zone"

        ( Just zone, Messages ) ->
            summaryView zone model.whitelist model.summaries

        ( Just zone, Writer { wasm, snapshot, summaries, shareSearch } ) ->
            E.column []
                [ viewSnapshot
                    zone
                    snapshot
                    model.whitelist
                    shareSearch
                , case wasm of
                    Nothing ->
                        Ei.button []
                            { onPress = Just UploadCodeM
                            , label = E.text "Choose WASM"
                            }

                    Just justWasm ->
                        E.column []
                            [ wasmView justWasm
                            , replaceWasm
                            ]
                , viewSummaries zone model.whitelist summaries
                ]

        ( _, Contacts ) ->
            case Uid.toList model.whitelist of
                Nothing ->
                    E.text "could not decode contacts"

                Just whitelist ->
                    E.column [] <|
                        List.map contactView whitelist

        ( Just _, Account ) ->
            E.text "Account page goes here."


contactView : ( Uid.Username, Uid.Fingerprint ) -> E.Element Msg
contactView ( username, fingerprint ) =
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
    Time.Zone
    -> Uid.UserIdMap
    -> List SnapSummary
    -> E.Element Msg
viewSummaries zone userIds summaries =
    E.column [] <| List.map (viewSnapSummary zone userIds) summaries


viewSnapSummary :
    Time.Zone
    -> Uid.UserIdMap
    -> SnapSummary
    -> E.Element Msg
viewSnapSummary zone whitelist { time, edit, author } =
    E.row []
        [ prettyTime time zone
        , E.text edit
        , E.text <| prettyAuthor whitelist author
        ]


summaryView :
    Time.Zone
    -> Uid.UserIdMap
    -> Mid.MessageIdMap Summary
    -> E.Element Msg
summaryView zone whitelist summaries =
    case Mid.toList summaries of
        Nothing ->
            E.text "could not convert summaries to list"

        Just summariesList ->
            E.column [] <|
                List.map (oneSummaryLine zone whitelist) <|
                    summariesList


oneSummaryLine :
    Time.Zone
    -> Uid.UserIdMap
    -> ( Mid.MessageId, Summary )
    -> E.Element Msg
oneSummaryLine zone whitelist ( messageId, summary ) =
    Ei.button []
        { onPress = Just (SummaryClickM messageId)
        , label = summaryLabel zone whitelist summary
        }


summaryLabel :
    Time.Zone
    -> Uid.UserIdMap
    -> Summary
    -> E.Element Msg
summaryLabel zone whitelist summary =
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


normalTextSize : Int
normalTextSize =
    25


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


ubuntu : E.Attribute Msg
ubuntu =
    Font.family [ Font.typeface "Ubuntu" ]


ubuntuMono : E.Attribute Msg
ubuntuMono =
    Font.family [ Font.typeface "Ubuntu Mono" ]


userInputView : String -> E.Element Msg
userInputView userInput =
    E.el [ ubuntuMono ] <|
        Ei.multiline []
            { onChange = NewMainM
            , text = userInput
            , placeholder = Nothing
            , spellcheck = False
            , label =
                Ei.labelAbove [] <|
                    E.text "Main box"
            }


blobsView : List Blob -> E.Element Msg
blobsView blobs =
    E.column []
        [ E.column [ E.spacing 10 ] <| List.map blobView blobs
        , uploadBlob
        ]


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
        , Ei.button []
            { onPress = Just <| DeleteBlobM blob
            , label = E.text "Delete"
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


replaceWasm : E.Element Msg
replaceWasm =
    Ei.button []
        { onPress = Just UploadCodeM
        , label = E.text "Replace WASM"
        }


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


white : E.Color
white =
    E.rgb255 255 255 255


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

        MessagesB ->
            MessagesM

        AccountB ->
            AccountM


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

        MessagesB ->
            "Documents"

        AccountB ->
            "Account"

        WriterB ->
            "Writer"


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
        ( Messages, MessagesB ) ->
            True

        _ ->
            False


port elmToJs : Je.Value -> Cmd msg


port jsToElm : (Je.Value -> msg) -> Sub msg


jsKeyVal : String -> Je.Value -> Je.Value
jsKeyVal key value =
    Je.object [ ( "key", Je.string key ), ( "value", value ) ]


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
                    , ( "msgId", Je.string id )
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
                    , ( "id", Je.string id )
                    ]


type ToBackend
    = NewMain String
    | NewTo String
    | MessagesClick
    | WriterClick
    | ContactsClick
    | AccountClick
    | DeleteBlob String
    | DeleteContact Uid.Username
    | SummaryClick Mid.MessageId


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
            Be.sequence [ Be.unsignedInt8 4, Be.string m ]

        NewTo to ->
            Be.sequence [ Be.unsignedInt8 9, Be.string to ]

        DeleteBlob id ->
            Be.sequence [ Be.unsignedInt8 11, Be.string id ]

        DeleteContact toDelete ->
            Be.sequence
                [ Be.unsignedInt8 12
                , encodeUsername toDelete
                ]

        SummaryClick messageId ->
            Be.sequence
                [ Be.unsignedInt8 13
                , encodeMessageId messageId
                ]


encodeMessageId : Mid.MessageId -> Be.Encoder
encodeMessageId (Mid.MessageId m) =
    Be.bytes m


encodeUsername : Uid.Username -> Be.Encoder
encodeUsername (Uid.Username u) =
    Be.bytes u


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
    | NewMainM String
    | NewToM String
    | DeleteContactM Uid.Username
    | UploadedM (Result Http.Error ())
    | FromBackendM Bytes.Bytes
    | SummaryClickM Mid.MessageId
    | BadWasmM String
    | NoBackendM
    | BlobSelectedM File.File
    | CodeSelectedM File.File
    | WasmOutputM String Wasm
    | JsonFromJsM Je.Value
    | ContactsM
    | WriteM
    | AccountM
    | MessagesM
    | NewWindowWidthM Int
    | DownloadCodeM Code
    | DownloadBlobM Blob
    | UploadCodeM
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


showB64Error : B64d.Error -> String
showB64Error error =
    case error of
        B64d.ValidationError ->
            "Validation error"

        B64d.InvalidByteSequence ->
            "Invalid byte sequence"


localUrl : String
localUrl =
    "http://localhost:17448"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WriteM ->
            ( model
            , WriterClick
                |> ToBackendE
                |> encodeToJs
                |> elmToJs
            )

        ContactsM ->
            ( model
            , ContactsClick
                |> ToBackendE
                |> encodeToJs
                |> elmToJs
            )

        NewWindowWidthM width ->
            ( { model | windowWidth = width }, Cmd.none )

        DownloadCodeM { contents, filename } ->
            ( model
            , Download.bytes filename "application/wasm" contents
            )

        WasmOutputM newWasmId wasm ->
            case ( model.page, model.lastWasmId ) of
                ( Writer _, Nothing ) ->
                    ( model, Cmd.none )

                ( Writer w, Just lastWasmId ) ->
                    if newWasmId /= lastWasmId then
                        ( model, Cmd.none )

                    else
                        ( { model
                            | page = Writer { w | wasm = Just wasm }
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

        UploadCodeM ->
            ( model
            , Select.file
                [ "application/wasm" ]
                CodeSelectedM
            )

        CodeSelectedM file ->
            ( model, cacheCode file )

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
            ( model, cacheBlob file )

        DownloadBlobM { id } ->
            ( model, Download.url <| localUrl ++ "/" ++ id )

        UploadBlobM ->
            ( model
            , Select.file
                [ "application/octet-stream" ]
                BlobSelectedM
            )

        NonsenseFromBackendM rawNonsense ->
            ( { model
                | fatal =
                    Just <|
                        "bad messeage from backend: "
                            ++ bytesToString rawNonsense
              }
            , Cmd.none
            )

        DeleteBlobM { id } ->
            ( model
            , DeleteBlob id
                |> ToBackendE
                |> encodeToJs
                |> elmToJs
            )

        DeleteContactM username ->
            ( model
            , DeleteContact username
                |> ToBackendE
                |> encodeToJs
                |> elmToJs
            )

        ShareBoxContentsM box ->
            ( { model | shareBox = box }, Cmd.none )

        AccountM ->
            ( model
            , AccountClick |> ToBackendE |> encodeToJs |> elmToJs
            )

        MessagesM ->
            ( model
            , MessagesClick |> ToBackendE |> encodeToJs |> elmToJs
            )

        NewMainM s ->
            updateOnUserInput s model

        NewToM t ->
            ( model
            , NewTo t
                |> ToBackendE
                |> encodeToJs
                |> elmToJs
            )

        SummaryClickM messageId ->
            ( model
            , SummaryClick messageId
                |> ToBackendE
                |> encodeToJs
                |> elmToJs
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


summaryD : Bd.Decoder ( Mid.MessageId, Summary )
summaryD =
    Bd.map2 (\m s -> ( m, s ))
        messageIdD
        (Bd.map3 Summary
            stringDecoder
            timeD
            usernameD
        )


timeD : Bd.Decoder Time.Posix
timeD =
    Bd.map
        (Time.millisToPosix << (*) 1000)
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


type alias Snapshot =
    { time : Time.Posix
    , shares : List Uid.Username
    , mainBox : String
    , blobs : List Blob
    , wasm : Maybe Code
    }


viewSnapshot :
    Time.Zone
    -> Snapshot
    -> Uid.UserIdMap
    -> String
    -> E.Element Msg
viewSnapshot zone { time, shares, mainBox, blobs, wasm } userIds shareSearch =
    E.column []
        [ prettyTime time zone
        , addShare shareSearch
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


addShare : String -> E.Element Msg
addShare contents =
    E.row []
        [ addShareBox contents
        , addShareButton contents
        ]


addShareButton : String -> E.Element Msg
addShareButton contents =
    Ei.button []
        { onPress = Just <| NewToM contents
        , label = E.text "Add share"
        }


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
                    { left = 0, right = 7, bottom = 0, top = 0 }
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
                    ( model, Cmd.none )

                Just code ->
                    case model.lastWasmId of
                        Nothing ->
                            ( { model
                                | lastWasmId =
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
                                                userInput
                                            )
                                    , wasmCode = code.contents
                                    , id = String.fromInt model.counter
                                    }
                                    |> encodeToJs
                                    |> elmToJs
                                , NewMain userInput
                                    |> ToBackendE
                                    |> encodeToJs
                                    |> elmToJs
                                ]
                            )

                        Just lastWasmId ->
                            ( model
                            , Cmd.batch
                                [ RerunWasmE
                                    { userInput =
                                        Be.encode (Be.string userInput)
                                    , id = lastWasmId
                                    }
                                    |> encodeToJs
                                    |> elmToJs
                                , NewMain userInput
                                    |> ToBackendE
                                    |> encodeToJs
                                    |> elmToJs
                                ]
                            )

        _ ->
            ( model, Cmd.none )


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


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ jsToElm JsonFromJsM
        , Browser.Events.onResize <|
            \w _ -> NewWindowWidthM w
        ]

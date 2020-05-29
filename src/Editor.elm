port module Editor exposing
    ( Model
    , Msg
    , initCmd
    , initModel
    , subscriptions
    , update
    , view
    )

import Base64.Decode
import Base64.Encode
import Bytes
import Bytes.Decode as D
import Dict
import Element
import Element.Font as Font
import Element.Input
import File
import File.Select as Select
import Json.Decode as Jd
import Json.Encode as Je
import Set
import Task


type alias Message =
    { from : Int
    , to : Int
    , time : Int
    , subject : String
    , userInput : String
    , code : Code
    , blobs : Dict.Dict String String
    }


type alias Draft =
    { to : Maybe Int
    , time : Int
    , subject : String
    , userInput : String
    , code : Maybe Code
    , blobs : Dict.Dict String String
    }


type alias Model =
    { myName : Maybe Int
    , myContacts : Set.Set Int
    , sendToBox : Maybe Int
    , addContactBox : Maybe Int
    , addContactError : Maybe String
    , messages : Dict.Dict String Message
    , opened : Opened
    , drafts : Dict.Dict String Draft
    , internalError : Maybe String
    , wasmError : Maybe String
    , outbox : List Message
    , sendMessageError : Maybe String
    , currentSendingId : Maybe Int
    }


type Opened
    = OMessage OpenedMessage
    | ODraft OpenedDraft
    | Neither


type alias OpenedMessage =
    { id : String
    , document : Maybe Document
    }


type Document
    = Ordering (List Document)
    | SmallString String


type alias OpenedDraft =
    { id : String
    , document : Maybe Document
    }


port elmToJs : Je.Value -> Cmd msg


port jsToElm : (Je.Value -> msg) -> Sub msg


type FromJs
    = UpdatedDraft String Draft
    | UpdatedMessages (Dict.Dict String Message)
    | WasmOutput String Bytes.Bytes
    | SendError String
    | InternalError String


type ToJs
    = TupdatedUserInput { id : String, userInput : String }
    | TupdatedRecipient { id : String, recipient : Int }
    | TupdatedSubject { id : String, subject : String }
    | TnewCode { code : Bytes.Bytes, filename : String }
    | TrequestBlob String
    | TmakeNewDraft
    | TdeleteBlob { blobId : String, draftId : String }
    | TaddNewContact Int
    | TrunDraftWasm String
    | TrunMessageWasm String
    | TsendDraft String
    | TnewBlob LoadedBlob


kvHelp : String -> List ( String, Je.Value ) -> Je.Value
kvHelp key value =
    Je.object
        [ ( "key", Je.string key )
        , ( "value", Je.object value )
        ]


sendToJs : ToJs -> Cmd msg
sendToJs toJs =
    elmToJs <|
        case toJs of
            TupdatedUserInput { id, userInput } ->
                kvHelp "updatedUserInput"
                    [ ( "id", Je.string id )
                    , ( "userInput", Je.string userInput )
                    ]

            TupdatedRecipient { id, recipient } ->
                kvHelp "updatedRecipient"
                    [ ( "id", Je.string id )
                    , ( "recipient", Je.int recipient )
                    ]

            TupdatedSubject { id, subject } ->
                kvHelp "updatedSubject"
                    [ ( "id", Je.string id )
                    , ( "subject", Je.string subject )
                    ]

            TnewCode { code, filename } ->
                kvHelp "newCode"
                    [ ( "code"
                      , Je.string <|
                            Base64.Encode.encode <|
                                Base64.Encode.bytes code
                      )
                    , ( "filename", Je.string filename )
                    ]

            TrequestBlob blobId ->
                kvHelp "requestBlob"
                    [ ( "id", Je.string blobId )
                    ]

            TmakeNewDraft ->
                kvHelp "makeNewDraft" []

            TdeleteBlob { blobId, draftId } ->
                kvHelp "deleteBlob"
                    [ ( "blobId", Je.string blobId )
                    , ( "draftId", Je.string draftId )
                    ]

            TaddNewContact contact ->
                kvHelp "addNewContact"
                    [ ( "contact", Je.int contact )
                    ]

            TrunDraftWasm draftId ->
                kvHelp "runDraftWasm"
                    [ ( "draftId", Je.string draftId )
                    ]

            TrunMessageWasm messageId ->
                kvHelp "runMessageWasm"
                    [ ( "messageId", Je.string messageId )
                    ]

            TsendDraft draftId ->
                kvHelp "sendDraft"
                    [ ( "draftId", Je.string draftId )
                    ]

            TnewBlob { fileName, draftId, contents } ->
                kvHelp "newBlob"
                    [ ( "fileName", Je.string fileName )
                    , ( "draftId", Je.string draftId )
                    , ( "contents"
                      , Je.string <|
                            Base64.Encode.encode <|
                                Base64.Encode.bytes contents
                      )
                    ]


subscriptions : Sub Msg
subscriptions =
    jsToElm RawFromJs


initCmd : Cmd Msg
initCmd =
    Cmd.none


initModel : Model
initModel =
    { myName = Nothing
    , myContacts = Set.empty
    , addContactBox = Nothing
    , addContactError = Nothing
    , internalError = Nothing
    , sendToBox = Nothing
    , sendMessageError = Nothing
    , drafts = Dict.empty
    , outbox = []
    , wasmError = Nothing
    , messages = Dict.empty
    , opened = Neither
    , currentSendingId = Nothing
    }


type Msg
    = AddNewContact
    | DeleteBlob String
    | UpdatedUserInput String
    | UpdatedSubject String
    | RawFromJs Je.Value
    | ClickAddProgramButton
    | UpdateContactBox String
    | CloseMessage
    | CloseDraft
    | UpdatedRecipient { id : String, rawRecipient : String }
    | OpenDraft String
    | OpenMessage String
    | MakeNewDraft
    | SendDraft String
    | ClickAddBlobButton String
    | BlobSelected String File.File
    | BlobLoaded LoadedBlob
    | DownloadFile String
    | ProgramSelected File.File
    | ProgramLoaded String Bytes.Bytes


type alias LoadedBlob =
    { fileName : String
    , draftId : String
    , contents : Bytes.Bytes
    }


decodeDocument : D.Decoder Document
decodeDocument =
    D.andThen decodeDocumentHelp D.unsignedInt8


decodeOrdering : D.Decoder Document
decodeOrdering =
    D.map Ordering (list decodeDocument)


showB64Error : Base64.Decode.Error -> String
showB64Error error =
    case error of
        Base64.Decode.ValidationError ->
            "validation error"

        Base64.Decode.InvalidByteSequence ->
            "invalid byte sequence"


sizedString : D.Decoder String
sizedString =
    D.unsignedInt32 Bytes.LE
        |> D.andThen D.string


{-| Pinched from the Bytes documentation.
-}
listStep :
    D.Decoder a
    -> ( Int, List a )
    -> D.Decoder (D.Step ( Int, List a ) (List a))
listStep decoder ( n, xs ) =
    if n <= 0 then
        D.succeed (D.Done xs)

    else
        D.map (\x -> D.Loop ( n - 1, x :: xs )) decoder


{-| Pinched from the Bytes documentation.
-}
list : D.Decoder a -> D.Decoder (List a)
list decoder =
    D.unsignedInt32 Bytes.LE
        |> D.andThen
            (\len -> D.loop ( len, [] ) (listStep decoder))


decodeDocumentHelp : Int -> D.Decoder Document
decodeDocumentHelp typeNum =
    case typeNum of
        0 ->
            decodeOrdering

        1 ->
            D.map SmallString sizedString

        _ ->
            D.fail


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DownloadFile blobId ->
            ( model
            , sendToJs <| TrequestBlob blobId
            )

        ClickAddProgramButton ->
            ( model
            , Select.file
                [ "application/wasm" ]
                ProgramSelected
            )

        ProgramSelected file ->
            ( model
            , Task.perform
                (ProgramLoaded (File.name file))
              <|
                File.toBytes file
            )

        ProgramLoaded filename fileBytes ->
            ( model
            , sendToJs <|
                TnewCode { code = fileBytes, filename = filename }
            )

        ClickAddBlobButton id ->
            ( model
            , Select.file
                [ "application/octet-stream" ]
                (BlobSelected id)
            )

        BlobSelected id file ->
            ( model
            , Task.perform (\c -> BlobLoaded { fileName = File.name file, draftId = id, contents = c }) <|
                File.toBytes file
            )

        AddNewContact ->
            addNewContactHelp model

        UpdateContactBox candidate ->
            case String.toInt candidate of
                Nothing ->
                    if candidate == "" then
                        ( { model | addContactBox = Nothing }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Just number ->
                    if number < 0 then
                        ( model, Cmd.none )

                    else
                        let
                            newModel =
                                { model | addContactBox = Just number }
                        in
                        ( newModel, Cmd.none )

        CloseMessage ->
            ( { model
                | opened = Neither
              }
            , Cmd.none
            )

        CloseDraft ->
            ( { model | opened = Neither }, Cmd.none )

        UpdatedRecipient { id, rawRecipient } ->
            ( model
            , case String.toInt rawRecipient of
                Nothing ->
                    Cmd.none

                Just recipient ->
                    sendToJs <|
                        TupdatedRecipient
                            { id = id, recipient = recipient }
            )

        MakeNewDraft ->
            ( model, sendToJs TmakeNewDraft )

        DeleteBlob blobId ->
            case model.opened of
                OMessage _ ->
                    ( model, Cmd.none )

                ODraft { id } ->
                    ( model, sendToJs <| TdeleteBlob { blobId = blobId, draftId = id } )

                Neither ->
                    ( model, Cmd.none )

        UpdatedUserInput updated ->
            case model.opened of
                ODraft { id } ->
                    ( model
                    , sendToJs <|
                        TupdatedUserInput
                            { id = id, userInput = updated }
                    )

                OMessage _ ->
                    ( model, Cmd.none )

                Neither ->
                    ( model, Cmd.none )

        UpdatedSubject updated ->
            case model.opened of
                ODraft { id } ->
                    ( model
                    , sendToJs <|
                        TupdatedSubject
                            { id = id, subject = updated }
                    )

                OMessage _ ->
                    ( model, Cmd.none )

                Neither ->
                    ( model, Cmd.none )

        RawFromJs json ->
            processRawFromJs json model

        OpenDraft id ->
            case Dict.get id model.drafts of
                Nothing ->
                    ( model, Cmd.none )

                Just { code } ->
                    ( { model | opened = ODraft { id = id, document = Nothing } }
                    , case code of
                        Nothing ->
                            Cmd.none

                        Just _ ->
                            sendToJs <| TrunDraftWasm id
                    )

        OpenMessage id ->
            case Dict.get id model.messages of
                Nothing ->
                    ( model, Cmd.none )

                Just _ ->
                    ( { model | opened = OMessage { id = id, document = Nothing } }
                    , sendToJs <| TrunMessageWasm id
                    )

        SendDraft id ->
            ( model, sendToJs <| TsendDraft id )

        BlobLoaded loaded ->
            ( model, sendToJs <| TnewBlob loaded )


processRawFromJs : Je.Value -> Model -> ( Model, Cmd Msg )
processRawFromJs json model =
    case Jd.decodeValue fromJsDecoder json of
        Err err ->
            ( { model | internalError = Just <| Jd.errorToString err }
            , Cmd.none
            )

        Ok fromJs ->
            updateFromJs model fromJs


type alias RawFromJsT =
    { key : String
    , value : String
    }


rawFromJsDecoder : Jd.Decoder RawFromJsT
rawFromJsDecoder =
    Jd.map2 RawFromJsT
        (Jd.field "key" Jd.string)
        (Jd.field "value" Jd.string)


fromJsDecoder : Jd.Decoder FromJs
fromJsDecoder =
    Jd.andThen fromJsDecoderHelp rawFromJsDecoder


fromJsDecoderHelp : RawFromJsT -> Jd.Decoder FromJs
fromJsDecoderHelp { key, value } =
    case key of
        "sendError" ->
            Jd.map SendError Jd.string

        "updatedDraft" ->
            updatedDraftDecoder

        "updatedMessages" ->
            messagesDecoder

        "wasmOutput" ->
            decodeWasmResult value

        unknown ->
            Jd.fail <| "unknown key: " ++ unknown


type alias JsonWasmOutput =
    { draftId : String
    , wasmOutput : String
    }


decodeWasmResult : String -> Jd.Decoder FromJs
decodeWasmResult json =
    case Jd.decodeString jsonWasmOutputDecoder json of
        Err err ->
            Jd.fail <| Jd.errorToString err

        Ok { draftId, wasmOutput } ->
            case Base64.Decode.decode Base64.Decode.bytes wasmOutput of
                Err err ->
                    Jd.fail <| showB64Error err

                Ok wasmBytes ->
                    Jd.succeed <|
                        WasmOutput draftId wasmBytes


jsonWasmOutputDecoder : Jd.Decoder JsonWasmOutput
jsonWasmOutputDecoder =
    Jd.map2 JsonWasmOutput Jd.string Jd.string


messagesDecoder : Jd.Decoder FromJs
messagesDecoder =
    Jd.map UpdatedMessages <| Jd.dict messageDecoder


updatedDraftDecoder : Jd.Decoder FromJs
updatedDraftDecoder =
    Jd.map2 UpdatedDraft
        (Jd.field "id" Jd.string)
        (Jd.field "draft" draftDecoder)


messageDecoder : Jd.Decoder Message
messageDecoder =
    Jd.map7 Message
        (Jd.field "from" Jd.int)
        (Jd.field "to" Jd.int)
        (Jd.field "time" Jd.int)
        (Jd.field "subject" Jd.string)
        (Jd.field "userInput" Jd.string)
        (Jd.field "code" codeDecoder)
        (Jd.field "blobs" <| Jd.dict Jd.string)


draftDecoder : Jd.Decoder Draft
draftDecoder =
    Jd.map6 Draft
        (Jd.maybe <| Jd.field "to" Jd.int)
        (Jd.field "time" Jd.int)
        (Jd.field "subject" Jd.string)
        (Jd.field "userInput" Jd.string)
        (Jd.maybe <| Jd.field "code" codeDecoder)
        (Jd.field "blobs" <| Jd.dict Jd.string)


type alias Code =
    { fileName : String
    , blobId : String
    }


codeDecoder : Jd.Decoder Code
codeDecoder =
    Jd.map2 Code Jd.string Jd.string


updateFromJs : Model -> FromJs -> ( Model, Cmd Msg )
updateFromJs model fromJs =
    case fromJs of
        UpdatedDraft id draft ->
            ( {model | drafts = Dict.insert id draft model.drafts }
            , Cmd.none
            )

        UpdatedMessages messages ->
            ( { model | messages = messages }, Cmd.none )

        SendError error ->
            ( { model | sendMessageError = Just error }, Cmd.none )

        WasmOutput id output ->
            case
                ( model.opened
                , D.decode decodeDocument output
                )
            of
                ( Neither, _ ) ->
                    ( model, Cmd.none )

                ( ODraft draft, Just document ) ->
                    if draft.id /= id then
                        ( model, Cmd.none )

                    else
                        ( { model
                            | opened =
                                ODraft
                                    { draft
                                        | document = Just document
                                    }
                          }
                        , Cmd.none
                        )

                ( _, Nothing ) ->
                    ( { model | wasmError = Just "decoding error" }, Cmd.none )

                ( OMessage message, Just document ) ->
                    if message.id /= id then
                        ( model, Cmd.none )

                    else
                        ( { model
                            | opened =
                                OMessage
                                    { message
                                        | document = Just document
                                    }
                          }
                        , Cmd.none
                        )


fontSize : Element.Attribute msg
fontSize =
    Font.size 25


view : Model -> Element.Element Msg
view model =
    case model.internalError of
        Just err ->
            Element.text <| "internal error: " ++ err

        Nothing ->
            Element.column [ Element.spacing 20, fontSize ]
                [ myUsernameIs model.myName
                , myContactsAre <| Set.toList model.myContacts
                , addNewContact model.addContactBox model.addContactError
                , viewMessages model.messages model.opened
                , makeNewDraft
                , editDrafts model.wasmError model.opened model.drafts
                ]


viewMessages : Dict.Dict String Message -> Opened -> Element.Element Msg
viewMessages messages opened =
    case opened of
        OMessage { id, document } ->
            case Dict.get id messages of
                Nothing ->
                    messageChooser messages

                Just message ->
                    viewMessage message { id = id, document = document }

        _ ->
            messageChooser messages


messageChooser : Dict.Dict String Message -> Element.Element Msg
messageChooser messages =
    Element.column [] <|
        List.map messageChooserButton <|
            Dict.toList messages


messageChooserButton : ( String, Message ) -> Element.Element Msg
messageChooserButton ( id, message ) =
    Element.Input.button []
        { onPress = Just <| OpenMessage id
        , label = messageChooserLabel message
        }


messageChooserLabel : Message -> Element.Element Msg
messageChooserLabel message =
    Element.column []
        [ Element.text <| "Subject: " ++ message.subject
        , Element.text <| "From: " ++ String.fromInt message.from
        ]


viewMessage : Message -> OpenedMessage -> Element.Element Msg
viewMessage message { document } =
    Element.column []
        [ closeMessage
        , Element.text <| "From: " ++ String.fromInt message.from
        , Element.text <| "Subject: " ++ message.subject
        , Element.text "User input:"
        , Element.paragraph [] [ Element.text message.userInput ]
        , case document of
            Nothing ->
                Element.none

            Just d ->
                displayDocument d
        , viewBlobs message.blobs
        ]


viewBlobs : Dict.Dict String String -> Element.Element Msg
viewBlobs blobs =
    Element.column [] <|
        List.map viewBlob <|
            Dict.toList blobs


viewBlob : ( String, String ) -> Element.Element Msg
viewBlob ( blobId, blobName ) =
    Element.row []
        [ Element.text blobName
        , Element.Input.button []
            { onPress = Just <| DownloadFile blobId
            , label = Element.text "Download"
            }
        ]


closeDraft : Element.Element Msg
closeDraft =
    Element.Input.button []
        { onPress = Just <| CloseDraft
        , label = Element.text "Back to drafts"
        }


closeMessage : Element.Element Msg
closeMessage =
    Element.Input.button []
        { onPress = Just <| CloseMessage
        , label = Element.text "Back to inbox"
        }


editDrafts : Maybe String -> Opened -> Dict.Dict String Draft -> Element.Element Msg
editDrafts wasmError maybeOpenDraft drafts =
    case maybeOpenDraft of
        ODraft openDraft ->
            case Dict.get openDraft.id drafts of
                Nothing ->
                    draftChooser drafts

                Just draft ->
                    editDraft draft wasmError openDraft

        _ ->
            draftChooser drafts


editDraft : Draft -> Maybe String -> OpenedDraft -> Element.Element Msg
editDraft draft wasmError { id, document } =
    Element.column []
        [ closeDraft
        , toBox id draft
        , subjectBox draft
        , userInputBox draft
        , addProgramButton
        , showProgram draft.code
        , case wasmError of
            Nothing ->
                Element.none

            Just err ->
                Element.el [ monospace ] <| Element.text err
        , addBlobButton id
        , editBlobs draft.blobs
        , sendDraft id
        , case document of
            Nothing ->
                Element.none

            Just d ->
                displayDocument d
        ]


showProgram : Maybe Code -> Element.Element Msg
showProgram code =
    case code of
        Nothing ->
            Element.none

        Just { fileName } ->
            Element.text fileName


editBlobs : Dict.Dict String String -> Element.Element Msg
editBlobs blobs =
    Element.column [] <|
        List.map editBlob <|
            Dict.toList blobs


editBlob : ( String, String ) -> Element.Element Msg
editBlob ( blobId, blobName ) =
    Element.row []
        [ Element.text blobName
        , Element.Input.button []
            { onPress = Just <| DownloadFile blobId
            , label = Element.text "Download"
            }
        , Element.Input.button []
            { onPress = Just <| DeleteBlob blobId
            , label = Element.text "Delete"
            }
        ]


sendDraft : String -> Element.Element Msg
sendDraft id =
    Element.Input.button []
        { onPress = Just <| SendDraft id
        , label = Element.text "Send message"
        }


toBox : String -> Draft -> Element.Element Msg
toBox id draft =
    Element.Input.text []
        { onChange = \r -> UpdatedRecipient { id = id, rawRecipient = r }
        , text =
            case draft.to of
                Nothing ->
                    ""

                Just recipient ->
                    String.fromInt recipient
        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type the recipient ID here"
        , label = Element.Input.labelAbove [] <| Element.text "To:"
        }


addNewContactHelp : Model -> ( Model, Cmd Msg )
addNewContactHelp model =
    case model.addContactBox of
        Nothing ->
            ( model, Cmd.none )

        Just newContact ->
            if Set.member newContact model.myContacts then
                ( { model | addContactError = Just "Already a contact" }, Cmd.none )

            else
                ( model, sendToJs <| TaddNewContact newContact )


subjectBox : Draft -> Element.Element Msg
subjectBox draft =
    Element.Input.text []
        { onChange = UpdatedSubject
        , text = draft.subject
        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type the subject"
        , label = Element.Input.labelAbove [] <| Element.text "Subject:"
        }


userInputBox : Draft -> Element.Element Msg
userInputBox draft =
    Element.Input.multiline [ monospace ]
        { onChange = UpdatedUserInput
        , text = draft.userInput
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    Element.text "Type user input here"
        , label =
            Element.Input.labelAbove [] <|
                Element.text "User input:"
        , spellcheck = True
        }


addProgramButton : Element.Element Msg
addProgramButton =
    Element.Input.button []
        { onPress = Just <| ClickAddProgramButton
        , label = Element.text "Add program"
        }


draftChooser : Dict.Dict String Draft -> Element.Element Msg
draftChooser drafts =
    Element.column [] <|
        List.map showDraftButton <|
            Dict.toList drafts


addBlobButton : String -> Element.Element Msg
addBlobButton id =
    Element.Input.button []
        { onPress = Just <| ClickAddBlobButton id
        , label = Element.text "Add file"
        }


showDraftButton : ( String, Draft ) -> Element.Element Msg
showDraftButton ( id, draft ) =
    Element.Input.button []
        { onPress = Just <| OpenDraft id
        , label =
            case draft.subject of
                "" ->
                    Element.text "no subject"

                _ ->
                    Element.text draft.subject
        }


monospace : Element.Attribute Msg
monospace =
    Font.family [ Font.typeface "Ubuntu Mono" ]


displayDocument : Document -> Element.Element Msg
displayDocument document =
    case document of
        Ordering documents ->
            Element.column [] <| List.map displayDocument documents

        SmallString s ->
            Element.el [ monospace ] <| Element.text s


makeNewDraft : Element.Element Msg
makeNewDraft =
    Element.Input.button []
        { onPress = Just MakeNewDraft
        , label = Element.text "Make new draft"
        }


addNewContact : Maybe Int -> Maybe String -> Element.Element Msg
addNewContact boxContents maybeError =
    Element.column [] <|
        [ Element.Input.text [ monospace ]
            { onChange = UpdateContactBox
            , text =
                case boxContents of
                    Just n ->
                        String.fromInt n

                    Nothing ->
                        ""
            , placeholder =
                Just <|
                    Element.Input.placeholder [ monospace ] <|
                        Element.text "Type their username"
            , label =
                Element.Input.labelAbove [ sansSerif ] <|
                    Element.text "Add someone to your contacts:"
            }
        , Element.Input.button [ sansSerif ]
            { onPress = Just AddNewContact
            , label = Element.text "Add new contact"
            }
        , case maybeError of
            Nothing ->
                Element.none

            Just error ->
                Element.text error
        ]


myContactsAre : List Int -> Element.Element Msg
myContactsAre contacts =
    Element.el [ sansSerif ] <|
        Element.text <|
            "My contacts: "
                ++ (case contacts of
                        [] ->
                            "you haven't got any yet"

                        oneOrMore ->
                            String.join ", " <| List.map String.fromInt oneOrMore
                   )


sansSerif : Element.Attribute msg
sansSerif =
    Font.family [ Font.typeface "EB Garamond" ]


myUsernameIs : Maybe Int -> Element.Element Msg
myUsernameIs maybeMyName =
    Element.el [ sansSerif ] <|
        Element.text <|
            "My username: "
                ++ (case maybeMyName of
                        Nothing ->
                            "you haven't got one yet"

                        Just nameInt ->
                            String.fromInt nameInt
                   )

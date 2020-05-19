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
import Bytes.Encode as E
import Dict
import Element
import Element.Font as Font
import Element.Input
import File
import File.Download as Download
import File.Select as Select
import Hex.Convert
import Json.Decode as Jd
import Json.Encode as Je
import Set
import Task
import Time
import Utils


type alias Model =
    { myName : Maybe Int
    , myContacts : Set.Set Int
    , sendToBox : Maybe Int
    , addContactBox : Maybe Int
    , addContactError : Maybe String
    , messages : List Utils.Message
    , opened : Opened
    , drafts : List Utils.Draft
    , internalError : Maybe String
    , compilerError : Maybe String
    , outbox : List Utils.Message
    , sendMessageError : Maybe String
    }


type Opened
    = OMessage OpenedMessage
    | ODraft OpenedDraft
    | Neither


type alias OpenedMessage =
    { message : Utils.Message
    , document : Maybe Utils.Document
    }


type alias OpenedDraft =
    { draft : Utils.Draft
    , document : Maybe Utils.Document
    }


port wasmDocumentPort : (String -> msg) -> Sub msg


port runWasmPort : Je.Value -> Cmd msg


port cacheBlobPort : String -> Cmd msg


runWasm : Bytes.Bytes -> String -> Cmd msg
runWasm wasm userInput =
    runWasmPort <|
        Je.object
            [ ( "wasm", Je.string <| Base64.Encode.encode <| Base64.Encode.bytes wasm )
            , ( "userInput", Je.string userInput )
            ]


port sendMessageError : (String -> msg) -> Sub msg


port getEditorInfo : () -> Cmd msg


port retrievedEditorInfo : (Je.Value -> msg) -> Sub msg


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ retrievedEditorInfo RetrievedEditorInfo
        , badWhitelist BadWhitelist
        , goodWhitelist GoodWhitelist
        , sendMessageError SendMessageError
        , wasmDocumentPort RawWasmDocument
        ]


initCmd : Cmd Msg
initCmd =
    getEditorInfo ()


port whitelistPort : Int -> Cmd msg


initModel : Model
initModel =
    { myName = Nothing
    , myContacts = Set.empty
    , addContactBox = Nothing
    , addContactError = Nothing
    , internalError = Nothing
    , sendToBox = Nothing
    , sendMessageError = Nothing
    , drafts = []
    , outbox = []
    , compilerError = Nothing
    , messages = []
    , opened = Neither
    }


type Msg
    = AddNewContact
    | ClickAddProgramButton OpenedDraft
    | RawWasmDocument String
    | UpdateContactBox String
    | RetrievedEditorInfo Je.Value
    | GoodWhitelist Int
    | BadWhitelist String
    | CloseMessage Utils.Message
    | CloseDraft Utils.Draft
    | UpdatedRecipient OpenedDraft String
    | UpdatedDraft OpenedDraft
    | OpenDraft Utils.Draft (List Utils.Draft)
    | OpenMessage Utils.Message (List Utils.Message)
    | MakeNewDraft
    | TimeForNewDraft Time.Posix
    | SendDraft Utils.Draft
    | SendMessageError String
    | ClickAddBlobButton OpenedDraft
    | BlobSelected OpenedDraft File.File
    | BlobLoaded String OpenedDraft Bytes.Bytes
    | DownloadFile String Bytes.Bytes
    | ProgramSelected OpenedDraft File.File
    | ProgramLoaded String OpenedDraft Bytes.Bytes


editorInfoDecoder : Jd.Decoder RawEditorInfo
editorInfoDecoder =
    Jd.map5 RawEditorInfo
        (Jd.field "myName" Jd.int)
        (Jd.field "myContacts" (Jd.list Jd.int))
        (Jd.field "inbox" (Jd.list Jd.string))
        (Jd.field "drafts" (Jd.list Jd.string))
        (Jd.field "outbox" (Jd.list Jd.string))


type alias RawEditorInfo =
    { myName : Int
    , myContacts : List Int
    , inbox : List String
    , drafts : List String
    , outbox : List String
    }


sendMessage : Utils.Message -> Cmd Msg
sendMessage message =
    let
        encodedBytes =
            E.encode <| Utils.encodeMessage message

        encodedB64 =
            Base64.Encode.encode <| Base64.Encode.bytes encodedBytes
    in
    sendMessagePort encodedB64


port sendMessagePort : String -> Cmd msg


encodeMessage : Utils.MsgOut -> E.Encoder
encodeMessage message =
    case message of
        Utils.MakeMyName ->
            E.unsignedInt8 0

        Utils.WhitelistSomeone id ->
            E.sequence
                [ E.unsignedInt8 1
                , E.unsignedInt32 Bytes.LE id
                ]

        Utils.SendThis humanMsg ->
            E.sequence
                [ E.unsignedInt8 2
                , Utils.encodeHumanMsg humanMsg
                ]


rawWasmDocumentHelp : String -> Model -> ( Model, Cmd Msg )
rawWasmDocumentHelp rawDocB64 model =
    case Base64.Decode.decode Base64.Decode.bytes rawDocB64 of
        Err err ->
            ( { model
                | internalError =
                    Just <|
                        Utils.showB64Error err
              }
            , Cmd.none
            )

        Ok rawBytes ->
            case
                ( model.opened
                , D.decode Utils.decodeDocument rawBytes
                )
            of
                ( Neither, _ ) ->
                    ( { model
                        | internalError = Just "received WASM document but no open message"
                      }
                    , Cmd.none
                    )

                ( ODraft draft, Just document ) ->
                    ( { model
                        | opened =
                            ODraft
                                { draft
                                    | document = Just document
                                }
                      }
                    , Cmd.none
                    )

                ( ODraft draft, Nothing ) ->
                    ( { model
                        | opened =
                            ODraft
                                { draft
                                    | document =
                                        Just <|
                                            Utils.SmallString "could not decode document generated by WASM"
                                }
                      }
                    , Cmd.none
                    )

                ( OMessage message, Just document ) ->
                    ( { model
                        | opened =
                            OMessage
                                { message
                                    | document = Just document
                                }
                      }
                    , Cmd.none
                    )

                ( OMessage message, Nothing ) ->
                    ( { model
                        | opened =
                            OMessage
                                { message
                                    | document =
                                        Just <|
                                            Utils.SmallString "could not decode document generated by WASM"
                                }
                      }
                    , Cmd.none
                    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RawWasmDocument rawDocB64 ->
            rawWasmDocumentHelp rawDocB64 model

        DownloadFile fileName blob ->
            ( model
            , Download.bytes fileName "application/octet-stream" blob
            )

        ClickAddProgramButton openedDraft ->
            ( model
            , Select.file
                [ "application/wasm" ]
                (ProgramSelected openedDraft)
            )

        ProgramSelected draft file ->
            ( model
            , Task.perform (ProgramLoaded (File.name file) draft) <|
                File.toBytes file
            )

        ProgramLoaded name { draft, document } fileBytes ->
            let
                newDraft =
                    { draft | code = Just ( name, fileBytes ) }

                newDrafts =
                    newDraft :: model.drafts

                newOpened =
                    { draft = newDraft
                    , document = document
                    }
            in
            ( { model | opened = ODraft newOpened }
            , cacheDrafts newDrafts
            )

        ClickAddBlobButton openedDraft ->
            ( model
            , Select.file
                [ "application/octet-stream" ]
                (BlobSelected openedDraft)
            )

        BlobSelected draft file ->
            ( model
            , Task.perform (BlobLoaded (File.name file) draft) <|
                File.toBytes file
            )

        BlobLoaded blobName { draft, document } fileBytes ->
            let
                blobs =
                    Dict.insert blobName fileBytes draft.blobs

                newDraft =
                    { draft | blobs = blobs }

                newDrafts =
                    newDraft :: model.drafts

                newOpened =
                    { draft = newDraft
                    , document = document
                    }
            in
            ( { model | opened = ODraft newOpened }
            , cacheDrafts newDrafts
            )

        SendMessageError error ->
            ( { model | sendMessageError = Just error }, Cmd.none )

        RetrievedEditorInfo jsonValue ->
            case Jd.decodeValue editorInfoDecoder jsonValue of
                Err err ->
                    ( { model
                        | internalError =
                            Just <|
                                "could not decode editor cache: "
                                    ++ Jd.errorToString err
                      }
                    , Cmd.none
                    )

                Ok raw ->
                    case ( Utils.decodeInbox raw.inbox, Utils.decodeDrafts raw.drafts, Utils.decodeInbox raw.outbox ) of
                        ( Err err, _, _ ) ->
                            ( { model
                                | internalError =
                                    Just <|
                                        "could not decode inbox: "
                                            ++ err
                              }
                            , Cmd.none
                            )

                        ( _, Err err, _ ) ->
                            ( { model
                                | internalError =
                                    Just <|
                                        "could not decode drafts: "
                                            ++ err
                              }
                            , Cmd.none
                            )

                        ( _, _, Err err ) ->
                            ( { model | internalError = Just <| "could not decode outbox: " ++ err }, Cmd.none )

                        ( Ok messages, Ok drafts, Ok outbox ) ->
                            ( { model
                                | messages = messages
                                , drafts = drafts
                                , myName = Just raw.myName
                                , myContacts = Set.fromList raw.myContacts
                                , outbox = outbox
                              }
                            , Cmd.none
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

        GoodWhitelist newContact ->
            ( { model
                | myContacts = Set.insert newContact model.myContacts
              }
            , Cmd.none
            )

        BadWhitelist error ->
            ( { model | addContactError = Just error }, Cmd.none )

        CloseMessage message ->
            ( { model
                | messages = message :: model.messages
                , opened = Neither
              }
            , Cmd.none
            )

        CloseDraft draft ->
            ( { model
                | drafts = draft :: model.drafts
                , opened = Neither
              }
            , Cmd.none
            )

        UpdatedRecipient { draft, document } rawRecipient ->
            let
                newDraft =
                    { draft | to = String.toInt rawRecipient }

                newDrafts =
                    newDraft :: model.drafts
            in
            ( { model
                | opened =
                    ODraft
                        { draft = newDraft
                        , document = document
                        }
              }
            , cacheDrafts newDrafts
            )

        UpdatedDraft { draft } ->
            let
                newDrafts =
                    draft :: model.drafts

                openDraft =
                    { draft = draft
                    , document = Nothing
                    }
            in
            ( { model | opened = ODraft openDraft }
            , Cmd.batch
                [ cacheDrafts newDrafts
                , case draft.code of
                    Nothing ->
                        Cmd.none

                    Just ( _, wasm ) ->
                        runWasm wasm draft.userInput
                ]
            )

        OpenDraft draft remainingDrafts ->
            let
                newDraft =
                    { draft = draft
                    , document = Nothing
                    }
            in
            case draft.code of
                Nothing ->
                    ( { model
                        | opened = ODraft newDraft
                        , compilerError = Just "no program"
                        , drafts = remainingDrafts
                      }
                    , Cmd.none
                    )

                Just ( _, wasm ) ->
                    ( { model
                        | opened = ODraft newDraft
                        , drafts = remainingDrafts
                      }
                    , runWasm wasm draft.userInput
                    )

        OpenMessage message remainingMessages ->
            let
                newMessage =
                    { message = message
                    , document = Nothing
                    }
            in
            ( { model
                | opened = OMessage newMessage
                , messages = remainingMessages
              }
            , runWasm (Tuple.second message.code) message.userInput
            )

        MakeNewDraft ->
            ( model, Task.perform TimeForNewDraft Time.now )

        TimeForNewDraft posixTime ->
            ( { model | opened = ODraft { draft = { to = Nothing, time = Time.posixToMillis posixTime, subject = "", userInput = "", code = Nothing, blobs = Dict.empty }, document = Nothing } }, Cmd.none )

        SendDraft draft ->
            case makeMessage draft model.myName of
                Err err ->
                    ( { model | sendMessageError = Just err }
                    , Cmd.none
                    )

                Ok message ->
                    let
                        newOutbox =
                            message :: model.outbox
                    in
                    ( { model
                        | opened = Neither
                        , outbox = message :: model.outbox
                      }
                    , Cmd.batch
                        [ sendMessage message, cacheOutbox newOutbox ]
                    )


port cacheOutboxPort : Je.Value -> Cmd msg


cacheOutbox : List Utils.Message -> Cmd Msg
cacheOutbox outbox =
    cacheOutboxPort <| encodeOutbox outbox


encodeOutbox : List Utils.Message -> Je.Value
encodeOutbox outbox =
    let
        asBytes =
            List.map (E.encode << Utils.encodeMessage) outbox

        asStrings =
            List.map
                (Base64.Encode.encode << Base64.Encode.bytes)
                asBytes
    in
    Je.list Je.string asStrings


makeMessage : Utils.Draft -> Maybe Int -> Result String Utils.Message
makeMessage { to, time, subject, userInput, code, blobs } maybeMyName =
    case ( to, maybeMyName, code ) of
        ( Nothing, _, _ ) ->
            Err "can't send because no recipient"

        ( _, Nothing, _ ) ->
            Err "can't send because you have no username"

        ( _, _, Nothing ) ->
            Err "can't send because there is no program"

        ( Just to_, Just myName, Just code_ ) ->
            Ok
                { from = myName
                , to = to_
                , time = time
                , subject = subject
                , userInput = userInput
                , code = code_
                , blobs = blobs
                }


port badWhitelist : (String -> msg) -> Sub msg


port cacheDraftsPort : Je.Value -> Cmd msg


cacheDrafts : List Utils.Draft -> Cmd msg
cacheDrafts messages =
    cacheDraftsPort <| encodeDrafts messages


encodeDrafts : List Utils.Draft -> Je.Value
encodeDrafts drafts =
    let
        asBytes =
            List.map (E.encode << Utils.encodeDraft) drafts

        asStrings =
            List.map (Base64.Encode.encode << Base64.Encode.bytes) asBytes
    in
    Je.list Je.string asStrings


port goodWhitelist : (Int -> msg) -> Sub msg


view : Model -> Element.Element Msg
view model =
    case model.internalError of
        Just err ->
            Element.text <| "internal error: " ++ err

        Nothing ->
            Element.column [ Element.spacing 20, Utils.fontSize ]
                [ myUsernameIs model.myName
                , myContactsAre <| Set.toList model.myContacts
                , addNewContact model.addContactBox model.addContactError
                , viewMessages model.messages model.opened
                , makeNewDraft
                , editDrafts model.compilerError model.opened model.drafts
                ]


viewMessages : List Utils.Message -> Opened -> Element.Element Msg
viewMessages messages opened =
    case opened of
        OMessage m ->
            viewMessage m

        _ ->
            messageChooser messages


messageChooser : List Utils.Message -> Element.Element Msg
messageChooser messages =
    Element.column [] <| List.indexedMap (messageChooserButton messages) messages


messageChooserButton : List Utils.Message -> Int -> Utils.Message -> Element.Element Msg
messageChooserButton messages index message =
    let
        before =
            List.take index messages

        after =
            List.drop (index + 1) messages
    in
    Element.Input.button []
        { onPress = Just <| OpenMessage message <| before ++ after
        , label = messageChooserLabel message
        }


messageChooserLabel : Utils.Message -> Element.Element Msg
messageChooserLabel message =
    Element.column []
        [ Element.text <| "Subject: " ++ message.subject
        , Element.text <| "From: " ++ String.fromInt message.from
        ]


viewMessage : OpenedMessage -> Element.Element Msg
viewMessage { message, document } =
    Element.column []
        [ closeMessage message
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


viewBlobs : Dict.Dict String Bytes.Bytes -> Element.Element Msg
viewBlobs blobs =
    Element.column [] <|
        List.map viewBlob <|
            Dict.toList blobs


viewBlob : ( String, Bytes.Bytes ) -> Element.Element Msg
viewBlob ( blobName, blob ) =
    Element.row []
        [ Element.text blobName
        , Element.Input.button []
            { onPress = Just <| DownloadFile blobName blob
            , label = Element.text "Download"
            }
        ]


closeDraft : Utils.Draft -> Element.Element Msg
closeDraft draft =
    Element.Input.button []
        { onPress = Just <| CloseDraft draft
        , label = Element.text "Back to drafts"
        }


closeMessage : Utils.Message -> Element.Element Msg
closeMessage message =
    Element.Input.button []
        { onPress = Just <| CloseMessage message
        , label = Element.text "Back to inbox"
        }


editDrafts : Maybe String -> Opened -> List Utils.Draft -> Element.Element Msg
editDrafts compilerError maybeOpenDraft drafts =
    case maybeOpenDraft of
        ODraft openDraft ->
            editDraft compilerError openDraft

        _ ->
            draftChooser drafts


editDraft : Maybe String -> OpenedDraft -> Element.Element Msg
editDraft compilerError draft =
    Element.column []
        [ closeDraft draft.draft
        , toBox draft
        , subjectBox draft
        , userInputBox draft
        , addProgramButton draft
        , showProgram draft.draft.code
        , case compilerError of
            Nothing ->
                Element.none

            Just err ->
                Element.el [ monospace ] <| Element.text err
        , addBlobButton draft
        , editBlobs draft
        , sendDraft draft.draft
        , case draft.document of
            Nothing ->
                Element.none

            Just d ->
                displayDocument d
        ]


showProgram : Maybe ( String, Bytes.Bytes ) -> Element.Element Msg
showProgram code =
    case code of
        Nothing ->
            Element.text "no program added"

        Just ( name, bytes ) ->
            Element.row [ Element.spacing 3 ]
                [ Element.text name
                , Element.text <| String.fromInt <| Bytes.width bytes
                ]


editBlobs : OpenedDraft -> Element.Element Msg
editBlobs { draft, document } =
    Element.column [] <|
        List.map (editBlob { draft = draft, document = document }) <|
            Dict.toList draft.blobs


editBlob : OpenedDraft -> ( String, Bytes.Bytes ) -> Element.Element Msg
editBlob { draft, document } ( blobName, blob ) =
    Element.row []
        [ Element.text blobName
        , Element.Input.button []
            { onPress = Just <| DownloadFile blobName blob
            , label = Element.text "Download"
            }
        , let
            newDraft =
                { draft | blobs = Dict.remove blobName draft.blobs }
          in
          Element.Input.button []
            { onPress = Just <| UpdatedDraft { draft = newDraft, document = document }
            , label = Element.text "Delete"
            }
        ]


sendDraft : Utils.Draft -> Element.Element Msg
sendDraft draft =
    Element.Input.button []
        { onPress = Just <| SendDraft draft
        , label = Element.text "Send message"
        }


toBox : OpenedDraft -> Element.Element Msg
toBox opened =
    Element.Input.text []
        { onChange = UpdatedRecipient opened
        , text =
            case opened.draft.to of
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
                case model.myName of
                    Nothing ->
                        ( { model | addContactError = Just "No username" }, Cmd.none )

                    Just myName ->
                        if myName == newContact then
                            ( { model | addContactError = Just "You tried to add yourself" }, Cmd.none )

                        else
                            let
                                newModel =
                                    { model
                                        | addContactBox = Nothing
                                        , addContactError = Nothing
                                    }
                            in
                            ( newModel
                            , whitelistPort newContact
                            )


subjectBox : OpenedDraft -> Element.Element Msg
subjectBox { draft, document } =
    Element.Input.text []
        { onChange = \t -> UpdatedDraft { draft = { draft | subject = t }, document = document }
        , text = draft.subject
        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type the subject"
        , label = Element.Input.labelAbove [] <| Element.text "Subject:"
        }


userInputBox : OpenedDraft -> Element.Element Msg
userInputBox { draft, document } =
    Element.Input.multiline [ monospace ]
        { onChange = \t -> UpdatedDraft { draft = { draft | userInput = t }, document = document }
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


addProgramButton : OpenedDraft -> Element.Element Msg
addProgramButton draft =
    Element.Input.button []
        { onPress = Just <| ClickAddProgramButton draft
        , label = Element.text "Add program"
        }


draftChooser : List Utils.Draft -> Element.Element Msg
draftChooser drafts =
    Element.column [] <| List.indexedMap (showDraftButton drafts) drafts


addBlobButton : OpenedDraft -> Element.Element Msg
addBlobButton draft =
    Element.Input.button []
        { onPress = Just <| ClickAddBlobButton draft
        , label = Element.text "Add file"
        }


showDraftButton : List Utils.Draft -> Int -> Utils.Draft -> Element.Element Msg
showDraftButton drafts index draft =
    let
        before =
            List.take index drafts

        after =
            List.drop (index + 1) drafts
    in
    Element.Input.button []
        { onPress = Just <| OpenDraft draft <| before ++ after
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


displayDocument : Utils.Document -> Element.Element Msg
displayDocument document =
    case document of
        Utils.Ordering documents ->
            Element.column [] <| List.map displayDocument documents

        Utils.SmallString s ->
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
                Element.Input.labelAbove [ Utils.sansSerif ] <|
                    Element.text "Add someone to your contacts:"
            }
        , Element.Input.button [ Utils.sansSerif ]
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
    Element.el [ Utils.sansSerif ] <|
        Element.text <|
            "My contacts: "
                ++ (case contacts of
                        [] ->
                            "you haven't got any yet"

                        oneOrMore ->
                            String.join ", " <| List.map String.fromInt oneOrMore
                   )


myUsernameIs : Maybe Int -> Element.Element Msg
myUsernameIs maybeMyName =
    Element.el [ Utils.sansSerif ] <|
        Element.text <|
            "My username: "
                ++ (case maybeMyName of
                        Nothing ->
                            "you haven't got one yet"

                        Just nameInt ->
                            String.fromInt nameInt
                   )

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
import Json.Decode as Jd
import Json.Encode as Je
import Set
import Task
import Time
import Truelang
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
    , module_ : Maybe ( String, String )
    , document : Maybe Utils.Document
    }


type alias OpenedDraft =
    { draft : Utils.Draft
    , module_ : Maybe ( String, String )
    , document : Maybe Utils.Document
    }


replaceModule : Utils.Code -> String -> String -> Utils.Code
replaceModule code oldName newCode =
    case oldName of
        "main" ->
            { code | main = newCode }

        _ ->
            let
                hashed =
                    Dict.fromList <| hashModules code.modules
            in
            case Dict.get oldName hashed of
                Nothing ->
                    if oldName == "" then
                        { code | modules = Set.toList <| Set.fromList <| newCode :: code.modules }

                    else
                        code

                Just _ ->
                    let
                        removed =
                            Dict.remove oldName hashed

                        asList =
                            Dict.values removed
                    in
                    { code | modules = newCode :: asList }


port wasmDocumentPort : (String -> msg) -> Sub msg


port runWasmPort : Je.Value -> Cmd msg


runWasm : String -> String -> Cmd msg
runWasm wat userInput =
    runWasmPort <|
        Je.object
            [ ( "wat", Je.string wat )
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
    | RawWasmDocument String
    | UpdateContactBox String
    | RetrievedEditorInfo Je.Value
    | GoodWhitelist Int
    | BadWhitelist String
    | CloseMessage Utils.Message
    | CloseDraft Utils.Draft
    | UpdatedRecipient OpenedDraft String
    | UpdatedDraftNotCode OpenedDraft
    | UpdatedMessageView OpenedMessage
    | MakeNewModule OpenedDraft
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
    | UpdatedCode OpenedDraft ( String, String )


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RawWasmDocument rawDocB64 ->
            case Base64.Decode.decode Base64.Decode.bytes rawDocB64 of
                Err err ->
                    ( { model | internalError = Just <| Utils.showB64Error err }, Cmd.none )

                Ok rawBytes ->
                    case ( model.opened, D.decode Utils.decodeDocument rawBytes ) of
                        ( Neither, _ ) ->
                            ( { model | internalError = Just "received WASM document but no open message" }, Cmd.none )

                        ( ODraft draft, Just document ) ->
                            ( { model | opened = ODraft { draft | document = Just document } }, Cmd.none )

                        ( ODraft draft, Nothing ) ->
                            ( { model | opened = ODraft { draft | document = Just <| Utils.SmallString "could not decode document generated by WASM" } }, Cmd.none )

                        ( OMessage message, Just document ) ->
                            ( { model | opened = OMessage { message | document = Just document } }, Cmd.none )

                        ( OMessage message, Nothing ) ->
                            ( { model | opened = OMessage { message | document = Just <| Utils.SmallString "could not decode document generated by WASM" } }, Cmd.none )

        DownloadFile fileName blob ->
            ( model
            , Download.bytes fileName "application/octet-stream" blob
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

        BlobLoaded blobName { draft, module_, document } fileBytes ->
            let
                blobs =
                    Dict.insert blobName fileBytes draft.blobs

                newDraft =
                    { draft | blobs = blobs }

                newDrafts =
                    newDraft :: model.drafts

                newOpened =
                    { draft = newDraft
                    , module_ = module_
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
                                            { model | addContactBox = Nothing, addContactError = Nothing }
                                    in
                                    ( newModel, whitelistPort newContact )

        UpdateContactBox candidate ->
            case String.toInt candidate of
                Nothing ->
                    if candidate == "" then
                        ( { model | addContactBox = Nothing }, Cmd.none )

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
            ( { model | myContacts = Set.insert newContact model.myContacts }, Cmd.none )

        BadWhitelist error ->
            ( { model | addContactError = Just error }, Cmd.none )

        CloseMessage message ->
            ( { model | messages = message :: model.messages, opened = Neither }, Cmd.none )

        CloseDraft draft ->
            ( { model | drafts = draft :: model.drafts, opened = Neither }, Cmd.none )

        UpdatedMessageView openedMessage ->
            ( { model | opened = OMessage openedMessage }, Cmd.none )

        UpdatedRecipient { draft, module_, document } rawRecipient ->
            let
                newDraft =
                    { draft | to = String.toInt rawRecipient }

                newDrafts =
                    newDraft :: model.drafts
            in
            ( { model | opened = ODraft { draft = newDraft, module_ = module_, document = document } }, cacheDrafts newDrafts )

        UpdatedDraftNotCode { draft, module_ } ->
            let
                newDrafts =
                    draft :: model.drafts

                openDraft =
                    { draft = draft
                    , module_ = module_
                    , document = Nothing
                    }
            in
            ( { model | opened = ODraft openDraft }
            , cacheDrafts newDrafts
            )

        UpdatedCode { draft } ( oldName, newModuleCode ) ->
            let
                addedNewModule =
                    replaceModule draft.code oldName newModuleCode

                newDraft =
                    { draft | code = addedNewModule }

                newDrafts =
                    newDraft :: model.drafts

                newOpened =
                    { draft = newDraft
                    , module_ =
                        case oldName of
                            "main" ->
                                Just ( "main", newModuleCode )

                            _ ->
                                Just
                                    ( Utils.hash newModuleCode
                                    , newModuleCode
                                    )
                    , document = Nothing
                    }
            in
            case Truelang.compile addedNewModule of
                Err err ->
                    ( { model
                        | opened = ODraft newOpened
                        , compilerError = Just err
                      }
                    , cacheDrafts newDrafts
                    )

                Ok wasm ->
                    ( { model | opened = ODraft newOpened }
                    , Cmd.batch
                        [ runWasm wasm draft.userInput
                        , cacheDrafts newDrafts
                        ]
                    )

        MakeNewModule { draft, document } ->
            ( { model | opened = ODraft { draft = draft, module_ = Just ( "", "" ), document = document } }, Cmd.none )

        OpenDraft draft remainingDrafts ->
            let
                newDraft =
                    { draft = draft
                    , module_ = Nothing
                    , document = Nothing
                    }
            in
            case Truelang.compile draft.code of
                Err err ->
                    ( { model | opened = ODraft newDraft, compilerError = Just err, drafts = remainingDrafts }, Cmd.none )

                Ok wasm ->
                    ( { model | opened = ODraft newDraft, drafts = remainingDrafts }, runWasm wasm draft.userInput )

        OpenMessage message remainingMessages ->
            let
                newMessage =
                    { message = message
                    , module_ = Nothing
                    , document = Nothing
                    }
            in
            case Truelang.compile message.code of
                Err err ->
                    ( { model | opened = OMessage { newMessage | document = Just <| Utils.SmallString <| "compiler error: " ++ err }, messages = remainingMessages }, Cmd.none )

                Ok wasm ->
                    ( { model | opened = OMessage newMessage, messages = remainingMessages }
                    , runWasm wasm message.userInput
                    )

        MakeNewDraft ->
            ( model, Task.perform TimeForNewDraft Time.now )

        TimeForNewDraft posixTime ->
            ( { model | opened = ODraft { draft = { to = Nothing, time = Time.posixToMillis posixTime, subject = "", userInput = "", code = { main = "", modules = [] }, blobs = Dict.empty }, module_ = Nothing, document = Nothing } }, Cmd.none )

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
    case ( to, maybeMyName ) of
        ( Nothing, _ ) ->
            Err "can't send because no recipient"

        ( _, Nothing ) ->
            Err "can't send because you have no username"

        ( Just to_, Just myName ) ->
            Ok
                { from = myName
                , to = to_
                , time = time
                , subject = subject
                , userInput = userInput
                , code = code
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
viewMessage { message, module_, document } =
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
        , viewCode { message = message, module_ = module_, document = document }
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


viewCode : OpenedMessage -> Element.Element Msg
viewCode { message, module_, document } =
    case module_ of
        Nothing ->
            Element.column [] <|
                messageOpenModuleButton message ( "main", message.code.main )
                    :: (List.map (messageOpenModuleButton message) <|
                            hashModules message.code.modules
                       )

        Just ( moduleName, code ) ->
            Element.column []
                [ Element.Input.button []
                    { onPress = Just <| UpdatedMessageView { message = message, module_ = Nothing, document = document }
                    , label = Element.text "Back to modules"
                    }
                , Element.text moduleName
                , Element.paragraph [] [ Element.text code ]
                ]


hashModules : List String -> List ( String, String )
hashModules modules =
    List.map (\s -> ( Utils.hash s, s )) modules


messageOpenModuleButton : Utils.Message -> ( String, String ) -> Element.Element Msg
messageOpenModuleButton message module_ =
    Element.Input.button []
        { onPress = Just <| UpdatedMessageView { message = message, module_ = Just module_, document = Nothing }
        , label = Element.text <| Tuple.first module_
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
        , case draft.document of
            Nothing ->
                Element.none

            Just d ->
                displayDocument d
        , editCode draft
        , case compilerError of
            Nothing ->
                Element.none

            Just err ->
                Element.el [ monospace ] <| Element.text err
        , editBlobs draft
        , sendDraft draft.draft
        ]


editBlobs : OpenedDraft -> Element.Element Msg
editBlobs { draft, module_, document } =
    Element.column [] <|
        List.map (editBlob { draft = draft, module_ = module_, document = document }) <|
            Dict.toList draft.blobs


editBlob : OpenedDraft -> ( String, Bytes.Bytes ) -> Element.Element Msg
editBlob { draft, module_, document } ( blobName, blob ) =
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
            { onPress = Just <| UpdatedDraftNotCode { draft = newDraft, module_ = module_, document = document }
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


subjectBox : OpenedDraft -> Element.Element Msg
subjectBox { draft, module_, document } =
    Element.Input.text []
        { onChange = \t -> UpdatedDraftNotCode { draft = { draft | subject = t }, module_ = module_, document = document }
        , text = draft.subject
        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type the subject"
        , label = Element.Input.labelAbove [] <| Element.text "Subject:"
        }


userInputBox : OpenedDraft -> Element.Element Msg
userInputBox { draft, module_, document } =
    Element.Input.multiline []
        { onChange = \t -> UpdatedDraftNotCode { draft = { draft | userInput = t }, module_ = module_, document = document }
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


addBlobButton : OpenedDraft -> Element.Element Msg
addBlobButton draft =
    Element.Input.button []
        { onPress = Just <| ClickAddBlobButton draft
        , label = Element.text "Add file"
        }


editCode : OpenedDraft -> Element.Element Msg
editCode draft =
    case draft.module_ of
        Nothing ->
            Element.column [] <|
                [ makeNewModuleButton draft
                , addBlobButton draft
                , Element.column [] <|
                    draftOpenModuleButton draft ( "main", draft.draft.code.main )
                        :: (List.map (draftOpenModuleButton draft) <|
                                hashModules draft.draft.code.modules
                           )
                ]

        Just ( moduleName, code ) ->
            Element.column []
                [ Element.Input.button []
                    { onPress = Just <| UpdatedDraftNotCode { draft | module_ = Nothing }
                    , label = Element.text "Back to modules"
                    }
                , Element.Input.text []
                    { onChange = \n -> UpdatedDraftNotCode { draft | module_ = Just ( n, code ) }
                    , text = moduleName
                    , placeholder =
                        Just <|
                            Element.Input.placeholder [] <|
                                Element.text "Type module name here"
                    , label =
                        Element.Input.labelAbove [] <|
                            Element.text "Module name"
                    }
                , Element.Input.multiline [ monospace ]
                    { onChange = \c -> UpdatedCode draft ( moduleName, c )
                    , text = code
                    , placeholder =
                        Just <|
                            Element.Input.placeholder [] <|
                                Element.text "Type your code here"
                    , label =
                        Element.Input.labelAbove [] <|
                            Element.text "Module code"
                    , spellcheck = False
                    }
                ]


makeNewModuleButton : OpenedDraft -> Element.Element Msg
makeNewModuleButton draft =
    Element.Input.button []
        { onPress = Just <| MakeNewModule draft
        , label = Element.text "New module"
        }


draftOpenModuleButton : OpenedDraft -> ( String, String ) -> Element.Element Msg
draftOpenModuleButton openedDraft ( name, code ) =
    Element.Input.button []
        { onPress = Just <| UpdatedDraftNotCode <| { openedDraft | module_ = Just ( name, code ) }
        , label = Element.text name
        }


draftChooser : List Utils.Draft -> Element.Element Msg
draftChooser drafts =
    Element.column [] <| List.indexedMap (showDraftButton drafts) drafts


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
            Element.text s


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

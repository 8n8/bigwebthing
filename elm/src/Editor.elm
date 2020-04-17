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
import SHA256
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
    , openMessage : Maybe ( Utils.Message, Maybe ( String, String ) )
    , drafts : List Utils.Draft
    , openDraft : Maybe ( Utils.Draft, Maybe ( String, String ) )
    , internalError : Maybe String
    , outbox : List Utils.Message
    , sendMessageError : Maybe String
    }


port sendMessageError : (String -> msg) -> Sub msg


port getEditorInfo : () -> Cmd msg


port retrievedEditorInfo : (Je.Value -> msg) -> Sub msg


subscriptions =
    Sub.batch
        [ retrievedEditorInfo RetrievedEditorInfo
        , badWhitelist BadWhitelist
        , goodWhitelist GoodWhitelist
        , sendMessageError SendMessageError
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
    , messages = []
    , openDraft = Nothing
    , openMessage = Nothing
    }


type Msg
    = AddNewContact
    | UpdateContactBox String
    | RetrievedEditorInfo Je.Value
    | GoodWhitelist Int
    | BadWhitelist String
    | CloseMessage Utils.Message
    | CloseDraft Utils.Draft
    | UpdatedRecipient ( Utils.Draft, Maybe ( String, String ) ) String
    | UpdatedDraft ( Utils.Draft, Maybe ( String, String ) )
    | UpdatedMessageView ( Utils.Message, Maybe ( String, String ) )
    | MakeNewModule Utils.Draft
    | OpenDraft Utils.Draft (List Utils.Draft)
    | OpenMessage Utils.Message (List Utils.Message)
    | MakeNewDraft
    | TimeForNewDraft Time.Posix
    | SendDraft Utils.Draft
    | SendMessageError String
    | ClickAddBlobButton Utils.Draft
    | BlobSelected Utils.Draft File.File
    | BlobLoaded String Utils.Draft Bytes.Bytes
    | DownloadFile String Bytes.Bytes


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
        DownloadFile fileName blob ->
            ( model
            , Download.bytes fileName "application/octet-stream" blob
            )

        ClickAddBlobButton draft ->
            ( model
            , Select.file
                [ "application/octet-stream" ]
                (BlobSelected draft)
            )

        BlobSelected draft file ->
            ( model
            , Task.perform (BlobLoaded (File.name file) draft) <|
                File.toBytes file
            )

        BlobLoaded blobName draft fileBytes ->
            let
                blobs =
                    Dict.insert blobName fileBytes draft.blobs

                newDraft =
                    { draft | blobs = blobs }

                newDrafts =
                    newDraft :: model.drafts
            in
            ( { model | openDraft = Just ( newDraft, Nothing ) }
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
            ( { model | messages = message :: model.messages, openMessage = Nothing }, Cmd.none )

        CloseDraft draft ->
            ( { model | drafts = draft :: model.drafts, openDraft = Nothing }, Cmd.none )

        UpdatedMessageView openMessage ->
            ( { model | openMessage = Just openMessage }, Cmd.none )

        UpdatedRecipient ( draft, maybeOpenModule ) rawRecipient ->
            let
                newDraft =
                    { draft | to = String.toInt rawRecipient }

                newDrafts =
                    newDraft :: model.drafts
            in
            ( { model | openDraft = Just ( newDraft, maybeOpenModule ) }, cacheDrafts newDrafts )

        UpdatedDraft ( draft, maybeOpenModule ) ->
            let
                newCode =
                    case maybeOpenModule of
                        Nothing ->
                            draft.code

                        Just ( name, code ) ->
                            case model.openDraft of
                                Nothing ->
                                    draft.code

                                Just ( _, Nothing ) ->
                                    draft.code

                                Just ( _, Just ( oldName, oldCode ) ) ->
                                    let
                                        removedOld =
                                            Dict.remove oldName draft.code
                                    in
                                    Dict.insert name code removedOld

                newDraft =
                    { draft | code = newCode }

                newDrafts =
                    newDraft :: model.drafts
            in
            ( { model | openDraft = Just ( newDraft, maybeOpenModule ) }, cacheDrafts newDrafts )

        MakeNewModule draft ->
            ( { model | openDraft = Just ( draft, Just ( "", "" ) ) }, Cmd.none )

        OpenDraft draft remainingDrafts ->
            ( { model | openDraft = Just ( draft, Nothing ), drafts = remainingDrafts }, Cmd.none )

        OpenMessage message remainingMessages ->
            ( { model | openMessage = Just ( message, Nothing ), messages = remainingMessages }, Cmd.none )

        MakeNewDraft ->
            ( model, Task.perform TimeForNewDraft Time.now )

        TimeForNewDraft posixTime ->
            ( { model | openDraft = Just ( { to = Nothing, time = Time.posixToMillis posixTime, subject = "", userInput = "", code = Dict.empty, blobs = Dict.empty }, Nothing ) }, Cmd.none )

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
                        | openDraft = Nothing
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
                , case model.myName of
                    Nothing ->
                        Element.text "Need username to display messages"

                    Just myName ->
                        viewMessages myName model.messages model.openMessage
                , makeNewDraft
                , case model.myName of
                    Nothing ->
                        Element.text "Need username to display drafts"

                    Just myName ->
                        editDrafts myName model.openDraft model.drafts
                ]


viewMessages : Int -> List Utils.Message -> Maybe ( Utils.Message, Maybe ( String, String ) ) -> Element.Element Msg
viewMessages myName messages maybeMessage =
    case maybeMessage of
        Nothing ->
            messageChooser messages

        Just openMessage ->
            viewMessage myName openMessage


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


messageChooserLabel message =
    Element.column []
        [ Element.text <| "Subject: " ++ message.subject
        , Element.text <| "From: " ++ String.fromInt message.from
        ]


viewMessage : Int -> ( Utils.Message, Maybe ( String, String ) ) -> Element.Element Msg
viewMessage myName ( message, maybeOpenModule ) =
    Element.column []
        [ closeMessage message
        , Element.text <| "From: " ++ String.fromInt message.from
        , Element.text <| "Subject: " ++ message.subject
        , Element.text "User input:"
        , Element.paragraph [] [ Element.text message.userInput ]
        , codeOutput message.code message.userInput myName
        , viewCode ( message, maybeOpenModule )
        , viewBlobs message.blobs
        ]


codeOutput : Dict.Dict String String -> String -> Int -> Element.Element Msg
codeOutput code userInput myName =
    case Truelang.runProgram code userInput myName of
        Nothing ->
            Element.text "no output"

        Just document ->
            displayDocument document


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


viewCode : ( Utils.Message, Maybe ( String, String ) ) -> Element.Element Msg
viewCode ( message, maybeOpenModule ) =
    case maybeOpenModule of
        Nothing ->
            Element.column [] <|
                (List.map (messageOpenModuleButton message) <|
                    Dict.toList message.code
                )

        Just ( moduleName, code ) ->
            Element.column []
                [ Element.Input.button []
                    { onPress = Just <| UpdatedMessageView ( message, Nothing )
                    , label = Element.text "Back to modules"
                    }
                , Element.text moduleName
                , Element.paragraph [] [ Element.text code ]
                ]


messageOpenModuleButton : Utils.Message -> ( String, String ) -> Element.Element Msg
messageOpenModuleButton message module_ =
    Element.Input.button []
        { onPress = Just <| UpdatedMessageView ( message, Just module_ )
        , label = Element.text <| Tuple.first module_
        }


editDrafts : Int -> Maybe ( Utils.Draft, Maybe ( String, String ) ) -> List Utils.Draft -> Element.Element Msg
editDrafts myName maybeOpenDraft drafts =
    case maybeOpenDraft of
        Nothing ->
            draftChooser drafts

        Just openDraft ->
            editDraft myName openDraft


editDraft : Int -> ( Utils.Draft, Maybe ( String, String ) ) -> Element.Element Msg
editDraft myName draft =
    Element.column []
        [ closeDraft <| Tuple.first draft
        , toBox draft
        , subjectBox draft
        , userInputBox draft
        , codeOutput ((.code << Tuple.first) draft) ((.userInput << Tuple.first) draft) myName
        , editCode draft
        , editBlobs draft
        , sendDraft <| Tuple.first draft
        ]


editBlobs : ( Utils.Draft, Maybe ( String, String ) ) -> Element.Element Msg
editBlobs ( draft, openModule ) =
    Element.column [] <|
        List.map (editBlob draft openModule) <|
            Dict.toList draft.blobs


editBlob : Utils.Draft -> Maybe ( String, String ) -> ( String, Bytes.Bytes ) -> Element.Element Msg
editBlob draft openModule ( blobName, blob ) =
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
            { onPress = Just <| UpdatedDraft ( newDraft, openModule )
            , label = Element.text "Delete"
            }
        ]


sendDraft : Utils.Draft -> Element.Element Msg
sendDraft draft =
    Element.Input.button []
        { onPress = Just <| SendDraft draft
        , label = Element.text "Send message"
        }


toBox : ( Utils.Draft, Maybe ( String, String ) ) -> Element.Element Msg
toBox ( draft, openModule ) =
    Element.Input.text []
        { onChange = UpdatedRecipient ( draft, openModule )
        , text =
            case draft.to of
                Nothing ->
                    ""

                Just recipient ->
                    String.fromInt recipient
        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type the recipient ID here"
        , label = Element.Input.labelAbove [] <| Element.text "To:"
        }


subjectBox : ( Utils.Draft, Maybe ( String, String ) ) -> Element.Element Msg
subjectBox ( draft, openModule ) =
    Element.Input.text []
        { onChange = \t -> UpdatedDraft ( { draft | subject = t }, openModule )
        , text = draft.subject
        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type the subject"
        , label = Element.Input.labelAbove [] <| Element.text "Subject:"
        }


userInputBox : ( Utils.Draft, Maybe ( String, String ) ) -> Element.Element Msg
userInputBox ( draft, openModule ) =
    Element.Input.multiline []
        { onChange = \t -> UpdatedDraft ( { draft | userInput = t }, openModule )
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


addBlobButton : Utils.Draft -> Element.Element Msg
addBlobButton draft =
    Element.Input.button []
        { onPress = Just <| ClickAddBlobButton draft
        , label = Element.text "Add file"
        }


editCode : ( Utils.Draft, Maybe ( String, String ) ) -> Element.Element Msg
editCode ( draft, openModule ) =
    case openModule of
        Nothing ->
            Element.column [] <|
                [ makeNewModuleButton draft
                , addBlobButton draft
                , Element.column [] <|
                    List.map (draftOpenModuleButton draft) <|
                        Dict.toList draft.code
                ]

        Just ( moduleName, code ) ->
            Element.column []
                [ Element.Input.button []
                    { onPress = Just <| UpdatedDraft ( draft, Nothing )
                    , label = Element.text "Back to modules"
                    }
                , Element.Input.text []
                    { onChange = \n -> UpdatedDraft ( draft, Just ( n, code ) )
                    , text = moduleName
                    , placeholder =
                        Just <|
                            Element.Input.placeholder [] <|
                                Element.text "Type module name here"
                    , label =
                        Element.Input.labelAbove [] <|
                            Element.text "Module name"
                    }
                , Element.Input.multiline []
                    { onChange = \c -> UpdatedDraft ( draft, Just ( moduleName, c ) )
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


makeNewModuleButton : Utils.Draft -> Element.Element Msg
makeNewModuleButton draft =
    Element.Input.button []
        { onPress = Just <| MakeNewModule draft
        , label = Element.text "New module"
        }


draftOpenModuleButton : Utils.Draft -> ( String, String ) -> Element.Element Msg
draftOpenModuleButton draft ( name, code ) =
    Element.Input.button []
        { onPress = Just <| UpdatedDraft ( draft, Just ( name, code ) )
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



-- programOutput :
--     Maybe String
--     -> Dict.Dict String Utils.Program
--     -> List Int
--     -> Maybe Int
--     -> Element.Element Msg
-- programOutput maybeOpenProgram programs contacts maybeMyName =
--     case ( maybeOpenProgram, maybeMyName ) of
--         ( Nothing, _ ) ->
--             Element.none
--
--         ( _, Nothing ) ->
--             Element.el [ monospace ] <| Element.text "no username, so can't run program"
--
--         ( Just programName, Just myName ) ->
--             let
--                 maybeOutput =
--                     Truelang.runProgram programName programs myName
--             in
--             case maybeOutput of
--                 Nothing ->
--                     Element.text "this program produces no output"
--
--                 Just output ->
--                     displayDocument output


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

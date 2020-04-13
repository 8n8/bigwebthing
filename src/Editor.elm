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
    }


port getEditorInfo : () -> Cmd msg


port retrievedEditorInfo : (Je.Value -> msg) -> Sub msg


subscriptions =
    Sub.batch
        [ retrievedEditorInfo RetrievedEditorInfo
        , badWhitelist BadWhitelist
        , goodWhitelist GoodWhitelist
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
    , drafts = []
    , messages = []
    , openDraft = Nothing
    , openMessage = Nothing
    }


type Msg
    = AddNewContact
    | UpdateContactBox String
    | RetrievedEditorInfo Je.Value
    | SendMessage Utils.Message
    | GoodWhitelist Int
    | BadWhitelist String
    | CloseMessage Utils.Message
    | UpdatedRecipient ( Utils.Draft, Maybe ( String, String ) ) String
    | UpdatedDraft ( Utils.Draft, Maybe ( String, String ) )
    | UpdatedMessageView ( Utils.Message, Maybe ( String, String ) )
    | MakeNewModule Utils.Draft
    | OpenDraft Utils.Draft
    | MakeNewDraft
    | TimeForNewDraft Time.Posix


editorInfoDecoder : Jd.Decoder RawEditorInfo
editorInfoDecoder =
    Jd.map4 RawEditorInfo
        (Jd.field "myName" Jd.int)
        (Jd.field "myContacts" (Jd.list Jd.int))
        (Jd.field "inbox" (Jd.list Jd.string))
        (Jd.field "drafts" (Jd.list Jd.string))


type alias RawEditorInfo =
    { myName : Int
    , myContacts : List Int
    , inbox : List String
    , drafts : List String
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
                    case Utils.decodeInbox raw.inbox of
                        Err err ->
                            ( { model
                                | internalError =
                                    Just <|
                                        "could not decode inbox: "
                                            ++ err
                              }
                            , Cmd.none
                            )

                        Ok messages ->
                            ( { model
                                | messages = messages
                                , myName = Just raw.myName
                                , myContacts = Set.fromList raw.myContacts
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

        SendMessage draft ->
            ( model, sendMessage draft )

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
                newDrafts =
                    draft :: model.drafts
            in
            ( { model | openDraft = Just ( draft, maybeOpenModule ) }, cacheDrafts newDrafts )

        MakeNewModule draft ->
            ( { model | openDraft = Just ( draft, Just ( "", "" ) ) }, Cmd.none )

        OpenDraft draft ->
            ( { model | openDraft = Just ( draft, Nothing ) }, Cmd.none )

        MakeNewDraft ->
            ( model, Task.perform TimeForNewDraft Time.now )

        TimeForNewDraft posixTime ->
            ( { model | openDraft = Just ( { to = Nothing, time = Time.posixToMillis posixTime, subject = "", userInput = "", code = Dict.empty, blobs = Dict.empty }, Nothing ) }, Cmd.none )


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
                , viewMessages model.messages model.openMessage
                , makeNewDraft
                , editDrafts model.openDraft model.drafts
                ]


viewMessages : List Utils.Message -> Maybe ( Utils.Message, Maybe ( String, String ) ) -> Element.Element Msg
viewMessages messages maybeMessage =
    case maybeMessage of
        Nothing ->
            messageChooser messages

        Just openMessage ->
            viewMessage openMessage


messageChooser : List Utils.Message -> Element.Element Msg
messageChooser messages =
    Element.column [] <| List.map messageChooserButton messages


messageChooserButton : Utils.Message -> Element.Element Msg
messageChooserButton message =
    Element.column []
        [ Element.text <| "Subject: " ++ message.subject
        , Element.text <| "From: " ++ String.fromInt message.from
        ]


viewMessage : ( Utils.Message, Maybe ( String, String ) ) -> Element.Element Msg
viewMessage ( message, maybeOpenModule ) =
    Element.column []
        [ closeMessage message
        , Element.text <| "From: " ++ String.fromInt message.from
        , Element.text <| "Subject: " ++ message.subject
        , Element.text "User input:"
        , Element.paragraph [] [ Element.text message.userInput ]
        , viewCode ( message, maybeOpenModule )
        ]


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
                List.map (messageOpenModuleButton message) <|
                    Dict.toList message.code

        Just ( moduleName, code ) ->
            Element.column []
                [ Element.text moduleName
                , Element.paragraph [] [ Element.text code ]
                ]


messageOpenModuleButton : Utils.Message -> ( String, String ) -> Element.Element Msg
messageOpenModuleButton message module_ =
    Element.Input.button []
        { onPress = Just <| UpdatedMessageView ( message, Just module_ )
        , label = Element.text <| Tuple.first module_
        }


editDrafts : Maybe ( Utils.Draft, Maybe ( String, String ) ) -> List Utils.Draft -> Element.Element Msg
editDrafts maybeOpenDraft drafts =
    case maybeOpenDraft of
        Nothing ->
            draftChooser drafts

        Just openDraft ->
            editDraft openDraft


editDraft : ( Utils.Draft, Maybe ( String, String ) ) -> Element.Element Msg
editDraft draft =
    Element.column []
        [ toBox draft
        , subjectBox draft
        , userInputBox draft
        , editCode draft
        ]


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


editCode : ( Utils.Draft, Maybe ( String, String ) ) -> Element.Element Msg
editCode ( draft, openModule ) =
    case openModule of
        Nothing ->
            Element.column [] <|
                makeNewModuleButton draft
                    :: (List.map (draftOpenModuleButton draft) <| Dict.toList draft.code)

        Just ( moduleName, code ) ->
            Element.column []
                [ Element.Input.text []
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
    Element.column [] <| List.map showDraftButton drafts


showDraftButton : Utils.Draft -> Element.Element Msg
showDraftButton draft =
    Element.Input.button []
        { onPress = Just <| OpenDraft draft
        , label = Element.text draft.subject
        }


monospace : Element.Attribute Msg
monospace =
    Font.family [ Font.typeface "Ubuntu Mono" ]


programOutput :
    Maybe String
    -> Dict.Dict String Utils.Program
    -> List Int
    -> Maybe Int
    -> Element.Element Msg
programOutput maybeOpenProgram programs contacts maybeMyName =
    case ( maybeOpenProgram, maybeMyName ) of
        ( Nothing, _ ) ->
            Element.none

        ( _, Nothing ) ->
            Element.el [ monospace ] <| Element.text "no username, so can't run program"

        ( Just programName, Just myName ) ->
            let
                maybeOutput =
                    Truelang.runProgram programName programs myName
            in
            case maybeOutput of
                Nothing ->
                    Element.text "this program produces no output"

                Just output ->
                    displayDocument output


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

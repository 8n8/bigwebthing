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
import Truelang
import Utils


type alias Model =
    { myName : Maybe Int
    , myContacts : List Int
    , sendToBox : Maybe Int
    , sendToError : Maybe SendMessageError
    , addContactBox : Maybe Int
    , addContactError : Maybe AddContactError
    , programs : Dict.Dict String Utils.Program
    , openProgram : Maybe String
    , internalError : Maybe InternalError
    , newContacts : List Int
    }


port getEditorInfo : () -> Cmd msg


port retrievedEditorInfo : (Je.Value -> msg) -> Sub msg


subscriptions =
    retrievedEditorInfo RetrievedEditorInfo


initCmd : Cmd Msg
initCmd =
    getEditorInfo ()


initModel : Model
initModel =
    { myName = Nothing
    , myContacts = []
    , addContactBox = Nothing
    , addContactError = Nothing
    , programs = Dict.empty
    , openProgram = Nothing
    , internalError = Nothing
    , newContacts = []
    , sendToBox = Nothing
    , sendToError = Nothing
    }


type InternalError
    = UpdatedUserInputButNoOpenProgram
    | UpdatedUserInputButNoProgram
    | UpdatedProgramButNoOpenProgram
    | UpdatedProgramButNoProgram
    | BadCache Base64.Decode.Error
    | BadDecodeCache String
    | BadEditorCacheDecode Jd.Error
    | NewUserInputButNoUsername String
    | NewDescriptionButNoUsername
    | SendMessageButBadOpenProgram


type Msg
    = UpdatedUserInput String
    | UpdatedProgramEditor String
    | MakeNewProgram
    | LaunchProgram String
    | AddNewContact
    | UpdateContactBox String
    | RetrievedEditorInfo Je.Value
    | UpdatedDescription String
    | UpdatedRecipientBox String
    | SendMessage


editorInfoDecoder : Jd.Decoder RawEditorInfo
editorInfoDecoder =
    Jd.map3 RawEditorInfo
        (Jd.field "myName" Jd.int)
        (Jd.field "myContacts" (Jd.list Jd.int))
        (Jd.field "editorCache" Jd.string)


type alias RawEditorInfo =
    { myName : Int
    , myContacts : List Int
    , editorCache : String
    }


combinePrograms :
    Dict.Dict String Utils.Program
    -> List Utils.Program
    -> Dict.Dict String Utils.Program
combinePrograms oldPrograms newPrograms =
    Dict.union oldPrograms <|
        Dict.fromList <|
            List.map plusHash newPrograms


plusHash : Utils.Program -> ( String, Utils.Program )
plusHash program =
    ( Utils.hash program.code, program )


sendMessage : String -> Utils.Version -> Int -> Cmd Msg
sendMessage code version recipient =
    let
        humanMsg =
            { to = recipient, code = code, version = version }

        msgOut =
            Utils.SendThis humanMsg

        encodedBytes =
            E.encode <| encodeMessage msgOut

        encodedB64 =
            Base64.Encode.encode <| Base64.Encode.bytes encodedBytes
    in
    sendMessagePort encodedB64


port sendMessagePort : String -> Cmd msg


whitelistSomeone : Int -> Cmd Msg
whitelistSomeone username =
    sendMessagePort <| Base64.Encode.encode <|
        Base64.Encode.bytes <| E.encode <| encodeMessage <|
        Utils.WhitelistSomeone username


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
                                BadEditorCacheDecode err
                      }
                    , Cmd.none
                    )

                Ok raw ->
                    case Utils.decodeEditorCache raw.editorCache of
                        Err err ->
                            ( { model
                                | internalError =
                                    Just <|
                                        BadDecodeCache err
                              }
                            , Cmd.none
                            )

                        Ok { newContacts, programs } ->
                            ( { model
                                | programs =
                                    combinePrograms
                                        model.programs
                                        programs
                                , myName = Just raw.myName
                                , myContacts = raw.myContacts
                                , newContacts =
                                    model.newContacts ++ newContacts
                              }
                            , Cmd.none
                            )

        UpdatedUserInput newUserInput ->
            case model.openProgram of
                Nothing ->
                    ( { model
                        | internalError =
                            Just UpdatedUserInputButNoOpenProgram
                      }
                    , Cmd.none
                    )

                Just programName ->
                    case Dict.get programName model.programs of
                        Nothing ->
                            ( { model
                                | internalError =
                                    Just
                                        UpdatedUserInputButNoProgram
                              }
                            , Cmd.none
                            )

                        Just program ->
                            case model.myName of
                                Nothing ->
                                    ( { model | internalError = Just <| NewUserInputButNoUsername "no username" }, Cmd.none )

                                Just myName ->
                                    let
                                        version : Utils.Version
                                        version =
                                            { description = "", userInput = newUserInput, author = myName }

                                        versions : List Utils.Version
                                        versions =
                                            [ version ]

                                        newProgram =
                                            case program.versions of
                                                [] ->
                                                    { program | versions = versions }

                                                v :: ersions ->
                                                    { program | versions = { v | userInput = newUserInput } :: ersions }

                                        newPrograms =
                                            Dict.insert programName newProgram model.programs

                                        newModel =
                                            { model | programs = newPrograms }
                                    in
                                    ( newModel, cacheModel newModel )

        UpdatedProgramEditor newCode ->
            case model.openProgram of
                Nothing ->
                    ( { model | internalError = Just UpdatedProgramButNoOpenProgram }, Cmd.none )

                Just programName ->
                    case Dict.get programName model.programs of
                        Nothing ->
                            ( { model | internalError = Just UpdatedProgramButNoProgram }, Cmd.none )

                        Just program ->
                            let
                                newProgram =
                                    { program | code = newCode }

                                newName =
                                    Utils.hash newCode

                                newPrograms =
                                    Dict.remove programName <|
                                        Dict.insert newName newProgram model.programs

                                newModel =
                                    { model | programs = newPrograms, openProgram = Just newName }
                            in
                            ( newModel, cacheModel newModel )

        UpdatedDescription newDescription ->
            case model.openProgram of
                Nothing ->
                    ( { model | internalError = Just UpdatedProgramButNoOpenProgram }, Cmd.none )

                Just programName ->
                    case Dict.get programName model.programs of
                        Nothing ->
                            ( { model | internalError = Just UpdatedProgramButNoProgram }, Cmd.none )

                        Just program ->
                            case model.myName of
                                Nothing ->
                                    ( { model | internalError = Just NewDescriptionButNoUsername }, Cmd.none )

                                Just myName ->
                                    let
                                        newProgram =
                                            case program.versions of
                                                [] ->
                                                    { program | versions = [ { description = newDescription, userInput = "", author = myName } ] }

                                                v :: ersions ->
                                                    { program | versions = { v | description = newDescription } :: ersions }

                                        newPrograms =
                                            Dict.insert programName newProgram model.programs

                                        newModel =
                                            { model | programs = newPrograms }
                                    in
                                    ( newModel, cacheModel newModel )

        MakeNewProgram ->
            let
                newProgram =
                    { code = "", versions = [] }

                name =
                    Utils.hash newProgram.code

                newPrograms =
                    Dict.insert name newProgram model.programs

                newModel =
                    { model | programs = newPrograms, openProgram = Just name }
            in
            ( newModel, cacheModel newModel )

        LaunchProgram programName ->
            ( { model | openProgram = Just programName }, Cmd.none )

        AddNewContact ->
            case model.addContactBox of
                Nothing ->
                    ( model, Cmd.none )

                Just newContact ->
                    let
                        newModel =
                            { model | newContacts = newContact :: model.newContacts }
                    in
                    ( newModel, Cmd.batch [cacheModel newModel, whitelistSomeone newContact] )

        SendMessage ->
            case model.sendToBox of
                Nothing ->
                    ( model, Cmd.none )

                Just recipient ->
                    case model.openProgram of
                        Nothing ->
                            ( model, Cmd.none )

                        Just programName ->
                            case Dict.get programName model.programs of
                                Nothing ->
                                    ( { model | internalError = Just SendMessageButBadOpenProgram }, Cmd.none )

                                Just program ->
                                    case program.versions of
                                        [] ->
                                            ( { model | sendToError = Just NothingHereToSend }, Cmd.none )

                                        v :: _ ->
                                            ( model, sendMessage program.code v recipient )

        UpdatedRecipientBox candidate ->
            case String.toInt candidate of
                Nothing ->
                    if candidate == "" then
                        ( { model | sendToBox = Nothing }, Cmd.none )

                    else
                        ( model, Cmd.none )

                Just number ->
                    if number < 0 then
                        ( model, Cmd.none )

                    else
                        let
                            newModel =
                                { model | sendToBox = Just number }
                        in
                        ( newModel, Cmd.none )

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


view : Model -> Element.Element Msg
view { internalError, newContacts, myName, myContacts, addContactBox, addContactError, programs, openProgram, sendToBox, sendToError } =
    case internalError of
        Just err ->
            Element.text <| "internal error: " ++ showInternalError err

        Nothing ->
            Element.column [ Element.spacing 20, Utils.fontSize ]
                [ myUsernameIs myName
                , myContactsAre myContacts
                , waitingContacts newContacts
                , addNewContact addContactBox addContactError
                , chooseAProgram programs openProgram
                , makeNewProgram
                , programOutput openProgram programs myContacts myName
                , myInput openProgram programs
                , editDescription openProgram programs
                , programCode openProgram programs
                , sendItTo sendToBox sendToError
                ]


type SendMessageError
    = YouCantSendToYourself
    | NothingHereToSend


sendItTo : Maybe Int -> Maybe SendMessageError -> Element.Element Msg
sendItTo boxContents maybeError =
    Element.column [] <|
        [ Element.Input.text []
            { onChange = UpdatedRecipientBox
            , text =
                case boxContents of
                    Just n ->
                        String.fromInt n

                    Nothing ->
                        ""
            , placeholder =
                Just <|
                    Element.Input.placeholder [] <|
                        Element.text "Type their username"
            , label =
                Element.Input.labelAbove [] <|
                    Element.text "Send this to someone else:"
            }
        , Element.Input.button []
            { onPress = Just SendMessage
            , label = Element.text "Send"
            }
        , case maybeError of
            Nothing ->
                Element.none

            Just YouCantSendToYourself ->
                Element.text "you can't send messages to yourself"

            Just NothingHereToSend ->
                Element.text "there is nothing here to send"
        ]


showInternalError : InternalError -> String
showInternalError error =
    case error of
        UpdatedUserInputButNoOpenProgram ->
            "updated user input but no open program"

        UpdatedUserInputButNoProgram ->
            "updated user input but no program"

        UpdatedProgramButNoOpenProgram ->
            "updated program but no open program"

        UpdatedProgramButNoProgram ->
            "updated program but no program"

        BadCache err ->
            "bad cache: " ++ Utils.showB64Error err

        BadDecodeCache err ->
            "bad decode cache: " ++ err

        BadEditorCacheDecode jsonError ->
            "bad editor cache decoder: " ++ Jd.errorToString jsonError

        NewUserInputButNoUsername err ->
            "new user input but no username: " ++ err

        NewDescriptionButNoUsername ->
            "new description but no username"

        SendMessageButBadOpenProgram ->
            "tried to send message but bad open program"


waitingContacts : List Int -> Element.Element Msg
waitingContacts contacts =
    case contacts of
        [] ->
            Element.none

        oneOrMore ->
            Element.el [ Utils.sansSerif ] <|
                Element.text <|
                    "Contacts waiting to be added: "
                        ++ (String.join ", " <| List.map String.fromInt oneOrMore)


port cacheEditorInfo : String -> Cmd msg


cacheModel : Model -> Cmd msg
cacheModel model =
    modelToCache model
        |> Utils.encodeCache
        |> Utils.toB64
        |> cacheEditorInfo


modelToCache : Model -> Utils.Cache
modelToCache { newContacts, programs } =
    { newContacts = newContacts
    , programs = Dict.values programs
    }


editDescription : Maybe String -> Dict.Dict String Utils.Program -> Element.Element Msg
editDescription maybeOpenProgram programs =
    case maybeOpenProgram of
        Nothing ->
            Element.none

        Just programName ->
            case Dict.get programName programs of
                Nothing ->
                    Element.text "Internal error: can't find program"

                Just program ->
                    Element.Input.multiline [ monospace ]
                        { onChange = UpdatedDescription
                        , text =
                            case program.versions of
                                [] ->
                                    ""

                                v :: _ ->
                                    v.description
                        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type description here"
                        , label = Element.Input.labelAbove [ Utils.sansSerif ] <| Element.text "Program description:"
                        , spellcheck = True
                        }


myInput : Maybe String -> Dict.Dict String Utils.Program -> Element.Element Msg
myInput maybeOpenProgram programs =
    case maybeOpenProgram of
        Nothing ->
            Element.none

        Just programName ->
            case Dict.get programName programs of
                Nothing ->
                    Element.text "Internal error: can't find program"

                Just program ->
                    Element.Input.multiline [ monospace ]
                        { onChange = UpdatedUserInput
                        , text =
                            case program.versions of
                                [] ->
                                    ""

                                v :: _ ->
                                    v.userInput
                        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type here"
                        , label = Element.Input.labelAbove [ Utils.sansSerif ] <| Element.text "Your input goes here:"
                        , spellcheck = True
                        }


programCode : Maybe String -> Dict.Dict String Utils.Program -> Element.Element Msg
programCode maybeOpenProgram programs =
    case maybeOpenProgram of
        Nothing ->
            Element.none

        Just programName ->
            case Dict.get programName programs of
                Nothing ->
                    Element.text "Internal error: can't find program"

                Just program ->
                    Element.Input.multiline [ monospace ]
                        { onChange = UpdatedProgramEditor
                        , text = program.code
                        , placeholder =
                            Just <|
                                Element.Input.placeholder [] <|
                                    Element.text "Type program here"
                        , label =
                            Element.Input.labelAbove [ Utils.sansSerif ] <|
                                Element.text "Program code:"
                        , spellcheck = False
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


makeNewProgram : Element.Element Msg
makeNewProgram =
    Element.Input.button [ Utils.sansSerif ]
        { onPress = Just MakeNewProgram
        , label = Element.text "Make new program"
        }


chooseAProgram : Dict.Dict String Utils.Program -> Maybe String -> Element.Element Msg
chooseAProgram programs maybeOpenProgram =
    Element.Input.radio [ Element.spacing 12 ]
        { onChange = LaunchProgram
        , selected = maybeOpenProgram
        , label =
            Element.Input.labelAbove [ Utils.sansSerif ] <|
                Element.text "Choose a program:"
        , options = Dict.values <| Dict.map programRadio programs
        }


programRadio :
    String
    -> Utils.Program
    -> Element.Input.Option String Msg
programRadio name program =
    let
        description =
            case program.versions of
                [] ->
                    ""

                v :: _ ->
                    v.description
    in
    Element.Input.option name (programRadioView name description)


programRadioView : String -> String -> Element.Element Msg
programRadioView name description =
    Element.column []
        [ Element.el [ monospace ] <| Element.text name
        , Element.paragraph [ Utils.sansSerif ] [ Element.text description ]
        ]


type AddContactError
    = YouTriedToAddYourself


addNewContact : Maybe Int -> Maybe AddContactError -> Element.Element Msg
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

            Just YouTriedToAddYourself ->
                Element.text "you can't add yourself to your contacts"
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

port module Editor exposing (Model, Msg, initCmd, initModel, subscriptions, update, view)

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


type alias Model =
    { myName : Maybe Int
    , myContacts : List Int
    , addContactBox : Maybe Int
    , addContactError : Maybe AddContactError
    , programs : Dict.Dict String Truelang.Program
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
    }


type InternalError
    = UpdatedUserInputButNoOpenProgram
    | UpdatedUserInputButNoProgram
    | UpdatedProgramButNoOpenProgram
    | UpdatedProgramButNoProgram
    | BadCache Base64.Decode.Error
    | BadDecodeCache String
    | BadEditorCacheDecode Jd.Error


type Msg
    = UpdatedUserInput String
    | UpdatedProgramEditor String
    | MakeNewProgram
    | LaunchProgram String
    | AddNewContact
    | UpdateContactBox String
    | RetrievedEditorInfo Je.Value


toB64 : Bytes.Bytes -> String
toB64 bs =
    Base64.Encode.encode <| Base64.Encode.bytes bs


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


decodeEditorCache : String -> Result String Cache
decodeEditorCache rawString =
    case Base64.Decode.decode Base64.Decode.bytes rawString of
        Err err ->
            Err <| "could not decode editor cache base64: " ++ showB64Error err

        Ok rawBytes ->
            case D.decode cacheDecoder rawBytes of
                Nothing ->
                    Err "could not decode editor cache"

                Just cache ->
                    Ok cache


showB64Error : Base64.Decode.Error -> String
showB64Error error =
    case error of
        Base64.Decode.ValidationError ->
            "validation error"

        Base64.Decode.InvalidByteSequence ->
            "invalid byte sequence"


sizedString : D.Decoder String
sizedString =
    D.unsignedInt32 Bytes.BE
        |> D.andThen D.string


decodeHumanMsg : D.Decoder HumanMsg
decodeHumanMsg =
    D.map3 HumanMsg (D.unsignedInt32 Bytes.BE) (D.unsignedInt32 Bytes.BE) decodeDocument


type alias HumanMsg =
    { from : Int
    , to : Int
    , document : Truelang.Document
    }


decodeDocument : D.Decoder Truelang.Document
decodeDocument =
    D.andThen decodeDocumentHelp D.unsignedInt8


decodeDocumentHelp : Int -> D.Decoder Truelang.Document
decodeDocumentHelp typeNum =
    case typeNum of
        0 ->
            decodeOrdering

        1 ->
            D.map Truelang.SmallString sizedString

        _ ->
            D.fail


decodeOrdering : D.Decoder Truelang.Document
decodeOrdering =
    D.map Truelang.Ordering (list decodeDocument)


cacheDecoder : D.Decoder Cache
cacheDecoder =
    D.map2 Cache
        (list (D.unsignedInt32 Bytes.BE))
        (list decodeProgram)


decodeProgram : D.Decoder Truelang.Program
decodeProgram =
    D.map4 Truelang.Program
        sizedString
        sizedString
        (list decodeHumanMsg)
        sizedString


{-| Pinched from the Bytes documentation.
-}
list : D.Decoder a -> D.Decoder (List a)
list decoder =
    D.unsignedInt32 Bytes.BE
        |> D.andThen
            (\len -> D.loop ( len, [] ) (listStep decoder))


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


combinePrograms : Dict.Dict String Truelang.Program -> List Truelang.Program -> Dict.Dict String Truelang.Program
combinePrograms oldPrograms newPrograms =
    Dict.union oldPrograms (Dict.fromList <| List.map plusHash newPrograms)


plusHash : Truelang.Program -> ( String, Truelang.Program )
plusHash program =
    ( hash program.code, program )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RetrievedEditorInfo jsonValue ->
            case Jd.decodeValue editorInfoDecoder jsonValue of
                Err err ->
                    ( { model | internalError = Just <| BadEditorCacheDecode err }, Cmd.none )

                Ok raw ->
                    case decodeEditorCache raw.editorCache of
                        Err err ->
                            ( { model | internalError = Just <| BadDecodeCache err }, Cmd.none )

                        Ok { newContacts, programs } ->
                            ( { model | programs = combinePrograms model.programs programs, newContacts = model.newContacts ++ newContacts }, Cmd.none )

        UpdatedUserInput newUserInput ->
            case model.openProgram of
                Nothing ->
                    ( { model | internalError = Just UpdatedUserInputButNoOpenProgram }, Cmd.none )

                Just programName ->
                    case Dict.get programName model.programs of
                        Nothing ->
                            ( { model | internalError = Just UpdatedUserInputButNoProgram }, Cmd.none )

                        Just program ->
                            let
                                newProgram =
                                    { program | userInput = newUserInput }

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

                                newPrograms =
                                    Dict.insert programName newProgram model.programs

                                newModel =
                                    { model | programs = newPrograms }
                            in
                            ( newModel, cacheModel newModel )

        MakeNewProgram ->
            let
                newProgram =
                    { code = "", description = "New program", inbox = [], userInput = "" }

                name =
                    hash newProgram.code

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
                    ( newModel, cacheModel newModel )

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
                        ( newModel, cacheModel newModel )


hash : String -> String
hash s =
    SHA256.toBase64 <| SHA256.fromString s


view : Model -> Element.Element Msg
view { myName, myContacts, addContactBox, addContactError, programs, openProgram } =
    Element.column []
        [ myUsernameIs myName
        , myContactsAre myContacts
        , addNewContact addContactBox addContactError
        , chooseAProgram programs openProgram
        , makeNewProgram
        , programOutput openProgram programs myContacts myName
        , myInput openProgram programs
        , programCode openProgram programs
        ]


port cachePort : String -> Cmd msg


cacheModel : Model -> Cmd msg
cacheModel model =
    modelToCache model
        |> encodeCache
        |> toB64
        |> cachePort


type alias Cache =
    { newContacts : List Int
    , programs : List Truelang.Program
    }


modelToCache : Model -> Cache
modelToCache { newContacts, programs } =
    { newContacts = newContacts
    , programs = Dict.values programs
    }


encodeCache : Cache -> Bytes.Bytes
encodeCache cache =
    E.encode <| cacheEncoder cache


cacheEncoder : Cache -> E.Encoder
cacheEncoder { newContacts, programs } =
    E.sequence
        [ encodeContacts newContacts
        , encodePrograms programs
        ]


encodeContacts : List Int -> E.Encoder
encodeContacts contacts =
    E.sequence <|
        (E.unsignedInt32 Bytes.BE <| List.length contacts)
            :: List.map (E.unsignedInt32 Bytes.BE) contacts


encodeProgram : Truelang.Program -> E.Encoder
encodeProgram { code, description, userInput } =
    E.sequence
        [ encodeSizedString code
        , encodeSizedString description
        , encodeSizedString userInput
        ]


encodeSizedString : String -> E.Encoder
encodeSizedString str =
    E.sequence
        [ E.unsignedInt32 Bytes.BE (E.getStringWidth str)
        , E.string str
        ]


encodeList : List a -> (a -> E.Encoder) -> E.Encoder
encodeList toEncode elementEncoder =
    E.sequence <|
        (E.unsignedInt32 Bytes.BE <| List.length toEncode)
            :: List.map elementEncoder toEncode


encodePrograms : List Truelang.Program -> E.Encoder
encodePrograms programs =
    encodeList programs encodeProgram


myInput : Maybe String -> Dict.Dict String Truelang.Program -> Element.Element Msg
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
                        , text = program.userInput
                        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type here"
                        , label = Element.Input.labelAbove [ sansSerif ] <| Element.text "Your input goes here:"
                        , spellcheck = True
                        }


programCode : Maybe String -> Dict.Dict String Truelang.Program -> Element.Element Msg
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
                            Element.Input.labelAbove [ sansSerif ] <|
                                Element.text "Program code:"
                        , spellcheck = False
                        }


sansSerif : Element.Attribute Msg
sansSerif =
    Font.family [ Font.typeface "Ubuntu" ]


monospace : Element.Attribute Msg
monospace =
    Font.family [ Font.typeface "Ubuntu Mono" ]


programOutput :
    Maybe String
    -> Dict.Dict String Truelang.Program
    -> List Int
    -> Maybe Int
    -> Element.Element Msg
programOutput maybeOpenProgram programs contacts maybeMyName =
    case ( maybeOpenProgram, maybeMyName ) of
        ( Nothing, _ ) ->
            Element.none

        ( _, Nothing ) ->
            Element.text "no username, so can't run program"

        ( Just programName, Just myName ) ->
            let
                ( maybeOutput, _ ) =
                    Truelang.runProgram programName programs myName
            in
            case maybeOutput of
                Nothing ->
                    Element.text "this program produces no output"

                Just output ->
                    displayDocument output


displayDocument : Truelang.Document -> Element.Element Msg
displayDocument document =
    case document of
        Truelang.Ordering documents ->
            Element.column [] <| List.map displayDocument documents

        Truelang.SmallString s ->
            Element.text s


makeNewProgram : Element.Element Msg
makeNewProgram =
    Element.Input.button []
        { onPress = Just MakeNewProgram
        , label = Element.text "Make new program"
        }


chooseAProgram : Dict.Dict String Truelang.Program -> Maybe String -> Element.Element Msg
chooseAProgram programs maybeOpenProgram =
    Element.Input.radio []
        { onChange = LaunchProgram
        , selected = maybeOpenProgram
        , label =
            Element.Input.labelAbove [] <|
                Element.text "Choose a program"
        , options = Dict.values <| Dict.map programRadio programs
        }


programRadio : String -> Truelang.Program -> Element.Input.Option String Msg
programRadio name { description } =
    Element.Input.option name (programRadioView name description)


programRadioView : String -> String -> Element.Element Msg
programRadioView name description =
    Element.column []
        [ Element.text name
        , Element.paragraph [] [ Element.text description ]
        ]


type AddContactError
    = YouTriedToAddYourself


addNewContact : Maybe Int -> Maybe AddContactError -> Element.Element Msg
addNewContact boxContents maybeError =
    Element.column [] <|
        [ Element.Input.text []
            { onChange = UpdateContactBox
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
                Element.Input.labelAbove [ sansSerif ] <|
                    Element.text "Add someone to your contacts"
            }
        , Element.Input.button []
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
    Element.text <|
        "My contacts: "
            ++ String.join ", " (List.map String.fromInt contacts)


myUsernameIs : Maybe Int -> Element.Element Msg
myUsernameIs maybeMyName =
    Element.text <|
        case maybeMyName of
            Nothing ->
                "you haven't got one yet"

            Just nameInt ->
                String.fromInt nameInt

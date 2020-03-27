port module Importer exposing (Model, Msg, initCmd, initModel, subscriptions, update, view)

import Base64.Decode
import Bytes
import Bytes.Decode as D
import Dict
import Element
import Element.Input
import Json.Decode as Jd
import Json.Encode as Je
import Utils


port getImporterInfo : () -> Cmd msg


port gotImporterInfo : (Je.Value -> msg) -> Sub msg


port cacheEditorInfo : String -> Cmd msg


port clearInbox : () -> Cmd msg


type alias Model =
    { programs : Dict.Dict String Utils.Program
    , inbox : List MsgIn
    , internalError : Maybe String
    }


type alias MsgIn =
    { from : Int
    , program : Utils.Program
    }


type Msg
    = GotImporterInfo Je.Value
    | ImportMessages


initCmd : Cmd Msg
initCmd =
    getImporterInfo ()


initModel : Model
initModel =
    { programs = Dict.empty
    , inbox = []
    , internalError = Nothing
    }


subscriptions =
    gotImporterInfo GotImporterInfo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImportMessages ->
            ( model, getImporterInfo () )

        GotImporterInfo jsonValue ->
            case Jd.decodeValue rawImporterInfoDecoder jsonValue of
                Err err ->
                    ( { model | internalError = Just <| Jd.errorToString err }, Cmd.none )

                Ok { editorCache, inbox } ->
                    case ( Utils.decodeEditorCache editorCache, decodeInbox inbox ) of
                        ( Err err, _ ) ->
                            ( { model | internalError = Just err }, Cmd.none )

                        ( _, Err err ) ->
                            ( { model | internalError = Just err }, Cmd.none )

                        ( Ok dEditorCache, Ok dInbox ) ->
                            let
                                newPrograms =
                                    updatePrograms dEditorCache.programs dInbox

                                newCache =
                                    { dEditorCache | programs = newPrograms }
                            in
                            ( model, cacheEditorCache newCache )


decodeInbox : String -> Result String (List Utils.HumanMsg)
decodeInbox rawB64 =
    if rawB64 == "There is no inbox!" then
        Ok []

    else
        case Base64.Decode.decode Base64.Decode.bytes rawB64 of
            Err err ->
                Err <| "could not decode inbox: " ++ Utils.showB64Error err

            Ok rawBytes ->
                case D.decode inboxDecoder rawBytes of
                    Nothing ->
                        Err "could not decode inbox bytes"

                    Just inbox ->
                        Ok inbox


inboxDecoder : D.Decoder (List Utils.HumanMsg)
inboxDecoder =
    Utils.list decodeMsgIn


decodeMsgIn : D.Decoder Utils.HumanMsg
decodeMsgIn =
    decodeAuthor |> D.andThen decodeMsgInHelp


decodeAuthor : D.Decoder Int
decodeAuthor =
    D.map Tuple.second <|
        D.map2 (\a b -> ( a, b ))
            (D.bytes 4)
            (D.unsignedInt32 Bytes.BE)


decodeMsgInHelp : Int -> D.Decoder Utils.HumanMsg
decodeMsgInHelp author =
    Utils.decodeHumanMsg
        |> D.andThen (checkAuthorsMatch author)


checkAuthorsMatch : Int -> Utils.HumanMsg -> D.Decoder Utils.HumanMsg
checkAuthorsMatch author msg =
    if author == msg.version.author then
        D.succeed msg

    else
        D.fail


programsToDict : List Utils.Program -> Dict.Dict String Utils.Program
programsToDict programs =
    Dict.fromList <| List.map (\p -> ( Utils.hash p.code, p )) programs


updatePrograms : List Utils.Program -> List Utils.HumanMsg -> List Utils.Program
updatePrograms oldProgramsList inbox =
    let
        oldPrograms =
            programsToDict oldProgramsList

        newPrograms =
            List.foldr insertMessage oldPrograms inbox
    in
    Dict.values newPrograms


insertMessage : Utils.HumanMsg -> Dict.Dict String Utils.Program -> Dict.Dict String Utils.Program
insertMessage { code, version } oldPrograms =
    let
        msgHash =
            Utils.hash code
    in
    case Dict.get msgHash oldPrograms of
        Nothing ->
            let
                newProgram =
                    { code = code, versions = [ version ] }
            in
            Dict.insert msgHash newProgram oldPrograms

        Just oldProgram ->
            let
                updatedProgram =
                    { oldProgram | versions = version :: oldProgram.versions }
            in
            Dict.insert msgHash updatedProgram oldPrograms


cacheEditorCache : Utils.Cache -> Cmd msg
cacheEditorCache cache =
    cacheEditorInfo <| Utils.toB64 <| Utils.encodeCache cache


view : Model -> Element.Element Msg
view model =
    case model.internalError of
        Just err ->
            Element.text err

        Nothing ->
            Element.Input.button []
                { onPress = Just ImportMessages
                , label = Element.text "Import messages"
                }


type alias RawImporterJson =
    { editorCache : String
    , inbox : String
    }


rawImporterInfoDecoder : Jd.Decoder RawImporterJson
rawImporterInfoDecoder =
    Jd.map2 RawImporterJson
        Jd.string
        Jd.string

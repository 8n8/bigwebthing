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


port cacheEditorInfoImporter : String -> Cmd msg


port clearInbox : () -> Cmd msg


type alias Model =
    { inbox : List MsgIn
    , internalError : Maybe String
    , programs : Dict.Dict String Utils.Program
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
    Cmd.none


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
                    ( { model | internalError = Just <| "could not decode Importer JSON: " ++ Jd.errorToString err }, Cmd.none )

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
                                    { dEditorCache | programs = Dict.values newPrograms }
                            in
                            ( { model | programs = newPrograms }, cacheEditorCache newCache )


allOks : List (Result a b) -> Result a (List b)
allOks results =
    List.foldr allOksHelp (Ok []) results


allOksHelp : Result a b -> Result a (List b) -> Result a (List b)
allOksHelp result accum =
    case result of
        Ok b ->
            case accum of
                Ok bs ->
                    Ok <| b :: bs

                Err err ->
                    Err err

        Err a ->
            Err a


toBytes : String -> Result Base64.Decode.Error Bytes.Bytes
toBytes b64 =
    Base64.Decode.decode Base64.Decode.bytes b64


decodeInbox : List String -> Result String (List SendThis)
decodeInbox rawB64s =
    case allOks <| List.map toBytes rawB64s of
        Err err ->
            Err <| "could not decode inbox: " ++ Utils.showB64Error err

        Ok rawBytes ->
            Ok <| Utils.justs <| List.map inboxDecodeHelp rawBytes


inboxDecodeHelp rawMsgBytes =
    D.decode decodeSendThis rawMsgBytes


decodeAuthor : D.Decoder Int
decodeAuthor =
    D.map Tuple.second <|
        D.map2 (\a b -> ( a, b ))
            (D.bytes 4)
            (D.unsignedInt32 Bytes.LE)


type alias SendThis =
    { from : Int
    , code : String
    , version : Utils.Version
    }


decodeSendThis : D.Decoder SendThis
decodeSendThis =
    D.map3 SendThis
        (D.unsignedInt32 Bytes.LE)
        Utils.sizedString
        Utils.decodeVersion


programsToDict : List Utils.Program -> Dict.Dict String Utils.Program
programsToDict programs =
    Dict.fromList <| List.map (\p -> ( Utils.hash p.code, p )) programs


updatePrograms : List Utils.Program -> List SendThis -> Dict.Dict String Utils.Program
updatePrograms oldProgramsList inbox =
    let
        oldPrograms =
            programsToDict oldProgramsList
    in
    List.foldr insertMessage oldPrograms inbox


insertMessage : SendThis -> Dict.Dict String Utils.Program -> Dict.Dict String Utils.Program
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
    cacheEditorInfoImporter <| Utils.toB64 <| Utils.encodeCache cache


view : Model -> Element.Element Msg
view model =
    case model.internalError of
        Just err ->
            Element.text <| "internal error: " ++ err

        Nothing ->
            Element.Input.button []
                { onPress = Just ImportMessages
                , label = Element.text "Import messages"
                }


type alias RawImporterJson =
    { editorCache : String
    , inbox : List String
    }


rawImporterInfoDecoder : Jd.Decoder RawImporterJson
rawImporterInfoDecoder =
    Jd.map2 RawImporterJson
        (Jd.field "editorCache" Jd.string)
        (Jd.field "inbox" <| Jd.list Jd.string)

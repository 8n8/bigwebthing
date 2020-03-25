port module Generator exposing
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


type Msg
    = GotGeneratorData Je.Value
    | GenerateMessages


type alias RawCache =
    { editorCache : String
    , myName : Int
    , receipts : String
    }


type alias Cache =
    { myName : Maybe Int
    , receipts : List Utils.Receipt
    , editorCache : Utils.Cache
    }


decodeMyName : Int -> Maybe Int
decodeMyName i =
    if i < 0 then
        Nothing

    else
        Just i


rawCacheDecoder : Jd.Decoder RawCache
rawCacheDecoder =
    Jd.map3 RawCache
        (Jd.field "editorCache" Jd.string)
        (Jd.field "myName" Jd.int)
        (Jd.field "receipts" Jd.string)


decodeCache : Je.Value -> Result String Cache
decodeCache json =
    case Jd.decodeValue rawCacheDecoder json of
        Err err ->
            Err <| "JSON decoding error: " ++ Jd.errorToString err

        Ok { myName, receipts, editorCache } ->
            case
                ( Utils.decodeReceipts receipts
                , Utils.decodeEditorCache editorCache
                )
            of
                ( Err err, _ ) ->
                    Err <| "Bad receipts: " ++ err

                ( _, Err err ) ->
                    Err <| "Bad editor cache: " ++ err

                ( Ok dReceipts, Ok dCache ) ->
                    Ok
                        { myName = decodeMyName myName
                        , receipts = dReceipts
                        , editorCache = dCache
                        }


type alias Model =
    { internalError : Maybe String
    , myName : Maybe Int
    }


initModel =
    { internalError = Nothing
    , myName = Nothing
    }


initCmd = Cmd.none


port gotGeneratorData : (Je.Value -> msg) -> Sub msg


port getGeneratorData : () -> Cmd msg


subscriptions =
    gotGeneratorData GotGeneratorData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateMessages ->
            ( model, getGeneratorData () )

        GotGeneratorData rawB64 ->
            case decodeCache rawB64 of
                Err err ->
                    ( { model | internalError = Just err }
                    , Cmd.none
                    )

                Ok cache ->
                    let

                        allMessages =
                            makeMessages cache
                    in
                    ( model, cacheMessages allMessages )


notSentYet :
    List Utils.HumanMsg
    -> List Utils.Receipt
    -> List Utils.HumanMsg
notSentYet messages receipts =
    List.filter (not << hasBeenSent receipts) messages


hasBeenSent : List Utils.Receipt -> Utils.HumanMsg -> Bool
hasBeenSent receipts message =
    List.any (receiptMatchesMessage message) receipts


receiptMatchesMessage : Utils.HumanMsg -> Utils.Receipt -> Bool
receiptMatchesMessage message receipt =
    (message.to == receipt.recipient)
        && (Utils.hashDocument message.document == receipt.hash)


makeMessages : Cache -> List Utils.MsgOut
makeMessages { myName, receipts, editorCache } =
    let
        humanMessages =
            case myName of
                Nothing ->
                    []

                Just dMyName ->
                    runPrograms editorCache.programs dMyName

        reduced =
            notSentYet humanMessages receipts

        allMsgs =
            List.map Utils.SendThis reduced
                ++ List.map
                    Utils.WhitelistSomeone
                    editorCache.newContacts
    in
    allMsgs


port cacheMessagesPort : String -> Cmd msg


cacheMessages : List Utils.MsgOut -> Cmd Msg
cacheMessages messages =
    cacheMessagesPort <|
        Base64.Encode.encode <|
            Base64.Encode.bytes <|
                E.encode <|
                    messagesEncoder messages


messagesEncoder : List Utils.MsgOut -> E.Encoder
messagesEncoder messages =
    E.sequence <|
        (E.unsignedInt32 Bytes.BE <| List.length messages)
            :: List.map encodeMessage messages


encodeMessage : Utils.MsgOut -> E.Encoder
encodeMessage message =
    case message of
        Utils.MakeMyName ->
            E.unsignedInt8 0

        Utils.WhitelistSomeone id ->
            E.sequence
                [ E.unsignedInt8 1
                , E.unsignedInt32 Bytes.BE id
                ]

        Utils.SendThis humanMsg ->
            E.sequence
                [ E.unsignedInt8 2
                , Utils.encodeHumanMsg humanMsg
                ]


runPrograms : List Utils.Program -> Int -> List Utils.HumanMsg
runPrograms programs myName =
    List.concatMap
        (runProgram myName <| programsToDict programs)
        programs


runProgram :
    Int
    -> Dict.Dict String Utils.Program
    -> Utils.Program
    -> List Utils.HumanMsg
runProgram myName programs program =
    Tuple.second <|
        Truelang.runProgram program.code programs myName


hash : String -> String
hash s =
    SHA256.toBase64 <| SHA256.fromString s


programsToDict :
    List Utils.Program
    -> Dict.Dict String Utils.Program
programsToDict programs =
    Dict.fromList <| List.map (\p -> ( hash p.code, p )) programs


view : Model -> Element.Element Msg
view model =
    Element.el [ Utils.sansSerif, Utils.fontSize ] <|
        case model.internalError of
            Just err ->
                Element.text err

            Nothing ->
                Element.Input.button []
                    { onPress = Just GenerateMessages
                    , label = Element.text "Generate messages"
                    }

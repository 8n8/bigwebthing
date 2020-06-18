port module Main exposing (main)

import Base64.Decode as B64d
import Base64.Encode as B64e
import Browser
import Bytes
import Bytes.Encode as Be
import Html
import Json.Decode as Jd
import Json.Encode as Je


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type GetMe
    = KeysFromCacheG
    | GeneratedKeysG
    | PowInfoG MyKeys
    | PowG MyKeys
    | NameFromServerG MyKeys


type AuthCode
    = AuthCode Bytes.Bytes


type alias Model =
    { myId : Maybe ( MyName, MyKeys )
    , processes : List Process
    , fatal : Maybe String
    }


type alias MyKeys =
    { encryption : KeyPair
    , signing : KeyPair
    }


type alias KeyPair =
    { public : Bytes.Bytes
    , secret : Bytes.Bytes
    }


type MyName
    = MyName Int


type Process
    = GetMeP GetMe


initModel : Model
initModel =
    { myId = Nothing
    , processes = [ GetMeP KeysFromCacheG ]
    , fatal = Nothing
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel, Cmd.none )


view : Model -> Html.Html Msg
view _ =
    Html.text "Not done yet"


type Simple
    = Simple


type ForProcess
    = KeysFromCacheF MyKeys
    | GeneratedKeysF MyKeys
    | AuthCodeF AuthCode
    | PowInfoF PowInfo
    | PowF Pow
    | NameFromServerF MyName


port elmToJs : Je.Value -> Cmd msg


port jsToElm : (Je.Value -> msg) -> Sub msg


type Pow
    = Pow Bytes.Bytes


type alias PowInfo =
    { unique : Bytes.Bytes
    , difficulty : Int
    }


jsKeyVal : String -> Je.Value -> Je.Value
jsKeyVal key value =
    Je.object [ ( "key", Je.string key ), ( "value", value ) ]


encodeToJs : ElmToJs -> Je.Value
encodeToJs value =
    case value of
        WebsocketE bytes ->
            jsKeyVal
                "toWebsocket"
                (Je.string <| B64e.encode <| B64e.bytes bytes)

        GetPowE powInfo ->
            jsKeyVal "getPow" (encodePowInfo powInfo)


encodePowInfo : PowInfo -> Je.Value
encodePowInfo { unique, difficulty } =
    Je.object
        [ ( "unique", Je.string <| B64e.encode <| B64e.bytes unique )
        , ( "difficulty", Je.int difficulty )
        ]


type ElmToJs
    = WebsocketE Bytes.Bytes
    | GetPowE PowInfo


type JsToElm
    = MyKeysFromCacheJ MyKeys


getAuthCode : Cmd Msg
getAuthCode =
    elmToJs <|
        encodeToJs <|
            WebsocketE <|
                Be.encode <|
                    Be.unsignedInt8 7


type Msg
    = ForProcessM ForProcess
    | SimpleM Simple
    | JsonFromJsM Je.Value


{-| Each piece of data from Javascript is a piece of JSON with fields
'key' and 'value'.
-}
decodeFromJs : Je.Value -> Result String JsToElm
decodeFromJs json =
    case Jd.decodeValue fromJsDecoder json of
        Err err ->
            Err <| Jd.errorToString err

        Ok ok ->
            Ok ok


fromJsDecoder : Jd.Decoder JsToElm
fromJsDecoder =
    Jd.andThen fromJsDecoderHelp <| Jd.field "key" Jd.string


fromJsDecoderHelp : String -> Jd.Decoder JsToElm
fromJsDecoderHelp key =
    case key of
        "fromCache" ->
            fromCacheDecoder

        badKey ->
            Jd.fail <| "bad key: \"" ++ badKey ++ "\""


fromCacheDecoder : Jd.Decoder JsToElm
fromCacheDecoder =
    Jd.andThen fromCacheDecoderHelp <| Jd.field "key" Jd.string


fromCacheDecoderHelp : String -> Jd.Decoder JsToElm
fromCacheDecoderHelp key =
    case key of
        "myKeys" ->
            myKeysFromCacheDecoder

        badKey ->
            Jd.fail <| "bad cache response key \"" ++ badKey ++ "\""


myKeysFromCacheDecoder : Jd.Decoder JsToElm
myKeysFromCacheDecoder =
    Jd.map2 (\e s -> MyKeysFromCacheJ <| MyKeys e s)
        (Jd.field "encryption" encryptionKeysDecoder)
        (Jd.field "signing" signingKeysDecoder)


signingKeysDecoder : Jd.Decoder KeyPair
signingKeysDecoder =
    Jd.map2 KeyPair
        (Jd.field "secretKey" key64)
        (Jd.field "publicKey" key32)


key64 : Jd.Decoder Bytes.Bytes
key64 =
    Jd.andThen (bytesDecoder 64) Jd.string


key32 : Jd.Decoder Bytes.Bytes
key32 =
    Jd.andThen (bytesDecoder 32) Jd.string


encryptionKeysDecoder : Jd.Decoder KeyPair
encryptionKeysDecoder =
    Jd.map2 KeyPair
        (Jd.field "secretKey" key32)
        (Jd.field "publicKey" key32)


showB64Error : B64d.Error -> String
showB64Error error =
    case error of
        B64d.ValidationError ->
            "Validation error"

        B64d.InvalidByteSequence ->
            "Invalid byte sequence"


bytesDecoder : Int -> String -> Jd.Decoder Bytes.Bytes
bytesDecoder length raw =
    case B64d.decode B64d.bytes raw of
        Err err ->
            Jd.fail <| showB64Error err

        Ok bytes ->
            let
                bytesLen =
                    Bytes.width bytes
            in
            if bytesLen == length then
                Jd.succeed bytes

            else
                Jd.fail <|
                    "expecting "
                        ++ String.fromInt length
                        ++ " bytes, but got "
                        ++ String.fromInt bytesLen


toMsg : JsToElm -> Msg
toMsg j =
    case j of
        MyKeysFromCacheJ myKeys ->
            ForProcessM <| KeysFromCacheF myKeys


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        JsonFromJsM json ->
            case decodeFromJs json of
                Err err ->
                    ( { model | fatal = Just err }, Cmd.none )

                Ok fromJs ->
                    update (toMsg fromJs) model

        ForProcessM forProcess ->
            router [] forProcess model

        SimpleM simple ->
            updateSimple simple model


updateSimple : Simple -> Model -> ( Model, Cmd Msg )
updateSimple _ model =
    ( model, Cmd.none )


type ProcessTick
    = FinishedT Model (Cmd Msg)
    | ContinuingT Model (Cmd Msg) Process
    | NotUsedMessageT


updateGetMe : GetMe -> ForProcess -> Model -> ProcessTick
updateGetMe getMe msg model =
    case ( getMe, msg ) of
        ( KeysFromCacheG, KeysFromCacheF myKeys ) ->
            ContinuingT
                model
                getPowInfo
                (GetMeP <| PowInfoG myKeys)

        ( KeysFromCacheG, _ ) ->
            NotUsedMessageT

        ( GeneratedKeysG, GeneratedKeysF myKeys ) ->
            ContinuingT
                model
                getPowInfo
                (GetMeP <| PowInfoG myKeys)

        ( GeneratedKeysG, _ ) ->
            NotUsedMessageT

        ( PowInfoG myKeys, PowInfoF powInfo ) ->
            ContinuingT
                model
                (getPow powInfo)
                (GetMeP <| PowG myKeys)

        ( PowInfoG _, _ ) ->
            NotUsedMessageT

        ( PowG myKeys, PowF pow ) ->
            ContinuingT
                model
                (elmToJs <| makeNameRequest pow myKeys)
                (GetMeP <| NameFromServerG myKeys)

        ( PowG _, _ ) ->
            NotUsedMessageT

        ( NameFromServerG myKeys, NameFromServerF myName ) ->
            FinishedT
                { model | myId = Just ( myName, myKeys ) }
                Cmd.none

        ( NameFromServerG _, _ ) ->
            NotUsedMessageT


getPowInfo : Cmd Msg
getPowInfo =
    elmToJs <|
        encodeToJs <|
            WebsocketE <|
                Be.encode <|
                    Be.unsignedInt8 3


getPow : PowInfo -> Cmd Msg
getPow powInfo =
    elmToJs <| encodeToJs <| GetPowE powInfo


makeNameRequest : Pow -> MyKeys -> Je.Value
makeNameRequest (Pow pow) { encryption, signing } =
    encodeToJs <|
        WebsocketE <|
            Be.encode <|
                Be.sequence
                    [ Be.unsignedInt8 1
                    , Be.bytes pow
                    , Be.bytes signing.public
                    , Be.bytes encryption.public
                    ]


processTick : Process -> ForProcess -> Model -> ProcessTick
processTick p msg model =
    case p of
        GetMeP getMe ->
            updateGetMe getMe msg model


router : List Process -> ForProcess -> Model -> ( Model, Cmd Msg )
router notRelevant msg model =
    case model.processes of
        [] ->
            ( model, Cmd.none )

        p :: rocesses ->
            case processTick p msg model of
                FinishedT newModel cmd ->
                    ( { newModel
                        | processes = rocesses ++ notRelevant
                      }
                    , cmd
                    )

                ContinuingT newModel cmd newP ->
                    ( { newModel
                        | processes = newP :: rocesses ++ notRelevant
                      }
                    , cmd
                    )

                NotUsedMessageT ->
                    router (p :: notRelevant) msg model


subscriptions : Model -> Sub Msg
subscriptions _ =
    jsToElm JsonFromJsM

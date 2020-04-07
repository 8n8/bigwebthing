module Utils exposing (..)

import Base64.Decode
import Base64.Encode
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Element
import Element.Font as Font
import Json.Decode as Jd
import Json.Encode as Je
import SHA256


sansSerif : Element.Attribute msg
sansSerif =
    Font.family [ Font.typeface "EB Garamond" ]


fontSize =
    Font.size 25


hash : String -> String
hash s =
    SHA256.toBase64 <| SHA256.fromString s


combineResult : List (Result a b) -> Result a (List b)
combineResult results =
    List.foldr combineHelp (Ok []) results


combineHelp : Result a b -> Result a (List b) -> Result a (List b)
combineHelp result accum =
    case ( result, accum ) of
        ( Ok b, Ok bs ) ->
            Ok <| b :: bs

        ( Ok _, Err a ) ->
            Err a

        ( Err a, _ ) ->
            Err a


decodeReceipts : List String -> Result String (List Receipt)
decodeReceipts raw =
    combineResult <| List.map decodeReceipt raw


decodeReceipt : String -> Result String Receipt
decodeReceipt rawB64 =
    case Base64.Decode.decode Base64.Decode.bytes rawB64 of
        Err err ->
            Err <|
                "could not decode receipt base64: "
                    ++ showB64Error err

        Ok rawBytes ->
            case D.decode receiptDecoder rawBytes of
                Nothing ->
                    Err "could not decode receipt bytes"

                Just receipt ->
                    Ok receipt


type alias Receipt =
    { recipient : Int
    , signature : Bytes.Bytes
    , hash : Bytes.Bytes
    }


receiptsDecoder : D.Decoder (List Receipt)
receiptsDecoder =
    list receiptDecoder


receiptDecoder : D.Decoder Receipt
receiptDecoder =
    D.map3 Receipt
        (D.unsignedInt32 Bytes.LE)
        (D.bytes 96)
        (D.bytes 32)


encodeDocument : Document -> E.Encoder
encodeDocument doc =
    let
        bytes =
            E.encode <| encodeDocumentHelp doc

        length =
            Bytes.width bytes
    in
    E.sequence
        [ E.unsignedInt32 Bytes.LE length
        , E.bytes bytes
        ]


encodeDocumentHelp : Document -> E.Encoder
encodeDocumentHelp doc =
    case doc of
        Ordering docs ->
            E.sequence <|
                [ E.unsignedInt8 2
                , E.unsignedInt32 Bytes.LE <| List.length docs
                ]
                    ++ List.map encodeDocument docs

        SmallString s ->
            E.sequence <|
                [ E.unsignedInt8 3
                , encodeSizedString s
                ]


encodeSizedString : String -> E.Encoder
encodeSizedString str =
    E.sequence
        [ E.unsignedInt32 Bytes.LE (E.getStringWidth str)
        , E.string str
        ]


type alias Cache =
    { newContacts : List Int
    , programs : List Program
    }


encodeCache : Cache -> Bytes.Bytes
encodeCache cache =
    E.encode <| cacheEncoder cache


encodeContacts : List Int -> E.Encoder
encodeContacts contacts =
    E.sequence <|
        (E.unsignedInt32 Bytes.LE <| List.length contacts)
            :: List.map (E.unsignedInt32 Bytes.LE) contacts


encodeProgram : Program -> E.Encoder
encodeProgram { code, versions } =
    E.sequence
        [ encodeSizedString code
        , encodeList versions encodeVersion
        ]


encodeHumanMsgs : List HumanMsg -> E.Encoder
encodeHumanMsgs msgs =
    encodeList msgs encodeHumanMsg


encodeList : List a -> (a -> E.Encoder) -> E.Encoder
encodeList toEncode elementEncoder =
    E.sequence <|
        (E.unsignedInt32 Bytes.LE <| List.length toEncode)
            :: List.map elementEncoder toEncode


toB64 : Bytes.Bytes -> String
toB64 bs =
    Base64.Encode.encode <| Base64.Encode.bytes bs


encodePrograms : List Program -> E.Encoder
encodePrograms programs =
    encodeList programs encodeProgram


cacheEncoder : Cache -> E.Encoder
cacheEncoder { newContacts, programs } =
    E.sequence
        [ encodeContacts newContacts
        , encodePrograms programs
        ]


type alias Program =
    { code : String
    , versions : List Version
    }


type alias Version =
    { description : String
    , userInput : String
    , author : Int
    }


type alias HumanMsg =
    { to : Int
    , code : String
    , version : Version
    }


justs : List (Maybe a) -> List a
justs maybes =
    List.foldr justsHelp [] maybes


justsHelp : Maybe a -> List a -> List a
justsHelp maybe accum =
    case maybe of
        Just a ->
            a :: accum

        Nothing ->
            accum


hashHumanMsg : HumanMsg -> Bytes.Bytes
hashHumanMsg humanMsg =
    SHA256.toBytes <|
        SHA256.fromBytes <|
            E.encode <|
                encodeHumanMsg humanMsg


type Document
    = Ordering (List Document)
    | SmallString String


type MsgOut
    = MakeMyName
    | WhitelistSomeone Int
    | SendThis HumanMsg


hashDocument : Document -> Bytes.Bytes
hashDocument document =
    SHA256.toBytes <|
        SHA256.fromBytes <|
            E.encode <|
                encodeDocument document


encodeHumanMsg : HumanMsg -> E.Encoder
encodeHumanMsg { to, code, version } =
    E.sequence
        [ E.unsignedInt32 Bytes.LE to
        , encodeSizedString code
        , encodeVersion version
        ]


encodeVersion : Version -> E.Encoder
encodeVersion { description, userInput, author } =
    E.sequence
        [ encodeSizedString description
        , encodeSizedString userInput
        , E.unsignedInt32 Bytes.LE author
        ]


decodeEditorCache : String -> Result String Cache
decodeEditorCache rawString =
    if rawString == "There is no cache!" then
        Ok { newContacts = [], programs = [] }

    else
        case Base64.Decode.decode Base64.Decode.bytes rawString of
            Err err ->
                Err <| "could not decode editor cache base64: " ++ showB64Error err

            Ok rawBytes ->
                case D.decode cacheDecoder rawBytes of
                    Nothing ->
                        Err "could not decode editor cache"

                    Just cache ->
                        Ok cache


cacheDecoder : D.Decoder Cache
cacheDecoder =
    D.map2 Cache
        (list (D.unsignedInt32 Bytes.LE))
        (list decodeProgram)


decodeProgram : D.Decoder Program
decodeProgram =
    D.map2 Program
        sizedString
        (list decodeVersion)


sizedString : D.Decoder String
sizedString =
    D.unsignedInt32 Bytes.LE
        |> D.andThen D.string


decodeHumanMsg : D.Decoder HumanMsg
decodeHumanMsg =
    D.map (\{ to, code, version } -> { to = to, code = code, version = version }) <|
        D.map3 HumanMsg
            (D.unsignedInt32 Bytes.LE)
            sizedString
            decodeVersion


decodeVersion : D.Decoder Version
decodeVersion =
    D.map3 Version
        sizedString
        sizedString
        (D.unsignedInt32 Bytes.LE)


decodeDocument : D.Decoder Document
decodeDocument =
    D.andThen decodeDocumentHelp D.unsignedInt8


decodeOrdering : D.Decoder Document
decodeOrdering =
    D.map Ordering (list decodeDocument)


decodeDocumentHelp : Int -> D.Decoder Document
decodeDocumentHelp typeNum =
    case typeNum of
        0 ->
            decodeOrdering

        1 ->
            D.map SmallString sizedString

        _ ->
            D.fail


{-| Pinched from the Bytes documentation.
-}
list : D.Decoder a -> D.Decoder (List a)
list decoder =
    D.unsignedInt32 Bytes.LE
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


showB64Error : Base64.Decode.Error -> String
showB64Error error =
    case error of
        Base64.Decode.ValidationError ->
            "validation error"

        Base64.Decode.InvalidByteSequence ->
            "invalid byte sequence"

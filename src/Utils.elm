module Utils exposing (..)

import Base64.Decode
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Element
import Element.Font as Font
import SHA256


sansSerif : Element.Attribute msg
sansSerif =
    Font.family [ Font.typeface "Ubuntu" ]


fontSize =
    Font.size 25


decodeReceipts : String -> Result String (List Receipt)
decodeReceipts rawB64 =
    if rawB64 == "There are no receipts!" then
        Ok []

    else
        case Base64.Decode.decode Base64.Decode.bytes rawB64 of
            Err err ->
                Err <| "could not decode receipts base64: " ++ showB64Error err

            Ok rawBytes ->
                case D.decode receiptsDecoder rawBytes of
                    Nothing ->
                        Err "could not decode receipts cache"

                    Just receipts ->
                        Ok receipts


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
        (D.unsignedInt32 Bytes.BE)
        (D.bytes 96)
        (D.bytes 32)


encodeDocument : Document -> E.Encoder
encodeDocument doc =
    case doc of
        Ordering docs ->
            E.sequence <|
                [ E.unsignedInt8 2
                , E.unsignedInt32 Bytes.BE <| List.length docs
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
        [ E.unsignedInt32 Bytes.BE (E.getStringWidth str)
        , E.string str
        ]


type alias Cache =
    { newContacts : List Int
    , programs : List Program
    }


type alias Program =
    { code : String
    , description : String
    , inbox : List HumanMsg
    , userInput : String
    }


type alias HumanMsg =
    { from : Int
    , to : Int
    , document : Document
    }


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
encodeHumanMsg { from, to, document } =
    E.sequence
        [ E.unsignedInt32 Bytes.BE from
        , E.unsignedInt32 Bytes.BE to
        , encodeDocument document
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
        (list (D.unsignedInt32 Bytes.BE))
        (list decodeProgram)


decodeProgram : D.Decoder Program
decodeProgram =
    D.map4 Program
        sizedString
        sizedString
        (list decodeHumanMsg)
        sizedString


sizedString : D.Decoder String
sizedString =
    D.unsignedInt32 Bytes.BE
        |> D.andThen D.string


decodeHumanMsg : D.Decoder HumanMsg
decodeHumanMsg =
    D.map3 HumanMsg (D.unsignedInt32 Bytes.BE) (D.unsignedInt32 Bytes.BE) decodeDocument


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


showB64Error : Base64.Decode.Error -> String
showB64Error error =
    case error of
        Base64.Decode.ValidationError ->
            "validation error"

        Base64.Decode.InvalidByteSequence ->
            "invalid byte sequence"

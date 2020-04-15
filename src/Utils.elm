module Utils exposing (..)

import Base64.Decode
import Base64.Encode
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Dict
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


onlyIfAllJust : List (Maybe a) -> Maybe (List a)
onlyIfAllJust maybes =
    List.foldr onlyIfAllJustHelp (Just []) maybes


type alias Message =
    { from : Int
    , to : Int
    , time : Int
    , subject : String
    , userInput : String
    , code : Dict.Dict String String
    , blobs : Dict.Dict String Bytes.Bytes
    }


type alias Draft =
    { to : Maybe Int
    , time : Int
    , subject : String
    , userInput : String
    , code : Dict.Dict String String
    , blobs : Dict.Dict String Bytes.Bytes
    }


encodeDraft : Draft -> E.Encoder
encodeDraft { to, time, subject, userInput, code, blobs } =
    E.sequence
        [ encodeMaybe to (E.unsignedInt32 Bytes.LE)
        , E.unsignedInt32 Bytes.LE time
        , encodeSizedString subject
        , encodeSizedString userInput
        , encodeList (Dict.toList code) encodeCode
        , encodeList (Dict.toList blobs) encodeBlob
        ]


decodeDraft : D.Decoder Draft
decodeDraft =
    map6 Draft
        (decodeMaybe <| D.unsignedInt32 Bytes.LE)
        (D.unsignedInt32 Bytes.LE)
        sizedString
        sizedString
        (D.map Dict.fromList <| list decodeCode)
        (D.map Dict.fromList <| list decodeBlob)


encodeMaybe : Maybe a -> (a -> E.Encoder) -> E.Encoder
encodeMaybe maybe encoder =
    case maybe of
        Nothing ->
            E.unsignedInt8 0

        Just value ->
            E.sequence
                [ E.unsignedInt8 1
                , encoder value
                ]


decodeMaybe : D.Decoder a -> D.Decoder (Maybe a)
decodeMaybe decoder =
    D.andThen (decodeMaybeHelp decoder) D.unsignedInt8


decodeMaybeHelp : D.Decoder a -> Int -> D.Decoder (Maybe a)
decodeMaybeHelp decoder indicator =
    if indicator == 0 then
        D.succeed Nothing

    else
        D.map Just decoder


encodeMessage : Message -> E.Encoder
encodeMessage { to, from, time, subject, userInput, code, blobs } =
    E.sequence
        [ E.unsignedInt32 Bytes.LE to
        , E.unsignedInt32 Bytes.LE from
        , E.unsignedInt32 Bytes.LE time
        , encodeSizedString subject
        , encodeSizedString userInput
        , encodeList (Dict.toList code) encodeCode
        , encodeList (Dict.toList blobs) encodeBlob
        ]


encodeCode : ( String, String ) -> E.Encoder
encodeCode ( name, code ) =
    E.sequence
        [ encodeSizedString name
        , encodeSizedString code
        ]


encodeBlob : ( String, Bytes.Bytes ) -> E.Encoder
encodeBlob ( name, blob ) =
    E.sequence
        [ encodeSizedString name
        , encodeBytes blob
        ]


decodeMessage : D.Decoder Message
decodeMessage =
    map7 Message
        (D.unsignedInt32 Bytes.LE)
        (D.unsignedInt32 Bytes.LE)
        (D.unsignedInt32 Bytes.LE)
        sizedString
        sizedString
        (D.map Dict.fromList <| list decodeCode)
        (D.map Dict.fromList <| list decodeBlob)


decodeCode : D.Decoder ( String, String )
decodeCode =
    D.map2 (\a b -> ( a, b ))
        sizedString
        sizedString


decodeBlob : D.Decoder ( String, Bytes.Bytes )
decodeBlob =
    D.map2 (\a b -> ( a, b ))
        sizedString
        decodeBytes


map6 :
    (a -> b -> c -> d -> e -> f -> result)
    -> D.Decoder a
    -> D.Decoder b
    -> D.Decoder c
    -> D.Decoder d
    -> D.Decoder e
    -> D.Decoder f
    -> D.Decoder result
map6 func decoderA decoderB decoderC decoderD decoderE decoderF =
    D.map func decoderA
        |> D.andThen (dmap decoderB)
        |> D.andThen (dmap decoderC)
        |> D.andThen (dmap decoderD)
        |> D.andThen (dmap decoderE)
        |> D.andThen (dmap decoderF)


map7 :
    (a -> b -> c -> d -> e -> f -> g -> result)
    -> D.Decoder a
    -> D.Decoder b
    -> D.Decoder c
    -> D.Decoder d
    -> D.Decoder e
    -> D.Decoder f
    -> D.Decoder g
    -> D.Decoder result
map7 func decoderA decoderB decoderC decoderD decoderE decoderF decoderG =
    D.map func decoderA
        |> D.andThen (dmap decoderB)
        |> D.andThen (dmap decoderC)
        |> D.andThen (dmap decoderD)
        |> D.andThen (dmap decoderE)
        |> D.andThen (dmap decoderF)
        |> D.andThen (dmap decoderG)


dmap : D.Decoder a -> (a -> b) -> D.Decoder b
dmap a b =
    D.map b a


onlyIfAllJustHelp : Maybe a -> Maybe (List a) -> Maybe (List a)
onlyIfAllJustHelp maybe accum =
    case ( maybe, accum ) of
        ( Just b, Just bs ) ->
            Just <| b :: bs

        ( _, Nothing ) ->
            Nothing

        ( Nothing, _ ) ->
            Nothing


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


encodeBytes : Bytes.Bytes -> E.Encoder
encodeBytes bytes =
    E.sequence
        [ E.unsignedInt32 Bytes.LE <| Bytes.width bytes
        , E.bytes bytes
        ]


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


decodeInbox : List String -> Result String (List Message)
decodeInbox rawMessages =
    let
        asBytes =
            List.map (Base64.Decode.decode Base64.Decode.bytes) rawMessages
    in
    case combineResult asBytes of
        Err err ->
            Err <| "could not decode inbox base64: " ++ showB64Error err

        Ok bytesList ->
            decodeInboxHelp bytesList


decodeDrafts : List String -> Result String (List Draft)
decodeDrafts rawDrafts =
    let
        asBytes =
            List.map (Base64.Decode.decode Base64.Decode.bytes) rawDrafts
    in
    case combineResult asBytes of
        Err err ->
            Err <| "could not decode inbox base64: " ++ showB64Error err

        Ok bytesList ->
            decodeDraftsHelp bytesList


decodeDraftsHelp : List Bytes.Bytes -> Result String (List Draft)
decodeDraftsHelp rawDrafts =
    let
        maybes =
            List.map (D.decode decodeDraft) rawDrafts
    in
    case onlyIfAllJust maybes of
        Nothing ->
            Err "could not decode drafts bytes"

        Just drafts ->
            Ok drafts


decodeInboxHelp : List Bytes.Bytes -> Result String (List Message)
decodeInboxHelp rawMessages =
    let
        maybes =
            List.map (D.decode decodeMessage) rawMessages
    in
    case onlyIfAllJust maybes of
        Nothing ->
            Err "could not decode inbox bytes"

        Just messages ->
            Ok messages


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


decodeBytes : D.Decoder Bytes.Bytes
decodeBytes =
    D.unsignedInt32 Bytes.LE |> D.andThen D.bytes


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

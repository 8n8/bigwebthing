module Utils exposing
    ( Code
    , Document(..)
    , Draft
    , Message
    , MsgOut(..)
    , decodeDocument
    , decodeDrafts
    , decodeInbox
    , encodeDraft
    , encodeHumanMsg
    , encodeMessage
    , encodeSizedString
    , fontSize
    , hash
    , justs
    , sansSerif
    , showB64Error
    )

import Base64.Decode
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Dict
import Element
import Element.Font as Font
import SHA256


sansSerif : Element.Attribute msg
sansSerif =
    Font.family [ Font.typeface "EB Garamond" ]


fontSize : Element.Attribute msg
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
    , code : Code
    , blobs : Dict.Dict String Bytes.Bytes
    }


type alias Code =
    { main : String
    , modules : List String
    }


type alias Draft =
    { to : Maybe Int
    , time : Int
    , subject : String
    , userInput : String
    , code : Code
    , blobs : Dict.Dict String Bytes.Bytes
    }


encodeDraft : Draft -> E.Encoder
encodeDraft { to, time, subject, userInput, code, blobs } =
    E.sequence
        [ encodeMaybe to (E.unsignedInt32 Bytes.LE)
        , E.unsignedInt32 Bytes.LE time
        , encodeSizedString subject
        , encodeSizedString userInput
        , encodeCode code
        , encodeList (Dict.toList blobs) encodeBlob
        ]


decodeDraft : D.Decoder Draft
decodeDraft =
    map6 Draft
        (decodeMaybe <| D.unsignedInt32 Bytes.LE)
        (D.unsignedInt32 Bytes.LE)
        sizedString
        sizedString
        decodeCode
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
        , encodeCode code
        , encodeList (Dict.toList blobs) encodeBlob
        ]


encodeCode : Code -> E.Encoder
encodeCode { main, modules } =
    E.sequence
        [ encodeSizedString main
        , encodeList modules encodeSizedString
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
        decodeCode
        (D.map Dict.fromList <| list decodeBlob)


decodeCode : D.Decoder Code
decodeCode =
    D.map2 Code
        sizedString
        (list sizedString)


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
                [ E.unsignedInt8 0
                , E.unsignedInt32 Bytes.LE <| List.length docs
                ]
                    ++ List.map encodeDocument docs

        SmallString s ->
            E.sequence <|
                [ E.unsignedInt8 1
                , encodeSizedString s
                ]


encodeSizedString : String -> E.Encoder
encodeSizedString str =
    E.sequence
        [ E.unsignedInt32 Bytes.LE (E.getStringWidth str)
        , E.string str
        ]


encodeList : List a -> (a -> E.Encoder) -> E.Encoder
encodeList toEncode elementEncoder =
    E.sequence <|
        (E.unsignedInt32 Bytes.LE <| List.length toEncode)
            :: List.map elementEncoder toEncode


encodeBytes : Bytes.Bytes -> E.Encoder
encodeBytes bytes =
    E.sequence
        [ E.unsignedInt32 Bytes.LE <| Bytes.width bytes
        , E.bytes bytes
        ]


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


type Document
    = Ordering (List Document)
    | SmallString String


type MsgOut
    = MakeMyName
    | WhitelistSomeone Int
    | SendThis HumanMsg


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


sizedString : D.Decoder String
sizedString =
    D.unsignedInt32 Bytes.LE
        |> D.andThen D.string


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

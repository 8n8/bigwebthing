module MessageIdMap exposing
    ( MessageIdMap
    , MessageId(..)
    , fromList
    , toList
    , empty
    )


import Dict
import Bytes
import Time
import Base64.Encode as B64e
import Base64.Decode as B64d
import Bytes.Encode as Be


fromList : List (MessageId, a) -> MessageIdMap a
fromList list =
    let
        asString = List.map (\(k, v) -> (toString k, v)) list
    in
    MakeMap <| Dict.fromList asString


empty : MessageIdMap a
empty =
    MakeMap Dict.empty


toString : MessageId -> String
toString (MessageId raw) =
    B64e.encode <| B64e.bytes raw


type MessageIdMap a =
    MakeMap (Dict.Dict String a)


type MessageId =
    MessageId Bytes.Bytes


toList : MessageIdMap a -> Maybe (List (MessageId, a))
toList (MakeMap m) =
    allJust <| List.map decodeItem <| Dict.toList m



allJust : List (Maybe a) -> Maybe (List a)
allJust maybes =
    allJustHelp maybes []


allJustHelp : List (Maybe a) -> List a -> Maybe (List a)
allJustHelp maybes accum =
    case maybes of
        [] ->
            Just accum

        Just m :: aybes ->
            allJustHelp aybes (m :: accum)

        Nothing :: _ ->
            Nothing


decodeItem : (String, a) -> Maybe (MessageId, a)
decodeItem (raw, v) =
    case B64d.decode B64d.bytes raw of
        Err _ ->
            Nothing

        Ok bytes ->
            Just (MessageId bytes, v)

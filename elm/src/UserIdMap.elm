module UserIdMap exposing
    ( Fingerprint(..)
    , UserId(..)
    , UserIdMap
    , Username(..)
    , empty
    , fromList
    , get
    , toList
    )

import Base64.Decode as B64d
import Base64.Encode as B64e
import Bytes
import Dict


fromList : List UserId -> UserIdMap
fromList list =
    let
        asString =
            List.map
                (\(UserId u f) -> ( toString u, f ))
                list
    in
    MakeMap <| Dict.fromList asString


type UserIdMap
    = MakeMap (Dict.Dict String Fingerprint)


type UserId
    = UserId Username Fingerprint


type Username
    = Username Bytes.Bytes


type Fingerprint
    = Fingerprint Bytes.Bytes


empty : UserIdMap
empty =
    MakeMap Dict.empty


toString : Username -> String
toString (Username raw) =
    B64e.encode <| B64e.bytes raw


toList : UserIdMap -> Maybe (List ( Username, Fingerprint ))
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

        (Just m) :: aybes ->
            allJustHelp aybes (m :: accum)

        Nothing :: _ ->
            Nothing


decodeItem : ( String, Fingerprint ) -> Maybe ( Username, Fingerprint )
decodeItem ( raw, f ) =
    case B64d.decode B64d.bytes raw of
        Err _ ->
            Nothing

        Ok bytes ->
            Just ( Username bytes, f )


get : Username -> UserIdMap -> Maybe Fingerprint
get username (MakeMap map) =
    Dict.get (toString username) map

port module Main exposing (main)

import Base64
import Base64.Decode
import Base64.Encode
import Browser
import Bytes
import Bytes.Decode as D
import Bytes.Encode as E
import Dict
import Hex.Convert
import Http
import Json.Decode as Jd
import Json.Encode as Je
import List.Nonempty as N
import Testable exposing (..)


sendMsgs : List Je.Value -> Cmd Msg
sendMsgs msgs =
    Cmd.batch <| List.map sendMsg msgs


cacheHomeHelp : Home -> Cmd msg
cacheHomeHelp home =
    case Base64.fromBytes <| E.encode <| encodeHome home of
        Nothing ->
            Cmd.none

        Just base64str ->
            cacheHome base64str


port doProofOfWork : Je.Value -> Cmd msg


port doneProofOfWork : (String -> msg) -> Sub msg


port requestHome : () -> Cmd msg


port retrievedHome : (Je.Value -> msg) -> Sub msg


port requestHash : String -> Cmd msg


port retrievedHash : (String -> msg) -> Sub msg


port cacheHome : String -> Cmd msg


port cacheHash : String -> Cmd msg


port sendMsg : Je.Value -> Cmd msg


port makeIdToken : Je.Value -> Cmd msg


port newIdToken : (String -> msg) -> Sub msg


port gotSecretKeys : (Je.Value -> msg) -> Sub msg


port getSecretKeys : () -> Cmd msg


main : Platform.Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


doProofOfWorkHelp : PowInfo -> Cmd Msg
doProofOfWorkHelp powInfo =
    case Base64.fromBytes powInfo.unique of
        Nothing ->
            Cmd.none

        Just b64 ->
            doProofOfWork (jsonEncodePowInfo powInfo.difficulty b64)


makeIdTokenHelp : MakeIdTokenHelp -> Cmd Msg
makeIdTokenHelp info =
    makeIdToken <| encodeTokenInfo info


encodeTokenInfo : MakeIdTokenHelp -> Je.Value
encodeTokenInfo m =
    let
        e =
            Je.string << toB64
    in
    Je.object
        [ ( "publicsign", e <| m.keys.publicSign )
        , ( "secretsign", e <| m.keys.secretSign )
        , ( "route", e <| E.encode <| E.unsignedInt8 m.route )
        , ( "authcode", e m.authCode )
        , ( "message", e m.message )
        ]


{-| When a new contact is added, it goes like this:

1.  look up key, request auth code, and request proof of
    work information
2.  construct proof of work
3.  construct id token
4.  send whitelist request
5.  cache new contact

-}
updateAddNewContact : Model -> ( Model, Cmd Msg )
updateAddNewContact (Model model) =
    case ( model.addContactBox, model.home.myName ) of
        ( Just contact, Just myName ) ->
            case decodeInt myName of
                Nothing ->
                    ( Model { model | internalErr = Just "could not decode myName to an int" }, Cmd.none )

                Just myNameInt ->
                    if contact == myNameInt then
                        ( Model { model | youTriedToAddYourselfToContacts = True, addContactBox = Nothing }, Cmd.none )

                    else
                        updateAddNewContactHelp contact (Model model)

        ( Just contact, _ ) ->
            ( Model { model | addContactBox = Nothing, youTriedToAddYourselfToContacts = False }, requestKeyFromServer contact )

        _ ->
            ( Model model, Cmd.none )


updateAddNewContactHelp : Int -> Model -> ( Model, Cmd Msg )
updateAddNewContactHelp contact (Model model) =
    let
        oldPrep =
            model.prepareForWhitelist

        prep =
            { oldPrep | name = Just contact }
    in
    ( Model
        { model
            | newAuthCodeHole = Just authCodeHoleForWhitelist
            , newPowInfoHole = Just newPowInfoHoleForWhitelist
            , keyForNameHole = Just newKeyHoleForWhitelist
            , prepareForWhitelist = prep
        }
    , Cmd.batch
        [ requestKeyFromServer contact
        , getAuthCode
        , getPowInfo
        ]
    )


newPowInfoHoleForWhitelist : Result Http.Error PowInfo -> Model -> ( Model, Cmd Msg )
newPowInfoHoleForWhitelist httpResponse (Model model) =
    case httpResponse of
        Err err ->
            ( Model { model | addContactErr = Just err }, Cmd.none )

        Ok powInfo ->
            let
                prep =
                    model.prepareForWhitelist

                newPrep =
                    { prep | powInfo = Just powInfo }

                newModel =
                    Model { model | prepareForWhitelist = newPrep }
            in
            whitelistStage2Cmd newModel


newKeyHoleForWhitelist : Result Http.Error Bytes.Bytes -> Model -> ( Model, Cmd Msg )
newKeyHoleForWhitelist httpResponse (Model model) =
    case httpResponse of
        Err err ->
            ( Model { model | addContactErr = Just err }, Cmd.none )

        Ok key ->
            let
                prep =
                    model.prepareForWhitelist

                newPrep =
                    { prep | key = Just key }
            in
            whitelistStage2Cmd (Model { model | prepareForWhitelist = newPrep })


whitelistStage2Cmd : Model -> ( Model, Cmd Msg )
whitelistStage2Cmd (Model model) =
    let
        p =
            model.prepareForWhitelist

        newModel =
            { model
                | newIdTokenHole = Just idTokenHoleForWhitelist
                , newProofOfWorkHole = Just powHoleForWhitelist
            }
    in
    case ( ( p.powInfo, p.name, p.authCode ), ( model.home.myKeys, p.pow, p.idToken ) ) of
        ( _, ( Nothing, _, _ ) ) ->
            ( Model { model | internalErr = Just "can't continue with whitelisting without crypto keys" }, Cmd.none )

        ( ( Just powInfo, _, _ ), ( _, Nothing, _ ) ) ->
            ( Model newModel, doProofOfWorkHelp powInfo )

        ( ( Just powInfo, Just name, Just authCode ), ( Just myKeys, Just pow, idToken ) ) ->
            let
                makeIdTokenCmd =
                    case idToken of
                        Just _ ->
                            Cmd.none

                        Nothing ->
                            makeIdTokenHelp
                                { keys = myKeys
                                , route = 10
                                , authCode = authCode
                                , message =
                                    E.encode <|
                                        E.sequence
                                            [ E.bytes pow
                                            , encodeInt name
                                            ]
                                }
            in
            ( Model newModel, makeIdTokenCmd )

        _ ->
            ( Model newModel, Cmd.none )


idTokenHoleForWhitelist : String -> Model -> ( Model, Cmd Msg )
idTokenHoleForWhitelist b64 (Model model) =
    case Base64.Decode.decode Base64.Decode.bytes b64 of
        Err err ->
            ( Model { model | internalErr = Just <| "could not decode ID token from port: " ++ showB64Err err }, Cmd.none )

        Ok idToken ->
            let
                prep =
                    model.prepareForWhitelist

                newPrep =
                    { prep | idToken = Just idToken }

                newModel =
                    Model { model | prepareForWhitelist = newPrep }
            in
            sendWhitelistRequest newModel


showB64Err : Base64.Decode.Error -> String
showB64Err err =
    case err of
        Base64.Decode.ValidationError ->
            "validation error"

        Base64.Decode.InvalidByteSequence ->
            "invalid byte sequence"


sendWhitelistRequest : Model -> ( Model, Cmd Msg )
sendWhitelistRequest (Model model) =
    let
        p =
            model.prepareForWhitelist
    in
    case ( p.idToken, p.pow, p.name ) of
        ( Just idToken, Just pow, Just name ) ->
            let
                httpBody =
                    E.encode <|
                        E.sequence
                            [ E.unsignedInt8 10
                            , E.bytes idToken
                            , E.bytes pow
                            , encodeInt name
                            ]

                request =
                    Http.post
                        { url = urlRoot ++ "/api"
                        , body = Http.bytesBody "" httpBody
                        , expect = Http.expectWhatever Whitelisted
                        }
            in
            ( Model model, request )

        _ ->
            ( Model model, Cmd.none )


powHoleForWhitelist : String -> Model -> ( Model, Cmd Msg )
powHoleForWhitelist b64 (Model model) =
    case ( Base64.Decode.decode Base64.Decode.bytes b64, model.home.myKeys ) of
        ( Err err, _ ) ->
            ( Model { model | internalErr = Just <| "could not decode proof of work from port: " ++ showB64Err err }, Cmd.none )

        ( _, Nothing ) ->
            ( Model { model | internalErr = Just "no crypto keys" }, Cmd.none )

        ( Ok pow, Just myKeys ) ->
            let
                prep =
                    model.prepareForWhitelist

                newPrep =
                    { prep | pow = Just pow }
            in
            whitelistStage2Cmd (Model { model | prepareForWhitelist = newPrep })


authCodeHoleForWhitelist : Result Http.Error Bytes.Bytes -> Model -> ( Model, Cmd Msg )
authCodeHoleForWhitelist httpResponse (Model model) =
    case ( httpResponse, model.home.myKeys ) of
        ( Err err, _ ) ->
            ( whitelistClearHoles <|
                Model { model | addContactErr = Just err }
            , Cmd.none
            )

        ( _, Nothing ) ->
            ( whitelistClearHoles <|
                Model { model | internalErr = Just "no crypto keys" }
            , Cmd.none
            )

        ( Ok authCode, Just myKeys ) ->
            let
                prep =
                    model.prepareForWhitelist

                newPrep =
                    { prep | authCode = Just authCode }
            in
            whitelistStage2Cmd (Model { model | prepareForWhitelist = newPrep })


whitelistClearHoles : Model -> Model
whitelistClearHoles (Model model) =
    let
        emptyPrep =
            { authCode = Nothing
            , name = Nothing
            , powInfo = Nothing
            , pow = Nothing
            , idToken = Nothing
            , key = Nothing
            }

        newModel =
            { model
                | prepareForWhitelist = emptyPrep
                , newPowInfoHole = Nothing
                , keyForNameHole = Nothing
                , newAuthCodeHole = Nothing
                , newProofOfWorkHole = Nothing
                , newIdTokenHole = Nothing
            }
    in
    Model newModel


powInfoHoleForMakeName : Result Http.Error PowInfo -> Model -> ( Model, Cmd Msg )
powInfoHoleForMakeName httpResponse (Model model) =
    case httpResponse of
        Err err ->
            ( Model { model | addContactErr = Just err }, Cmd.none )

        Ok powInfo ->
            let
                newModel =
                    { model
                        | newProofOfWorkHole = Just powHoleForMakeName
                        , newPowInfoHole = Nothing
                    }
            in
            ( Model newModel
            , doProofOfWorkHelp powInfo
            )


powHoleForMakeName : String -> Model -> ( Model, Cmd Msg )
powHoleForMakeName b64 (Model model) =
    case ( Base64.Decode.decode Base64.Decode.bytes b64, model.home.myKeys ) of
        ( Err err, _ ) ->
            ( Model { model | internalErr = Just <| "could not decode proof of work from port: " ++ showB64Err err }, Cmd.none )

        ( _, Nothing ) ->
            ( Model { model | internalErr = Just "no crypt keys" }, Cmd.none )

        ( Ok pow, Just myKeys ) ->
            let
                newModel =
                    { model | newProofOfWorkHole = Nothing }
            in
            ( Model newModel, requestName myKeys.publicSign pow )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        Whitelisted (Err err) ->
            ( whitelistClearHoles <|
                Model { model | addContactErr = Just err }
            , Cmd.none
            )

        Whitelisted (Ok ()) ->
            case
                ( model.prepareForWhitelist.name
                , model.prepareForWhitelist.key
                )
            of
                ( Just name, Just key ) ->
                    let
                        contacts =
                            model.home.contacts

                        newContacts =
                            Dict.insert name key contacts

                        home =
                            model.home

                        newHome =
                            { home | contacts = newContacts }

                        newModel =
                            Model { model | home = newHome }
                    in
                    ( whitelistClearHoles newModel
                    , cacheHomeHelp newHome
                    )

                ( Nothing, _ ) ->
                    ( whitelistClearHoles <|
                        Model
                            { model
                                | internalErr = Just "Successful whitelist response, but no name locally to add to contacts."
                            }
                    , Cmd.none
                    )

                ( _, Nothing ) ->
                    ( whitelistClearHoles <|
                        Model
                            { model
                                | internalErr = Just "Successful whitelist response, but no key locally to add to contacts."
                            }
                    , Cmd.none
                    )

        NewIdToken rawToken ->
            case model.newIdTokenHole of
                Nothing ->
                    ( Model { model | internalErr = Just "unexpected id token" }, Cmd.none )

                Just updateOnNewToken ->
                    updateOnNewToken rawToken (Model model)

        KeyForName key ->
            case model.keyForNameHole of
                Nothing ->
                    ( Model { model | internalErr = Just "unexpected key for name" }, Cmd.none )

                Just updateOnNewKey ->
                    updateOnNewKey key (Model model)

        NewAuthCode authCode ->
            case model.newAuthCodeHole of
                Nothing ->
                    ( Model
                        { model
                            | internalErr = Just "unexpected authentication code"
                        }
                    , Cmd.none
                    )

                Just updateOnAuthCode ->
                    updateOnAuthCode authCode (Model model)

        AddNewContact ->
            updateAddNewContact (Model model)

        UpdateContactBox newInfo ->
            case String.toInt newInfo of
                Nothing ->
                    if newInfo == "" then
                        ( Model { model | addContactBox = Nothing }
                        , Cmd.none
                        )

                    else
                        ( Model model, Cmd.none )

                Just num ->
                    if num < 0 then
                        ( Model model, Cmd.none )

                    else
                        ( Model { model | addContactBox = Just num }
                        , Cmd.none
                        )

        NewName (Ok newName) ->
            let
                oldHome =
                    model.home

                newHome =
                    { oldHome | myName = Just newName }
            in
            ( Model { model | home = newHome }
            , cacheHomeHelp newHome
            )

        NewName (Err err) ->
            ( Model { model | getNameError = Just err }, Cmd.none )

        DoneProofOfWork b64 ->
            case model.newProofOfWorkHole of
                Nothing ->
                    ( Model
                        { model
                            | internalErr = Just "unexpected proof of work"
                        }
                    , Cmd.none
                    )

                Just processProofOfWork ->
                    processProofOfWork b64 (Model model)

        PowInfoResponse powInfo ->
            case model.newPowInfoHole of
                Nothing ->
                    ( Model { model | internalErr = Just "unexpected proof of work information" }, Cmd.none )

                Just powInfoProcessor ->
                    powInfoProcessor powInfo (Model model)

        LaunchProgram programName ->
            case Dict.get programName model.home.programs of
                Nothing ->
                    ( Model { model | openProgram = Nothing }
                    , Cmd.none
                    )

                Just program ->
                    reRunProgram (Model model) program

        LookupRaw hashes ->
            ( Model { model | toLookUp = N.toList hashes }
            , Cmd.batch <| List.map requestHash <| N.toList hashes
            )

        NewRawKeys rawKeys ->
            case Jd.decodeValue decodeKeys rawKeys of
                Err err ->
                    ( Model
                        { model
                            | internalErr = Just <| Jd.errorToString err
                        }
                    , Cmd.none
                    )

                Ok keys ->
                    let
                        oldHome =
                            model.home

                        newHome =
                            { oldHome | myKeys = Just keys }

                        newModel =
                            { model | home = newHome }
                    in
                    case model.openProgram of
                        Nothing ->
                            ( Model newModel, cacheHomeHelp newHome )

                        Just ( program, _ ) ->
                            reRunProgram (Model newModel) program

        RetrievedHome rawHome ->
            case Jd.decodeValue decodeHomeTop rawHome of
                Err err ->
                    ( Model
                        { model
                            | internalErr = Just <| Jd.errorToString err
                        }
                    , Cmd.none
                    )

                Ok { rawB64Home, exists } ->
                    if exists then
                        case Base64.toBytes rawB64Home of
                            Just bytes ->
                                case D.decode decodeHome bytes of
                                    Nothing ->
                                        ( Model
                                            { model
                                                | internalErr = Just "Could not decode bytes for Home"
                                            }
                                        , Cmd.none
                                        )

                                    Just home ->
                                        ( Model
                                            { model
                                                | home = home
                                                , newPowInfoHole = Just powInfoHoleForMakeName
                                            }
                                        , case home.myName of
                                            Nothing ->
                                                getPowInfo

                                            Just _ ->
                                                Cmd.none
                                        )

                            Nothing ->
                                ( Model { model | internalErr = Just "Could not convert base64 to bytes for Home" }, Cmd.none )

                    else
                        ( Model { model | newPowInfoHole = Just powInfoHoleForMakeName }, Cmd.batch [ getPowInfo, getSecretKeys () ] )

        RetrievedHash raw ->
            ( Model model, Cmd.none )

        UpdatedLeft newLeftText ->
            case model.openProgram of
                Nothing ->
                    ( Model model, Cmd.none )

                Just ( program, _ ) ->
                    let
                        newProg =
                            { program | typedIn = newLeftText }
                    in
                    reRunProgram (Model model) newProg

        UpdatedEditor newCode ->
            case model.openProgram of
                Nothing ->
                    ( Model model, Cmd.none )

                Just ( program, _ ) ->
                    let
                        newProg =
                            { program | code = newCode }

                        newPrograms =
                            updatePrograms model.home.programs model.openProgram newProg

                        oldHome =
                            model.home

                        newHome =
                            { oldHome | programs = newPrograms }
                    in
                    reRunProgram (Model { model | home = newHome }) newProg

        UpdatedDescription newDescription ->
            case model.openProgram of
                Nothing ->
                    ( Model model, Cmd.none )

                Just ( program, doc ) ->
                    let
                        newProg =
                            { program | description = newDescription }

                        newPrograms =
                            Dict.insert (hash program.code) newProg model.home.programs

                        oldHome =
                            model.home

                        newHome =
                            { oldHome | programs = newPrograms }
                    in
                    ( Model { model | home = newHome, openProgram = Just ( newProg, doc ) }, Cmd.none )

        MakeNewProgram ->
            let
                newProgram =
                    { code = "", description = "New program", inbox = [], blobs = [], typedIn = "" }

                newPrograms =
                    Dict.insert (hash newProgram.code) newProgram model.home.programs

                oldHome =
                    model.home

                newHome =
                    { oldHome | programs = newPrograms }
            in
            ( Model { model | home = newHome, openProgram = Just ( newProgram, Nothing ) }, Cmd.none )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        { home = initHome
        , addContactBox = Nothing
        , openProgram = Nothing
        , lookedUpBlob = Nothing
        , toLookUp = []
        , internalErr = Nothing
        , editProgram = False
        , getNameError = Nothing
        , addContactErr = Nothing
        , youTriedToAddYourselfToContacts = False
        , newIdTokenHole = Nothing
        , newProofOfWorkHole = Nothing
        , newAuthCodeHole = Nothing
        , keyForNameHole = Nothing
        , newPowInfoHole = Nothing
        , prepareForWhitelist = { authCode = Nothing, name = Nothing, powInfo = Nothing, pow = Nothing, idToken = Nothing, key = Nothing }
        }
    , Cmd.batch [ requestHome () ]
    )


reRunProgram : Model -> Testable.Program -> ( Model, Cmd Msg )
reRunProgram (Model model) program =
    let
        ( p, doc, msgs ) =
            runProgram program model.home.programs

        oldHome =
            model.home

        newOutbox =
            model.home.outbox ++ msgs
    in
    case model.home.myKeys of
        Nothing ->
            ( Model { model | home = { oldHome | outbox = newOutbox }, openProgram = Just ( p, doc ) }
            , getSecretKeys ()
            )

        Just myKeys ->
            case
                encodeMsgs
                    { msgs = newOutbox
                    , pubKeys = model.home.pubKeys
                    , myKeys = myKeys
                    , nonceBase = model.home.biggestNonceBase
                    }
            of
                Err err ->
                    ( Model
                        { model
                            | internalErr = Just <| "error encoding messages: " ++ err
                            , home = { oldHome | outbox = newOutbox }
                        }
                    , Cmd.none
                    )

                Ok encodedMsgs ->
                    let
                        newHome =
                            { oldHome
                                | outbox = newOutbox
                                , biggestNonceBase = oldHome.biggestNonceBase + List.length msgs
                            }
                    in
                    ( Model
                        { model
                            | home = newHome
                            , openProgram = Just ( p, doc )
                        }
                    , Cmd.batch
                        [ sendMsgs encodedMsgs
                        , cacheHomeHelp newHome
                        ]
                    )


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ retrievedHome RetrievedHome
        , retrievedHash RetrievedHash
        , gotSecretKeys NewRawKeys
        , newIdToken NewIdToken
        , doneProofOfWork DoneProofOfWork
        ]

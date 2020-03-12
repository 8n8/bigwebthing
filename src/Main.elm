port module Main exposing (main)

import Base64
import Browser
import Bytes.Decode as D
import Bytes.Encode as E
import Dict
import Hex.Convert
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


port newIdToken : (Je.Value -> msg) -> Sub msg


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg (Model model) =
    case msg of
        NewIdToken rawToken ->
            case model.newIdTokenHole of
                Nothing ->
                    ( Model { model | internalErr = Just "unexpected id token" }, Cmd.none )

                Just updateOnNewToken ->
                    updateOnNewToken rawToken (Model model)

        KeyForName name (Ok key) ->
            ( Model model, getAuthCode )

        NewAuthCode (Ok newAuthCode) ->
            case model.newAuthCodeHole of
                Nothing ->
                    ( Model { model | internalErr = Just "unexpected authentication code" }, Cmd.none )

                Just updateOnAuthCode ->
                    updateOnAuthCode newAuthCode (Model model)

        NewAuthCode (Err err) ->
            ( Model model, Cmd.none )

        KeyForName name (Err err) ->
            ( Model { model | addContactErr = Just err }, Cmd.none )

        AddNewContact ->
            case ( model.addContactBox, model.home.myName ) of
                ( Just contact, Just myName ) ->
                    case decodeInt myName of
                        Nothing ->
                            ( Model { model | internalErr = Just "could not decode myName to an int" }, Cmd.none )

                        Just myNameInt ->
                            if contact == myNameInt then
                                ( Model { model | youTriedToAddYourselfToContacts = True, addContactBox = Nothing }, Cmd.none )

                            else
                                ( Model { model | addContactBox = Nothing, youTriedToAddYourselfToContacts = False }, requestKeyFromServer contact )

                ( Just contact, _ ) ->
                    ( Model { model | addContactBox = Nothing, youTriedToAddYourselfToContacts = False }, requestKeyFromServer contact )

                _ ->
                    ( Model model, Cmd.none )

        UpdateContactBox newInfo ->
            case String.toInt newInfo of
                Nothing ->
                    if newInfo == "" then
                        ( Model { model | addContactBox = Nothing }, Cmd.none )

                    else
                        ( Model model, Cmd.none )

                Just num ->
                    if num < 0 then
                        ( Model model, Cmd.none )

                    else
                        ( Model { model | addContactBox = Just num }, Cmd.none )

        NewName (Ok newName) ->
            let
                oldHome =
                    model.home

                newHome =
                    { oldHome | myName = Just newName }
            in
            ( Model { model | home = newHome }, cacheHomeHelp newHome )

        NewName (Err err) ->
            ( Model { model | getNameError = Just err }, Cmd.none )

        DoneProofOfWork b64 ->
            case model.newProofOfWorkHole of
                Nothing ->
                    ( Model { model | internalErr = Just "unexpected proof of work" }, Cmd.none )

                Just processProofOfWork ->
                    processProofOfWork b64 (Model model)

        -- case Base64.toBytes b64 of
        --     Nothing ->
        --         ( { model | internalErr = Just "Could not convert proof of work from Base64 to bytes." }, Cmd.none )
        --     Just proofOfWork ->
        --         case ( model.home.myName, model.home.myKeys ) of
        --             ( Nothing, Just keys ) ->
        --                 ( model, requestName keys.publicSign proofOfWork )
        --             _ ->
        --                 ( model, Cmd.none )
        PowInfoResponse (Ok powInfo) ->
            ( Model model, doProofOfWorkHelp powInfo )

        PowInfoResponse (Err err) ->
            ( Model { model | getNameError = Just err }, Cmd.none )

        LaunchProgram programName ->
            case Dict.get programName model.home.programs of
                Nothing ->
                    ( Model { model | openProgram = Nothing }, Cmd.none )

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
                    ( Model { model | internalErr = Just <| Jd.errorToString err }, Cmd.none )

                Ok { rawB64Home, exists } ->
                    if exists then
                        case Base64.toBytes rawB64Home of
                            Just bytes ->
                                case D.decode decodeHome bytes of
                                    Nothing ->
                                        ( Model { model | internalErr = Just "Could not decode bytes for Home" }, Cmd.none )

                                    Just home ->
                                        ( Model { model | home = home }
                                        , case home.myName of
                                            Nothing ->
                                                getPowInfo

                                            Just _ ->
                                                Cmd.none
                                        )

                            Nothing ->
                                ( Model { model | internalErr = Just "Could not convert base64 to bytes for Home" }, Cmd.none )

                    else
                        ( Model model, Cmd.batch [ getPowInfo, getSecretKeys () ] )

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

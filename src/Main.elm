port module Main exposing (main)

import Base64
import Browser
import Bytes.Decode as D
import Bytes.Encode as E
import Dict
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


port requestHome : () -> Cmd msg


port retrievedHome : (String -> msg) -> Sub msg


port requestHash : String -> Cmd msg


port retrievedHash : (String -> msg) -> Sub msg


port cacheHome : String -> Cmd msg


port cacheHash : String -> Cmd msg


port sendMsg : Je.Value -> Cmd msg


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowProgramCheckBox check ->
            ( { model | editProgram = check }, Cmd.none )

        LaunchProgram programName ->
            case Dict.get programName model.home.programs of
                Nothing ->
                    ( { model | openProgram = Nothing }, Cmd.none )

                Just program ->
                    reRunProgram model program

        LookupRaw hashes ->
            ( { model | toLookUp = N.toList hashes }
            , Cmd.batch <| List.map requestHash <| N.toList hashes
            )

        NewRawKeys rawKeys ->
            case Jd.decodeValue decodeSecretKeys rawKeys of
                Err err ->
                    ( { model
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
                            ( model, Cmd.none )

                        Just ( program, _ ) ->
                            reRunProgram newModel program

        RetrievedHome rawHome ->
            case Base64.toBytes rawHome of
                Just bytes ->
                    case D.decode decodeHome bytes of
                        Nothing ->
                            ( model, Cmd.none )

                        Just home ->
                            ( { model | home = home }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        RetrievedHash raw ->
            ( model, Cmd.none )

        UpdatedLeft newLeftText ->
            case model.openProgram of
                Nothing ->
                    ( model, Cmd.none )

                Just ( program, _ ) ->
                    let
                        newProg =
                            { program | typedIn = newLeftText }
                    in
                    reRunProgram model newProg

        UpdatedEditor newCode ->
            case model.openProgram of
                Nothing ->
                    ( model, Cmd.none )

                Just ( program, _ ) ->
                    let
                        newProg =
                            { program | code = newCode }
                        newPrograms = updatePrograms model.home.programs model.openProgram newProg
                        oldHome = model.home
                        newHome = { oldHome | programs = newPrograms }
                    in
                    reRunProgram { model | home = newHome} newProg


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( newProgram, rightDoc, outMsgs ) =
            runProgram defaultHome Dict.empty
    in
    ( { home = initHome
      , openProgram = Just ( defaultHome, rightDoc )
      , lookedUpBlob = Nothing
      , toLookUp = []
      , internalErr = Nothing
      , editProgram = False
      }
    , requestHome ()
    )


reRunProgram : Model -> Testable.Program -> ( Model, Cmd Msg )
reRunProgram model program =
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
            ( { model | home = { oldHome | outbox = newOutbox }, openProgram = Just ( p, doc ) }
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
                    ( { model
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
                    ( { model
                        | home = newHome
                        , openProgram = Just ( p, doc )
                      }
                    , Cmd.batch
                        [ sendMsgs encodedMsgs
                        , cacheHomeHelp newHome
                        ]
                    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ retrievedHome RetrievedHome
        , retrievedHash RetrievedHash
        , gotSecretKeys NewRawKeys
        ]

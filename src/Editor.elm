port module Editor exposing
    ( Model
    , Msg
    , initCmd
    , initModel
    , subscriptions
    , update
    , view
    )

import Base64.Decode
import Base64.Encode
import Bytes
import Bytes.Decode as D
import Http
import Bytes.Encode as E
import Dict
import Element
import Element.Font as Font
import Element.Input
import File
import File.Download as Download
import File.Select as Select
import Hex.Convert
import Json.Decode as Jd
import Json.Encode as Je
import Set
import Task
import Time
import Element.Font as Font


type alias Message =
    { from : Int
    , to : Int
    , time : Int
    , subject : String
    , userInput : String
    , code : ( String, BlobId )
    , blobs : Dict.Dict Int String
    }


type BlobId = BlobId Int


type alias Draft =
    { to : Maybe Int
    , time : Int
    , subject : String
    , userInput : String
    , code : Maybe { fileName : String, blobId : Int }
    , blobs : Dict.Dict Int String
    }


type alias Model =
    { myName : Maybe Int
    , myContacts : Set.Set Int
    , sendToBox : Maybe Int
    , addContactBox : Maybe Int
    , addContactError : Maybe String
    , messages : Dict.Dict Int Message
    , opened : Opened
    , drafts : Dict.Dict Int Draft
    , internalError : Maybe String
    , compilerError : Maybe String
    , outbox : List Message
    , sendMessageError : Maybe String
    , currentSendingId : Maybe Int
    , sending : Dict.Dict Int Sending
    , signingKeys : Maybe SigningKeys
    , iota : Int
    , processes : Dict.Dict Int ProcessStatus
    }


type ProcessStatus
    = PwhiteList WhiteListProgress
    | PDownload DownloadProgress
    | Pfailed String


type DownloadProgress
    = RequestingBlob String


type WhiteListProgress
    = MakingIdToken MakeIdTokenProgress
    | MakingProofOfWork IdToken
    | SendingRequest IdToken ProofOfWork


type ProofOfWork = ProofOfWork Bytes.Bytes


type IdToken = IdToken Bytes.Bytes


type GetMyNameProgress
    = GmakingProofOfWork ProofOfWorkProgress
    | GgettingSigningKeys ProofOfWork
    | GsendingRequest SigningKeys ProofOfWork


type ProofOfWorkProgress
    = PgettingInfo
    | PaskJsForPow


type MakeIdTokenProgress
    = GettingMyName GetMyNameProgress
    | GettingAuthCode MyName
    | GettingSignature MyName AuthCode

type AuthCode = AuthCode Bytes.Bytes

type MyName = MyName Int


type alias SigningKeys =
    { public : Bytes.Bytes
    , secret : Bytes.Bytes
    }


decodeSigningKeys : Bytes.Bytes -> Maybe SigningKeys
decodeSigningKeys raw =
    D.decode
    (D.map2 SigningKeys
        (D.bytes 32)
        (D.bytes 64)) raw


type alias Sending =
    { message : Message
    , progress : SendingProgress
    }


{-| The steps to send a message are:

  - request signing and crypto keys from the DB if necessary
  - generate new signing and crypto keys
  - request my user ID name from the DB if necessary
  - request my user ID from the server if necessary
  - send all the blobs
  - send the message in 15KB chunks

-}
type SendingProgress
    = RequestingSigningKeysFromDb
    | RequestingCryptoKeysFromDb
    | GeneratingSigningKeys
    | GeneratingCryptoKeys
    | RequestingMyIdFromDb
    | RequestingMyIdFromServer
    | SendingBlobs { sent : Set.Set String, notSent : Set.Set String }
    | SendingChunks (List Bytes.Bytes)
    | Failed String


type Opened
    = OMessage OpenedMessage
    | ODraft OpenedDraft
    | Neither


type alias OpenedMessage =
    { id : Int
    , document : Maybe Document
    }


type Document
    = Ordering (List Document)
    | SmallString String


type alias OpenedDraft =
    { id : Int
    , document : Maybe Document
    }


port elmToJs : Je.Value -> Cmd msg


port jsToElm : (Je.Value -> msg) -> Sub msg


type FromJs
    = UpdatedDrafts (Dict.Dict Int Draft)


type ToJs
    = TupdatedDraftUserInput {pid : Pid, userInput : String}
    | TupdatedRecipient { id : Int, recipient : Int }
    | TnewCode { code : Bytes.Bytes, name : FileName }
    | Twhitelist Int
    | TrequestBlob Int
    | TmakeNewDraft Int
    | TdeleteBlob {blobId : Int, draftId : Int}
    | TupdatedUserInput Int String
    | TupdatedSubject Int String


type Pid = Pid Int


sendToJs : ToJs -> Cmd msg
sendToJs toJs =
    elmToJs <|
    case toJs of
        TupdatedDraftUserInput {pid, userInput} ->
            Je.object
                [ ("key", Je.string "updatedDraftUserInput")
                , ("pid", Je.int <| (\(Pid p) -> p) pid)
                , ("userInput", Je.string userInput)
                ]
        TupdatedRecipient {id, recipient} ->
            Je.object
                [ ("key", Je.string "updatedRecipient")
                , ("pid", Je.int id)
                , ("recipient", Je.int recipient)
                ]


subscriptions : Sub Msg
subscriptions =
    jsToElm RawFromJs


initCmd : Cmd Msg
initCmd =
    Cmd.none


initModel : Model
initModel =
    { myName = Nothing
    , myContacts = Set.empty
    , addContactBox = Nothing
    , addContactError = Nothing
    , internalError = Nothing
    , sendToBox = Nothing
    , sendMessageError = Nothing
    , drafts = Dict.empty
    , outbox = []
    , compilerError = Nothing
    , messages = Dict.empty
    , opened = Neither
    , currentSendingId = Nothing
    , sending = Dict.empty
    , iota = 0
    , processes = Dict.empty
    , signingKeys = Nothing
    }


type Msg
    = AddNewContact
    | DeleteBlob Int
    | UpdatedUserInput Int String
    | UpdatedSubject Int String
    | NewPowInfo Int (Result Http.Error PowInfo)
    | RawFromJs Je.Value
    | ClickAddProgramButton Int
    | RawWasmDocument String
    | UpdateContactBox String
    | RetrievedEditorInfo Je.Value
    | GoodWhitelist Int
    | BadWhitelist String
    | CloseMessage
    | CloseDraft
    | UpdatedRecipient Int String
    | UpdatedDraft OpenedDraft
    | OpenDraft Int
    | OpenMessage Int
    | MakeNewDraft
    | TimeForNewDraft Time.Posix
    | SendDraft Int
    | SendMessageError String
    | ClickAddBlobButton Int
    | BlobSelected Int File.File
    | BlobLoaded String Int Bytes.Bytes
    | DownloadFile {fileName : String, blobId : Int}
    | ProgramSelected File.File
    | ProgramLoaded FileName Bytes.Bytes

type FileName = FileName String


editorInfoDecoder : Jd.Decoder RawEditorInfo
editorInfoDecoder =
    Jd.map5 RawEditorInfo
        (Jd.field "myName" Jd.int)
        (Jd.field "myContacts" (Jd.list Jd.int))
        (Jd.field "inbox" (Jd.list Jd.string))
        (Jd.field "drafts" (Jd.list Jd.string))
        (Jd.field "outbox" (Jd.list Jd.string))


type alias RawEditorInfo =
    { myName : Int
    , myContacts : List Int
    , inbox : List String
    , drafts : List String
    , outbox : List String
    }


decodeDocument : D.Decoder Document
decodeDocument =
    D.andThen decodeDocumentHelp D.unsignedInt8


decodeOrdering : D.Decoder Document
decodeOrdering =
    D.map Ordering (list decodeDocument)


showB64Error : Base64.Decode.Error -> String
showB64Error error =
    case error of
        Base64.Decode.ValidationError ->
            "validation error"

        Base64.Decode.InvalidByteSequence ->
            "invalid byte sequence"


sizedString : D.Decoder String
sizedString =
    D.unsignedInt32 Bytes.LE
        |> D.andThen D.string


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


{-| Pinched from the Bytes documentation.
-}
list : D.Decoder a -> D.Decoder (List a)
list decoder =
    D.unsignedInt32 Bytes.LE
        |> D.andThen
            (\len -> D.loop ( len, [] ) (listStep decoder))


decodeDocumentHelp : Int -> D.Decoder Document
decodeDocumentHelp typeNum =
    case typeNum of
        0 ->
            decodeOrdering

        1 ->
            D.map SmallString sizedString

        _ ->
            D.fail


rawWasmDocumentHelp : String -> Model -> ( Model, Cmd Msg )
rawWasmDocumentHelp rawDocB64 model =
    case Base64.Decode.decode Base64.Decode.bytes rawDocB64 of
        Err err ->
            ( { model
                | internalError =
                    Just <|
                        showB64Error err
              }
            , Cmd.none
            )

        Ok rawBytes ->
            case
                ( model.opened
                , D.decode decodeDocument rawBytes
                )
            of
                ( Neither, _ ) ->
                    ( { model
                        | internalError = Just "received WASM document but no open message"
                      }
                    , Cmd.none
                    )

                ( ODraft draft, Just document ) ->
                    ( { model
                        | opened =
                            ODraft
                                { draft
                                    | document = Just document
                                }
                      }
                    , Cmd.none
                    )

                ( ODraft draft, Nothing ) ->
                    ( { model
                        | opened =
                            ODraft
                                { draft
                                    | document =
                                        Just <|
                                            SmallString "could not decode document generated by WASM"
                                }
                      }
                    , Cmd.none
                    )

                ( OMessage message, Just document ) ->
                    ( { model
                        | opened =
                            OMessage
                                { message
                                    | document = Just document
                                }
                      }
                    , Cmd.none
                    )

                ( OMessage message, Nothing ) ->
                    ( { model
                        | opened =
                            OMessage
                                { message
                                    | document =
                                        Just <|
                                            SmallString "could not decode document generated by WASM"
                                }
                      }
                    , Cmd.none
                    )


type alias WasmResult =
    { err : String
    , output : String
    }


wasmOutputDecoder : Jd.Decoder WasmResult
wasmOutputDecoder =
    Jd.map2 WasmResult
        (Jd.field "err" Jd.string)
        (Jd.field "output" Jd.string)


localGetDecoder : Jd.Decoder LocalGetResult
localGetDecoder =
    Jd.map2 LocalGetResult
        (Jd.field "err" Jd.string)
        (Jd.field "output" Jd.string)


type alias LocalGetResult =
    { err : String
    , output : String
    }


decodeWasmOutput : Je.Value -> Result String Document
decodeWasmOutput json =
    case Jd.decodeValue wasmOutputDecoder json of
        Err err ->
            Err <|
                "could not decode wasm output: "
                    ++ Jd.errorToString err

        Ok wasmResult ->
            if wasmResult.err == "" then
                case
                    Base64.Decode.decode
                        Base64.Decode.bytes
                        wasmResult.output
                of
                    Err err ->
                        Err <| showB64Error err

                    Ok rawBytes ->
                        case D.decode decodeDocument rawBytes of
                            Nothing ->
                                Err "could not decode document generated by WASM"

                            Just document ->
                                Ok document

            else
                Err wasmResult.err


decodeLocalGetResult : Je.Value -> Result String Bytes.Bytes
decodeLocalGetResult json =
    case Jd.decodeValue localGetDecoder json of
        Err err ->
            Err <|
                 "could not decode localget output: " ++ Jd.errorToString err

        Ok {output, err} ->
            if err == "" then
                case Base64.Decode.decode Base64.Decode.bytes output of
                    Err errb64 ->
                        Err <| showB64Error errb64
                    Ok rawBytes ->
                        Ok rawBytes

            else
                Err err
                                

processWasmOutput : Je.Value -> Model -> ( Model, Cmd Msg )
processWasmOutput json model =
    case decodeWasmOutput json of
        Err err ->
            ( { model
                | internalError = Just <| "could not decode wasm output: " ++ err
              }
            , Cmd.none
            )

        Ok document ->
            case model.opened of
                OMessage omsg ->
                    ( { model
                        | opened =
                            OMessage
                                { omsg | document = Just document }
                      }
                    , Cmd.none
                    )

                ODraft odraft ->
                    ( { model
                        | opened =
                            ODraft
                                { odraft | document = Just document }
                      }
                    , Cmd.none
                    )

                Neither ->
                    ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        RawWasmDocument rawDocB64 ->
            rawWasmDocumentHelp rawDocB64 model

        DownloadFile {fileName, blobId} ->
            ( { model | processes = Dict.insert model.iota (PDownload (RequestingBlob fileName)) model.processes }, sendToJs <| TrequestBlob blobId)

        ClickAddProgramButton openedDraft ->
            ( model
            , Select.file
                [ "application/wasm" ]
                ProgramSelected
            )

        ProgramSelected file ->
            ( model
            , Task.perform
                (ProgramLoaded (FileName <| File.name file)) <|
                File.toBytes file
            )

        ProgramLoaded name fileBytes ->
            ( model
            , sendToJs <|
                TnewCode { code = fileBytes, name = name }
            )

        ClickAddBlobButton id ->
            ( model
            , Select.file
                [ "application/octet-stream" ]
                (BlobSelected id)
            )

        BlobSelected id file ->
            ( model
            , Task.perform (BlobLoaded (File.name file) id) <|
                File.toBytes file
            )

        SendMessageError error ->
            ( { model | sendMessageError = Just error }, Cmd.none )

        AddNewContact ->
            addNewContactHelp model

        UpdateContactBox candidate ->
            case String.toInt candidate of
                Nothing ->
                    if candidate == "" then
                        ( { model | addContactBox = Nothing }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                Just number ->
                    if number < 0 then
                        ( model, Cmd.none )

                    else
                        let
                            newModel =
                                { model | addContactBox = Just number }
                        in
                        ( newModel, Cmd.none )

        GoodWhitelist newContact ->
            ( { model
                | myContacts = Set.insert newContact model.myContacts
              }
            , Cmd.none
            )

        BadWhitelist error ->
            ( { model | addContactError = Just error }, Cmd.none )

        CloseMessage ->
            ( { model | opened = Neither
              }
            , Cmd.none
            )

        CloseDraft ->
            ( { model | opened = Neither } , Cmd.none)

        UpdatedRecipient id rawRecipient ->
            (model
            , case String.toInt rawRecipient of
                Nothing -> Cmd.none
                Just recipient ->
                    sendToJs <|
                        TupdatedRecipient
                            {id = id, recipient = recipient}
            )

        MakeNewDraft ->
            ( model, Task.perform TimeForNewDraft Time.now )

        TimeForNewDraft posixTime ->
            ( { model | opened = ODraft { document = Nothing, id = model.iota }, iota = model.iota + 1 }, sendToJs <| TmakeNewDraft model.iota)

        DeleteBlob blobId ->
            case model.opened of
                OMessage _ -> (model, Cmd.none)
                ODraft {id} -> (model, sendToJs <| TdeleteBlob {blobId = blobId, draftId = id})
                Neither -> (model, Cmd.none)

        UpdatedUserInput draftId newUserInput ->
            (model
            , sendToJs <| TupdatedUserInput draftId newUserInput )

        UpdatedSubject draftId newSubject ->
            (model
            ,sendToJs <| TupdatedSubject draftId newSubject)


fontSize : Element.Attribute msg
fontSize =
    Font.size 25


view : Model -> Element.Element Msg
view model =
    case model.internalError of
        Just err ->
            Element.text <| "internal error: " ++ err

        Nothing ->
            Element.column [ Element.spacing 20, fontSize ]
                [ myUsernameIs model.myName
                , myContactsAre <| Set.toList model.myContacts
                , addNewContact model.addContactBox model.addContactError
                , viewMessages model.messages model.opened
                , makeNewDraft
                , editDrafts model.compilerError model.opened model.drafts
                ]


viewMessages : Dict.Dict Int Message -> Opened -> Element.Element Msg
viewMessages messages opened =
    case opened of
        OMessage {id, document} ->
            case Dict.get id messages of
                Nothing -> messageChooser messages
                Just message ->
                    viewMessage message {id=id, document=document}

        _ ->
            messageChooser messages


messageChooser : Dict.Dict Int Message -> Element.Element Msg
messageChooser messages =
    Element.column [] <| List.map messageChooserButton <|
        Dict.toList messages


messageChooserButton : (Int, Message) -> Element.Element Msg
messageChooserButton (id, message) =
    Element.Input.button []
        { onPress = Just <| OpenMessage id
        , label = messageChooserLabel message
        }


messageChooserLabel : Message -> Element.Element Msg
messageChooserLabel message =
    Element.column []
        [ Element.text <| "Subject: " ++ message.subject
        , Element.text <| "From: " ++ String.fromInt message.from
        ]


viewMessage : Message -> OpenedMessage -> Element.Element Msg
viewMessage message { document } =
    Element.column []
        [ closeMessage
        , Element.text <| "From: " ++ String.fromInt message.from
        , Element.text <| "Subject: " ++ message.subject
        , Element.text "User input:"
        , Element.paragraph [] [ Element.text message.userInput ]
        , case document of
            Nothing ->
                Element.none

            Just d ->
                displayDocument d
        , viewBlobs message.blobs
        ]


viewBlobs : Dict.Dict Int String -> Element.Element Msg
viewBlobs blobs =
    Element.column [] <|
        List.map viewBlob <|
            Dict.toList blobs


viewBlob : ( Int, String ) -> Element.Element Msg
viewBlob ( blobId, blobName ) =
    Element.row []
        [ Element.text blobName
        , Element.Input.button []
            { onPress = Just <| DownloadFile {fileName = blobName, blobId = blobId}
            , label = Element.text "Download"
            }
        ]


closeDraft : Element.Element Msg
closeDraft =
    Element.Input.button []
        { onPress = Just <| CloseDraft
        , label = Element.text "Back to drafts"
        }


closeMessage : Element.Element Msg
closeMessage =
    Element.Input.button []
        { onPress = Just <| CloseMessage
        , label = Element.text "Back to inbox"
        }


editDrafts : Maybe String -> Opened -> Dict.Dict Int Draft -> Element.Element Msg
editDrafts compilerError maybeOpenDraft drafts =
    case maybeOpenDraft of
        ODraft openDraft ->
            case Dict.get openDraft.id drafts of
                Nothing -> draftChooser drafts
                Just draft ->
                    editDraft draft compilerError openDraft

        _ ->
            draftChooser drafts


editDraft : Draft -> Maybe String -> OpenedDraft -> Element.Element Msg
editDraft draft compilerError {id, document} =
    Element.column []
        [ closeDraft
        , toBox id draft
        , subjectBox draft id
        , userInputBox draft id
        , addProgramButton id
        , showProgram draft.code
        , case compilerError of
            Nothing ->
                Element.none

            Just err ->
                Element.el [ monospace ] <| Element.text err
        , addBlobButton id
        , editBlobs draft.blobs
        , sendDraft id
        , case document of
            Nothing ->
                Element.none

            Just d ->
                displayDocument d
        ]


showProgram : Maybe { fileName : String, blobId : Int } -> Element.Element Msg
showProgram code =
    case code of
        Nothing ->
            Element.none

        Just { fileName, blobId } ->
                Element.text fileName


editBlobs : Dict.Dict Int String -> Element.Element Msg
editBlobs blobs =
    Element.column [] <|
        List.map editBlob <|
            Dict.toList blobs


editBlob : ( Int, String ) -> Element.Element Msg
editBlob ( blobId, blobName ) =
    Element.row []
        [ Element.text blobName
        , Element.Input.button []
            { onPress = Just <| DownloadFile {fileName = blobName,  blobId = blobId}
            , label = Element.text "Download"
            }
        , Element.Input.button []
            { onPress = Just <| DeleteBlob blobId
            , label = Element.text "Delete"
            }
        ]


sendDraft : Int -> Element.Element Msg
sendDraft id =
    Element.Input.button []
        { onPress = Just <| SendDraft id
        , label = Element.text "Send message"
        }


toBox : Int -> Draft -> Element.Element Msg
toBox id draft =
    Element.Input.text []
        { onChange = UpdatedRecipient id
        , text =
            case draft.to of
                Nothing ->
                    ""

                Just recipient ->
                    String.fromInt recipient
        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type the recipient ID here"
        , label = Element.Input.labelAbove [] <| Element.text "To:"
        }


addNewContactHelp : Model -> ( Model, Cmd Msg )
addNewContactHelp model =
    case model.addContactBox of
        Nothing ->
            ( model, Cmd.none )

        Just newContact ->
            if Set.member newContact model.myContacts then
                ( { model | addContactError = Just "Already a contact" }, Cmd.none )

            else
                case model.myName of
                    Nothing ->
                        ( { model | processes = Dict.insert (model.iota + 1) (PwhiteList (MakingIdToken (GettingMyName (GmakingProofOfWork PgettingInfo)))) model.processes, iota = model.iota + 1 }, getProofOfWorkInfo model.iota )

                    Just myName ->
                        if myName == newContact then
                            ( { model | addContactError = Just "You tried to add yourself" }, Cmd.none )

                        else
                            ( { model | processes = Dict.insert (model.iota + 1) (PwhiteList (MakingIdToken (GettingAuthCode (MyName myName)))) model.processes, iota = model.iota + 1 }, getAuthCode model.iota )


getProofOfWorkInfo : Int -> Cmd Msg
getProofOfWorkInfo pid =
    Http.post
        { url = apiUrl
        , body = Http.bytesBody
            "application/octet-stream"
            (E.encode <| E.unsignedInt8 3)
        , expect = Http.expectBytes (NewPowInfo pid) decodePowInfo
        }


type alias PowInfo =
    { difficulty : Int
    , unique : Bytes.Bytes
    }


decodePowInfo : D.Decoder PowInfo
decodePowInfo =
    D.map2 PowInfo
        D.unsignedInt8
        (D.bytes 8)


apiUrl = "http://localhost:3001/api"

subjectBox : Draft -> Int -> Element.Element Msg
subjectBox draft id =
    Element.Input.text []
        { onChange = UpdatedSubject id
        , text = draft.subject
        , placeholder = Just <| Element.Input.placeholder [] <| Element.text "Type the subject"
        , label = Element.Input.labelAbove [] <| Element.text "Subject:"
        }


userInputBox : Draft -> Int -> Element.Element Msg
userInputBox draft id =
    Element.Input.multiline [ monospace ]
        { onChange = UpdatedUserInput id
        , text = draft.userInput
        , placeholder =
            Just <|
                Element.Input.placeholder [] <|
                    Element.text "Type user input here"
        , label =
            Element.Input.labelAbove [] <|
                Element.text "User input:"
        , spellcheck = True
        }


addProgramButton : Int -> Element.Element Msg
addProgramButton id =
    Element.Input.button []
        { onPress = Just <| ClickAddProgramButton id
        , label = Element.text "Add program"
        }


draftChooser : Dict.Dict Int Draft -> Element.Element Msg
draftChooser drafts =
    Element.column [] <| List.map showDraftButton <|
        Dict.toList drafts


addBlobButton : Int -> Element.Element Msg
addBlobButton id =
    Element.Input.button []
        { onPress = Just <| ClickAddBlobButton id
        , label = Element.text "Add file"
        }


showDraftButton : (Int, Draft) -> Element.Element Msg
showDraftButton (id, draft) =
    Element.Input.button []
        { onPress = Just <| OpenDraft id
        , label =
            case draft.subject of
                "" ->
                    Element.text "no subject"

                _ ->
                    Element.text draft.subject
        }


monospace : Element.Attribute Msg
monospace =
    Font.family [ Font.typeface "Ubuntu Mono" ]


displayDocument : Document -> Element.Element Msg
displayDocument document =
    case document of
        Ordering documents ->
            Element.column [] <| List.map displayDocument documents

        SmallString s ->
            Element.el [ monospace ] <| Element.text s


makeNewDraft : Element.Element Msg
makeNewDraft =
    Element.Input.button []
        { onPress = Just MakeNewDraft
        , label = Element.text "Make new draft"
        }


addNewContact : Maybe Int -> Maybe String -> Element.Element Msg
addNewContact boxContents maybeError =
    Element.column [] <|
        [ Element.Input.text [ monospace ]
            { onChange = UpdateContactBox
            , text =
                case boxContents of
                    Just n ->
                        String.fromInt n

                    Nothing ->
                        ""
            , placeholder =
                Just <|
                    Element.Input.placeholder [ monospace ] <|
                        Element.text "Type their username"
            , label =
                Element.Input.labelAbove [ sansSerif ] <|
                    Element.text "Add someone to your contacts:"
            }
        , Element.Input.button [ sansSerif ]
            { onPress = Just AddNewContact
            , label = Element.text "Add new contact"
            }
        , case maybeError of
            Nothing ->
                Element.none

            Just error ->
                Element.text error
        ]


myContactsAre : List Int -> Element.Element Msg
myContactsAre contacts =
    Element.el [ sansSerif ] <|
        Element.text <|
            "My contacts: "
                ++ (case contacts of
                        [] ->
                            "you haven't got any yet"

                        oneOrMore ->
                            String.join ", " <| List.map String.fromInt oneOrMore
                   )


sansSerif : Element.Attribute msg
sansSerif =
    Font.family [ Font.typeface "EB Garamond" ]


myUsernameIs : Maybe Int -> Element.Element Msg
myUsernameIs maybeMyName =
    Element.el [ sansSerif ] <|
        Element.text <|
            "My username: "
                ++ (case maybeMyName of
                        Nothing ->
                            "you haven't got one yet"

                        Just nameInt ->
                            String.fromInt nameInt
                   )

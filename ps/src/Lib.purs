module Lib where

import Prelude
import Foreign as F
import Effect as E
import Effect.Console (log)
import Foreign.Object as Fo
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.ArrayBuffer.Types as Dat
import Data.Map as Map
import Localforage as Localforage

main = log "hello"

foreign import cacheSet :: String -> F.Foreign -> E.Effect F.Foreign

foreign import cacheGet :: String -> E.Effect F.Foreign

foreign import toElm :: String -> E.Effect Unit

elmHandler :: F.Foreign -> E.Effect Unit
elmHandler fromElm =
    case decodeFromElm fromElm of
        Left err ->
            toElm $ encodeToElm $ InternalError err

        Right decoded ->
            elmHandlerHelp decoded


sendToElm :: ToElm -> E.Effect Unit
sendToElm msg =
    toElm $ encodeToElm msg


data ToElm
    = UpdatedDraft String Draft
    | UpdatedMessages (Map.Map String Message)
    | WasmOutput String (Dat.ArrayView Dat.Uint8)
    | SendError String
    | InternalError String


type Message =
    { from :: Int
    , to :: Int
    , time :: Int
    , subject :: String
    , userInput :: String
    , code :: Code
    , blobs :: Map.Map String String
    }


type Draft =
    { to :: Maybe Int
    , time :: Maybe Int
    , subject :: String
    , userInput :: String
    , code :: Maybe Code
    , blobs :: Map.Map String String
    }


decodeMaybe
    :: String
    -> F.Foreign
    -> (F.Foreign -> Either String a)
    -> Either String (Maybe a)
decodeMaybe key object decoder =
    case Fo.lookup key object of
        Nothing ->
            Right Nothing

        Just rawValue ->
            case decoder rawValue of
                Left err ->
                    Left err

                Right decoded ->
                    Right $ Just decoded


decodeInt :: F.Foreign -> Either String Int
decodeInt foreign =
    let
        valueType = F.typeOf foreign
    in
        if valueType == "number" then
            Right $ F.unsafeFromForeign foreign
        else
            Left $ "expecting number, got " <> valueType


lookupMaybeInt :: String -> F.Foreign -> Either String (Maybe Int)
lookupMaybeInt key object =
    decodeMaybe key object decodeInt


decodeDraft :: F.Foreign -> Either String Draft
decodeDraft foreign = do
    to <- lookupMaybeInt "to" foreign
    time <- lookupMaybeInt "time" foreign
    subject <- lookupString "subject" foreign
    userInput <- lookupString "userInput" foreign

    
    


type Code =
    { fileName :: String, blobId :: String }


elmHandlerHelp :: FromElm -> E.Effect Unit
elmHandlerHelp fromElm =
    case fromElm of
        TupdatedUserInput u ->
            updatedUserInput u
            

type UpdatedUserInput = { id :: String, userInput :: String }


updatedUserInput :: UpdatedUserInput -> E.Effect Unit
updatedUserInput u = do
    eitherOldDraft <- getDraft u.id
    case eitherOldDraft of
        Left err ->
            sendToElm $ InternalError err

        Right oldDraft -> do
            let newDraft = oldDraft { userInput = u.userInput }
            writeDraft newDraft
            sendToElm $ UpdatedDraft u.id newDraft


getDraft :: String -> E.Effect (Either String Draft)
getDraft id = do
    candidate <- cacheGet id
    case decodeDraft candidate of
        Left _ -> do
            eitherDisk <- Localforage.getItem id
            case eitherDisk of
                Left err ->
                    pure $ Left err

                Right foreignDraft ->
                    decodeDraft foreignDraft

        Right draft ->
            Right draft
    

data FromElm
    = TupdatedUserInput UpdatedUserInput
    | TupdatedRecipient { id :: String, recipient :: Int }
    | TupdatedSubject { id :: String, subject :: String }
    | TnewCode { code :: Dat.ArrayView Dat.Uint8, filename :: String }
    | TrequestBlob String
    | TmakeNewDraft
    | TdeleteBlob { blobId :: String, draftId :: String }
    | TaddNewContact Int
    | TrunDraftWasm String
    | TrunMessageWasm String
    | TsendDraft String
    | TnewBlob LoadedBlob


type LoadedBlob =
    { fileName :: String
    , draftId :: String
    , contents :: Dat.ArrayView Dat.Uint8
    }


lookupString :: String -> F.Foreign -> Either String String
lookupString key object =
    case Fo.lookup key object of
        Nothing ->
            Left $ "no key \"" <> key <> "\""

        Just value ->
            let
                valueType = F.typeOf value
            in
                if valueType == "string" then
                    Right $ F.unsafeFromForeign value

                else
                    Left $ "expecting string, got " <> valueType


decodeUpdatedUserInput :: F.Foreign -> Either String FromElm
decodeUpdatedUserInput candidate = do
    id <- lookupString "id" candidate
    userInput <- lookupString "userInput" candidate
    pure $ TupdatedUserInput { id = id, userInput = userInput }


decodeFromElm :: F.Foreign -> Either String FromElm
decodeFromElm fromElm =
    case Fo.lookup "key" fromElm of
        Nothing ->
            Left "no key \"key\""

        Just key ->
            case Fo.lookup "value" fromElm of
                Nothing ->
                    Left "no key \"value\""

                Just value ->
                    decodeFromElmHelp key value


decodeFromElmHelp :: String -> F.Foreign -> Either String FromElm
decodeFromElmHelp key value =
    case key of
        "updatedUserInput" ->
            decodeUpdatedUserInput value

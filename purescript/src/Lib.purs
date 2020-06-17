module Lib where

import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Prelude (Unit, bind, discard, pure, unit)
import Data.Array as Array
import Data.Maybe


main :: Effect Unit
main =
    log "hi"


data State =
    Black


initState :: State
initState = Black

globalStateRef :: Effect (Ref.Ref State)
globalStateRef = Ref.new initState

tick :: Input -> Effect Unit
tick input = do
    stateRef <- globalStateRef
    oldState <- Ref.read stateRef
    let stepped = step input oldState
    _ <- Ref.modify (update stepped.update) stateRef
    mapM_ doIo stepped.outputs


mapM_ :: (Output -> Effect Unit) -> Array Output -> Effect Unit
mapM_ doIo_ outputs =
    case Array.uncons outputs of
        Just {head : o, tail : os} -> do
            doIo o
            mapM_ doIo_ os

        Nothing ->
            pure unit


update :: Update -> State -> State
update updateValue state =
    case updateValue of
        UpdateColor ->
            Black


data Input
    = Init

data Update
    = UpdateColor

type Stepped = {update :: Update, outputs :: Array Output}

step :: Input -> State -> Stepped
step input state =
    case input of
        Init ->
            {update : UpdateColor, outputs : [Log "hi"]}


data Output
    = Log String


doIo :: Output -> Effect Unit
doIo output =
    case output of
        Log toLog ->
            log toLog

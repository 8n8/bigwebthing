module Main where

import qualified Control.Concurrent.STM.TQueue as Q
import qualified Control.Concurrent.STM as Stm
import Control.Concurrent (forkIO)
import qualified Data.ByteString as B
import qualified Control.Exception as E
import qualified Crypto.Noise.DH as Dh
import Crypto.Noise.DH.Curve25519 (Curve25519)
import qualified Data.ByteArray as Ba


main :: IO ()
main =
    do
    inQ <- Stm.atomically Q.newTQueue
    writeQ inQ Start
    mainHelp initState inQ


mainHelp :: State -> Q.TQueue In -> IO ()
mainHelp oldState inQ =
    do
    input <- readQ inQ
    let (newState, output) = update oldState input
    forkIO $ run output inQ
    mainHelp newState inQ


update :: State -> In -> (State, Out)
update state input =
    case input of

    Start ->
        (state, ReadKeys)

    KeysFile (Left _) ->
        (state, MakeKeys)

    KeysFile (Right raw) ->
        case Dh.dhBytesToPair $ Ba.convert raw of
        Nothing ->
            (state, Panic "couldn't make static keys")

        Just keys ->
            (state { staticKeysS = GotKeys keys }, GetListener)


staticKeysPath :: String
staticKeysPath =
    "staticKeys"


run :: Out -> Q.TQueue In -> IO ()
run output inQ =
    case output of

    ReadKeys ->
        do
        eitherRaw <- E.try $ B.readFile staticKeysPath
        writeQ inQ $ KeysFile eitherRaw

    DoNothing ->
        return ()

    MakeKeys ->
        do
        keys <- Dh.dhGenKey
        writeQ inQ $ NewKeys keys


writeQ :: Stm.TQueue a -> a -> IO ()
writeQ q v =
    Stm.atomically $ Q.writeTQueue q v


data In
    = Start
    | KeysFile (Either IOError B.ByteString)
    | NewKeys NoiseKeys


data Out
    = ReadKeys
    | DoNothing
    | MakeKeys
    | Panic String
    | GetListener


readQ :: Stm.TQueue a -> IO a
readQ =
    Stm.atomically . Q.readTQueue


initState :: State
initState =
    State
    { staticKeysS = NoKeys
    }


data KeysState
    = NoKeys
    | GotKeys NoiseKeys


type NoiseKeys = Dh.KeyPair Curve25519


data State =
    State
    { staticKeysS :: KeysState
    }

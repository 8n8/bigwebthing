{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import qualified Control.Concurrent.STM.TVar as TVar
import qualified Control.Concurrent.STM as Stm
import System.Environment (getArgs)
import qualified System.IO as Sio
import Control.Exception (try)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import qualified Control.Concurrent.STM.TQueue as Q
import Crypto.Noise.DH.Curve25519 (Curve25519)
import qualified Crypto.Noise.DH as Dh
import qualified Data.ByteString as B
import qualified Data.ByteArray as Ba
import qualified Network.Simple.TCP as Tcp


main :: IO ()
main =
    do
    state <- Stm.atomically $ TVar.newTVar initState
    mainHelp state Start


initState :: State
initState =
    State
    { expectingSizeS = False
    , socketS = Nothing
    }


updateStm :: TVar.TVar State -> In -> Stm.STM Out
updateStm stateVar input =
    do
    oldState <- TVar.readTVar stateVar
    let (newState, output) = update oldState input
    TVar.writeTVar stateVar newState
    return output


mainHelp :: TVar.TVar State -> In -> IO ()
mainHelp state in_ =
    do
    out <- Stm.atomically $ updateStm state in_
    run state out


data In
    = Start
    | ServerConn Tcp.Socket
    | MessageFromServer (Maybe B.ByteString)


type NoiseKeys
    = Dh.KeyPair Curve25519


data State
    = State
    { socketS :: Maybe Tcp.Socket
    , expectingSizeS :: Bool
    , handshake :: Maybe Noise.HandshakeState
    }


data Out
    = ConnectToServer
    | ReadKeysFile
    | ReadConn Tcp.Socket Int
    | Print T.Text


run :: Stm.TVar State -> Out -> IO ()
run state out =
    case out of

    ConnectToServer ->
        Tcp.connect "127.0.0.1" "3000" $ \(conn, _) ->
        mainHelp state $ ServerConn conn

    ReadConn socket size ->
        do
        message <- Tcp.recv socket size
        mainHelp state $ MessageFromServer message


badServer :: State -> (State, Out)
badServer state =
    (state { socketS = Nothing }, Print "bad internet")


onMessageFromServer :: State -> B.ByteString -> (State, Out)
onMessageFromServer state raw =
    if expectingSizeS state then

        if B.length raw == 2 then
        let
        size = B.index raw 0 + (B.index raw 1 << 8)
        in
        case serverConnS state of
        Nothing ->
            badServer state

        Just socket ->
            ( state { expectingSizeS = False }
            , ReadConn socket size
            )

        else
        badServer state

    else
        onBodyFromServer raw state


onBodyFromServer :: B.ByteString -> State -> (State, Out)
onBodyFromServer encrypted state =
    case 
    Noise.readMessage encrypted (


update :: State -> In -> (State, Out)
update state in_ =
    case in_ of

    MessageFromServer Nothing ->
        badServer state

    MessageFromServer (Just raw) ->
        onMessageFromServer state raw

    Start ->
        (state, ReadKeysFile)

    ServerConn conn ->
        ( state
            { expectingSizeS = True
            , socketS = Just conn
            }
        , ReadConn conn 2
        )

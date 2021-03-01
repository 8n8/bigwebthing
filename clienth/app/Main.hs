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
import qualified Crypto.Noise as Noise
import qualified Crypto.Noise.DH as Dh
import Crypto.Noise.Cipher.AESGCM (AESGCM)
import Crypto.Noise.Hash.SHA256 (SHA256)
import Crypto.Noise.DH.Curve25519 (Curve25519)
import qualified Data.Attoparsec.ByteString as P


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


type NoiseState
    = Noise.NoiseState AESGCM Curve25519 SHA256


data State
    = State
    { socketS :: Maybe Tcp.Socket
    , expectingSizeS :: Bool
    , handshakeS :: Maybe NoiseState
    , kk1FromS :: Maybe (Kk1, PublicKey)
    }


type PublicKey
    = Dh.PublicKey Curve25519


newtype Kk1
    = Kk1 B.ByteString


data FromServer
    = NewKk1F Kk1 PublicKey


data Out
    = ConnectToServer
    | ReadKeysFile
    | ReadConn Tcp.Socket Int
    | Print T.Text
    | MakeKeyPair


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
    case handshakeS state of
    Nothing ->
        badServer state

    Just handshake ->
        case Noise.readMessage encrypted handshake of
        Noise.NoiseResultMessage plain handshake' ->
            let
            state' = state { handshakeS = Just handshake' }
            in
            onPlainFromServer plain state'


parseFromServer :: B.ByteString -> Either T.Text FromServer
parseFromServer raw =
    case P.parseOnly fromServerP raw of
    Left err ->
        Left $ T.pack err

    Right ok ->
        Right ok


onPlainFromServer :: B.ByteString -> State -> (State, Out)
onPlainFromServer plain state =
    case parseFromServer plain of
    Left err ->
        badServer state

    Right fromServer ->
        onGoodFromServer fromServer state


onGoodFromServer :: FromServer -> State -> (State, Out)
onGoodFromServer fromServer state =
    case fromServer of
    NewKk1F kk1 theirId ->
        (state { kk1FromS = Just (kk1, theirId) }, MakeKeyPair)


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

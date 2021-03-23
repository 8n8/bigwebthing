{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Foundation
import Foundation.IO
import Foundation.VFS.FilePath
import Foundation.VFS.Path
import Foundation.Collection
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Control.Monad.STM (atomically)
import qualified Control.Exception as E
import qualified Crypto.Noise.DH as Dh
import Crypto.Noise.DH.Curve25519 (Curve25519)
import qualified Data.ByteArray as Ba
import GHC.IO.Handle (Handle)
import qualified Database.SQLite.Simple as Sql
import qualified Network.Simple.TCP as T
import Data.Map (Map, insert, empty)


main :: IO ()
main =
    do
    state <- atomically $ newTVar initState
    mainHelp state Start


mainHelp :: TVar State -> Input -> IO ()
mainHelp stateTVar input =
    do
    output <- atomically $
        do
        state <- readTVar stateTVar
        let (state', output) = update input state
        writeTVar stateTVar state'
        return output
    run stateTVar output


run :: TVar State -> Output -> IO ()
run state output =
    case output of
    TcpRecv address socket ->
        do
        message <- E.try $ T.recv socket xk1Size
        mainHelp state $ TcpMessage address message

    Sequence outs ->
        mapM_ (run state) outs
        
    Put handle bytes ->
        hPut handle bytes
        
    ReadFile path ->
        do
        contents <- E.try $ readFile path
        mainHelp state (FileContents path contents)

    GenerateNoiseKeys ->
        do
        keys <- Dh.dhGenKey
        mainHelp state (NewDhKeys keys)

    Panic err ->
        error err

    GetWriteHandle path ->
        withFile path WriteMode $ \handle ->
        mainHelp state (WriteHandle path handle)

    GetDbConn ->
        Sql.withConnection (filePathToLString dbPath) $ \conn ->
        mainHelp state (DbConn conn)

    StartTcpServer ->
        T.serve (T.Host "127.0.0.1") "8000" $ \(conn, address) ->
        mainHelp state (NewTcpConn conn address)


initState =
    State
    { staticKeys = Nothing
    , db = Nothing
    , connsAwaitingKeys = []
    , connsAwaitingXk1 = empty
    }


data State
    = State
    { staticKeys :: !(Maybe KeyPair)
    , db :: !(Maybe Sql.Connection)
    , connsAwaitingKeys :: [(T.SockAddr, T.Socket)]
    , connsAwaitingXk1 :: Map T.SockAddr ConnAwaitingXk1
    }


data ConnAwaitingXk1
    = ConnAwaitingXk1
    { conn :: !T.Socket
    , ephemeral :: KeyPair
    }


data Output
    = ReadFile FilePath
    | Panic String
    | GenerateNoiseKeys
    | GetDbConn
    | GetWriteHandle FilePath
    | Put Handle (UArray Word8)
    | Sequence [Output]
    | StartTcpServer
    | TcpRecv T.SockAddr T.Socket


data Input
    = Start
    | FileContents FilePath (Either IOException (UArray Word8))
    | NewDhKeys KeyPair
    | WriteHandle FilePath Handle
    | DbConn Sql.Connection
    | NewTcpConn T.Socket T.SockAddr


type KeyPair
    = Dh.KeyPair Curve25519


topPath :: FilePath
topPath =
    "bigwebthing"


staticKeysPath :: FilePath
staticKeysPath =
    topPath </> "statickeys"


dbPath :: FilePath
dbPath =
    topPath </> "database.sqlite"


update :: Input -> State -> (State, Output)
update Start state =
    (state, ReadFile staticKeysPath)


update (FileContents path eitherContents) state =
    if path == staticKeysPath then
    case eitherContents of
    Left _ ->
        (state, GenerateNoiseKeys)

    Right contents ->
        case Dh.dhBytesToPair $ Ba.convert contents of
        Nothing ->
            (state, Panic "couldn't parse static keys")

        Just keys ->
            (state { staticKeys = Just keys }, GetDbConn)

    else
    (state, Panic $ "unknown file: " <> show path)


update (NewDhKeys keys) state =
    case staticKeys state of
    Nothing ->
        ( state { staticKeys = Just keys }
        , GetWriteHandle staticKeysPath
        )

    Just _ ->
        keysForConn keys state


update (WriteHandle path handle) state =
    if path == staticKeysPath then
    case staticKeys state of
    Nothing ->
        (state, Panic "no static keys to write to file")

    Just keys ->
        (state, Sequence [Put handle (encodeKeys keys), GetDbConn])

    else
    (state, Panic $ "unexpected file write handle: " <> show path)


update (DbConn conn) state =
    (state { db = Just conn }, StartTcpServer)


update (NewTcpConn conn address) state =
    ( state
        { connsAwaitingKeys =
            (address, conn) : connsAwaitingKeys state
        }
    , GenerateNoiseKeys
    )


keysForConn keys state =
    case connsAwaitingKeys state of
    [] ->
        (state, Panic "got unwanted keys")

    (address, conn):onns -> 
        ( state
            { connsAwaitingKeys = onns
            , connsAwaitingXk1 =
                insert
                    address
                    (ConnAwaitingXk1 conn keys)
                    (connsAwaitingXk1 state)
            }
        , TcpRecv address conn
        )


encodeKeys :: KeyPair -> UArray Word8
encodeKeys (secret, _) =
    Ba.convert $ Dh.dhSecToBytes secret

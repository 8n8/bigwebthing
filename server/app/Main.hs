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
import qualified Crypto.Noise as N
import qualified Crypto.Noise.DH as Dh
import Crypto.Noise.DH.Curve25519 (Curve25519)
import qualified Data.ByteArray as Ba
import GHC.IO.Handle (Handle)
import qualified Database.SQLite.Simple as Sql
import qualified Network.Simple.TCP as T
import qualified Data.Map as M
import qualified Data.ByteString as B
import Crypto.Noise.Cipher.ChaChaPoly1305 (ChaChaPoly1305)
import Crypto.Noise.Hash.BLAKE2s (BLAKE2s)
import Crypto.Noise.HandshakePatterns (noiseXK)


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


xk1Size =
    48


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
    , conns = M.empty
    }


data State
    = State
    { staticKeys :: !(Maybe KeyPair)
    , db :: !(Maybe Sql.Connection)
    , connsAwaitingKeys :: [(T.SockAddr, T.Socket)]
    , conns :: M.Map T.SockAddr ConnStatus
    }


data ConnStatus
    = Xk1W T.Socket KeyPair
    | Xk3W T.Socket NoiseState
    | AnyW T.Socket NoiseState


type NoiseState
    = N.NoiseState ChaChaPoly1305 Curve25519 BLAKE2s


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
    | TcpSend T.SockAddr T.Socket B.ByteString
    | DoNothing
    | DbQuery T.SockAddr Sql.Query B.ByteString


data Input
    = Start
    | FileContents FilePath (Either IOException (UArray Word8))
    | NewDhKeys KeyPair
    | WriteHandle FilePath Handle
    | DbConn Sql.Connection
    | NewTcpConn T.Socket T.SockAddr
    | TcpMessage T.SockAddr (Either IOException (Maybe B.ByteString))


type KeyPair
    = Dh.KeyPair Curve25519


topPath :: FilePath
topPath =
    "bigwebthing"


staticKeysPath :: FilePath
staticKeysPath =
    topPath </> "statickeys"


dbPathLegacy =
    filePathToLString dbPath


dbPath :: FilePath
dbPath =
    topPath </> "database.sqlite"


update :: Input -> State -> (State, Output)
update Start state =
    (state, ReadFile staticKeysPath)


update (TcpMessage address rawMessage) state =
    (\f -> case staticKeys state of
        Nothing ->
            (state, Panic "got TCP message but no static keys")

        Just keys ->
            f keys) $ \staticKeys ->

    (\f ->
    let bad =
            ( state { conns = M.delete address $ conns state }
            , DoNothing
            )
    in
    case rawMessage of
        Left err ->
            bad

        Right Nothing ->
            bad

        Right (Just bytes) ->
            f bytes) $ \bytes ->

    case M.lookup address $ conns state of
    Nothing ->
        ( state
        , Panic $ mconcat
            [ "unexpected TCP message from "
            , show address 
            , ": "
            , show rawMessage
            ]
        )

    Just (Xk1W socket ephemeralKeys) ->
        onXk1 bytes socket staticKeys ephemeralKeys address state

    Just (Xk3W socket noise) ->
        onXk3 bytes socket noise address state


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


onXk3
    :: B.ByteString
    -> T.Socket
    -> NoiseState
    -> T.SockAddr
    -> State
    -> (State, Output)
onXk3 raw socket noise address state =
    case N.readMessage (Ba.convert raw) noise of
    N.NoiseResultMessage "" noise' ->
        ( state
            { conns =
                M.insert
                    address
                    (AnyW socket noise')
                    (conns state)
            }
        , fetchFirstDataBatch address noise'
        )

    N.NoiseResultNeedPSK _ ->
        (state, Panic "NoiseResultNeedPSK")

    N.NoiseResultException _ ->
        ( state { conns = M.delete address (conns state) }
        , DoNothing
        )


fetchFirstDataBatch :: T.SockAddr -> NoiseState -> Output
fetchFirstDataBatch address noise =
    (\f -> case N.remoteStaticKey noise of
        Nothing ->
            Panic "No remote static key"

        Just remoteKey ->
            f remoteKey) $ \theirKey ->

    let
    q sql =
        DbQuery address sql $
        Ba.convert $
        Dh.dhPubToBytes theirKey
    in
    Sequence
        [ q getKk1sSql
        , q getKk2sSql
        , q getKkTransportsSql
        , q getPaymentsSql
        , q getBlobUploadsSql
        ]


getKk1sSql :: Sql.Query
getKk1sSql =
    "SELECT (kk1, sender) \
    \FROM kk1s \
    \WHERE sender=? OR recipient=?;"


getKk2sSql :: Sql.Query
getKk2sSql =
    "SELECT (sessionid, kk2, sender) \
    \FROM kk2s \
    \WHERE sender=? OR recipient=?;"


getKkTransportsSql :: Sql.Query
getKkTransportsSql =
    "SELECT (sessionid, kktransport, sender, timestamp) \
    \FROM kktransports \
    \WHERE sender=? OR recipient=?;"


getBlobUploadsSql :: Sql.Query
getBlobUploadsSql =
    "SELECT (timestamp, blobid) \
    \FROM blobuploads \
    \WHERE uploader=?;"


getPaymentsSql :: Sql.Query
getPaymentsSql =
    "SELECT (amount, timestamp, signature) \
    \FROM payments \
    \WHERE payer=?;"


onXk1
    :: B.ByteString
    -> T.Socket
    -> KeyPair
    -> KeyPair
    -> T.SockAddr
    -> State
    -> (State, Output)
onXk1 raw socket static ephemeral address state =
    let
    options = noiseOptions static ephemeral
    noise :: NoiseState
    noise = N.noiseState options noiseXK
    bad = (state {conns = M.delete address $ conns state}, DoNothing)
    panic = (state, Panic "NoiseResultNeedPSK")
    in
    case N.readMessage (Ba.convert raw) noise of
    N.NoiseResultMessage "" noise' ->
        case N.writeMessage "" noise' of
        N.NoiseResultMessage xk2 noise'' ->
            ( state
                { conns =
                    M.insert
                        address
                        (Xk3W socket noise'')
                        (conns state)
                }
            , Sequence
                [ TcpSend address socket (Ba.convert xk2)
                , TcpRecv address socket
                ]
            )

        N.NoiseResultNeedPSK _ ->
            panic

        N.NoiseResultException _ ->
            panic

    N.NoiseResultMessage _ _ ->
        bad

    N.NoiseResultNeedPSK _ ->
        panic

    N.NoiseResultException _ ->
        bad
            

noiseOptions :: KeyPair -> KeyPair -> N.HandshakeOpts Curve25519
noiseOptions myStatic myEphemeral =
    N.setLocalStatic (Just myStatic)
    . N.setLocalEphemeral (Just myEphemeral)
    $ N.defaultHandshakeOpts N.ResponderRole ""


keysForConn keys state =
    case connsAwaitingKeys state of
    [] ->
        (state, Panic "got unwanted keys")

    (address, conn):onns -> 
        ( state
            { connsAwaitingKeys = onns
            , conns =
                M.insert address (Xk1W conn keys) (conns state)
            }
        , TcpRecv address conn
        )


encodeKeys :: KeyPair -> UArray Word8
encodeKeys (secret, _) =
    Ba.convert $ Dh.dhSecToBytes secret

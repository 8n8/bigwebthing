{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Control.Concurrent.STM as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import Control.Concurrent (forkIO)
import qualified Data.ByteString as B
import qualified Control.Exception as E
import qualified Crypto.Noise.DH as Dh
import Crypto.Noise.DH.Curve25519 (Curve25519)
import qualified Data.ByteArray as Ba
import qualified Network.Simple.TCP as Tcp
import qualified Data.Map as Map
import Crypto.Noise.Hash.SHA256 (SHA256) 
import Crypto.Noise.Cipher.AESGCM (AESGCM)
import qualified Crypto.Noise as Noise
import Crypto.Noise.HandshakePatterns (noiseXK)
import qualified Data.Text as T
import qualified Data.Attoparsec.ByteString as P
import qualified Database.SQLite.Simple as Db
import Debug.Trace (trace)


main :: IO ()
main =
    do
    state <- Stm.atomically $ TVar.newTVar initState
    mainHelp state Start


data NoiseState
    = NoiseState (Noise.NoiseState AESGCM Curve25519 SHA256)
    | MakingEphemeral


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
    _ <- forkIO $ run state out
    return ()


sequenceHelp :: TVar.TVar State -> [Out] -> IO ()
sequenceHelp state outs =
    trace "sequenceHelp" $
    trace (show outs) $
    case outs of
    [] ->
        trace "no outs" $
        return ()

    o:uts ->
        trace "o:uts" $
        do
        run state o
        sequenceHelp state uts


parallelsHelp :: TVar.TVar State -> [Out] -> IO ()
parallelsHelp state outs =
    case outs of
    [] ->
        return ()

    o:uts ->
        do
        _ <- forkIO $ run state o
        parallelsHelp state uts


saveKk2Sql :: Db.Query
saveKk2Sql =
    "INSERT INTO kk2s (kk2, sessionid, sender, recipient) VALUES (?, ?, ?, ?);"


getNewKk1sSql :: Db.Query
getNewKk1sSql =
    "SELECT (kk1, sessionid, sender) FROM kk1s WHERE sessionid NOT IN (SELECT sessionid FROM kk2s);"


createTablesSql :: [Db.Query]
createTablesSql =
    [ "CREATE TABLE IF NOT EXISTS payments (payer INTEGER NOT NULL, amount INTEGER NOT NULL, PRIMARY KEY (payer, amount));"
    , "CREATE TABLE IF NOT EXISTS kk1s (kk1 UNIQUE BLOB NOT NULL, sessionid UNIQUE BLOB NOT NULL, sender BLOB NOT NULL, recipient BLOB NOT NULL);"
    , "CREATE TABLE IF NOT EXISTS kk2s (kk2 UNIQUE BLOB NOT NULL, sessionid UNIQUE BLOB NOT NULL, sender BLOB NOT NULL, recipient BLOB NOT NULL);"
    ]


run :: TVar.TVar State -> Out -> IO ()
run state out =
    trace ("run: " <> show out) $
    case out of

    SaveKk2InDb address row ->
        Db.withConnection dbPath $ \conn ->
        do
        result <- E.try $ Db.execute conn saveKk2Sql row
        mainHelp state $ SaveKk2Result address result

    FetchNewKk1sFor address sender ->
        Db.withConnection dbPath $ \conn ->
        Db.fold conn getNewKk1sSql (Db.Only sender) () $ \_ row  ->
        mainHelp state $ NewKk1For address row

    SetupDb ->
        trace "run SetupDb" $
        Db.withConnection dbPath $ \conn ->
        mapM_ (Db.execute_ conn) createTablesSql

    Parallel outs ->
        parallelsHelp state outs

    Sequence outs ->
        sequenceHelp state outs

    ReadKeys ->
        trace "run ReadKeys" $
        do
        eitherRaw <- E.try $ B.readFile staticKeysPath
        mainHelp state $ KeysFile eitherRaw

    DoNothing ->
        return ()

    MakeKeys ->
        do
        keys <- Dh.dhGenKey
        mainHelp state $ NewKeys keys

    Panic err ->
        error $ show err

    StartServer ->
        Tcp.serve (Tcp.Host "127.0.0.1") "3000" $ \conn ->
        mainHelp state $ NewSock conn

    WriteKeys keys ->    
        B.writeFile staticKeysPath keys

    SocketReceive address size socket ->
        do
        message <- Tcp.recv socket size
        mainHelp state $ FromSocket address size message

    SocketSend address socket message ->
        do
        result <- E.try $ Tcp.send socket message
        mainHelp state $ SocketSendResult address result


makeKk1Message
    :: Conn
    -> B.ByteString
    -> B.ByteString
    -> B.ByteString
    -> Maybe (B.ByteString, Conn)
makeKk1Message conn kk1 sessionId sender =
    let
    plain = B.singleton 0 <> sessionId <> kk1 <> sender
    in
    case noise conn of
    MakingEphemeral ->
        Nothing

    NoiseState noise' ->
        case Noise.writeMessage (Ba.convert plain) noise' of
        Noise.NoiseResultNeedPSK _ ->
            Nothing

        Noise.NoiseResultException _ ->
            Nothing

        Noise.NoiseResultMessage encrypted noise'' ->
            Just
                ( Ba.convert encrypted
                , conn { noise = NoiseState noise'' }
                )


update :: State -> In -> (State, Out)
update state input =
    case input of

    NewKk1For address (rawKk1, rawSessionId, rawSender) ->
        case Map.lookup address $ connsS state of
        Nothing ->
            (state, DoNothing)

        Just conn ->
            case makeKk1Message conn rawKk1 rawSessionId rawSender of
            Nothing ->
                (state, DoNothing)

            Just (msg, newConn) ->
                let
                newConns = Map.insert address newConn (connsS state)
                in
                ( state { connsS = newConns }
                , SocketSend address (socketC conn) msg
                )

    Start ->
        trace "Start" $
        (state, Sequence [SetupDb, ReadKeys])

    KeysFile (Left _) ->
        trace "KeysFile Left" $
        (state, MakeKeys)

    KeysFile (Right raw) ->
        case Dh.dhBytesToPair $ Ba.convert raw of
        Nothing ->
            (state, Panic "couldn't make static keys")

        Just keys ->
            (state { staticKeysS = GotKeys keys }, StartServer)

    NewKeys keys ->
        onNewKeys state keys

    NewSock (socket, address) ->
        ( state
            { connsS =
                Map.insert address (initConn socket) (connsS state)
            }
        , MakeKeys
        )

    FromSocket address _ Nothing ->
        ( state { connsS = Map.delete address (connsS state) }
        , DoNothing
        )

    FromSocket address size (Just message) ->
        onFromSocket state address size message

    SocketSendResult _ (Right ()) ->
        (state, DoNothing)

    SocketSendResult address (Left _) ->
        (deleteConn address state, DoNothing)

    SaveKk2Result _ (Right ()) ->
        (state, DoNothing)

    SaveKk2Result address (Left _) ->
        (deleteConn address state, DoNothing)


onFromSocket
    :: State
    -> Tcp.SockAddr
    -> Int
    -> B.ByteString
    -> (State, Out)
onFromSocket state address size message =
    if B.length message /= size then
    ( state { connsS = Map.delete address (connsS state) }
    , DoNothing
    )
    else
    case Map.lookup address (connsS state) of
    Nothing ->
        ( state
        , Panic $
            T.concat
            [ "no connection with address: "
            , T.pack (show address)
            ]
        )

    Just conn ->
        onMessageToConn address conn state message


sizeP :: B.ByteString -> Maybe Int
sizeP raw =
    if B.length raw /= 2 then
    Nothing
    else
    let
    little :: Int
    little = fromIntegral $ raw `B.index` 0
    big :: Int
    big = fromIntegral $ raw `B.index` 1
    in
    Just $ little + big * 256


onMessageToConn
    :: Tcp.SockAddr
    -> Conn
    -> State
    -> B.ByteString
    -> (State, Out)
onMessageToConn address conn state message =
    (\n f ->
    case n of
    NoiseState noiseState ->
        f noiseState

    MakingEphemeral ->
        (state, Panic "no noise state onMessageToConn")
    ) (noise conn) $ \noiseState ->
    if readingSizeC conn then
    let
    newConn = conn { readingSizeC = False }
    newConns = Map.insert address newConn (connsS state)
    in
    case sizeP message of
    Nothing ->
        (deleteConn address state, DoNothing)

    Just size ->
        ( state { connsS = newConns }
        , SocketReceive address size (socketC conn)
        )
    else
    case expectingC conn of
    Xk1E ->
        case Noise.readMessage (Ba.convert message) noiseState of

        Noise.NoiseResultNeedPSK _ ->
            (deleteConn address state, DoNothing)

        Noise.NoiseResultException _ ->
            (deleteConn address state, DoNothing)

        Noise.NoiseResultMessage "" noise' ->
            case Noise.writeMessage "" noise' of
            Noise.NoiseResultNeedPSK _ ->
                (deleteConn address state, DoNothing)

            Noise.NoiseResultException _ ->
                (deleteConn address state, DoNothing)

            Noise.NoiseResultMessage xk2 noise'' ->
                let
                newConn =
                    conn
                        { noise = NoiseState noise''
                        , readingSizeC = True
                        , expectingC = WithPayloadE
                        }
                newConns = Map.insert address newConn $ connsS state
                in
                ( state { connsS = newConns }
                , Parallel
                    [ SocketSend address (socketC conn) (Ba.convert xk2)
                    , SocketReceive address 2 (socketC conn)
                    ]
                )

        Noise.NoiseResultMessage _ _ ->
            (deleteConn address state, DoNothing)

    WithPayloadE ->
        case Noise.readMessage (Ba.convert message) noiseState of
        Noise.NoiseResultNeedPSK _ ->
            (deleteConn address state, DoNothing)

        Noise.NoiseResultException _ ->
            (deleteConn address state, DoNothing)

        Noise.NoiseResultMessage plain noise' ->
            case parseFromClient $ Ba.convert plain of

            Nothing ->
                (deleteConn address state, DoNothing)

            Just decoded ->
                onGoodMessage
                    decoded
                    address
                    (updateNoise address noise' state)


updateNoise
    :: Tcp.SockAddr
    -> Noise.NoiseState AESGCM Curve25519 SHA256
    -> State
    -> State
updateNoise address newNoise state =
    case Map.lookup address (connsS state) of
    Nothing ->
        state

    Just conn ->
        let
        newConn = conn { noise = NoiseState newNoise }
        in
        state { connsS = Map.insert address newConn (connsS state) }


pubToBytes :: PublicKey -> B.ByteString
pubToBytes =
    Ba.convert . Dh.dhPubToBytes


dbPath :: String
dbPath =
    "db.sqlite"


onGoodMessage :: FromClient -> Tcp.SockAddr -> State -> (State, Out)
onGoodMessage fromClient address state =
    case fromClient of
    UploadKk2F recipient sessionId kk2 ->
        case getSenderId address (connsS state) of
        Nothing ->
            ( state
            , Panic $
                mconcat
                [ "could not find static key for "
                , T.pack $ show address
                , " for KK2 upload"
                ]
            )

        Just senderId ->
            ( expectingSize address state
            , Parallel
                [ SaveKk2InDb
                    address
                    ( (\(Kk2 k) -> k) kk2
                    , (\(SessionId s) -> s) sessionId
                    , pubToBytes  senderId
                    , pubToBytes recipient
                    )
                , getSize address state
                ]
            )

    DownloadNewKk1sToMeF ->
        case getSenderId address (connsS state) of
        Nothing ->
            ( state
            , Panic $
                mconcat
                [ "could not find static key for "
                , T.pack $ show address
                , " for request for new KK1s"
                ]
            )

        Just senderId ->
            ( expectingSize address state
            , Parallel
                [ FetchNewKk1sFor address $ pubToBytes senderId
                , getSize address state
                ]
            )


getSize :: Tcp.SockAddr -> State -> Out
getSize address state =
    case Map.lookup address $ connsS state of
    Nothing ->
        DoNothing

    Just conn ->
        SocketReceive address 2 (socketC conn)
 

getSenderId
    :: Tcp.SockAddr
    -> Map.Map Tcp.SockAddr Conn
    -> Maybe PublicKey
getSenderId address conns =
    case Map.lookup address conns of

    Nothing ->
        Nothing

    Just conn -> 
        case noise conn of

        NoiseState n ->
            Noise.remoteStaticKey n

        MakingEphemeral ->
            Nothing


parseFromClient :: B.ByteString -> Maybe FromClient
parseFromClient raw =
    case P.parseOnly fromClientP raw of
    Left _ ->
        Nothing

    Right ok ->
        Just ok


fromClientP :: P.Parser FromClient
fromClientP =
    do
    message <- fromClientHelpP
    P.endOfInput
    return message


fromClientHelpP :: P.Parser FromClient
fromClientHelpP =
    P.choice
    [ uploadKk2P
    , downloadNewKk1sToMeP
    ]


downloadNewKk1sToMeP :: P.Parser FromClient
downloadNewKk1sToMeP =
    do
    _ <- P.word8 1
    return DownloadNewKk1sToMeF


uploadKk2P :: P.Parser FromClient
uploadKk2P =
    do
    _ <- P.word8 0
    theirId <- publicKeyP
    sessionId <- sessionIdP
    kk2 <- kk2P
    return $ UploadKk2F theirId sessionId kk2


kk2P :: P.Parser Kk2
kk2P =
    Kk2 <$> P.take kk2Size


publicKeySize :: Int
publicKeySize =
    32


publicKeyP :: P.Parser PublicKey
publicKeyP =
    do
    raw <- P.take publicKeySize
    case Dh.dhBytesToPub $ Ba.convert raw of
        Nothing ->
            fail "couldn't parse public key"

        Just key ->
            return key


sessionIdSize :: Int
sessionIdSize =
    24


sessionIdP :: P.Parser SessionId
sessionIdP =
    SessionId <$> P.take sessionIdSize


kk2Size :: Int
kk2Size =
    48


data FromClient
    = UploadKk2F PublicKey SessionId Kk2
    | DownloadNewKk1sToMeF


type PublicKey
    = Dh.PublicKey Curve25519


newtype SessionId
    = SessionId B.ByteString


newtype Kk2
    = Kk2 B.ByteString


deleteConn :: Tcp.SockAddr -> State -> State
deleteConn address state =
    state { connsS = Map.delete address (connsS state) }


onNewKeys :: State -> NoiseKeys -> (State, Out)
onNewKeys state keys =
    case staticKeysS state of
    GotKeys staticKeys ->
        case getNeedsKeys $ connsS state of
        Nothing ->
            (state, Panic "unexpected new Noise keys")
        
        Just (address, conn) ->
            let
            newConn =
                conn { noise = NoiseState $ initNoise keys staticKeys }
            in
            ( state
                { connsS = Map.insert address newConn (connsS state) }
            , SocketReceive address 2 (socketC conn)
            )

    NoKeys ->
        trace "NoKeys" $
        ( state { staticKeysS = GotKeys keys }
        , Parallel
            [ WriteKeys $ encodeKeys keys
            , StartServer
            ]
        )


getNeedsKeys :: Map.Map Tcp.SockAddr Conn -> Maybe (Tcp.SockAddr, Conn)
getNeedsKeys conns =
    case Map.toList $ Map.filterWithKey needsKeys conns of
    [] ->
        Nothing

    first:_ ->
        Just first


needsKeys :: Tcp.SockAddr -> Conn -> Bool
needsKeys _ conn =
    case noise conn of

    NoiseState _ ->
        False

    MakingEphemeral ->
        True


initConn :: Tcp.Socket -> Conn
initConn socket =
    Conn
    { socketC = socket
    , readingSizeC = True
    , expectingC = Xk1E
    , noise = MakingEphemeral
    }


initNoise :: NoiseKeys -> NoiseKeys -> Noise.NoiseState AESGCM Curve25519 SHA256
initNoise ephemerals static =
    let
    options =
        Noise.setLocalStatic (Just static)
        . Noise.setLocalEphemeral (Just ephemerals)
        $ Noise.defaultHandshakeOpts Noise.ResponderRole ""
    in
    Noise.noiseState options noiseXK


data Conn =
    Conn
    { socketC :: Tcp.Socket
    , readingSizeC :: Bool
    , expectingC :: ConnExpecting
    , noise :: NoiseState
    }


data ConnExpecting
    = Xk1E
    | WithPayloadE


encodeKeys :: NoiseKeys -> B.ByteString
encodeKeys (secret, _) =
    Ba.convert $ Dh.dhSecToBytes secret


staticKeysPath :: String
staticKeysPath =
    "staticKeys"


data In
    = Start
    | KeysFile (Either IOError B.ByteString)
    | NewKeys NoiseKeys
    | NewSock (Tcp.Socket, Tcp.SockAddr)
    | FromSocket Tcp.SockAddr Int (Maybe B.ByteString)
    | SocketSendResult Tcp.SockAddr (Either IOError ())
    | SaveKk2Result Tcp.SockAddr (Either IOError ())
    | NewKk1For
        Tcp.SockAddr
        (B.ByteString, B.ByteString, B.ByteString)


data Out
    = ReadKeys
    | DoNothing
    | MakeKeys
    | Panic T.Text
    | StartServer
    | WriteKeys B.ByteString
    | SocketReceive Tcp.SockAddr Int Tcp.Socket
    | SocketSend Tcp.SockAddr Tcp.Socket B.ByteString
    | SaveKk2InDb
        Tcp.SockAddr
        (B.ByteString, B.ByteString, B.ByteString, B.ByteString)
    | Parallel [Out]
    | Sequence [Out]
    | FetchNewKk1sFor Tcp.SockAddr B.ByteString
    | SetupDb
    deriving Show


expectingSize :: Tcp.SockAddr -> State -> State
expectingSize address state =
    case Map.lookup address $ connsS state of
    Nothing ->
        state

    Just conn ->
        let
        newConn = conn { readingSizeC = True }
        in
        state { connsS = Map.insert address newConn (connsS state) }


initState :: State
initState =
    State
    { staticKeysS = NoKeys
    , connsS = Map.empty
    }


data KeysState
    = NoKeys
    | GotKeys NoiseKeys


type NoiseKeys = Dh.KeyPair Curve25519


data State =
    State
    { staticKeysS :: KeysState
    , connsS :: Map.Map Tcp.SockAddr Conn
    }

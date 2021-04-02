{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Foundation
import Foundation.IO
import Foundation.VFS.FilePath
import Foundation.VFS.Path
import Foundation.String
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
import qualified Crypto.PubKey.Ed25519 as Sig
import Data.Bits ((.&.), shiftR, shiftL)
import qualified Data.Attoparsec.ByteString as P
import Control.Monad.Fail (fail)
import Data.ByteString.Base64.URL (encodeBase64Unpadded')
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)


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
    TcpRecv address socket size ->
        do
        message <- E.try $ T.recv socket size
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
    , conns :: M.Map T.SockAddr (ConnWait, T.Socket, NoiseState)
    , gettingBalance :: M.Map T.SockAddr FromClient
    , awaitingBlob :: M.Map BlobId T.SockAddr
    }


data ConnWait
    = Xk1W
    | Xk3W
    | SizeW
    | BodyW


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
    | TcpRecv T.SockAddr T.Socket Int
    | TcpSend T.SockAddr T.Socket B.ByteString
    | DoNothing
    | DbQuery T.SockAddr Sql.Query B.ByteString
    | PutPaymentInDb Word32 Word32 B.ByteString


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

    Just (Xk1W, socket, noise) ->
        onXk1 bytes socket noise address state

    Just (Xk3W, socket, noise) ->
        onXk3 bytes socket noise address state

    Just (BodyW, socket, noise) ->
        onBody bytes socket noise address state


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


parseBody :: B.ByteString -> Maybe FromClient 
parseBody raw =
    case P.parseOnly bodyP raw of
    Left _ ->
        Nothing

    Right ok ->
        Just ok


bodyP :: P.Parser FromClient
bodyP =
    do
    body <- P.choice
        [ kk1P
        , kk2P
        , kkTransportP
        , blobP
        , paymentP
        , requestBlobP
        , addContactP
        , removeContactP
        ]
    P.endOfInput
    return body


kk1Size =
    48


kk1P :: P.Parser FromClient
kk1P =
    do
    _ <- P.word8 0
    recipient <- publicKeyP
    kk1 <- P.take kk1Size
    return $
        PaidF $
        Kk1P (Recipient recipient) (Kk1 $ Ba.convert kk1)


kk2Size =
    48


kk2P :: P.Parser FromClient
kk2P =
    do
    _ <- P.word8 1
    recipient <- publicKeyP
    kk2 <- P.take kk2Size
    sessionId <- sessionIdP
    return $
        PaidF $
        Kk2P
        (Recipient recipient)
        (Kk2 $ Ba.convert kk2)
        sessionId


transportSize =
    72


kkTransportP :: P.Parser FromClient
kkTransportP =
    do
    _ <- P.word8 2
    recipient <- publicKeyP
    transport <- P.take transportSize
    sessionId <- sessionIdP
    return
        $ PaidF
        $ KkTransportP
            (Recipient recipient)
            (KkTransport $ Ba.convert transport)
            sessionId


sessionIdSize =
    24


sessionIdP :: P.Parser SessionId
sessionIdP =
    do
    sessionId <- P.take sessionIdSize
    return $ SessionId $ Ba.convert sessionId


blobP :: P.Parser FromClient
blobP =
    do
    _ <- P.word8 3
    blobId <- blobIdP
    blob <- P.takeByteString
    return $ PaidF $ BlobP blobId (Blob $ Ba.convert blob)


paymentP :: P.Parser FromClient
paymentP =
    do
    _ <- P.word8 4
    amount <- word32P
    timestamp <- word32P
    payer <- publicKeyP
    return $
        FreeF $
        PaymentF
        (Amount amount)
        (Timestamp timestamp)
        (Payer payer)


word32P :: P.Parser Word32
word32P =
    do
    raw <- P.take 4
    return $
        (fromIntegral $ B.index raw 0) +
        (shiftL (fromIntegral $ B.index raw 1) 8) +
        (shiftL (fromIntegral $ B.index raw 2) 16) +
        (shiftL (fromIntegral $ B.index raw 3) 24)


blobIdSize =
    24


blobIdP :: P.Parser BlobId
blobIdP =
    do
    blobId <- P.take blobIdSize
    return $ BlobId $ Ba.convert blobId


requestBlobP :: P.Parser FromClient
requestBlobP =
    do
    _ <- P.word8 5
    blobId <- blobIdP
    return $ FreeF $ RequestBlobF blobId


addContactP :: P.Parser FromClient
addContactP =
    do
    _ <- P.word8 6
    contact <- publicKeyP
    return $ PaidF $ AddContactP $ Contact contact


dhlen =
    32


removeContactP :: P.Parser FromClient
removeContactP =
    do
    _ <- P.word8 7
    contact <- publicKeyP
    return $ PaidF $ RemoveContactP $ Contact contact


publicKeyP :: P.Parser PublicKey
publicKeyP =
    do
    raw <- P.take dhlen
    case Dh.dhBytesToPub (Ba.convert raw) of
        Nothing ->
            fail "couldn't parse public key"
        
        Just key ->
            return key 


onBody raw socket noise address state =
    let
    bad =
        ( state { conns = M.delete address (conns state) }
        , DoNothing
        )
    in
    (\f -> case N.readMessage (Ba.convert raw) noise of
    N.NoiseResultMessage plain noise' ->
        f plain noise') $ \plain noise' ->

    (\f -> case parseBody (Ba.convert plain) of
    Nothing ->
        bad

    Just message ->
        f message) $ \message ->

    (\f -> case N.remoteStaticKey noise' of
        Nothing ->
            (state, Panic "no remote static key in handshake")

        Just theirId ->
            f theirId) $ \theirId ->

    let
    state' =
        state
            { conns =
                M.insert address (SizeW, socket, noise') (conns state)
            , gettingBalance =
                M.insert address address message
            }
    in
    ( state'
    , Sequence
        [ DbQuery address getBalanceSql theirId
        , TcpRecv address socket 2
        ]
    )


getBalanceSql = "\
    \
        
        
blobPath :: BlobId -> FilePath
blobPath (BlobId blobId) =
    "blobs" </> blobNameB64 blobId


blobNameB64 :: UArray Word8 -> FileName
blobNameB64 =
    fromString .
    T.unpack .
    decodeUtf8 .
    Ba.convert .
    encodeBase64Unpadded' .
    Ba.convert


putPaymentInDb (Amount amount) (Timestamp time) (Payer theirId) =
    PutPaymentInDb amount time (Ba.convert $ Dh.dhPubToBytes theirId)


data FromClient
    = RequestBlobF BlobId
    | Kk1F Recipient Kk1
    | Kk2F Recipient Kk2 SessionId
    | KkTransportF Recipient KkTransport SessionId
    | BlobF BlobId Blob
    | AddContactF Contact
    | RemoveContactF Contact


paymentsKey :: PublicKey
paymentsKey =
    case Dh.dhBytesToPub $ Ba.convert $ B.pack paymentKeyBytes of
    Nothing ->
        error "couldn't construct hard-coded payments key"

    Just key ->
        key


paymentKeyBytes :: [Word8]
paymentKeyBytes =
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]


type Bytes
    = UArray Word8


newtype Payer
    = Payer PublicKey


newtype Recipient
    = Recipient PublicKey


newtype Kk1
    = Kk1 Bytes


newtype Kk2
    = Kk2 Bytes


newtype KkTransport
    = KkTransport Bytes


newtype SessionId
    = SessionId Bytes


newtype BlobId
    = BlobId Bytes
    deriving (Eq, Ord)


newtype Blob
    = Blob Bytes


newtype Amount
    = Amount Word32


newtype Timestamp
    = Timestamp Word32


newtype Contact
    = Contact PublicKey


type PublicKey
    = Dh.PublicKey Curve25519


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
                    (SizeW, socket, noise')
                    (conns state)
            }
        , Sequence
            [ fetchFirstDataBatch address noise'
            , TcpRecv address socket 2
            ]
        )

    N.NoiseResultMessage _ _ ->
        ( state { conns = M.delete address (conns state) }
        , DoNothing
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
    -> NoiseState
    -> T.SockAddr
    -> State
    -> (State, Output)
onXk1 raw socket noise address state =
    let
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
                        (Xk3W, socket, noise'')
                        (conns state)
                }
            , Sequence
                [ TcpSend address socket (Ba.convert xk2)
                , TcpRecv address socket xk3Size
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


xk3Size :: Int
xk3Size =
    64
            

noiseOptions :: KeyPair -> KeyPair -> N.HandshakeOpts Curve25519
noiseOptions myStatic myEphemeral =
    N.setLocalStatic (Just myStatic)
    . N.setLocalEphemeral (Just myEphemeral)
    $ N.defaultHandshakeOpts N.ResponderRole ""


keysForConn ephemeral state =
    (\f -> case staticKeys state of
        Nothing ->
            (state, Panic "got ephemeral keys before static keys")

        Just staticKeys ->
            f staticKeys) $ \staticKeys ->

    let
    options = noiseOptions staticKeys ephemeral
    noise :: NoiseState
    noise = N.noiseState options noiseXK
    in
    case connsAwaitingKeys state of
    [] ->
        (state, Panic "got unwanted keys")

    (address, conn):onns -> 
        ( state
            { connsAwaitingKeys = onns
            , conns =
                M.insert address (Xk1W, conn, noise) (conns state)
            }
        , TcpRecv address conn xk1Size
        )


encodeKeys :: KeyPair -> UArray Word8
encodeKeys (secret, _) =
    Ba.convert $ Dh.dhSecToBytes secret

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import Data.Bits ((.&.), shiftR)
import Debug.Trace (trace)
import qualified Data.Set as Set
import qualified System.Directory as Dir
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Attoparsec.ByteString as P
import qualified Control.Concurrent.STM.TQueue as Q
import qualified Control.Concurrent.STM as Stm
import qualified Control.Concurrent.STM.TVar as TVar
import qualified Data.ByteString as B
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Map as Map
import qualified Network.Simple.TCP as Tcp
import qualified Control.Exception as E
import qualified Crypto.Random as CryptoRand
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import Data.Word (Word8)
import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Database.SQLite.Simple as Db
import qualified Database.SQLite.Simple.ToField as DbTf
import qualified Text.Hex
import qualified Data.Text.IO as Tio


main :: IO ()
main = do
    model <- TVar.newTVarIO (InitS EmptyI)
    updateIo model StartM


updateIo :: TVar.TVar State -> Msg -> IO ()
updateIo mainState msg = do
    output <- Stm.atomically $ do
        model <- TVar.readTVar mainState
        let (output, newModel) = update model msg
        TVar.writeTVar mainState newModel
        return output
    io mainState output


-- Assocated data
authCodeA :: B.ByteString
authCodeA =
    B.pack [197, 154, 22, 2, 21, 159, 38, 105, 240, 15, 236, 142, 31, 124, 100, 71, 22, 117, 69, 163, 39, 221, 135, 100, 193, 244, 134, 63, 28, 226, 89, 31]


data Msg
    = StartM
    | TcpMsgInM Tcp.SockAddr B.ByteString
    | NewTcpConnM (Queue TcpInstruction) Tcp.SockAddr
    | DeadTcpM Tcp.SockAddr
    | BatchM [Maybe Msg]
    | RandomGenM CryptoRand.ChaChaDRG
    | AccessListM (Either E.IOException B.ByteString)
    | MessagesFromDbM Recipient [(B.ByteString, B.ByteString)]


instance Show Msg where
    show msg =
        case msg of
        StartM ->
            "StartM"

        TcpMsgInM address raw ->
            "TcpMsgIn (" <> show address <> ") (" <> show raw <> ")"

        NewTcpConnM _ address ->
            mconcat
            [ "NewTcpConnM (Queue TcpInstruction) ("
            , show address
            , ")"
            ]

        DeadTcpM address ->
            "DeadTcpM " <> show address

        BatchM msgs ->
            "BatchM " <> show msgs

        RandomGenM _ ->
            "RandomGenM CryptoRand.ChaChaDRG"

        AccessListM a ->
            "AccessListM " <> show a

        MessagesFromDbM recipient rows ->
            "MessagesFromDbM " <> show recipient <> " " <> show rows

data State
    = InitS Init
    | ReadyS Ready
    | FailedS
    deriving Show


data Init
    = EmptyI
    | ReadingAccessListI
    | GettingRandomI (Set.Set PublicKey)
    deriving Show


sendToClient :: Queue TcpInstruction -> ToClient -> Output
sendToClient q =
    MsgInQO q . Send . encodeToClient


encodeUint16 :: Int -> B.ByteString
encodeUint16 i =
    B.pack
    [ fromIntegral $ i .&. 0xFF
    , fromIntegral $ (shiftR i 8) .&. 0xFF
    ]


encodeToClient :: ToClient -> B.ByteString
encodeToClient msg =
    let
    raw =
        case msg of
        AuthCodeToSign code ->
            B.singleton 0 <> code

        NewMessage (Sender sender) (InboxMessage message) ->
            mconcat
            [ B.singleton 1
            , ByteArray.convert sender
            , message
            ]

        NoMessages ->
            B.singleton 2
    in
    encodeUint16 (B.length raw) <> raw


data ToClient
    = AuthCodeToSign B.ByteString
    | NewMessage Sender InboxMessage
    | NoMessages


pubToBytes :: Ed.PublicKey -> B.ByteString
pubToBytes =
    ByteArray.convert


data Ready
    = Ready
        { tcpConns :: Map.Map Tcp.SockAddr TcpConn
        , randomGen :: CryptoRand.ChaChaDRG
        , accessList :: Set.Set PublicKey
        }


instance Show Ready where
    show (Ready conns _ accessList) =
        mconcat
        [ "Ready {\n"
        , "    tcpConns = "
        , show conns
        , "\n    accessList = "
        , show accessList
        , "\n}"
        ]


data Output
    = DoNothingO
    | PrintO T.Text
    | ReadAccessListO
    | StartTcpServerO
    | BatchO [Output]
    | MakeDirIfNotThereO FilePath
    | MsgInQO (Queue TcpInstruction) TcpInstruction
    | DeleteInboxMessageDbO Sender Recipient InboxMessage
    | GetRandomGenO
    | SaveMessageToDbO Sender Recipient InboxMessage
    | GetMessageFromDbO Recipient


instance Show Output where
    show output =
        case output of
        GetMessageFromDbO recipient ->
            "GetMessageFromDbO " <> show recipient

        GetRandomGenO ->
            "GetRandomGenO"

        SaveMessageToDbO sender recipient msg ->
            mconcat
            [ "SaveMessageToDbO ("
            , show sender
            , ") ("
            , show recipient
            , ") ("
            , show msg
            , ")"
            ]

        DoNothingO ->
            "DoNothingO"

        PrintO txt ->
            "PrintO " <> T.unpack txt

        ReadAccessListO ->
            "ReadAccessListO"

        StartTcpServerO ->
            "StartTcpServerO"

        BatchO outputs ->
            "BatchO " <> mconcat (map show outputs)

        MakeDirIfNotThereO path ->
            "MakeDirIfNotThereO " <> path

        MsgInQO _ instruction ->
            "MsgInQO " <> show instruction

        DeleteInboxMessageDbO sender recipient msg ->
            mconcat
            [ "DeleteInboxMessageDbO ("
            , show sender
            , ") ("
            , show recipient
            , ") ("
            , show msg
            , ")"
            ]


rootPath :: FilePath
rootPath =
    "bwtdata"


dbPath :: FilePath
dbPath =
    rootPath </> "database.sqlite"


findConn
    :: Recipient
    -> Map.Map Tcp.SockAddr TcpConn
    -> Maybe (Tcp.SockAddr, TcpConn)
findConn recipient conns =
    let
    filtered = Map.filter (findConnHelp recipient) conns
    in
    case Map.toList filtered of
    [] ->
        Nothing

    found:_ ->
        Just found


findConnHelp :: Recipient -> TcpConn -> Bool
findConnHelp (Recipient r) (TcpConn _ auth) =
    case auth of
    Authenticated (Sender s) ->
        s == r

    Untrusted _ ->
        False


updateReady :: State -> (Ready -> (Output, State)) -> (Output, State)
updateReady state f =
    case state of
    InitS _ ->
        (DoNothingO, state)

    ReadyS ready ->
        f ready

    FailedS ->
        (DoNothingO, state)


update :: State -> Msg -> (Output, State)
update model msg =
    let
    pass = (DoNothingO, model)
    dbg =
        mconcat
        [ "model: "
        , show model
        , "\nmsg: "
        , show msg
        , "\n"
        ]
    in
    trace dbg $
    case msg of
    AccessListM eitherRaw ->
        case model of
        InitS EmptyI ->
            pass

        InitS (GettingRandomI _) ->
            pass

        ReadyS _ ->
            pass

        FailedS ->
            pass

        InitS ReadingAccessListI ->
            case eitherRaw of
            Left err ->
                ( PrintO $
                  mconcat
                  [ "could not read access list file:\n"
                  , T.pack $ show err
                  ]
                , FailedS
                )

            Right raw ->
                case P.parseOnly accessListP raw of
                Left err ->
                    ( PrintO $
                      mconcat
                      [ "could not parse access list file:\n"
                      , T.pack err
                      , ":\n"
                      , Text.Hex.encodeHex raw
                      ]
                    , FailedS
                    )

                Right accessList ->
                    (GetRandomGenO, InitS $ GettingRandomI accessList)

    MessagesFromDbM recipient raw ->
        updateReady model $ \ready ->
        case findConn recipient $ tcpConns ready of
        Nothing ->
            pass

        Just (_, TcpConn _ (Untrusted _)) ->
            pass

        Just (address, TcpConn q (Authenticated _)) ->
            case raw of
            [] ->
                ( BatchO
                    [ sendToClient q NoMessages
                    , MsgInQO q Die
                    ]
                , ReadyS $
                  ready
                    { tcpConns =
                        Map.delete address $ tcpConns ready
                    }
                )

            (rawSender, rawMessage):_ ->
                case Ed.publicKey rawSender of
                CryptoFailed err ->
                    ( PrintO $
                      mconcat
                      [ "could not parse public key from server: "
                      , Text.Hex.encodeHex rawSender
                      , ":\n"
                      , T.pack $ show err
                      ]
                    , FailedS
                    )
                CryptoPassed sender ->
                    ( BatchO
                        [ sendToClient q $
                          NewMessage
                          (Sender sender)
                          (InboxMessage rawMessage)
                        , MsgInQO q Die
                        , DeleteInboxMessageDbO
                            (Sender sender)
                            recipient
                            (InboxMessage rawMessage)
                        ]
                    , ReadyS $ ready
                        { tcpConns =
                            Map.delete address $ tcpConns ready
                        }
                    )


    RandomGenM randomGen ->
        case model of
        InitS (GettingRandomI accessList) ->
            ( StartTcpServerO
            , ReadyS $
                Ready
                    { tcpConns = Map.empty
                    , randomGen
                    , accessList
                    }
            )

        InitS ReadingAccessListI ->
            pass

        InitS EmptyI ->
            pass

        ReadyS _ ->
            pass

        FailedS ->
            pass

    DeadTcpM address ->
        case model of
        InitS _ ->
            pass

        FailedS ->
            pass

        ReadyS ready ->
            case Map.lookup address (tcpConns ready) of
            Nothing ->
                pass

            Just (TcpConn q _) ->
                ( MsgInQO q Die
                , ReadyS $
                    ready
                        { tcpConns =
                            Map.delete address (tcpConns ready)
                        }
                )

    StartM ->
        (ReadAccessListO, InitS ReadingAccessListI)

    TcpMsgInM address rawMessage ->
        updateOnRawTcpMessage address rawMessage model

    BatchM msgs ->
        let
        (outputs, newModel) = batchUpdate model msgs []
        in
        (BatchO outputs, newModel)

    NewTcpConnM q address ->
        case model of
        InitS _ ->
            pass

        FailedS ->
            pass

        ReadyS ready ->
            let
            (auth, gen) =
                CryptoRand.randomBytesGenerate
                authLength
                (randomGen ready)
            newConns =
                Map.insert
                    address
                    (TcpConn q (Untrusted (AuthCode auth)))
                    (tcpConns ready)
            in
            ( sendToClient q $ AuthCodeToSign auth
            , ReadyS $ ready {tcpConns = newConns, randomGen = gen}
            )


accessListP :: P.Parser (Set.Set PublicKey)
accessListP = do
    asList <- fmap (: []) oneAccessKeyP
    P.endOfInput
    return $ Set.fromList asList


instance Ord PublicKey where
    compare (PublicKey a) (PublicKey b) =
        let
        a' :: B.ByteString
        a' = ByteArray.convert a
        b' :: B.ByteString
        b' = ByteArray.convert b
        in
        compare a' b'


newtype PublicKey
    = PublicKey Ed.PublicKey
    deriving (Eq, Show)


oneAccessKeyP :: P.Parser PublicKey
oneAccessKeyP = do
    b64 <- P.take 43 -- unpadded Base64 32-byte public key
    _ <- P.word8 10 -- '\n'

    case B64.decodeUnpadded b64 of
        Left err ->
            fail $
                T.unpack $
                mconcat
                [ "could not decode Base64 access list item:\n"
                , T.pack err
                , Text.Hex.encodeHex b64
                ]

        Right bs ->
            case Ed.publicKey bs of
                CryptoFailed err ->
                    fail $
                        T.unpack $
                        mconcat
                        [ "could not convert bytestring to public "
                        , "signing key:\n"
                        , T.pack $ show err
                        , ":\n"
                        , Text.Hex.encodeHex bs
                        ]

                CryptoPassed key ->
                    return $ PublicKey key


authLength :: Int
authLength =
    32


batchUpdate :: State -> [Maybe Msg] -> [Output] -> ([Output], State)
batchUpdate model msgs outputs =
    case msgs of
    [] ->
        (outputs, model)

    Nothing : sgs ->
        batchUpdate model sgs outputs

    Just m : sgs ->
        let
        (output, newModel) = update model m
        in
        batchUpdate newModel sgs (output : outputs)


updateOnRawTcpMessage
    :: Tcp.SockAddr
    -> B.ByteString
    -> State
    -> (Output, State)
updateOnRawTcpMessage address rawMessage model =
    case model of
    InitS _ ->
        (DoNothingO, model)

    FailedS ->
        (DoNothingO, model)

    ReadyS ready ->
        case Map.lookup address (tcpConns ready) of
        Nothing ->
            (DoNothingO, model)

        Just (TcpConn q _) ->
            case P.parseOnly fromClientP rawMessage of
            Left _ ->
                let
                newConns = Map.delete address (tcpConns ready)
                in
                ( MsgInQO q Die
                , ReadyS $ ready { tcpConns = newConns}
                )

            Right untrusted ->
                updateOnTcpMessage address ready untrusted


updateOnTcpMessage
    :: Tcp.SockAddr
    -> Ready
    -> FromClient
    -> (Output, State)
updateOnTcpMessage address ready untrusted =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case Map.lookup address $ tcpConns ready of
    Nothing ->
        (DoNothingO, ReadyS ready)

    Just (TcpConn q (Authenticated sender)) ->
        case untrusted of
        SignedAuthCodeF _ _ ->
            (MsgInQO q Die, ReadyS ready)

        SendMessage recipient inboxMessage ->
            ( BatchO
                [ SaveMessageToDbO sender recipient inboxMessage
                , MsgInQO q Die
                ]
            , ReadyS ready
            )

        GetMessage ->
            ( GetMessageFromDbO $ senderToRecipient sender
            , ReadyS ready
            )


    Just (TcpConn q (Untrusted authCode)) ->
        case untrusted of
        SignedAuthCodeF sender signed ->
            if validAuthSig authCode signed sender then
            ( DoNothingO
            , ReadyS $
                ready
                    { tcpConns =
                        Map.insert
                            address
                            (TcpConn q (Authenticated sender))
                            (tcpConns ready)
                    }
            )
            else
            pass

        SendMessage _ _ ->
            pass

        GetMessage ->
            pass


validAuthSig :: AuthCode -> Ed.Signature -> Sender -> Bool
validAuthSig (AuthCode authCode) signature (Sender sender) =

    Ed.verify sender (authCodeA <> authCode) signature


senderToRecipient :: Sender -> Recipient
senderToRecipient (Sender s) =
    Recipient s


data TcpConn
    = TcpConn (Queue TcpInstruction) AuthStatus


instance Show TcpConn where
    show (TcpConn _ authStatus) =
        "TcpConn (Queue TcpInstruction) " <> show authStatus


data AuthStatus
    = Authenticated Sender
    | Untrusted AuthCode
    deriving Show


newtype AuthCode
    = AuthCode B.ByteString
    deriving Show


data FromClient
    = SignedAuthCodeF Sender Ed.Signature
    | SendMessage Recipient InboxMessage
    | GetMessage
    deriving Show


fromClientP :: P.Parser FromClient
fromClientP = do
    msg <- P.choice
        [ do
            _ <- P.word8 0
            sender <- senderP
            SignedAuthCodeF sender <$> signatureP
        , do
            _ <- P.word8 1
            recipient <- recipientP
            SendMessage recipient <$> inboxMessageP
        , do
            _ <- P.word8 2
            return GetMessage
        ]
    P.endOfInput
    return msg


recipientP :: P.Parser Recipient
recipientP = do
    Recipient <$> signingKeyP


senderP :: P.Parser Sender
senderP = do
    Sender <$> signingKeyP


signingKeyP :: P.Parser Ed.PublicKey
signingKeyP = do
    raw <- P.take Ed.publicKeySize
    case Ed.publicKey raw of
        CryptoPassed key ->
            return key

        CryptoFailed err ->
            fail $ show err


signatureP :: P.Parser Ed.Signature
signatureP = do
    raw <- P.take Ed.signatureSize
    case Ed.signature raw of
        CryptoPassed signature ->
            return signature

        CryptoFailed err ->
            fail $ show err


inboxMessageP :: P.Parser InboxMessage
inboxMessageP = do
    _ <- P.word8 1
    raw <- P.scan 0 msgScanner
    return $ InboxMessage raw


msgScanner :: Int -> Word8 -> Maybe Int
msgScanner counter _ =
    if counter < 100 then
    Just $ counter + 1
    else
    Nothing


newtype InboxMessage
    = InboxMessage B.ByteString
    deriving Show


newtype Recipient
    = Recipient Ed.PublicKey
    deriving Show


accessListPath =
    rootPath </> "accessList.txt"


io :: TVar.TVar State -> Output -> IO ()
io mainState output = do
  case output of
    PrintO msg ->
        Tio.putStrLn msg

    DoNothingO -> do
        return ()

    ReadAccessListO -> do
        result <- E.try $ B.readFile accessListPath
        updateIo mainState $ AccessListM result

    StartTcpServerO -> do
        tcpServer mainState

    BatchO outputs -> do
        mapM_ (io mainState) outputs

    MsgInQO q msg -> do
        writeQ q msg

    MakeDirIfNotThereO path -> do
        Dir.createDirectoryIfMissing True path

    DeleteInboxMessageDbO sender recipient message -> do
        Db.withConnection dbPath $ \conn ->
            Db.execute
                conn
                deleteMessageSql
                (sender, recipient, message)

    GetRandomGenO -> do
        drg <- CryptoRand.drgNew
        updateIo mainState $ RandomGenM drg

    SaveMessageToDbO sender recipient inboxMessage -> do
        Db.withConnection dbPath $ \conn ->
            Db.execute
                conn
                saveMessageSql
                (sender, recipient, inboxMessage)

    GetMessageFromDbO recipient -> do
        result <- Db.withConnection dbPath $ \conn ->
            Db.query
                conn
                getMessageSql
                (Db.Only recipient)
        updateIo mainState $ MessagesFromDbM recipient result


saveMessageSql :: Db.Query
saveMessageSql =
    "INSERT INTO messages (sender, recipient, message) \
    \VALUES (?, ?, ?);"


getMessageSql :: Db.Query
getMessageSql =
    "SELECT (sender, message) FROM messages WHERE recipient=?;"


newtype Sender
    = Sender Ed.PublicKey
    deriving (Eq, Show)


instance Ord Sender where
    compare (Sender a) (Sender b) =
        let
        a' :: B.ByteString
        a' = ByteArray.convert a
        b' :: B.ByteString
        b' = ByteArray.convert b
        in
        compare a' b'


instance DbTf.ToField Sender where
    toField (Sender s) =
        DbTf.toField $ pubToBytes s


instance DbTf.ToField Recipient where
    toField (Recipient r) =
        DbTf.toField $ pubToBytes r


instance DbTf.ToField InboxMessage where
    toField (InboxMessage i) =
        DbTf.toField i


deleteMessageSql :: Db.Query
deleteMessageSql =
    "DELETE FROM messages \
    \WHERE sender = ? AND recipient = ? and message = ?;"


tcpServer :: TVar.TVar State -> IO ()
tcpServer mainState =
    Tcp.serve (Tcp.Host "127.0.0.1") "11453" (tcpServerHelp mainState)


readQ :: Stm.STM (Q.TQueue a) -> IO a
readQ q =
    Stm.atomically $ do
        q_ <- q
        Q.readTQueue q_


writeQ :: Stm.STM (Q.TQueue a) -> a -> IO ()
writeQ q value =
    Stm.atomically $ do
        q_ <- q
        Q.writeTQueue q_ value


type Queue a = Stm.STM (Stm.TQueue a)


tcpServerHelp :: TVar.TVar State -> (Tcp.Socket, Tcp.SockAddr) -> IO ()
tcpServerHelp mainState (socket, address) = do
    let instructionsQ = Q.newTQueue :: Queue TcpInstruction
    updateIo mainState $ NewTcpConnM instructionsQ address
    tcpReceiver mainState socket address
    tcpSender mainState address instructionsQ socket


data TcpInstruction
    = Die
    | Send B.ByteString
    deriving Show


tcpSend :: Tcp.Socket -> B.ByteString -> IO (Either E.IOException ())
tcpSend socket msg =
    E.try $ Tcp.send socket msg


tcpSender
    :: TVar.TVar State
    -> Tcp.SockAddr
    -> Queue TcpInstruction
    -> Tcp.Socket
    -> IO ()
tcpSender mainState address instructionsQ socket = do
    instruction <- readQ instructionsQ
    case instruction of
        Die ->
            return ()

        Send msg -> do
            eitherOk <- tcpSend socket msg
            case eitherOk of
                Left _ -> do
                    Tio.putStrLn "0"
                    updateIo mainState $ DeadTcpM address

                Right () ->
                    tcpSender mainState address instructionsQ socket


tcpRecv
    :: Tcp.Socket
    -> Int
    -> IO (Either E.IOException (Maybe B.ByteString))
tcpRecv socket len =
    E.try $ Tcp.recv socket len


uint16P :: P.Parser Int
uint16P = do
    b0 <- uint8P
    b1 <- uint8P
    return $ b0 + b1 * 256


tcpReceiver :: TVar.TVar State -> Tcp.Socket -> Tcp.SockAddr -> IO ()
tcpReceiver mainState socket address = do
    eitherMsgLen <- tcpRecv socket 2
    case eitherMsgLen of
        Left err -> do
            Tio.putStrLn "1"
            Tio.putStrLn $ "tcpRecv error: " <> T.pack (show err)
            updateIo mainState $ DeadTcpM address

        Right Nothing -> do
            Tio.putStrLn "2"
            updateIo mainState $ DeadTcpM address

        Right (Just rawLen) ->
            case P.eitherResult $ P.parse uint16P rawLen of
            Left _ -> do
                Tio.putStrLn "3"
                updateIo mainState $ DeadTcpM address

            Right len -> do
                if len < maxMessageLength then do
                    eitherMsg <- tcpRecv socket len
                    case eitherMsg of
                        Left _ -> do
                            Tio.putStrLn "4"
                            updateIo mainState $ DeadTcpM address

                        Right Nothing -> do
                            Tio.putStrLn "5"
                            updateIo mainState $ DeadTcpM address

                        Right (Just msg) -> do
                            updateIo mainState $ TcpMsgInM address msg
                            tcpReceiver mainState socket address

                else do
                    Tio.putStrLn "6"
                    updateIo mainState $ DeadTcpM address


uint8P :: P.Parser Int
uint8P =
    fromIntegral <$> P.anyWord8


maxMessageLength :: Int
maxMessageLength =
    16000

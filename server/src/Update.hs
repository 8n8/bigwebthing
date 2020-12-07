{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Update
    ( update
    , Ready(..)
    , Msg(..)
    , State(..)
    , Output(..)
    , Init(..)
    , PublicKey(..)
    , TcpConn(..)
    , TcpGetting(..)
    , AuthStatus(..)
    , Sender(..)
    , AuthCode(..)
    , Recipient(..)
    , authLength
    ) where


import Debug.Trace (trace)
import qualified Data.Map as Map
import Data.Bits ((.&.), shiftR)
import qualified Data.Set as Set
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Attoparsec.ByteString as P
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Network.Simple.TCP as Tcp
import qualified Control.Exception as E
import qualified Crypto.Random as CryptoRand
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import Data.Word (Word8)
import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Database.SQLite.Simple.ToField as DbTf
import qualified Database.SQLite.Simple as Db
import qualified Text.Hex


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


pubToBytes :: Ed.PublicKey -> B.ByteString
pubToBytes =
    ByteArray.convert


findConnHelp :: Ed.PublicKey -> TcpConn -> Bool
findConnHelp initiator (TcpConn _ auth _) =
    case auth of
    Authenticated (Sender s) ->
        s == initiator

    Untrusted _ ->
        False


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


senderToPub :: Sender -> PublicKey
senderToPub (Sender s) =
    PublicKey s


validAuthSig :: AuthCode -> Ed.Signature -> Sender -> Bool
validAuthSig (AuthCode authCode) signature (Sender sender) =

    Ed.verify sender (authCodeA <> authCode) signature


senderToRecipient :: Sender -> Recipient
senderToRecipient (Sender s) =
    Recipient s


instance Show TcpConn where
    show (TcpConn _ authStatus g) =
        "TcpConn (Queue TcpInstruction) " <> show authStatus <> show g


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
    raw <- P.scan 0 msgScanner
    return $ InboxMessage raw


msgScanner :: Int -> Word8 -> Maybe Int
msgScanner counter _ =
    if counter < 100 then
    Just $ counter + 1
    else
    Nothing


-- Assocated data
authCodeA :: B.ByteString
authCodeA =
    B.pack [197, 154, 22, 2, 21, 159, 38, 105, 240, 15, 236, 142, 31, 124, 100, 71, 22, 117, 69, 163, 39, 221, 135, 100, 193, 244, 134, 63, 28, 226, 89, 31]


data Output
    = DoNothingO
    | SetupDbO
    | TcpRecvO Tcp.SockAddr Tcp.Socket Int
    | CloseSocketO Tcp.Socket
    | MsgInSocketO Tcp.SockAddr Tcp.Socket B.ByteString
    | PrintO T.Text
    | ReadAccessListO
    | StartTcpServerO
    | BatchO [Output]
    | DeleteInboxMessageDbO Sender Recipient InboxMessage
    | GetRandomGenO
    | SaveMessageToDbO Sender Recipient InboxMessage
    | GetMessageFromDbO Recipient
    deriving (Show, Eq)


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


newtype InboxMessage
    = InboxMessage B.ByteString
    deriving (Show, Eq)


newtype Recipient
    = Recipient Ed.PublicKey
    deriving (Show, Eq)


newtype PublicKey
    = PublicKey Ed.PublicKey
    deriving (Eq, Show)


data AuthStatus
    = Authenticated Sender
    | Untrusted AuthCode
    deriving (Show, Eq)


newtype AuthCode
    = AuthCode B.ByteString
    deriving (Show, Eq)


data TcpConn
    = TcpConn Tcp.Socket AuthStatus TcpGetting
    deriving Eq


data TcpGetting
    = LengthG
    | BodyG
    deriving (Show, Eq)


data State
    = InitS Init
    | ReadyS Ready
    | FailedS
    deriving (Show, Eq)


updateReady :: State -> (Ready -> (Output, State)) -> (Output, State)
updateReady state f =
    case state of
    InitS _ ->
        (DoNothingO, state)

    ReadyS ready ->
        f ready

    FailedS ->
        (DoNothingO, state)


accessListP :: P.Parser (Set.Set PublicKey)
accessListP = do
    asList <- P.many1 oneAccessKeyP
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


findConn
    :: Ed.PublicKey
    -> Map.Map Tcp.SockAddr TcpConn
    -> Maybe (Tcp.SockAddr, TcpConn)
findConn initiator conns =
    let
    filtered = Map.filter (findConnHelp initiator) conns
    in
    case Map.toList filtered of
    [] ->
        Nothing

    found:_ ->
        Just found


data FromClient
    = SignedAuthCodeF Sender Ed.Signature
    | SendMessage Recipient InboxMessage
    | GetMessage
    deriving Show


uint16P :: P.Parser Int
uint16P = do
    b0 <- uint8P
    b1 <- uint8P
    return $ b0 + b1 * 256


tcpLengthP :: P.Parser Int
tcpLengthP = do
    len <- uint16P
    P.endOfInput
    return len


updateOnRawTcpMessage
    :: Tcp.SockAddr
    -> Either E.IOException (Maybe B.ByteString)
    -> State
    -> (Output, State)
updateOnRawTcpMessage address eitherRaw model =
    case model of
    InitS _ ->
        (DoNothingO, model)

    FailedS ->
        (DoNothingO, model)

    ReadyS ready ->
        case Map.lookup address (tcpConns ready) of
        Nothing ->
            (DoNothingO, model)

        Just (TcpConn socket auth getting) ->
            let
            onBad =
                let
                newConns = Map.delete address (tcpConns ready)
                in
                ( CloseSocketO socket
                , ReadyS $ ready { tcpConns = newConns}
                )
            in
            case eitherRaw of
            Left _ ->
                onBad

            Right Nothing ->
                onBad

            Right (Just raw) ->
                case getting of
                BodyG ->
                    case P.parseOnly fromClientP raw of
                    Left _ ->
                        onBad

                    Right untrusted ->
                        updateOnTcpMessage address ready untrusted

                LengthG ->
                    case P.parseOnly tcpLengthP raw of
                    Left _ ->
                        onBad

                    Right len ->
                        let
                        newConns =
                            Map.insert
                            address
                            (TcpConn socket auth BodyG)
                            (tcpConns ready)
                        in
                        if len > maxMessageLength then
                        onBad
                        else
                        ( TcpRecvO address socket len
                        , ReadyS $ ready { tcpConns = newConns }
                        )


maxMessageLength :: Int
maxMessageLength =
    200


uint8P :: P.Parser Int
uint8P =
    fromIntegral <$> P.anyWord8


-- senderEqRecipient :: Sender -> Recipient -> Bool
-- senderEqRecipient (Sender s) (Recipient r) =
--     s == r


updateOnTcpMessage
    :: Tcp.SockAddr
    -> Ready
    -> FromClient
    -> (Output, State)
updateOnTcpMessage address ready untrusted =
    case Map.lookup address $ tcpConns ready of
    Nothing ->
        (DoNothingO, ReadyS ready)

    Just (TcpConn socket (Authenticated sender) _) ->
        case untrusted of
        SignedAuthCodeF _ _ ->
            (CloseSocketO socket, ReadyS ready)

        SendMessage recipient inboxMessage ->
            -- if senderEqRecipient sender recipient then
            -- (DoNothingO, ReadyS ready)
            -- else
            ( BatchO
                [ SaveMessageToDbO sender recipient inboxMessage
                , CloseSocketO socket
                ]
            , ReadyS $ ready
                { tcpConns = Map.delete address $ tcpConns ready }
            )

        GetMessage ->
            ( GetMessageFromDbO $ senderToRecipient sender
            , ReadyS ready
            )

    Just (TcpConn socket (Untrusted authCode) _) ->
        let
        closeSocket = (CloseSocketO socket, ReadyS ready)
        in
        case untrusted of
        SignedAuthCodeF sender signed ->
            let
            validSig = validAuthSig authCode signed sender
            allowed =
                Set.member (senderToPub sender) (accessList ready)
            in
            if validSig && allowed then
            ( TcpRecvO address socket 2
            , ReadyS $
                ready
                    { tcpConns =
                        Map.insert
                            address
                            (TcpConn
                                socket
                                (Authenticated sender)
                                LengthG)
                            (tcpConns ready)
                    }
            )
            else
            closeSocket

        SendMessage _ _ ->
            closeSocket

        GetMessage ->
            closeSocket


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


authLength :: Int
authLength =
    32


sendToClient :: Tcp.SockAddr -> Tcp.Socket -> ToClient -> Output
sendToClient address socket =
    MsgInSocketO address socket . encodeToClient


data ToClient
    = AuthCodeToSign B.ByteString
    | NewMessage Sender InboxMessage
    | NoMessages


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


data Init
    = EmptyI
    | ReadingAccessListI
    | GettingRandomI (Set.Set PublicKey)
    deriving (Show, Eq)


data Msg
    = StartM
    | TcpMsgInM
        Tcp.SockAddr
        (Either E.IOException (Maybe B.ByteString))
    | TcpSendResultM Tcp.SockAddr (Either E.IOException ())
    | NewTcpConnM Tcp.Socket Tcp.SockAddr
    | DeadTcpM Tcp.SockAddr
    | BatchM [Maybe Msg]
    | RandomGenM CryptoRand.ChaChaDRG
    | AccessListM (Either E.IOException B.ByteString)
    | MessagesFromDbM
        Recipient
        (Either Db.SQLError [(B.ByteString, B.ByteString)])
    | DbErrM Ed.PublicKey (Either Db.SQLError ())


instance Show Msg where
    show msg =
        case msg of
        DbErrM id_ err ->
            "DbErrM " <> show id_ <> " " <> show err

        StartM ->
            "StartM"

        TcpSendResultM address eitherErr ->
            mconcat
            [ "TcpSendResultM ("
            , show address
            , ") ("
            , show eitherErr
            , ")"
            ]

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


data Ready
    = Ready
        { tcpConns :: Map.Map Tcp.SockAddr TcpConn
        , randomGen :: CryptoRand.ChaChaDRG
        , accessList :: Set.Set PublicKey
        }


instance Eq Ready where
    (==) (Ready t1 _ a1) (Ready t2 _ a2) =
        t1 == t2 && a1 == a2


updateOnAccessList
    :: State
    -> Either E.IOException B.ByteString
    -> (Output, State)
updateOnAccessList model eitherRaw =
    let
    pass = (DoNothingO, model)
    in
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


updateOnTcpSend
    :: State
    -> Tcp.SockAddr
    -> Either E.IOException ()
    -> (Output, State)
updateOnTcpSend model address eitherError =
    updateReady model $ \ready ->
    case eitherError of
    Right () ->
        (DoNothingO, model)

    Left _ ->
        case Map.lookup address $ tcpConns ready of
        Nothing ->
            (DoNothingO, model)

        Just (TcpConn socket _ _) ->
            ( CloseSocketO socket
            , ReadyS $
                ready
                    { tcpConns =
                        Map.delete address $ tcpConns ready
                    }
            )


updateOnDbErr
    :: State
    -> Ed.PublicKey
    -> Either Db.SQLError ()
    -> (Output, State)
updateOnDbErr model initiator eitherErr =
    let
    pass = (DoNothingO, model)
    in
    updateReady model $ \ready ->
    case eitherErr of
    Right () ->
        pass

    Left _ ->
        case findConn initiator $ tcpConns ready of
        Nothing ->
            pass

        Just (_, TcpConn _ (Untrusted _) _) ->
            pass

        Just (address, TcpConn socket (Authenticated _) _) ->
            ( BatchO
                [ sendToClient address socket NoMessages
                , CloseSocketO socket
                ]
            , ReadyS $
              ready
                { tcpConns =
                    Map.delete address $ tcpConns ready
                }
            )


updateOnMessagesFromDb
    :: State
    -> Recipient
    -> Either Db.SQLError [(B.ByteString, B.ByteString)]
    -> (Output, State)
updateOnMessagesFromDb
    model
    recipient@(Recipient initiator)
    eitherRaw =

    let
    pass = (DoNothingO, model)
    in
    updateReady model $ \ready ->
    case findConn initiator $ tcpConns ready of
    Nothing ->
        pass

    Just (_, TcpConn _ (Untrusted _) _) ->
        pass

    Just (address, TcpConn socket (Authenticated _) _) ->
        let
        noMessages =
            ( BatchO
                [ sendToClient address socket NoMessages
                , CloseSocketO socket
                ]
            , ReadyS $
              ready
                { tcpConns =
                    Map.delete address $ tcpConns ready
                }
            )
        in
        case eitherRaw of
        Left _ ->
            noMessages

        Right [] ->
            noMessages

        Right ((rawSender, rawMessage):_) ->
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
                    [ sendToClient address socket $
                      NewMessage
                      (Sender sender)
                      (InboxMessage rawMessage)
                    , CloseSocketO socket
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


updateOnRandomGen :: State -> CryptoRand.ChaChaDRG -> (Output, State)
updateOnRandomGen model gen =
    let
    pass = (DoNothingO, model)
    in
    case model of
    InitS (GettingRandomI accessList) ->
        ( StartTcpServerO
        , ReadyS $
            Ready
                { tcpConns = Map.empty
                , randomGen = gen
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


updateOnDeadTcp :: State -> Tcp.SockAddr -> (Output, State)
updateOnDeadTcp model address =
    let
    pass = (DoNothingO, model)
    in
    case model of
    InitS _ ->
        pass

    FailedS ->
        pass

    ReadyS ready ->
        case Map.lookup address (tcpConns ready) of
        Nothing ->
            pass

        Just (TcpConn socket _ _) ->
            ( CloseSocketO socket
            , ReadyS $
                ready
                    { tcpConns =
                        Map.delete address (tcpConns ready)
                    }
            )


updateOnNewTcpConn
    :: State
    -> Tcp.Socket
    -> Tcp.SockAddr
    -> (Output, State)
updateOnNewTcpConn model socket address =
    let
    pass = (DoNothingO, model)
    in
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
                    (TcpConn
                        socket (Untrusted (AuthCode auth)) LengthG)
                    (tcpConns ready)
            in
            ( BatchO
                [ sendToClient address socket $ AuthCodeToSign auth
                , TcpRecvO address socket 2
                ]
            , ReadyS $ ready {tcpConns = newConns, randomGen = gen}
            )


update :: State -> Msg -> (Output, State)
update model msg =
    trace ("msg: " <> show msg <> "\n") $
    trace ("model: " <> show model) $
    case msg of
    DbErrM id_ err ->
        updateOnDbErr model id_ err

    AccessListM eitherRaw ->
        updateOnAccessList model eitherRaw

    TcpSendResultM address eitherError ->
        updateOnTcpSend model address eitherError

    MessagesFromDbM recipient raw ->
        updateOnMessagesFromDb model recipient raw

    RandomGenM gen ->
        updateOnRandomGen model gen

    DeadTcpM address ->
        updateOnDeadTcp model address

    StartM ->
        ( BatchO [SetupDbO, ReadAccessListO ]
        , InitS ReadingAccessListI
        )

    TcpMsgInM address rawMessage ->
        updateOnRawTcpMessage address rawMessage model

    BatchM msgs ->
        let
        (outputs, newModel) = batchUpdate model msgs []
        in
        (BatchO outputs, newModel)

    NewTcpConnM socket address ->
        updateOnNewTcpConn model socket address

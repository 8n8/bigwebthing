{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified System.Directory as Dir
import System.IO.Error (isDoesNotExistError)
import qualified Data.ByteArray as ByteArray
import qualified Data.Attoparsec.ByteString as P
import qualified Control.Concurrent.STM.TQueue as Q
import qualified Control.Concurrent.STM as Stm
import qualified Data.ByteString as B
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import qualified Data.Map as Map
import qualified Network.Simple.TCP as Tcp
import qualified Control.Concurrent as CC
import qualified Control.Exception as E
import qualified Crypto.Random as CryptoRand
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Data.Time.Clock as Clock
import qualified Data.Bits as Bits
import Data.Word (Word8)
import qualified Text.Hex as Hex
import qualified Crypto.Hash as Hash
import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Database.SQLite.Simple as Db
import qualified Database.SQLite.Simple.ToField as DbTf


main :: IO ()
main =
    mainHelp (InitS EmptyI) StartM


-- Assocated data
authCodeA :: B.ByteString
authCodeA =
    B.pack [197, 154, 22, 2, 21, 159, 38, 105, 240, 15, 236, 142, 31, 124, 100, 71, 22, 117, 69, 163, 39, 221, 135, 100, 193, 244, 134, 63, 28, 226, 89, 31]


accountsAd :: B.ByteString
accountsAd =
    B.pack [25, 167, 137, 174, 59, 72, 173, 96, 92, 204, 174, 149, 236, 102, 197, 62, 241, 201, 146, 98, 35, 128, 92, 11, 120, 177, 109, 10, 209, 58, 90, 162]


data Msg
    = StartM
    | FileContentsM FilePath (Either E.IOException B.ByteString)
    | TcpMsgInM Tcp.SockAddr B.ByteString
    | NewTcpConnM (Queue TcpInstruction) Tcp.SockAddr
    | DeadTcpM Tcp.SockAddr
    | TheTimeM [Clock.UTCTime]
    | BatchM [Maybe Msg]
    | UserAccountHashFromDbM B.ByteString [AccountsSigFromDb]
    | RandomGenM CryptoRand.ChaChaDRG
    | ShortenFromDbM Sender [Shortenable]


newtype AccountsSigFromDb
    = AccountsSigFromDb B.ByteString


instance Db.FromRow AccountsSigFromDb where
    fromRow = AccountsSigFromDb <$> Db.field


data State
    = InitS Init
    | ReadyS Ready
    | FailedS T.Text


data Init
    = EmptyI
    | ReadingMemCacheI
    | GettingTimeI MemCache
    | GettingRandomI MemCache [Clock.UTCTime]


makeUsername :: Counter -> Username
makeUsername (Counter c) =
    Username $ encodeUint64 c


data ToClient
    = NewMessage Sender InboxMessage
    | NewUsername Username
    | Prices ShorteningPrice BlobPrice MessageUploadPrice
    | Acknowledgement Hash32


newtype ShorteningPrice
    = ShorteningPrice Money


newtype BlobPrice
    = BlobPrice Money


newtype MessageUploadPrice
    = MessageUploadPrice Money


newtype Sender
    = Sender Ed.PublicKey
    deriving Eq


instance Ord Sender where
    compare (Sender a) (Sender b) =
        compare (pubToBytes a) (pubToBytes b)


pubToBytes :: Ed.PublicKey -> B.ByteString
pubToBytes =
    ByteArray.convert


data Ready
    = Ready
        { tcpConns :: Map.Map Tcp.SockAddr TcpConn
        , randomGen :: CryptoRand.ChaChaDRG
        , counter :: Counter
        , time :: [Clock.UTCTime]
        , shortening :: Jobs ShorteningStatus ShortenWaiting
        , uploadingBlob :: Jobs UploadingBlob UploadingBlob
        , downloadingBlob :: Jobs (Sender, BlobId) (Sender, BlobId)
        , sending :: Jobs Sending Sending
        }


type ShortenWaiting
    = (Sender, PaymentDetails, Shortenable)


type Sending
    = (Sender, PaymentDetails, Recipient, InboxMessage)


type UploadingBlob
    = (Sender, PaymentDetails, UploadedBlob, BlobId)

data ShorteningStatus
    = GettingAccountsHashFromDbH Sender PaymentDetails Shortenable
    | CheckingForExisting Sender Shortenable


data MemCache
    = MemCache
        { counterM :: Counter
        }


newtype Counter
    = Counter Integer


newtype Username
    = Username B.ByteString
    deriving (Ord, Eq)


instance DbTf.ToField Username where
    toField (Username u) =
        DbTf.toField u


sendNewUsername :: Ready -> Sender -> Username -> Output
sendNewUsername ready sender username =
    case getUserQ (tcpConns ready) sender of
    Nothing ->
        DoNothingO

    Just q ->
        MsgInQO q $ Send $ encodeToClient $ NewUsername username


data Output
    = DoNothingO
    | ReadFileO FilePath
    | StartTcpServerO
    | WriteFileO FilePath B.ByteString
    | BatchO [Output]
    | MakeDirIfNotThereO FilePath
    | MsgInQO (Queue TcpInstruction) TcpInstruction
    | GetTheTimeO
    | GetUserAccountsHashDbO B.ByteString
    | DeleteInboxMessageDbO Sender Recipient InboxMessage
    | GetRandomGenO
    | LookupShortenInDbO Sender
    | SaveUserAccountsSigDbO Sender B.ByteString
    | SaveMessageToDbO Sender Recipient InboxMessage
    | ShortenableInDbO Sender Username Shortenable
    | UpdateShortenableInDbO Sender Shortenable


rootPath :: FilePath
rootPath =
    "bwtdata"


memCachePath :: FilePath
memCachePath =
    rootPath </> "memcache"


dbPath :: FilePath
dbPath =
    rootPath </> "database.sqlite"


deleteMessage :: Sender -> Sender -> InboxMessage -> Output
deleteMessage (Sender messageSender) (Sender sender) msg =
    DeleteInboxMessageDbO
        (Sender messageSender)
        (Recipient sender)
        msg


uploadBlob
    :: Ready
    -> Ed.PublicKey
    -> Ed.Signature
    -> Maybe (Output, State)
uploadBlob ready senderKeyFromDb accountsSignature =
    case uploadingBlob ready of
    NoJobs ->
        Nothing

    Jobs (sender, paymentDetails, uploadedBlob, blobId) waiting ->
        let
        relevant = Sender senderKeyFromDb == sender
        validSig = isValidSig sender paymentDetails accountsSignature
        validPay = isUploadBlobPayment paymentDetails
        in
        if relevant && validSig && validPay then
        Just
            ( BatchO
                [ saveNewAccountsSig sender paymentDetails
                , saveBlob uploadedBlob blobId
                ]
            , ReadyS $
                ready
                    { uploadingBlob =
                        case waiting of
                        [] ->
                            NoJobs

                        w:aiting ->
                            Jobs w aiting
                    }
            )
        else
        Nothing


saveBlob :: UploadedBlob -> BlobId -> Output
saveBlob (UploadedBlob blob) blobId =
    WriteFileO (makeBlobPath blobId) blob


update :: State -> Msg -> (Output, State)
update model msg =
    let
    pass = (DoNothingO, model)
    in
    case msg of
    ShortenFromDbM dbSender rows ->
        case model of
        InitS _ ->
            pass

        FailedS _ ->
            pass

        ReadyS ready ->
            case shortening ready of
            NoJobs ->
                pass

            Jobs (GettingAccountsHashFromDbH _ _ _) _ ->
                pass

            Jobs (CheckingForExisting sender shortenable) waiting ->
                if length rows == 1 then
                if dbSender == sender then
                if (head rows) == shortenable then
                pass
                else
                ( UpdateShortenableInDbO sender shortenable
                , ReadyS $
                    ready
                        { shortening =
                            case waiting of
                            [] ->
                                NoJobs

                            (newS, newP, newShortenable)  : aiting ->
                                Jobs
                                    (GettingAccountsHashFromDbH 
                                        newS
                                        newP
                                        newShortenable)
                                    aiting
                        }
                )
                else
                pass
                else
                if length rows == 0 then
                let
                username = makeUsername $ counter ready
                newCounter :: Counter
                newCounter =
                    case counter ready of
                    Counter c ->
                        Counter $ c + 1
                newReady = ready { counter = newCounter }
                in
                ( BatchO
                    [ dumpCache newReady
                    , sendNewUsername ready sender username
                    , ShortenableInDbO sender username shortenable
                    ]
                , ReadyS newReady
                )
                else
                pass

    RandomGenM gen ->
        case model of
        InitS (GettingRandomI cache times) ->
            ( DoNothingO
            , ReadyS $
                Ready
                    { tcpConns = Map.empty
                    , randomGen = gen
                    , counter = counterM cache
                    , time = times
                    , shortening = NoJobs
                    , uploadingBlob = NoJobs
                    , downloadingBlob = NoJobs
                    , sending = NoJobs
                    }
            )

        InitS EmptyI ->
            pass

        InitS ReadingMemCacheI ->
            pass

        InitS (GettingTimeI _) ->
            pass

        ReadyS _ ->
            pass

        FailedS _ ->
            pass

    UserAccountHashFromDbM _ [] ->
        pass

    UserAccountHashFromDbM rawSender [AccountsSigFromDb rawSig] ->
        case (model, Ed.publicKey rawSender, Ed.signature rawSig) of
        (InitS _, _, _) ->
            pass

        (FailedS _, _, _) ->
            pass

        (ReadyS ready, CryptoPassed senderKey, CryptoPassed sig) ->
            case sendMessage ready senderKey sig of
            Just r ->
                r

            Nothing ->
                case uploadBlob ready senderKey sig of
                Just r ->
                    r

                Nothing ->
                    shortenOnAccounts ready senderKey sig

        (_, CryptoFailed keyErr, _) ->
            ( DoNothingO
            , FailedS $
                mconcat
                [ "could not parse Ed25519 public key from database: "
                , T.pack $ show keyErr
                ]
            )

        (_, _, CryptoFailed sigErr) ->
            ( DoNothingO
            , FailedS $
                mconcat
                [ "could not parse Ed25519 signature from database: "
                , T.pack $ show sigErr
                ]
            )

    UserAccountHashFromDbM _ _ ->
        pass

    TheTimeM times ->
        case model of
        InitS EmptyI ->
            pass

        InitS ReadingMemCacheI ->
            pass

        InitS (GettingTimeI memCache) ->
            ( GetRandomGenO
            , InitS $ GettingRandomI memCache times
            )

        InitS (GettingRandomI _ _) ->
            pass

        ReadyS _ ->
            pass

        FailedS _ ->
            pass

    DeadTcpM address ->
        case model of
        InitS _ ->
            pass

        FailedS _ ->
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
        (ReadFileO memCachePath, InitS ReadingMemCacheI)

    FileContentsM path contents ->
        updateOnFileContents path contents model

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

        FailedS _ ->
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
            ( DoNothingO
            , ReadyS $ ready {tcpConns = newConns, randomGen = gen}
            )


shortenOnAccounts
    :: Ready
    -> Ed.PublicKey
    -> Ed.Signature
    -> (Output, State)
shortenOnAccounts ready senderKeyFromDb accountsSignature =
    let
    pass = (DoNothingO, ReadyS ready)
    in
    case shortening ready of
    Jobs (GettingAccountsHashFromDbH s p shortenable) waiting ->
        let
        relevant = Sender senderKeyFromDb == s
        validSig = isValidSig s p accountsSignature
        validPay = isShortenPayment p
        in
        if relevant && validSig && validPay then
        ( BatchO [LookupShortenInDbO s, saveNewAccountsSig s p]
        , ReadyS $
            ready
                { shortening =
                    Jobs (CheckingForExisting s shortenable) waiting
                }
        )
        else
        pass

    Jobs (CheckingForExisting _ _) _ ->
        pass

    NoJobs ->
        pass


sendMessage
    :: Ready
    -> Ed.PublicKey
    -> Ed.Signature
    -> Maybe (Output, State)
sendMessage ready senderKeyFromDb accountsSignature =
    case sending ready of
    NoJobs ->
        Nothing

    Jobs (sender, paymentDetails, recipient, inboxMessage) waiting ->
        let
        relevant = Sender senderKeyFromDb == sender
        validSig = isValidSig sender paymentDetails accountsSignature
        validPay = isSendPayment paymentDetails
        in
        if relevant && validSig && validPay then
        Just
            ( BatchO
                [ forwardMessageIfPossible
                    ready
                    sender
                    recipient
                    inboxMessage
                , SaveMessageToDbO sender recipient inboxMessage
                , saveNewAccountsSig sender paymentDetails
                , case waiting of
                    [] ->
                        DoNothingO

                    (newSender, _, _, _):_ ->
                        getPaymentHashFromDb newSender
                ]
            , ReadyS $
                ready
                    { sending =
                        case waiting of
                        [] ->
                            NoJobs

                        w:aiting ->
                            Jobs w aiting
                    }
            )
        else
        Nothing


forwardMessageIfPossible
    :: Ready
    -> Sender
    -> Recipient
    -> InboxMessage
    -> Output
forwardMessageIfPossible
    ready
    sender
    (Recipient recipient)
    inboxMessage =

    case getUserQ (tcpConns ready) (Sender recipient) of
    Nothing ->
        DoNothingO

    Just recipientQ ->
        MsgInQO recipientQ $
        Send $
        encodeToClient $
        NewMessage sender inboxMessage


saveNewAccountsSig :: Sender -> PaymentDetails -> Output
saveNewAccountsSig
    sender
    (PaymentDetails _ _ (AccountsSignature sig)) =

    SaveUserAccountsSigDbO sender (ByteArray.convert sig)


isValidSig :: Sender -> PaymentDetails -> Ed.Signature -> Bool
isValidSig (Sender sender) (PaymentDetails old new newSig) oldSig =
    let
    oldHash =
        case old of
        OldTransaction (Transaction _ _ _ _ (Hash32 h)) ->
            h
    oldWithAd = accountsAd <> oldHash
    newHash =
        case new of
        NewTransaction (Transaction _ _ _ _ h) ->
            h
    newWithAd = accountsAd <> (case newHash of
        Hash32 h ->
            h)
    maybeExpecteNewHash =
        hash32 $ oldHash <> (encodeTransactionToHash $
        case new of
        NewTransaction t ->
            t)
    in
    case maybeExpecteNewHash of
    Nothing ->
        False

    Just expectedNewHash ->
        let
        validOldSig = Ed.verify sender oldWithAd oldSig
        validNewHash = newHash == expectedNewHash
        validNewSig =
            Ed.verify sender newWithAd $
            case newSig of
            AccountsSignature s ->
                s
        oldBalance =
            case old of
            OldTransaction (Transaction _ _ (Balance (Money balance)) _ _) ->
                balance
        newBalance =
            case new of
            NewTransaction (Transaction _ _ (Balance (Money balance)) _ _) ->
                balance
        newAmount =
            case new of
            NewTransaction (Transaction (Amount (Money a)) _ _ _ _) ->
                a
        balanceOk = (oldBalance + newAmount) == newBalance
        in
        validOldSig &&
        validNewHash &&
        validNewSig &&
        balanceOk &&
        (newBalance >= 0)


encodeTransactionToHash :: Transaction -> B.ByteString
encodeTransactionToHash (Transaction amount time balance payment _) =
    mconcat
    [ encodeAmount amount
    , encodeTime time
    , encodeBalance balance
    , encodePayment payment
    ]


encodeAmount :: Amount -> B.ByteString
encodeAmount (Amount m) =
    encodeMoney m


encodeTime :: PosixTime -> B.ByteString
encodeTime (PosixTime t) =
    encodeUint64 t


encodeBalance :: Balance -> B.ByteString
encodeBalance (Balance money) =
    encodeMoney money


encodeMoney :: Money -> B.ByteString
encodeMoney (Money m) =
    encodeUint32 m


encodePayment :: Payment -> B.ByteString
encodePayment payment =
    case payment of
    PaymentToServer (Hash32 h) ->
        B.singleton 0 <> h

    AccountTopUp (TopUpId t) ->
        B.singleton 1 <> t


authLength :: Int
authLength =
    32


isUploadBlobPayment :: PaymentDetails -> Bool
isUploadBlobPayment
    (PaymentDetails _ (NewTransaction (Transaction a _ _ _ _)) _) =

    a == Amount blobPrice


isSendPayment :: PaymentDetails -> Bool
isSendPayment
    (PaymentDetails _ (NewTransaction (Transaction a _ _ _ _)) _) =

    a == Amount messageUploadPrice


isShortenPayment :: PaymentDetails -> Bool
isShortenPayment
    (PaymentDetails _ (NewTransaction (Transaction a _ _ _ _)) _) =

    a == Amount shorteningPrice


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

    FailedS _ ->
        (DoNothingO, model)

    ReadyS ready ->
        case Map.lookup address (tcpConns ready) of
        Nothing ->
            (DoNothingO, model)

        Just (TcpConn q _) ->
            case P.eitherResult (P.parse fromClientP rawMessage) of
            Left _ ->
                let
                newConns = Map.delete address (tcpConns ready)
                in
                ( MsgInQO q Die
                , ReadyS $ ready { tcpConns = newConns}
                )

            Right untrusted ->
                updateOnTcpMessage address ready untrusted


shorteningPrice :: Money
shorteningPrice =
    Money 1


blobPrice :: Money
blobPrice =
    Money 5


messageUploadPrice :: Money
messageUploadPrice =
    Money 3


prices :: ToClient
prices =
    Prices
        (ShorteningPrice shorteningPrice)
        (BlobPrice blobPrice)
        (MessageUploadPrice messageUploadPrice)


encodeToClient :: ToClient -> B.ByteString
encodeToClient msg =
    case msg of
    NewMessage sender message ->
        mconcat
        [ B.singleton 5
        , encodeSender sender
        , encodeInboxMessage message
        ]

    NewUsername (Username u) ->
        B.singleton 2 <> u

    Prices
        (ShorteningPrice (Money shortening))
        (BlobPrice (Money blob))
        (MessageUploadPrice (Money message)) ->

        mconcat
        [ B.singleton 1
        , encodeUint32 shortening
        , encodeUint32 blob
        , encodeUint32 message
        ]

    Acknowledgement (Hash32 hash) ->
        B.singleton 4 <> hash


encodeInboxMessage :: InboxMessage -> B.ByteString
encodeInboxMessage (InboxMessage m) =
    m


encodeSender :: Sender -> B.ByteString
encodeSender (Sender s) =
    pubToBytes s


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
            pass

        GetPrices ->
            (MsgInQO q $ Send $ encodeToClient prices, ReadyS ready)

        DeleteMessage messageSender message ->
            (deleteMessage messageSender sender message, ReadyS ready)

        SendMessage paymentDetails recipient inboxMessage ->
            case sending ready of
            NoJobs ->
                ( getPaymentHashFromDb sender
                , ReadyS $
                    ready
                        { sending =
                            Jobs
                                ( sender
                                , paymentDetails
                                , recipient
                                , inboxMessage
                                )
                                []
                        }
                )

            Jobs current waiting ->
                let
                newJob =
                    (sender, paymentDetails, recipient, inboxMessage)
                in
                ( DoNothingO
                , ReadyS $
                    ready
                        { sending =
                            Jobs
                                current
                                (reverse $ newJob : waiting)
                        }
                )

        ShortenId paymentDetails shortenable ->
            case shortening ready of
            NoJobs ->
                ( getPaymentHashFromDb sender
                , ReadyS $
                    ready
                        { shortening =
                            Jobs
                            (GettingAccountsHashFromDbH
                                sender
                                paymentDetails
                                shortenable)
                            []
                        }
                )

            Jobs current waiting ->
                let
                newJob = (sender, paymentDetails, shortenable)
                in
                ( DoNothingO
                , ReadyS $
                    ready
                        { shortening
                            = Jobs
                                current
                                (reverse $ newJob : waiting)
                        }
                )

        UploadBlob paymentDetails blobId uploadedBlob ->
            case uploadingBlob ready of
            NoJobs ->
                ( getPaymentHashFromDb sender
                , ReadyS $
                    ready
                        { uploadingBlob =
                            Jobs
                                ( sender
                                , paymentDetails
                                , uploadedBlob
                                , blobId
                                )
                                []
                        }
                )

            Jobs current waiting ->
                let
                newJob =
                    (sender, paymentDetails, uploadedBlob, blobId)
                in
                ( DoNothingO
                , ReadyS $
                    ready
                        { uploadingBlob =
                            Jobs current (reverse $ newJob : waiting)
                        }
                )

        DownloadBlob blobId ->
            case downloadingBlob ready of
            NoJobs ->
                ( downloadBlob blobId
                , ReadyS $
                    ready
                        { downloadingBlob = Jobs (sender, blobId) [] }
                )

            Jobs current waiting ->
                ( DoNothingO
                , ReadyS $
                    ready
                        { downloadingBlob =
                            Jobs
                                current
                                (reverse $ (sender, blobId) : waiting)
                        }
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

        GetPrices ->
            pass

        ShortenId _ _ ->
            pass

        UploadBlob _ _ _ ->
            pass

        DownloadBlob _ ->
            pass

        SendMessage _ _ _ ->
            pass

        DeleteMessage _ _ ->
            pass


downloadBlob :: BlobId -> Output
downloadBlob blobId =
    ReadFileO (makeBlobPath blobId)


blobsPath :: FilePath
blobsPath =
    rootPath </> "blobs"


makeBlobPath :: BlobId -> FilePath
makeBlobPath (BlobId b) =
    blobsPath </>
    T.unpack (Hex.encodeHex $ ByteArray.convert b)


getPaymentHashFromDb :: Sender -> Output
getPaymentHashFromDb (Sender sender) =
    GetUserAccountsHashDbO $ pubToBytes sender 


validAuthSig :: AuthCode -> SignedAuthCode -> Sender -> Bool
validAuthSig
    (AuthCode authCode)
    (SignedAuthCode signature)
    (Sender sender) =

    Ed.verify sender (authCodeA <> authCode) signature


data Jobs a b
    = Jobs a [b]
    | NoJobs


dumpCache :: Ready -> Output
dumpCache ready =
    WriteFileO
        memCachePath
        (encodeCache $ readyToMemCache ready)


encodeCache :: MemCache -> B.ByteString
encodeCache cache =
    encodeCounter $ counterM cache


encodeCounter :: Counter -> B.ByteString
encodeCounter (Counter c) =
    encodeUint64 c


encodeUint32 :: Int -> B.ByteString
encodeUint32 i =
    B.pack $ map (encodeUintHelp i) (take 4 [0..])


encodeUintHelp :: Int -> Int -> Word8
encodeUintHelp int counter =
    fromIntegral $ int `Bits.shiftR` (counter * 8) Bits..&. 0xFF


encodeUint64 :: Integer -> B.ByteString
encodeUint64 i =
    B.pack $ map (encodeUintegerHelp i) (take 8 [0..])


encodeUintegerHelp :: Integer -> Int -> Word8
encodeUintegerHelp int counter =
    fromIntegral $ int `Bits.shiftR` (counter * 8) Bits..&. 0xFF


readyToMemCache :: Ready -> MemCache
readyToMemCache ready =
    MemCache
        { counterM = counter ready
        }


data TcpConn
    = TcpConn (Queue TcpInstruction) AuthStatus


data AuthStatus
    = Authenticated Sender
    | Untrusted AuthCode


-- The hash is the hash of the previous transaction hash combined
-- with the encoded transaction:
--
--     previousHash <> amount <> posixTime <> balance <> payment
--
data Transaction
    = Transaction Amount PosixTime Balance Payment Hash32


data Payment
    = PaymentToServer Hash32
    | AccountTopUp TopUpId


newtype AuthCode
    = AuthCode B.ByteString


data FromClient
    = SignedAuthCodeF Sender SignedAuthCode
    | GetPrices
    | ShortenId PaymentDetails Shortenable
    | UploadBlob PaymentDetails BlobId UploadedBlob
    | DownloadBlob BlobId
    | SendMessage PaymentDetails Recipient InboxMessage
    | DeleteMessage Sender InboxMessage


paymentDetailsP :: P.Parser PaymentDetails
paymentDetailsP = do
    oldTransaction <- transactionP
    newTransaction <- transactionP
    signature <- signatureP
    return $
        PaymentDetails
            (OldTransaction oldTransaction)
            (NewTransaction newTransaction)
            (AccountsSignature signature)


transactionP :: P.Parser Transaction
transactionP = do
    amount <- moneyP
    time <- timeP
    balance <- moneyP
    payment <- paymentP
    hash <- hash32P
    return $
        Transaction
            (Amount amount)
            time
            (Balance balance)
            payment
            hash


paymentP :: P.Parser Payment
paymentP = do
    P.choice [paymentToServerP, accountTopUpP]


accountTopUpP :: P.Parser Payment
accountTopUpP = do
    _ <- P.word8 1
    topUpId <- topUpIdP
    return $ AccountTopUp topUpId


topUpIdP :: P.Parser TopUpId
topUpIdP = do
    raw <- P.take 32
    return $ TopUpId raw


moneyP :: P.Parser Money
moneyP = do
    raw <- uint32P
    return $ Money raw


paymentToServerP :: P.Parser Payment
paymentToServerP = do
    _ <- P.word8 0
    hash <- hash32P
    return $ PaymentToServer hash


newtype BlobId
    = BlobId B.ByteString


newtype TopUpId
    = TopUpId B.ByteString


newtype Balance
    = Balance Money


newtype Money
    = Money Int
    deriving Eq


newtype Amount
    = Amount Money
    deriving Eq


data PaymentDetails
    = PaymentDetails OldTransaction NewTransaction AccountsSignature


newtype OldTransaction
    = OldTransaction Transaction


newtype NewTransaction
    = NewTransaction Transaction


newtype AccountsSignature
    = AccountsSignature Ed.Signature


fromClientP :: P.Parser FromClient
fromClientP = do
    msg <- P.choice
        [ do
            _ <- P.word8 0
            sender <- senderP
            signedAuth <- signedAuthCodeP
            return $ SignedAuthCodeF sender signedAuth
        , do
            _ <- P.word8 4
            return GetPrices
        , do
            _ <- P.word8 6
            paymentDetails <- paymentDetailsP
            shortenable <- shortenableP
            return $ ShortenId paymentDetails shortenable
        , do
            _ <- P.word8 7
            paymentDetails <- paymentDetailsP
            blobId <- blobIdP
            blob <- uploadedBlobP
            return $ UploadBlob paymentDetails blobId blob
        , do
            _ <- P.word8 8
            blobId <- blobIdP
            return $ DownloadBlob blobId
        , do
            _ <- P.word8 9
            paymentDetails <- paymentDetailsP
            recipient <- recipientP
            message <- inboxMessageP
            return $ SendMessage paymentDetails recipient message
        , do
            _ <- P.word8 10
            sender <- senderP
            message <- messageP
            return $ DeleteMessage sender message
        ]
    P.endOfInput
    return msg


shortenableLength :: Int
shortenableLength =
    61


shortenableP :: P.Parser Shortenable
shortenableP = do
    raw <- P.take shortenableLength
    return $ Shortenable raw


signingKeyP :: P.Parser Ed.PublicKey
signingKeyP = do
    raw <- P.take Ed.publicKeySize
    case Ed.publicKey raw of
        CryptoPassed key ->
            return key

        CryptoFailed err ->
            fail $ show err


hash32P :: P.Parser Hash32
hash32P = do
    raw <- P.take 32
    return $ Hash32 raw


blobIdP :: P.Parser BlobId
blobIdP = do
    raw <- P.take 32
    return $ BlobId raw


senderP :: P.Parser Sender
senderP = do
    key <- signingKeyP
    return $ Sender key


recipientP :: P.Parser Recipient
recipientP = do
    key <- signingKeyP
    return $ Recipient key


signatureP :: P.Parser Ed.Signature
signatureP = do
    raw <- P.take Ed.signatureSize
    case Ed.signature raw of
        CryptoPassed signature ->
            return signature

        CryptoFailed err ->
            fail $ show err


signedAuthCodeP :: P.Parser SignedAuthCode
signedAuthCodeP = do
    sig <- signatureP
    return $ SignedAuthCode sig


timeP :: P.Parser PosixTime
timeP = do
    raw <- uint64P
    return $ PosixTime raw


-- Seconds since POSIX epoch.
newtype PosixTime
    = PosixTime Integer


uint64P :: P.Parser Integer
uint64P = do
    raw <- P.take 8
    let bytes = B.unpack raw
    let withIndices = zip bytes ([0..] :: [Integer])
    let powered =
          map (\(b, i) -> fromIntegral b * 256 ^ i) withIndices
    return $ sum powered


inboxMessageP :: P.Parser InboxMessage
inboxMessageP = do
    raw <- P.take 32
    return $ InboxMessage raw


messageP :: P.Parser InboxMessage
messageP = do
    raw <- P.take 32
    return $ InboxMessage raw


newtype SignedAuthCode
    = SignedAuthCode Ed.Signature


uploadedBlobP :: P.Parser UploadedBlob
uploadedBlobP = do
    blob <- P.takeByteString
    return $ UploadedBlob blob


newtype UploadedBlob
    = UploadedBlob B.ByteString


newtype Shortenable
    = Shortenable B.ByteString
    deriving Eq


instance DbTf.ToField Shortenable where
    toField (Shortenable s) =
        DbTf.toField s


instance Db.FromRow Shortenable where
    fromRow =
        fmap Shortenable Db.field


newtype InboxMessage
    = InboxMessage B.ByteString


newtype Recipient
    = Recipient Ed.PublicKey


newtype Hash32
    = Hash32 B.ByteString
    deriving Eq


memCacheP :: P.Parser MemCache
memCacheP = do
    counterM <- counterP
    return $ MemCache { counterM }


counterP :: P.Parser Counter
counterP = do
    raw <- uint64P
    return $ Counter raw


uint8P :: P.Parser Int
uint8P =
    fromIntegral <$> P.anyWord8


uint32P :: P.Parser Int
uint32P = do
    b0 <- uint8P
    b1 <- uint8P
    b2 <- uint8P
    b3 <- uint8P
    return $ b0 + b1 * 256 + b2 * 256 * 256 + b3 * 256 * 256 * 256


dumpInitCache :: Output
dumpInitCache =
    WriteFileO memCachePath $ encodeCache initMemCache


initMemCache :: MemCache
initMemCache =
    MemCache
        { counterM = Counter 0
        }


updateOnFileContents
    :: FilePath
    -> Either E.IOException B.ByteString
    -> State
    -> (Output, State)
updateOnFileContents path result model =
    let
    pass = (DoNothingO, model)
    in
    case model of
    InitS EmptyI ->
        pass

    InitS (GettingTimeI _) ->
        pass

    InitS (GettingRandomI _ _) ->
        pass

    InitS ReadingMemCacheI ->
        if path == memCachePath then
        case result of
        Left exception ->
            if isDoesNotExistError exception then
            ( BatchO [dumpInitCache, GetTheTimeO]
            , InitS $ GettingTimeI initMemCache
            )
            else
            ( DoNothingO
            , FailedS $
                "could not read memory cache: " <>
                T.pack (show exception)
            )

        Right contents ->
            case P.eitherResult (P.parse memCacheP contents) of
            Left err ->
                ( DoNothingO
                , FailedS $ "corrupt memory cache: " <> T.pack err
                )

            Right cache ->
                (GetTheTimeO, InitS $ GettingTimeI cache)

        else
        pass

    ReadyS _ ->
        pass

    FailedS _ ->
        pass


getUserQ
    :: Map.Map Tcp.SockAddr TcpConn
    -> Sender
    -> Maybe (Queue TcpInstruction)
getUserQ conns sender =
    Map.lookup sender $ toUserConns conns


toUserConns
    :: Map.Map Tcp.SockAddr TcpConn
    -> Map.Map Sender (Queue TcpInstruction)
toUserConns =
    Map.foldrWithKey toUserConnsHelp Map.empty


toUserConnsHelp
    :: Tcp.SockAddr
    -> TcpConn
    -> Map.Map Sender (Queue TcpInstruction)
    -> Map.Map Sender (Queue TcpInstruction)
toUserConnsHelp _ (TcpConn q auth) oldUserConns =
    case auth of
    Authenticated sender ->
        Map.insert sender q oldUserConns

    Untrusted _ ->
        oldUserConns


type Digest
    = Hash.Digest Hash.Blake2b_256


hash32 :: B.ByteString -> Maybe Hash32
hash32 raw =
    case makeDigest raw of
    Nothing ->
        Nothing

    Just digest ->
        Just $ Hash32 $ ByteArray.convert digest


makeDigest :: B.ByteString -> Maybe Digest
makeDigest =
    Hash.digestFromByteString


io :: Output -> IO (Maybe Msg)
io output =
    case output of
    DoNothingO ->
        return Nothing

    ReadFileO path -> do
        result <- E.try $ B.readFile path
        return $ Just $ FileContentsM path result

    StartTcpServerO -> do
        _ <- CC.forkIO tcpServer
        return Nothing

    GetTheTimeO -> do
        lazyTime <- mapM (\_ -> Clock.getCurrentTime) lazyInts
        return $ Just $ TheTimeM lazyTime

    WriteFileO path contents -> do
        B.writeFile path contents
        return Nothing

    BatchO outputs -> do
        inputs <- mapM io outputs
        return $ Just $ BatchM inputs

    MsgInQO q msg -> do
        writeQ q msg
        return Nothing

    MakeDirIfNotThereO path -> do
        Dir.createDirectoryIfMissing True path
        return Nothing

    GetUserAccountsHashDbO sender -> do
        result <- Db.withConnection dbPath $ \conn ->
            Db.query conn getUserAccountsHashSql (Db.Only sender)
        return $ Just $ UserAccountHashFromDbM sender result 

    DeleteInboxMessageDbO sender recipient message -> do
        Db.withConnection dbPath $ \conn ->
            Db.execute
                conn
                deleteMessageSql
                (sender, recipient, message)
        return Nothing

    GetRandomGenO -> do
        drg <- CryptoRand.drgNew
        return $ Just $ RandomGenM drg

    LookupShortenInDbO sender -> do
        result <- Db.withConnection dbPath $ \conn ->
            Db.query conn getShortenSql (Db.Only sender)
        return $ Just $ ShortenFromDbM sender result

    SaveUserAccountsSigDbO sender sigBytes -> do
        Db.withConnection dbPath $ \conn ->
            Db.execute conn insertAccountsSigSql (sender, sigBytes)
        return Nothing

    SaveMessageToDbO sender recipient inboxMessage -> do
        Db.withConnection dbPath $ \conn ->
            Db.execute
                conn
                saveMessageSql
                (sender, recipient, inboxMessage)
        return Nothing

    ShortenableInDbO sender username shortenable -> do
        Db.withConnection dbPath $ \conn ->
            Db.execute
                conn
                shortenableInDbSql
                (sender, username, shortenable)
        return Nothing

    UpdateShortenableInDbO sender shortenable -> do
        Db.withConnection dbPath $ \conn ->
            Db.execute
                conn
                updateShortenableInDbSql
                (shortenable, sender)
        return Nothing


updateShortenableInDbSql :: Db.Query
updateShortenableInDbSql =
    "UPDATE shortenings SET shortenable = ? WHERE user = ?;" 


shortenableInDbSql :: Db.Query
shortenableInDbSql =
    "INSERT INTO shortenings (user, short, shortenable) \
    \VALUES (?, ?, ?);"


saveMessageSql :: Db.Query
saveMessageSql =
    "INSERT INTO messages (sender, recipient, message) \
    \VALUES (?, ?, ?);"


insertAccountsSigSql :: Db.Query
insertAccountsSigSql =
    "INSERT INTO accountsignatures (user, signature) VALUES (?, ?);"


getShortenSql :: Db.Query
getShortenSql =
    "SELECT shortened FROM shortenings WHERE user = ?;"


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
            

getUserAccountsHashSql :: Db.Query
getUserAccountsHashSql =
    "SELECT signature FROM accountsignatures WHERE user = ?;"


lazyInts :: [Integer]
lazyInts =
    [1..]


tcpServer :: IO ()
tcpServer =
    Tcp.serve (Tcp.Host "127.0.0.1") "11453" tcpServerHelp


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


tcpServerHelp :: (Tcp.Socket, Tcp.SockAddr) -> IO ()
tcpServerHelp (socket, address) = do
    let instructionsQ = Q.newTQueue :: Queue TcpInstruction
    writeQ msgQ $ NewTcpConnM instructionsQ address
    receiverId <- CC.forkIO $ tcpReceiver socket address
    tcpSender address receiverId instructionsQ socket


data TcpInstruction
    = Die
    | Send B.ByteString


tcpSend :: Tcp.Socket -> B.ByteString -> IO (Either E.IOException ())
tcpSend socket msg =
    E.try $ Tcp.send socket msg


tcpSender
    :: Tcp.SockAddr
    -> CC.ThreadId
    -> Queue TcpInstruction
    -> Tcp.Socket
    -> IO ()
tcpSender address receiverId instructionsQ socket = do
    instruction <- readQ instructionsQ
    case instruction of
        Die ->
            CC.killThread receiverId

        Send msg -> do
            eitherOk <- tcpSend socket msg
            case eitherOk of
                Left _ -> do
                    writeQ msgQ $ DeadTcpM address
                    CC.killThread receiverId

                Right () ->
                    tcpSender address receiverId instructionsQ socket


tcpRecv
    :: Tcp.Socket
    -> IO (Either E.IOException (Maybe B.ByteString))
tcpRecv socket =
    E.try $ Tcp.recv socket maxMessageLength


tcpReceiver :: Tcp.Socket -> Tcp.SockAddr -> IO ()
tcpReceiver socket address = do
    eitherMsg <- tcpRecv socket
    case eitherMsg of
        Left _ ->
            writeQ msgQ $ DeadTcpM address

        Right Nothing ->
            writeQ msgQ $ DeadTcpM address

        Right (Just msg) -> do
            writeQ msgQ $ TcpMsgInM address msg
            tcpReceiver socket address


maxMessageLength :: Int
maxMessageLength =
    16000
        
        
msgQ :: Stm.STM (Q.TQueue Msg)
msgQ =
    Q.newTQueue


mainHelp :: State -> Msg -> IO ()
mainHelp oldState oldMsg =
    case oldState of
    FailedS err ->
        Tio.putStrLn err

    _ -> do
        let (output, newState) = update oldState oldMsg
        maybeMsg <- io output
        newMsg <- case maybeMsg of
            Nothing ->
                Stm.atomically $ do
                    q <- msgQ
                    Q.readTQueue q
            Just m ->
                return m
        mainHelp newState newMsg

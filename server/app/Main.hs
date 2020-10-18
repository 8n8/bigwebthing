{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import qualified Data.ByteArray as ByteArray
import qualified Crypto.KDF.Argon2 as Argon2
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
import qualified Data.Set as Set
import qualified Text.Hex as Hex
import qualified Crypto.Hash as Hash


main :: IO ()
main =
    mainHelp InitS StartM


data Msg
    = StartM
    | FileContentsM FilePath (Either E.IOException B.ByteString)
    | TcpMsgIn Tcp.SockAddr B.ByteString
    | NewTcpConnM (Queue B.ByteString) Tcp.SockAddr Tcp.SockAddr
    | NewTcpMsgM Tcp.SockAddr B.ByteString
    | DeadTcpM Tcp.SockAddr


data State
    = ReadyS Ready
    | InitS
    | FailedS T.Text


toClient :: TcpConn -> ToClient -> Output
toClient conn msg =
    MsgInQO (instructionsQ conn) (Send $ encodeToClient msg)


encodeToClient :: ToClient -> B.ByteString
encodeToClient to =
    let
    message = encodeToClientHelp to
    in
    mconcat
    [ encodeUint16 $ B.length message
    , message
    ]


encodeToClientHelp :: ToClient -> B.ByteString
encodeToClientHelp to =
    case to of
    NewMessage (Sender (Username sender)) (Message message) -> 
        B.singleton 0 <> sender <> message

    NewUsername (Username username) ->
        B.singleton 1 <> username

    Price p ->
        B.singleton 2 <> encodeUint32 p

    Acknowledgement (Hash32 hash) ->
        B.singleton 3 <> hash


data ToClient
    = NewMessage Sender Message
    | NewUsername Username
    | Price Int
    | Acknowledgement Hash32


newtype PowRandom
    = PowRandom B.ByteString


newtype Difficulty
    = Difficulty Int


newtype Message
    = Message B.ByteString


newtype Sender
    = Sender Username


data Ready
    = Ready
        { users :: Map.Map Username Auth
        , tcpConns :: Map.Map Tcp.SockAddr TcpConn
        , randomGen :: CryptoRand.ChaChaDRG
        , counter :: Integer
        , time :: [Clock.UTCTime]
        , rateLimits ::
            Map.Map Tcp.SockAddr (Clock.UTCTime, Maybe Clock.DiffTime)
        , sendingMessage :: Jobs SendingMessage SendMessageWait
        }


data SendingMessage
    = GettingWhitelist Sender Recipient Message
    | GettingBilling Sender Recipient Message


data SendMessageWait
    = SendMessageWait Sender Recipient Message


data MemCache
    = MemCache
        { usersM :: Map.Map Username Auth
        , counterM :: Integer
        }


data Auth
    = Auth PasswordHash Salt Argon2.Options


newtype Salt
    = Salt B.ByteString


newtype PasswordHash
    = PasswordHash B.ByteString


newtype Username
    = Username B.ByteString
    deriving (Ord, Eq)


data Output
    = DoNothingO
    | ReadFileO FilePath
    | StartTcpServerO
    | WriteFileO FilePath B.ByteString
    | BatchO [Output]
    | MakeDirIfNotThereO FilePath
    | MsgInQO (Queue TcpInstruction) TcpInstruction


rootPath =
    "bwtdata"

memCachePath =
    rootPath </> "memcache"


update :: State -> Msg -> (Output, State)
update model msg =
    case msg of
    StartM ->
        (ReadFileO memCachePath, InitS)

    FileContentsM path contents ->
        updateOnFileContents path contents model

    TcpMsgIn address rawMessage ->
        updateOnRawTcpMessage address rawMessage model


updateOnRawTcpMessage
    :: Tcp.SockAddr
    -> B.ByteString
    -> State
    -> (Output, State)
updateOnRawTcpMessage address rawMessage model = 
    case model of
    InitS ->
        (DoNothingO, model)

    FailedS _ ->
        (DoNothingO, model)

    ReadyS ready ->
        case Map.lookup address (tcpConns ready) of
        Nothing ->
            (DoNothingO, model)

        Just conn ->
            case P.eitherResult (P.parse tcpMessageP rawMessage) of
            Left err ->
                let
                newConns = Map.delete address (tcpConns ready)
                kills = map KillThreadO (threadIds conn)
                in
                ( ReadyS $ ready { tcpConns = newConns}
                , kills
                )

            Right untrusted ->
                updateOnTcpMessage address conn ready untrusted


updateOnTcpMessage
    :: Tcp.SockAddr
    -> Ready
    -> TcpMessage
    -> (Output, State)
updateOnTcpMessage address ready untrusted =
    case Map.lookup address $ tcpConns ready of
    Nothing ->
        (DoNothingO, ReadyS ready)

    Just conn ->
        case untrusted of
        NeedsAuthT needsAuth ->
            updateOnNeedsAuthMessage address conn ready needsAuth

        FreeT free ->
            updateOnFreeMessage address conn ready free


updateOnNeedsAuthMessage
    :: Tcp.SockAddr
    -> TcpConn
    -> Ready
    -> NeedsAuth
    -> (Output, State)
updateOnNeedsAuthMessage address (TcpConn q auth) ready untrusted =
    case auth of
    Untrusted ->
        (DoNothingO, ReadyS ready)

    Authenticated sender ->
        case untrusted of
        SendMessage recipient message ->
            let
            newSending =
                case sendingMessage ready of
                NoJobs ->
                    Jobs
                        (GettingBilling sender recipient message)
                        []
                Jobs current waiting ->
                    Jobs
                    current
                    (SendMessageWait sender recipient message :
                     waiting)
            newReady =
                ready { sendingMessage = newSending }
            in
            ( ReadFileO (makeBillingPath recipient)
            , ReadyS newReady
            )


data Billing
    = Billing
        { subscriptions :: [(Clock.UTCTime, Int)]
        , startDate :: Clock.UTCTime
        }


data Jobs a b
    = Jobs a [b]
    | NoJobs


powUniqueLength =
    16


updateOnFreeMessage
    :: Tcp.SockAddr
    -> TcpConn
    -> Ready
    -> Free
    -> (Output, State)
updateOnFreeMessage address conn ready free =
    case free of
    NewAccount password ->
        case Map.lookup address (rateLimits ready) of
        Nothing ->
            makeNewAccount address password conn ready

        Just (lastTime, lastLag) ->
            let
            now = head $ time ready
            in
            if now - lastTime < 2 * lastLag then
            (DoNothingO, ReadyS ready)
            
            else
            makeNewAccount address password conn ready


makeRateLimit
    :: Maybe (Clock.UTCTime, Maybe Clock.DiffTime)
    -> Clock.UTCTime
    -> (Clock.UTCTime, Maybe Clock.DiffTime)
makeRateLimit maybeOld now =
    case maybeOld of
    Nothing ->
        (now, Nothing)

    Just old ->
        (now, Just $ now - old)


makeSalt :: CryptoRand.ChaChaDRG -> (Salt, CryptoRand.ChaChaDRG)
makeSalt gen =
    let
    (bytes, newGen) =
        CryptoRand.randomBytesGenerate saltLength gen
    in
    (Salt bytes, newGen)


makeNewAccount
    :: Tcp.SockAddr
    -> Password
    -> TcpConn
    -> Ready
    -> (Output, State)
makeNewAccount address password conn@(TcpConn q authStatus) ready =
    let
    (salt, gen) = makeSalt (randomGen ready)
    in
    case (hashPassword password salt, authStatus) of
    (_, Authenticated _) ->
        (DoNothingO, ReadyS ready)

    (Left err, Untrusted) ->
        (DoNothingO, FailedS $ "could not hash password: " <> err)

    (Right hash, Untrusted) ->
        let
        rateLimit =
            makeRateLimit
                (Map.lookup address $ rateLimits ready)
                (head $ time ready)
        newUsername = Username $ encodeUint64 $ counter ready
        auth = Auth hash salt
        newReady =
            ready
                { randomGen = gen
                , counter = counter ready + 1
                , rateLimits =
                    Map.insert address rateLimit (rateLimits ready)
                , time = tail $ time ready
                , users =
                    Map.insert
                        newUsername
                        (Auth hash salt argonOptions)
                        (users ready)
                , tcpConns =
                    Map.insert
                        address
                        (TcpConn q (Authenticated newUsername))
                        (tcpConns ready)
                }
        in
        ( BatchO
            [ toClient conn $ NewUsername newUsername
            , dumpCache newReady
            ]
        , ReadyS newReady
        )
        

dumpCache :: Ready -> Output
dumpCache ready =
    WriteFileO
        memCachePath
        (encodeCache $ readyToMemCache ready)


newtype RootPath
    = RootPath FilePath


encodeCache :: MemCache -> B.ByteString
encodeCache cache =
    mconcat
    [ encodeUsers $ usersM cache
    , encodeUint64 $ counterM cache
    ]


encodeUsers :: Map.Map Username Auth -> B.ByteString
encodeUsers users =
    let
    asList = Map.toList users
    in
    mconcat
    [ encodeUint32 $ length asList
    , mconcat $ map encodeUser $ Map.toList users
    ]


encodeUser :: (Username, Auth) -> B.ByteString
encodeUser (username, auth) =
    encodeUsername username <> encodeAuth auth


encodeUsername :: Username -> B.ByteString
encodeUsername (Username bytes) =
    bytes


encodeAuth :: Auth -> B.ByteString
encodeAuth (Auth passwordHash salt argonOptions) =
    encodePasswordHash <> encodeSalt <> encodeArgonOptions


encodePasswordHash :: PasswordHash -> B.ByteString
encodePasswordHash (PasswordHash bytes) =
    bytes


encodeSalt :: Salt -> B.ByteString
encodeSalt (Salt bytes) =
    bytes


encodeArgonOptions :: Argon2.Options -> B.ByteString
encodeArgonOptions options =
    mconcat
    [ encodeUint32 $ Argon2.iterations options
    , encodeUint32 $ Argon2.memory options
    , B.singleton $ Argon2.parallelism options
    , encodeArgonVersion $ Argon2.version options
    ]


encodeArgonVersion :: Argon2.Version -> B.ByteString
encodeArgonVersion version =
    case version of
    Argon2.Version10 ->
        B.singleton 10

    Argon2.Version13 ->
        B.singleton 13


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
        { usersM = users ready
        , counterM = counter ready
        }


argonOptions :: Argon2.Options
argonOptions =
    Argon2.Options
        { iterations = 1
        , memory = 256 * 1024
        , parallelism = 4
        , variant = Argon2.Argon2id
        , version = Argon2.Version13
        }


hashPassword :: Password -> Salt -> Either T.Text PasswordHash
hashPassword (Password password) (Salt salt) =
    case Argon2.hash argonOptions password salt passwordHashLength of
    CryptoPassed hash ->
        Right $ PasswordHash hash

    CryptoFailed err ->
        Left $ T.pack $ show err


data TcpConn
    = TcpConn (Queue TcpInstruction) AuthStatus


data AuthStatus
    = Authenticated Username
    | Untrusted


data TcpMessage
    = NeedsAuthT NeedsAuth
    | FreeT Free


data Free
    = SignIn Password Username
    | NewAccount Password


newtype Password
    = Password B.ByteString


data ProofOfWork
    = ProofOfWork ProofOfWorkUnique ProofOfWorkByClient


newtype ProofOfWorkByClient
    = ProofOfWorkByClient B.ByteString


newtype ProofOfWorkUnique
    = ProofOfWorkUnique B.ByteString


data NeedsAuth
    = SendMessage Recipient B.ByteString
    | DeleteMessage Hash32
    | GetPrice
    | UploadContacts [Username]


newtype Recipient
    = Recipient Username


newtype Hash32
    = Hash32 B.ByteString


memCacheP :: P.Parser MemCache
memCacheP = do
    usersM <- mapP usernameP authP
    return $ MemCache { usersM }


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


mapP :: (Ord k) => P.Parser k -> P.Parser v -> P.Parser (Map.Map k v)
mapP kP vP = do
    len <- uint32P
    asList <- P.count len $ do
        k <- kP
        v <- vP
        return $ (k, v)
    return $ Map.fromList asList


saltLength :: Int
saltLength =
    24


saltP :: P.Parser Salt
saltP = do
    salt <- P.take saltLength
    return $ Salt salt


usernameLength :: Int
usernameLength =
    8


usernameP :: P.Parser Username
usernameP = do
    username <- P.take usernameLength
    return $ Username username


passwordHashLength :: Int
passwordHashLength =
    32


passwordHashP :: P.Parser PasswordHash
passwordHashP = do
    hash <- P.take passwordHashLength
    return $ PasswordHash hash


authP :: P.Parser Auth
authP = do
    hash <- passwordHashP
    salt <- saltP
    return $ Auth hash salt


updateOnFileContents
    :: FilePath
    -> Either E.IOException B.ByteString
    -> State
    -> (Output, State)
updateOnFileContents path result model =
    case model of
    InitS ->
        if path == memCachePath then
        let
        output = StartTcpServerO
        in
        case result of
        Left _ ->
            (output, ReadyS emptyReady)

        Right contents ->
            case P.eitherResult (P.parse memCacheP contents) of
            Left err ->
                ( DoNothingO
                , FailedS $ "corrupt memory cache: " <> T.pack err
                )

            Right cache ->
                (output, ReadyS $ Ready { users = usersM cache })

        else
        (DoNothingO, model)

    ReadyS ready ->
        case sendingMessage ready of
        NoJobs ->
            (DoNothingO, ReadyS ready)

        Jobs (GettingBilling sender recipient message) waiting ->
            if path == makeBillingPath recipient then
            case result of
            Left _ ->
                promoteSending ready

            Right contents ->
                case P.eitherResult (P.parse billingP contents) of
                Left err ->
                    ( DoNothingO
                    , FailedS $ "corrupt billing: " <> T.pack err
                    )

                Right billing ->
                    if paidUp billing then
                    ( ReadFileO $ makeWhitelistPath recipient
                    , ReadyS $
                        ready 
                        { sendingMessage =
                            Jobs
                                (GettingWhitelist
                                    sender
                                    recipient
                                    message)
                                waiting
                        }
                    )
                    else
                    promoteSending ready
            else
            (DoNothingO, ReadyS ready)

        Jobs (GettingWhitelist sender recipient message) waiting ->
            if path == makeWhitelistPath recipient then
            case result of
            Left _ ->
                promoteSending ready

            Right contents ->
                case P.eitherResult (P.parse whitelistP contents) of
                Left err ->
                    ( DoNothingO
                    , FailedS $ "corrupt whitelist: " <> T.pack err
                    )

                Right whitelist ->
                    if Set.member sender whitelist then
                    sendWantedMessage sender recipient message ready
                    else
                    promoteSending ready
            else
            (DoNothingO, ReadyS ready)

    FailedS _ ->
        (DoNothingO, model)


sendWantedMessage
    :: Sender
    -> Recipient
    -> Message
    -> Ready
    -> (Output, State)
sendWantedMessage sender recipient message ready =
    case getUserQ (tcpConns ready) recipient of
    Nothing ->
        saveMessage sender recipient message ready

    Just recipientQ ->
        forwardMessage sender recipientQ message ready


forwardMessage :: Sender -> Queue TcpInstruction


saveMessage
    :: Sender
    -> Recipient
    -> Message
    -> Ready
    -> (Output, State)
saveMessage sender recipient message ready =
    let
    (promoteO, promoteReady) = promoteSending ready
    in
    case (makeDigest message, getUserQ (tcpConns ready) sender) of
        (Nothing, _) ->
            (DoNothingO, FailedS "couldn't hash the message")

        (Just _, Nothing) ->
            (promoteO, promoteReady)

        (Just digest, Just senderQ) ->
            ( BatchO
                [ MakeDirIfNotThereO $ makeInboxPath recipient
                , WriteFileO (makeMessagePath digest) message
                , promoteO
                , acknowledgeMessage digest senderQ 
                ]
            , promoteReady
            )


type Digest
    = Hash.Digest Hash.Blake2b_256


acknowledgeMessage :: Digest -> Queue TcpInstruction -> Output
acknowledgeMessage digest senderQ =
    MsgInQO senderQ $
    Send $
    encodeToClient $
    Acknowledgement $
    ByteArray.convert digest


makeDigest :: B.ByteString -> Maybe Digest
makeDigest =
    Hash.digestFromByteString


makeInboxPath :: Recipient -> FilePath
makeInboxPath (Recipient recipient) =
    inboxesPath </> T.unpack (Hex.encodeHex recipient)


makeMessagePath :: String -> Recipient -> Maybe FilePath
makeMessagePath messageHash (Recipient recipient) =
    inboxesPath </>
    T.unpack (Hex.encodeHex recipient) </>
    messageHash

    
inboxesPath :: FilePath
inboxesPath =
    rootPath </> "inboxes"


promoteSending :: Ready -> (Output, State)
promoteSending ready =
    case sendingMessage ready of
    NoJobs ->
        (DoNothingO, ReadyS $ ready { sendingMessage = NoJobs })

    Jobs current [] ->
        (DoNothingO, ReadyS $ ready { sendingMessage = NoJobs })

    Jobs current (SendMessageWait sender recipient message:aiting) ->
        (ReadFileO (makeWhitelistPath recipient)
        , ReadyS $
            ready
                { sendingMessage =
                    Jobs
                        (GettingWhitelist sender recipient message)
                        aiting
                }
        )
        

makeWhitelistPath :: Username -> FilePath
makeWhitelistPath (Username username) =
    rootPath </> "whitelists" </> T.unpack (Hex.encodeHex username)


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
    instructionsQ <- Q.newTQueue :: Queue TcpInstruction
    writeQ msgQ $ NewTcpConnM instructionsQ address
    receiverId <- CC.forkIO $ tcpReceiver socket address
    tcpSender address receiverId instructionsQ socket


data TcpInstruction
    = Die
    | Send B.ByteString


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
            eitherOk <- E.try (Tcp.send socket msg)
            case eitherOk of
                Left _ -> do
                    writeQ msgQ $ DeadTcpM address
                    CC.killThread receiverId

                Right () ->
                    tcpSender address receiverId instructionsQ socket


tcpReceiver :: Tcp.Socket -> Tcp.SockAddr -> IO ()
tcpReceiver socket address = do
    eitherMsg <- E.try $ Tcp.recv socket maxMessageLength
    case eitherMsg of
        Left _ ->
            writeQ msgQ $ DeadTcpM address

        Right msg ->
            writeQ msgQ $ NewTcpMsgM address msg
            tcpSender socket address


maxMessageLength :: Int
maxMessageLength =
    16000


tcpReceiverHelp :: Tcp.Socket -> Tcp.SockAddr -> IO ()
tcpReceiverHelp socket address = do
    maybeMsg <- Tcp.recv socket maxMessageLength
    case maybeMsg of
        Nothing ->
            return ()

        Just msg -> do
            Stm.atomically $ do
                q <- msgQ
                Q.writeTQueue q (TcpMsgIn address msg)
            tcpReceiverHelp (socket, address)
        
        
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

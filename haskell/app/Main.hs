{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Streamly.Prelude as S
import Streamly
import qualified Control.Concurrent as C
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import qualified Network.Simple.TCP as Tcp
import qualified Control.Monad.STM as Stm
import qualified Control.Concurrent.STM.TQueue as Q
import qualified Data.Attoparsec.ByteString as P
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Handler.Warp (run)
import qualified Network.WebSockets as Ws
import qualified Network.Wai as Wai
import qualified Web.Scotty as Sc
import Data.Text.Encoding (decodeUtf8')
import Control.Monad.IO.Class (liftIO)
import Data.Word (Word8)


newtype TcpMsg
    = AuthCodeT B.ByteString
    deriving Eq


newtype UiMsg
    = SendMsg T.Text


cryptoClient :: IO ()
cryptoClient =
    Tcp.connect "localhost" "59285" $ \(socket, _) -> do
    _ <- C.forkIO $ cryptoSend socket
    cryptoReceive socket


tcpClient :: IO ()
tcpClient =
    Tcp.connect "localhost" "3001" $ \(socket, _) -> do
        _ <- C.forkIO $ tcpSend socket
        tcpReceive socket


maxServerMsg :: Int
maxServerMsg =
    16000


encodeUint32 :: Int -> B.ByteString
encodeUint32 theInt =
    let
        f accum i =
            (fromIntegral $ theInt >> (i * 8) & 0xFF) : accum
    in
        B.pack $ reverse $ foldr f [] [0..3]


tcpDelay :: Int
tcpDelay =
    10000000


onTcpReceiveErr :: T.Text -> Tcp.Socket -> IO ()
onTcpReceiveErr err socket = do
    Tio.putStrLn err
    C.threadDelay tcpDelay
    tcpReceive socket


tcpReceive ::  Tcp.Socket -> IO ()
tcpReceive socket = do
    rawLen <- Tcp.recv socket 4
    case decodeRawLen rawLen of
        Left err ->
            onTcpReceiveErr err socket

        Right len -> do
            rawMsg <- Tcp.recv socket len
            case decodeTcpMsg rawMsg of
                Left err ->
                    onTcpReceiveErr err socket

                Right msg -> do
                    Stm.atomically $ do
                        q <- inQ
                        Q.writeTQueue q (FromServerI msg)
                    tcpReceive socket


onCryptoReceiveErr :: T.Text -> Tcp.Socket -> IO ()
onCryptoReceiveErr err socket = do
    Tio.putStrLn err
    C.threadDelay tcpDelay
    cryptoReceive socket


data FromCrypto
    = EncryptedF Encrypted
    | DecryptedF Decrypted 
    | SignedF Signed
    | SigCheckF SigCheck
    | PubKeysF CryptoKeys



encryptedP :: P.Parser Encrypted
encryptedP = do
    _ <- P.word8 1
    recipient <- P.take 32
    len <- uint32P
    unencrypted <- P.take len
    encrypted <- P.takeByteString
    return $ Encrypted recipient unencrypted encrypted
    


sigCheckP :: P.Parser SigCheck
sigCheckP = do
    _ <- P.word8 4
    P.choice [fmap BadS badSigCheckP, fmap GoodS goodSigCheckP]


badSigCheckP :: P.Parser BadSig
badSigCheckP = do
    _ <- P.word8 0
    sender <- P.take 32
    signature <- P.takeByteString
    return $ BadSig sender signature


goodSigCheckP :: P.Parser GoodSig
goodSigCheckP = do
    _ <- P.word8 1
    sender <- P.take 32
    signature <- sizedBytesP
    unsigned <- P.takeByteString
    return $ GoodSig sender signature unsigned


signedP :: P.Parser Signed
signedP = do
    _ <- P.word8 3
    unsigned <- sizedBytesP
    signed <- P.takeByteString
    return $ Signed unsigned signed


decryptedP :: P.Parser Decrypted
decryptedP = do
    _ <- P.word8 2
    P.choice [fmap BadD badDecryptedP, fmap GoodD goodDecryptedP]


badDecryptedP :: P.Parser BadDecrypted
badDecryptedP = do
    _ <- P.word8 0
    sender <- P.take 32
    msg <- P.takeByteString
    return $ BadDecrypted sender msg


sizedBytesP :: P.Parser B.ByteString
sizedBytesP = do
    len <- uint32P
    P.take len


goodDecryptedP :: P.Parser GoodDecrypted
goodDecryptedP = do
    _ <- P.word8 1
    sender <- P.take 32
    encrypted <- sizedBytesP
    decrypted <- P.takeByteString
    return $ GoodDecrypted sender encrypted decrypted


fromCryptoP :: P.Parser FromCrypto
fromCryptoP =
    P.choice
        [ fmap EncryptedF encryptedP
        , fmap DecryptedF decryptedP
        , fmap SignedF signedP
        , fmap SigCheckF sigCheckP
        , fmap PubKeysF pubKeysP
        ]


decodeCryptoMsg :: Maybe B.ByteString -> Either T.Text FromCrypto
decodeCryptoMsg maybeRaw =
    case maybeRaw of
        Nothing ->
            Left "maybeRaw in decodeCryptoMsg was Nothing"

        Just bytes ->
            case P.eitherResult $ P.parse fromCryptoP bytes of
                Left err ->
                    Left $ T.pack err

                Right ok ->
                    Right ok


cryptoReceive :: Tcp.Socket -> IO ()
cryptoReceive socket = do
    rawLen <- Tcp.recv socket 4
    case decodeRawLen rawLen of
        Left err ->
            onCryptoReceiveErr err socket

        Right len -> do
            rawMsg <- Tcp.recv socket len
            case decodeCryptoMsg rawMsg of
                Left err ->
                    onCryptoReceiveErr err socket

                Right msg -> do
                    Stm.atomically $ do
                        q <- inQ
                        Q.writeTQueue q $ FromCryptoI msg
                    cryptoReceive socket


inQ :: Stm.STM (Q.TQueue In)
inQ =
    Q.newTQueue
        

data In
    = FromServerI FromServer
    | FromWebsocketI FromWebsocket
    | FromCryptoI FromCrypto


data Signed = Signed
    { beforeS :: B.ByteString
    , afterS :: B.ByteString
    }


data SigCheck
    = BadS BadSig
    | GoodS GoodSig


data BadSig = BadSig
    { senderA :: B.ByteString
    , signatureA :: B.ByteString
    }


data GoodSig = GoodSig
    { senderO :: B.ByteString
    , signatureO :: B.ByteString
    , unsignedO :: B.ByteString
    }


data Encrypted = Encrypted
    { recipientE :: B.ByteString
    , unencryptedE :: B.ByteString
    , encryptedE :: B.ByteString
    }


data Decrypted
    = BadD BadDecrypted
    | GoodD GoodDecrypted


data BadDecrypted = BadDecrypted
    { senderB :: B.ByteString
    , messageB :: B.ByteString
    }

data GoodDecrypted = GoodDecrypted
    { senderG :: B.ByteString
    , encryptedG :: B.ByteString
    , decryptedG :: B.ByteString
    }


data FromWebsocket
    = ToServerF B.ByteString
    | GetPowF PowInfo
    | CacheGetF T.Text
    | CacheSetF T.Text B.ByteString
    | CacheDeleteF T.Text
    | SendMessageF SendMessage
    
    
data SendMessage
    = SendMessage
        { recipientS :: B.ByteString
        , draftIdS :: T.Text
        }


fromWebsocketP :: P.Parser FromWebsocket
fromWebsocketP = do
    P.choice
        [ toServerP
        , GetPowF <$> powInfoP
        , cacheGetP
        , cacheSetP
        , cacheDeleteP
        , sendMessageP
        ]


cacheDeleteP :: P.Parser FromWebsocket
cacheDeleteP = do
    _ <- P.word8 4
    key <- stringP
    return $ CacheDeleteF key


cacheSetP :: P.Parser FromWebsocket
cacheSetP = do
    _ <- P.word8 3
    key <- stringP
    value <- P.takeByteString
    return $ CacheSetF key value


cacheGetP :: P.Parser FromWebsocket
cacheGetP = do
    _ <- P.word8 2
    CacheGetF <$> stringP


toServerP :: P.Parser FromWebsocket
toServerP = do
    _ <- P.word8 0
    ToServerF <$> P.takeByteString


stringP :: P.Parser T.Text
stringP = do
    len <- uint32P
    raw <- P.take len
    case decodeUtf8' raw of
        Left err ->
            fail $ show err
        Right str ->
            return str


sendMessageP :: P.Parser FromWebsocket
sendMessageP = do
    _ <- P.word8 5
    toId <- P.take 10
    draftId <- stringP
    return $ SendMessageF $ SendMessage toId draftId


uint32P :: P.Parser Int
uint32P = do
    l0 <- fmap fromIntegral P.anyWord8
    l1 <- fmap fromIntegral P.anyWord8
    l2 <- fmap fromIntegral P.anyWord8
    l3 <- fmap fromIntegral P.anyWord8
    return $ l0 + l1*256 + l2 * 256*256 + l3 * 256*256*256


data FromServer
    = KeysForIdF CryptoKeys
    | PowInfoF PowInfo


data CryptoKeys = CryptoKeys SignKey EncryptKey


newtype SignKey = SignKey B.ByteString
    

newtype EncryptKey = EncryptKey B.ByteString


data PowInfo
    = PowInfo Int B.ByteString


newtype AuthCode = AuthCode B.ByteString


pubKeysP :: P.Parser CryptoKeys
pubKeysP = do
    sign <- P.take 32
    encrypt <- P.take 32
    P.endOfInput
    return $ CryptoKeys (SignKey sign) (EncryptKey encrypt)


powInfoP :: P.Parser PowInfo
powInfoP = do
    difficulty <- fmap fromIntegral P.anyWord8
    unique <- P.take 16
    P.endOfInput
    return $ PowInfo difficulty unique


fromServerP :: P.Parser FromServer
fromServerP =
    P.choice
        [ fmap KeysForIdF pubKeysP
        , fmap PowInfoF powInfoP
        ]


decodeTcpMsg :: Maybe B.ByteString -> Either T.Text FromServer
decodeTcpMsg maybeRaw =
    case maybeRaw of
        Nothing ->
            Left "maybeRaw in decodeTcpMsg was Nothing"

        Just bytes ->
            case P.eitherResult $ P.parse fromServerP bytes of
                Left err ->
                    Left $ T.pack err

                Right ok ->
                    Right ok


decodeRawLen :: Maybe B.ByteString -> Either T.Text Int
decodeRawLen maybeRaw =
    case maybeRaw of
        Nothing ->
            Left "maybeRaw in decodeRawLen was Nothing"

        Just bytes ->
            case P.eitherResult $ P.parse uint32P bytes of
                Left err ->
                    Left $ T.pack err

                Right ok ->
                    Right ok


toCryptoQ :: Stm.STM (Q.TQueue B.ByteString)
toCryptoQ =
    Q.newTQueue


cryptoSend :: Tcp.Socket -> IO ()
cryptoSend socket = do
    out <- Stm.atomically $ do
        q <- toCryptoQ
        Q.readTQueue q
    Tcp.send socket out
    cryptoSend socket


tcpSend :: Tcp.Socket -> IO ()
tcpSend socket = do
    out <- Stm.atomically $ do
        q <- toServerQ 
        Q.readTQueue q
    Tcp.send socket out
    tcpSend socket


toServerQ :: Stm.STM (Q.TQueue B.ByteString)
toServerQ =
    Q.newTQueue


websocketPort :: Int
websocketPort =
    17448


uiServer :: IO ()
uiServer = do
    app <- scottyApp
    run websocketPort $ websocketsOr
        Ws.defaultConnectionOptions websocket app


websocket :: Ws.ServerApp
websocket pending = do
    conn <- Ws.acceptRequest pending
    websocketReceive conn
    websocketSend conn


scottyApp :: IO Wai.Application
scottyApp =
    Sc.scottyApp $
        Sc.get "/" $ Sc.file "static/index.html"


websocketReceive :: Ws.Connection -> IO ()
websocketReceive conn = do
    raw <- Ws.receiveData conn
    case P.eitherResult $ P.parse fromWebsocketP raw of
        Left err -> do
            Tio.putStrLn $ "FATAL ERROR: " <> T.pack err
            return ()

        Right msg -> do
            Stm.atomically $ do
                q <- inQ
                Q.writeTQueue q $ FromWebsocketI msg
            websocketReceive conn


toWebsocketQ :: Stm.STM (Q.TQueue B.ByteString)
toWebsocketQ =
    Q.newTQueue


websocketSend :: Ws.Connection -> IO ()
websocketSend conn = do
    out <- Stm.atomically $ do
        q <- toWebsocketQ
        Q.readTQueue q
    Ws.sendDataMessage conn $ Ws.Binary $ Bl.fromStrict out
    websocketSend conn


data CacheMsg
    = CacheMsg


inStream :: Serial In
inStream =
    S.repeatM $ Stm.atomically $ do
        q <- inQ
        Q.readTQueue q
        

isSendMsg :: In -> Maybe SendMessage
isSendMsg in_ =
    case in_ of
        FromWebsocketI (SendMessageF msg) ->
            Just msg

        _ ->
            Nothing


sendMsgRequests :: Serial SendMessage
sendMsgRequests =
    S.mapMaybe isSendMsg inStream


{-| Sending a message (a draft) requires these steps:
1. Read the draft from file
2. Chunk up the draft, and encrypt and send each chunk
3. For each blob ID in the draft, read it from file, chunk it up
   and send each chunk

There is a common pattern here. I need a function that takes a
stream of byte chunks, and then encrypts and sends each chunk.
-}
data SendErr
    = Fatal T.Text
    | Network T.Text
    | None


encrypt :: B.ByteString -> B.ByteString -> IO ()
encrypt recipient message =
    Stm.atomically $ do
        q <- toCryptoQ
        Q.writeTQueue q $ mconcat
            [ B.singleton 1
            , recipient
            , message
            ]
    

encrypted :: B.ByteString -> B.ByteString -> Serial B.ByteString
encrypted recipient chunk =
    S.mapMaybe (encryptedHelp recipient chunk) inStream


encryptedHelp
    :: B.ByteString
    -> B.ByteString
    -> In
    -> Maybe B.ByteString
encryptedHelp recipient chunk msgIn =
    case msgIn of
        FromCryptoI (EncryptedF (Encrypted recip unenc enc)) ->
            if recip == recipient && chunk == unenc then
                Just enc
            else
                Nothing

        _ ->
            Nothing
                    

makeIdToken :: Word8 -> B.ByteString -> IO B.ByteString
makeIdToken indicator message =
    undefined


sendChunk :: B.ByteString -> B.ByteString -> IO (Maybe SendErr)
sendChunk recipient chunk = do
    encrypt recipient chunk
    maybeEncrypted <- S.head $ encrypted recipient chunk
    case maybeEncrypted of
        Nothing ->
            return $ Just $ Fatal "couldn't encrypt"

        Just encrypted -> do
            idToken <- makeIdToken 4 encrypted
            _ <- Stm.atomically $ do
                q <- toServerQ
                Q.writeTQueue q $ mconcat
                    [ B.singleton 4
                    , idToken
                    , recipient
                    , encrypted
                    ]
            return Nothing


sendChunks :: Serial B.ByteString -> B.ByteString -> Serial SendErr
sendChunks chunks recipient = S.mapMaybe id $ do
    chunk <- chunks
    sent <- liftIO $ sendChunk recipient chunk
    return sent
    -- fmap (sendChunk recipient) chunks
    -- S.concatMapM (sendChunk recipient) chunks


sentMsgs :: Serial SendErr
sentMsgs =
    S.concatMap sendMsg sendMsgRequests


clientDataDir =
    "clientData"


data Blob = Blob
    { idB :: T.Text
    , mimeB :: T.Text
    , filenameB :: T.Text
    , sizeB :: Int
    }


data Code = Code
    { contentsC :: B.ByteString
    , mimeC :: T.Text
    , filenameC :: T.Text
    }


codeP :: P.Parser (Maybe Code)
codeP =
    P.choice
        [ P.word8 0 >> return Nothing
        , fmap Just someCodeP
        ]


someCodeP :: P.Parser Code
someCodeP = do
    _ <- P.word8 1
    contents <- sizedBytesP
    mime <- stringP
    filename <- stringP
    return $ Code contents mime filename



listP :: P.Parser a -> P.Parser [a]
listP p = do
    len <- uint32P
    listHelpP p len []


listHelpP :: P.Parser a -> Int -> [a] -> P.Parser [a]
listHelpP p len accum =
    if len == length accum then
        return $ reverse accum

    else do
        el <- p
        listHelpP p len (el:accum)
    
    

draftP :: P.Parser Draft
draftP = do
    draftId <- stringP
    subject <- stringP
    to <- sizedBytesP
    time <- stringP
    userInput <- stringP
    code <- codeP
    blobs <- listP blobP
    return $ Draft
        { idD = draftId
        , subjectD = subject
        , toD = to
        , timeD = time
        , userInputD = userInput
        , codeD = code
        , blobsD = blobs
        }


blobP :: P.Parser Blob
blobP = do
    blobId <- stringP
    mime <- stringP
    filename <- stringP
    size <- uint32P
    return $ Blob
        { idB = blobId
        , mimeB = mime
        , filenameB = filename
        , sizeB = size
        }


data Draft = Draft
    { idD :: T.Text
    , subjectD :: T.Text
    , toD :: B.ByteString
    , timeD :: T.Text
    , userInputD :: T.Text
    , codeD :: Maybe Code
    , blobsD :: [Blob]
    }


parseDraft :: B.ByteString -> Either T.Text Draft
parseDraft raw =
    case P.eitherResult $ P.parse draftP raw of
        Left err ->
            Left $ T.pack err

        Right parsed ->
            Right parsed


chunkDraft :: B.ByteString -> Serial B.ByteString
chunkDraft raw =
    if B.length raw > maxChunkSize then
        S.yield $ B.singleton 0 <> raw
    else
        S.fromList $ chunkDraftHelp (Sha256.hash raw) raw []


maxChunkSize = 15500


chunkDraftHelp
    :: B.ByteString
    -> B.ByteString
    -> [B.ByteString]
    -> [B.ByteString]
chunkDraftHelp hash raw accum =
    if B.length raw > maxChunkSize then
        chunkDraftHelp hash (B.drop maxchunkSize raw) $
            (mconcat
                [ B.singleton 1
                , hash
                , encodeUint32 $ length accum
                , B.take maxChunkSize raw
                ]) : accum

    else
        accum


sendBlob :: B.ByteString -> Blob -> Serial SendErr
sendBlob = undefined


sendMsg :: SendMessage -> Serial SendErr
sendMsg (SendMessage recipient draftId) = do
    raw <- liftIO $ B.readFile $ T.unpack $
        clientDataDir <> "/" <> draftId
    case parseDraft raw of
        Left err ->
            return $ Fatal $ "could not parse draft on disk: " <> err

        Right draft ->
             sendChunks (chunkDraft raw) recipient <>
             mconcat (fmap (sendBlob recipient) (blobsD draft))


main :: IO ()
main = do
    _ <- C.forkIO tcpClient
    _ <- C.forkIO uiServer
    _ <- C.forkIO cryptoClient
    return ()

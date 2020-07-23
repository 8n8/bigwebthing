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
import qualified Control.Concurrent.STM as Stm
import qualified Control.Concurrent.STM.TVar as TVar
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
import qualified Crypto.Hash.SHA256 as Sha256
import qualified Data.Set as Set
import qualified System.Directory as Fs
import Data.Bits ((.&.), shiftR)
import Streamly.Internal.FileSystem.File (toChunksWithBufferOf)
import qualified Network.HTTP.Req as R
import Streamly.Internal.Memory.Array.Types (toList)


tcpClient :: IO ()
tcpClient =
    Tcp.connect "localhost" "3001" $ \(socket, _) -> do
        tcpAuth socket
        tcpReceive socket


encodeUint32 :: Int -> B.ByteString
encodeUint32 theInt =
    let
        f :: Int -> [Word8] -> [Word8]
        f i accum =
            (fromIntegral $ theInt `shiftR` (i * 8) .&. 0xFF) : accum
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


tcpAuthMeaning :: B.ByteString
tcpAuthMeaning =
    B.pack
        [ 0x6b, 0x87, 0x4c, 0xde, 0xcc, 0xf0, 0x28, 0xb3
        , 0x7c, 0x4e, 0xde, 0xee, 0x15, 0xca, 0x92, 0x93
        ]


tcpAuth :: Tcp.Socket -> IO ()
tcpAuth socket = do
    authCode <- getAuthCode
    signed <- signMessage $ tcpAuthMeaning <> authCode
    myId <- getMyId
    Tcp.send socket $ myId <> signed


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


sizedBytesP :: P.Parser B.ByteString
sizedBytesP = do
    len <- uint32P
    P.take len


inQ :: Stm.STM (Q.TQueue In)
inQ =
    Q.newTQueue


data In
    = FromServerI FromServer


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


stringP :: P.Parser T.Text
stringP = do
    len <- uint32P
    raw <- P.take len
    case decodeUtf8' raw of
        Left err ->
            fail $ show err
        Right str ->
            return str


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
    encrypt_ <- P.take 32
    P.endOfInput
    return $ CryptoKeys (SignKey sign) (EncryptKey encrypt_)


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


uiServer :: IO ()
uiServer = do
    app <- httpApi
    run 11833 $ websocketsOr
        Ws.defaultConnectionOptions websocket app


filePath :: T.Text -> T.Text
filePath filename =
    clientDataDir <> "/" <> filename


lockFile :: T.Text -> IO ()
lockFile filename =
    Stm.atomically $ do
        var <- fileLocks
        locks <- TVar.readTVar var
        TVar.writeTVar var $ Set.insert filename locks


unlockFile :: T.Text -> IO ()
unlockFile filename =
    Stm.atomically $ do
        var <- fileLocks
        locks <- TVar.readTVar var
        TVar.writeTVar var $ Set.delete filename locks


httpApi :: IO Wai.Application
httpApi =
    Sc.scottyApp $ do
        Sc.get "/static/:requested" $ do
            requested <- Sc.param "requested"
            Sc.file $ "static/" ++ requested

        Sc.get "/cache/get/:key" $ do
            key <- Sc.param "key"
            contents <- liftIO $ do
                _ <- lockFile key
                contents <- B.readFile $ T.unpack $ filePath key
                _ <- unlockFile key
                return contents
            Sc.raw $ Bl.fromStrict contents

        Sc.post "/cache/set/:key" $ do
            key <- Sc.param "key"
            body <- fmap Bl.toStrict Sc.body
            liftIO $ do
                _ <- lockFile key
                B.writeFile (T.unpack $ filePath key) body
                unlockFile key

        Sc.post "/cache/delete/:key" $ do
            key <- Sc.param "key"
            liftIO $ do
                _ <- lockFile key
                Fs.removeFile $ T.unpack $ filePath key
                unlockFile key

        Sc.post "/sendmessage/:draftid" $ do
            draftId <- Sc.param "draftid"
            recipient <- fmap Bl.toStrict Sc.body
            liftIO $ S.drain $ sendMessage draftId recipient

        Sc.post "/whitelist/add" $ do
            whitelistee <- fmap Bl.toStrict Sc.body
            liftIO $ whitelist whitelistee

        Sc.post "/whitelist/remove" $ do
            whitelistee <- fmap Bl.toStrict Sc.body
            liftIO $ unwhitelist whitelistee


getProofOfWork :: IO B.ByteString
getProofOfWork = do
    info <- R.runReq R.defaultHttpConfig $ do
        bs <- R.req
            R.GET
            (R.http serverUrl R./: "proofofworkinfo")
            R.NoReqBody
            R.bsResponse
            mempty
        return $ R.responseBody bs

    R.runReq R.defaultHttpConfig $ do
        bs <- R.req
            R.POST
            (R.http cryptoUrl R./: "proofofwork")
            (R.ReqBodyBs info)
            R.bsResponse
            mempty
        return $ R.responseBody bs


whitelistMeaning :: B.ByteString
whitelistMeaning =
    B.pack
        [ 0xdf, 0xc2, 0xfb, 0x02, 0xba, 0x19, 0xfd, 0x38
        , 0x80, 0xfc, 0x93, 0xca, 0xd6, 0xf6, 0x37, 0x33
        ]


unwhitelistMeaning :: B.ByteString
unwhitelistMeaning =
    B.pack
        [ 0x32, 0x52, 0x4c, 0xa3, 0x77, 0xa2, 0x86, 0x0e
        , 0x8b, 0xec, 0xdf, 0xe4, 0x23, 0xae, 0xf1, 0x8f
        ]


whitelist :: B.ByteString -> IO ()
whitelist whitelistee = do
    proofOfWork <- getProofOfWork
    myId <- getMyId
    authCode <- getAuthCode
    signed <- signMessage $ mconcat
        [ whitelistMeaning
        , authCode
        , whitelistee
        ]
    _ <- R.runReq R.defaultHttpConfig $ R.req
        R.POST
        (R.http serverUrl R./: "whitelist" R./: "add")
        (R.ReqBodyBs $ proofOfWork <> myId <> signed)
        R.ignoreResponse
        mempty
    return ()


unwhitelist :: B.ByteString -> IO ()
unwhitelist unwhitelistee = do
    myId <- getMyId
    authCode <- getAuthCode
    signed <- signMessage $ mconcat
        [ unwhitelistMeaning
        , authCode
        , unwhitelistee
        ]
    _ <- R.runReq R.defaultHttpConfig $ R.req
        R.POST
        (R.http serverUrl R./: "whitelist" R./: "remove")
        (R.ReqBodyBs $ myId <> signed)
        R.ignoreResponse
        mempty
    return ()


fileLocks :: Stm.STM (TVar.TVar (Set.Set T.Text))
fileLocks =
    TVar.newTVar Set.empty


websocket :: Ws.ServerApp
websocket pending = do
    conn <- Ws.acceptRequest pending
    websocketSend conn


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


cryptoUrl :: T.Text
cryptoUrl =
    "localhost:59285"


encryptMessage :: B.ByteString -> B.ByteString -> IO B.ByteString
encryptMessage recipientID message =
    R.runReq R.defaultHttpConfig $ do
        bs <- R.req
            R.POST
            (R.http cryptoUrl R./: "encrypt")
            (R.ReqBodyBs $ recipientID <> message)
            R.bsResponse
            mempty
        return $ R.responseBody bs


myIdTVar :: Stm.STM (Stm.TVar (Maybe B.ByteString))
myIdTVar =
    TVar.newTVar Nothing


getMyId :: IO B.ByteString
getMyId = do
    maybeId <- Stm.atomically $ do
        tvar <- myIdTVar
        TVar.readTVar tvar
    case maybeId of
        Just myId ->
            return myId

        Nothing -> do
            rawKeys <- R.runReq R.defaultHttpConfig $ do
                        bs <- R.req
                            R.GET
                            (R.http cryptoUrl R./: "getmykeys")
                            R.NoReqBody
                            R.bsResponse
                            mempty
                        return $ R.responseBody bs

            case P.eitherResult $ P.parse keysP rawKeys of
                Left err -> do
                    fail $ show err

                Right keys -> do
                    Stm.atomically $ do
                        tvar <- myKeysTVar
                        TVar.writeTVar tvar $ Just keys
                    myId <- makeUserId keys
                    Stm.atomically $ do
                        tvar <- myIdTVar
                        TVar.writeTVar tvar $ Just myId
                    return myId


myKeysTVar :: Stm.STM (TVar.TVar (Maybe Keys))
myKeysTVar =
    TVar.newTVar Nothing


makeUserId :: Keys -> IO B.ByteString
makeUserId keys =
    R.runReq R.defaultHttpConfig $ do
        bs <- R.req
            R.POST
            (R.http cryptoUrl R./: "userid")
            (R.ReqBodyBs $ encodeKeys keys)
            R.bsResponse
            mempty
        return $ R.responseBody bs


encodeKeys :: Keys -> B.ByteString
encodeKeys keys =
    signK keys <> encryptK keys


serverUrl :: T.Text
serverUrl =
    "localhost"


sendMessageMeaning :: B.ByteString
sendMessageMeaning =
    B.pack
        [ 0x0a, 0xcb, 0x78, 0x89, 0x67, 0xcf, 0x64, 0x19
        , 0x2a, 0xdd, 0x32, 0x63, 0x61, 0x2d, 0x10, 0x18
        ]


signMessage :: B.ByteString -> IO B.ByteString
signMessage message =
    R.runReq R.defaultHttpConfig $ do
        bs <- R.req
            R.POST
            (R.http cryptoUrl R./: "sign")
            (R.ReqBodyBs message)
            R.bsResponse
            mempty
        return $ R.responseBody bs


getAuthCode :: IO B.ByteString
getAuthCode =
    R.runReq R.defaultHttpConfig $ do
        bs <- R.req
            R.GET
            (R.http serverUrl R./: "authcode")
            R.NoReqBody
            R.bsResponse
            mempty
        return $ R.responseBody bs


sendChunk :: B.ByteString -> B.ByteString -> IO ()
sendChunk recipient chunk = do
    myId <- getMyId
    authCode <- getAuthCode
    encrypted <- encryptMessage recipient chunk
    signed <- signMessage $ mconcat
        [ sendMessageMeaning
        , authCode
        , recipient
        , encrypted
        ]

    _ <- R.runReq R.defaultHttpConfig $
        R.req
            R.POST
            (R.http serverUrl R./: "message" R./: "send")
            (R.ReqBodyBs $ myId <> signed)
            R.ignoreResponse
            mempty

    return ()


sendChunks :: Serial B.ByteString -> B.ByteString -> Serial ()
sendChunks chunks recipient = do
    chunk <- chunks
    liftIO $ sendChunk recipient chunk


clientDataDir :: T.Text
clientDataDir =
    "clientData"


data Blob = Blob
    { mimeB :: T.Text
    , idB :: T.Text
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


data Keys = Keys
    { signK :: B.ByteString
    , encryptK :: B.ByteString
    }


keysP :: P.Parser Keys
keysP = do
    sign <- P.take 32
    encrypt <- P.take 32
    return $ Keys sign encrypt


maxChunkSize :: Int
maxChunkSize =
    15500


chunkDraftHelp
    :: B.ByteString
    -> B.ByteString
    -> [B.ByteString]
    -> [B.ByteString]
chunkDraftHelp hash raw accum =
    if B.length raw > maxChunkSize then
        chunkDraftHelp hash (B.drop maxChunkSize raw) $
            (mconcat
                [ B.singleton 1
                , hash
                , encodeUint32 $ length accum
                , B.take maxChunkSize raw
                ]) : accum

    else
        accum


blobStream :: T.Text -> Serial B.ByteString
blobStream filepath = do
    fmap (B.pack . toList) $ toChunksWithBufferOf maxChunkSize $
        T.unpack filepath


hashFile :: T.Text -> IO B.ByteString
hashFile filepath = do
    _ <- lockFile filepath
    ctx <- S.foldr hashFileHelp Sha256.init $ blobStream filepath
    _ <- unlockFile filepath
    return $ Sha256.finalize ctx


hashFileHelp :: B.ByteString -> Sha256.Ctx -> Sha256.Ctx
hashFileHelp chunk ctx =
    Sha256.update ctx chunk


sendBigBlob :: B.ByteString -> Blob -> Serial ()
sendBigBlob recipient blob = do
    hash <- liftIO $ hashFile $ filePath $ filenameB blob

    (chunk, counter) <- S.zipWith (\a b -> (a, b))
        (blobStream $ filenameB blob) (S.fromList [0..])

    encrypted <- liftIO $ encryptMessage recipient $ mconcat
        [ B.singleton 3
        , hash
        , encodeUint32 counter
        , chunk
        ]

    liftIO $ sendChunk recipient encrypted


sendSmallBlob :: B.ByteString -> Blob -> IO ()
sendSmallBlob recipient blob = do
    let path = filePath $ filenameB blob
    _ <- lockFile path
    raw <- B.readFile $ T.unpack $ path
    _ <- unlockFile path
    encrypted <- liftIO $ encryptMessage recipient $
        B.singleton 2 <> raw
    sendChunk recipient encrypted


sendBlob :: B.ByteString -> Blob -> Serial ()
sendBlob recipient blob =
    if sizeB blob > maxChunkSize then do
        _ <- liftIO $ lockFile $ filenameB blob
        _ <- sendBigBlob recipient blob
        liftIO $ unlockFile $ filenameB blob

    else
        S.yieldM $ sendSmallBlob recipient blob


sendMessage :: T.Text -> B.ByteString -> Serial ()
sendMessage draftId recipient = do
    raw <- liftIO $ B.readFile $ T.unpack $
        clientDataDir <> "/" <> draftId
    case parseDraft raw of
        Left err -> 
            liftIO $ Tio.putStrLn err

        Right draft ->
             sendChunks (chunkDraft raw) recipient <>
             mconcat (fmap (sendBlob recipient) (blobsD draft))


main :: IO ()
main = do
    _ <- C.forkIO tcpClient
    _ <- C.forkIO uiServer

    return ()

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main (main) where

import qualified Control.Exception as E
import qualified Control.Concurrent.STM as Stm
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Handler.Warp (run)
import qualified Control.Concurrent.STM.TQueue as Q
import Crypto.Noise.DH.Curve25519 (Curve25519)
import Crypto.Noise.Hash.BLAKE2s (BLAKE2s)
import Crypto.Noise.Cipher.ChaChaPoly1305 (ChaChaPoly1305)
import Crypto.Noise.HandshakePatterns (noiseKK)
import qualified Crypto.Noise.DH as Dh
import qualified Crypto.Noise as Noise
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as Bl
import qualified System.Directory as Dir
import qualified Data.Text as T
import System.FilePath ((</>))
import qualified Data.Attoparsec.ByteString as P
import qualified Graphics.UI.Webviewhs as Wv
import qualified Web.Scotty as Sc
import qualified Network.Wai as Wai
import qualified Network.WebSockets as Ws
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.Hash.BLAKE2.BLAKE2b as Blake
import qualified Crypto.Cipher.ChaChaPoly1305 as ChaPoly
import qualified System.IO as Io
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as Bc
import Data.Word (Word8)
import Control.Concurrent (forkIO, threadDelay)
import qualified Text.Hex
import qualified Network.Simple.TCP as Tcp
import qualified Data.Map as Map
import Control.Exception.Base (SomeException)
import qualified Data.Time.Clock as Clock
import Data.Text.Encoding (encodeUtf8, decodeUtf8')
import qualified Data.Bits as Bits
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Crypto.KDF.Argon2 as Argon2
import qualified Crypto.Random as CryptoRand
import qualified Crypto.Hash as Hash
import qualified Data.ByteArray as Ba
import qualified Data.Set as Set
import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Hydrogen as H
import qualified Data.IntSet as IntSet


main :: IO ()
main =
    mainHelp (InitS EmptyI) StartM


msgQ :: Stm.STM (Q.TQueue Msg)
msgQ =
    Q.newTQueue


mainHelp :: State -> Msg -> IO ()
mainHelp oldState oldMsg = do
    let (output, newState) = update oldState oldMsg
    maybeMsg <- io output
    newMsg <- case maybeMsg of
        Nothing ->
            Stm.atomically $ do
                q <- msgQ
                Q.readTQueue q
        Just m ->
            return m
    case newState of
        FinishedS ->
            return ()

        _ ->
            mainHelp newState newMsg


secretKeyPath =
    "~/.bigwebthingSECRET"


io :: Output -> IO (Maybe Msg)
io output =
    case output of
    ReadSecretKeyO -> do
        raw <- E.try $ B.readFile secretKeyPath
        return $ Just $ SecretKeyFileM raw

    DoNothingO ->
        return Nothing

    BatchO outputs -> do
        inputs <- mapM io outputs
        return $ Just $ BatchM inputs

    BytesInQO q bytes -> do
        Stm.atomically $ do
            q_ <- q
            Q.writeTQueue q_ bytes
        return Nothing

    StartTcpClientO -> do
        tcpClient
        return Nothing


maxMessageLength =
    16000


tcpClient =
    Tcp.connect serverUrl serverPort $ \(conn, _) -> do
        _ <- forkIO $ tcpListen conn
        tcpSend conn


tcpSend :: Tcp.Socket -> IO ()
tcpSend conn = do
    msg <- Stm.atomically $ do
       q <- toServerQ
       Q.readTQueue q
    Tcp.send conn msg
    tcpSend conn


restartTcp :: IO ()
restartTcp = do
    Stm.atomically $ do
        q <- msgQ
        Stm.writeTQueue q RestartingTcpM
    threadDelay tcpDelay
    tcpClient


tcpListen :: Tcp.Socket -> IO ()
tcpListen conn = do
    maybeRawLength <- Tcp.recv conn 2
    case maybeRawLength of
        Nothing ->
            restartTcp

        Just rawLength ->
            case parseLength rawLength of
            Left err ->
                error err

            Right len ->
                if len > maxMessageLength then
                error $
                    "TCP too long: " ++ show len
                else do
                    maybeMessage <- Tcp.recv conn len
                    case maybeMessage of
                        Nothing ->
                            restartTcp

                        Just message -> do
                            Stm.atomically $ do
                                q <- msgQ
                                Q.writeTQueue q $ FromServerM $ message
                            tcpListen conn


tcpDelay =
    20 * 1000000


serverUrl =
    "http://localhost"


serverPort =
    "11453"

data Output
    = GetArgsO
    | GenerateSecretKeyO
    | ReadSecretKeyO
    | PrintO T.Text
    | DoNothingO
    | ReadFileO FilePath
    | BatchO [Output]
    | StartTcpClientO
    | BytesInQO Q B.ByteString
    | WriteFileO FilePath B.ByteString
    | ReadFileStrictO FilePath


data Msg
    = StartM
    | SecretKeyFileM (Either E.IOException B.ByteString)
    | BatchM [Maybe Msg]
    | FromServerM B.ByteString
    | RestartingTcpM


data State
    = ReadyS Ready
    | InitS Init
    | FinishedS


data Init
    = EmptyI
    | GettingKeysFromFileI
    | GeneratingSecretKeyI
    | ReadingArgsI Ed.SecretKey


data Ready
    = Ready
        { secretKey :: Ed.SecretKey
        , loggedIn :: Bool
        }


type Q
    = Stm.STM (Q.TQueue B.ByteString)


updateReady :: State -> (Ready -> (Output, State)) -> (Output, State)
updateReady model f =
    case model of
    InitS _ ->
        (DoNothingO, model)

    ReadyS ready ->
        f ready


updateInit :: State -> (Init -> (Output, State)) -> (Output, State)
updateInit model f =
    case model of
    InitS init_ ->
        f init_

    ReadyS _ ->
        (DoNothingO, model)


update :: State -> Msg -> (Output, State)
update model msg =
    case msg of
    StartM ->
        ( BatchO [StartTcpClientO, ReadSecretKeyO]
        , InitS EmptyI
        )

    BatchM msgs ->
        let
        (outputs, newModel) = batchUpdate model msgs []
        in
        (BatchO outputs, newModel)

    SecretKeyFileM (Left ioErr) ->
        (GenerateSecretKeyO, InitS GeneratingSecretKeyI)

    SecretKeyFileM (Right raw) ->
        case model of
        InitS GettingKeysFromFileI ->
            case P.eitherResult $ P.parse parseCrypto raw of
            Left err ->
                ( PrintO $
                  mconcat
                  [ "internal error: corrupted secret key file: \n"
                  , T.pack err
                  , ": \n"
                  , Text.Hex.encodeHex raw
                  ] 
                , FinishedS
                )

            Right key ->
                (GetArgsO, InitS $ ReadingArgsI key)

    FromServerM raw ->
        updateReady model $ fromServerUpdate raw

    RestartingTcpM ->
        updateReady model restartingTcpUpdate


-- Assocated data
authCodeA :: B.ByteString
authCodeA =
    B.pack [197, 154, 22, 2, 21, 159, 38, 105, 240, 15, 236, 142, 31, 124, 100, 71, 22, 117, 69, 163, 39, 221, 135, 100, 193, 244, 134, 63, 28, 226, 89, 31]


encodeUint32 :: Int -> B.ByteString
encodeUint32 i =
    B.pack $ map (encodeUintHelp i) (take 4 [0..])


encodeUintHelp :: Int -> Int -> Word8
encodeUintHelp int counter =
    fromIntegral $ int `Bits.shiftR` (counter * 8) Bits..&. 0xFF


encodeUint16 :: Int -> B.ByteString
encodeUint16 i =
    B.pack $ map (encodeUintHelp i) [0, 1]


sendToServer :: ToServer -> Output
sendToServer msg =
    BytesInQO toServerQ $ encodeToServer msg


data ToServer
    = SignedAuthCodeT Ed.PublicKey Ed.Signature
    | SendMessageT Ed.PublicKey T.Text
    | GetMessage


newtype MyId
    = MyId Ed.PublicKey


newtype TheirId
    = TheirId Ed.PublicKey


signPubKeyP :: P.Parser Ed.PublicKey
signPubKeyP = do
    raw <- P.take Ed.publicKeySize
    case Ed.publicKey raw of
        CryptoFailed err ->
            fail $ show err

        CryptoPassed key ->
            return key


encodeToServer :: ToServer -> B.ByteString
encodeToServer toServer =
    case toServer of
    SignedAuthCodeT publicKey signature ->
        mconcat
        [ B.singleton 0
        , Ba.convert publicKey
        , Ba.convert signature
        ]

    SendMessageT recipient message ->
        mconcat
        [ B.singleton 1
        , Ba.convert recipient
        , encodeUtf8 message
        ]


encodeSignature :: Ed.Signature -> B.ByteString
encodeSignature sig =
    Ba.convert sig


newtype InboxMessage
    = InboxMessage T.Text


restartingTcpUpdate :: Ready -> (Output, State)
restartingTcpUpdate ready =
    ( DoNothingO
    , ReadyS $ ready { loggedIn = False }
    )


fromServerUpdate :: B.ByteString -> Ready -> (Output, State)
fromServerUpdate raw ready =
    case P.eitherResult $ P.parse fromServerP raw of
    Left err ->
        ( PrintO $
          mconcat
          [ "Received bad message from server:\n"
          , Text.Hex.encodeHex raw
          , T.pack err
          ]
        , FinishedS
        ) 

    Right (InboxMessageF sender message) ->
        case decodeUtf8' $ B64.encodeUnpadded $ Ba.convert sender of
        Left err ->
            ( PrintO $
              mconcat
              [ "internal error: could not decode Base64 ByteString:\n"
              , T.pack $ show err
              , ":\n"
              , Text.Hex.encodeHex $ Ba.convert sender
              ]
            , FinishedS
            )

        Right b64 ->
            (PrintO $ b64 <> ": " <> message, FinishedS)

    Right (AuthCodeToSignF (AuthCode authCode)) ->
            let
            publicKey = Ed.toPublic $ secretKey ready
            toSign = authCodeA <> authCode
            signature = Ed.sign (secretKey ready) publicKey toSign
            in
            ( sendToServer $ SignedAuthCodeT publicKey signature
            , ReadyS ready
            )
        

encodeSizedString :: T.Text -> B.ByteString
encodeSizedString string =
    let
    encoded = encodeUtf8 string
    in
    encodeUint32 (B.length encoded) <> encoded


allRight :: [Either a b] -> Either a [b]
allRight eithers =
    allRightHelp eithers []


allRightHelp :: [Either a b] -> [b] -> Either a [b]
allRightHelp eithers accum =
    case eithers of
    [] ->
        Right $ reverse accum

    Left l : _ ->
        Left l

    Right r : remains ->
        allRightHelp remains (r : accum)


toServerQ :: Q
toServerQ =
    Q.newTQueue


fromServerP :: P.Parser FromServer
fromServerP = do
    msg <- P.choice
        [ do
            _ <- P.word8 0
            authCode <- authCodeP
            return $ AuthCodeToSignF authCode
        , do
            _ <- P.word8 5
            sender <- publicKeyP
            inboxMessage <- inboxMessageP
            return $ InboxMessageF sender inboxMessage
        ]
    P.endOfInput
    return msg


newtype Sender
    = Sender Ed.PublicKey


senderP :: P.Parser Sender
senderP = do
    raw <- publicKeyP
    return $ Sender raw


authCodeLength :: Int
authCodeLength =
    32


authCodeP :: P.Parser AuthCode
authCodeP = do
    raw <- P.take authCodeLength
    return $ AuthCode raw


inboxMessageP :: P.Parser T.Text
inboxMessageP = do
    _ <- P.word8 1
    raw <- P.scan 0 msgScanner
    case decodeUtf8' raw of
        Left err ->
            fail $ show err

        Right valid ->
            return valid


msgScanner :: Int -> Word8 -> Maybe Int
msgScanner counter byte =
    if counter < 100 then
    if validByte byte then
    Just $ counter + 1
    else
    Nothing
    else
    Nothing


validBytes :: IntSet.IntSet
validBytes =
    IntSet.fromList
        [ 32 -- space
        , 33 -- !
        , 34 -- "
        , 35 -- #
        , 36 -- $
        , 37 -- %
        , 38 -- &
        , 39 -- '
        , 40 -- (
        , 41 -- )
        , 42 -- *
        , 43 -- +
        , 44 -- ,
        , 45 -- -
        , 46 -- .
        , 47 -- /
        , 48 -- 0
        , 49 -- 1
        , 50 -- 2
        , 51 -- 3
        , 52 -- 4
        , 53 -- 5
        , 54 -- 6
        , 55 -- 7
        , 56 -- 8
        , 57 -- 9
        , 58 -- :
        , 59 -- ;
        , 60 -- <
        , 61 -- =
        , 62 -- >
        , 63 -- ?
        , 64 -- @
        , 65 -- A
        , 66 -- B
        , 67 -- C
        , 68 -- D
        , 69 -- E
        , 70 -- F
        , 71 -- G
        , 72 -- H
        , 73 -- I
        , 74 -- J
        , 75 -- K
        , 76 -- J
        , 77 -- M
        , 78 -- N
        , 79 -- O
        , 80 -- P
        , 81 -- Q
        , 82 -- R
        , 83 -- S
        , 84 -- T
        , 85 -- U
        , 86 -- V
        , 87 -- W
        , 88 -- X
        , 89 -- Y
        , 90 -- Z
        , 91 -- [
        , 92 -- \
        , 93 -- ]
        , 94 -- ^
        , 95 -- _
        -- 96 is backtick, which I don't want
        , 97 -- a
        , 98 -- b
        , 99 -- c
        , 100 -- d
        , 101 -- e
        , 102 -- f
        , 103 -- g
        , 104 -- h
        , 105 -- i
        , 106 -- j
        , 107 -- k
        , 108 -- l
        , 109 -- m
        , 110 -- n
        , 111 -- o
        , 112 -- p
        , 113 -- q
        , 114 -- r
        , 115 -- s
        , 116 -- t
        , 117 -- u
        , 118 -- v
        , 119 -- w
        , 120 -- x
        , 121 -- y
        , 122 -- z
        , 123 -- {
        , 124 -- |
        , 125 -- }
        , 126 -- ~
        ]


validByte :: Word8 -> Bool
validByte byte =
    IntSet.member (fromIntegral byte) validBytes


publicKeyP :: P.Parser Ed.PublicKey
publicKeyP = do
    raw <- P.take Ed.publicKeySize
    case Ed.publicKey raw of
        CryptoFailed err ->
            fail $ show err

        CryptoPassed key ->
            return key


data FromServer
    = AuthCodeToSignF AuthCode
    | InboxMessageF Ed.PublicKey T.Text


newtype AuthCode
    = AuthCode B.ByteString


allJusts :: [Maybe a] -> Maybe [a]
allJusts maybes =
    allJustsHelp maybes []


allJustsHelp :: [Maybe a ] -> [a] -> Maybe [a]
allJustsHelp maybes justs =
    case maybes of
    [] ->
        Just justs

    Nothing:_ ->
        Nothing

    Just m:aybes ->
        allJustsHelp aybes (m:justs)


allOk :: [Either a b] -> Either a [b]
allOk eithers =
    allOkHelp eithers []


allOkHelp :: [Either a b] -> [b] -> Either a [b]
allOkHelp eithers accum =
    case eithers of
    [] ->
        Right accum

    Left a : _ ->
        Left a

    Right b : ithers ->
        allOkHelp ithers (b:accum)


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


encodeVarInt :: Integer -> B.ByteString
encodeVarInt i =
    let
    wds = encodeVarIntHelp i []
    len = fromIntegral $ length wds
    in
    B.singleton len <> B.pack wds


encodeVarIntHelp :: Integer -> [Word8] -> [Word8]
encodeVarIntHelp remaining accum =
    if remaining == 0 then
    accum
    else 
    encodeVarIntHelp
        (remaining `Bits.shiftR` 8)
        (fromIntegral (remaining Bits..&. 0xFF) : accum)


varIntP :: P.Parser Integer
varIntP = do
    len <- P.anyWord8
    varIntHelpP len 0 0

varIntHelpP :: Word8 -> Integer -> Integer -> P.Parser Integer
varIntHelpP len sofar counter =
    if fromIntegral counter == len then
    return sofar

    else do
        word <- P.anyWord8
        varIntHelpP
            len
            (sofar + (fromIntegral word) * (256 ^ counter))
            (counter + 1)


secretSigningP :: P.Parser Ed.SecretKey
secretSigningP = do
    raw <- P.take Ed.secretKeySize
    case Ed.secretKey raw of
        CryptoFailed err ->
            fail $ show err

        CryptoPassed key ->
            return key


uint32P :: P.Parser Int
uint32P = do
    b0 <- uint8P
    b1 <- uint8P
    b2 <- uint8P
    b3 <- uint8P
    return $ b0 + b1 * 256 + b2 * 256 * 256 + b3 * 256 * 256 * 256


uint16P :: P.Parser Int
uint16P = do
    b0 <- uint8P
    b1 <- uint8P
    return $ b0 + b1 * 256


parseLength :: B.ByteString -> Either String Int
parseLength bytes2 =
    P.eitherResult $ P.parse uint16P bytes2


uint8P :: P.Parser Int
uint8P =
    fromIntegral <$> P.anyWord8


parseCrypto :: P.Parser Ed.SecretKey
parseCrypto = do
    raw <- P.take Ed.secretKeySize
    case Ed.secretKey raw of
        CryptoFailed err ->
            fail $ show err

        CryptoPassed key ->
            return key

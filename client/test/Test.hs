import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Hedgehog as Th
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Hedgehog as H
import qualified Lib as L
import qualified Data.Attoparsec.ByteString as P
import Crypto.Error (CryptoFailable(CryptoFailed, CryptoPassed))
import qualified Crypto.PubKey.Ed25519 as Ed

main :: IO ()
main =
    Tasty.defaultMain properties


properties :: Tasty.TestTree
properties =
    Tasty.testGroup ""
    [ Th.testProperty "reverse" propReverse
    , Th.testProperty "parseRecipient" parseRecipient
    , Th.testProperty "uint16parseDecode" uint16parseDecode
    , Th.testProperty "toServerEncoder" toServerEncoder
    ]



uint16parseDecode :: H.Property
uint16parseDecode =
    H.property $ do
        n <- H.forAll $ Gen.int (Range.constant 0 65535)
        case P.parseOnly L.uint16P (L.encodeUint16 n) of
            Left _ ->
                H.failure

            Right n1 ->
                n H.=== n1


toServerG :: H.Gen L.ToServer
toServerG =
    P.choice
    [ do
        key <- keyG
        rawSig <- Gen.bytes (Range.singleton Ed.signatureSize)
        sig <- case Ed.signature rawSig of
            CryptoFailed err ->
                fail $ show err

            CryptoPassed sig ->
                return sig

        return $ L.SignedAuthCodeT key sig

    , do
        key <- keyG
        txt <- Gen.text (Range.constant 1 100) Gen.unicode
        return $ L.SendMessageT key txt
    , return L.GetMessageT
    ]


keyG = do
    raw <- Gen.bytes (Range.singleton Ed.publicKeySize)
    case Ed.publicKey raw of
        CryptoFailed err ->
            fail $ show err

        CryptoPassed key ->
            return key


toServerEncoder :: H.Property
toServerEncoder =
    H.property $ do
        to <- H.forAll toServerG
        case P.parseOnly toServerP (L.encodeToServer to) of
            Left _ ->
                H.failure

            Right ok ->
                ok H.=== to


toServerP :: P.Parser L.ToServer
toServerP = do
    len <- L.uint16P
    raw <- P.take len
    case P.parseOnly toServerHelpP raw of
        Left err ->
            fail err

        Right ok ->
            return ok


toServerHelpP :: P.Parser L.ToServer
toServerHelpP = do
    msg <- P.choice
        [ do
            _ <- P.word8 0
            key <- L.publicKeyP
            sig <- signatureP
            return $ L.SignedAuthCodeT key sig
        , do
            _ <- P.word8 1
            key <- L.publicKeyP
            message <- L.inboxMessageP
            return $ L.SendMessageT key message
        , do
            _ <- P.word8 2
            return L.GetMessageT
        ]
    P.endOfInput
    return msg


signatureP :: P.Parser Ed.Signature
signatureP = do
    raw <- P.take Ed.signatureSize
    case Ed.signature raw of
        CryptoFailed err ->
            fail $ show err

        CryptoPassed sig ->
            return sig


parseRecipient :: H.Property
parseRecipient =
    H.property $ do
        raw <- H.forAll $ Gen.string (Range.linear 44 100) Gen.unicode
        case L.parseRecipient raw of
            Left _ ->
                return ()

            Right _ ->
                H.failure

        raw2 <- H.forAll $ Gen.string ( Range.linear 0 42) Gen.unicode
        case L.parseRecipient raw2 of
            Left _ ->
                return ()

            Right _ ->
                H.failure


propReverse :: H.Property
propReverse =
    H.property $ do
        xs <- H.forAll $ Gen.list (Range.linear 0 100) Gen.alpha
        reverse (reverse xs) H.=== xs

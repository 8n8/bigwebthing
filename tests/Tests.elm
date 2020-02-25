module Tests exposing (..)

import Bytes.Decode as D
import Bytes.Encode as E
import Dict
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Hex.Convert
import Parser as P
import Test exposing (..)
import Testable exposing (..)


suite : Test
suite =
    describe "The Testable module"
        [ bytesEncDec
        ]


bytesEncDec : Test
bytesEncDec =
    describe "bytes encoders and decoders"
        [ humanMsgEncDec
        , listEncDec
        , homeEncDec
        , keysEncDec
        ]


keysEncDec : Test
keysEncDec =
    describe "secret keys encoder and decoder"
        [ test "simple" <|
            \_ ->
                case ( maybeEncrypt, maybeSign ) of
                    ( Just encrypt, Just sign ) ->
                        let
                            keys =
                                { encrypt = encrypt, sign = sign }

                            encodedKeys =
                                E.encode (encodeSecretKeys (Just keys))

                            decodedKeys =
                                D.decode decodeMyKeys encodedKeys
                        in
                        Expect.equal (Just <| Just keys) decodedKeys

                    _ ->
                        Expect.fail "could not make keys"
        ]


maybeEncrypt =
    Hex.Convert.toBytes "052B643EC1F72E94C0F2C2F28A1D997F76E93EDC01E8A2C3011C389CC0C62565"


maybeSign =
    Hex.Convert.toBytes "85C73E38539F7694B844D146EB323CE026EBD0573E7F2755C9F88A540C2AC4625F508CDCCB7AB9B4D98BD2BEDB40D9D3F120BF029169D2D72A97DD9B5AB9805B"


maybeEncrypt2 =
    Hex.Convert.toBytes "F83BDFC4DC03822FBC463C8DF9E47E55618C47A8E0909D4718962CC51B061F56"


maybeSign2 =
    Hex.Convert.toBytes "E0BDC2FA53F7C2F62692E963F1A98FF82EF139C586A56C064640877679380AA01267764D41C461B590EB501D780CDEA42709575362F11F3797CBB7CF09EAC17E"


homeEncDec : Test
homeEncDec =
    describe "home encoder and decoder"
        [ test "simple" <|
            \_ ->
                let
                    home =
                        { biggestNonceBase = 0
                        , myKeys = Nothing
                        , outbox = []
                        , programs = Dict.empty
                        , pubKeys = Dict.empty
                        }

                    encodedHome =
                        E.encode (encodeHome home)

                    decodedHome =
                        D.decode decodeHome encodedHome
                in
                Expect.equal (Just home) decodedHome
        , test "non-trivial" <|
            \_ ->
                let
                    home =
                        { biggestNonceBase = 0
                        , myKeys = Nothing
                        , outbox = [ { from = "x", to = "y", document = SmallString "z" } ]
                        , programs = Dict.empty
                        , pubKeys = Dict.empty
                        }

                    encodedHome =
                        E.encode (encodeHome home)

                    decodedHome =
                        D.decode decodeHome encodedHome
                in
                Expect.equal (Just home) decodedHome
        , test "non-empty programs" <|
            \_ ->
                case ( maybeEncrypt, maybeSign ) of
                    ( Just encrypt, Just sign ) ->
                        let
                            home =
                                { biggestNonceBase = 0
                                , myKeys =
                                    Just
                                        { encrypt = encrypt
                                        , sign = sign
                                        }
                                , outbox = []
                                , programs = Dict.fromList [ ( "zQqphWFHtsW0/yt9/uXaIKo4JTCZ7xtKZKztIzya/ik=", { blobs = [], code = "g", description = "New program", inbox = [], typedIn = "" } ) ]
                                , pubKeys = Dict.empty
                                }

                            encodedHome =
                                E.encode (encodeHome home)

                            decodedHome =
                                D.decode decodeHome encodedHome
                        in
                        Expect.equal (Just home) decodedHome

                    _ ->
                        Expect.fail "couldn't make keys"
        , test "run-time fail" <|
            \_ ->
                case ( maybeEncrypt2, maybeSign2 ) of
                    ( Just encrypt, Just sign ) ->
                        let
                            home =
                                { biggestNonceBase = 0
                                , myKeys =
                                    Just
                                        { encrypt = encrypt
                                        , sign = sign
                                        }
                                , outbox = []
                                , programs = Dict.fromList [ ( "zQqphWFHtsW0/yt9/uXaIKo4JTCZ7xtKZKztIzya/ik=", { blobs = [], code = "g", description = "New program", inbox = [], typedIn = "" } ) ]
                                , pubKeys = Dict.empty
                                }

                            encodedHome =
                                E.encode (encodeHome home)

                            decodedHome =
                                D.decode decodeHome encodedHome
                        in
                        Expect.equal (Just home) decodedHome

                    _ ->
                        Expect.fail "couldn't make keys"
        ]


listEncDec : Test
listEncDec =
    describe "list decoders and encoders"
        [ test "empty" <|
            \_ ->
                let
                    encodedList =
                        E.encode (encodeList [] E.signedInt8)

                    decodedList =
                        D.decode (Testable.list D.signedInt8) encodedList
                in
                Expect.equal (Just []) decodedList
        ]


humanMsgEncDec : Test
humanMsgEncDec =
    let
        humanMsg =
            { from = "a"
            , to = "b"
            , document = SmallString "c"
            }

        encodedHumanMsg =
            E.encode (encodeHumanMsg humanMsg)

        decodedHumanMsg =
            D.decode decodeHumanMsg encodedHumanMsg

        encodedString =
            E.encode (encodeSizedString "a")

        decodedString =
            D.decode sizedString encodedString

        document =
            SmallString "a"

        encodedDocument =
            E.encode (encodeDocument document)

        decodedDocument =
            D.decode decodeDocument encodedDocument
    in
    describe "Bytes encoders and decoders"
        [ test "sized strings" <|
            \_ -> Expect.equal (Just "a") decodedString
        , test "HumanMsg" <|
            \_ -> Expect.equal (Just humanMsg) decodedHumanMsg
        , test "document" <|
            \_ -> Expect.equal (Just document) decodedDocument
        , fuzz3 string string string "SmallString fuzz" <|
            \from to str ->
                let
                    msg =
                        { from = from, to = to, document = SmallString str }

                    encodedMsg =
                        E.encode (encodeHumanMsg msg)

                    decodedMsg =
                        D.decode decodeHumanMsg encodedMsg
                in
                Expect.equal (Just msg) decodedMsg
        ]

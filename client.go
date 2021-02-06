package main

import (
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"database/sql"
	"encoding/base64"
	"errors"
	"fmt"
	"github.com/flynn/noise"
	_ "github.com/mattn/go-sqlite3"
	"io"
	"net"
	"os"
)

func main() {
	err := mainErr()
	if err != nil {
		fmt.Println(err.Error())
	}
}

func mainErr() error {
	args, err := parseArgs(os.Args[1:])
	if err != nil {
		return err
	}
	return args.run()
}

type Args interface {
	run() error
}

const dhlen = 32

func parseUserId(raw string) ([dhlen]byte, error) {
	var userid [dhlen]byte
	decoded, err := base64.RawURLEncoding.DecodeString(raw)
	if err != nil {
		return userid, err
	}
	if len(decoded) != dhlen {
		return userid, BadUserIdLength(len(decoded))
	}
	copy(userid[:], decoded)
	return userid, nil
}

func parseArgs(args []string) (Args, error) {
	argsLength := len(args)
	if argsLength == 0 {
		return Bwt{}, nil
	}
	if argsLength == 1 {
		return parseOneArg(args[0])
	}
	userId, err := parseUserId(args[1])
	if err != nil {
		return nil, err
	}
	if argsLength == 3 && args[0] == "write" {
		msg, err := parseMessage(args[2])
		if err != nil {
			return nil, err
		}
		return Write_{to: userId, msg: msg}, nil
	}
	return nil, BadArgs{}
}

type Bwt struct{}

func makeStaticKeys() (noise.DHKey, error) {
	staticKeys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	if err != nil {
		return staticKeys, err
	}

	f, err := os.OpenFile(
		staticKeysPath,
		os.O_WRONLY|os.O_CREATE,
		0600)
	if err != nil {
		return staticKeys, fmt.Errorf("couldn't save static keys: %s", err)
	}

	err = encodeStaticKeys(f, staticKeys)
	if err != nil {
		return staticKeys, fmt.Errorf("couldn't encode static keys: %s", err)
	}

	return staticKeys, nil
}

func getStaticKeys() (noise.DHKey, error) {
	f, err := os.Open(staticKeysPath)
	if err != nil {
		return makeStaticKeys()
	}

	return parseStaticKeys(f)
}

const staticKeysPath = "staticKeys"

func (Bwt) run() error {
	staticKeys, err := getStaticKeys()
	if err != nil {
		return fmt.Errorf("couldn't get static keys: %s", err)
	}

	conn, err := net.Dial("tcp", serverUrl)
	if err != nil {
		return fmt.Errorf("couldn't connect to server: %s", err)
	}
	defer conn.Close()

	shake, err := handshake(staticKeys, conn)
	if err != nil {
		return fmt.Errorf("couldn't do handshake with server: %s", err)
	}

	req, rx, tx, err := shake.WriteMessage([]byte{}, []byte{RequestKk1sToMe})
	if err != nil {
		return fmt.Errorf("couldn't encrypt request for KK1s: %s", err)
	}

	_, err = conn.Write(req)
	if err != nil {
		return fmt.Errorf("couldn't send request for KK1s: %s", err)
	}

	for {
		size, err := uint16P(conn)
		if err != nil {
			return fmt.Errorf("couldn't read message size from server: %s", err)
		}

		encrypted := make([]byte, size)
		n, err := conn.Read(encrypted)
		if n != size {
			return fmt.Errorf("wrong number of bytes from server: expected %d, got %d", size, n)
		}
		if err != io.EOF && err != nil {
			return fmt.Errorf("error on reading KK1 from server: %s", err)
		}

		plain, err := rx.Decrypt([]byte{}, cryptoAd, encrypted)
		if err != nil {
			return fmt.Errorf("couldn't decrypt KK1 message from server")
		}

		if len(plain) == 0 {
			return errors.New("empty message from server")
		}

		if plain[0] == 1 {
			break
		}
		if plain[0] == 2 {
			var kk1 [Kk1Size]byte
			n := copy(kk1[:], plain)
			if n != Kk1Size {
				return fmt.Errorf("couldn't read KK1 from message: expected %d bytes, got %d", Kk1Size, n)
			}

			var theirId [dhlen]byte
			n = copy(theirId[:], plain)
			if n != dhlen {
				return fmt.Errorf("couldn't read their ID from message: expected %d bytes, got %d", dhlen, n)
			}

			upSize := encodeUint16(Kk2ToServerSize)
			up := make([]byte, 0, 2+CryptoOverhead+Kk2ToServerSize)
			up = append(up, upSize...)

			kk2Plain := make([]byte, 0, Kk2ToServerSize)
			kk2Plain = append(kk2Plain, UploadKk2Indicator)
			kk2Plain = append(kk2Plain, theirId[:]...)
			kk2Plain = append(kk2Plain, kk1[:SessionIdSize]...)
			secret, err := makeSessionSecret()
			if err != nil {
				return fmt.Errorf("couldn't make session secret: %s", err)
			}

			var sessionId [SessionIdSize]byte
			copy(sessionId[:], kk1[:])
			err = cacheSecret(sessionId, secret)
			if err != nil {
				return err
			}

			random, err := initRandomGen(secret)
			if err != nil {
				return fmt.Errorf("couldn't initiate random generator: %s", err)
			}

			kkConfig := makeKkConfig(staticKeys, theirId, random)
			kkShake, err := noise.NewHandshakeState(kkConfig)
			if err != nil {
				return fmt.Errorf("couldn't make new KK handshake: %s", err)
			}

			_, _, _, err = kkShake.ReadMessage([]byte{}, kk1[:])
			if err != nil {
				return fmt.Errorf("couldn't read in KK1: %s", err)
			}
			kk2Plain, _, _, err = kkShake.WriteMessage(kk2Plain, []byte{})
			if err != nil {
				return fmt.Errorf("couldn't create KK2: %s", err)
			}

			encrypted := tx.Encrypt(up, cryptoAd, kk2Plain[:])
			_, err = conn.Write(encrypted)
			if err != nil {
				return fmt.Errorf("couldn't send KK2 to server: %s", err)
			}
			continue
		}
		return fmt.Errorf("unexpected message indicator: %d", plain[0])
	}

	return nil
}

const CryptoOverhead = 16
const Kk1FromServerSize = 1 + dhlen + Kk1Size + 1 + CryptoOverhead
const Kk1Size = 48
const SessionIdSize = 24
const Kk2Size = 48
const TransportSize = 72
const Kk2ToServerSize = 1 + dhlen + SessionIdSize + Kk2Size

type Conn struct {
}

const UploadKk2Indicator = 0

var serverStaticKey = [dhlen]byte{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

func serverCipherSuite() noise.CipherSuite {
	return noise.NewCipherSuite(
		noise.DH25519,
		noise.CipherAESGCM,
		noise.HashSHA256)
}

func makeKkConfig(staticKeys noise.DHKey, theirId [dhlen]byte, randomGen RandomGen) noise.Config {
	return noise.Config{
		CipherSuite: noise.NewCipherSuite(
			noise.DH25519,
			noise.CipherAESGCM,
			noise.HashSHA256),
		Random:        randomGen,
		Pattern:       noise.HandshakeKK,
		Initiator:     false,
		StaticKeypair: staticKeys,
		PeerStatic:    theirId[:],
	}
}

const dbPath = "sessionSecrets.sqlite"

const cacheSecretSql = `INSERT INTO sessionsecrets (sessionid, secret) values (?, ?);`

func cacheSecret(
	sessionId [SessionIdSize]byte,
	secret [SecretSize]byte) error {

	db, err := sql.Open("sqlite3", dbPath)
	if err != nil {
		return fmt.Errorf(
			"couldn't open SQLITE database: %s",
			err)
	}
	defer db.Close()

	_, err = db.Exec(cacheSecretSql, sessionId[:], secret[:])
	if err != nil {
		return fmt.Errorf(
			"couldn't insert secret to SQL: %s", err)
	}
	return nil
}

func noiseServerConfig(staticKeys noise.DHKey) noise.Config {
	return noise.Config{
		CipherSuite:   serverCipherSuite(),
		Random:        rand.Reader,
		Pattern:       noise.HandshakeXK,
		Initiator:     true,
		StaticKeypair: staticKeys,
		PeerStatic:    serverStaticKey[:],
	}
}

func uint16P(f io.Reader) (int, error) {
	bs := make([]byte, 2)
	n, err := f.Read(bs)
	if n != 2 {
		return 0, fmt.Errorf("couldn't read message length: expected 2, got %d", n)
	}
	if err != io.EOF && err != nil {
		return 0, fmt.Errorf("error reading message length: %s", err)
	}

	num := 0
	num += int(bs[0])
	num += int(bs[1]) * 256
	return num, nil
}

func handshake(
	staticKeys noise.DHKey,
	conn net.Conn) (*noise.HandshakeState, error) {

	config := noiseServerConfig(staticKeys)
	shake, err := noise.NewHandshakeState(config)
	if err != nil {
		return shake, fmt.Errorf("couldn't make handshake state for server: %s", err)
	}

	msg1, _, _, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return shake, fmt.Errorf("couldn't make first server handshake message: %s", err)
	}

	_, err = conn.Write(msg1)
	if err != nil {
		return shake, fmt.Errorf("couldn't send first handshake message to server: %s", err)
	}

	const response1Size = 32 + 16 + 16
	response1 := make([]byte, response1Size)
	n, err := conn.Read(response1)
	if n != response1Size {
		return shake, fmt.Errorf("couldn't read enough bytes from server in first response: got %d, expected %d", n, response1Size)
	}
	if err != io.EOF && err != nil {
		return shake, fmt.Errorf("couldn't read first response from server: %s", err)
	}

	_, _, _, err = shake.ReadMessage([]byte{}, response1)
	return shake, err
}

const RequestKk1sToMe = 1

const serverUrl = "localhost:3001"

func encodeUint16(n int) []byte {
	result := make([]byte, 2)
	result[0] = byte(n & 0xFF)
	result[1] = byte((n >> 1) & 0xFF)
	return result
}

func encodeStaticKeys(f io.Writer, staticKeys noise.DHKey) error {
	_, err := f.Write(staticKeys.Private)
	if err != nil {
		return fmt.Errorf("couldn't encode secret DH key: %s", err)
	}

	_, err = f.Write(staticKeys.Public)
	if err != nil {
		return fmt.Errorf("couldn't encode public DH key: %s", err)
	}
	return nil
}

type WritingNewKksFailed struct{}

func (WritingNewKksFailed) Error() string {
	return "could not write new KK1 messages to 'public' file"
}

func parseOneArg(arg string) (Args, error) {
	switch arg {
	case "help":
		return Help{}, nil
	case "myid":
		return MyId{}, nil
	case "read":
		return Read_{}, nil
	}
	return nil, BadArgs{}
}

type BadArgs struct{}

const usage = `Update crypto

    $ bwt

Get usage

    $ bwt help

Get my ID

    $ bwt myid

Read messages

    $ bwt read

Write a new message

    $ bwt write <recipient ID> "hi"`

const badArgsMessage = "bad arguments"

func (BadArgs) Error() string {
	return badArgsMessage
}

type Write_ struct {
	to  [dhlen]byte
	msg [plaintextSize]byte
}

func (Write_) run() error {
	return errors.New("method Write_.run is not implemented yet")
}

type MessageTooLong struct{}

func (MessageTooLong) Error() string {
	return "message too long"
}

type EmptyMessage struct{}

func (EmptyMessage) Error() string {
	return "empty message"
}

func parseMessage(raw string) ([plaintextSize]byte, error) {
	asBytes := []byte(raw)
	var plain [plaintextSize]byte
	// The -1 is because the first byte is used for storing the
	// length.
	if len(asBytes) > plaintextSize-1 {
		return plain, MessageTooLong{}
	}
	copy(plain[1:], asBytes)
	plain[0] = byte(len(asBytes))
	return plain, messageOk(plain)
}

type WritingTransportFailed int

func (w WritingTransportFailed) Error() string {
	return fmt.Sprintf("bad byte count on writing new transport: %d", w)
}

func messageOk(msg [plaintextSize]byte) error {
	for i := 0; i < int(msg[0]); i++ {
		if msg[i+1] < 32 || msg[i+1] > 126 {
			return BadChar(msg[i+1])
		}
	}
	return nil
}

type BadChar int

func (b BadChar) Error() string {
	return fmt.Sprintf("character not allowed: %d", b)
}

type NoSessionsCantSend struct{}

func (NoSessionsCantSend) Error() string {
	return "no sessions, so can't send message"
}

type Read_ struct{}

func (Read_) run() error {
	return errors.New("method Read_.run is not implemented yet")
}

var cryptoAd []byte = []byte{100, 117, 182, 195, 110, 70, 39, 86, 128, 240}

func (r RandomGen) Read(p []byte) (int, error) {
	r.stream.XORKeyStream(p, p)
	return len(p), nil
}

const SecretSize = 2 * aes.BlockSize

type RandomGen struct {
	stream cipher.Stream
}

func initRandomGen(secret [SecretSize]byte) (RandomGen, error) {
	block, err := aes.NewCipher(secret[:aes.BlockSize])
	if err != nil {
		return *new(RandomGen), err
	}

	stream := cipher.NewCTR(block, secret[aes.BlockSize:])
	return RandomGen{stream}, nil
}

func makeSessionSecret() ([SecretSize]byte, error) {
	var secret [SecretSize]byte
	_, err := rand.Read(secret[:])
	return secret, err
}

const plaintextSize = 24

type BadKkIndicator int

func (b BadKkIndicator) Error() string {
	return "bad KK indicator"
}

type NoPublicFile struct{}

func (NoPublicFile) Error() string {
	return "need file named 'public'"
}

type TooShortForSecretKk1 struct{}

func (TooShortForSecretKk1) Error() string {
	return "too short for secret KK1"
}

type TooShortForSecret struct{}

func (TooShortForSecret) Error() string {
	return "too short for secret"
}

type TooShortForTheirId struct{}

func (TooShortForTheirId) Error() string {
	return "too short for their ID"
}

func parseDhlen(f io.Reader) ([dhlen]byte, error) {
	var id [dhlen]byte
	n, err := f.Read(id[:])
	if err != io.EOF && err != nil {
		return id, fmt.Errorf("couldn't parse DH key: %s", err)
	}
	if n < dhlen {
		return id, TooShortForTheirId{}
	}
	return id, nil
}

type TooShortForUint32 struct{}

func (TooShortForUint32) Error() string {
	return "not enough bytes for uint32"
}

func parseStaticKeys(f io.Reader) (noise.DHKey, error) {
	secret, err := parseDhlen(f)
	if err != nil {
		return *new(noise.DHKey), err
	}
	public, err := parseDhlen(f)
	return noise.DHKey{
		Private: secret[:],
		Public:  public[:],
	}, err
}

type ExpectSecretEnd struct {
	pos    int
	lenraw int
}

func (e ExpectSecretEnd) Error() string {
	return fmt.Sprintf("bad secret file: expecting file end at position %d, but length is %d", e.pos, e.lenraw)
}

type MyId struct{}

func (MyId) run() error {
	return errors.New("MyId.run is not implemented yet")
}

type Help struct{}

func (Help) run() error {
	fmt.Println(usage)
	return nil
}

type BadUserIdLength int

func (b BadUserIdLength) Error() string {
	return fmt.Sprintf(
		"wrong length: expected 43, got %d", int(b))
}

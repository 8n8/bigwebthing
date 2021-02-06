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
	"io/ioutil"
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

func makeKk2Responses(
	secrets Secrets,
	kk1Rxs map[Kk1Rx]struct{}) ([]byte, Secrets, error) {

	kk2s := make([]byte, 0, (1+kk2Size)*len(kk1Rxs))
	for k := range kk1Rxs {
		secret, err := makeSessionSecret()
		if err != nil {
			return kk2s, secrets, err
		}

		shake, err := initShakeRx(
			secret,
			k.theirid,
			secrets.staticKeys)
		if err != nil {
			return kk2s, secrets, err
		}

		_, _, _, err = shake.ReadMessage([]byte{}, k.kk1[:])
		if err != nil {
			return kk2s, secrets, err
		}

		kk2s = append(kk2s, kk2Indicator)
		kk2s, _, _, err = shake.WriteMessage(kk2s, []byte{})
		if err != nil {
			return kk2s, secrets, err
		}

		secrets.receiving[kk1AndId{k.kk1, k.theirid}] = secret
	}
	return kk2s, secrets, nil
}

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

		switch plain[0] {
		case 1:
			break
		case 2:
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

type Kk2Row struct {
	kk2    [Kk2Size]byte
	sender [dhlen]byte
}

type TransportRow struct {
	transport [TransportSize]byte
	sender    [dhlen]byte
}

type MessagesToMe struct {
	kk1s       map[[Kk1Size]byte][dhlen]byte
	kk2s       map[[SessionIdSize]byte]Kk2Row
	transports map[[SessionIdSize]byte]TransportRow
}

func initMessagesToMe() MessagesToMe {
	return MessagesToMe{
		kk1s:       make(map[[Kk1Size]byte][dhlen]byte),
		kk2s:       make(map[[SessionIdSize]byte]Kk2Row),
		transports: make(map[[SessionIdSize]byte]TransportRow),
	}
}

const UploadKk2Indicator = 0

type MessageToMe interface {
	NoMoreMessages() bool
	Insert(MessagesToMe)
}

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

func makeKk1TopUps(
	secrets Secrets,
	sessions Sessions) ([]byte, Secrets, error) {

	topUps := makeTopUpCounts(sessions, secrets.contacts)

	total := 0
	for _, count := range topUps {
		total += count
	}

	kk1s := make([]byte, 0, total*(kk1Size+1))
	for id, count := range topUps {
		for i := 0; i < count; i++ {
			secret, err := makeSessionSecret()
			if err != nil {
				return kk1s, secrets, err
			}

			kk1, err := makeKk1(
				id,
				secrets.staticKeys,
				secret)
			if err != nil {
				return kk1s, secrets, err
			}
			kk1s = append(kk1s, kk1Indicator)
			kk1s = append(kk1s, kk1[:]...)
			secrets.sending[kk1AndId{kk1, id}] = secret
		}
	}
	return kk1s, secrets, nil
}

func encodeUint16(n int) []byte {
	result := make([]byte, 2)
	result[0] = byte(n & 0xFF)
	result[1] = byte((n >> 1) & 0xFF)
	return result
}

func encodeUint32(n int) []byte {
	result := make([]byte, 4)
	for i := 0; i < 4; i++ {
		result[i] = byte((n >> (i * 8)) & 0xFF)
	}
	return result
}

func encodeSessionSecrets(
	s map[kk1AndId][SecretSize]byte,
	buf []byte) []byte {

	buf = append(buf, encodeUint32(len(s))...)

	for k, secret := range s {
		buf = append(buf, k.kk1[:]...)
		buf = append(buf, k.theirId[:]...)
		buf = append(buf, secret[:]...)
	}
	return buf
}

func secretsSize(secrets Secrets) int {
	staticKeys := 2 * dhlen
	contacts := 4 + len(secrets.contacts)*dhlen
	const sessionSize = kk1Size + dhlen + SecretSize
	sending := 4 + len(secrets.sending)*sessionSize
	receiving := 4 + len(secrets.receiving)*sessionSize
	return staticKeys + contacts + sending + receiving
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

func encodeContacts(
	contacts map[[dhlen]byte]struct{},
	buf []byte) []byte {

	buf = append(buf, encodeUint32(len(contacts))...)
	for contact := range contacts {
		buf = append(buf, contact[:]...)
	}
	return buf
}

func saveKks(kks []byte) error {
	if len(kks) == 0 {
		return nil
	}
	f, err := os.OpenFile(
		publicPath,
		os.O_APPEND|os.O_WRONLY|os.O_CREATE,
		0644)
	if err != nil {
		return err
	}
	defer f.Close()

	n, err := f.Write(kks)
	if err != nil {
		return err
	}
	if n != len(kks) {
		return WritingNewKksFailed{}
	}
	return nil
}

type WritingNewKksFailed struct{}

func (WritingNewKksFailed) Error() string {
	return "could not write new KK1 messages to 'public' file"
}

func makeKk1(
	theirid [dhlen]byte,
	staticKeys noise.DHKey,
	secret [SecretSize]byte) ([kk1Size]byte, error) {

	var kk1 [kk1Size]byte
	shake, err := initShakeTx(secret, theirid, staticKeys)
	if err != nil {
		return kk1, err
	}

	kk1Slice, _, _, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return kk1, err
	}

	copy(kk1[:], kk1Slice)
	return kk1, err
}

func makeTopUpCounts(
	sessions Sessions,
	contacts map[[dhlen]byte]struct{}) map[[dhlen]byte]int {

	ids := make(map[[dhlen]byte]int)
	for contact := range contacts {
		ids[contact] = 0
	}

	for k := range sessions.kk2Tx {
		_, ok := ids[k.theirid]
		if ok {
			ids[k.theirid] += 1
		}
	}

	for k := range sessions.kk1Tx {
		_, ok := ids[k.theirId]
		if ok {
			ids[k.theirId] += 1
		}
	}

	topups := make(map[[dhlen]byte]int)
	for id, count := range ids {
		if count < sessionsLevel {
			topups[id] = sessionsLevel - count
		}
	}
	return topups
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

func getSession(
	ks map[Kk2Tx]struct{},
	theirid [dhlen]byte) (Kk2Tx, bool) {

	for k := range ks {
		if k.theirid == theirid {
			return k, true
		}
	}
	return *new(Kk2Tx), false
}

type Write_ struct {
	to  [dhlen]byte
	msg [plaintextSize]byte
}

func (Write_) run() error {
	return errors.New("Write_.run is not implemented yet")
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

const kk1Indicator = 0
const kk2Indicator = 1
const transportIndicator = 2

type WritingTransportFailed int

func (w WritingTransportFailed) Error() string {
	return fmt.Sprintf("bad byte count on writing new transport: %d", w)
}

func writeTransport(transport [transportSize]byte) error {
	f, err := os.OpenFile(
		publicPath,
		os.O_APPEND|os.O_WRONLY|os.O_CREATE,
		0644)
	if err != nil {
		return err
	}
	defer f.Close()

	indicated := make([]byte, 1+transportSize)
	indicated[0] = transportIndicator
	copy(indicated[1:], transport[:])
	n, err := f.Write(indicated)
	if n != 1+transportSize {
		return WritingTransportFailed(n)
	}
	return err
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
	return errors.New("Read_.run is not implemented yet")
}

const kk1Size = 48

type Session interface {
	insert(Sessions) Sessions
}

func (k TransportRx) insert(sessions Sessions) Sessions {
	sessions.transportRx[k] = struct{}{}
	return sessions
}

func makeSessionFor(
	kk1 [kk1Size]byte,
	contact [dhlen]byte,
	kk2s map[[kk2Size]byte]struct{},
	transports map[[transportSize]byte]struct{},
	secrets Secrets) (Session, error) {

	txSecret, ok := secrets.sending[kk1AndId{kk1, contact}]
	if ok {
		return txSession(
			contact,
			kk2s,
			transports,
			txSecret,
			secrets.staticKeys)
	}

	return rxSession(kk1, contact, transports, secrets)
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

func initShake(
	secret [SecretSize]byte,
	contact [dhlen]byte,
	staticKeys noise.DHKey,
	initiator bool) (*noise.HandshakeState, error) {

	cipherSuite := noise.NewCipherSuite(
		noise.DH25519,
		noise.CipherAESGCM,
		noise.HashSHA256)

	random, err := initRandomGen(secret)
	if err != nil {
		return new(noise.HandshakeState), err
	}

	config := noise.Config{
		CipherSuite:   cipherSuite,
		Random:        random,
		Pattern:       noise.HandshakeKK,
		Initiator:     initiator,
		StaticKeypair: staticKeys,
		PeerStatic:    contact[:],
	}

	return noise.NewHandshakeState(config)
}

func initShakeRx(
	secret [SecretSize]byte,
	contact [dhlen]byte,
	staticKeys noise.DHKey) (*noise.HandshakeState, error) {

	return initShake(secret, contact, staticKeys, false)
}

func initShakeTx(
	secret [SecretSize]byte,
	contact [dhlen]byte,
	staticKeys noise.DHKey) (*noise.HandshakeState, error) {

	return initShake(secret, contact, staticKeys, true)
}

func txSession(
	contact [dhlen]byte,
	kk2s map[[kk2Size]byte]struct{},
	transports map[[transportSize]byte]struct{},
	secret [SecretSize]byte,
	staticKeys noise.DHKey) (Session, error) {

	kk2, foundKk2 := findKk2Tx(secret, staticKeys, contact, kk2s)
	if !foundKk2 {
		return Kk1Tx{
			theirId: contact,
			secret:  secret,
		}, nil
	}

	plain, foundTransport := findTransportTx(secret, contact, staticKeys, kk2, transports)
	if !foundTransport {
		return Kk2Tx{
			theirid: contact,
			secret:  secret,
			kk2:     kk2,
		}, nil
	}

	return TransportTx{
		theirid: contact,
		secret:  secret,
		kk2:     kk2,
		plain:   plain,
	}, nil
}

func findKk2Tx(
	secret [SecretSize]byte,
	staticKeys noise.DHKey,
	contact [dhlen]byte,
	kk2s map[[kk2Size]byte]struct{}) (
	[kk2Size]byte,
	bool) {

	for kk2 := range kk2s {
		shake, err := initShakeTx(secret, contact, staticKeys)
		if err != nil {
			return kk2, false
		}

		_, _, _, err = shake.WriteMessage([]byte{}, []byte{})
		if err != nil {
			return kk2, false
		}

		_, _, _, err = shake.ReadMessage([]byte{}, kk2[:])
		if err == nil {
			return kk2, true
		}
	}
	return *new([kk2Size]byte), false
}

func findTransportTx(
	secret [SecretSize]byte,
	contact [dhlen]byte,
	staticKeys noise.DHKey,
	kk2 [kk2Size]byte,
	transports map[[transportSize]byte]struct{}) (
	[plaintextSize]byte,
	bool) {

	var plain [plaintextSize]byte
	for transport := range transports {
		shake, err := initShakeTx(secret, contact, staticKeys)
		if err != nil {
			return plain, false
		}

		_, _, _, err = shake.WriteMessage([]byte{}, []byte{})
		if err != nil {
			return plain, false
		}

		_, cipher, _, err := shake.ReadMessage(
			[]byte{},
			kk2[:])
		if err != nil {
			return plain, false
		}

		plainSlice, err := cipher.Decrypt(
			[]byte{},
			cryptoAd,
			transport[:])
		if err == nil {
			copy(plain[:], plainSlice)
			return plain, true
		}
	}

	return plain, false
}

func (k Kk1Tx) insert(sessions Sessions) Sessions {
	sessions.kk1Tx[k] = struct{}{}
	return sessions
}

func (k Kk2Tx) insert(sessions Sessions) Sessions {
	sessions.kk2Tx[k] = struct{}{}
	return sessions
}

func makeSessionSecret() ([SecretSize]byte, error) {
	var secret [SecretSize]byte
	_, err := rand.Read(secret[:])
	return secret, err
}

func readNewKk1Rx(
	kk1 [kk1Size]byte,
	contact [dhlen]byte,
	staticKeys noise.DHKey) (Session, error) {

	tmpSecret, err := makeSessionSecret()
	if err != nil {
		return nil, err
	}

	shake, err := initShakeRx(
		tmpSecret,
		contact,
		staticKeys)
	if err != nil {
		return nil, err
	}

	_, _, _, err = shake.ReadMessage([]byte{}, kk1[:])
	if err == nil {
		return Kk1Rx{
			theirid: contact,
			kk1:     kk1,
		}, nil
	}

	return NoSession{}, nil
}

func rxSession(
	kk1 [kk1Size]byte,
	contact [dhlen]byte,
	transports map[[transportSize]byte]struct{},
	secrets Secrets) (Session, error) {

	secret, ok := secrets.receiving[kk1AndId{kk1, contact}]
	if !ok {
		return readNewKk1Rx(kk1, contact, secrets.staticKeys)
	}

	return readOldKk1Rx(
		kk1,
		contact,
		secret,
		transports,
		secrets.staticKeys)
}

func findTransportRx(
	secret [SecretSize]byte,
	staticKeys noise.DHKey,
	kk1 [kk1Size]byte,
	contact [dhlen]byte,
	transports map[[transportSize]byte]struct{}) (
	[transportSize]byte, bool) {

	for transport := range transports {
		shake, err := initShakeRx(secret, contact, staticKeys)
		if err != nil {
			return transport, false
		}

		_, _, _, err = shake.ReadMessage([]byte{}, kk1[:])
		if err != nil {
			return transport, false
		}

		_, cipher, _, err := shake.WriteMessage(
			[]byte{},
			[]byte{})
		if err != nil {
			return transport, false
		}

		_, err = cipher.Decrypt(
			[]byte{},
			cryptoAd,
			transport[:])
		if err == nil {
			return transport, true
		}
	}
	return *new([transportSize]byte), false
}

func readOldKk1Rx(
	kk1 [kk1Size]byte,
	contact [dhlen]byte,
	secret [SecretSize]byte,
	transports map[[transportSize]byte]struct{},
	staticKeys noise.DHKey) (Session, error) {

	transport, foundTransport := findTransportRx(
		secret,
		staticKeys,
		kk1,
		contact,
		transports)
	if !foundTransport {
		return Kk2Rx{
			kk1:     kk1,
			theirid: contact,
			secret:  secret,
		}, nil
	}

	return TransportRx{
		theirid:   contact,
		secret:    secret,
		kk1:       kk1,
		transport: transport,
	}, nil
}

type NoSession struct{}

func (NoSession) insert(sessions Sessions) Sessions {
	return sessions
}

func parseKk1(p Parser) (Parser, error) {
	var kk1 [kk1Size]byte
	n := copy(kk1[:], p.raw[p.cursor:])
	if n < kk1Size {
		return p, TooShortForKk1(p)
	}
	p.public.kk1s[kk1] = struct{}{}
	p.cursor += kk1Size
	return p, nil
}

const kk2Size = 48

func parseKk2(p Parser) (Parser, error) {
	var kk2 [kk2Size]byte
	n := copy(kk2[:], p.raw[p.cursor:])
	if n < kk2Size {
		return p, TooShortForKk2(p)
	}
	p.public.kk2s[kk2] = struct{}{}
	p.cursor += kk2Size
	return p, nil
}

const plaintextSize = 24
const transportOverhead = 16
const transportSize = plaintextSize + transportOverhead

type TooShortForKk1 Parser

type TooShortForKk2 Parser

func (t TooShortForKk2) Error() string {
	return fmt.Sprintf("bad public file: failed parsing KK2: parser position %d, but length is %d", t.cursor, t.lenraw)
}

type TooShortForTransport Parser

func (t TooShortForTransport) Error() string {
	return fmt.Sprintf("bad public file: failed parsing KK transport: parser position %d, but length is %d", t.cursor, t.lenraw)
}

func (t TooShortForKk1) Error() string {
	return fmt.Sprintf("bad public file: failed parsing KK1: parser position %d, but length is %d", t.cursor, t.lenraw)
}

func parseTransport(p Parser) (Parser, error) {
	var transport [transportSize]byte
	n := copy(transport[:], p.raw[p.cursor:])
	if n < transportSize {
		return p, TooShortForTransport(p)
	}
	p.public.transports[transport] = struct{}{}
	p.cursor += transportSize
	return p, nil
}

type Parser struct {
	raw    []byte
	lenraw int
	cursor int
	public Public
}

func parseKk(p Parser) (Parser, error) {
	indicator := p.raw[p.cursor]
	p.cursor++

	switch indicator {
	case 0:
		return parseKk1(p)
	case 1:
		return parseKk2(p)
	case 2:
		return parseTransport(p)
	}
	return p, BadKkIndicator(indicator)
}

type BadKkIndicator int

func (b BadKkIndicator) Error() string {
	return "bad KK indicator"
}

const publicPath = "public"

type NoPublicFile struct{}

func (NoPublicFile) Error() string {
	return "need file named 'public'"
}

func initParser(raw []byte) Parser {
	return Parser{
		raw:    raw,
		lenraw: len(raw),
		cursor: 0,
		public: initPublic(),
	}
}

func initPublic() Public {
	return Public{
		kk1s:       make(map[[kk1Size]byte]struct{}),
		kk2s:       make(map[[kk2Size]byte]struct{}),
		transports: make(map[[transportSize]byte]struct{}),
	}
}

func getPublic() (Public, error) {
	raw, err := ioutil.ReadFile(publicPath)
	var pathError *os.PathError
	if errors.As(err, &pathError) {
		return initPublic(), nil
	}
	if err != nil {
		return *new(Public), err
	}

	parser := initParser(raw)
	for parser.cursor < parser.lenraw {
		parser, err = parseKk(parser)
		if err != nil {
			break
		}
	}
	return parser.public, err
}

type kk1AndId struct {
	kk1     [kk1Size]byte
	theirId [dhlen]byte
}

type Secrets struct {
	staticKeys noise.DHKey
	contacts   map[[dhlen]byte]struct{}
	sending    map[kk1AndId][SecretSize]byte
	receiving  map[kk1AndId][SecretSize]byte
}

const secretPath = "secret"

func parseSecretKk1(raw []byte, pos int) ([kk1Size]byte, int, error) {
	var kk1 [kk1Size]byte
	n := copy(kk1[:], raw[pos:])
	if n < kk1Size {
		return kk1, pos, TooShortForSecretKk1{}
	}
	pos += kk1Size

	return kk1, pos, nil
}

type TooShortForSecretKk1 struct{}

func (TooShortForSecretKk1) Error() string {
	return "too short for secret KK1"
}

type TooShortForSecret struct{}

func (TooShortForSecret) Error() string {
	return "too short for secret"
}

func parseSecret(raw []byte, pos int) ([SecretSize]byte, int, error) {
	var secret [SecretSize]byte
	n := copy(secret[:], raw[pos:])
	if n < SecretSize {
		return secret, pos, TooShortForSecret{}
	}
	pos += SecretSize
	return secret, pos, nil
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

func parseUint32(raw []byte, pos int) (int, int, error) {
	var bytes [4]byte
	n := copy(bytes[:], raw[pos:])
	if n < 4 {
		return 0, pos, TooShortForUint32{}
	}

	pos += 4

	n = 0
	for i := 0; i < 4; i++ {
		n += int(bytes[i] << (i * 8))
	}
	return n, pos, nil
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

func (k Kk1Rx) insert(sessions Sessions) Sessions {
	sessions.kk1Rx[k] = struct{}{}
	return sessions
}

const sessionsLevel = 10

type Sessions struct {
	transportRx map[TransportRx]struct{}
	transportTx map[TransportTx]struct{}
	kk2Rx       map[Kk2Rx]struct{}
	kk2Tx       map[Kk2Tx]struct{}
	kk1Rx       map[Kk1Rx]struct{}
	kk1Tx       map[Kk1Tx]struct{}
}

func (k TransportTx) insert(sessions Sessions) Sessions {
	sessions.transportTx[k] = struct{}{}
	return sessions
}

type TransportTx struct {
	theirid [dhlen]byte
	secret  [SecretSize]byte
	kk2     [kk2Size]byte
	plain   [plaintextSize]byte
}

type TransportRx struct {
	theirid   [dhlen]byte
	secret    [SecretSize]byte
	kk1       [kk1Size]byte
	transport [transportSize]byte
}

type Kk2Rx struct {
	kk1     [kk1Size]byte
	theirid [dhlen]byte
	secret  [SecretSize]byte
}

func (k Kk2Rx) insert(sessions Sessions) Sessions {
	sessions.kk2Rx[k] = struct{}{}
	return sessions
}

type Kk2Tx struct {
	theirid [dhlen]byte
	secret  [SecretSize]byte
	kk2     [kk2Size]byte
}

func makeTransport(
	msg [plaintextSize]byte,
	k Kk2Tx,
	staticKeys noise.DHKey) ([transportSize]byte, error) {

	var transport [transportSize]byte
	shake, err := initShakeTx(k.secret, k.theirid, staticKeys)
	if err != nil {
		return transport, err
	}

	// make the KK1
	_, _, _, err = shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return transport, err
	}

	// read in the KK2 from the other party
	_, cipher, _, err := shake.ReadMessage([]byte{}, k.kk2[:])
	if err != nil {
		return transport, err
	}

	encrypted := cipher.Encrypt([]byte{}, cryptoAd, msg[:])
	copy(transport[:], encrypted)
	return transport, nil
}

type Kk1Rx struct {
	theirid [dhlen]byte
	kk1     [kk1Size]byte
}

type Kk1Tx struct {
	theirId [dhlen]byte
	secret  [SecretSize]byte
}

func addSessions(s1 Sessions, s2 Sessions) Sessions {
	for t := range s2.transportRx {
		s1.transportRx[t] = struct{}{}
	}
	for t := range s2.transportTx {
		s1.transportTx[t] = struct{}{}
	}
	for k := range s2.kk2Rx {
		s1.kk2Rx[k] = struct{}{}
	}
	for k := range s2.kk2Tx {
		s1.kk2Tx[k] = struct{}{}
	}
	for k := range s2.kk1Rx {
		s1.kk1Rx[k] = struct{}{}
	}
	for k := range s2.kk1Tx {
		s1.kk1Tx[k] = struct{}{}
	}
	return s1
}

func initSessions() Sessions {
	return Sessions{
		transportRx: make(map[TransportRx]struct{}),
		transportTx: make(map[TransportTx]struct{}),
		kk2Rx:       make(map[Kk2Rx]struct{}),
		kk2Tx:       make(map[Kk2Tx]struct{}),
		kk1Rx:       make(map[Kk1Rx]struct{}),
		kk1Tx:       make(map[Kk1Tx]struct{}),
	}
}

type Public struct {
	kk1s       map[[kk1Size]byte]struct{}
	kk2s       map[[kk2Size]byte]struct{}
	transports map[[transportSize]byte]struct{}
}

func makeSessionsFor(
	contact [dhlen]byte,
	public Public,
	secrets Secrets) (Sessions, error) {

	sessions := initSessions()
	for kk1 := range public.kk1s {
		session, err := makeSessionFor(
			kk1,
			contact,
			public.kk2s,
			public.transports,
			secrets)
		if err != nil {
			return sessions, err
		}
		sessions = session.insert(sessions)
	}
	return sessions, nil
}

func makeSessions(public Public, secrets Secrets) (Sessions, error) {
	sessions := initSessions()
	for contact := range secrets.contacts {
		newSessions, err := makeSessionsFor(
			contact,
			public,
			secrets)
		if err != nil {
			return sessions, err
		}
		sessions = addSessions(sessions, newSessions)
	}
	return sessions, nil
}

func getSessions(secrets Secrets) (Sessions, error) {
	public, err := getPublic()
	if err != nil {
		return *new(Sessions), err
	}
	return makeSessions(public, secrets)
}

func showTransportRx(
	t TransportRx,
	staticKeys noise.DHKey) (string, error) {

	shake, err := initShakeRx(t.secret, t.theirid, staticKeys)
	if err != nil {
		return "", err
	}

	_, _, _, err = shake.ReadMessage([]byte{}, t.kk1[:])
	if err != nil {
		return "", err
	}

	_, cipher, _, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return "", err
	}

	plain, err := cipher.Decrypt([]byte{}, cryptoAd, t.transport[:])
	if err != nil {
		return "", err
	}
	size := int(plain[0])
	return string(plain[1 : size+1]), nil
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

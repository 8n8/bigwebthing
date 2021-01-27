package main

import (
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"encoding/base64"
	"errors"
	"fmt"
	"github.com/flynn/noise"
	"io/ioutil"
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
	if argsLength == 2 && args[0] == "addcontact" {
		return AddContact(userId), nil
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

func (Bwt) run() error {
	secrets, err := getSecrets()
	if err != nil {
		return err
	}

	sessions, err := getSessions(secrets)
	if err != nil {
		return err
	}

	kk1s, secrets, err := makeKk1TopUps(secrets, sessions)
	if err != nil {
		return err
	}

	kk2s, secrets, err := makeKk2Responses(
		secrets,
		sessions.kk1Rx)
	if err != nil {
		return err
	}

	err = saveKks(append(kk1s, kk2s...))
	if err != nil {
		return err
	}

	return saveSecrets(secrets)
}

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

func encodeStaticKeys(staticKeys noise.DHKey, buf []byte) []byte {
	buf = append(buf, staticKeys.Private...)
	buf = append(buf, staticKeys.Public...)
	return buf
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

func encodeSecrets(secrets Secrets) []byte {
	buf := make([]byte, 0, secretsSize(secrets))

	buf = encodeStaticKeys(secrets.staticKeys, buf)
	buf = encodeContacts(secrets.contacts, buf)
	buf = encodeSessionSecrets(secrets.sending, buf)
	buf = encodeSessionSecrets(secrets.receiving, buf)
	return buf
}

func saveSecrets(secrets Secrets) error {
	return ioutil.WriteFile(
		secretPath, encodeSecrets(secrets), 0644)
}

func saveKks(kks []byte) error {
	if len(kks) == 0 {
		return nil
	}
	f, err := os.OpenFile(
		publicPath,
		os.O_APPEND | os.O_WRONLY | os.O_CREATE,
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

	for k := range sessions.kk1kk2Tx {
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

    $ bwt write <recipient ID> "hi"

Add contact

    $ bwt addcontact <contact ID>`

const badArgsMessage = "bad arguments"

func (BadArgs) Error() string {
	return badArgsMessage
}

type AddContact [dhlen]byte

func (a AddContact) run() error {
	secrets, err := getSecrets()
	if err != nil {
		return err
	}

	_, alreadyAcontact := secrets.contacts[a]
	if alreadyAcontact {
		fmt.Println("already a contact")
		return nil
	}

	secrets.contacts[a] = struct{}{}
	return saveSecrets(secrets)
}

func getSession(
	ks map[Kk1Kk2Tx]struct{},
	theirid [dhlen]byte) (Kk1Kk2Tx, bool) {

	for k := range ks {
		if k.theirid == theirid {
			return k, true
		}
	}
	return *new(Kk1Kk2Tx), false
}

type Write_ struct {
	to [dhlen]byte
	msg [plaintextSize]byte
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
	if len(asBytes) > plaintextSize - 1 {
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
		os.O_APPEND | os.O_WRONLY | os.O_CREATE,
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

func (s Write_) run() error {
	secrets, err := getSecrets()
	if err != nil {
		return err
	}

	sessions, err := getSessions(secrets)
	if err != nil {
		return err
	}

	session, ok := getSession(sessions.kk1kk2Tx, s.to)
	if !ok {
		return NoSessionsCantSend{}
	}

	transport, err := makeTransport(
		s.msg,
		session,
		secrets.staticKeys)
	if err != nil {
		return err
	}

	return writeTransport(transport)
}

type NoSessionsCantSend struct{}

func (NoSessionsCantSend) Error() string {
	return "no sessions, so can't send message"
}

type Read_ struct{}

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

	session, done, err := txSessions(
		kk1,
		contact,
		kk2s,
		transports,
		secrets)
	if done {
		return session, err
	}

	return rxSessions(kk1, contact, kk2s, transports, secrets)
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

func txSessions(
	kk1 [kk1Size]byte,
	contact [dhlen]byte,
	kk2s map[[kk2Size]byte]struct{},
	transports map[[transportSize]byte]struct{},
	secrets Secrets) (Session, bool, error) {

	secret, ok := secrets.sending[kk1AndId{kk1, contact}]
	if !ok {
		return nil, false, nil
	}

	// So it's a KK1 that I sent out at some stage.

	shake, err := initShakeTx(secret, contact, secrets.staticKeys)
	if err != nil {
		return nil, true, err
	}

	// Replicate the KK1.
	replica, _, _, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return nil, true, err
	}
	if !bytesEqual(replica, kk1[:]) {
		return nil, true, CouldntReplicateKk1{}
	}

	// See if there is a KK2 that someone sent in response.
	if len(kk2s) == 0 {
		// There aren't any KK2s at all.
		return Kk1Tx{
			theirId: contact,
			secret: secret,
		}, true, nil
	}

	// There are some KK2s, but are there any from this person?
	var cipher *noise.CipherState
	var kk2 [kk2Size]byte
	for kk2 = range kk2s {
		_, cipher, _, err = shake.ReadMessage([]byte{}, kk2[:])
		if err == nil {
			break
		}
	}
	if err != nil {
		return Kk1Tx{
			theirId: contact,
			secret:  secret,
		}, true, nil
	}

	// So there is a responding KK2.
	if len(transports) == 0 {
		return Kk1Kk2Tx{
			theirid: contact,
			secret: secret,
			kk2: kk2,
		}, true, nil
	}
	for transport := range transports {
		plainArr, err := cipher.Decrypt(
			[]byte{},
			cryptoAd,
			transport[:])
		if err == nil {
			var plain [plaintextSize]byte
			copy(plain[:], plainArr)
			// So I sent out a transport.
			return TransportTx{
				theirid: contact,
				secret:  secret,
				kk2:     kk2,
				plain:   plain,
			}, true, nil
		}
	}

	// So there is a KK1 and a KK2 but no transport.
	return Kk1Kk2Tx{
		theirid: contact,
		secret:  secret,
		kk2:     kk2,
	}, true, nil
}

func (k Kk1Tx) insert(sessions Sessions) Sessions {
	sessions.kk1Tx[k] = struct{}{}
	return sessions
}

func (k Kk1Kk2Tx) insert(sessions Sessions) Sessions {
	sessions.kk1kk2Tx[k] = struct{}{}
	return sessions
}

func makeSessionSecret() ([SecretSize]byte, error) {
	var secret [SecretSize]byte
	_, err := rand.Read(secret[:])
	return secret, err
}

func rxSessions(
	kk1 [kk1Size]byte,
	contact [dhlen]byte,
	kk2s map[[kk2Size]byte]struct{},
	transports map[[transportSize]byte]struct{},
	secrets Secrets) (Session, error) {

	secret, ok := secrets.receiving[kk1AndId{kk1, contact}]
	if !ok {
		// It's not a KK1 I have interacted with before.
		tmpSecret, err := makeSessionSecret()
		if err != nil {
			return nil, err
		}
		shake, err := initShakeRx(
			tmpSecret, contact, secrets.staticKeys)
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

	// It's a KK1 that I have responded to in the past.

	shake, err := initShakeRx(secret, contact, secrets.staticKeys)
	if err != nil {
		return nil, err
	}

	_, _, _, err = shake.ReadMessage([]byte{}, kk1[:])
	if err != nil {
		return NoSession{}, nil
	}

	// So it's a valid KK1 from this contact.

	newKk2Slice, cipher, _, err := shake.WriteMessage(
		[]byte{}, []byte{})
	if err != nil {
		return nil, err
	}
	var newKk2 [kk2Size]byte
	copy(newKk2[:], newKk2Slice)

	_, kk2Exists := kk2s[newKk2]
	if !kk2Exists {
		return Kk1Rx{
			theirid: contact,
			kk1:     kk1,
		}, nil
	}

	// I have already responded to the KK1 with a KK2.

	if len(transports) == 0 {
		return Kk1Kk2Rx{
			kk1: kk1,
			theirid: contact,
			secret: secret,
		}, nil
	}
	var transport [transportSize]byte
	for transport = range transports {
		_, err = cipher.Decrypt(
			[]byte{}, cryptoAd, transport[:])
		if err == nil {
			break
		}
	}
	if err != nil {
		return Kk1Kk2Rx{
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

func bytesEqual(as []byte, bs []byte) bool {
	if len(as) != len(bs) {
		return false
	}
	for i, a := range as {
		if a != bs[i] {
			return false
		}
	}
	return true
}

type NoSession struct{}

func (NoSession) insert(sessions Sessions) Sessions {
	return sessions
}

func parseKk1(p Parser) (Parser, error) {
	if p.lenraw-p.cursor < kk1Size {
		return p, TooShortForKk1(p)
	}

	var kk1 [kk1Size]byte
	copy(kk1[:], p.raw[p.cursor:])
	p.public.kk1s[kk1] = struct{}{}
	p.cursor += kk1Size
	return p, nil
}

const kk2Size = 48

func parseKk2(p Parser) (Parser, error) {
	if p.lenraw-p.cursor < kk2Size {
		return p, TooShortForKk2(p)
	}

	var kk2 [kk2Size]byte
	copy(kk2[:], p.raw[p.cursor:])
	p.public.kk2s[kk2] = struct{}{}
	p.cursor += kk2Size
	return p, nil
}

const plaintextSize = 24
const transportOverhead = 16
const transportSize = plaintextSize + transportOverhead

type CouldntReplicateKk1 struct{}

func (CouldntReplicateKk1) Error() string {
	return "couldn't replicate KK1"
}

type TooShortForKk1 Parser

type TooShortForKk2 Parser

func (t TooShortForKk2) Error() string {
	return fmt.Sprintf("bad 'public' file: failed parsing KK2: parser position %d, but length is %d", t.cursor, t.lenraw)
}

type TooShortForTransport Parser

func (t TooShortForTransport) Error() string {
	return fmt.Sprintf("bad 'public' file: failed parsing KK transport: parser position %d, but length is %d", t.cursor, t.lenraw)
}

func (t TooShortForKk1) Error() string {
	return fmt.Sprintf("bad 'public' file: failed parsing KK1: parser position %d, but length is %d", t.cursor, t.lenraw)
}

func parseTransport(p Parser) (Parser, error) {
	if p.lenraw-p.cursor < transportSize {
		return p, TooShortForTransport(p)
	}
	var transport [transportSize]byte
	copy(transport[:], p.raw[p.cursor:])
	p.public.transports[transport] = struct{}{}
	p.cursor += transportSize
	return p, nil
}

type Parser struct {
	raw     []byte
	lenraw  int
	cursor  int
	public  Public
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
		raw:     raw,
		lenraw:  len(raw),
		cursor:  0,
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

func parseSessionSecrets(
	raw []byte,
	pos int) (map[kk1AndId][SecretSize]byte, int, error) {

	n, pos, err := uint32P(raw, pos)
	if err != nil {
		return nil, pos, err
	}

	secrets := make(map[kk1AndId][SecretSize]byte)
	var kk1 [kk1Size]byte
	var theirid [dhlen]byte
	var secret [SecretSize]byte
	for i := 0; i < n; i++ {
		kk1, pos, err = kk1P(raw, pos)
		if err != nil {
			return nil, pos, err
		}

		theirid, pos, err = dhlenP(raw, pos)
		if err != nil {
			return nil, pos, err
		}

		secret, pos, err = secretP(raw, pos)
		if err != nil {
			return nil, pos, err
		}

		secrets[kk1AndId{kk1, theirid}] = secret
	}
	return secrets, pos, nil
}

func kk1P(raw []byte, pos int) ([kk1Size]byte, int, error) {
	if len(raw)-pos < kk1Size {
		return *new([kk1Size]byte), pos, TooShortForKk1{}
	}

	var kk1 [kk1Size]byte
	copy(kk1[:], raw[pos:])
	pos += kk1Size

	return kk1, pos, nil
}

type TooShortForSecret struct{}

func (TooShortForSecret) Error() string {
	return "too short for secret"
}

func secretP(raw []byte, pos int) ([SecretSize]byte, int, error) {
	if len(raw)-pos < SecretSize {
		return *new([SecretSize]byte), pos, TooShortForSecret{}
	}

	var secret [SecretSize]byte
	copy(secret[:], raw[pos:])
	pos += SecretSize
	return secret, pos, nil
}

type TooShortForTheirId struct{}

func (TooShortForTheirId) Error() string {
	return "too short for their ID"
}

func dhlenP(raw []byte, pos int) ([dhlen]byte, int, error) {
	if len(raw)-pos < dhlen {
		return *new([dhlen]byte), pos, TooShortForTheirId{}
	}

	var theirId [dhlen]byte
	copy(theirId[:], raw[pos:])
	pos += dhlen
	return theirId, pos, nil
}

type TooShortForUint32 struct{}

func (TooShortForUint32) Error() string {
	return "not enough bytes for uint32"
}

func uint32P(raw []byte, pos int) (int, int, error) {
	if len(raw)-pos < 4 {
		return 0, pos, TooShortForUint32{}
	}

	bs := raw[pos : pos+4]
	pos += 4

	n := 0
	for i := 0; i < 4; i++ {
		n += int(bs[i] << (i * 8))
	}
	return n, pos, nil
}

func parseContacts(
	raw []byte,
	pos int) (map[[dhlen]byte]struct{}, int, error) {

	n, pos, err := uint32P(raw, pos)
	if err != nil {
		return *new(map[[dhlen]byte]struct{}), pos, err
	}

	contacts := make(map[[dhlen]byte]struct{}, n)
	for i := 0; i < n; i++ {
		contact, pos, err := dhlenP(raw, pos)
		if err != nil {
			return contacts, pos, err
		}
		contacts[contact] = struct{}{}
	}
	return contacts, pos, nil
}

func parseStaticKeys(raw []byte, pos int) (noise.DHKey, int, error) {
	secret, pos, err := dhlenP(raw, pos)
	if err != nil {
		return *new(noise.DHKey), pos, err
	}
	public, pos, err := dhlenP(raw, pos)
	return noise.DHKey{
		Private: secret[:],
		Public:  public[:],
	}, pos, err
}

func parseSecrets(raw []byte) (Secrets, error) {
	pos := 0
	var secret Secrets

	staticKeys, pos, err := parseStaticKeys(raw, pos)
	if err != nil {
		return secret, err
	}

	contacts, pos, err := parseContacts(raw, pos)
	if err != nil {
		return secret, err
	}

	sending, pos, err := parseSessionSecrets(raw, pos)
	if err != nil {
		return secret, err
	}

	receiving, pos, err := parseSessionSecrets(raw, pos)
	if err != nil {
		return secret, err
	}

	if pos != len(raw) {
		return secret, ExpectSecretEnd{
			pos:    pos,
			lenraw: len(raw)}
	}

	return Secrets{
		staticKeys: staticKeys,
		contacts:   contacts,
		sending:    sending,
		receiving:  receiving,
	}, nil
}

type ExpectSecretEnd struct {
	pos    int
	lenraw int
}

func (e ExpectSecretEnd) Error() string {
	return fmt.Sprintf("bad secret file: expecting file end at position %d, but length is %d", e.pos, e.lenraw)
}

func makeSecrets() (Secrets, error) {
	staticKeys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	if err != nil {
		return *new(Secrets), err
	}
	sending := make(map[kk1AndId][SecretSize]byte)
	receiving := make(map[kk1AndId][SecretSize]byte)
	secrets := Secrets{
		staticKeys: staticKeys,
		contacts:   make(map[[dhlen]byte]struct{}),
		sending:    sending,
		receiving:  receiving,
	}
	err = saveSecrets(secrets)
	return secrets, err
}

func getSecrets() (Secrets, error) {
	raw, err := ioutil.ReadFile(secretPath)
	var pathError *os.PathError
	if errors.As(err, &pathError) {
		return makeSecrets()
	}
	if err != nil {
		return *new(Secrets), err
	}
	return parseSecrets(raw)
}

func (k Kk1Rx) insert(sessions Sessions) Sessions {
	sessions.kk1Rx[k] = struct{}{}
	return sessions
}

const sessionsLevel = 1

type Sessions struct {
	transportRx map[TransportRx]struct{}
	transportTx map[TransportTx]struct{}
	kk1kk2Rx    map[Kk1Kk2Rx]struct{}
	kk1kk2Tx    map[Kk1Kk2Tx]struct{}
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

type Kk1Kk2Rx struct {
	kk1     [kk1Size]byte
	theirid [dhlen]byte
	secret  [SecretSize]byte
}

func (k Kk1Kk2Rx) insert(sessions Sessions) Sessions {
	sessions.kk1kk2Rx[k] = struct{}{}
	return sessions
}

type Kk1Kk2Tx struct {
	theirid [dhlen]byte
	secret  [SecretSize]byte
	kk2     [kk2Size]byte
}

func makeTransport(
	msg [plaintextSize]byte,
	k Kk1Kk2Tx,
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
	for k := range s2.kk1kk2Rx {
		s1.kk1kk2Rx[k] = struct{}{}
	}
	for k := range s2.kk1kk2Tx {
		s1.kk1kk2Tx[k] = struct{}{}
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
		kk1kk2Rx:    make(map[Kk1Kk2Rx]struct{}),
		kk1kk2Tx:    make(map[Kk1Kk2Tx]struct{}),
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
			contact, public, secrets)
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
	return string(plain), nil
}

func (Read_) run() error {
	secrets, err := getSecrets()
	if err != nil {
		return err
	}

	sessions, err := getSessions(secrets)
	if err != nil {
		return err
	}

	for transport := range sessions.transportRx {
		pretty, err := showTransportRx(
			transport, secrets.staticKeys)
		if err != nil {
			return err
		}
		fmt.Println(pretty)
	}

	return nil
}

type MyId struct{}

func (MyId) run() error {
	secrets, err := getSecrets()
	if err != nil {
		return err
	}
	str := base64.RawURLEncoding.EncodeToString(
		secrets.staticKeys.Public)
	fmt.Println(str)
	return nil
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

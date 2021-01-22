package main

import (
	"encoding/base64"
	"fmt"
	"github.com/flynn/noise"
	"os"
	"io/ioutil"
	"crypto/rand"
	"io"
)

func main() {
	err := mainErr()
	if (err != nil) {
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

func parseUserId(raw string) ([]byte, error) {
	decoded, err := base64.RawURLEncoding.DecodeString(raw)
	if err != nil {
		return []byte{}, err
	}
	if len(decoded) != dhlen {
		return []byte{}, BadUserIdLength(len(decoded))
	}
	return decoded, nil
}

func parseArgs(args []string) (Args, error) {
	argsLength := len(args)
	if argsLength == 1 {
		return parseOneArg(args[0])
	}
	if argsLength == 2 {
		userId, err := parseUserId(args[1])
		if err != nil {
			return nil, err
		}
		return parseTwoArgs(args[0], userId)
	}
	return nil, BadArgs{}
}

func parseOneArg(arg string) (Args, error) {
	switch arg {
	case "help":
		return Help{}, nil
	case "myid":
		return MyId{}, nil
	case "get":
		return Read_{}, nil
	}
	return nil, BadArgs{}
}

type BadArgs struct{}

const usage = `
Get usage

    $ bwt help

Get my ID

    $ bwt myid

Read messages

    $ bwt read

Write a new message from STDIN

    $ bwt write <recipient ID>

Add contact

    $ bwt addcontact <contact ID>`


const badArgsMessage = "bad arguments: usage:\n" + usage

func (BadArgs) Error() string {
	return badArgsMessage
}

func parseTwoArgs(arg1 string, userId []byte) (Args, error) {
	switch arg1 {
	case "send":
		return Write_(userId), nil
	case "addcontact":
		return AddContact(userId), nil
	}
	return nil, BadArgs{}
}

type AddContact []byte

func (a AddContact) run() error {
	fmt.Println("addcontact not implemented yet")
	return nil
}

type Write_ []byte

func (s Write_) run() error {
	fmt.Println("send not implemented yet")
	return nil
}

type Read_ struct{}

type UserId []byte

type Ephemeral struct {
	id UserId
	keys noise.DHKey
}

type Cache struct {
	staticKeys noise.DHKey
	contacts []UserId
	ephemerals []Ephemeral
}

func readCache() (Cache, error) {
	fmt.Println("readCache not implemented yet")
	var cache Cache
	return cache, nil
}

func requestMessage(cache Cache) error {
	fmt.Println("requestMessage not implemented yet")
	return nil
}

func downloadMessage(cache Cache) ([]byte, error) {
	fmt.Println("downloadMessage not implemented yet")
	return []byte{}, nil
}

const kk1_size = 48

type Session interface {
	insert(Sessions) Sessions
}

func (k TransportRx) insert(sessions Sessions) Sessions {
	sessions.transportRx = append(sessions.transportRx, k)
	return sessions
}

// The session can be one of these types:
// 
// 0. not to me or from this contact
// sending
// 	1. KK1
// 	2. KK1 KK2
// receiving
// 	3. KK1
// 	4. KK1 KK2
// 	5. KK1 KK2 Transport
func makeSessionFor(
	kk1 Kk1,
	contact UserId,
	kk2s []Kk2,
	transports []KkTransport,
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

func makeRandomGen(secret Secret) io.Reader {
	panic("makeRandomGen not implemented yet")
	return rand.Reader
}

func initShake(
	secret Secret,
	contact UserId,
	staticKeys noise.DHKey,
	initiator bool) (*noise.HandshakeState, error) {

	cipherSuite := noise.NewCipherSuite(
		noise.DH25519,
		noise.CipherChaChaPoly,
		noise.HashBLAKE2s)

	config := noise.Config{
		CipherSuite: cipherSuite,
		Random: makeRandomGen(secret),
		Pattern: noise.HandshakeKK,
		Initiator: initiator,
		StaticKeypair: staticKeys,
		PeerStatic: contact,
	}

	return noise.NewHandshakeState(config)
}

func initShakeRx(
	secret Secret,
	contact UserId,
	staticKeys noise.DHKey) (*noise.HandshakeState, error) {

	return initShake(secret, contact, staticKeys, false)
}

func initShakeTx(
	secret Secret,
	contact UserId,
	staticKeys noise.DHKey) (*noise.HandshakeState, error) {

	return initShake(secret, contact, staticKeys, true)
}

func txSessions(
	kk1 Kk1,
	contact UserId,
	kk2s []Kk2,
	transports []KkTransport,
	secrets Secrets) (Session, bool, error) {

	secret, ok := secrets.sending.get(kk1, contact)
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
	if !bytesEqual(replica, kk1) {
		return nil, true, CouldntReplicateKk1{}
	}

	// See if there is a KK2 that someone sent in response.
	var cipher *noise.CipherState
	var kk2 Kk2
	for _, kk2 = range kk2s {
		_, cipher, _, err = shake.ReadMessage([]byte{}, kk2)
		if err == nil {
			break
		}
	}
	if err != nil {
		// So there is no responding KK2.
		return Kk1Tx{
			theirId: contact,
			secret: secret,
		}, true, nil
	}

	// So there is a responding KK2.
	var plain []byte
	for _, transport := range transports {
		plain, err = cipher.Decrypt(
			[]byte{},
			cryptoAd,
			transport)
		if err == nil {
			// So I sent out a transport.
			return TransportTx{
				theirid: contact,
				secret: secret,
				kk2: kk2,
				plain: plain,
			}, true, nil
		}
	}

	// So there is a KK1 and a KK2 but no transport.
	return Kk1Kk2Tx{
		theirid: contact,
		secret: secret,
		kk2: kk2,
	}, true, nil
}

func (k Kk1Tx) insert(sessions Sessions) Sessions {
	sessions.kk1Tx = append(sessions.kk1Tx, k)
	return sessions
}

func (k Kk1Kk2Tx) insert(sessions Sessions) Sessions {
	sessions.kk1kk2Tx = append(sessions.kk1kk2Tx, k)
	return sessions
}

func makeSessionSecret() (Secret, error) {
	panic("makeSessionSecret not implemented yet")
	return *new(Secret), nil
}

func rxSessions(
	kk1 Kk1,
	contact UserId,
	kk2s []Kk2,
	transports []KkTransport,
	secrets Secrets) (Session, error) {

	secret, ok := secrets.receiving.get(kk1, contact)
	if !ok {
		tmpSecret, err := makeSessionSecret()
		if err != nil {
			return nil, err
		}
		shake, err := initShakeRx(
			tmpSecret, contact, secrets.staticKeys)
		if err != nil {
			return nil, err
		}
		_, _, _, err = shake.ReadMessage([]byte{}, kk1)
		if err == nil {
			return Kk1Rx{
				theirid: contact,
				kk1: kk1,
			}, nil
		}
		return NoSession{}, nil
	}

	shake, err := initShakeRx(secret, contact, secrets.staticKeys)
	if err != nil {
		return nil, err
	}


	_, _, _, err = shake.ReadMessage([]byte{}, kk1)
	if err != nil {
		return NoSession{}, nil
	}

	newKk2, _, cipher, err := shake.WriteMessage(
		[]byte{}, []byte{})
	if err != nil {
		return nil, err
	}

	if !matchingKk2(newKk2, kk2s) {
		return Kk1Rx{
			theirid: contact,
			kk1: kk1,
		}, nil
	}

	var transport []byte
	for _, transport = range transports {
		_, err = cipher.Decrypt(
			[]byte{}, cryptoAd, transport)
		if err == nil {
			break
		}
	}
	if err != nil {
		return Kk1Kk2Rx{
			kk1: kk1,
			theirid: contact,
			secret: secret,
		}, nil
	}

	return TransportRx{
		theirid: contact,
		secret: secret,
		kk1: kk1,
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

func matchingKk2(newKk2 Kk2, oldKk2s []Kk2) bool {
	for _, oldKk2 := range oldKk2s {
		if bytesEqual(newKk2, oldKk2) {
			return true
		}
	}
	return false
}

type NoSession struct{}

func (NoSession) insert(sessions Sessions) Sessions {
	return sessions
}

func parseKk1(p Parser) (Parser, error) {
	if p.lenraw - p.cursor < kk1_size {
		return p, TooShortForKk1(p)
	}

	kk1 := Kk1(p.raw[p.cursor: p.cursor + kk1_size])
	kk1s := append(p.public.kk1s, kk1)
	return Parser{
		raw: p.raw,
		lenraw: p.lenraw,
		cursor: p.cursor + kk1_size,
		counter: p.counter,
		public: Public{
			kk1s: kk1s,
			kk2s: p.public.kk2s,
			transports: p.public.transports,
		},
	}, nil
}

const kk2_size = 48

func parseKk2(p Parser) (Parser, error) {
	if p.lenraw - p.cursor < kk2_size {
		return p, TooShortForKk2(p)
	}

	kk2 := Kk2(p.raw[p.cursor: p.cursor + kk2_size])
	kk2s := append(p.public.kk2s, kk2)
	return Parser{
		raw: p.raw,
		lenraw: p.lenraw,
		cursor: p.cursor + kk2_size,
		counter: p.counter,
		public: Public{
			kk1s: p.public.kk1s,
			kk2s: kk2s,
			transports: p.public.transports,
		},
	}, nil
}

const kk_transport_size = 40

type KkTransport []byte

type Kk1 []byte

func makeConfig(
	secret []byte,
	userId UserId,
	staticKeys noise.DHKey) noise.Config {

	panic("makeConfig not implemented yet")
	return *new(noise.Config)
}

type CouldntReplicateKk1 struct{}

func (CouldntReplicateKk1) Error() string {
	return "couldn't replicate KK1"
}

type Kk2 []byte

type TooShortForKk1 Parser

type TooShortForKk2 Parser

func (t TooShortForKk2) Error() string {
	return fmt.Sprintf("bad 'public' file: failed parsing KK2: parser position %d, but length is %d", t.cursor, t.lenraw)
}

type TooShortForTransport Parser

func (t TooShortForTransport) Error() string {
	return fmt.Sprintf("bad 'public' file: failed parsing KK transport: parser position &d, but length is &d", t.cursor, t.lenraw)
}

func (t TooShortForKk1) Error() string {
	return fmt.Sprintf("bad 'public' file: failed parsing KK1: parser position %d, but length is %d", t.cursor, t.lenraw)
}

func parseKkTransport(p Parser) (Parser, error) {
	if p.lenraw - p.cursor < kk_transport_size {
		return p, TooShortForTransport(p)
	}
	transport := KkTransport(
		p.raw[p.cursor: p.cursor + kk_transport_size])
	transports := append(p.public.transports, transport)
	return Parser{
		raw: p.raw,
		lenraw: p.lenraw,
		cursor: p.cursor + kk_transport_size,
		counter: p.counter,
		public: Public{
			kk1s: p.public.kk1s,
			kk2s: p.public.kk2s,
			transports: transports,
		},
	}, nil
}

type Parser struct {
	raw []byte
	lenraw int
	cursor int
	counter int
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
		return parseKkTransport(p)
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
		raw: raw,
		lenraw: len(raw),
		cursor: 0,
		counter: 0,
		public: Public{
			kk1s: make([]Kk1, 0),
			kk2s: make([]Kk2, 0),
			transports: make([]KkTransport, 0),
		},
	}
}

func getPublic() (Public, error) {
	raw, err := ioutil.ReadFile(publicPath)
	if os.IsNotExist(err) {
		return *new(Public), NoPublicFile{}
	}
	if err != nil {
		return *new(Public), err
	}

	parser := initParser(raw)
	for ; parser.cursor < parser.lenraw; parser.counter++ {
		parser, err = parseKk(parser)
		if err != nil {
			return parser.public, err
		}
	}
	return parser.public, nil
}

const kk1Size = 48

type SessionSecrets interface {
	insert(Kk1, UserId, Secret)
	remove(Kk1)
	get(Kk1, UserId) (Secret, bool)
}

type Secret []byte

type Secrets struct {
	staticKeys noise.DHKey
	contacts []UserId
	sending SessionSecrets
	receiving SessionSecrets
}

const secretPath = "secret"

func parseSessionSecrets(raw []byte, pos int) (SessionSecrets, int, error) {
	panic("parseSessions not implemented yet")
	return nil, 0, nil
}

func parseContacts(raw []byte, pos int) ([]UserId, int, error) {
	panic("parseContacts not implemented yet")
	return *new([]UserId), 0, nil
}

func parseStaticKeys(raw []byte, pos int) (noise.DHKey, int, error) {
	panic("parseStaticKeys not implemented yet")
	return *new(noise.DHKey), 0, nil
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
			pos: pos,
			lenraw: len(raw)}
	}

	return Secrets {
		staticKeys: staticKeys,
		contacts: contacts,
		sending: sending,
		receiving: receiving,
	}, nil
}

type ExpectSecretEnd struct {
	pos int
	lenraw int
}

func (e ExpectSecretEnd) Error() string {
	return fmt.Sprintf("bad secret file: expecting file end at position %d, but length is %d", e.pos, e.lenraw)
}

func makeSecrets() (Secrets, error) {
	panic("makeSecret not done yet")
	return *new(Secrets), nil
}

func getSecrets() (Secrets, error) {
	raw, err := ioutil.ReadFile(secretPath)
	if os.IsNotExist(err) {
		return makeSecrets()
	}
	if err != nil {
		return *new(Secrets), err
	}
	return parseSecrets(raw)
}

type TransportSession struct {
	theirid UserId
	secret Secret
	kk1 Kk1
	kk2 Kk2
	transport KkTransport
}

type Kk1Kk2Session struct {
	theirid UserId
	secret Secret
	kk1 Kk1
	kk2 Kk2
}

func (k Kk1Rx) insert(sessions Sessions) Sessions {
	sessions.kk1Rx = append(sessions.kk1Rx, k)
	return sessions
}

type Kk1TxSession struct {
	theirId UserId
	secret Secret
	kk1 Kk1
}

type Sessions struct {
	transportRx []TransportRx
	transportTx []TransportTx
	kk1kk2Rx []Kk1Kk2Rx
	kk1kk2Tx []Kk1Kk2Tx
	kk1Rx []Kk1Rx
	kk1Tx []Kk1Tx
}

func (k TransportTx) insert(sessions Sessions) Sessions {
	sessions.transportTx = append(sessions.transportTx, k)
	return sessions
}

type TransportTx struct {
	theirid UserId
	secret Secret
	kk2 []byte
	plain []byte
}

type TransportRx struct {
	theirid UserId
	secret Secret
	kk1 []byte
	transport []byte
}

type Kk1Kk2Rx struct {
	kk1 []byte
	theirid UserId
	secret Secret
}

func (k Kk1Kk2Rx) insert(sessions Sessions) Sessions {
	sessions.kk1kk2Rx = append(sessions.kk1kk2Rx, k)
	return sessions
}

type Kk1Kk2Tx struct {
	theirid UserId
	secret Secret
	kk2 []byte
}

type Kk1Rx struct {
	theirid UserId
	kk1 []byte
}

type Kk1Tx struct {
	theirId UserId
	secret Secret
}

func addSessions(s1 Sessions, s2 Sessions) Sessions {
	return Sessions{
		transportRx: append(s1.transportRx, s2.transportRx...),
		kk1kk2Rx: append(s1.kk1kk2Rx, s2.kk1kk2Rx...),
		kk1kk2Tx: append(s1.kk1kk2Tx, s2.kk1kk2Tx...),
		kk1Rx: append(s1.kk1Rx, s2.kk1Rx...),
		kk1Tx: append(s1.kk1Tx, s2.kk1Tx...),
	}
}

func initSessions() Sessions {
	return Sessions{
		transportRx: make([]TransportRx, 0),
		transportTx: make([]TransportTx, 0),
		kk1kk2Rx: make([]Kk1Kk2Rx, 0),
		kk1kk2Tx: make([]Kk1Kk2Tx, 0),
		kk1Rx: make([]Kk1Rx, 0),
		kk1Tx: make([]Kk1Tx, 0),
	}
}

type Public struct {
	kk1s []Kk1
	kk2s []Kk2
	transports []KkTransport
}

func makeSessionsFor(
	contact UserId,
	public Public,
	secrets Secrets) (Sessions, error) {

	sessions := initSessions()
	for _, kk1 := range public.kk1s {
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
	for _, contact := range secrets.contacts {
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

func showTransportRx(t TransportRx, staticKeys noise.DHKey) (string, error) {
	shake, err := initShakeRx(t.secret, t.theirid, staticKeys)
	if err != nil {
		return "", err
	}

	_, _, _, err = shake.ReadMessage([]byte{}, t.kk1)
	if err != nil {
		return "", err
	}

	_, _, cipher, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return "", err
	}

	plain, err := cipher.Decrypt([]byte{}, cryptoAd, t.transport)
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

	for _, transport := range sessions.transportRx {
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
	fmt.Println("myid not implemented yet")
	return nil
}

type Help struct{}

func (Help) run() error {
	fmt.Println("help not implmented yet")
	return nil
}

type BadUserIdLength int

func (b BadUserIdLength) Error() string {
	return fmt.Sprintf("wrong length: expected 43, got %d", int(b))
}

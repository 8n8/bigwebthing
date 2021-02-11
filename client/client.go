package main

import (
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"database/sql"
	"errors"
	"fmt"
	"github.com/flynn/noise"
	"io"
	"io/ioutil"
	"net"
	"os"
)

var killCh chan struct{} = make(chan struct{})
var inCh chan In = make(chan In)

func main() {
	var state State
	var in In = Start{}

	go func() {
		for {
			out := in.update(&state)
			go out.run()
			in = <-inCh
		}
	}()

	<-killCh
}

type State struct {
	mode          int
	status        int
	staticKeys    noise.DHKey
	kk1           Kk1From
	conn          net.Conn
	handshake     *noise.HandshakeState
	readingSize   bool
	rx            *noise.CipherState
	tx            *noise.CipherState
	size          int
	cachingSecret bool
	secret        ClientSecret
}

// Modes
const UpdateCrypto = 1

// Statuses
const ListeningForNewKk1 = 1
const ListeningForXk2 = 2
const MakingClientSecret = 3
const MakingServerSecret = 4

// API indicators to server
const UploadKk2 = 0
const GiveMeNewKk1s = 1

// API indicators from server
const NoMoreMessages = 1

// Sizes
const SessionIdSize = 24
const dhlen = 32
const SecretSize = 2 * aes.BlockSize
const CryptoOverhead = 16
const Xk2FromServerSize = dhlen + 2*CryptoOverhead
const SizedXk3NewKk1Size = 1 + dhlen + CryptoOverhead + 1 + CryptoOverhead
const Kk1Size = 48
const Kk2Size = dhlen + 2*CryptoOverhead
const Kk2ToServerSize = 1 + CryptoOverhead + 1 + dhlen + Kk2Size

// Paths and URLs
const staticKeysPath = "staticKeys"
const dbPath = "sessionSecrets.sqlite"
const serverUrl = "localhost:3001"

// SQL
const cacheSecretSql = "INSERT INTO sessionsecrets (sessionid, secret) values (?, ?);"

// Inputs
type In interface {
	update(*State) Out
}

type Start struct{}

type Arguments []string

type NewStaticKeys struct {
	keys noise.DHKey
	err  error
}

type StaticKeysFile struct {
	raw []byte
	err error
}

type ServerConnection struct {
	conn net.Conn
	err  error
}

type ReadResult struct {
	raw []byte
	n   int
	err error
}

type DbHandle struct {
	db  *sql.DB
	err error
}

type CacheSecretResult struct {
	err error
}

type WrittenToServer struct {
	err error
}

type WrittenFile struct {
	err error
}

// Outputs

type Out interface {
	run()
}

type GetArgs struct{}

type ReadStaticKeysFile struct{}

type WriteStaticKeys []byte

type ConnectToServer struct{}

type MakeStaticKeys struct{}

type Read struct {
	reader io.Reader
	size   int
}

type ToServer struct {
	message []byte
	conn    net.Conn
}

type End struct{}

type GetDbHandle struct{}

type CacheSecret struct {
	id     [SessionIdSize]byte
	secret [SecretSize]byte
	db     *sql.DB
}

type Panic struct {
	err error
}

type Print string

type Sequence []Out

type DoNothing struct{}

// State transitions

func (Start) update(*State) Out {
	return GetArgs{}
}

func (args Arguments) update(s *State) Out {
	if len(args) == 2 && args[1] == "update" {
		s.mode = UpdateCrypto
		return ReadStaticKeysFile{}
	}

	return Sequence([]Out{Print("bad arguments\n"), End{}})
}

func (k NewStaticKeys) update(s *State) Out {
	if k.err != nil {
		return Panic{fmt.Errorf("bad static keys: %s", k.err)}
	}

	s.staticKeys = k.keys

	encoded := encodeStaticKeys(k.keys)
	written := WriteStaticKeys(encoded)

	return Sequence([]Out{written, ConnectToServer{}})
}

func (kf StaticKeysFile) update(s *State) Out {
	if kf.err != nil {
		return MakeStaticKeys{}
	}

	if s.mode == UpdateCrypto {
		staticKeys, err := parseStaticKeys(kf.raw)
		if err != nil {
			return Panic{fmt.Errorf("couldn't parse static keys: %s", err)}
		}

		s.staticKeys = staticKeys
		return ConnectToServer{}
	}

	return Panic{errors.New("bad mode in StaticKeysFile.update")}
}

func (c ServerConnection) update(s *State) Out {
	if c.err != nil {
		return badServer
	}

	s.status = MakingServerSecret
	s.conn = c.conn
	return Read{rand.Reader, SecretSize}
}

func (r ReadResult) update(s *State) Out {
	if s.mode == UpdateCrypto &&
		s.status == ListeningForXk2 &&
		!s.readingSize {

		return requestKk1s(r, s)
	}

	if s.mode == UpdateCrypto &&
		s.status == ListeningForNewKk1 &&
		!s.readingSize {

		return processKk1(r, s)
	}

	if s.mode == UpdateCrypto &&
		s.status == MakingClientSecret {

		return sendKk2(r, s)
	}

	if s.readingSize {
		if r.n != 1 {
			return badServer
		}

		s.size = int(r.raw[0])
		s.readingSize = false
		return Read{s.conn, int(s.size)}
	}

	if s.mode == UpdateCrypto && s.status == MakingServerSecret {
		return startServerHandshake(r, s)
	}

	return Panic{errors.New("unexpected ReadResult")}
}

func (d DbHandle) update(s *State) Out {
	if d.err != nil {
		return Panic{fmt.Errorf("no database handle: %s", d.err)}
	}

	if s.cachingSecret {
		return CacheSecret{s.secret.id, s.secret.secret, d.db}
	}

	return Panic{errors.New("unexpected database handle")}
}

func (c CacheSecretResult) update(*State) Out {
	if c.err != nil {
		return Panic{fmt.Errorf("couldn't cache secrets: %s", c.err)}
	}
	return DoNothing{}
}

func (w WrittenToServer) update(s *State) Out {
	if w.err != nil {
		return badServer
	}
	return DoNothing{}
}

func (w WrittenFile) update(s *State) Out {
	if w.err != nil {
		return Panic{fmt.Errorf("could't write file: %s", w.err)}
	}
	return DoNothing{}
}

// Output runners

func (GetArgs) run() {
	inCh <- Arguments(os.Args)
}

func (ReadStaticKeysFile) run() {
	contents, err := ioutil.ReadFile(staticKeysPath)
	inCh <- StaticKeysFile{contents, err}
}

func (w WriteStaticKeys) run() {
	inCh <- WrittenFile{ioutil.WriteFile(staticKeysPath, []byte(w), 0400)}
}

func (ConnectToServer) run() {
	conn, err := net.Dial("tcp", serverUrl)
	inCh <- ServerConnection{conn, err}
}

func (MakeStaticKeys) run() {
	keys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	inCh <- NewStaticKeys{keys, err}
}

func (r Read) run() {
	raw := make([]byte, r.size)
	n, err := r.reader.Read(raw)
	inCh <- ReadResult{raw, n, err}
}

func (t ToServer) run() {
	_, err := t.conn.Write(t.message)
	inCh <- WrittenToServer{err}
}

func (End) run() {
	killCh <- struct{}{}
}

func (GetDbHandle) run() {
	db, err := sql.Open("sqlite3", dbPath)
	inCh <- DbHandle{db, err}
}

func (c CacheSecret) run() {
	_, err := c.db.Exec(cacheSecretSql, c.id[:], c.secret[:])
	inCh <- CacheSecretResult{err}
}

func (p Panic) run() {
	panic(p.err.Error())
}

func (p Print) run() {
	fmt.Print(p)
}

func (s Sequence) run() {
	for _, out := range s {
		out.run()
	}
}

func (DoNothing) run() {
}

// Helper functions

func encodeStaticKeys(keys noise.DHKey) []byte {
	encoded := make([]byte, 0, 2*dhlen)
	encoded = append(encoded, keys.Private...)
	encoded = append(encoded, keys.Public...)
	return encoded
}

func parseStaticKeys(raw []byte) (noise.DHKey, error) {
	private := make([]byte, dhlen)
	n := copy(private, raw)
	if n != dhlen {
		return *new(noise.DHKey), fmt.Errorf("expecting %d bytes for Private key but got %d", dhlen, n)
	}

	public := make([]byte, dhlen)
	n = copy(public, raw[dhlen:])
	if n != dhlen {
		return *new(noise.DHKey), fmt.Errorf("expecting %d bytes for Public Key but got %d", dhlen, n)
	}

	return noise.DHKey{Private: private, Public: public}, nil
}

var badServer Out = Sequence([]Out{Print("bad server\n"), End{}})

func requestKk1s(r ReadResult, s *State) Out {
	if r.n != Xk2FromServerSize {
		return badServer
	}

	_, _, _, err := s.handshake.ReadMessage([]byte{}, r.raw[:r.n])
	if err != nil {
		return badServer
	}

	xk3 := make([]byte, 1, SizedXk3NewKk1Size)
	xk3[0] = SizedXk3NewKk1Size
	xk3, tx, rx, err := s.handshake.WriteMessage(
		xk3,
		[]byte{GiveMeNewKk1s})
	if err != nil {
		return Panic{fmt.Errorf("couldn't do WriteMessage in requestKk2s: %s", err)}
	}

	s.status = ListeningForNewKk1
	s.readingSize = true
	s.rx = rx
	s.tx = tx

	request := ToServer{xk3, s.conn}
	return Sequence([]Out{request, Read{s.conn, 1}})
}

func parseKk1From(raw []byte) (Kk1From, error) {
	const expected = 1 + Kk1Size + dhlen
	if len(raw) != expected {
		return *new(Kk1From), fmt.Errorf("expecting %d bytes but got %d", expected, len(raw))
	}

	var kk1 [Kk1Size]byte
	copy(kk1[:], raw[1:])

	var sender [dhlen]byte
	copy(sender[:], raw[1+Kk1Size:])

	return Kk1From{kk1, sender}, nil
}

type Kk1From struct {
	kk1    [Kk1Size]byte
	sender [dhlen]byte
}

var cryptoAd []byte = []byte{100, 117, 182, 195, 110, 70, 39, 86, 128, 240}

func processKk1(r ReadResult, s *State) Out {
	if r.n != s.size {
		return badServer
	}

	encrypted := r.raw[:r.n]
	plain, err := s.rx.Decrypt([]byte{}, cryptoAd, encrypted)
	if err != nil {
		return badServer
	}

	if len(plain) == 1 && plain[0] == NoMoreMessages {
		return End{}
	}

	kk1From, err := parseKk1From(plain)
	if err != nil {
		return badServer
	}

	s.kk1 = kk1From
	s.status = MakingClientSecret

	return Read{rand.Reader, SecretSize}
}

type RandomGen struct {
	stream cipher.Stream
}

func initRandomGen(secret [2 * aes.BlockSize]byte) (RandomGen, error) {
	block, err := aes.NewCipher(secret[:aes.BlockSize])
	if err != nil {
		return *new(RandomGen), err
	}

	stream := cipher.NewCTR(block, secret[aes.BlockSize:])
	return RandomGen{stream}, nil
}

func (r RandomGen) Read(p []byte) (int, error) {
	r.stream.XORKeyStream(p, p)
	return len(p), nil
}

func makeKkConfig(
	staticKeys noise.DHKey,
	theirId [dhlen]byte,
	randomGen RandomGen) noise.Config {

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

func sendKk2(r ReadResult, s *State) Out {
	if r.n != SecretSize {
		return Panic{errors.New("bad new secret size")}
	}

	var secret [SecretSize]byte
	copy(secret[:], r.raw)
	random, err := initRandomGen(secret)
	if err != nil {
		return Panic{fmt.Errorf("couldn't initialise random generator: %s", err)}
	}

	config := makeKkConfig(s.staticKeys, s.kk1.sender, random)
	shake, err := noise.NewHandshakeState(config)
	if err != nil {
		return Panic{fmt.Errorf("couldn't make new handshake state in sendKk2: %s", err)}
	}

	_, _, _, err = shake.ReadMessage([]byte{}, s.kk1.kk1[:])
	if err != nil {
		return badServer
	}

	kk2, _, _, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return Panic{fmt.Errorf("couldn't make kk2 in sendKk2: %s", err)}
	}

	s.readingSize = true
	s.status = ListeningForNewKk1

	plain := make([]byte, 1, 1+dhlen+Kk2Size)
	plain[0] = UploadKk2
	plain = append(plain, s.kk1.sender[:]...)
	plain = append(plain, kk2...)

	toServer := make([]byte, 1, Kk2ToServerSize)
	toServer[0] = Kk2ToServerSize
	toServer = s.tx.Encrypt(toServer, cryptoAd, plain)

	var sessionId [SessionIdSize]byte
	copy(sessionId[:], s.kk1.kk1[:])

	s.secret = ClientSecret{sessionId, secret}
	s.cachingSecret = true

	return Sequence([]Out{
		GetDbHandle{},
		ToServer{toServer, s.conn},
		Read{s.conn, 1}})
}

type ClientSecret struct {
	id     [SessionIdSize]byte
	secret [SecretSize]byte
}

func serverCipherSuite() noise.CipherSuite {
	return noise.NewCipherSuite(
		noise.DH25519,
		noise.CipherAESGCM,
		noise.HashSHA256)
}

var serverStaticKey = [dhlen]byte{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

func noiseServerConfig(staticKeys noise.DHKey, random io.Reader) noise.Config {
	return noise.Config{
		CipherSuite:   serverCipherSuite(),
		Random:        random,
		Pattern:       noise.HandshakeXK,
		Initiator:     true,
		StaticKeypair: staticKeys,
		PeerStatic:    serverStaticKey[:],
	}
}

func initServerShake(staticKeys noise.DHKey, secret [SecretSize]byte) (*noise.HandshakeState, error) {
	random, err := initRandomGen(secret)
	if err != nil {
		return new(noise.HandshakeState), err
	}

	config := noiseServerConfig(staticKeys, random)
	return noise.NewHandshakeState(config)
}

func startServerHandshake(r ReadResult, s *State) Out {
	var secret [SecretSize]byte
	if r.n != SecretSize {
		return Panic{
			errors.New("couldn't make session secret")}
	}
	copy(secret[:], r.raw)

	shake, err := initServerShake(s.staticKeys, secret)
	if err != nil {
		return Panic{fmt.Errorf("couldn't initialize server handshake in startServerHandshake: %s", err)}
	}

	xk1, _, _, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return Panic{fmt.Errorf("couldn't make xk1 in initServerShake: %s", err)}
	}

	s.handshake = shake
	s.status = ListeningForXk2
	s.readingSize = true

	return Sequence([]Out{ToServer{xk1, s.conn}, Read{s.conn, 1}})
}

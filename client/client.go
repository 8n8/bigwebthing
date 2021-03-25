package main

import (
	"bytes"
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"crypto/sha256"
	"database/sql"
	"encoding/hex"
	"fmt"
	"github.com/flynn/noise"
	"github.com/gorilla/websocket"
	_ "github.com/mattn/go-sqlite3"
	"github.com/mitchellh/go-homedir"
	"github.com/webview/webview"
	"golang.org/x/crypto/nacl/secretbox"
	"hash"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"path"
	"time"
)

const Xk2Size = 64

func main() {
	homeDir := getHomeDir()
	initDb(homeDir)
	staticKeys := getStaticKeys(homeDir)
	go httpServer(homeDir)
	go gui()
	go tcpConn(staticKeys)
	processor(homeDir, staticKeys)
}

const makeSessionTableSql = `
CREATE TABLE IF NOT EXISTS sessions (
	seed BLOB NOT NULL UNIQUE,
	id BLOB NOT NULL UNIQUE);
`

func dataPath(homeDir string) string {
	return path.Join(homeDir, "BigWebThing")
}

func dbPath(homeDir string) string {
	return path.Join(dataPath(homeDir), "database")
}

func initDb(homeDir string) {
	db := getDb(homeDir)
	defer db.Close()

	_, err := db.Exec(makeSessionTableSql)
	if err != nil {
		panic("couldn't make sessions table")
	}
}

func processor(homeDir string, staticKeys noise.DHKey) {
	state := initState(homeDir, staticKeys)
	for {
		processOne(&state)
	}
}

func initState(homeDir string, staticKeys noise.DHKey) State {
	return State{
		homeDir:    homeDir,
		staticKeys: staticKeys,
	}
}

type State struct {
	homeDir          string
	staticKeys       noise.DHKey
	blobIdOriginal   []byte
	blobIdExpecting  []byte
	blobKeyExpecting [32]byte
	blobAssemblePath string
	blobAssembleHash hash.Hash
	rxs              map[[sessionIdSize]byte]Session
	txs              map[[sessionIdSize]byte]Session
}

type Session struct {
	theirId     []byte
	cipherState *noise.CipherState
}

func processOne(state *State) {
	select {
	case fromServer := <-fromServerCh:
		processFromServer(state, fromServer)
	case fromWebsocket := <-fromWebsocketCh:
		processFromWebsocket(state, fromWebsocket)
	}
}

func processFromWebsocket(state *State, message []byte) {
	indicator := message[0]
	body := message[1:]

	switch indicator {
	case 0:
		processNewContact(state, body)
	case 1:
		processDeleteContact(state, body)
	case 2:
		processSendDocument(state, body)
	case 3:
		processRetrieveDocument(state, body)
	}

	panic("bad indicator from frontend: " + string(indicator))
}

const deleteContactSql = `
DELETE FROM contacts WHERE publickey=?;
`

func processDeleteContact(state *State, body []byte) {
	db := getDb(state.homeDir)
	defer db.Close()

	stmt, err := db.Prepare(deleteContactSql)
	if err != nil {
		panic("couldn't make database statement: " + deleteContactSql + ": " + err.Error())
	}
	defer stmt.Close()

	_, err = stmt.Exec(body)
	if err != nil {
		panic("couldn't delete contact from database: " + err.Error())
	}
}

const saveContactSql = `
INSERT INTO contacts(publickey) values (?);
`

func processNewContact(state *State, body []byte) {
	db := getDb(state.homeDir)
	defer db.Close()

	stmt, err := db.Prepare(saveContactSql)
	if err != nil {
		panic("couldn't make database statement: " + saveContactSql + ": " + err.Error())
	}
	defer stmt.Close()

	_, err = stmt.Exec(body)
	if err != nil {
		panic("couldn't insert new contact into database: " + err.Error())
	}
}

func makeSecret() [32]byte {
	var secret [32]byte
	n, err := rand.Read(secret[:])
	if n != 32 {
		panic(fmt.Sprintf("couldn't make secret: expecting %d bytes, but got %d", 32, n))
	}
	if err != nil {
		panic("couldn't make secret: " + err.Error())
	}
	return secret
}

func makeDocReader(homeDir string, doc []byte) io.Reader {
	readers := make([]io.Reader, 0)
	i := 0
	for i < len(doc) {
		indicator := doc[i]
		i++
		switch indicator {
		case 0:
			rawSize := doc[i : i+4]
			size := uint32P(rawSize)
			i += 4
			text := doc[i : i+size]
			i += size

			part := make([]byte, 1, 1+4+size)
			part[0] = 0
			part = append(part, rawSize...)
			part = append(part, text...)
			readers = append(readers, bytes.NewBuffer(part))

		case 1:
			id := doc[i : i+localUniqueSize]
			i += localUniqueSize

			path := kvPath(homeDir, hex.EncodeToString(id))
			f := makeFileReadClose(path)
			readers = append(readers, f)
		default:
			panic("bad indicator in document: " + string(indicator))
		}
	}
	return io.MultiReader(readers...)
}

type FileReadClose struct {
	f io.ReadCloser
}

func (f FileReadClose) Read(buf []byte) (int, error) {
	n, err := f.f.Read(buf)
	if err == io.EOF {
		f.f.Close()
	}
	return n, err
}

func makeFileReadClose(path string) io.Reader {
	f, err := os.Open(path)
	if err != nil {
		panic("couldn't open file: " + err.Error())
	}

	return FileReadClose{f}
}

func processSendDocument(state *State, message []byte) {
	recipient := message[:dhlen]
	doc := message[dhlen:]

	docReader := makeDocReader(state.homeDir, doc)

	secret := makeSecret()

	const maxChunkSize = 15892
	hash := sha256.New()
	chunkBuf := make([]byte, maxChunkSize)
	blobId := makeBlobId()
	for {
		n, err := docReader.Read(chunkBuf)
		hash.Write(chunkBuf[:n])
		if err == io.EOF {
			sum := hash.Sum([]byte{})[:checksumSize]
			plain := make([]byte, 1+checksumSize+n)
			plain[0] = 1
			copy(plain[1:], sum)
			copy(plain[1+checksumSize:], chunkBuf)
			sendBlob(plain, blobId, &secret)

			sendTransport(state, recipient, blobId, secret)
			return
		}

		plain := make([]byte, 1+blobIdSize+maxChunkSize)
		plain[0] = 0
		newBlobId := makeBlobId()
		copy(plain[1:], newBlobId)
		copy(plain[1+blobIdSize:], chunkBuf)
		sendBlob(plain, blobId, &secret)
		copy(blobId, newBlobId)
	}
}

func sendTransport(
	state *State,
	recipient []byte,
	blobId []byte,
	secret [32]byte) {

	plain := make([]byte, 56)
	copy(plain, blobId)
	copy(plain[blobIdSize:], secret[:])

	for sessionId, session := range state.txs {
		transport := session.cipherState.Encrypt(
			[]byte{}, clientAd, plain)
		message := make([]byte, 1, 129)
		message[0] = 2
		message = append(message, recipient...)
		message = append(message, transport...)
		message = append(message, sessionId[:]...)
		toServerCh <- message
		delete(state.txs, sessionId)
		return
	}
}

func makeBlobId() []byte {
	blobId := make([]byte, 24)
	n, err := rand.Read(blobId)
	if n != 24 {
		panic(fmt.Sprintf("couldn't get blob ID bytes: expecting %d, got %d", 24, n))
	}
	if err != nil {
		panic("couldn't make blob ID: " + err.Error())
	}
	return blobId
}

func makeNonce() [24]byte {
	var nonce [24]byte
	n, err := rand.Read(nonce[:])
	if n != 24 {
		panic(fmt.Sprintf("couldn't get nonce bytes: expecting %d, got %d", 24, n))
	}
	if err != nil {
		panic("couldn't make nonce: " + err.Error())
	}
	return nonce
}

func sendBlob(plain []byte, blobId []byte, secret *[32]byte) {
	nonce := makeNonce()
	encrypted := secretbox.Seal(nonce[:], plain, &nonce, secret)

	message := make([]byte, 1, 1+blobIdSize+len(encrypted))
	message[0] = 3
	message = append(message, blobId...)
	message = append(message, encrypted...)

	toServerCh <- message
}

func processRetrieveDocument(state *State, message []byte) {
	blobId := message[:blobIdSize]
	secret := message[blobIdSize:]

	toServer := make([]byte, 1, 25)
	toServer[0] = 5
	toServer = append(toServer, blobId...)

	toServerCh <- toServer

	state.blobIdOriginal = blobId
	state.blobIdExpecting = blobId
	copy(state.blobKeyExpecting[:], secret)
	state.blobAssemblePath = makeBlobAssemblePath(
		state.homeDir, blobId)
	state.blobAssembleHash = sha256.New()
}

func makeBlobAssemblePath(homeDir string, blobId []byte) string {
	return path.Join(
		assemblePath(homeDir),
		hex.EncodeToString(blobId))
}

func assemblePath(homeDir string) string {
	return path.Join(dataPath(homeDir), "assemble")
}

func processFromServer(state *State, message []byte) {
	indicator := message[0]
	body := message[1:]

	switch indicator {
	case 0:
		processKk1(state, body)
	case 1:
		processKk2(state, body)
	case 2:
		processKkTransport(state, body)
	case 3:
		processBlob(state, body)
	case 4:
		processPayment(state, body)
	case 5:
		processBlobUpload(state, body)
	}
	panic("bad indicator from server: " + string(indicator))
}

const kkTransportSize = 72

func processKk2(state *State, body []byte) {
	kk2 := body[:kk2Size]
	var sessionId [sessionIdSize]byte
	copy(sessionId[:], body[kk2Size:])

	seed, theirId, ok := getSeedFromDb(state.homeDir, sessionId[:])
	if !ok {
		return
	}

	shake, err := noise.NewHandshakeState(
		clientConfig(theirId, state.staticKeys, true, seed))
	if err != nil {
		panic("couldn't make client handshake: " + err.Error())
	}

	_, _, _, err = shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		panic("couldn't make KK1: " + err.Error())
	}

	_, tx, _, err := shake.ReadMessage([]byte{}, kk2)
	if err != nil {
		return
	}

	state.txs[sessionId] = Session{theirId, tx}
}

const getSeedSql = `
SELECT (seed, theirid) FROM sessions WHERE sessionid=?;
`

func getSeedFromDb(
	homeDir string,
	sessionId []byte) (
	[]byte,
	[]byte,
	bool) {

	db := getDb(homeDir)
	defer db.Close()

	rows, err := db.Query(getSeedSql, sessionId)
	if err != nil {
		panic("couldn't request seed from database: " + err.Error())
	}

	if !rows.Next() {
		return []byte{}, []byte{}, false
	}

	seed := make([]byte, 2*aes.BlockSize)
	theirId := make([]byte, dhlen)

	rows.Scan(&seed, &theirId)
	return seed, theirId, true
}

const kk2Size = 48

func processKkTransport(state *State, body []byte) {
	transport := body[:kkTransportSize]
	var sessionId [sessionIdSize]byte
	copy(sessionId[:], body[kkTransportSize:])
	timestamp := body[kkTransportSize+sessionIdSize:]

	session, ok := state.rxs[sessionId]
	if !ok {
		return
	}

	plain, err := session.cipherState.Decrypt([]byte{}, clientAd, transport)
	if err != nil {
		return
	}

	blobId := plain[:blobIdSize]
	secret := plain[blobIdSize:]

	message := make([]byte, 1, 93)
	message[0] = 1
	message = append(message, session.theirId...)
	message = append(message, blobId...)
	message = append(message, secret...)
	message = append(message, timestamp...)

	toWebsocketCh <- message
}

const blobIdSize = 24

func bytesEqual(as []byte, bs []byte) bool {
	for i, a := range as {
		if a != bs[i] {
			return false
		}
	}
	return true
}

func processBlob(state *State, body []byte) {
	blobId := body[:blobIdSize]
	if !bytesEqual(state.blobIdExpecting, blobId) {
		panic(fmt.Sprintf("received unexpected blob: expecting %v, got %v", state.blobIdExpecting, blobId))
	}

	blob := body[blobIdSize:]
	var nonce [24]byte
	copy(nonce[:], blob)
	encrypted := blob[24:]

	plain, ok := secretbox.Open([]byte{}, encrypted, &nonce, &state.blobKeyExpecting)
	if !ok {
		panic("couldn't decrypt blob from server")
	}

	indicator := plain[0]
	switch indicator {
	case 0:
		processNotFinalChunk(state, plain[1:])
	case 1:
		processFinalChunk(state, plain[1:])
	}
	panic(fmt.Sprintf("bad blob indicator: %d", indicator))
}

func processNotFinalChunk(state *State, blob []byte) {
	nextId := blob[:blobIdSize]
	chunk := blob[blobIdSize:]

	state.blobAssembleHash.Write(chunk)

	f, err := os.OpenFile(state.blobAssemblePath, os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0600)
	if err != nil {
		panic("couldn't open blob assembly file for not final chunk: " + err.Error())
	}

	_, err = f.Write(chunk)
	if err != nil {
		panic("couldn't write not final chunk to assembly file: " + err.Error())
	}
	f.Close()

	state.blobIdExpecting = nextId

	request := make([]byte, 25)
	request[0] = 5
	copy(request[1:], nextId)
	toServerCh <- request
}

const checksumSize = blobIdSize

func processFinalChunk(state *State, blob []byte) {
	checksum := blob[:checksumSize]
	chunk := blob[checksumSize:]

	state.blobAssembleHash.Write(chunk)
	hash := state.blobAssembleHash.Sum([]byte{})

	if !bytesEqual(checksum, hash) {
		panic(fmt.Sprintf("bad checksum on assembled file: expecting %v, but got %v", checksum, hash))
	}

	f, err := os.OpenFile(state.blobAssemblePath, os.O_APPEND|os.O_WRONLY, 0600)
	if err != nil {
		panic("couldn't open blob assembly file for final chunk: " + err.Error())
	}

	_, err = f.Write(chunk)
	if err != nil {
		panic("couldn't write final chunk to assembly file: " + err.Error())
	}
	f.Close()

	sendFileToFrontend(state)
}

func sendFileToFrontend(state *State) {
	f, err := os.Open(state.blobAssemblePath)
	if err != nil {
		panic("couldn't open temporary blob file: " + err.Error())
	}

	doc := make([]byte, 0)
	for {
		indicator := make([]byte, 1)
		n, err := f.Read(indicator)
		if n != 1 {
			break
		}
		if err != nil {
			panic("error reading from temporary blob file: " + err.Error())
		}

		switch indicator[0] {
		case 0:
			doc = append(doc, plainTextSegment(f)...)
		case 1:
			doc = append(doc, binarySegment(state, f)...)
		}
	}

	message := make([]byte, 1+blobIdSize+len(doc))
	message[0] = 6
	copy(message[1:], state.blobIdOriginal)
	copy(message[1+blobIdSize:], doc)

	toWebsocketCh <- message
}

func binarySegment(state *State, f *os.File) []byte {
	rawSize := make([]byte, 8)
	n, err := f.Read(rawSize)
	if n != 8 {
		panic(fmt.Sprintf("couldn't read enough bytes for binary size in document: expecting %d, got %d", 8, n))
	}
	if err != nil {
		panic("error reading binary size in document: " + err.Error())
	}

	size := uint48P(rawSize)
	id := makeLocalUnique()

	path := kvPath(state.homeDir, hex.EncodeToString(id))

	fBin, err := os.Create(path)
	if err != nil {
		panic("couldn't create file in key-value store for new binary: " + err.Error())
	}

	n_, err := io.CopyN(fBin, f, size)
	if n_ != size {
		panic(fmt.Sprintf("couldn't write binary to file in key-value store: expected %d bytes but got %d", size, n))
	}

	fBin.Close()

	message := make([]byte, 17)
	message[0] = 1
	copy(message[1:], id)
	return message
}

func kvPath(homeDir string, filename string) string {
	return path.Join(dataPath(homeDir), filename)
}

const localUniqueSize = 16

func makeLocalUnique() []byte {
	unique := make([]byte, localUniqueSize)
	n, err := rand.Read(unique)
	if n != localUniqueSize {
		panic(fmt.Sprintf("not enough bytes for local unique: expecting %d, got %d", localUniqueSize, n))
	}
	if err != nil {
		panic("error generating local unique: " + err.Error())
	}

	return unique
}

func uint48P(raw []byte) int64 {
	var n int64 = 0
	for i := 0; i < 8; i++ {
		n += int64(raw[i]) << (8 * i)
	}
	return n
}

func plainTextSegment(f *os.File) []byte {
	rawSize := make([]byte, 4)
	n, err := f.Read(rawSize)
	if n != 4 {
		panic(fmt.Sprintf("couldn't read enough bytes for text size in document: expecting %d, got %d", 4, n))
	}
	if err != nil {
		panic("error reading text size in document: " + err.Error())
	}

	size := uint32P(rawSize)
	encoded := make([]byte, 1+4+size)
	encoded[0] = 0

	n, err = f.Read(encoded[1+4:])
	if n != size {
		panic(fmt.Sprintf("couldn't read enough text from document: expecting %d, got %d", size, n))
	}
	if err != nil {
		panic("couldn't read text segment in document: " + err.Error())
	}

	return encoded
}

func uint32P(raw []byte) int {
	n := 0
	for i := 0; i < 4; i++ {
		n += int(raw[i]) << (8 * i)
	}
	return n
}

func processPayment(state *State, body []byte) {
	message := make([]byte, 9)
	message[0] = 5
	copy(message[1:], body)
	toWebsocketCh <- message
}

func processBlobUpload(state *State, body []byte) {
	message := make([]byte, 29)
	message[0] = 4
	copy(message[1:], body)
	toWebsocketCh <- message
}

type Csprng struct {
	stream cipher.Stream
}

func (c Csprng) Read(p []byte) (n int, err error) {
	c.stream.XORKeyStream(p, p)
	return len(p), nil
}

func initCsprng(seed []byte) Csprng {
	if len(seed) != 2*aes.BlockSize {
		panic(fmt.Sprintf("bad seed: expecting %d bytes, but got %d", 2*aes.BlockSize, len(seed)))
	}
	key := seed[:aes.BlockSize]
	iv := seed[aes.BlockSize:]

	block, err := aes.NewCipher(key)
	if err != nil {
		panic("couldn't make block: " + err.Error())
	}

	return Csprng{cipher.NewCTR(block, iv)}
}

func clientConfig(
	theirId []byte,
	staticKeys noise.DHKey,
	initiator bool,
	seed []byte) noise.Config {

	random := initCsprng(seed)
	return noise.Config{
		CipherSuite:   noise.NewCipherSuite(noise.DH25519, noise.CipherAESGCM, noise.HashSHA256),
		Random:        random,
		Pattern:       noise.HandshakeKK,
		Initiator:     initiator,
		StaticKeypair: staticKeys,
		PeerStatic:    theirId,
	}
}

const saveSeedSql = "INSERT INTO sessions(seed, theirid, sessionid) values (?, ?, ?);"

const sessionIdSize = 24

func getDb(homeDir string) *sql.DB {
	db, err := sql.Open("sqlite3", dbPath(homeDir))
	if err != nil {
		panic("couldn't get database connection: " + err.Error())
	}
	return db
}

func saveSeed(seed []byte, sessionId []byte, theirId []byte, homeDir string) {
	db := getDb(homeDir)
	defer db.Close()

	stmt, err := db.Prepare(saveSeedSql)
	if err != nil {
		panic("couldn't make database statement: " + err.Error())
	}
	defer stmt.Close()

	_, err = stmt.Exec(seed, theirId, sessionId)
	if err != nil {
		panic("couldn't insert seed into database: " + err.Error())
	}
}

func processKk1(state *State, body []byte) {
	sender := body[:dhlen]
	kk1 := body[dhlen:]

	if !inContacts(sender, state.homeDir) {
		return
	}

	seed := make([]byte, 2*aes.BlockSize)
	shake, err := noise.NewHandshakeState(
		clientConfig(sender, state.staticKeys, false, seed))
	if err != nil {
		panic("couldn't make new client handshake: " + err.Error())
	}

	saveSeed(seed, kk1[:sessionIdSize], sender, state.homeDir)

	encoded := make([]byte, 1, 105)
	encoded[0] = 1
	encoded = append(encoded, sender...)
	encoded, _, _, err = shake.WriteMessage(encoded, []byte{})
	if err != nil {
		panic("couldn't make KK2: " + err.Error())
	}

	encoded = append(encoded, kk1...)

	toServerCh <- encoded
}

const selectContact = "SELECT * FROM contacts WHERE publickey=?;"

func inContacts(theirId []byte, homeDir string) bool {
	db := getDb(homeDir)
	defer db.Close()

	rows, err := db.Query(selectContact, theirId)
	if err != nil {
		panic("couldn't query contacts table in database: " + err.Error())
	}

	return rows.Next()
}

func connSleep() {
	time.Sleep(10 * time.Second)
}

func tcpConn(staticKeys noise.DHKey) {
	for {
		oneTcpConn(staticKeys)
		time.Sleep(10 * time.Second)
	}
}

var serverPk = []byte{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

const serverUrl = "localhost:8080"

func serverConfig(staticKeys noise.DHKey) noise.Config {
	return noise.Config{
		CipherSuite:   noise.NewCipherSuite(noise.DH25519, noise.CipherAESGCM, noise.HashSHA256),
		Random:        rand.Reader,
		Pattern:       noise.HandshakeXK,
		Initiator:     true,
		StaticKeypair: staticKeys,
		PeerStatic:    serverPk,
	}
}

func oneTcpConn(staticKeys noise.DHKey) {
	conn, err := net.Dial("tcp", serverUrl)
	if err != nil {
		return
	}

	defer conn.Close()

	shake, err := noise.NewHandshakeState(serverConfig(staticKeys))
	if err != nil {
		panic("couldn't initialize Noise handshake: " + err.Error())
	}

	xk1, _, _, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		panic("couldn't make XK1: " + err.Error())
	}

	_, err = conn.Write(xk1)
	if err != nil {
		return
	}

	xk2 := make([]byte, Xk2Size)
	n, err := conn.Read(xk2)
	if n != Xk2Size {
		return
	}
	if err != nil {
		return
	}

	_, _, _, err = shake.ReadMessage([]byte{}, xk2)
	if err != nil {
		panic("couldn't read XK2: " + err.Error())
	}

	xk3, tx, rx, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		panic("couldn't make XK3: " + err.Error())
	}

	_, err = conn.Write(xk3)
	if err != nil {
		return
	}

	end := make(chan struct{})
	go func() {
		tcpReceiver(rx, conn)
		end <- struct{}{}
	}()
	go func() {
		tcpSender(tx, conn)
		end <- struct{}{}
	}()
	<-end
}

func tcpReceiver(rx *noise.CipherState, conn net.Conn) {
	rawSize := make([]byte, 2)
	n, err := conn.Read(rawSize)
	if n != 2 {
		return
	}
	if err != nil {
		return
	}

	size := int(rawSize[0]) + (int(rawSize[1]) << 8)

	encrypted := make([]byte, size)
	n, err = conn.Read(encrypted)
	if n != size {
		return
	}
	if err != nil {
		return
	}

	plain, err := rx.Decrypt([]byte{}, serverAd, encrypted)
	if err != nil {
		panic("bad decryption of message from server: " + err.Error())
	}

	fromServerCh <- plain
}

var toServerCh chan []byte = make(chan []byte)

var fromServerCh chan []byte = make(chan []byte)

const authSize = 16

var serverAd = []byte{235, 77, 23, 199, 11, 162, 118, 80, 65, 84, 165, 211, 116, 185, 150, 149}

var clientAd = []byte{216, 250, 225, 37, 41, 14, 211, 157, 57, 37, 19, 161, 188, 13, 147, 209}

func tcpSender(tx *noise.CipherState, conn net.Conn) {
	for {
		if oneTcpSend(tx, conn) != nil {
			return
		}
	}
}

func oneTcpSend(tx *noise.CipherState, conn net.Conn) error {
	message := <-toServerCh
	encryptedSize := authSize + len(message)
	toServer := make([]byte, 2, 2+encryptedSize)
	toServer[0] = byte(encryptedSize & 0xFF)
	toServer[1] = byte((encryptedSize >> 8) & 0xFF)
	toServer = tx.Encrypt(toServer, serverAd, message)
	_, err := conn.Write(toServer)
	return err
}

func gui() {
	w := webview.New(false)
	defer w.Destroy()
	w.SetTitle("BigWebThing")
	w.SetSize(800, 600, webview.HintNone)
	w.Navigate("http://localhost" + clientPort)
	w.Run()
}

const clientPort = ":9724"

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func indexPath(homeDir string) string {
	return path.Join(homeDir, "index.html")
}

func httpServer(homeDir string) {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		f, err := os.Open(indexPath(homeDir))
		if err != nil {
			panic("couldn't read index.html: " + err.Error())
		}
		defer f.Close()
		_, err = io.Copy(w, f)
		if err != nil {
			panic("couldn't send index.html: " + err.Error())
		}
	})
	http.HandleFunc("/websocket", func(w http.ResponseWriter, r *http.Request) {
		conn, err := upgrader.Upgrade(w, r, nil)
		if err != nil {
			panic("couldn't upgrade websocket: " + err.Error())
		}
		handleWebsockets(conn, homeDir)
	})
	http.HandleFunc("/open/", func(w http.ResponseWriter, r *http.Request) {
		path := kvPath(homeDir, r.URL.Path[len("/open/"):])
		f, err := os.Open(path)
		if err != nil {
			panic("couldn't read " + path + ": " + err.Error())
		}
		defer f.Close()
		_, err = io.Copy(w, f)
		if err != nil {
			panic("couldn't send " + path + ": " + err.Error())
		}
	})
	http.HandleFunc("/save/", func(w http.ResponseWriter, r *http.Request) {
		path := kvPath(homeDir, r.URL.Path[len("/save/"):])
		f, err := os.Create(path)
		if err != nil {
			panic("couldn't read " + path + ": " + err.Error())
		}
		defer f.Close()
		_, err = io.Copy(f, r.Body)
		if err != nil {
			panic("couldn't write blob to file: " + path + ": " + err.Error())
		}
	})
	http.ListenAndServe(":9001", nil)
}

var fromWebsocketCh = make(chan []byte)

func readWebsocket(conn *websocket.Conn, homeDir string) {
	for {
		_, message, err := conn.ReadMessage()
		if err != nil {
			panic("couldn't read from websocket: " + err.Error())
		}

		fromWebsocketCh <- message
	}
}

var toWebsocketCh = make(chan []byte)

func writeWebsocket(conn *websocket.Conn, homeDir string) {
	for {
		err := conn.WriteMessage(websocket.BinaryMessage, <-toWebsocketCh)
		if err != nil {
			panic("couldn't write websocket: " + err.Error())
		}
	}
}

func handleWebsockets(conn *websocket.Conn, homeDir string) {
	go readWebsocket(conn, homeDir)
	writeWebsocket(conn, homeDir)
}

func getHomeDir() string {
	home, err := homedir.Dir()
	if err != nil {
		panic("couldn't get home dir: " + err.Error())
	}
	return home
}

func staticKeysPath(homePath string) string {
	return path.Join(homePath, "staticKeys")
}

func makeKeys(homePath string) noise.DHKey {
	keys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	if err != nil {
		panic("couldn't generate static keys: " + err.Error())
	}

	encoded := make([]byte, 2*dhlen)
	copy(encoded, keys.Private)
	copy(encoded[:dhlen], keys.Public)
	err = ioutil.WriteFile(staticKeysPath(homePath), encoded, 0400)
	if err != nil {
		panic("couldn't write static keys file: " + err.Error())
	}
	return keys
}

func getStaticKeys(homePath string) noise.DHKey {
	raw, err := ioutil.ReadFile(staticKeysPath(homePath))
	if err != nil {
		return makeKeys(homePath)
	}

	if len(raw) != 2*dhlen {
		panic(fmt.Sprintf("bad static keys file: expecting %d bytes, but got %d", 2*dhlen, len(raw)))
	}

	return noise.DHKey{
		Private: raw[:dhlen],
		Public:  raw[dhlen:],
	}
}

const dhlen = 32

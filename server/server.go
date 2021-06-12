package main

import (
	"github.com/flynn/noise"
	"fmt"
	"crypto/rand"
	"net"
	"os"
	"time"
	"errors"
	"io/ioutil"
	"database/sql"
	_ "github.com/mattn/go-sqlite3"
)

func main() {
	keys := getStaticKeys()
	db := setupDb()
	defer db.Close()

	listener, err := net.Listen("tcp", ":4040")
	if err != nil {
		panic(err)
	}

	newSessions := make(chan Session)
	go setupSessions(listener, keys, newSessions)

	toSends := make(chan ToSend)
	go sendOutMessages(toSends)

	rawMessages := make(chan RawMessage)
	sessions := make(map[string]Session)
	for {
		select {
		case s := <-newSessions:
			sessions[s.conn.RemoteAddr().String()] = s
			go listenToConn(s.conn, rawMessages)
			sendThemAllTheirStuff(s, db, toSends)

		case raw := <-rawMessages:
			respond(raw, sessions, db, toSends)
		}
	}
}

func sendOutMessages(toSends chan ToSend) {
	for {
		s <- toSends
		go func() {
			_, err := s.conn.Write(s.message)
			if err != nil {
				return
			}
		}
	}
}

func respond(
	raw []byte,
	sessions map[string]Session,
	db *sql.DB,
	toSends chan ToSend) {


}

func setupSessions(
	listener net.Listener,
	keys noise.DHKey,
	newSessions chan Session) {

	for {
		conn, err := listener.Accept()
		if err != nil {
			continue
		}
		go func() {
			s, err := doHandshake(conn, keys)
			if err != nil {
				return
			}
			newSessions <- Session{
				conn: conn,
				receive: s.receive,
				send: s.send,
				theirId: s.theirId,
			}
		}()
	}
}

func listenToConn(conn net.Conn, rawMessages chan RawMessage) {
	for {
		raw, err := readRaw(conn)
		if err != nil {
			return
		}

		rawMessages <- RawMessage{
			conn: conn,
			raw: raw,
		}
	}
}

type ToSend struct {
	conn net.Conn
	message []byte
}

const databasePath = "database.sqlite3"

func setupDb() *sql.DB {
	db, err := sql.Open("sqlite3", databasePath)
	if err != nil {
		panic(err)
	}

	for _, createTable := range createTables {
		_, err = db.Exec(createTable)
		if err != nil {
			panic(err)
		}
	}

	return db
}

type Sender struct {
	send *noise.CipherState
	theirId []byte
	conn net.Conn
}

func handleConn(
	conn net.Conn,
	keys noise.DHKey,
	messages chan RawMessage,
	senders chan Sender) {

	s, err := doHandshake(conn, keys)
	if err != nil {
		return
	}

	senders <- Sender{
		send: s.send,
		theirId: s.theirId,
		conn: conn,
	}

	for {
		message, err := readRaw(conn)
		if err != nil {
			return
		}

		messages <- RawMessage{
			conn: conn,
			raw: message,
		}
	}
}

func readRaw(conn net.Conn) ([]byte, error) {
	rawlen := make([]byte, 2)
	n, err := conn.Read(rawlen)
	if err != nil {
		return []byte{}, err
	}
	if n != 2 {
		return []byte{}, errors.New("not enough bytes for message length")
	}

	len_ := uint16(rawlen[0]) + uint16(rawlen[1]) * 256
	if len_ > 15998 {
		return []byte{}, errors.New("message length is greater than 15998 bytes")
	}
	message := make([]byte, len_)
	n, err = conn.Read(message)
	if err != nil {
		return []byte{}, err
	}
	if n != int(len_) {
		return []byte{}, errors.New("not enough bytes for message")
	}

	return message, nil
}

type RawMessage struct {
	conn net.Conn
	raw []byte
}

const messagesDir = "messages"

func loadMessages() [][]byte {
	fileInfos, err := ioutil.ReadDir(messagesDir)
	if err != nil {
		panic(err)
	}

	messages := make([][]byte, len(fileInfos))
	for i, fileInfo := range fileInfos {
		path := messagesDir + "/" + fileInfo.Name()
		message, err := os.ReadFile(path)
		if err != nil {
			panic(err)
		}
		messages[i] = message
	}

	return messages
}

func noiseConfig(staticKeys noise.DHKey) noise.Config {
	return noise.Config{
		CipherSuite: noise.NewCipherSuite(
			noise.DH25519,
			noise.CipherChaChaPoly,
			noise.HashBLAKE2s),
		Pattern: noise.HandshakeXK,
		Initiator: false,
		StaticKeypair: staticKeys,
	}
}

func makeHandshakeState(
	staticKeys noise.DHKey) *noise.HandshakeState {

	config := noiseConfig(staticKeys)
	handshake, err := noise.NewHandshakeState(config)
	if err != nil {
		panic(err)
	}
	return handshake
}

type Session struct {
	conn net.Conn
	receive *noise.CipherState
	send *noise.CipherState
	theirId []byte
}

func send(s ToSend) {
	_, err := s.conn.Write(s.message)
	if err != nil {
		s.conn.Close()
	}
}

const tcpDeadline = 5 * time.Second

func readXk1(conn net.Conn, handshake *noise.HandshakeState) error {
	err := conn.SetDeadline(time.Now().Add(tcpDeadline))
	if err != nil {
		return err
	}

	xk1 := make([]byte, xk1len)
	n, err := conn.Read(xk1)
	if err != nil {
		return err
	}
	if n != xk1len {
		return errors.New("XK1 too short")
	}

	_, _, _, err = handshake.ReadMessage([]byte{}, xk1)
	return err
}

func sendXk2(conn net.Conn, handshake *noise.HandshakeState) error {
	err := conn.SetDeadline(time.Now().Add(tcpDeadline))
	if err != nil {
		return err
	}

	xk2, _, _, err := handshake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return err
	}

	_, err = conn.Write(xk2)
	return err
}

type CryptoState struct {
	send *noise.CipherState
	receive *noise.CipherState
	theirId []byte
}

func readXk3(
	conn net.Conn,
	handshake *noise.HandshakeState) (CryptoState, error) {

	var s CryptoState

	err := conn.SetDeadline(time.Now().Add(tcpDeadline))
	if err != nil {
		return s, err
	}

	xk3 := make([]byte, xk3len)
	n, err := conn.Read(xk3)
	if err != nil {
		return s, err
	}
	if n != xk3len {
		return s, errors.New("XK3 too short")
	}

	_, s.send, s.receive, err = handshake.ReadMessage(
		[]byte{},
		xk3)
	if err != nil {
		return s, err
	}

	s.theirId = handshake.PeerStatic()
	return s, nil
}

func doHandshake(
	conn net.Conn,
	staticKeys noise.DHKey) (
	CryptoState,
	error) {

	handshake := makeHandshakeState(staticKeys)

	var s CryptoState

	err := readXk1(conn, handshake)
	if err != nil {
		return s, err
	}

	err = sendXk2(conn, handshake)
	if err != nil {
		return s, err
	}

	s, err = readXk3(conn, handshake)
	if err != nil {
		return s, err
	}

	return s, nil
}

const getKk1sFromOthersSql = `
SELECT timestamp, recipient, sender, kk1
FROM kk1
WHERE recipient=?;
`

const createKk1Table = `
CREATE TABLE IF NOT EXISTS kk1 (
	timestamp INTEGER NOT NULL,
	recipient BLOB NOT NULL,
	sender BLOB NOT NULL,
	kk1 BLOB UNIQUE NOT NULL
);
`

const createKk2Table = `
CREATE TABLE IF NOT EXISTS kk2 (
	timestamp INTEGER NOT NULL,
	recipient BLOB NOT NULL,
	sender BLOB NOT NULL,
	sessionid BLOB UNIQUE NOT NULL,
	kk2 BLOB UNIQUE NOT NULL
);
`

const createKkTransportTable = `
CREATE TABLE IF NOT EXISTS kktransport (
	timestamp INTEGER NOT NULL,
	recipient BLOB NOT NULL,
	sender BLOB NOT NULL,
	sessionid BLOB UNIQUE NOT NULL,
	blobid BLOB NOT NULL,
	kktransport BLOB UNIQUE NOT NULL
);
`

const createBlobTable = `
CREATE TABLE IF NOT EXISTS blob (
	timestamp INTEGER NOT NULL,
	author BLOB NOT NULL,
	id BLOB NOT NULL,
	counter INTEGER NOT NULL,
	PRIMARY KEY (id, counter)
);
`

const createAddContactTable = `
CREATE TABLE IF NOT EXISTS addcontact (
	timestamp INTEGER NOT NULL,
	contacter BLOB NOT NULL,
	contactee BLOB NOT NULL
);
`

const createRemoveContactTable = `
CREATE TABLE IF NOT EXISTS removecontact (
	timestamp INTEGER NOT NULL,
	contacter BLOB NOT NULL,
	contactee BLOB NOT NULL
);
`

const createPaymentTable = `
CREATE TABLE IF NOT EXISTS payment (
	timestamp INTEGER NOT NULL,
	contacter BLOB NOT NULL,
	contactee BLOB NOT NULL
);
`

const createGetDataOfTable =`
CREATE TABLE IF NOT EXISTS getdataof (
	timestamp INTEGER NOT NULL,
	theirid BLOB NOT NULL
);
`

const createGetBlobTable = `
CREATE TABLE IF NOT EXISTS getblob (
	timestamp INTEGER NOT NULL,
	id BLOB NOT NULL
);
`

var createTables = []string{
	createKk1Table,
	createKk2Table,
	createKkTransportTable,
	createBlobTable,
	createAddContactTable,
	createRemoveContactTable,
	createPaymentTable,
	createGetDataOfTable,
	createGetBlobTable,
}

func getStaticKeys() noise.DHKey {
	raw, err := os.ReadFile(staticKeysPath)
	if err == nil {
		return noise.DHKey{
			Public: raw[:dhlen],
			Private: raw[dhlen: dhlen*2],
		}
	}

	if !os.IsNotExist(err) {
		panic(err)
	}

	keys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	if err != nil {
		panic(err)
	}
	err = os.WriteFile(staticKeysPath, encodeKeys(keys), 0400)
	if err != nil {
		panic(err)
	}
	return keys
}

func encodeKeys(keys noise.DHKey) []byte {
	encoded := make([]byte, 2*dhlen)
	copy(encoded, keys.Public)
	copy(encoded, keys.Private)
	return encoded
}

const dhlen = 32

const staticKeysPath = "staticKeys"

func serializeKeys(keys noise.DHKey) []byte {
	result := make([]byte, 2 * idlen, 2 * idlen)
	copy(result, keys.Private)
	copy(result[idlen:], keys.Public)
	return result
}

const messagesPath = "messages"

const maxInt32 = 2147483647

func intPow(base, power int) int {
	result := 1
	for p := 0; p < power; p++ {
		result = result * base
	}
	return result
}

func decodeUint32(b []byte) uint32 {
	b0 := uint32(b[0])
	b1 := uint32(b[1])
	b2 := uint32(b[2])
	b3 := uint32(b[3])
	return b0 + b1 * 256 + b2 * 256 * 256 + b3 * 256 * 256 * 256
}

func parseAmount(raw []byte, pos int) (uint32, int, error) {
	if len(raw) < pos + 4 {
		return 0, pos, fmt.Errorf("not enough bytes for amount")
	}

	amount := decodeUint32(raw[pos: pos+4])
	return amount, pos+4, nil
}

func parseBlobCounter(raw []byte, pos int) (uint32, int, error) {
	if len(raw) < pos + 4 {
		return 0, pos, fmt.Errorf("not enough bytes for blob counter")
	}

	counter := decodeUint32(raw[pos: pos+4])
	return counter, pos+4, nil
}

const (
	idlen = 32
	xk1len = 48
	xk3len = 64
	kk1len = 48
	kk2len = 48
	kktransportlen = 48
	hashlen = 32
)

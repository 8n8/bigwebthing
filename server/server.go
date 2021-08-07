package main

import (
	"database/sql"
	_ "github.com/mattn/go-sqlite3"
	"github.com/flynn/noise"
	"os"
	"net"
	"fmt"
	"errors"
	"encoding/hex"
	"golang.org/x/crypto/blake2s"
)

var ad = []byte("Authenticated data for BigWebThing messages between client and server. BigWebThing is a system for sharing apps.")

func main() {
	auths := make(chan Auth)
	go getAuths(auths)

	db := setupDb()

	deadConn := make(chan string)

	send := make(chan Send)
	go func() {
		for {
			m := <-send
			_, err := m.conn.Write(m.msg)
			if err != nil {
				m.conn.Close()
				deadConn<-m.conn.RemoteAddr().String()
			}
		}
	}()

	receiveds := make(chan Received)

	for {
		select {
		case auth := <-auths:
			sendThemAlltheirStuff(auth, db, send)
			go readMessage(auth, receiveds)
			db.auths = append(db.auths, auth)

		case r := <-receiveds:
			r.msg.handle(r.auth, db, send)

		case deleteConn(&db, <-deadConn):
		}
	}
}

func deleteConn(db *Db, address string) {
	for i, a := range db.conns {
		if a.conn.RemoteAddr().String() == address {
			db.conns[i] = db.conns(len(db.conns) - 1)
			db.conns = db.conns[:len(db.conns) - 1]
			return
		}
	}
}

type Received struct {
	msg Parsed
	auth Auth
}

func readMessage(
	auth Auth,
	receiveds chan Received,
	dead chan string) {

	err := readMessageErr(auth, receiveds)
	if err != nil {
		dead <- auth.conn.RemoteAddr().String()
		auth.conn.Close()
	}
}

func parseUint16(raw [2]byte) int {
	result := 0
	result += int(raw[0])
	result += int(raw[1]) * 256
	return result
}

type Parsed interface {
	handle(Auth, *sql.DB, chan Send)
}

func readMessageErr(auth Auth, receiveds chan Received) error {
	var sizebuf [2]byte
	n, err := auth.conn.Read(sizebuf[:])
	if n != 2 {
		return fmt.Errorf(
			"bad size size: expecting 2: got %d",
			n)
	}
	if err != nil {
		return fmt.Errorf("bad size read: %s", err)
	}

	size := parseUint16(sizebuf)
	if size > maxMsgSize {
		return fmt.Errorf(
			"bad message size: expecting %d, got %d",
			maxMsgSize,
			size)
	}

	msgbuf := make([]byte, size)
	n, err = auth.conn.Read(msgbuf)
	if n != size {
		return fmt.Errorf(
			"bad message length: expecting %d, got %d",
			size,
			n)
	}
	if err != nil {
		return fmt.Errorf("bad message read: %s", err)
	}

	plain, err := auth.rx.Decrypt([]byte{}, ad, msgbuf)
	if err != nil {
		return fmt.Errorf("bad message decrypt: %s", err)
	}

	var received Received
	received.msg, err = parse(plain)
	if err != nil {
		return fmt.Errorf("bad message parse: %s", err)
	}

	received.auth = auth
	receiveds <- received

	return nil
}

func parse(raw []byte) (Parsed, error) {
	if len(raw) == 0 {
		return nil, errors.New("empty message")
	}

	switch raw[0] {
	case 0:
		return parsePeerToPeer(raw[1:])
	}

	return nil, fmt.Errorf("bad message indicator: %d", raw[0])
}

// This is a message from one user to another:
// 
// 	32 bytes: recipient
// 	the rest: the message
// 
func parsePeerToPeer(raw []byte) (Parsed, error) {
	if len(raw) < dhlen + 1 {
		return nil, fmt.Errorf(
			"message is too short: expecting at least %d bytes, but got %d",
			33,
			len(raw))
	}

	var to [dhlen]byte
	copy(to[:], raw)
	return To{
		to: recipient,
		message: raw[dhlen:],
	}, nil
}

type To struct {
	to [dhlen]byte
	message []byte
}

func (t To) handle(auth Auth, db *Db, send chan Send) {
	fromTo := FromTo{
		from: auth.publickey,
		to: t.to,
		message: t.message,
	}

	_, ok := db.contacts[Contact{t.to, auth.publickey}]
	if !ok {
		return
	}

	db.fromTo = append(db.fromTo, fromTo)

	f, err := os.OpenFile(metadataPath, fappend, 0600)
	if err != nil {
		panic("can't append to metadata file: " + err.Error())
	}

	hash := blake2s.Sum256(t.message)
	path := "blobs/" + hex.EncodeToString(hash[:])

	err = os.WriteFile(path, t.message, 0400)
	if err != nil {
		panic(fmt.Sprintf(
			"couldn't write blob file: %s: %s",
			path,
			err))
	}

	other, ok := db.conns[auth.conn.RemoteAddr().String()]
	if !ok {
		return
	}

	plain := encodeFrom(From{auth.publickey, t.message})
	size := 16 + len(plain)
	encrypted := make([]byte, 2, 2 + size)
	encrypted[0] = byte(size & 0xFF)
	encrypted[1] = byte((size >> 8) & 0xFF)
	encrypted, err = other.tx.Encrypt(encrypted, ad, plain)
	if err != nil {
		return
	}

	send <- Send{other.conn, encrypted}
}

type Send struct {
	conn net.Conn
	msg []byte
}

func sender(ch chan Send) {
	for {
		send := <-ch
		_, err := send.conn.Write(send.msg)
	}
}

type Auth struct {
	conn net.Conn
	tx *noise.CipherState
	rx *noise.CipherState
	publickey [dhlen]byte
}

const (
	xx1len = 48
	xx3len = 48
	dhlen = 32
	maxMsgSize = 16000
	metadataPath = "metadata"
	fappend = os.O_APPEND|os.O_CREATE|os.O_WRONLY
)

func getAuths(auths chan Auth) {
	certificate := getCertificate()

	noiseConfig := noise.Config{
		StaticKeypair: getKeys(),
		CipherSuite: noise.NewCipherSuite(
			noise.DH25519,
			noise.CipherChaChaPoly,
			noise.HashBLAKE2s),
		Pattern: noise.HandshakeXX,
		Initiator: false}

	listener, err := net.Listen("tcp", "8080")
	if err != nil {
		panic("couldn't start TCP server: " + err.Error())
	}

	for {
		conn, err := listener.Accept()
		if err != nil {
			continue
		}
		go doHandshake(conn, auths, certificate, noiseConfig)
	}
}

func doHandshake(
	conn net.Conn,
	auths chan Auth,
	certificate []byte,
	noiseConfig noise.Config) {

	var xx1 [xx1len]byte
	n, err := conn.Read(xx1[:])
	if n != xx1len || err != nil {
		return
	}

	handshake, err := noise.NewHandshakeState(noiseConfig)
	if err != nil {
		panic("couldn't initialise handshake: " + err.Error())
	}

	_, _, _, err = handshake.ReadMessage([]byte{}, xx1[:])
	if err != nil {
		return
	}

	xx2, _, _, err := handshake.WriteMessage([]byte{}, certificate)
	if err != nil {
		return
	}

	_, err = conn.Write(xx2)
	if err != nil {
		return
	}

	var xx3 [xx3len]byte
	n, err = conn.Read(xx3[:])
	if n != xx3len || err != nil {
		return
	}

	var auth Auth
	_, auth.rx, auth.tx, err = handshake.ReadMessage(
		[]byte{}, xx3[:])
	if err != nil {
		return
	}

	copy(auth.publickey[:], handshake.PeerEphemeral())
	auths <- auth
}

type Xx3 struct {
	xx3 [xx3len]byte
	conn net.Conn
	handshake *noise.HandshakeState
}

type Xx1 struct {
	xx1 [xx1len]byte
	conn net.Conn
}


func getKeys() noise.DHKey {
	raw, err := os.ReadFile("keys")
	if err != nil {
		panic("couldn't read keys file: " + err.Error())
	}

	return noise.DHKey{
		Private: raw[:dhlen],
		Public: raw[dhlen:2*dhlen],
	}
}

func getCertificate() []byte {
	contents, err := os.ReadFile("certificate")
	if err != nil {
		panic("couldn't read certificate file: " + err.Error())
	}

	return contents
}

func zeroDb() Db {
	return Db{
		fromTo: make([]FromTo, 0),
		users: make([]User, 0),
		conns: make(map[string]Auth),
		contacts: make(map[Contact]struct{}),
	}
}

func setupDb() Db {
	raw, err := os.ReadFile(metadataPath)
	if os.IsNotExist(err) {
		return zeroMetadata()
	}

	parsed, err := parseMetadata(raw)
	if err != nil {
		panic("couldn't parse metadata file: " + err.Error())
	}

	db := zeroDb()
	db.fromTo

	return parsed
}

func parseMetadata(raw []byte) (Metadata, error) {
	
}

type Db struct {
	fromTo []FromTo
	users []User
	conns map[string]Auth
	contacts map[Contact]struct{}
}

type User struct {
	id uint64
	url string
	salt [dhlen]byte
	publickey [dhlen]byte
}

type Contact struct {
	contactee [dhlen]byte
	contacter [dhlen]byte
}

type FromTo struct {
	from [dhlen]byte
	to [dhlen]byte
	message []byte
}

type From struct {
	from [dhlen]byte
	message []byte
}

func encodeFrom(f From) []byte {
	result := make([]byte, 1 + dhlen + len(f.message))
	result[0] = 0
	copy(result[1:], f.from[:])
	copy(result[1+dhlen:], f.message)
	return result
}

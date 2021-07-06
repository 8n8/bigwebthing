package client

import (
	"crypto/rand"
	"database/sql"
	"encoding/base64"
	"fmt"
	"github.com/flynn/noise"
	"net"
	"os"
)

const xk2Len = 64

const serverUrl = "localhost:4040"

var serverStaticKey = []byte{
	0xf7, 0x8e, 0x78, 0x33, 0x31, 0x83, 0xea, 0x5b, 0xa9, 0x68, 0x1a, 0x74, 0xd0, 0x4b, 0xb7, 0xf4, 0xa8, 0xda, 0x86, 0xf1, 0xb8, 0xae, 0xad, 0x95, 0x8c, 0xfa, 0xc, 0x8d, 0x26, 0x8, 0x25, 0x73}

func AddContact(publicId, localId string) {
	err := addContactErr(publicId, localId)
	if err != nil {
		fmt.Printf("%s", err)
	}
}

func addContactErr(publicId, localId string) error {
	staticKey := getStaticKey()
	handshake, err := noise.NewHandshakeState(noise.Config{
		CipherSuite: noise.NewCipherSuite(
			noise.DH25519,
			noise.CipherChaChaPoly,
			noise.HashBLAKE2s),
		Random:        rand.Reader,
		Pattern:       noise.HandshakeXK,
		Initiator:     true,
		StaticKeypair: staticKey,
		PeerStatic:    serverStaticKey,
	})
	if err != nil {
		panic(err)
	}

	xk1, _, _, err := handshake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return err
	}

	conn, err := net.Dial("tcp", serverUrl)
	if err != nil {
		return err
	}
	defer conn.Close()

	_, err = conn.Write(xk1)
	if err != nil {
		return err
	}

	xk2 := make([]byte, xk2Len)
	n, err := conn.Read(xk2)
	if err != nil {
		return err
	}
	if n != xk2Len {
		return NotEnoughXk2Bytes(n)
	}

	_, _, _, err = handshake.ReadMessage([]byte{}, xk2)
	if err != nil {
		return err
	}

	xk3, tx, _, err := handshake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return err
	}

	_, err = conn.Write(xk3)
	if err != nil {
		return err
	}

	addContact := make([]byte, 1+addContactLen)
	addContact[0] = 1
	addContact[1] = addContactId

	contacter, err := base64.RawURLEncoding.DecodeString(publicId)
	if err != nil {
		return err
	}
	copy(addContact[1+1+4:], contacter)
	copy(addContact[1+1+4+32:], staticKey.Public)

	encrypted := make(
		[]byte, 2+1, 2+1+addContactLen+authTagLen)
	copy(encrypted, encodeUint16(1+addContactLen+authTagLen))

	_, err = tx.Encrypt(encrypted, ad, addContact)
	if err != nil {
		return err
	}

	_, err = conn.Write(encrypted)
	if err != nil {
		return err
	}

	return saveNewContact(contacter, localId)
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

const createSessionsTable = `
CREATE TABLE IF NOT EXISTS session (
	seed BLOB NOT NULL,
	sessionid BLOB UNIQUE NOT NULL,
	txorrx INTEGER NOT NULL
);
`

const createSentBlobKeysTable = `
CREATE TABLE IF NOT EXISTS blobkey (
	blobid BLOB UNIQUE NOT NULL,
	blobkey BLOB NOT NULL
);
`

const createFriendlyNamesTable = `
CREATE TABLE IF NOT EXISTS friendlyname (
	name TEXT UNIQUE NOT NULL,
	key BLOB UNIQUE NOT NULL
);
`

var createTables = []string{
	createSessionsTable,
	createSentBlobKeysTable,
	createFriendlyNamesTable,
}

const addContactSql = `
INSERT INTO friendlyname (name, key) VALUES (?, ?);
`

type NotEnoughXk2Bytes int

func (n NotEnoughXk2Bytes) Error() string {
	return fmt.Sprintf("Bad internet: not enough bytes for XK2 message: %d", n)
}

func saveNewContact(contacter []byte, localId string) error {
	db := setupDb()
	_, err := db.Exec(addContactSql, contacter, localId)
	return err
}

func encodeUint16(u uint16) []byte {
	return []byte{byte(u & 0xff), byte((u >> 8) & 0xff)}
}

var ad = []byte{
	248, 0, 56, 173, 54, 51, 61, 107, 170, 15, 53, 205, 36, 4, 67, 156, 186, 159, 44, 196, 114, 221, 191, 251, 143, 167, 73, 252, 139, 144, 60, 66,
}

const addContactLen = 69
const authTagLen = 16
const addContactId = 4

func RemoveContact(publicId string, localId string) {
}

func Send(path string, recipient string) {
}

func Open(messageId int) {
}

func View() {
}

func MyId() {
}

const dhlen = 32

const staticKeysPath = "statickeys"

func getStaticKey() noise.DHKey {
	raw, err := os.ReadFile(staticKeysPath)
	if os.IsNotExist(err) {
		key, err := noise.DH25519.GenerateKeypair(rand.Reader)
		if err != nil {
			panic(err)
		}
		encoded := make([]byte, 2*dhlen)
		copy(encoded, key.Private)
		copy(encoded[dhlen:], key.Public)
		err = os.WriteFile(staticKeysPath, encoded, 0400)
		if err != nil {
			panic(err)
		}
		return key
	}

	private := make([]byte, dhlen)
	if copy(private, raw) != dhlen {
		panic("not enough bytes to decode private key")
	}
	public := make([]byte, dhlen)
	if copy(public, raw[dhlen:]) != dhlen {
		panic("not enough bytes to decode public key")
	}

	return noise.DHKey{
		Private: private,
		Public:  public,
	}
}

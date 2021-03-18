package main

import (
	"net"
	"crypto/rand"
	"github.com/flynn/noise"
	"io/ioutil"
	"fmt"
	"database/sql"
	_ "github.com/mattn/go-sqlite3"
	"encoding/hex"
	"time"
)

const staticKeysPath = topPath + "/statickeys"

const topPath = "bigwebthing"

const dbPath = topPath + "/database.sqlite"

func getStaticKeys() noise.DHKey {
	raw, err := ioutil.ReadFile(staticKeysPath)
	if err != nil {
		return makeKeys()
	}

	if len(raw) != 2*dhlen {
		panic(fmt.Sprintf("bad static keys file: expecting %d bytes, but got %d", 2*dhlen, len(raw)))
	}

	return noise.DHKey{
		Private: raw[:dhlen],
		Public:  raw[dhlen:],
	}
}

func makeKeys() noise.DHKey {
	keys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	if err != nil {
		panic("couldn't generate static keys: " + err.Error())
	}

	encoded := make([]byte, 2*dhlen)
	copy(encoded, keys.Private)
	copy(encoded[:dhlen], keys.Public)
	err = ioutil.WriteFile(staticKeysPath, encoded, 0400)
	if err != nil {
		panic("couldn't write static keys file: " + err.Error())
	}
	return keys
}

const dhlen = 32

func main() {
	staticKeys := getStaticKeys()

	setupDb()

	ln, err := net.Listen("tcp", ":8080")
	if err != nil {
		panic("couldn't start TCP server: " + err.Error())
	}

	for {
		conn, err := ln.Accept()
		if err != nil {
			continue
		}
		go handleConnection(conn, staticKeys)
	}
}

func getDb() *sql.DB {
	db, err := sql.Open("sqlite3", dbPath)
	if err != nil {
		panic("couldn't get database connection: " + err.Error())
	}
	return db
}

const makeKk1sTableSql = `
CREATE TABLE IF NOT EXISTS kk1s (
	sessionid BLOB NOT NULL UNIQUE,
	halfkk1 BLOB NOT NULL UNIQUE,
	sender BLOB NOT NULL,
	recipient BLOB NOT NULL);
`

const makeKk2sTableSql = `
CREATE TABLE IF NOT EXISTS kk2s (
	sessionid BLOB NOT NULL UNIQUE,
	kk2 BLOB NOT NULL UNIQUE,
	sender BLOB NOT NULL,
	recipient BLOB NOT NULL);
`

const makeKkTransportTableSql = `
CREATE TABLE IF NOT EXISTS kktransports (
	sessionid BLOB NOT NULL UNIQUE,
	kktransport BLOB NOT NULL UNIQUE,
	sender BLOB NOT NULL,
	recipient BLOB NOT NULL,
	timestamp INTEGER NOT NULL);
`

const makePaymentsTableSql = `
CREATE TABLE IF NOT EXISTS payments (
	amount INTEGER NOT NULL,
	timestamp INTEGER NOT NULL,
	payer BLOB NOT NULL);
`

const makeBlobUploadTableSql = `
CREATE TABLE IF NOT EXISTS blobuploads (
	timestamp INTEGER NOT NULL,
	blobid BLOB UNIQUE NOT NULL,
	uploader BLOB NOT NULL);
`

const makeContactsTableSql = `
CREATE TABLE IF NOT EXISTS contacts (
	contacter BLOB NOT NULL,
	contactee BLOB NOT NULL,
	timestamp INTEGER NOT NULL,
	current INTEGER NOT NULL,
	PRIMARY KEY (contacter, contactee));
`

var setupSqls = []string{
	makeKk1sTableSql,
	makeKk2sTableSql,
	makeKkTransportTableSql,
	makePaymentsTableSql,
	makeBlobUploadTableSql,
}

func setupDb() {
	db := getDb()
	defer db.Close()
	for _, setupSql := range setupSqls {
		_, err := db.Exec(setupSql)
		if err != nil {
			panic("couldn't make table: " + setupSql + ": " + err.Error())
		}
	}
}

const xk1Size = 48

func noiseConfig(staticKeys noise.DHKey) noise.Config {
	return noise.Config{
		CipherSuite: noise.NewCipherSuite(noise.DH25519, noise.CipherAESGCM, noise.HashSHA256),
		Random: rand.Reader,
		Initiator: false,
		StaticKeypair: staticKeys,
	}
}

func handleConnection(conn net.Conn, staticKeys noise.DHKey) {
	xk1 := make([]byte, xk1Size)
	n, err := conn.Read(xk1)
	if n != xk1Size {
		return
	}
	if err != nil {
		return
	}

	shake, err := noise.NewHandshakeState(noiseConfig(staticKeys))
	if err != nil {
		panic("couldn't initiate handshake: " + err.Error())
	}

	_, _, _, err = shake.ReadMessage([]byte{}, xk1)
	if err != nil {
		return
	}

	xk2, _, _, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return
	}

	_, err = conn.Write(xk2)
	if err != nil {
		return
	}

	xk3 := make([]byte, xk3Size)
	n, err = conn.Read(xk3)
	if n != xk3Size {
		return
	}
	if err != nil {
		return
	}

	_, tx, rx, err := shake.ReadMessage([]byte{}, xk3)
	if err != nil {
		return
	}

	theirId := shake.PeerStatic()

	for {
		rawSize := make([]byte, 2)
		n, err := conn.Read(rawSize)
		if n != 2 {
			return
		}
		if err != nil {
			return
		}

		size := int(rawSize[0]) + (int(rawSize[1]) << 8)
		if size > maxFromClientSize {
			return
		}
		buf := make([]byte, size)
		n, err = conn.Read(buf)
		if n != size {
			return
		}
		if err != nil {
			return
		}

		plain, err := rx.Decrypt([]byte{}, serverAd, buf)
		if err != nil {
			return
		}

		if processFromClient(plain, theirId, conn, rx) {
			return
		}
	}
}

func processFromClient(
	message []byte,
	theirId []byte,
	conn net.Conn,
	rx *noise.CipherState) bool {

	if len(message) == 0 {
		return false
	}
	indicator := message[0]
	body := message[1:]
	switch indicator {
	case 0:
		return processKk1(body, theirId, conn, rx)
	case 1:
		return processKk2(body, theirId, conn, rx)
	case 2:
		return processKkTransport(body, theirId, conn, rx)
	case 3:
		return processBlob(body, theirId, conn, rx)
	case 4:
		return processPayment(body, theirId, conn, rx)
	case 5:
		return processRequestBlob(body, theirId, conn, rx)
	}
	return false
}

const recordBlobUploadSql =`
INSERT INTO blobuploads(timestamp, blobid, uploader) VALUES (?, ?, ?);
`

func recordBlobUpload(blobId, theirId []byte) bool {
	db := getDb()
	stmt, err := db.Prepare(recordBlobUploadSql)
	if err != nil {
		panic("couldn't make save blob upload statement: " + err.Error())
	}
	defer stmt.Close()
	_, err = stmt.Exec(time.Now().Unix(), blobId, theirId)
	return err == nil
}

func processBlob(
	body []byte,
	theirId []byte,
	conn net.Conn,
	rx *noise.CipherState) bool {

	balance := getBalance(theirId)
	if balance < blobUploadPrice {
		return false
	}

	if len(body) < 1 + blobIdSize {
		return false
	}
	blobId := body[:blobIdSize]
	blob := body[blobIdSize:]
	if len(blob) > 15957 {
		return false
	}

	if !recordBlobUpload(blobId, theirId) {
		return false
	}

	saveBlob(blobId, blob)
	return true
}

const getPaymentsSql = `
SELECT (amount) FROM payments WHERE payer=?;
`

func getPayments(db *sql.DB, theirId []byte) int {
	rows, err := db.Query(getPaymentsSql, theirId)
	if err != nil {
		panic("could not run query to get payments: " + getPaymentsSql + ": " + err.Error())
	}

	total := 0
	for rows.Next() {
		var amount int
		rows.Scan(&amount)
		total += amount
	}

	return total
}

const getBlobsSql = `
SELECT (timestamp) FROM blobuploads WHERE uploader=?;
`

// There is an upload fee and a fee per unit size per unit time stored.
func getBlobsCost(db *sql.DB, theirId []byte) int {
	rows, err := db.Query(getBlobsSql, theirId)
	if err != nil {
		panic("could not run query to get blobuploads: " + getBlobsSql + ": " + err.Error())
	}

	numBlobs := 0
	var seconds uint64 = 0

	// OK to convert to unsigned, because the epoch is 1970, and
	// there won't be any timestamps before then.
	now := uint64(time.Now().Unix())

	for rows.Next() {
		var t uint64
		rows.Scan(&t)
		seconds += now - t
		numBlobs++
	}

	const blobsInGb = 62500 // 1GB / 16KB
	const secondsInMonth = 30*24*60*60
	gbMonths := int(seconds / (secondsInMonth * blobsInGb))
	return gbMonths * pencePerGbPerMonth + numBlobs * pencePerGbUpload
}

const getContactsSql = `
SELECT (timestamp) FROM contacts WHERE contactee=?;
`

const getKk2sSql = `
SELECT (timestamp) FROM kk2s WHERE sender=?;
`

const getNumContactsSql = `
SELECT COUNT(*) FROM contacts WHERE contactee=?;
`

func getNumContacts(db *sql.DB, theirId []byte) int {
	rows, err := db.Query(getNumContactsSql, theirId)
	if err != nil {
		panic("couldn't run query to get number of contacts: " + getNumContactsSql + ": " + err.Error())
	}

	if !rows.Next() {
		panic("no result on number of contacts query")
	}

	var total int
	rows.Scan(&total)
	return total
}

func getNumTransports(db *sql.DB, theirId []byte) int {
	rows, err := db.Query(getNumTransportsSql, theirId)
	if err != nil {
		panic("couldn't run query to get number of transports: " + getNumTransportsSql + ": " + err.Error())
	}

	if !rows.Next() {
		panic("no results on number of transports query")
	}
}

func getKksCost(db *sql.DB, theirId []byte) int {
	numKk1s := getNumKk1s(db, theirId)
	numKk2s := getNumKk2s(db, theirId)
	numTransports := getNumTransports(db, theirId)
	numContacts := getNumContacts(db, theirId)
	total := numKk1s + numKk2s + numTransports + numContacts
	return pencePer1000Kks * total / 1000
}

func getBalance(theirId []byte) int {
	db := getDb()
	defer db.Close()

	payments := getPayments(db, theirId)
	blobsCost := getBlobsCost(db, theirId)
	kksCost := getKksCost(db, theirId)

	return payments - blobsCost - kksCost
}

const (
	pencePerGbPerMonth = 100
	pencePerGbUpload = 100
	pencePer1000Kks = 100
)

func saveBlob(blobId []byte, blob []byte) {
	path := makeBlobPath(blobId)
	err := ioutil.WriteFile(path, blob, 0400)
	if err != nil {
		panic("couldn't write new blob to file: " + err.Error())
	}
}

var payAuth = []byte{0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}

const savePaymentSql = `
INSERT INTO payments(amount, timestamp, payer) VALUES (?, ?, ?);
`

func bytesEqual(as []byte, bs []byte) bool {
	for i, a := range as {
		if a != bs[i] {
			return false
		}
	}
	return true
}

func processPayment(
	raw []byte,
	theirId []byte,
	conn net.Conn,
	rx *noise.CipherState) bool {

	if !bytesEqual(theirId, payAuth) {
		return false
	}
	rawAmount := raw[:4]
	amount := uint32P(rawAmount)
	payer := raw[4:]

	db := getDb()
	defer db.Close()

	stmt, err := db.Prepare(savePaymentSql)
	if err != nil {
		panic("couldn't make save payment statement: " + err.Error())
	}
	defer stmt.Close()

	_, err = stmt.Exec(amount, getPosixSeconds(), theirId)
	return err == nil
}

func getPosixSeconds() int {
	return int(time.Now().Unix())
}

func uint32P(raw []byte) int {
	n := 0
	for i := 0; i < 4; i++ {
		n += int(raw[i]) << (8 * i)
	}
	return n
}

var serverAd = []byte{235, 77, 23, 199, 11, 162, 118, 80, 65, 84, 165, 211, 116, 185, 150, 149}

const blobIdSize = 24

const blobsPath = topPath + "/blobs"

func makeBlobPath(blobId []byte) string {
	return blobsPath + "/" + hex.EncodeToString(blobId)
}

func processRequestBlob(
	blobId []byte,
	theirId []byte,
	conn net.Conn,
	rx *noise.CipherState) bool {

	if len(blobId) != blobIdSize {
		return false
	}

	blob, err := ioutil.ReadFile(makeBlobPath(blobId))
	if err != nil {
		return false
	}

	message := make([]byte, 1, 1 + blobIdSize + len(blob))
	message[0] = 3
	message = append(message, blobId...)
	message = append(message, blob...)

	encrypted := rx.Encrypt([]byte{}, serverAd, message)
	_, err = conn.Write(encrypted)
	return err == nil
}

const maxFromClientSize = 16000

const xk3Size = 64

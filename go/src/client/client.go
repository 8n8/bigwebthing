package main

import (
	"bytes"
	"goji.io"
	"goji.io/pat"
	"constants"
	"crypto/rand"
	"crypto/sha256"
	"encoding/base64"
	"errors"
	"golang.org/x/crypto/blake2b"
	"database/sql"
	_ "github.com/mattn/go-sqlite3"
	"fmt"
	"github.com/gorilla/websocket"
	"golang.org/x/crypto/argon2"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/nacl/sign"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"
)

type UiInput struct {
	w    http.ResponseWriter
	r    *http.Request
	done chan struct{}
}

const serverDomain = "http://localhost"

const serverHttpUrl = serverDomain + ":" + constants.ServerHttpPort

const serverTcpUrl = serverDomain + ":" + constants.ServerTcpPort

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}


type MyKeys struct {
	sign struct{
		public *[32]byte
		secret *[64]byte
		}
	encrypt struct{
		public *[32]byte
		secret *[32]byte
		}
}

var STOP = make(chan error)
var INPUT = make(chan Input)
var TOWEBSOCKET = make(chan []byte)
var AUTHCODE = make(chan [constants.AuthCodeLength]byte, 1)
var MYKEYS = getCryptoKeys()
var PATHS = makePaths()
var CACHELOCK sync.Mutex
var POWINFO = make(chan PowInfo, 1)
var PROOFOFWORK = make(chan [24]byte, 1)
var CONTACTS = make(chan map[string]struct{}, 1)
var LOG = make(chan string)
var UNIQUEID chan string
var MYID [constants.IdLength]byte = getMyId()

func makePaths() Paths {
	homedir, err := os.UserHomeDir()
	if err != nil {
		panic("could not get user home directory: " + err.Error())
	}

	root := homedir + os.PathSeparator + ".bigwebthing" +
		os.PathSeparator

	blob := func(hash [hashLen]byte) string {
		return root + "blobs" + os.PathSeparator +
			base64.URLEncoding.EncodeToString(hash)
	}

	tmp := func(hash []byte) string {
		return root + "tmp" + os.PathSeparator +
			base64.URLEncoding.EncodeToString(hash)
	}

	return Paths{
		myKeys: root + "myKeys",
		db: root + "database.sqlite",
		blob: blob,
		log: root + "log",
		tmp: tmp,
		myId: root + "myId",
	}
}

func chunkToTempFiles(encoded io.Reader) [][32]byte {
	tmpHashes := make([][32]byte, 0)
	for {
		const chunkSize = constants.MaxChunk - blobOverhead
		chunk := make([]byte, chunkSize)
		n, err := encoded.Read(chunk)
		if err != nil && err != EOF {
			panic(fmt.Sprintf("failed reading multireader: ", err))
		}
		hash := blake2b.Sum256(chunk)
		err := ioutil.WriteFile(PATHS.tmp(hash), chunk)
		if err != nil {
			panic(fmt.Sprintf("failed writing temporary file: ", err))
		}
		tmpHashes = append([][32]byte{hash}, tmpHashes...)
		if err == io.EOF || n < chunkSize {
			return tmpHashes
		}
	}
}

type Paths struct {
	myKeys string
	database string
	blob func([hashLen]byte) string
	log string
	tmp func([32]byte) string
	myId string
}

type bytesliceSet interface {
	insert([]byte)
	remove([]byte)
	contains([]byte) bool
}

func makeTcpAuth(
	myId [constants.IdLength]byte,
	authCode [constants.AuthCodeLength]byte,
	secretSign *[64]byte) []byte {

	auth := make([]byte, 13+sign.Overhead+16+16)
	copy(auth, myId[:])
	toSign := make([]byte, 32)
	copy(toSign, constants.TcpAuth)
	copy(toSign[32:], authCode[:])
	copy(auth[13:], sign.Sign([]byte{}, toSign, secretSign))
	return auth
}

var frontendDir = filepath.Join(homeDir, "frontend")

func readKeysFromFile() (MyKeys, error) {
	raw, err := ioutil.ReadFile(PATHS.myKeys)
	if err != nil {
		return *new(MyKeys), err
	}

	return parseKeys(raw)
}

func parseKeys(raw []byte) (MyKeys, error) {
	var keys MyKeys

	if len(raw) != 32+64+32+32 {
		return keys, errors.New("keys file is the wrong length")
	}

	copy(keys.sign.public[:], raw)
	copy(keys.sign.secret[:], raw[32:])
	copy(keys.encrypt.public[:], raw[32+64:])
	copy(keys.encrypt.secret[:], raw[32+64+32:])

	return keys, nil
}

func makeNewKeys() (MyKeys, error) {
	var keys MyKeys
	var err error
	keys.sign.public, keys.sign.secret, err = sign.GenerateKey(
		rand.Reader)
	if err != nil {
		return keys, err
	}

	keys.encrypt.public, keys.encrypt.secret, err = box.GenerateKey(
		rand.Reader)
	if err != nil {
		return keys, err
	}

	err = ioutil.WriteFile(
		PATHS.myKeys,
		encodeKeys(keys),
		0500)

	return keys, err
}

func encodeKeys(keys MyKeys) []byte {
	encoded := make([]byte, 32 + 64 + 32 + 32)
	copy(encoded, keys.sign.public[:])
	copy(encoded[32:], keys.sign.secret[:])
	copy(encoded[32+64:], keys.encrypt.public[:])
	copy(encoded[32+64+32:], keys.encrypt.secret[:])

	return encoded
}

var OUTPUT = make(chan Output)

func (StartUiServer) output() {
	http.HandleFunc(
		"/websocket",
		func(w http.ResponseWriter, r *http.Request) {
			conn, err := upgrader.Upgrade(w, r, nil)
			if err != nil {
				panic(err)
			}

			for {
				err := conn.WriteMessage(
					websocket.TextMessage, <-TOWEBSOCKET)
				if err != nil {
					panic(err)
				}
			}
		})
	http.HandleFunc(
		"/api",
		func(w http.ResponseWriter, r *http.Request) {
			done := make(chan struct{})
			body, err := ioutil.ReadAll(r.Body)
			if err != nil {
				panic(err)
			}
			INPUT <- ApiRequest{
				done: done,
				body: body,
				w: w,
			}
			<-done
		})
	panic(http.ListenAndServe(":11833", nil))
}

type ApiRequest struct {
	done chan struct{}
	body []byte
	w http.ResponseWriter
}

func parseSetSnapshot(body []byte) (UiRequest, error) {
	messageId, pos, err := parseUint32(body, 0)
	if err != nil {
		return *new(UiRequest), errors.New(
			"set snapshot: " + err.Error())
	}

	previous, pos, err := parseBytes(body, pos)
	if err != nil {
		return *new(UiRequest), errors.New(
			"set snapshot: " + err.Error())
	}

	snapshot := body[pos:]

	return SetSnapshot{
		previous: previous,
		snapshot: snapshot,
		messageId: messageId,
	}, nil
}

type SetSnapshot struct {
	previous []byte
	snapshot []byte
	messageId int
}

type DiffFromDb struct {
	hash [hashLen]byte
	previousHash [hashLen]byte
	start int
	end int
	insert []byte
	iWroteIt bool
	time int64
}

type BytesDiff struct {
	insert []byte
	// Positions between bytes in the old slice. So 0 means
	// the front of the slice, 1 means between the first and second
	// bytes.
	start int // end of identical section at front of slice
	end int // start of identical section at end of slice
}

func makeDiff(old, new_ []byte) BytesDiff {
	oldLen := len(old)
	newLen := len(new_)

	shortest := func() []byte {
		if oldLen < newLen {
			return old
		}
		return new_
	}()

	start := 0
	for _ = range shortest {
		oldChar := old[start]
		newChar := new_[start]
		if oldChar != newChar {
			break
		}
		start++
	}

	end := oldLen
	for _ = range shortest {
		oldChar := old[end - 1]
		newChar := new_[end - 1]
		if oldChar != newChar {
			break
		}
		end--
	}

	insertChars := new_[start: newLen - end]

	return BytesDiff{
		insert: insertChars,
		start: start,
		end: end,
	}
}

const hashLen = 20

func makeHash(blob []byte) [hashLen]byte {
	var result [hashLen]byte
	hash := blake2b.Sum256(blob)
	copy(result[:], hash[:])
	return result
}

func (s SetSnapshot) update(done chan struct{}, w http.ResponseWriter) Output {
	diff := makeDiff(s.previous, s.snapshot)
	return CacheDiff{
		diff: diff,
		messageId: s.messageId,
		hash: makeHash([]byte(s.snapshot)),
		previousHash: makeHash([]byte(s.previous)),
	}
}

type CacheDiff struct {
	diff BytesDiff
	messageId int
	hash [hashLen]byte
	previousHash [hashLen]byte
}

type DiffSigDb struct {
	author [constants.IdLength]byte
	hash [hashLen]byte
	previousHash [hashLen]byte
	signature [32 + sign.Overhead]byte
}

func (s CacheDiff) output() {
	database, err := sql.Open("sqlite3", PATHS.database)
	if err != nil {
		panic("could not open database: " + err.Error())
	}

	statement, err := database.Prepare("INSERT INTO diffs (message_id, hash, previous_hash, start, end, insert, i_wrote_it, time) VALUES (?, ?, ?, ?, ?, ?, ?, ?);")
	if err != nil {
		panic("could not prepare database statement: " + err.Error())
	}

	_, err = statement.Exec(
		s.messageId,
		s.hash,
		s.previousHash,
		s.diff.start,
		s.diff.end,
		s.diff.insert,
		true,
		time.Now().Unix())
	if err != nil {
		panic("could not insert new diff into database: " + err.Error())
	}
}

func parseSendMessage(body []byte) (UiRequest, error) {
	bodyLen := len(body)
	const expectedLen = 4 + 2 * hashLen + constants.IdLength
	if bodyLen != expectedLen {
		return *new(UiRequest), fmt.Errorf("send message request body should be %v bytes, but is actually %v bytes", expectedLen, bodyLen)
	}

	var result SendMessageRequest
	result.messageId = decodeInt(body[:4])
	copy(result.hash[:], body[4:])
	copy(result.previousHash[:], body[4 + hashLen:])
	copy(result.to[:], body[4 + 2 * hashLen:])

	return result, nil
}

type SendMessageRequest struct {
	hash [hashLen]byte
	previousHash [hashLen]byte
	to [constants.IdLength]byte
	messageId int
}

func (s SendMessageRequest) update(done chan struct{}, w http.ResponseWriter) Output {
	return SendMessage{
		done: done,
		messageId: s.messageId,
		hash: s.hash,
		previousHash: s.previousHash,
		to: s.to,
	}
}

type SendMessage struct {
	done chan struct{}
	messageId int
	hash [hashLen]byte
	previousHash [hashLen]byte
	to [constants.IdLength]byte
}

func getDiffsFromDb(messageId int) []DiffFromDb {
	database, err := sql.Open("sqlite3", PATHS.database)
	if err != nil {
		panic("couldn't get database handle: " + err.Error())
	}
	defer database.Close()
	rows, err := database.Query("SELECT hash, previous_hash, start, end, insert, i_wrote_it, time FROM diffs WHERE message_id = ?;", messageId)
	if err != nil {
		panic("couldn't read diffs from database: " + err.Error())
	}

	diffs := make([]DiffFromDb, 0)
	for rows.Next() {
		var d DiffFromDb
		rows.Scan(&d.hash, &d.previousHash, &d.start, &d.end, &d.insert, &d.iWroteIt, &d.time)
		diffs = append(diffs, d)
	}
	return diffs
}

func getDiffSignaturesFromDb(messageId int) []DiffSigDb {
	database, err := sql.Open("sqlite3", PATHS.database)
	if err != nil {
		panic("couldn't get database handle: " + err.Error())
	}
	defer database.Close()
	rows, err := database.Query("SELECT author, hash, previous_hash, signature FROM diff_signatures WHERE message_id = ?;", messageId)
	if err != nil {
		panic("couldn't read diff_signatures from database")
	}

	sigs := make([]DiffSigDb, 0)
	for rows.Next() {
		var d DiffSigDb
		rows.Scan(&d.author, &d.hash, &d.previousHash, &d.signature)
		sigs = append(sigs, d)
	}
	return sigs
}

type Diff struct {
	hash [hashLen]byte
	previousHash [hashLen]byte
	start int
	end int
	insert []byte
	time int64
	author [constants.IdLength]byte
	signature [32 + sign.Overhead]byte
}

func equalBytes(b1, b2 []byte) bool {
	for i, b := range b1 {
		if b != b2[i] {
			return false
		}
	}
	return true
}

func hashDiff(hash, previousHash [hashLen]byte) string {
	catHash := make([]byte, 2 * hashLen)
	copy(catHash, hash[:])
	copy(catHash[hashLen:], previousHash[:])
	bytes := makeHash(catHash)
	return base64.URLEncoding.EncodeToString(bytes[:])
}

func signDiff(
	hash [hashLen]byte,
	previousHash [hashLen]byte,
	secretKey *[64]byte) [32 + sign.Overhead]byte {

	catHash := make([]byte, 2 * hashLen)
	copy(catHash, hash[:])
	copy(catHash[hashLen:], previousHash[:])
	toSign := makeHash(catHash)
	signed := sign.Sign([]byte{}, toSign[:], secretKey)
	var signArr [32 + sign.Overhead]byte
	copy(signArr[:], signed)
	return signArr
}

func equalHash(h1, h2 [hashLen]byte) bool {
	for i, h := range h1 {
		if h != h2[i] {
			return false
		}
	}
	return true
}

func diffMatchesSig(d DiffFromDb, s DiffSigDb) bool {
	return equalHash(d.hash, s.hash) &&
		equalHash(d.previousHash, s.previousHash)
}

func parseDiff(
	d DiffFromDb,
	diffSignatures []DiffSigDb,
	myId [constants.IdLength]byte,
	secretKey *[64]byte) (Diff, error) {

	var author [constants.IdLength]byte
	var signature [32 + sign.Overhead]byte

	if d.iWroteIt {
		author = myId
		signature = signDiff(d.hash, d.previousHash, secretKey)
	} else {
		for _, sig := range diffSignatures {
			if diffMatchesSig(d, sig) {
				author = sig.author
				signature = sig.signature
				break
			}
		}
		return *new(Diff), fmt.Errorf("could not find signature for foreign diff: %v", d)
	}

	return Diff{
		hash: d.hash,
		previousHash: d.previousHash,
		start: d.start,
		end: d.end,
		insert: d.insert,
		time: d.time,
		author: author,
		signature: signature,
	}, nil
}

func parseDiffsFromDb(
	rawDiffs []DiffFromDb,
	diffSignatures []DiffSigDb,
	myId [constants.IdLength]byte,
	secretKey *[64]byte) (map[string]Diff, error) {

	diffs := make(map[string]Diff)
	for _, rawDiff := range rawDiffs {
		diff, err := parseDiff(rawDiff, diffSignatures, myId, secretKey)
		if err != nil {
			return diffs, err
		}
		diffs[hashDiff(diff.hash, diff.previousHash)] = diff
	}
	return diffs, nil
}

var emptyHash = makeHash([]byte{})

func findDiff(hash [hashLen]byte, diffs map[string]Diff) (Diff, error) {
	for _, diff := range diffs {
		if equalHash(hash, diff.hash) {
			return diff, nil
		}
	}
	return *new(Diff), fmt.Errorf(
		"could not find diff with hash %v", hash)
}

func pruneDiffs(oldDiffs map[string]Diff, send SendMessage) ([]Diff, error) {
	newDiffs := make([]Diff, 0)
	topHash := hashDiff(send.hash, send.previousHash)
	oldestDiff, foundIt := oldDiffs[topHash]
	if !foundIt {
		return newDiffs, fmt.Errorf("could not find top diff")
	}

	for {
		diffHash := hashDiff(oldestDiff.hash, oldestDiff.previousHash)
		newDiffs = append([]Diff{oldestDiff}, newDiffs...)
		if equalHash(oldestDiff.previousHash, emptyHash) {
			return newDiffs, nil
		}

		oldestDiff, err := findDiff(oldestDiff.previousHash, oldDiffs)
		if err != nil {
			return newDiffs, fmt.Errorf(
				"could not find diff: " + err.Error())
		}
	}
}

func applyDiff(snapshot []byte, diff Diff) ([]byte, error) {
	oldHash := makeHash(snapshot)
	if !equalHash(oldHash, diff.previousHash) {
		return []byte{}, errors.New("bad previous hash")
	}
	oldLen := len(snapshot)
	if diff.start > oldLen - 1 {
		return []byte{}, errors.New("start index out of range")
	}
	if diff.end > oldLen - 1 {
		return []byte{}, errors.New("end index out of range")
	}
	front := snapshot[:diff.start]
	end := snapshot[diff.end:]
	newShot := append(front, append(diff.insert, end...)...)
	newHash := makeHash(newShot)
	if !equalHash(newHash, diff.hash) {
		return []byte{}, errors.New("bad new hash")
	}
	return newShot, nil
}

func parseBlob(raw []byte, pos int) (Blob, int, error) {
	hash, pos, err := parseHash(raw, pos)
	if err != nil {
		return *new(Blob), pos, err
	}

	size, pos, err := parseUint32(raw, pos)
	if err != nil {
		return *new(Blob), pos, err
	}

	return Blob{
		hash: hash,
		size: size,
	}, pos, nil
}

type Snapshot struct {
	subject string
	userInput string
	programHash [hashLen]byte
	blobs []Blob
}

func parseSnapshot(raw []byte) (Snapshot, error) {
	subject, pos, err := parseString(raw, 0)
	if err != nil {
		return *new(Snapshot), err
	}

	userInput, pos, err := parseString(raw, pos)
	if err != nil {
		return *new(Snapshot), err
	}

	programHash, pos, err := parseHash(raw, pos)
	if err != nil {
		return *new(Snapshot), err
	}

	blobs := make([]Blob, 0)
	rawLen := len(raw)
	for {
		if pos == rawLen {
			return Snapshot{
				subject: subject,
				userInput: userInput,
				programHash: programHash,
				blobs: blobs,
			}, nil
		}

		blob, pos, err := parseBlob(raw, pos)
		if err != nil {
			return *new(Snapshot), err
		}

		blobs = append(blobs, blob)
	}
}

func extractBlobsFromSnapshot(snapshot []byte) ([][hashLen]byte, error) {
	parsed, err := parseSnapshot(snapshot)
	blobNames := make([][hashLen]byte, 0)
	for _, blob := range parsed.blobs {
		blobNames = append(blobNames, blob.hash)
	}
	return blobNames, nil
}

func extractBlobHashes(diffs []Diff) ([][hashLen]byte, error) {
	blobNames := make([][hashLen]byte, 0)
	snapshot := []byte{}
	for _, diff := range diffs {
		var err error
		snapshot, err = applyDiff(snapshot, diff)
		if err != nil {
			return blobNames, err
		}
		newBlobs, err := extractBlobsFromSnapshot(snapshot)
		if err != nil {
			return blobNames, err
		}
		for _, blob := range newBlobs {
			blobNames = append(blobNames, blob)
		}
	}
	return blobNames, nil
}

func combine(byteses ...[]byte) []byte {
	combined := make([]byte, 0)
	for _, bytes := range byteses {
		combined = append(combined, bytes...)
	}
	return combined
}

func encodeDiff(diff Diff) []byte {
	return combine(
		diff.hash[:],
		diff.previousHash[:],
		encodeUint32(diff.start),
		encodeUint32(diff.end),
		encodeUint32(len(diff.insert)),
		encodeBytes(diff.insert),
		encodeUint64(diff.time),
		diff.signature[:])
}

func encodeBytes(bytes []byte) []byte {
	numBytes := len(bytes)
	result := make([]byte, 4 + numBytes)
	copy(result, encodeUint32(numBytes))
	copy(result[4:], bytes)
	return result
}

func encodeDiffs(diffs []Diff) []byte {
	encoded := make([]byte, 0)
	for _, diff := range diffs {
		encoded = append(encoded, encodeDiff(diff)...)
	}
	return encoded
}

func (s SendMessage) output() {
	s.done <- struct{}{}
	rawDiffs := getDiffsFromDb(s.messageId)
	diffSignatures := getDiffSignaturesFromDb(s.messageId)

	fullDiffs, err := parseDiffsFromDb(rawDiffs, diffSignatures, MYID, MYKEYS.sign.secret)
	if err != nil {
		panic("could not parse diffs from database: " + err.Error())
	}

	prunedDiffs, err := pruneDiffs(fullDiffs, s)
	if err != nil {
		panic("couldn't prune the diffs: " + err.Error())
	}

	blobs, err := extractBlobHashes(prunedDiffs)
	if err != nil {
		panic("couldn't extract blob names from diffs: " + err.Error())
	}

	encodedDiffs := encodeDiffs(prunedDiffs)
	encodeAndSend(encodedDiffs, blobs, s)
}

const blobOverhead = (
	1 + // indicator
	constants.IdLength +
	sign.Overhead +
	constants.MeaningLength +
	constants.AuthCodeLength +
	24 + // nonce
	secretbox.Overhead -
	32) // for the hash of the next blob in the chain

func encodeAndSend(diffs []byte, blobs [][hashLen]byte, s SendMessage) {
	readers := []io.Reader{bytes.NewBuffer(diffs)}
	totalSize := int64(len(diffs))

	for _, blob := range blobs {
		file, err := os.Open(PATHS.blob(blob))
		if err != nil {
			panic(fmt.Sprintf("cannot find file for blob %v", blob))
		}
		defer file.Close()
		fileInfo, err := file.Stat()
		if err != nil {
			panic(fmt.Sprintf("cannot Stat file for blob %v", blob))
		}
		size := encodeUint32(int(fileInfo.Size()))
		readers = append(readers, bytes.NewBuffer(size))
		totalSize += fileInfo.Size()

		readers = append(readers, file)
	}

	encoded := io.MultiReader(readers...)

	reversed := chunkToTempFiles(encoded)

	var lastHash [32]byte
	var secretKey [32]byte
	_, err := io.ReadFull(rand.Reader, secretKey[:])
	if err != nil {
		panic(err)
	}
	bytesSent := 0
	for _, chunkHash := range reversed {
		chunk, err := ioutil.ReadFile(PATHS.tmp(chunkHash))
		if err != nil {
			panic("failed reading temporary file: " + err.Error())
		}
		withHash := append(lastHash[:], chunk...)
		var nonce [24]byte
		_, err = io.ReadFull(rand.Reader, nonce[:])
		if err != nil {
			panic(err)
		}

		encrypted := secretbox.Seal(nonce[:], withHash, &nonce, &secretKey)
		lastHash = blake2b.Sum256(encrypted)

		authCode := <-AUTHCODE
		toSign := combine(constants.UploadBlob, authCode[:], encrypted)
		preamble := combine([]byte{6}, MYID[:])
		signed := sign.Sign(preamble, toSign, MYKEYS.sign.secret)
		_, err = http.Post(
			serverApiUrl,
			"application/octet-stream",
			bytes.NewBuffer(signed))
		if err != nil {
			LOG <- "Blob upload failed: " + err.Error()
			TOWEBSOCKET <- []byte{1}
			return
		}

		TOWEBSOCKET <- combine(
			[]byte{4},
			s.hash[:],
			s.previousHash[:],
			encodeUint32(int(totalSize)),
			encodeUint32(bytesSent))
	}

	sendPointer(lastHash, secretKey, s.to)
}

func sendPointer(
	hash,
	secretKey [32]byte,
	to [constants.IdLength]byte) {

	encryptionKey, err := getEncryptionKey(to)
	if err != nil {
		LOG <- fmt.Sprintf("Could not get encryption key for %v: %v", to, err)
		TOWEBSOCKET <- []byte{1}
		return
	}

	var nonce [24]byte
	_, err = io.ReadFull(rand.Reader, nonce[:])
	if err != nil {
		panic(err)
	}

	message := append(hash[:], secretKey[:]...)

	pow := <-PROOFOFWORK
	preamble := combine([]byte{7}, pow[:], to[:], nonce[:])

	encrypted := box.Seal(preamble, message, &nonce, &encryptionKey, MYKEYS.encrypt.secret)

	_, err = http.Post(
		serverApiUrl,
		"application/octet-stream",
		bytes.NewBuffer(encrypted))

	if err != nil {
		LOG <- fmt.Sprintf("Failed sending pointer message: %v", err)
		TOWEBSOCKET <- []byte{1}
	}
}

func getEncryptionKey(to [constants.IdLength]byte) ([32]byte, error) {
	response, err := http.Post(
		serverApiUrl,
		"application/octet-stream",
		bytes.NewBuffer(append([]byte{3}, to[:]...)))
	if err != nil {
		return *new([32]byte), err
	}

	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		return *new([32]byte), err
	}

	return parseNewEncryptionKey(body)
}

func parseNewEncryptionKey(raw []byte) ([32]byte, error) {
	lenRaw := len(raw)
	if lenRaw == 0 {
		return *new([32]byte), errors.New("empty body")
	}

	switch raw[0] {
	case 0:
		return *new([32]byte), errors.New("no key available")
	case 1:
		if lenRaw != 33 {
			return *new([32]byte), fmt.Errorf("body should be 33 bytes long, but it is %d", lenRaw)
		}
		var key [32]byte
		copy(key[:], raw[1:])
		return key, nil
	}
	return *new([32]byte), fmt.Errorf("bad indicator byte: expecting 0 or 1, but got %d", raw[0])
}

func parseUiRequest(body []byte) (UiRequest, error) {
	if len(body) == 0 {
		return *new(UiRequest), errors.New("empty")
	}

	switch body[0] {
	case 0:
		return parseSetSnapshot(body[1:])
	case 1:
		return parseSendMessage(body[1:])
	// case 2:
	// 	return parseAddToWhitelist(body[1:])
	// case 3:
	// 	return parseRemoveFromWhitelist(body[1:])
	// case 4:
	// 	return parseGetBlob(body[1:])
	// case 5:
	// 	return parseGetSnapshot(body[1:])
	// case 6:
	// 	return parseGetWhitelist(body[1:])
	// case 7:
	// 	return parseGetMyId(body[1:])
	// case 8:
	// 	return parseGetDraftsSummary(body[1:])
	// case 9:
	// 	return parseGetSentSummary(body[1:])
	// case 10:
	// 	return parseGetInboxSummary(body[1:])
	}

	return *new(UiRequest), errors.New(
		"bad indicator byte: " + string(body[0]))
}

type UiRequest interface {
	update(chan struct{}, http.ResponseWriter) Output
}

func (a ApiRequest) update() Output {
	parsed, err := parseUiRequest(a.body)
	if err != nil {
		return Panic(
			"could not parse API request from UI: " + err.Error())
	}
	return parsed.update(a.done, a.w)
}

func getWhitelistee(r *http.Request) []byte {
	whitelistee := make([]byte, constants.IdLength)
	n, err := r.Body.Read(whitelistee)
	if n != constants.IdLength {
		panic(fmt.Sprintf(
			"/whitelist/add: body must be %v bytes long",
			constants.IdLength))
	}
	if err != nil {
		panic(err)
	}
	return whitelistee
}

func sendNewEncryptionKey() {
	public, secret, err := box.GenerateKey(rand.Reader)
	if err != nil {
		panic("could not generate encryption keys: " + err.Error())
	}

	preamble := make([]byte, 1 + constants.IdLength)
	preamble[0] = 1
	copy(preamble[1:], MYID[:])

	toSign := make([]byte, 32 + constants.MeaningLength + constants.AuthCodeLength)
	copy(toSign, public[:])
	copy(toSign[32:], constants.MyEncryptionKey)
	authCode := <-AUTHCODE
	copy(toSign[32+constants.MeaningLength:], authCode[:])

	signed := sign.Sign(preamble, toSign, MYKEYS.sign.secret)

	_, err = http.Post(
		serverApiUrl,
		"application/octet-stream",
		bytes.NewBuffer(signed))
	if err != nil {
		LOG <- err.Error()
		return
	}

	database, err := sql.Open("sqlite3", PATHS.database)
	if err != nil {
		panic("could not open database: " + err.Error())
	}
	defer database.Close()
	statement, err := database.Prepare("INSERT INTO my_encryption_keys (public, secret) VALUES (?, ?);")
	if err != nil {
		panic("could not prepare database statement: " + err.Error())
	}
	_, err = statement.Exec(public, secret)
	if err != nil {
		panic("could not insert new encryption keys into database: " + err.Error())
	}
}

const networkSleep = 30 * time.Second

func tcpListenTillFail(conn net.Conn) {
	auth := makeTcpAuth(
		MYID, <-AUTHCODE, MYKEYS.sign.secret)
	n, err := conn.Write(auth)
	if n != len(auth) {
		return
	}
	if err != nil {
		return
	}

	for {
		indicator := make([]byte, 1)
		n, err = conn.Read(indicator)
		if n != 1 {
			return
		}
		if err != nil {
			return
		}

		switch indicator[0] {
		case 0:
			sendNewEncryptionKey()
		case 1:
			msg := make([]byte, tcpMsgLen)
			n, err = conn.Read(msg)
			if n != tcpMsgLen {
				return
			}
			if err != nil {
				return
			}

			INPUT <- MsgFromServer{
				myCryptoKeys: getMyCryptoKeysFromDb(),
				msg: msg,
				contacts: getContactsFromDb(),
				}
		default:
			panic("received bad message from the server: " + string(indicator[0]))
		}
	}
}

func getMyCryptoKeysFromDb() []*[32]byte {
	database, err := sql.Open("sqlite3", PATHS.database)
	if err != nil {
		panic("could not open database: " + err.Error())
	}
	defer database.Close()

	rows, err := database.Query("SELECT secret FROM my_encryption_keys;")
	if err != nil {
		panic("couldn't read crypto keys from database: " + err.Error())
	}

	keys := make([]*[32]byte, 0)
	for rows.Next() {
		var k [32]byte
		rows.Scan(&k)
		keys = append(keys, &k)
	}
	return keys
}

func getContactsFromDb() []Contact {
	database, err := sql.Open("sqlite3", PATHS.database)
	if err != nil {
		panic("could not open database: " + err.Error())
	}
	defer database.Close()

	rows, err := database.Query("SELECT user, sign, encrypt FROM public_keys;")
	if err != nil {
		panic("couldn't read public_keys from database: " + err.Error())
	}

	contacts := make([]Contact, 0)
	for rows.Next() {
		var c Contact
		rows.Scan(&c.id, &c.sign, &c.encrypt)
		contacts = append(contacts, c)
	}
	return contacts
}

// The server strips off the indicator byte and the proof of work
// before forwarding it to the client
const tcpMsgLen = 24 + box.Overhead + 32 + 32

func (StartTcpListener) output() {
	for {
		conn, err := net.Dial("tcp", serverTcpUrl)
		if err != nil {
			INPUT <- BadNetwork{}
			time.Sleep(networkSleep)
			continue
		}

		INPUT <- GoodNetwork{}

		tcpListenTillFail(conn)
		INPUT <- BadNetwork{}
		time.Sleep(networkSleep)
	}
}

func main() {
	OUTPUT <- Start{}

	go func() {
		for {
			OUTPUT <- (<-INPUT).update()
		}
	}()

	go func() {
		for {
			go (<-OUTPUT).output()
		}
	}()

	select{}
}

type Start struct{}

func (Start) output() {
	OUTPUT <- SetupDatabase{}
	OUTPUT <- StartTcpListener{}
	OUTPUT <- StartUiServer{}
	OUTPUT <- StartLogger{}
	OUTPUT <- GetPowInfo{}
	OUTPUT <- MakeProofOfWork{}
}

type MakeProofOfWork struct{}

func (MakeProofOfWork) output() {
	for {
		PROOFOFWORK <- makePow(<-POWINFO)
	}
}

type SetupDatabase struct {}

var makeTables = []string{
`CREATE TABLE IF NOT EXISTS diffs (
	message_id INTEGER NOT NULL,
	hash BLOB NOT NULL,
	previous_hash BLOB NOT NULL,
	author BLOB,
	time INTEGER NOT NULL,
	start INTEGER NOT NULL,
	end INTEGER NOT NULL,
	PRIMARY KEY (hash, previous_hash)
);`,
`CREATE TABLE IF NOT EXISTS sent (
	hash BLOB NOT NULL,
	time INTEGER NOT NULL,
	to BLOB NOT NULL,
	PRIMARY KEY (hash, time, to)
);`,
`CREATE TABLE IF NOT EXISTS received (
	from BLOB NOT NULL,
	hash BLOB NOT NULL,
	time INTEGER NOT NULL,
	PRIMARY KEY (from, hash, time)
);`,
`CREATE TABLE IF NOT EXISTS whitelist (
	user BLOB NOT NULL PRIMARY KEY
);`,
`CREATE TABLE IF NOT EXISTS public_sign_keys (
	user BLOB NOT NULL PRIMARY KEY,
	key BLOB NOT NULL UNIQUE
);`,
`CREATE TABLE IF NOT EXISTS acknowledgements (
	from BLOB NOT NULL,
	time INTEGER NOT NULL,
	hash BLOB NOT NULL,
	signature BLOB NOT NULL PRIMARY KEY
);`,
`CREATE TABLE IF NOT EXISTS my_encryption_keys (
	public BLOB NOT NULL PRIMARY KEY,
	secret BLOB NOT NULL UNIQUE
);`,
	}

func (SetupDatabase) output() {
	database, err := sql.Open("sqlite3", PATHS.database)
	if err != nil {
		panic("could not open database: " + err.Error())
	}
	defer database.Close()

	for _, query := range makeTables {
		_, err = database.Exec(query)
		if err != nil {
			panic(fmt.Sprintf(
				"error running query:\n%s\n%s", err.Error()))
		}
	}
}

type StartLogger struct{}

func (StartLogger) output() {
	f, err := os.OpenFile(logPath, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0600)
	if err != nil {
		panic(err)
	}

	for {
		msg := fmt.Sprintf("%v   %s\n", time.Now(), <-LOG)
		n, err := f.Write([]byte(msg))
		if n == 0 {
			panic("log message was empty")
		}
		if err != nil {
			panic(err)
		}
	}
}

type GetPowInfo struct{}

func (GetPowInfo) output() {
	bad := func() {
		INPUT <- BadNetwork{}
		time.Sleep(networkSleep)
	}
	for {
		resp, err := http.Get(serverHttpUrl + "/proofofworkinfo")
		if err != nil {
			bad()
			continue
		}

		raw := make([]byte, 17)
		n, err := resp.Body.Read(raw)
		if n != 17 {
			bad()
			continue
		}
		if err != nil {
			bad()
			continue
		}

		POWINFO <- PowInfo{
			difficulty: raw[0],
			unique:     raw[1:],
		}
	}
}

type GetAuthCode struct{}

func (GetAuthCode) output() {
	bad := func() {
		INPUT <- BadNetwork{}
		time.Sleep(networkSleep)
	}
	for {
		resp, err := http.Get(serverHttpUrl + "/authcode")
		if err != nil {
			bad()
			continue
		}

		var authCode [constants.AuthCodeLength]byte
		n, err := resp.Body.Read(authCode[:])
		if n != constants.AuthCodeLength {
			bad()
			continue
		}
		if err != nil {
			bad()
			continue
		}
		AUTHCODE <- authCode
	}
}

type StartTcpListener struct{}

type StartUiServer struct{}

const myIdPath = "myId"

func getMyId() [constants.IdLength]byte {
	myIdBytes, err := ioutil.ReadFile(PATHS.myId)
	var myId [constants.IdLength]byte
	copy(myId[:], myIdBytes)
	if err == nil {
		return myId
	}

	newIdBytes := argon2.IDKey(
		append(MYKEYS.sign.public[:], MYKEYS.encrypt.public[:]...),
		[]byte{},
		60,
		256*1024,
		4,
		constants.IdLength)
	err = ioutil.WriteFile(PATHS.myId, newIdBytes, 0500)
	if err != nil {
		panic("could not write my new ID to file: " + err.Error())
	}

	copy(myId[:], newIdBytes)
	return myId
}

type GetCryptoKeys struct{}

func getCryptoKeys() MyKeys {
	keys, err := readKeysFromFile()
	if err != nil {
		keys, err = makeNewKeys()
	}
	if err != nil {
		panic("could not make crypto keys: " + err.Error())
	}
	return keys
}

func intPower(base, power int) int {
	result := 1
	for i := 0; i < power; i++ {
		result = result * base
	}
	return result
}

func int64Power(base, power int64) int64 {
	var result int64 = 1
	for i := int64(0); i < power; i++ {
		result = result * base
	}

	return result
}

func decodeInt(bs []byte) int {
	// Most significant byte should be the last one (Little-Endian).
	result := 0
	for i, b := range bs {
		result += int(b) * intPower(256, i)
	}
	return result
}

func decodeInt64(bs [8]byte) int64 {
	var result int64 = 0
	for i, b := range bs {
		result += int64(b) * int64Power(256, int64(i))
	}
	return result
}

type MsgFromServer struct {
	myCryptoKeys []*[32]byte
	contacts []Contact
	msg []byte
}

type Contact struct {
	id []byte
	sign [32]byte
	encrypt [32]byte
}

func (m MsgFromServer) update() Output {
	var nonce [24]byte
	copy(nonce[:], m.msg[:24])
	encrypted := m.msg[24:]
	for _, contact := range m.contacts {
		for _, secretKey := range m.myCryptoKeys {
			decrypted, ok := box.Open(
				[]byte{},
				encrypted,
				&nonce,
				&contact.encrypt,
				secretKey)
			if ok {
				var tempKey [32]byte
				copy(tempKey[:], decrypted[32:])
				return LookupBlob{
					hash: decrypted[:32],
					tempKey: tempKey,
					contact: contact,
				}
			}
		}
	}
	return Log("received a bad message")
}

type LookupBlob struct {
	hash []byte
	tempKey [32]byte
	contact Contact
}

const serverApiUrl = serverHttpUrl + "/api"

func parseDiffFromServer(r io.Reader) (Diff, error) {
	var diff Diff
	_, err := io.ReadFull(r, diff.hash[:])
	if err != nil {
		return diff, fmt.Errorf("failed reading diff hash: %v", err)
	}

	_, err = io.ReadFull(r, diff.previousHash[:])
	if err != nil {
		return diff, fmt.Errorf("failed reading diff previous hash: %v", err)
	}

	diff.start, err = parseUint32Reader(r)
	if err != nil {
		return diff, fmt.Errorf("failed reading diff start: %v", err)
	}

	diff.end, err = parseUint32Reader(r)
	if err != nil {
		return diff, fmt.Errorf("failed reading diff end: %v", err)
	}

	insertLength, err := parseUint32Reader(r)
	if err != nil {
		return diff, fmt.Errorf("failed reading insert length: %v", err)
	}

	insert := make([]byte, insertLength)
	_, err = io.ReadFull(r, insert)
	if err != nil {
		return diff, fmt.Errorf("failed reading insert: %v", err)
	}
	diff.insert = insert

	var rawTime [8]byte
	_, err = io.ReadFull(r, rawTime[:])
	if err != nil {
		return diff, fmt.Errorf("failed reading raw time: %v", err)
	}
	diff.time = decodeInt64(rawTime)

	_, err = io.ReadFull(r, diff.author[:])
	if err != nil {
		return diff, fmt.Errorf("failed reading diff author: %v", err)
	}

	_, err = io.ReadFull(r, diff.signature[:])
	if err != nil {
		return diff, fmt.Errorf("failed reading diff signature: %v", err)
	}

	return diff, nil
}

func parseUint32Reader(r io.Reader) (int, error) {
	raw := make([]byte, 4)
	_, err := io.ReadFull(r, raw)
	return decodeInt(raw), err
}

func parseDiffsFromServer(r io.Reader) ([]Diff, error) {
	rawLength := make([]byte, 4)
	_, err := io.ReadFull(r, rawLength)
	if err != nil {
		return *new([]Diff), fmt.Errorf("couldnt't read number of diffs: %v", err)
	}

	numDiffs := decodeInt(rawLength)

	diffs := make([]Diff, numDiffs)
	for i := 0; i < numDiffs; i++ {
		diff, err := parseDiffFromServer(r)
		if err != nil {
			return diffs, fmt.Errorf("failed parsing diff %d out of %d: %v", i + 1, numDiffs, err)
		}
		diffs[i] = diff
	}
	return diffs, nil
}

func (g LookupBlob) output() {
	const mediumSizedNumberNothingSpecial = 1000
	ch := make(chan ByteStream, mediumSizedNumberNothingSpecial)
	go getChunkStream(ch, g)
	reader := Reader{
		ch: ch,
		finished: false,
	}

	diffs, err := parseDiffsFromServer(reader)
	if err != nil {
		LOG <- fmt.Sprintf("could not parse diffs from %v: %v", g.contact.id, err)
		return
	}

	blobHashes, err := extractBlobHashes(diffs)
	if err != nil {
		LOG <- fmt.Sprintf("could not extract blob hashes in message from %v: %v", g.contact.id, err)
		return
	}

	for {
		final, err := readBlobToFile(reader, blobHashes)
		if err != nil {
			err = deleteBlobs(blobHashes)
			if err != nil {
				panic("could not delete blobs after failed message decoding from %v: %v", g.contact.id, err)
			}
			LOG <- fmt.Sprintf("could not read blob in message from %v: %v", g.contact.id, err)
			return
		}
	}

	err = writeDiffsToDb(diffs)
	if err != nil {
		panic("could not write diffs to database: " + err.Error())
	}
}

func readBlobToFile(
	r io.Reader, blobHashes [][hashLen]byte) (bool, error) {

	blobLength, err := parseUint32Reader(r)
	if err == io.EOF {
		return true, nil
	}
	if err != nil {
		return false, fmt.Errorf("failed reading blob length: %v", err)
	}

	tmpFilename := encodeUint64(<-UNIQUE)

	tmpFile, err := os.Create(PATHS.tmp(tmpFilename))
	if err != nil {
		return false, err
	}
	defer tmpFile.Close()

	hashHandle := blake2b.New256(nil)
	tee := io.TeeReader(r, hashHandle)

	_, err = io.CopyN(tmpFile, tee, blobLength)
	if err != nil {
		return false, err
	}

	hash := hashHandle.Sum([]byte{})

	err = os.Rename(PATHS.tmp(tmpFileName), PATHS.blob(hash))
	if err != nil {
		return false, err
	}
}

type Reader struct {
	ch chan ByteStream
	finished bool
}

func (r Reader) Read(p []byte) (int, error) {
	for i := range p {
		if r.finished {
			return i, io.EOF
		}
		element := <-r.ch
		r.finished = element.read(p, i)
	}
	return len(p), nil
}

func getChunkStream(ch chan ByteStream, g LookupBlob) {
	nextHash := g.hash
	for {
		resp, err := http.Post(
			serverApiUrl,
			"application/octet-stream",
			bytes.NewBuffer(append([]byte{9}, g.hash...)))
		if err != nil {
			INPUTS <-BadNetwork{}
			return
		}
		defer resp.Body.Close()
		body, err := ioutil.ReadAll(resp.Body)
		if err != nil {
			INPUTS <-BadNetwork{}
			return
		}
		parsed, err := parseChunk(body, g.tempKey)
		if err != nil {
			panic("bad chunk from server: " + err.Error())
		}
		for _, b := range parsed.chunk{
			ch <- Byte(b)
		}

		if parsed.final {
			ch <- EndOfStream{}
			return
		}
		nextHash = parsed.nextHash
	}
}

type Byte byte

func (b Byte) read(p []byte, i int) bool {
	p[i] = byte(b)
	return false
}

type EndOfStream struct{}

func (EndOfStream) read(_ []byte, _ int) bool {
	return true
}

type ByteStream interface {
	read([]byte, int) bool
}

func parseChunk(raw []byte, secretKey *[32]byte) (ParsedChunk, error) {
	lenRaw := len(raw)
	const minLen = 24 + 32 + 1 + secretbox.Overhead
	if lenRaw < 24 + 32 + 1 + secretbox.Overhead {
		return *new(ParsedChunk), fmt.Errorf("expecting chunk from server to be at least %d bytes, but only got %d bytes", minLen, lenRaw)
	}
	var nonce [24]byte
	copy(nonce[:], raw)

	decrypted, ok := secretbox.Open([]byte{}, raw[24:], &nonce, secretKey)
	if !ok {
		return *new(ParsedChunk), errors.New("decryption of server chunk failed")
	}

	var nextHash [32]byte
	copy(nextHash[:], decrypted)
	return ParsedChunk{
		final: allZeros(nextHash),
		chunk: decrypted[32:],
		nextHash: nextHash,
	}
}

type ParsedChunk struct {
	final bool
	chunk []byte
	nextHash [32]byte
}

type RawChunkStream chan []byte




type GotBlob struct {
	tempKey [32]byte
	contact Contact
	hash []byte
	body []byte
}

type Output interface {
	output()
}

type Input interface {
	update() Output
}

func (u UiInput) update() Output {
	return u
}

type UiInWithUrl struct {
	w    http.ResponseWriter
	r    *http.Request
	done chan struct{}
	path string
}

func (u UiInput) output() {
	INPUT <- UiInWithUrl{
		w:    u.w,
		r:    u.r,
		done: u.done,
		path: u.r.URL.Path,
	}
}

type HttpFail struct {
	w      http.ResponseWriter
	status int
	msg    []byte
	done   chan struct{}
}

func (h HttpFail) output() {
	h.w.WriteHeader(h.status)
	h.w.Write(h.msg)
	h.done <- struct{}{}
}

func routeErr(msg string) (Route, error) {
	return *new(Route), errors.New(msg)
}

func cacheGetP(raw []string) (Route, error) {
	if len(raw) != 3 {
		return routeErr("path not 3 elements")
	}

	if !sliceEq(raw[:2], "cache", "get") {
		return routeErr("path should begin \"cache/get\"")
	}

	if len(raw[2]) == 0 {
		return routeErr("key is empty")
	}

	return CacheGetRequest(raw[2]), nil
}

func cacheSetP(raw []string) (Route, error) {
	if len(raw) != 3 {
		return routeErr("path not 3 elements")
	}

	if !sliceEq(raw[:2], "cache", "set") {
		return routeErr("path should begin \"cache/set\"")
	}

	if len(raw[2]) == 0 {
		return routeErr("key is empty")
	}

	return CacheSetRequest(raw[2]), nil
}

func cacheDeleteP(raw []string) (Route, error) {
	if len(raw) != 3 {
		return routeErr("path not 3 elements")
	}

	if !sliceEq(raw[:2], "cache", "delete") {
		return routeErr("path should begin \"cache/delete\"")
	}

	if len(raw[2]) == 0 {
		return routeErr("key is empty")
	}

	return CacheDeleteRequest(raw[2]), nil
}

func whitelistRemoveP(raw []string) (Route, error) {
	if len(raw) != 2 {
		return routeErr("path not 2 elements")
	}

	if !sliceEq(raw[:2], "whitelist", "remove") {
		return routeErr("path should begin \"whitelist/remove\"")
	}

	return WhitelistRemove{}, nil
}

func whitelistAddP(raw []string) (Route, error) {
	if len(raw) != 2 {
		return routeErr("path not 2 elements")
	}

	if !sliceEq(raw[:2], "whitelist", "add") {
		return routeErr("path should begin \"whitelist/add\"")
	}

	return WhitelistAdd{}, nil
}

func sliceEq(s1 []string, s2 ...string) bool {
	if len(s1) != len(s2) {
		return false
	}

	for i, s := range s1 {
		if s != s2[i] {
			return false
		}
	}

	return true
}

func (w WhitelistRemove) handle(u UiInWithUrl) Output {
	return ReadUnwhitelistee{
		w:    u.w,
		r:    u.r,
		done: u.done,
	}
}

type ReadUnwhitelistee struct {
	w    http.ResponseWriter
	r    *http.Request
	done chan struct{}
}

func (r ReadUnwhitelistee) output() {
	unwhitelistee := make([]byte, constants.IdLength)
	n, err := r.r.Body.Read(unwhitelistee)
	INPUT <- TriedReadingUnwhitelistee{
		w:             r.w,
		done:          r.done,
		n:             n,
		err:           err,
		myId:          <-MYID,
		authCode:      <-AUTHCODE,
		secretSign:    *(<-MYKEYS).sign.secret,
		unwhitelistee: unwhitelistee,
	}
}

type TriedReadingUnwhitelistee struct {
	w             http.ResponseWriter
	done          chan struct{}
	n             int
	err           error
	myId          []byte
	authCode      []byte
	secretSign    [64]byte
	unwhitelistee []byte
}

func (t TriedReadingUnwhitelistee) update() Output {
	fail := func(msg string) HttpFail {
		return HttpFail{
			w:      t.w,
			status: 400,
			msg:    []byte(msg),
			done:   t.done,
		}
	}
	if t.n != constants.IdLength {
		return fail("couldn't read unwhitelistee from body")
	}
	if t.err != nil {
		return fail(t.err.Error())
	}

	toSign := make([]byte, constants.MeaningLength+constants.AuthCodeLength+constants.IdLength)
	copy(toSign, constants.WhitelistRemove)
	copy(toSign[constants.MeaningLength:], t.authCode)
	copy(
		toSign[constants.MeaningLength+constants.AuthCodeLength:],
		t.unwhitelistee)
	signed := sign.Sign(t.myId, toSign, &t.secretSign)
	return WhitelistRequest{
		w:    t.w,
		body: signed,
		done: t.done,
		path: serverHttpUrl + "/whitelist/remove",
	}
}

type PowInfo struct {
	unique     []byte
	difficulty byte
}

type TriedReadingWhitelistee struct {
	authCode    []byte
	myId        []byte
	secretSign  [64]byte
	powInfo     PowInfo
	whitelistee []byte
	w           http.ResponseWriter
	done        chan struct{}
	n           int
	err         error
}

func (t TriedReadingWhitelistee) update() Output {
	fail := func(msg string) HttpFail {
		return HttpFail{
			w:      t.w,
			status: 400,
			msg:    []byte(msg),
			done:   t.done,
		}
	}
	if t.n != constants.IdLength {
		return fail("couldn't read whitelistee from body")
	}
	if t.err != nil {
		return fail(t.err.Error())
	}

	pow := makePow(t.powInfo)

	toSign := make([]byte, constants.MeaningLength+constants.AuthCodeLength+constants.IdLength)
	copy(toSign, constants.WhitelistAdd)
	copy(toSign[constants.MeaningLength:], t.authCode)
	copy(
		toSign[constants.MeaningLength+constants.AuthCodeLength:],
		t.whitelistee)
	signed := sign.Sign(append(pow, t.myId...), toSign, &t.secretSign)
	return WhitelistRequest{
		w:    t.w,
		body: signed,
		done: t.done,
		path: serverHttpUrl + "/whitelist/add",
	}
}

type WhitelistRequest struct {
	w    http.ResponseWriter
	body []byte
	done chan struct{}
	path string
}

func (w WhitelistRequest) output() {
	_, err := http.Post(
		w.path,
		"application/octet-stream",
		bytes.NewBuffer(w.body))
	INPUT <- SentWhitelistRequest{
		w:    w.w,
		err:  err,
		done: w.done,
	}
}

type SentWhitelistRequest struct {
	w    http.ResponseWriter
	err  error
	done chan struct{}
}

func (s SentWhitelistRequest) update() Output {
	if s.err != nil {
		return FailedRelay{
			w:      s.w,
			status: 500,
			msg:    []byte{},
			done:   s.done,
		}
	}
	return HttpOk(s.done)
}

type HttpOk chan struct{}

func (h HttpOk) output() {
	h <- struct{}{}
}

type FailedRelay struct {
	w      http.ResponseWriter
	status int
	msg    []byte
	done   chan struct{}
}

func (f FailedRelay) output() {
	f.w.WriteHeader(f.status)
	f.w.Write(f.msg)
	f.done <- struct{}{}
	INPUT <- BadNetwork{}
}

func makePow(info PowInfo) [24]byte {
	counter := 0
	var toHash [24]byte
	copy(toHash[:], info.unique)
	for {
		copy(toHash[16:], encodeUint32(counter))
		hash := argon2.IDKey(toHash[:], []byte{}, 1, 64*1024, 4, 32)
		counter += 1
		for _, b := range hash {
			if b < info.difficulty {
				continue
			}
		}
		return toHash
	}
}

func (c CacheDeleteRequest) handle(u UiInWithUrl) Output {
	return UiCacheDelete{
		done: u.done,
		path: cachePath(string(c)),
	}
}

func (c CacheGetRequest) handle(u UiInWithUrl) Output {
	return UiCacheGet{
		path: cachePath(string(c)),
		w:    u.w,
		done: u.done,
	}
}

type UiCacheGet struct {
	done chan struct{}
	path string
	w    http.ResponseWriter
}

func (c CacheSetRequest) handle(u UiInWithUrl) Output {
	return UiCacheSet{
		done: u.done,
		path: cachePath(string(c)),
		r:    u.r.Body,
	}
}

type UiCacheSet struct {
	done chan struct{}
	path string
	r    io.Reader
}

func (u UiCacheSet) output() {
	CACHELOCK.Lock()
	file, err := os.Create(u.path)
	if err != nil {
		STOP <- err
	}
	n, err := io.Copy(file, u.r)
	CACHELOCK.Unlock()

	if n == 0 {
		STOP <- errors.New("did not write any bytes to new file")
	}
	if err != nil {
		STOP <- err
	}

	u.done <- struct{}{}
}

func (u UiCacheGet) output() {
	CACHELOCK.Lock()
	file, err := os.Open(u.path)
	if err != nil {
		STOP <- err
	}
	n, err := io.Copy(u.w, file)
	CACHELOCK.Unlock()

	if n == 0 {
		STOP <- errors.New("no bytes read from file: " + u.path)
	}
	if err != nil {
		STOP <- err
	}

	u.done <- struct{}{}
}

type UiCacheDelete struct {
	done chan struct{}
	path string
}

func (u UiCacheDelete) output() {
	CACHELOCK.Lock()
	err := os.Remove(u.path)
	CACHELOCK.Unlock()

	if err != nil {
		STOP <- err
	}

	u.done <- struct{}{}
}

func (w WhitelistAdd) handle(u UiInWithUrl) Output {
	return ReadWhitelistee{
		w:    u.w,
		r:    u.r,
		done: u.done,
	}
}

type ReadWhitelistee struct {
	w    http.ResponseWriter
	r    *http.Request
	done chan struct{}
}

func (r ReadWhitelistee) output() {
	whitelistee := make([]byte, constants.IdLength)
	n, err := r.r.Body.Read(whitelistee)
	INPUT <- TriedReadingWhitelistee{
		authCode:    <-AUTHCODE,
		myId:        <-MYID,
		secretSign:  *(<-MYKEYS).sign.secret,
		powInfo:     <-POWINFO,
		whitelistee: whitelistee,
		w:           r.w,
		done:        r.done,
		n:           n,
		err:         err,
	}
}

type ReadDraftAndRecipient struct {
	w       http.ResponseWriter
	r       *http.Request
	done    chan struct{}
	draftId string
}

func (r ReadDraftAndRecipient) output() {
	recipient := make([]byte, constants.IdLength)
	n, httpErr := r.r.Body.Read(recipient)
	draft, fileErr := ioutil.ReadFile(cachePath(r.draftId))
	INPUT <- TriedReadingDraftAndRecipient{
		w:         r.w,
		r:         r.r,
		done:      r.done,
		n:         n,
		httpErr:   httpErr,
		draft:     draft,
		fileErr:   fileErr,
		recipient: recipient,
		draftId:   r.draftId,
	}
}




type TriedReadingDraftAndRecipient struct {
	w         http.ResponseWriter
	r         *http.Request
	done      chan struct{}
	n         int
	httpErr   error
	draft     []byte
	fileErr   error
	recipient []byte
	draftId   string
}

type Draft struct {
	id        string
	subject   string
	to        []byte
	time      string
	userInput string
	code      MaybeCode
	blobs     []Blob
}

type MaybeCode interface {
	f()
}

// Messages are encoded as follows:
// + sized encoded message metadata
// + number of blobs
// + blobs
//		Each blob is encoded like:
// 		+ metadata
//		+ sized blob body
func (s SendMessageRequest) output() {
	encoded := make(chan EncodingChunk)
	INPUTS <- EncodingChunks(encoded)

	draftLength := len(s.rawDraft)
	encodedDraft := make([]byte, 4 + draftLength)
	copy(encodedDraft, encodeUint32(draftLength))
	copy(encodedDraft[4:], s.drawDraft)
	chunkUpBytes(bytes.NewBuffer(encodedDraft), encoded)


}

type NoCode struct{}

type Code struct {
	contents []byte
	filename string
}

type Blob struct {
	hash [hashLen]byte
	size     int
}

func parseString(raw []byte, pos int) (string, int, error) {
	bs, pos, err := parseBytes(raw, pos)
	if err != nil {
		return "", pos, err
	}
	return string(bs), pos, nil
}

func parseBytes(raw []byte, pos int) ([]byte, int, error) {
	length, pos, err := parseUint32(raw, pos)
	if err != nil {
		return []byte{}, pos, err
	}

	return raw[4:length], pos + length, nil
}

func parseHash(raw []byte, pos int) ([hashLen]byte, int, error) {
	rawLen := len(raw)
	if rawLen < pos + hashLen {
		return *new([hashLen]byte), pos, fmt.Errorf("raw must be at least %d bytes, but got %d", hashLen, rawLen)
	}

	var hash [hashLen]byte
	copy(hash[:], raw)
	return hash, pos + hashLen, nil
}

func parseDraft(raw []byte) (Draft, error) {
	var draft Draft
	pos := 0

	id, pos, err := parseString(raw, pos)
	if err != nil {
		return draft, err
	}

	subject, pos, err := parseString(raw, pos)
	if err != nil {
		return draft, err
	}

	to, pos, err := parseBytes(raw, pos)
	if err != nil {
		return draft, err
	}

	time_, pos, err := parseString(raw, pos)
	if err != nil {
		return draft, err
	}

	userInput, pos, err := parseString(raw, pos)
	if err != nil {
		return draft, err
	}

	code, pos, err := parseCode(raw, pos)
	if err != nil {
		return draft, err
	}

	blobs, pos, err := parseBlobs(raw, pos)
	if err != nil {
		return draft, err
	}

	return Draft{
		id:        id,
		subject:   subject,
		to:        to,
		time:      time_,
		userInput: userInput,
		code:      code,
		blobs:     blobs,
	}, nil
}

func parseUint32(raw []byte, pos int) (int, int, error) {
	if len(raw) < 4 {
		return 0, pos, errors.New("raw is less than 4 bytes long")
	}
	return decodeInt(raw[pos : pos+4]), pos + 4, nil
}

func parseBlobs(raw []byte, pos int) ([]Blob, int, error) {

	length, pos, err := parseUint32(raw, pos)
	if err != nil {
		return []Blob{}, pos, err
	}

	blobs := make([]Blob, length)

	for i, _ := range blobs {
		blob, pos, err := parseBlob(raw, pos)
		if err != nil {
			return blobs, pos, err
		}

		blobs[i] = blob
	}

	return blobs, pos, nil
}

func parseCode(raw []byte, pos int) (MaybeCode, int, error) {
	if len(raw) == 0 {
		return *new(MaybeCode), pos, errors.New("empty byte slice")
	}

	indicator := raw[0]
	pos += 1

	if indicator == 0 {
		return NoCode{}, pos, nil
	}

	if indicator != 1 {
		return *new(MaybeCode), pos, errors.New("indicator is not 0 or 1")
	}

	contents, pos, err := parseBytes(raw, pos)
	if err != nil {
		return *new(MaybeCode), pos, err
	}

	filename, pos, err := parseString(raw, pos)
	return Code{
		contents: contents,
		filename: filename,
	}, pos, nil
}

func (Code) f() {}

func (NoCode) f() {}

const maxChunkSize = 15500

func encodeUint64(theInt int64) []byte {
	result := make([]byte, 8)
	for i, _ := range result {
		result[i] = byte((theInt >> (i * 8)) & 0xFF)
	}
	return result
}

func encodeUint32(theInt int) []byte {
	result := make([]byte, 4)
	for i, _ := range result {
		result[i] = byte((theInt >> (i * 8)) & 0xFF)
	}
	return result
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func ceilDiv(a, b int) int {
	if a == b {
		return 1
	}
	return (a / b) + 1
}

func chunkUpDraft(raw []byte) [][]byte {
	rawLen := len(raw)
	if rawLen <= maxChunkSize {
		return [][]byte{append([]byte{0x00}, raw...)}
	}

	hash := sha256.Sum256(raw)
	numChunks := ceilDiv(rawLen, maxChunkSize)
	chunks := make([][]byte, numChunks)
	for i, _ := range chunks {
		chunkStart := i * maxChunkSize
		chunkEnd := min((i+1)*maxChunkSize, rawLen)
		chunk := make([]byte, 1+32+4+chunkEnd-chunkStart)
		chunk[0] = 1
		copy(chunk[1:], hash[:])
		copy(chunk[1+32:], encodeUint32(i))
		copy(chunk[1+32+4:], raw[chunkStart:chunkEnd])
		chunks[i] = chunk
	}
	return chunks
}

func (t TriedReadingDraftAndRecipient) update() Output {
	fail := func(msg string) HttpFail {
		return HttpFail{
			w:      t.w,
			status: 400,
			msg:    []byte(msg),
			done:   t.done,
		}
	}
	if t.n != constants.IdLength {
		return fail("couldn't read recipient ID from body")
	}
	if t.httpErr != nil {
		return fail(t.httpErr.Error())
	}
	if t.fileErr != nil {
		return fail(t.fileErr.Error())
	}
	draft, err := parseDraft(t.draft)
	if err != nil {
		return fail(err.Error())
	}
	return StartSendingDraft{
		draftChunks: chunkUpDraft(t.draft),
		blobs:       draft.blobs,
		recipient:   t.recipient,
		w:           t.w,
		done:        t.done,
		draftId:     t.draftId,
	}
}

type StartSendingDraft struct {
	draftChunks [][]byte
	blobs       []Blob
	recipient   []byte
	w           http.ResponseWriter
	done        chan struct{}
	draftId     string
}

func (s StartSendingDraft) output() {
	errs := make(chan error)
	for _, chunk := range s.draftChunks {
		INPUT <- ChunkToSend{
			chunk:     chunk,
			recipient: s.recipient,
			errs:      errs,
		}
	}

	for _, blob := range s.blobs {
		INPUT <- BlobToSend{
			blob:      blob,
			recipient: s.recipient,
			errs:      errs,
		}
	}

	for _, _ = range s.draftChunks {
		for _, _ = range s.blobs {
			err := <-errs
			if err != nil {
				INPUT <- FailedSend{
					recipient: s.recipient,
					w:         s.w,
					done:      s.done,
					err:       err,
				}
				return
			}
		}
	}

	s.done <- struct{}{}
}

func (c ChunkToSend) update() Output {
	firstPart := make([]byte, 16+16+13)
	copy(firstPart, constants.SendMessage)
	copy(firstPart[16:], c.authCode)
	copy(firstPart[16+16:], c.recipient)
	encrypted := box.Seal(
		firstPart,
		c.chunk,
		&c.nonce,
		&c.recipientKey,
		c.keys.encrypt.secret)
	signed := sign.Sign(c.myId, encrypted, c.keys.sign.secret)
	return SendChunk{
		chunk: signed,
		errs:  c.errs,
	}
}

type SendChunk struct {
	chunk []byte
	errs  chan error
}

func (s SendChunk) output() {
	_, err := http.Post(
		serverHttpUrl+"/message/send",
		"application/octet-stream",
		bytes.NewBuffer(s.chunk))
	s.errs <- err
}

type ChunkToSend struct {
	chunk        []byte
	recipient    []byte
	recipientKey [32]byte
	keys         MyKeys
	myId         []byte
	authCode     []byte
	errs         chan error
	nonce        [24]byte
}

type BlobToSend struct {
	blob      Blob
	recipient []byte
	errs      chan error
}

func (b BlobToSend) update() Output {
	if b.blob.size <= maxChunkSize {
		return SendSmallBlob{
			path:      cachePath(b.blob.id),
			recipient: b.recipient,
			errs:      b.errs,
		}
	}

	return SendLargeBlob{
		id:        b.blob.id,
		recipient: b.recipient,
		errs:      b.errs,
	}
}

type SendSmallBlob struct {
	path      string
	recipient []byte
	errs      chan error
}

func (s SendSmallBlob) output() {
	CACHELOCK.Lock()
	contents, err := ioutil.ReadFile(s.path)
	CACHELOCK.Unlock()
	if err != nil {
		STOP <- err
	}
	INPUT <- ChunkToSend{
		recipient: s.recipient,
		errs:      s.errs,
		chunk:     append([]byte{0, 2}, contents...),
	}
}

type SendLargeBlob struct {
	id        string
	recipient []byte
	errs      chan error
}

func (s SendLargeBlob) output() {
}

type FailedSend struct {
	recipient []byte
	w         http.ResponseWriter
	done      chan struct{}
	err       error
}

func (f FailedSend) update() Output {
	return HttpFail{
		w:      f.w,
		status: 500,
		msg:    []byte(f.err.Error()),
		done:   f.done,
	}
}

type CacheGetRequest string

type CacheSetRequest string

type CacheDeleteRequest string

type WhitelistAdd struct{}

type WhitelistRemove struct{}

type Route interface {
	handle(UiInWithUrl) Output
}

func parsePath(raw string) (Route, error) {
	parts := strings.Split(raw, "/")

	cacheGet, err := cacheGetP(parts)
	if err == nil {
		return cacheGet, nil
	}

	cacheSet, err := cacheSetP(parts)
	if err == nil {
		return cacheSet, nil
	}

	cacheDelete, err := cacheDeleteP(parts)
	if err == nil {
		return cacheDelete, nil
	}

	sendMessage, err := sendMessageP(parts)
	if err == nil {
		return sendMessage, nil
	}

	whitelistAdd, err := whitelistAddP(parts)
	if err == nil {
		return whitelistAdd, nil
	}

	whitelistRemove, err := whitelistRemoveP(parts)
	if err == nil {
		return whitelistRemove, nil
	}

	return *new(Route), errors.New("bad path: " + raw)
}

func (u UiInWithUrl) update() Output {
	route, err := parsePath(u.path)
	if err != nil {
		return HttpFail{
			w:      u.w,
			status: 400,
			msg:    []byte(err.Error()),
			done:   u.done,
		}
	}

	return route.handle(u)
}

type EncryptedAndSigned struct {
	fromId []byte
	signed []byte
}

type TheirKeys struct {
	encrypt [32]byte
	sign    [32]byte
}

type MsgFromServerContext struct {
	myId          []byte
	signed        []byte
	fromId        []byte
	contacts      bytesliceSet
	theirKeys     TheirKeys
	theirKeysErr  error
	secretSign    [64]byte
	secretEncrypt [32]byte
}

type Encrypted struct {
	meaning   []byte
	recipient []byte
	encrypted []byte
	nonce     [constants.NonceLength]byte
}

func parseUnsigned(raw []byte) (Encrypted, error) {
	const afterMeaning = constants.MeaningLength
	const afterAuth = afterMeaning + constants.AuthCodeLength
	const afterId = afterAuth + constants.IdLength
	const afterNonce = afterId + constants.NonceLength

	if len(raw) < afterNonce+1 {
		return *new(Encrypted), errors.New("raw unsigned was too short")
	}

	var nonce [constants.NonceLength]byte
	copy(nonce[:], raw[afterId:])

	return Encrypted{
		meaning:   raw[:afterMeaning],
		recipient: raw[afterAuth:afterId],
		encrypted: raw[afterId:],
		nonce:     nonce,
	}, nil
}

type ClientChunk interface {
	handle() Output
}

type SmallClientChunk []byte

type Log string

var backendDir = filepath.Join(homeDir, "backend")

var logPath = filepath.Join(backendDir, "log")

var homeDir = "clientData"

func (log Log) output() {
	LOG <- string(log)
}

type Panic string

func (p Panic) output() {
	panic(string(p))
}

type DoNothing struct{}

func (DoNothing) output() {}

type Todo struct{}

func (Todo) output() {
}

type GoodNetwork struct{}

type BadNetwork struct{}

func (GoodNetwork) update() Output {
	msg := base64.StdEncoding.EncodeToString([]byte{4})
	return ToWebsocket(msg)
}

type ToWebsocket []byte

func (t ToWebsocket) output() {
	TOWEBSOCKET <- []byte(t)
}

func (BadNetwork) update() Output {
	msg := base64.StdEncoding.EncodeToString([]byte{3})
	return ToWebsocket(msg)
}

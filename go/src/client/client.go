package main

import (
	"archive/tar"
	"bytes"
	"common"
	"crypto/rand"
	"crypto/subtle"
	"encoding/base64"
	"encoding/binary"
	"encoding/gob"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"math/big"
	"mime/multipart"
	"net"
	"net/http"
	"os"
	"sort"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/pkg/browser"
	"goji.io"
	"goji.io/pat"
	"golang.org/x/crypto/argon2"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/nacl/sign"
	"golang.org/x/crypto/ssh/terminal"
)

var apps []appMsgT
var appsMux sync.Mutex
var homeCode string
var appCodes = map[string]blake2bHash{}
var appCodesMux sync.Mutex
var publicSign publicSignT
var secretSign [64]byte
var invites = map[inviteT]struct{}{}
var invitesMux sync.Mutex
var uninvites = map[inviteT]struct{}{}
var chunksLoading = map[blake2bHash][]fileChunkPtrT{}
var chunksLoadingMux sync.Mutex
var dataDir string
var port string
var chunksAwaitingReceipt = map[blake2bHash]chunkAwaitingReceiptT{}
var chunksAwaitingReceiptMux sync.Mutex
var appsAwaitingReceipt = map[blake2bHash]publicSignT{}
var appsAwaitingReceiptMux sync.Mutex
var symmetricKeys = map[publicSignT]symmetricEncrypt{}
var symmetricKeysMux sync.Mutex
var keyPairs = map[publicEncryptT]secretEncryptT{}
var keyPairsMux sync.Mutex
var awaitingSymmetricKey = map[publicSignT]sendChunkT{}
var awaitingSymmetricKeyMux sync.Mutex
var tcpInChan = make(chan common.ClientToClient)
var tcpOutChan = make(chan common.ClientToClient)

type publicSignT [32]byte
type publicEncryptT [32]byte
type secretEncryptT [32]byte
type blake2bHash [32]byte
type symmetricEncrypt [32]byte
type secretSignT [64]byte

func makeMemberList() map[publicSignT]struct{} {
	members := make(map[publicSignT]struct{})
	members[common.TruesPubSign] = struct{}{}
	addedMember := true
	for addedMember {
		addedMember = false
		for invite, _ := range invites {
			_, alreadyMember := members[invite.Invitee]
			if alreadyMember {
				continue
			}
			_, ok := members[invite.Author]
			if !ok {
				continue
			}
			for uninvite, _ := range uninvites {
				if uninviteCancels(uninvite, invite) {
					continue
				}
			}
			members[invite.Invitee] = struct{}{}
			addedMember = true
		}
		if !addedMember {
			break
		}
	}
	return members
}

func processInvites(
	rawInvites []byte,
	err error) (map[inviteT]struct{}, error) {

	invs := make(map[inviteT]struct{})
	if err == nil {
		return invs, nil
	}
	var invitesSlice []inviteT
	err = json.Unmarshal(rawInvites, &invitesSlice)
	if err != nil {
		return invs, nil
	}
	for _, invite := range invitesSlice {
		invs[invite] = struct{}{}
	}
	return invs, err
}

func appsFile() string {
	return dataDir + "/apps.txt"
}

func equalHashes(as [32]byte, bs [32]byte) bool {
	for i, b := range bs {
		if as[i] != b {
			return false
		}
	}
	return true
}

func uninviteCancels(u inviteT, i inviteT) bool {
	if !equalHashes(u.Invitee, i.Invitee) {
		return false
	}
	if !equalHashes(u.Author, i.Author) {
		return false
	}
	return u.PosixTime > i.PosixTime
}

type inviteT struct {
	PosixTime int64
	Invitee   [32]byte
	Author    [32]byte
	Signature [common.SigSize]byte
}

func getFilePart(
	bodyFileReader *multipart.Reader) (*multipart.Part, error) {

	filepart, err := bodyFileReader.NextPart()
	if err != nil {
		return filepart, err
	}
	var filepartname string = filepart.FormName()
	if filepartname != "file" {
		msg := "Could not find form element \"file\"."
		return filepart, errors.New(msg)
	}
	return filepart, nil
}

func getTagsPart(bodyFileReader *multipart.Reader) ([]byte, error) {
	tagsPart, err := bodyFileReader.NextPart()
	if err != nil {
		return *new([]byte), err
	}
	if tagsPart.FormName() != "tags" {
		msg := "Could not find form element \"tags\"."
		return *new([]byte), errors.New(msg)
	}
	return ioutil.ReadAll(tagsPart)
}

func writeAppToFile(
	r *http.Request) (string, map[string]struct{}, error) {

	bodyFileReader, err := r.MultipartReader()
	if err != nil {
		return "", make(map[string]struct{}), err
	}
	filepart, err := getFilePart(bodyFileReader)
	if err != nil {
		return "", make(map[string]struct{}), err
	}
	tmpFileName, err := genCode()
	if err != nil {
		return "", make(map[string]struct{}), err
	}
	tmpPath := dataDir + "/tmp/" + tmpFileName
	fileHandle, err := os.Create(tmpPath)
	defer fileHandle.Close()
	if err != nil {
		return "", make(map[string]struct{}), err
	}
	hasher, err := blake2b.New256(nil)
	if err != nil {
		return "", make(map[string]struct{}), err
	}
	tee := io.TeeReader(filepart, hasher)
	_, err = io.Copy(fileHandle, tee)
	if err != nil {
		return "", make(map[string]struct{}), err
	}
	hash := base64.URLEncoding.EncodeToString(hasher.Sum(nil))
	err = os.Rename(tmpPath, dataDir+"/apps/"+hash)
	if err != nil {
		return "", make(map[string]struct{}), err
	}
	tagBytes, err := getTagsPart(bodyFileReader)
	if err != nil {
		return "", make(map[string]struct{}), err
	}
	tags, err := parseTags(tagBytes)
	if err != nil {
		return "", make(map[string]struct{}), err
	}
	return hash, tags, nil
}

func tagOk(tag string) error {
	if len(tag) > 100 {
		return errors.New("Tag too long.")
	}
	return nil
}

func parseTags(bs []byte) (map[string]struct{}, error) {
	f := func(c rune) bool {
		return c == ','
	}
	tagslice := strings.FieldsFunc(string(bs), f)
	tagmap := make(map[string]struct{})
	for _, tag := range tagslice {
		err := tagOk(tag)
		if err != nil {
			return tagmap, err
		}
		tagmap[tag] = struct{}{}
	}
	return tagmap, nil
}

func hashFromString(s string) ([32]byte, error) {
	hashSlice, err := base64.URLEncoding.DecodeString(s)
	if err != nil {
		return *new([32]byte), err
	}
	if len(hashSlice) != 32 {
		return *new([32]byte), errors.New(
			"Hash wrong length.")
	}
	var hash [32]byte
	for i, b := range hashSlice {
		hash[i] = b
	}
	return hash, nil
}

func makeConn(
	secretSign [64]byte) (net.Conn, error) {

	conn, err := net.Dial("tcp", "localhost:4000")
	if err != nil {
		return conn, err
	}
	enc := gob.NewEncoder(conn)
	dec := gob.NewDecoder(conn)
	var authCode [common.AuthCodeLength]byte
	err = dec.Decode(&authCode)
	if err != nil {
		return conn, err
	}
	authSig := common.AuthSigT{
		publicSign,
		signedAuthToSlice(sign.Sign(
			make([]byte, 0),
			common.AuthCodeToSlice(
				authCode),
			&secretSign)),
	}
	err = enc.Encode(authSig)
	return conn, err
}

func tcpServer() {
	stop := make(chan struct{})
	for {
		conn, err := makeConn(secretSign)
		if err != nil {
			time.Sleep(time.Second)
			continue
		}
		func() {
			go func() {
				for {
					msg, err := common.ReadClientToClient(conn)
					if err != nil {
						stop <- struct{}{}
					}
					tcpInChan <- msg
				}
			}()
			go func() {
				for {
					msg, err := common.EncodeClientToClient(
						<-tcpOutChan)
					if err != nil {
						continue
					}
					n, err := conn.Write(msg)
					if err != nil {
						stop <- struct{}{}
					}
					if n != len(msg) {
						stop <- struct{}{}
					}
				}
			}()
			<-stop
		}()
	}
}

func strEq(s1, s2 string) bool {
	eq := subtle.ConstantTimeCompare([]byte(s1), []byte(s2))
	return eq == 1
}

func getDocHash(securityCode string) (blake2bHash, error) {

	appCodesMux.Lock()
	defer appCodesMux.Unlock()
	for sc, hash := range appCodes {
		if strEq(sc, securityCode) {
			return hash, nil
		}
	}
	return *new(blake2bHash), errors.New("No document hash found.")
}

func hashToStr(h [32]byte) string {
	asSlice := common.HashToSlice(h)
	return base64.URLEncoding.EncodeToString(asSlice)
}

type sendAppJsonT struct {
	AppHash   string
	Recipient string
}

type logSendErrT struct {
	PosixTime int64
	AppHash   [32]byte
	Recipient [32]byte
	Err       error
}

func logSendErr(err error, appHash [32]byte, recipient [32]byte) {
	msg := logSendErrT{
		time.Now().Unix(),
		appHash,
		recipient,
		err,
	}
	encoded, jsonErr := json.Marshal(msg)
	if jsonErr != nil {
		fmt.Print(jsonErr)
		return
	}
	f, openErr := os.OpenFile(
		dataDir+"/sendErrors.txt",
		appendFlags,
		0600)
	defer f.Close()
	if openErr != nil {
		fmt.Print(openErr)
		return
	}
	_, writeErr := f.Write(append([]byte("\n"), encoded...))
	if writeErr != nil {
		fmt.Print(writeErr)
	}
}

type logSentSuccessT struct {
	appHash   [32]byte
	recipient [32]byte
}

func logSentSuccess(
	appHash blake2bHash,
	recipient publicSignT) {

	msg := logSentSuccessT{appHash, recipient}
	encoded, jsonErr := json.Marshal(msg)
	if jsonErr != nil {
		logSendErr(jsonErr, appHash, recipient)
		return
	}
	f, openErr := os.OpenFile(
		dataDir+"/sentMsgPath", appendFlags, 0600)
	if openErr != nil {
		logSendErr(openErr, appHash, recipient)
		return
	}
	defer f.Close()
	_, writeErr := f.Write(append([]byte("\n"), encoded...))
	if writeErr != nil {
		logSendErr(writeErr, appHash, recipient)
	}
}

var receiptCode = [16]byte{
	0xdc, 0x61, 0x9b, 0xd6, 0xec, 0xb3, 0xe8, 0x24, 0x23, 0xcc,
	0x4b, 0xef, 0xbe, 0xae, 0xb4, 0xeb}

func receiptHash(appHash [32]byte, contextCode [16]byte) []byte {
	concat := make([]byte, 48)
	i := 0
	for ; i < 32; i++ {
		concat[i] = appHash[i]
	}
	for ; i < 48; i++ {
		concat[i] = contextCode[i-32]
	}
	return common.HashToSlice(blake2b.Sum256(concat))
}

var appReceiptCode = [16]byte{
	0xaf, 0xa1, 0x4b, 0xde, 0x8c, 0x62, 0x94, 0x65, 0xe6, 0x1b,
	0x8f, 0xee, 0x21, 0x1b, 0x22, 0x82}

var appMsgCode = [16]byte{159, 43, 151, 217, 160, 129, 184, 128,
	213, 154, 17, 231, 181, 214, 127, 163}

func concatTags(t map[string]struct{}) []byte {
	tagSlice := make([]string, len(t))
	i := 0
	for tag, _ := range t {
		tagSlice[i] = tag
	}
	sort.Strings(tagSlice)
	strConcat := ""
	for _, tag := range tagSlice {
		strConcat = strConcat + tag
	}
	return []byte(strConcat)
}

func encodeInt64(i int64) []byte {
	b := make([]byte, 8)
	binary.LittleEndian.PutUint64(b, uint64(i))
	return b
}

type appMsgT struct {
	Author    [32]byte
	Tags      map[string]struct{}
	AppHash   [32]byte
	PosixTime int64
	Sig       [common.SigSize]byte
}

func makeChunkFilePaths(ptrs []fileChunkPtrT) []string {
	filePaths := make([]string, len(ptrs))
	for i, ptr := range ptrs {
		filePaths[i] = makeChunkFilePath(ptr.chunkHash)
	}
	return filePaths
}

func makeAppPath(appHash blake2bHash) string {
	fileName := base64.URLEncoding.EncodeToString(
		common.HashToSlice(appHash))
	return dataDir + "/apps/" + fileName
}

func (appSig appMsgT) process(author publicSignT) {
	signedHash, ok := sign.Open(
		make([]byte, 0),
		common.SigToSlice(appSig.Sig),
		&appSig.Author)
	appHash := common.HashToSlice(hashApp(
		appSig.Tags, appSig.AppHash, appSig.PosixTime))
	if !(ok && bytes.Equal(appHash, signedHash)) {
		return
	}
	chunksLoadingMux.Lock()
	defer chunksLoadingMux.Unlock()
	delete(chunksLoading, appSig.AppHash)
	chunkPtrs, ok := chunksLoading[appSig.AppHash]
	if !ok {
		return
	}
	finalChunkPtr := chunkPtrs[len(chunkPtrs)-1]
	if !finalChunkPtr.lastChunk {
		return
	}
	filePaths := makeChunkFilePaths(chunkPtrs)
	err := assembleApp(filePaths, author, appSig)
	delete(chunksLoading, appSig.AppHash)
	if err != nil {
		return
	}
	_ = sendAppReceipt(appSig.AppHash, author)
}

func hashApp(
	tags map[string]struct{},
	appHash blake2bHash,
	posixTime int64) blake2bHash {

	tagBytes := concatTags(tags)
	lenTags := len(tagBytes)
	concat := make([]byte, lenTags+32+8+32)
	i := 0
	for ; i < lenTags; i++ {
		concat[i] = tagBytes[i]
	}
	for ; i < lenTags+32; i++ {
		concat[i] = appHash[i-lenTags]
	}
	timeBytes := encodeInt64(posixTime)
	for ; i < lenTags+32+8; i++ {
		concat[i] = timeBytes[i-lenTags-32]
	}
	for ; i < lenTags+32+8+32; i++ {
		concat[i] = appMsgCode[i-lenTags-32-8]
	}
	return blake2b.Sum256(concat)
}

type searchResultT struct {
	Author    string
	Tags      []string
	Hash      string
	Posixtime int64
}

func requestEncryptionKey(sendChunk sendChunkT) {
	pub, priv, err := box.GenerateKey(rand.Reader)
	if err != nil {
		return
	}
	tcpOutChan <- common.ClientToClient{
		Msg:       common.GiveMeASymmetricKey{*pub},
		Recipient: sendChunk.recipient,
		Author:    publicSign,
	}
	awaitingSymmetricKeyMux.Lock()
	awaitingSymmetricKey[sendChunk.recipient] = sendChunk
	awaitingSymmetricKeyMux.Unlock()
	keyPairsMux.Lock()
	keyPairs[publicEncryptT(*pub)] = secretEncryptT(*priv)
	keyPairsMux.Unlock()
}

func hashHereIsKey(h common.HereIsAnEncryptionKey) blake2bHash {
	result := make([]byte, 32+common.EncryptedKeyLen+24)
	i := 0
	for ; i < 32; i++ {
		result[i] = h.MyPublicEncrypt[i]
	}
	for ; i < 32+common.EncryptedKeyLen; i++ {
		result[i] = h.EncryptedSymmetricKey[i-32]
	}
	for ; i < 32+common.EncryptedKeyLen+24; i++ {
		result[i] = h.Nonce[i-32-common.EncryptedKeyLen]
	}
	return blake2b.Sum256(result)
}

func sliceToSig(bs []byte) [common.SigSize]byte {
	var result [common.SigSize]byte
	for i, b := range bs {
		result[i] = b
	}
	return result
}

type fileChunkPtrT struct {
	chunkHash [32]byte
	counter   int
	lastChunk bool
}

func sendChunk(s sendChunkT) {
	errOut := func(err error) {
		fmt.Println(err)
		logSendErr(err, s.appMsg.AppHash, s.recipient)
	}
	chunkBuffer := make([]byte, common.ChunkContentSize)
	numBytesRead, err := s.file.Read(chunkBuffer)
	if err != nil {
		errOut(err)
		return
	}
	chunk := chunkBuffer[:numBytesRead]
	lastChunk := numBytesRead < common.ChunkContentSize
	var beforeEncoding Decrypted
	beforeEncoding = FileChunk{
		AppHash:   s.appMsg.AppHash,
		Chunk:     chunk,
		Counter:   s.counter,
		LastChunk: lastChunk,
	}
	encodedMsg, err := common.EncodeData(&beforeEncoding)
	if err != nil {
		errOut(err)
		return
	}
	nonce, err := makeNonce()
	if err != nil {
		errOut(err)
		return
	}
	symmetricKeysMux.Lock()
	symmetricEncrypt, ok := symmetricKeys[s.recipient]
	symmetricKeysMux.Unlock()
	symEncArr := [32]byte(symmetricEncrypt)
	if !ok {
		errOut(errors.New("no symmetric key"))
		return
	}
	tcpOutChan <- common.ClientToClient{
		Msg: common.Encrypted{
			Msg: secretbox.Seal(
				make([]byte, 0),
				encodedMsg,
				&nonce,
				&symEncArr),
			Nonce: nonce,
		},
		Recipient: s.recipient,
		Author:    publicSign,
	}
	c := chunkAwaitingReceiptT{
		s.appMsg,
		s.file,
		s.recipient,
		blake2bHash(blake2b.Sum256(chunk)),
		s.counter,
		lastChunk,
	}
	chunksAwaitingReceipt[c.chunkHash] = c
}

type Decrypted interface {
	process(publicSignT)
}

type ReceiptT struct {
	Sig       [common.SigSize]byte
	ChunkHash blake2bHash
}

type AppReceiptT struct {
	Sig     [common.SigSize]byte
	AppHash [32]byte
}

type FileChunk struct {
	AppHash   [32]byte
	Chunk     []byte
	Counter   int
	LastChunk bool
}

type searchQueryT struct {
	Tags         []string
	SearchString string
}

func isSubset(sub []string, super map[string]struct{}) bool {
	for _, s := range sub {
		_, ok := super[s]
		if !ok {
			return false
		}
	}
	return true
}

func matchesSearch(searchString string, tags map[string]struct{}) bool {
	for tag, _ := range tags {
		if strings.Contains(tag, searchString) {
			return true
		}
	}
	return false
}

func matchingApp(app appMsgT, q searchQueryT) bool {
	return isSubset(q.Tags, app.Tags) &&
		matchesSearch(q.SearchString, app.Tags)
}

func filterApps(q searchQueryT) []appMsgT {
	var filtered []appMsgT
	appsMux.Lock()
	defer appsMux.Unlock()
	for _, app := range apps {
		if matchingApp(app, q) {
			filtered = append(filtered, app)
		}
	}
	return filtered
}

func setToSlice(set map[string]struct{}) []string {
	result := make([]string, len(set))
	i := 0
	for s, _ := range set {
		result[i] = s
		i++
	}
	return result
}

func appToSearchResult(app appMsgT) searchResultT {
	author := base64.URLEncoding.EncodeToString(common.HashToSlice(app.Author))
	appHash := base64.URLEncoding.EncodeToString(common.HashToSlice(app.AppHash))
	return searchResultT{
		author,
		setToSlice(app.Tags),
		appHash,
		app.PosixTime,
	}
}

func search(q searchQueryT) (searchResultsT, error) {
	filtered := filterApps(q)
	matchingApps := make([]searchResultT, len(filtered))
	matchingTags := make(map[string]struct{})
	for i, app := range filtered {
		matchingApps[i] = appToSearchResult(app)
		for tag, _ := range app.Tags {
			if strings.Contains(tag, q.SearchString) {
				matchingTags[tag] = struct{}{}
			}
		}
	}
	return searchResultsT{
		Apps: matchingApps,
		Tags: setToSlice(matchingTags),
	}, nil
}

type searchResultsT struct {
	Apps []searchResultT
	Tags []string
}

var inviteContext = [16]byte{49, 46, 232, 88, 87, 218, 38, 83, 52, 64, 244, 143, 33, 23, 18, 19}

func inviteHash(posixTime int64, invitee [32]byte) [32]byte {
	concat := make([]byte, 56)
	i := 0
	for ; i < 32; i++ {
		concat[i] = invitee[i]
	}
	encodedTime := encodeInt64(posixTime)
	for ; i < 40; i++ {
		concat[i] = encodedTime[i-32]
	}
	for ; i < 56; i++ {
		concat[i] = inviteContext[i-40]
	}
	return blake2b.Sum256(concat)
}

func invitesToSlice() []inviteT {
	invitesSlice := make([]inviteT, len(invites))
	i := 0
	for invite, _ := range invites {
		invitesSlice[i] = invite
		i++
	}
	return invitesSlice
}

func getHashSecurityCode(hash blake2bHash) (string, error) {
	for c, h := range appCodes {
		if equalHashes(h, hash) {
			return c, nil
		}
	}
	return "", errors.New("Could not find app.")
}

func decodeMsg(bs []byte) (Decrypted, error) {
	var buf bytes.Buffer
	n, err := buf.Write(bs)
	var msg Decrypted
	if n != len(bs) {
		return msg, errors.New("Could not read whole messag.")
	}
	if err != nil {
		return msg, err
	}
	dec := gob.NewDecoder(&buf)
	err = dec.Decode(&msg)
	return msg, err
}

func authBytes(author publicSignT) *[32]byte {
	asBytes := [32]byte(author)
	return &asBytes
}

func (appReceipt AppReceiptT) process(author publicSignT) {
	signed, ok := sign.Open(
		make([]byte, 0),
		common.SigToSlice(appReceipt.Sig),
		authBytes(author))
	if !ok {
		return
	}
	if !bytes.Equal(signed, common.HashToSlice(appReceipt.AppHash)) {
		return
	}
	appsAwaitingReceiptMux.Lock()
	appAuthor, ok := appsAwaitingReceipt[appReceipt.AppHash]
	if !ok {
		return
	}
	if appAuthor != author {
		return
	}
	delete(appsAwaitingReceipt, appReceipt.AppHash)
	appsAwaitingReceiptMux.Unlock()
	logSentSuccess(appReceipt.AppHash, author)
}

func (receipt ReceiptT) process(author publicSignT) {
	chunksAwaitingReceiptMux.Lock()
	defer chunksAwaitingReceiptMux.Unlock()
	c, ok := chunksAwaitingReceipt[receipt.ChunkHash]
	if !ok {
		return
	}
	if c.lastChunk {
		err := sendAppMsg(c.appMsg, author)
		if err != nil {
			logSendErr(err, c.appMsg.AppHash, author)
			return
		}
		appsAwaitingReceiptMux.Lock()
		appsAwaitingReceipt[c.appMsg.AppHash] = author
		appsAwaitingReceiptMux.Unlock()
	}
	sendChunk(sendChunkT{
		c.appMsg,
		c.file,
		c.recipient,
		c.counter + 1,
	})
}

func sendAppMsg(appMsg appMsgT, recipient publicSignT) error {
	symmetricKeysMux.Lock()
	symmetricKey, ok := symmetricKeys[recipient]
	symmetricKeysMux.Unlock()
	if !ok {
		return errors.New("couldn't find symmetric key")
	}
	encoded, err := common.EncodeData(Decrypted(appMsg))
	if err != nil {
		return err
	}
	nonce, err := makeNonce()
	if err != nil {
		return err
	}
	keyBytes := [32]byte(symmetricKey)
	encrypted := secretbox.Seal(
		make([]byte, 0),
		encoded,
		&nonce,
		&keyBytes)
	tcpOutChan <- common.ClientToClient{
		Msg:       common.Encrypted{encrypted, nonce},
		Recipient: recipient,
		Author:    publicSign,
	}
	return nil
}

const (
	appendFlags = os.O_APPEND | os.O_CREATE | os.O_WRONLY
)

var passwordChars = []rune("abcdefghjkmnpqrstuvwxyz23456789")

func makePassword() (string, error) {
	password := make([]rune, 10)
	numChars := big.NewInt(int64(len(passwordChars)))
	for i := range password {
		bigI, err := rand.Int(rand.Reader, numChars)
		if err != nil {
			return "", err
		}
		password[i] = passwordChars[int(bigI.Int64())]
	}
	return string(password), nil
}

func sliceToSecretKey(secret []byte) [64]byte {
	var result [64]byte
	for i, b := range secret {
		result[i] = b
	}
	return result
}

func secretKeyToSlice(secret [64]byte) []byte {
	result := make([]byte, 64)
	for i, b := range secret {
		result[i] = b
	}
	return result
}

func makeSalt() ([32]byte, error) {
	nonceSlice := make([]byte, 32)
	var nonce [32]byte
	n, err := rand.Read(nonceSlice)
	if err != nil {
		return nonce, err
	}
	if n != 32 {
		return nonce, errors.New("Faulty random bytes reader.")
	}
	for i, b := range nonceSlice {
		nonce[i] = b
	}
	return nonce, nil
}

func makeSymmetricKey() ([32]byte, error) {
	nonceSlice := make([]byte, 32)
	var nonce [32]byte
	n, err := rand.Read(nonceSlice)
	if err != nil {
		return nonce, err
	}
	if n != 32 {
		return nonce, errors.New("Faulty random bytes reader.")
	}
	for i, b := range nonceSlice {
		nonce[i] = b
	}
	return nonce, nil
}

func makeNonce() ([24]byte, error) {
	nonceSlice := make([]byte, 24)
	var nonce [24]byte
	n, err := rand.Read(nonceSlice)
	if err != nil {
		return nonce, err
	}
	if n != 24 {
		return nonce, errors.New("Faulty random bytes reader.")
	}
	for i, b := range nonceSlice {
		nonce[i] = b
	}
	return nonce, nil
}

func genCode() (string, error) {
	authSlice := make([]byte, 16)
	_, err := rand.Read(authSlice)
	if err != nil {
		return "", err
	}
	return base64.URLEncoding.EncodeToString(authSlice), nil
}

type chunkAwaitingReceiptT struct {
	appMsg    appMsgT
	file      *os.File
	recipient [32]byte
	chunkHash blake2bHash
	counter   int
	lastChunk bool
}

type sendChunkT struct {
	appMsg    appMsgT
	file      *os.File
	recipient [32]byte
	counter   int
}

func signedAuthToSlice(bs []byte) [common.AuthSigSize]byte {
	var result [common.AuthSigSize]byte
	for i, b := range bs {
		result[i] = b
	}
	return result
}

type secretsFileT struct {
	Publicsign          [32]byte
	Nonce               [24]byte
	Salt                [32]byte
	EncryptedSecretSign []byte
}

type keysT struct {
	publicsign [32]byte
	secretsign [64]byte
}

func slowHash(pw []byte, salt [32]byte) [32]byte {
	return common.SliceToHash(argon2.IDKey(
		pw,
		common.HashToSlice(salt),
		10,
		64*1024,
		4,
		32))
}

func extractKeys(password []byte, secretsFile []byte) (keysT, error) {
	var decoded secretsFileT
	err := json.Unmarshal(secretsFile, &decoded)
	var keys keysT
	if err != nil {
		return keys, err
	}
	symmetricKey := slowHash(password, decoded.Salt)
	secretSignBytes, ok := secretbox.Open(
		make([]byte, 0),
		decoded.EncryptedSecretSign,
		&decoded.Nonce,
		&symmetricKey)
	if !ok {
		return keys, errors.New("Could not decrypt keys.")
	}
	return keysT{
		decoded.Publicsign,
		sliceToSecretKey(secretSignBytes)}, nil
}

func createKeys(dataDir string) error {
	pubSign, secretSign, err := sign.GenerateKey(rand.Reader)
	if err != nil {
		return err
	}
	nonce, err := makeNonce()
	if err != nil {
		return err
	}
	salt, err := makeSalt()
	if err != nil {
		return err
	}
	password, err := makePassword()
	if err != nil {
		return err
	}
	fmt.Println(password)
	secretKey := slowHash([]byte(password), salt)
	encryptedSecrets := secretbox.Seal(
		make([]byte, 0),
		secretKeyToSlice(*secretSign),
		&nonce,
		&secretKey)
	secretsFile := secretsFileT{
		Publicsign:          *pubSign,
		Nonce:               nonce,
		Salt:                salt,
		EncryptedSecretSign: encryptedSecrets,
	}
	encodedFile, err := json.Marshal(secretsFile)
	if err != nil {
		fmt.Println("Could not encode.")
		return err
	}
	err = ioutil.WriteFile(keysFile(dataDir), encodedFile, 0600)
	return err
}

func invitesFile() string {
	return dataDir + "/invites.txt"
}

func uninvitesFile() string {
	return dataDir + "/uninvites.txt"
}

func keysFile(dataDir string) string {
	return dataDir + "/TOP_SECRET_DONT_SHARE.txt"
}

func gobRegister() {
	gob.Register(*new(common.ClientToClient))
	gob.Register(*new(common.Encrypted))
	gob.Register(*new(common.GiveMeASymmetricKey))
	gob.Register(*new(common.HereIsAnEncryptionKey))
	gob.Register(*new(AppReceiptT))
	gob.Register(*new(ReceiptT))
	gob.Register(*new(FileChunk))
	gob.Register(*new(appMsgT))
}

func getCryptoKeys() error {
	rawSecrets, err := ioutil.ReadFile(keysFile(dataDir))
	if err != nil {
		err := createKeys(dataDir)
		if err != nil {
			return err
		}
		rawSecrets, err = ioutil.ReadFile(keysFile(dataDir))
		if err != nil {
			return err
		}
	}
	fmt.Println("Please enter your password:")
	password, err := terminal.ReadPassword(int(syscall.Stdin))
	keys, err := extractKeys(password, rawSecrets)
	if err != nil {
		return err
	}
	secretSign = keys.secretsign
	publicSign = keys.publicsign
	return nil
}

func readArgs() error {
	args := os.Args
	if len(args) != 3 {
		return errors.New("There must be two command-line arguments.")
	}
	port = args[1]
	dataDir = args[2]
	return nil
}

func setup() error {
	gobRegister()
	err := readArgs()
	if err != nil {return err}
	err = os.RemoveAll(dataDir + "/tmp")
	if err != nil {return err}
	err = os.Mkdir(dataDir + "/tmp", 0755)
	if err != nil {return err}
	homeCode, err := genCode()
	if err != nil {return err}
	err = getCryptoKeys()
	if err != nil {return err}
	invites, err = processInvites(ioutil.ReadFile(invitesFile()))
	if err != nil {return err}
	uninvites, err = processInvites(ioutil.ReadFile(uninvitesFile()))
	if err != nil {return err}
	rawApps, err := ioutil.ReadFile(appsFile())
	if err != nil {return err}
	err = json.Unmarshal(rawApps, &apps)
	if err != nil {return err}
	err = browser.OpenURL(appUrl(homeCode))
	return err
}

func main() {
	err := setup()
	if err != nil {
		fmt.Println(err)
		return
	}
	go tcpServer()
	go httpServer()
	for {processTcpInput(<-tcpInChan)}
}

func processTcpInput(c common.ClientToClient) {
	invitesMux.Lock()
	_, isMember := makeMemberList()[c.Author]
	invitesMux.Unlock()
	if !isMember {
		return
	}
	switch c.Msg.(type) {
	case common.Encrypted:
		processEncrypted(
			(c.Msg).(common.Encrypted),
			c.Author)
	case common.GiveMeASymmetricKey:
		processGiveMeKey(
			(c.Msg).(common.GiveMeASymmetricKey),
			c.Author)
	case common.HereIsAnEncryptionKey:
		processHereIsAnEncryptionKey(
			(c.Msg).(common.HereIsAnEncryptionKey),
			c.Author)
	}
}

func (chunk FileChunk) process(author publicSignT) {
	chunksLoadingMux.Lock()
	defer chunksLoadingMux.Unlock()
	previousChunks, ok := chunksLoading[chunk.AppHash]
	chunkHash := blake2b.Sum256(chunk.Chunk)
	if !ok && chunk.Counter != 0 {
		return
	}
	if !ok {
		chunksLoading[chunk.AppHash] = []fileChunkPtrT{
			fileChunkPtrT{
				chunkHash,
				chunk.Counter,
				chunk.LastChunk,
			}}
	} else {
		lastChunk := previousChunks[len(previousChunks)-1]
		if lastChunk.counter != chunk.Counter-1 {
			return
		}
		chunksLoading[chunk.AppHash] = append(
			chunksLoading[chunk.AppHash],
			fileChunkPtrT{
				chunkHash: chunkHash,
				counter:   chunk.Counter,
				lastChunk: chunk.LastChunk,
			})
	}
	err := writeChunk(chunk.Chunk, chunkHash)
	if err != nil {
		delete(chunksLoading, chunk.AppHash)
		return
	}
	err = sendChunkReceipt(chunkHash, author)
	if err != nil {
		delete(chunksLoading, chunk.AppHash)
	}
}

func processGiveMeKey(
	keyRequest common.GiveMeASymmetricKey,
	author [32]byte) {

	m := makeSymmetricKeyT{
		publicSign,
		secretSign,
		keyRequest.MyPublicEncrypt,
		author,
		tcpOutChan,
	}
	pub, priv, err := box.GenerateKey(rand.Reader)
	if err != nil {
		return
	}
	symmetricKey, err := makeSymmetricKey()
	if err != nil {
		return
	}
	nonce, err := makeNonce()
	if err != nil {
		return
	}
	encryptedKey := box.Seal(
		make([]byte, 0),
		common.HashToSlice(symmetricKey),
		&nonce,
		&m.recipientPublicEncrypt,
		priv)
	hereIs := common.HereIsAnEncryptionKey{
		m.recipientPublicEncrypt,
		*pub,
		encryptedKeyToArr(encryptedKey),
		nonce,
		*new([common.SigSize]byte),
	}
	signature := sliceToSig(sign.Sign(
		make([]byte, 0),
		common.HashToSlice(hashHereIsKey(hereIs)),
		&m.mySecretSign))
	hereIs.Sig = signature
	m.tcpOutChan <- common.ClientToClient{
		hereIs,
		m.recipient,
		m.myPubSign,
	}
	symmetricKeysMux.Lock()
	symmetricKeys[m.recipient] = symmetricKey
	symmetricKeysMux.Unlock()
}

func processEncrypted(encrypted common.Encrypted, author [32]byte) {
	symmetricKeysMux.Lock()
	decryptionKey, ok := symmetricKeys[author]
	symmetricKeysMux.Unlock()
	if !ok {
		return
	}
	decryptionKeyAs32Byte := [32]byte(decryptionKey)
	decrypted, ok := secretbox.Open(
		make([]byte, 0),
		encrypted.Msg,
		&encrypted.Nonce,
		&decryptionKeyAs32Byte)
	if !ok {
		return
	}
	decoded, err := decodeMsg(decrypted)
	if err != nil {
		return
	}
	decoded.process(author)
}

func memberMapToList() []publicSignT {
	members := makeMemberList()
	memberList := make([]publicSignT, len(members))
	i := 0
	for k, _ := range members {
		memberList[i] = k
		i++
	}
	return memberList
}

func encodePubId(pubId publicSignT) []byte {
	return []byte(base64.URLEncoding.EncodeToString(
		common.HashToSlice(pubId)))
}

func processSearch(rawRequest []byte) ([]byte, error) {

	var searchQuery searchQueryT
	err := json.Unmarshal(rawRequest, &searchQuery)
	if err != nil {
		return *new([]byte), err
	}
	matchingApps, err := search(searchQuery)
	if err != nil {
		return *new([]byte), err
	}
	return json.Marshal(matchingApps)
}

func serveDoc(w http.ResponseWriter, filePath string) error {
	fmt.Println("Top of serveDoc.")
	fileHandle, err := os.Open(filePath)
	if err != nil {
		return err
	}
	_, err = io.Copy(w, fileHandle)
	return err
}

func findApp(appHash publicSignT) (appMsgT, error) {
	appsMux.Lock()
	defer appsMux.Unlock()
	for _, thisApp := range apps {
		if equalHashes(thisApp.AppHash, appHash) {
			return thisApp, nil
		}
	}
	return *new(appMsgT), errors.New("Could not find app.")
}

func appUrl(appCode string) string {
	return fmt.Sprintf(
		"http://localhost:%s/getapp/%s/index.html",
		port,
		appCode)
}

func unpackTarArchive(source string, dest string) error {
	fileHandle, err := os.Open(source)
	if err != nil {
		return err
	}

	err = os.Mkdir(dest, 0755)
	if err != nil {
		return err
	}

	tarReader := tar.NewReader(fileHandle)
	for {
		tarHeader, err := tarReader.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		sourcePath := dest + "/" + tarHeader.Name
		sourceHandle, err := os.Create(sourcePath)
		if err != nil {
			return err
		}
		_, err = io.Copy(sourceHandle, tarReader)
		if err != nil {
			return err
		}
	}
	return nil
}

func encKeyToSlice(key [common.EncryptedKeyLen]byte) []byte {
	result := make([]byte, common.EncryptedKeyLen)
	for i, b := range key {
		result[i] = b
	}
	return result
}

func processHereIsAnEncryptionKey(
	newKey common.HereIsAnEncryptionKey,
	author [32]byte) {

	awaitingSymmetricKeyMux.Lock()
	defer awaitingSymmetricKeyMux.Unlock()
	awaitingKey, ok := awaitingSymmetricKey[author]
	if !ok {
		return
	}
	keyHash := hashHereIsKey(newKey)
	signed, ok := sign.Open(
		make([]byte, 0),
		common.SigToSlice(newKey.Sig),
		&author)
	if !ok {
		return
	}
	if !bytes.Equal(signed, common.HashToSlice([32]byte(keyHash))) {
		return
	}
	keyPairsMux.Lock()
	secretKey, ok := keyPairs[newKey.YourPublicEncrypt]
	keyPairsMux.Unlock()
	secretKeyBytes := [32]byte(secretKey)
	if !ok {
		return
	}
	keySlice, ok := box.Open(
		make([]byte, 0),
		encKeyToSlice(newKey.EncryptedSymmetricKey),
		&newKey.Nonce,
		&newKey.MyPublicEncrypt,
		&secretKeyBytes)
	if !ok {
		return
	}
	symmetricKey := common.SliceToHash(keySlice)
	delete(awaitingSymmetricKey, author)
	symmetricKeysMux.Lock()
	symmetricKeys[author] = symmetricKey
	symmetricKeysMux.Unlock()
	chunksAwaitingReceiptMux.Lock()
	defer chunksAwaitingReceiptMux.Unlock()
	sendChunk(sendChunkT(awaitingKey))
}

type makeSymmetricKeyT struct {
	myPubSign              [32]byte
	mySecretSign           [64]byte
	recipientPublicEncrypt [32]byte
	recipient              [32]byte
	tcpOutChan             chan common.ClientToClient
}

func encryptedKeyToArr(slice []byte) [common.EncryptedKeyLen]byte {
	var result [common.EncryptedKeyLen]byte
	for i, b := range slice {
		result[i] = b
	}
	return result
}

func makeChunkFilePath(chunkHash [32]byte) string {
	filename := base64.URLEncoding.EncodeToString(
		common.HashToSlice(chunkHash))
	return dataDir + "/tmp/" + filename
}

func writeChunksToTmpFile(
	filePaths []string,
	tmpPath string,
	appHash [32]byte) error {

	tmpDestF, err := os.OpenFile(tmpPath, appendFlags, 0600)
	if err != nil {
		return err
	}
	hasher, err := blake2b.New256(nil)
	if err != nil {
		return err
	}
	for _, filePath := range filePaths {
		err := addChunkToFile(filePath, hasher, tmpDestF)
		if err != nil {
			return err
		}
	}
	finalHash := hasher.Sum(make([]byte, 0))
	if !bytes.Equal(
		finalHash,
		common.HashToSlice(appHash)) {

		return errors.New("Hash of assembled app is wrong.")
	}
	return nil
}

func addChunkToFile(
	filePath string,
	hasher io.Writer,
	destHandle io.Writer) error {

	chunk, err := ioutil.ReadFile(filePath)
	if err != nil {
		return err
	}
	nFile, err := destHandle.Write(chunk)
	if err != nil {
		return err
	}
	nHash, err := hasher.Write(chunk)
	if err != nil {
		return err
	}
	if nFile != len(chunk) {
		return errors.New("Could not write whole chunk to file.")
	}
	if nFile != nHash {
		return errors.New("Could not write whole chunk to hasher.")
	}
	return nil
}

func assembleApp(
	filePaths []string, appSender [32]byte, appMsg appMsgT) error {

	tmpPath := makeChunkFilePath(appMsg.AppHash)
	finalPath := makeAppPath(appMsg.AppHash)
	err := writeChunksToTmpFile(filePaths, tmpPath, appMsg.AppHash)
	if err != nil {
		return err
	}
	_ = os.Rename(tmpPath, finalPath)
	appsMux.Lock()
	defer appsMux.Unlock()
	apps = append(apps, appMsg)
	encodedApps, err := json.Marshal(apps)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(appsFile(), encodedApps, 0600)
	if err != nil {
		return err
	}
	return nil
}

func sendAppReceipt(appHash blake2bHash, recipient publicSignT) error {
	var receipt Decrypted
	receipt = AppReceiptT{
		sliceToSig(sign.Sign(
			make([]byte, 0),
			receiptHash(appHash, appReceiptCode),
			&secretSign)),
		appHash,
	}
	encoded, err := common.EncodeData(&receipt)
	if err != nil {
		return err
	}
	nonce, err := makeNonce()
	if err != nil {
		return err
	}
	symmetricKeysMux.Lock()
	symmetricKey, ok := symmetricKeys[recipient]
	symmetricKeysMux.Unlock()
	if !ok {
		return errors.New("no symmetric key")
	}
	keyArr := [32]byte(symmetricKey)
	encrypted := secretbox.Seal(
		make([]byte, 0),
		encoded,
		&nonce,
		&keyArr)
	tcpOutChan <- common.ClientToClient{
		Msg:       common.Encrypted{encrypted, nonce},
		Recipient: recipient,
		Author:    publicSign,
	}
	return nil
}

func writeChunk(chunk []byte, chunkHash blake2bHash) error {
	fileName := base64.URLEncoding.EncodeToString(
		common.HashToSlice(chunkHash))
	filePath := dataDir + "/tmp/" + fileName
	err := ioutil.WriteFile(filePath, chunk, 0600)
	return err
}

func sendChunkReceipt(
	chunkHash blake2bHash, recipient publicSignT) error {

	symmetricKeysMux.Lock()
	symmetricKey, ok := symmetricKeys[recipient]
	symmetricKeysMux.Unlock()
	if !ok {
		return errors.New("no symmetric key")
	}
	var receipt Decrypted
	secretSignAsBytes := [64]byte(secretSign)
	receipt = ReceiptT{
		sliceToSig(sign.Sign(
			make([]byte, 0),
			receiptHash(chunkHash, receiptCode),
			&secretSignAsBytes)),
		chunkHash}
	encoded, err := common.EncodeData(&receipt)
	if err != nil {
		return err
	}
	nonce, err := makeNonce()
	if err != nil {
		return err
	}
	keyAsBytes := [32]byte(symmetricKey)
	encrypted := secretbox.Seal(
		make([]byte, 0),
		encoded,
		&nonce,
		&keyAsBytes)
	tcpOutChan <- common.ClientToClient{
		Msg:       common.Encrypted{encrypted, nonce},
		Recipient: recipient,
		Author:    publicSign,
	}
	return nil
}

func hash(i interface{}) ([32]byte, error) {
	var buf bytes.Buffer
	enc := gob.NewEncoder(&buf)
	err := enc.Encode(&i)
	if err != nil {
		return *new([32]byte), err
	}
	return blake2b.Sum256(buf.Bytes()), nil
}

func httpGetApp(w http.ResponseWriter, r *http.Request) {
	securityCode := pat.Param(r, "securityCode")
	filename := pat.Param(r, "filename")
	if strEq(securityCode, homeCode) {
		err := serveDoc(w, "home/"+filename)
		if err != nil {
			http.Error(w, err.Error(), 500)
			return
		}
	}
	docHash, err := getDocHash(securityCode)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	filePath := fmt.Sprintf(
		"%s/tmp/%s/%s",
		dataDir,
		hashToStr(docHash),
		filename)
	err = serveDoc(w, filePath)
	if err != nil {
		http.Error(w, err.Error(), 500)
	}
}

func httpMakeApp(w http.ResponseWriter, r *http.Request) {
	securityCode := pat.Param(r, "securityCode")
	appHash := pat.Param(r, "apphash")
	if !strEq(securityCode, homeCode) {
		http.Error(w, "Bad security code", 400)
		return
	}
	hashSlice, err := base64.URLEncoding.DecodeString(appHash)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	hash := common.SliceToHash(hashSlice)
	hashStr := hashToStr(hash)
	tmpPath := dataDir + "/tmp/" + hashStr
	_, err = os.Stat(tmpPath)
	appPath := dataDir + "/apps/" + hashStr
	appCodesMux.Lock()
	defer appCodesMux.Unlock()
	if err == nil {
		appCode, err := getHashSecurityCode(hash)
		if err != nil {
			http.Error(w, err.Error(), 400)
			return
		}
		err = browser.OpenURL(appUrl(appCode))
		if err != nil {
			http.Error(w, err.Error(), 500)
		}
		return
	}
	err = unpackTarArchive(appPath, tmpPath)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	newCode, err := genCode()
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	err = browser.OpenURL(appUrl(newCode))
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	appCodes[newCode] = hash
}

func httpInvite(w http.ResponseWriter, r *http.Request) {
	securityCode := pat.Param(r, "securityCode")
	if !strEq(securityCode, homeCode) {
		http.Error(w, "Bad security code", 400)
		return
	}
	inviteeRaw := pat.Param(r, "invitee")
	inviteeBytes, err := base64.URLEncoding.DecodeString(inviteeRaw)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	invitee := common.SliceToHash(inviteeBytes)
	theTime := time.Now().Unix()
	invite := inviteT{
		PosixTime: theTime,
		Invitee:   invitee,
		Author:    publicSign,
		Signature: sliceToSig(sig(
			common.HashToSlice(inviteHash(theTime, invitee)),
			&secretSign)),
	}
	invitesMux.Lock()
	invites[invite] = struct{}{}
	err = saveInvites()
	invitesMux.Unlock()
	if err != nil {
		http.Error(w, err.Error(), 500)
	}
}

func saveInvites() error {
	encoded, err := json.Marshal(invitesToSlice())
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(invitesFile(), encoded, 0600)
	return err
}

func sig(msg []byte, privateKey *[64]byte) []byte {
	return sign.Sign(make([]byte, 0), msg, privateKey)
}

func httpSendApp(w http.ResponseWriter, r *http.Request) {
	if !strEq(pat.Param(r, "securityCode"), homeCode) {
		http.Error(w, "Bad security code", 400)
		return
	}
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	var sendAppJson sendAppJsonT
	err = json.Unmarshal(body, &sendAppJson)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	appHashSlice, err := base64.URLEncoding.DecodeString(
		sendAppJson.AppHash)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	appHash := common.SliceToHash(appHashSlice)
	recipientSlice, err := base64.URLEncoding.DecodeString(
		sendAppJson.Recipient)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	recipient := common.SliceToHash(recipientSlice)
	app, err := findApp(appHash)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	filepath := dataDir + "/apps/" + hashToStr(appHash)
	fileHandle, err := os.Open(filepath)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	symmetricKeysMux.Lock()
	_, ok := symmetricKeys[recipient]
	symmetricKeysMux.Unlock()
	chunk := sendChunkT{app, fileHandle, recipient, 0}
	if !ok {
		requestEncryptionKey(chunk)
	}
	chunksAwaitingReceiptMux.Lock()
	defer chunksAwaitingReceiptMux.Unlock()
	sendChunk(chunk)
}

func httpSaveApp(w http.ResponseWriter, r *http.Request) {
	if !strEq(pat.Param(r, "securityCode"), homeCode) {
		http.Error(w, "Bad security code", 400)
		return
	}
	hash, tags, err := writeAppToFile(r)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	appHash, err := hashFromString(hash)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	posixTime := time.Now().Unix()
	app := appMsgT{
		publicSign,
		tags,
		appHash,
		posixTime,
		sliceToSig(sig(
			common.HashToSlice(hashApp(tags, appHash, posixTime)),
			&secretSign)),
	}
	appsMux.Lock()
	defer appsMux.Unlock()
	apps = append(apps, app)
	encodedApps, err := json.Marshal(apps)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	err = ioutil.WriteFile(appsFile(), encodedApps, 0600)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	w.Write(common.HashToSlice(appHash))
}

func httpGetMyId(w http.ResponseWriter, r *http.Request) {
	if !strEq(pat.Param(r, "securityCode"), homeCode) {
		http.Error(w, "Bad security code", 400)
		return
	}
	w.Write(encodePubId(publicSign))
}

func httpGetMembers(w http.ResponseWriter, r *http.Request) {
	if !strEq(pat.Param(r, "securityCode"), homeCode) {
		http.Error(w, "Bad security code", 400)
		return
	}
	invitesMux.Lock()
	encoded, err := json.Marshal(memberMapToList())
	invitesMux.Unlock()
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	w.Write(encoded)
}

func httpSearchApps(w http.ResponseWriter, r *http.Request) {
	if !strEq(pat.Param(r, "securityCode"), homeCode) {
		http.Error(w, "Bad security code", 400)
		return
	}
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	encoded, err := processSearch(body)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	w.Write(encoded)
}

func httpServer() {
	mux := goji.NewMux()
	mux.HandleFunc(
		pat.Get("/getapp/:securityCode/:filename"), httpGetApp)
	mux.HandleFunc(
		pat.Get("/makeapproute/:securityCode/:apphash"), httpMakeApp)
	mux.HandleFunc(
		pat.Post("/invite/:securityCode/:invitee"), httpInvite)
	mux.HandleFunc(
		pat.Get("/getmyid/:securityCode"), httpGetMyId)
	mux.HandleFunc(
		pat.Get("/getmembers/:securityCode"), httpGetMembers)
	mux.HandleFunc(
		pat.Post("/sendapp/:securityCode"), httpSendApp)
	mux.HandleFunc(
		pat.Post("/saveapp/:securityCode"), httpSaveApp)
	mux.HandleFunc(
		pat.Post("/searchapps/:securityCode"), httpSearchApps)
	http.ListenAndServe(":"+port, mux)
}

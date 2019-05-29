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
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/pkg/browser"
	"goji.io"
	"goji.io/pat"
	"golang.org/x/crypto/argon2"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/nacl/sign"
	"golang.org/x/crypto/ssh/terminal"
	"io"
	"io/ioutil"
	"mime/multipart"
	"net"
	"net/http"
	"os"
	"sort"
	"strings"
	"syscall"
	"time"
)

type stateT struct {
	apps                  []appMsgT
	httpChan              chan httpInputT
	tcpInChan             chan common.ClientToClient
	tcpOutChan            chan common.ClientToClient
	homeCode              string
	appCodes              map[string]blake2bHash
	publicSign            publicSignT
	secretSign            [64]byte
	invites               map[inviteT]struct{}
	uninvites             map[inviteT]struct{}
	members               map[publicSignT]struct{}
	chunksLoading         map[blake2bHash][]fileChunkPtrT
	dataDir               string
	port                  string
	chunksAwaitingReceipt map[blake2bHash]chunkAwaitingReceiptT
	appsAwaitingReceipt   map[blake2bHash]publicSignT
	symmetricKeys         map[publicSignT]symmetricEncrypt
	keyPairs              map[publicEncryptT]secretEncryptT
	awaitingSymmetricKey  map[publicSignT]sendChunkT
}

type publicSignT [32]byte
type publicEncryptT [32]byte
type secretEncryptT [32]byte
type blake2bHash [32]byte
type symmetricEncrypt [32]byte
type secretSignT [64]byte

func makeMemberList(
	invites map[inviteT]struct{},
	uninvites map[inviteT]struct{}) map[publicSignT]struct{} {

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

func processInvites(rawInvites []byte, err error) (map[inviteT]struct{}, error) {
	invites := make(map[inviteT]struct{})
	if err != nil {
		return invites, nil
	}
	if err != nil {
		return invites, nil
	}
	var invitesSlice []inviteT
	err = json.Unmarshal(rawInvites, &invitesSlice)
	if err != nil {
		return invites, nil
	}
	for _, invite := range invitesSlice {
		invites[invite] = struct{}{}
	}
	return invites, err
}

func processApps(rawApps []byte, err error) ([]appMsgT, error) {
	var apps []appMsgT
	if err != nil {
		return apps, err
	}
	err = json.Unmarshal(rawApps, &apps)
	return apps, err
}

func appsFile(dataDir string) string {
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
	r *http.Request,
	dataDir string) (string, map[string]struct{}, error) {

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

var subRouteApps = map[string]struct{}{
	"getapp":       struct{}{},
	"makeapproute": struct{}{},
	"invite":       struct{}{},
}

func makeConn(
	publicSign publicSignT,
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

func tcpListen(
	conn net.Conn,
	inChan chan common.ClientToClient,
	connErrChan chan struct{}) {

	for {
		cToC, err := common.ReadClientToClient(conn)
		if err != nil {
			connErrChan <- struct{}{}
		}
		inChan <- cToC
	}
}

func tcpServer(
	inChan chan common.ClientToClient,
	outChan chan common.ClientToClient,
	secretSign [64]byte,
	publicSign [32]byte) {

	var conn net.Conn
	connErr := errors.New("Not connected yet.")
	connErrCh := make(chan struct{})
	for {
		conn, connErr = makeConn(publicSign, secretSign)
		if connErr != nil {
			time.Sleep(time.Second)
			continue
		}
		func() {
			go tcpListen(conn, inChan, connErrCh)
			go tcpSend(conn, outChan, connErrCh)
			<-connErrCh
		}()
	}
}

func tcpSend(
	conn net.Conn,
	outChan chan common.ClientToClient,
	connErrChan chan struct{}) {

	for {
		toSend := <-outChan
		encoded, err := common.EncodeClientToClient(toSend)
		if err != nil {
			connErrChan <- struct{}{}
		}
		n, err := conn.Write(encoded)
		if err != nil {
			connErrChan <- struct{}{}
		}
		if n != len(encoded) {
			connErrChan <- struct{}{}
		}
	}
}

type normalApiInputT struct {
	w            http.ResponseWriter
	securityCode string
	body         []byte
	route        string
	subRoute     string
	doneCh       chan struct{}
}

func strEq(s1, s2 string) bool {
	eq := subtle.ConstantTimeCompare([]byte(s1), []byte(s2))
	return eq == 1
}

type unpackAppT struct {
	appCodes map[string]blake2bHash
	w        http.ResponseWriter
	appHash  [32]byte
	doneCh   chan struct{}
	appPath  string
	tmpPath  string
	port     string
}

type newAppCodeT struct {
	w       http.ResponseWriter
	appHash [32]byte
	doneCh  chan struct{}
	newCode string
}

func getDocHash(
	securityCode string,
	appCodes map[string]blake2bHash) (blake2bHash, error) {

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
	AppHash string
	Recipient string
}

type logSendErrT struct {
	PosixTime int64
	AppHash   [32]byte
	Recipient [32]byte
	Err       error
}

func logSendErr(
	err error,
	appHash [32]byte,
	recipient [32]byte,
	dataDir string) {

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
		dataDir + "/sendErrors.txt",
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
	recipient publicSignT,
	dataDir string) {

	msg := logSentSuccessT{appHash, recipient}
	encoded, jsonErr := json.Marshal(msg)
	if jsonErr != nil {
		logSendErr(jsonErr, appHash, recipient, dataDir)
		return
	}
	f, openErr := os.OpenFile(
		dataDir + "/sentMsgPath", appendFlags, 0600)
	if openErr != nil {
		logSendErr(openErr, appHash, recipient, dataDir)
		return
	}
	defer f.Close()
	_, writeErr := f.Write(append([]byte("\n"), encoded...))
	if writeErr != nil {
		logSendErr(writeErr, appHash, recipient, dataDir)
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

type sendAppMsgT struct {
	myPublicSign publicSignT
	symmetricKey symmetricEncrypt
	outChan chan common.ClientToClient
	msg appMsgT
	recipient publicSignT
}

func cleanChunksLoading(
	oldChunksLoading map[blake2bHash][]fileChunkPtrT,
	appHash blake2bHash) map[blake2bHash][]fileChunkPtrT {

	newChunksLoading := make(map[blake2bHash][]fileChunkPtrT)
	for appHash, chunkPtr := range oldChunksLoading {
		newChunksLoading[appHash] = chunkPtr
	}
	delete(newChunksLoading, appHash)
	return newChunksLoading
}

func makeChunkFilePaths(ptrs []fileChunkPtrT, dataDir string) []string {
	filePaths := make([]string, len(ptrs))
	for i, ptr := range ptrs {
		filePaths[i] = makeChunkFilePath(ptr.chunkHash, dataDir)
	}
	return filePaths
}

func makeAppPath(dataDir string, appHash blake2bHash) string {
	fileName := base64.URLEncoding.EncodeToString(
		common.HashToSlice(appHash))
	return dataDir + "/apps/" + fileName
}

func (appSig appMsgT) process(author publicSignT, s stateT) stateT {
	newS := s
	newS.chunksLoading = cleanChunksLoading(
		s.chunksLoading, appSig.AppHash)
	signedHash, ok := sign.Open(
		make([]byte, 0),
		common.SigToSlice(appSig.Sig),
		&appSig.Author)
	appHash := common.HashToSlice(hashApp(
		appSig.Tags, appSig.AppHash, appSig.PosixTime))
	if !(ok && bytes.Equal(appHash, signedHash)) {
		return newS
	}
	chunkPtrs, ok := s.chunksLoading[appSig.AppHash]
	if !ok {
		return newS
	}
	finalChunkPtr := chunkPtrs[len(chunkPtrs)-1]
	if !finalChunkPtr.lastChunk {
		return newS
	}
	symmetricKey, ok := s.symmetricKeys[author]
	if !ok {
		return newS
	}
	return assembleAppNew(assembleApp{
		symmetricKey: symmetricKey,
		filePaths:    makeChunkFilePaths(chunkPtrs, s.dataDir),
		appHash:      appSig.AppHash,
		tmpPath:      makeChunkFilePath(appSig.AppHash, s.dataDir),
		finalPath:    makeAppPath(s.dataDir, appSig.AppHash),
		appSender:    author,
		appMsg: appSig,
	}, newS)
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

func requestEncryptionKey(sendChunk sendChunkT, s stateT) stateT {
	pub, priv, err := box.GenerateKey(rand.Reader)
	if err != nil {
		return s
	}
	sendChunk.tcpOutChan <- common.ClientToClient{
		Msg:       common.GiveMeASymmetricKey{*pub},
		Recipient: sendChunk.recipient,
		Author:    sendChunk.myPublicSign,
	}
	return addChunkToState(addKeysToState(s, *pub, *priv), sendChunk)
}

func addChunkToState(s stateT, sendChunk sendChunkT) stateT {
	newAwaitingSK := copyAwaitingKeys(s.awaitingSymmetricKey)
	newAwaitingSK[sendChunk.recipient] = sendChunk
	newS := s
	newS.awaitingSymmetricKey = newAwaitingSK
	return newS
}

func addKeysToState(s stateT, pub [32]byte, priv [32]byte) stateT {
	newKeyPairs := copyKeyPairs(s.keyPairs)
	newKeyPairs[publicEncryptT(pub)] = secretEncryptT(priv)
	newS := s
	newS.keyPairs = newKeyPairs
	return newS
}

type awaitingKeysT map[publicSignT]sendChunkT

func copyAwaitingKeys(old awaitingKeysT) awaitingKeysT {
	newAKs := make(awaitingKeysT)
	for k, v := range old {
		newAKs[k] = v
	}
	return newAKs
}

type keyPairsT map[publicEncryptT]secretEncryptT

func copyKeyPairs(old keyPairsT) keyPairsT {
	newKPs := make(keyPairsT)
	for k, v := range old {
		newKPs[k] = v
	}
	return newKPs
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
	appHash blake2bHash
}

func sendChunk(state stateT, s sendChunkT) stateT {
	fileHandle, err := os.Open(s.filepath)
	errOut := func(err error) stateT {
		fmt.Println("Error in sendChunkT sender func:")
		fmt.Println(err)
		logSendErr(err, s.appMsg.AppHash, s.recipient, s.dataDir)
		return state
	}
	if err != nil {
		return errOut(err)
	}
	newOffset, err := fileHandle.Seek(s.offset, 0)
	if newOffset != s.offset {
		return errOut(errors.New("New offset is wrong."))
	}
	if err != nil {
		return errOut(err)
	}
	chunkBuffer := make([]byte, common.ChunkContentSize)
	numBytesRead, err := fileHandle.Read(chunkBuffer)
	if err != nil {
		return errOut(err)
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
		return errOut(err)
	}
	nonce, err := makeNonce()
	if err != nil {
		return errOut(err)
	}
	s.tcpOutChan <- common.ClientToClient{
		Msg: common.Encrypted{
			Msg: secretbox.Seal(
				make([]byte, 0),
				encodedMsg,
				&nonce,
				&s.symmetricEncryptKey),
			Nonce: nonce,
		},
		Recipient: s.recipient,
		Author:    s.myPublicSign,
	}
	c := chunkAwaitingReceiptT{
		s.appMsg,
		s.filepath,
		s.offset,
		s.recipient,
		s.symmetricEncryptKey,
		blake2bHash(blake2b.Sum256(chunk)),
		s.counter,
		lastChunk,
	}
	chunksAwaiting := make(map[blake2bHash]chunkAwaitingReceiptT)
	for k, v := range state.chunksAwaitingReceipt {
		chunksAwaiting[k] = v
	}
	chunksAwaiting[c.chunkHash] = c
	newState := state
	newState.chunksAwaitingReceipt = chunksAwaiting
	return newState
}

type Decrypted interface {
	process(publicSignT, stateT) stateT
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

func filterApps(all []appMsgT, q searchQueryT) []appMsgT {
	var filtered []appMsgT
	for _, app := range all {
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

func search(apps []appMsgT, q searchQueryT) (searchResultsT, error) {
	filtered := filterApps(apps, q)
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

type serveDocT struct {
	w        http.ResponseWriter
	doneCh   chan struct{}
	filePath string
}

type makeInviteT struct {
	invitee   [32]byte
	posixTime int64
	w         http.ResponseWriter
	doneCh    chan struct{}
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

func copyInvites(
	invites map[inviteT]struct{}) map[inviteT]struct{} {

	newInvites := make(map[inviteT]struct{})
	for invite, _ := range invites {
		newInvites[invite] = struct{}{}
	}
	return newInvites
}

func invitesToSlice(invites map[inviteT]struct{}) []inviteT {
	invitesSlice := make([]inviteT, len(invites))
	i := 0
	for invite, _ := range invites {
		invitesSlice[i] = invite
		i++
	}
	return invitesSlice
}

type writeUpdatedInvitesT struct {
	filepath string
	toWrite  []byte
	w        http.ResponseWriter
	doneCh   chan struct{}
}

func getHashSecurityCode(appCodes map[string]blake2bHash, hash blake2bHash) (string, error) {
	for c, h := range appCodes {
		if equalHashes(h, hash) {
			return c, nil
		}
	}
	return "", errors.New("Could not find app.")
}

type httpInputT struct {
	w      http.ResponseWriter
	r      *http.Request
	route  string
	doneCh chan struct{}
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

func (appReceipt AppReceiptT) process(
	author publicSignT,
	s stateT) stateT {

	signed, ok := sign.Open(
		make([]byte, 0),
		common.SigToSlice(appReceipt.Sig),
		authBytes(author))
	if !ok {
		return s
	}
	if !bytes.Equal(signed, common.HashToSlice(appReceipt.AppHash)) {
		return s
	}
	appAuthor, ok := s.appsAwaitingReceipt[appReceipt.AppHash]
	if !ok {
		return s
	}
	if appAuthor != author {
		return s
	}
	newAppsAwaiting := make(map[blake2bHash]publicSignT)
	for k, v := range s.appsAwaitingReceipt {
		newAppsAwaiting[k] = v
	}
	delete(newAppsAwaiting, appReceipt.AppHash)
	newS := s
	newS.appsAwaitingReceipt = newAppsAwaiting
	logSentSuccess(appReceipt.AppHash, author, s.dataDir)
	return newS
}

func (receipt ReceiptT) process(author publicSignT, s stateT) stateT {
	fmt.Println("The receipt is:")
	fmt.Println(receipt)
	chunkAwaiting, ok := s.chunksAwaitingReceipt[receipt.ChunkHash]
	fmt.Println(len(s.chunksAwaitingReceipt))
	if !ok {
		fmt.Println("Received unexpected receipt.")
		return s
	}
	if chunkAwaiting.lastChunk {
		newAppsAwaiting := make(map[blake2bHash]publicSignT)
		for k, v := range s.appsAwaitingReceipt {
			newAppsAwaiting[k] = v
		}
		newS := s
		newS.appsAwaitingReceipt = newAppsAwaiting
		symmetricKey, ok := s.symmetricKeys[author]
		if !ok {
			fmt.Println("Could not find symmetric key for:")
			fmt.Println(author)
			return s
		}
		return sendAppMsg(sendAppMsgT{
			s.publicSign,
			symmetricKey,
			s.tcpOutChan,
			chunkAwaiting.appMsg,
			author,
		}, newS)
	}
	return sendChunk(s, sendChunkT{
		s.publicSign,
		s.dataDir,
		chunkAwaiting.appMsg,
		s.tcpOutChan,
		chunkAwaiting.filepath,
		chunkAwaiting.offset + common.ChunkContentSize,
		chunkAwaiting.recipient,
		chunkAwaiting.symmetricEncryptKey,
		chunkAwaiting.counter + 1,
	})
}

func sendAppMsg(s sendAppMsgT, state stateT) stateT {
	var msg Decrypted
	msg = s.msg
	encoded, err := common.EncodeData(&msg)
	if err != nil {
		fmt.Println(err)
		return state
	}
	nonce, err := makeNonce()
	if err != nil {
		fmt.Println(err)
		return state
	}
	keyBytes := [32]byte(s.symmetricKey)
	encrypted := secretbox.Seal(
		make([]byte, 0),
		encoded,
		&nonce,
		&keyBytes)
	s.outChan <- common.ClientToClient{
		Msg: common.Encrypted{encrypted, nonce},
		Recipient: s.recipient,
		Author: s.myPublicSign,
	}
	return state
}

const (
	pwlen = 5
	appendFlags = os.O_APPEND | os.O_CREATE | os.O_WRONLY
)

func makePassword() ([]byte, error) {
	pw := make([]byte, pwlen)
	n, err := rand.Read(pw)
	if err != nil {
		return make([]byte, 0), err
	}
	if n != pwlen {
		return make([]byte, 0), errors.New("Wrong number of bytes.")
	}
	return pw, err
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
	appMsg              appMsgT
	filepath            string
	offset              int64
	recipient           [32]byte
	symmetricEncryptKey [32]byte
	chunkHash blake2bHash
	counter             int
	lastChunk bool
}

type appAwaitingReceiptT struct {
	appHash   [32]byte
	recipient [32]byte
	lastChunkAwaiting chunkAwaitingReceiptT
}

type sendChunkT struct {
	myPublicSign        [32]byte
	dataDir             string
	appMsg              appMsgT
	tcpOutChan          chan common.ClientToClient
	filepath            string
	offset              int64
	recipient           [32]byte
	symmetricEncryptKey [32]byte
	counter             int
}

func signedAuthToSlice(bs []byte) [common.AuthSigSize]byte {
	var result [common.AuthSigSize]byte
	for i, b := range bs {
		result[i] = b
	}
	return result
}

type secretsFileT struct {
	Publicsign    [32]byte
	Publicencrypt [32]byte
	Nonce         [24]byte
	Salt          [32]byte
	Secretkeys    []byte
}

type secretKeysT struct {
	Secretsign    [64]byte
	Secretencrypt [32]byte
}

type keysT struct {
	publicsign    [32]byte
	publicencrypt [32]byte
	secretsign    [64]byte
	secretencrypt [32]byte
}

func slowHash(pw []byte, salt [32]byte) [32]byte {
	return common.SliceToHash(argon2.IDKey(
		pw,
		common.HashToSlice(salt),
		300,
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
	decryptedKeys, ok := secretbox.Open(
		make([]byte, 0),
		decoded.Secretkeys,
		&decoded.Nonce,
		&symmetricKey)
	if !ok {
		return keys, errors.New("Could not decrypt keys.")
	}
	var decodedKeys secretKeysT
	err = json.Unmarshal(decryptedKeys, &decodedKeys)
	if err != nil {
		return keys, err
	}
	return keysT{
		decoded.Publicsign,
		decoded.Publicencrypt,
		decodedKeys.Secretsign,
		decodedKeys.Secretencrypt}, nil
}

func createKeys(dataDir string) error {
	pubEnc, secretEnc, err := box.GenerateKey(rand.Reader)
	if err != nil {
		return err
	}
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
	secretKeys := secretKeysT{*secretSign, *secretEnc}
	encodedSecrets, err := json.Marshal(secretKeys)
	if err != nil {
		return err
	}
	password, err := makePassword()
	if err != nil {
		return err
	}
	fmt.Println(hex.EncodeToString(password))
	secretKey := slowHash(password, salt)
	encryptedSecrets := secretbox.Seal(
		make([]byte, 0),
		encodedSecrets,
		&nonce,
		&secretKey)
	secretsFile := secretsFileT{
		Publicsign:    *pubSign,
		Publicencrypt: *pubEnc,
		Nonce:         nonce,
		Salt:          salt,
		Secretkeys:    encryptedSecrets,
	}
	encodedFile, err := json.Marshal(secretsFile)
	if err != nil {
		fmt.Println("Could not encode.")
		return err
	}
	err = ioutil.WriteFile(keysFile(dataDir), encodedFile, 0600)
	return err
}

func invitesFile(dataDir string) string {
	return dataDir + "/invites.txt"
}

func uninvitesFile(dataDir string) string {
	return dataDir + "/uninvites.txt"
}

func keysFile(dataDir string) string {
	return dataDir + "/TOP_SECRET_DONT_SHARE.txt"
}

func initState(dataDir string, port string) (stateT, error) {
	homeCode, err := genCode()
	var s stateT
	if err != nil {
		return s, err
	}
	rawSecrets, err := ioutil.ReadFile(keysFile(dataDir))
	if err != nil {
		err := createKeys(dataDir)
		if err != nil {
			return s, err
		}
		rawSecrets, err = ioutil.ReadFile(keysFile(dataDir))
		if err != nil {
			return s, err
		}
	}
	fmt.Println("Please enter your password:")
	passwordtxt, err := terminal.ReadPassword(int(syscall.Stdin))
	password := make([]byte, hex.DecodedLen(len(passwordtxt)))
	_, err = hex.Decode(password, passwordtxt)
	if err != nil {
		return s, err
	}
	keys, err := extractKeys(password, rawSecrets)
	if err != nil {
		return s, err
	}
	invites, err := processInvites(ioutil.ReadFile(invitesFile(dataDir)))
	if err != nil {
		return s, err
	}
	uninvites, err := processInvites(ioutil.ReadFile(uninvitesFile(dataDir)))
	if err != nil {
		return s, err
	}
	apps, err := processApps(ioutil.ReadFile(appsFile(dataDir)))
	if err != nil {
		return s, err
	}
	memberList := makeMemberList(invites, uninvites)
	return stateT{
		apps:           apps,
		httpChan:       make(chan httpInputT),
		tcpInChan:      make(chan common.ClientToClient),
		tcpOutChan:     make(chan common.ClientToClient),
		homeCode:       homeCode,
		appCodes:       make(map[string]blake2bHash),
		publicSign:     keys.publicsign,
		secretSign:     keys.secretsign,
		invites:        invites,
		uninvites:      uninvites,
		members:        memberList,
		chunksLoading:  make(map[blake2bHash][]fileChunkPtrT),
		dataDir:        dataDir,
		port:           port,
	}, nil
}

func main() {
	gob.Register(*new(common.ClientToClient))
	gob.Register(*new(common.Encrypted))
    gob.Register(*new(common.GiveMeASymmetricKey))
	gob.Register(*new(common.HereIsAnEncryptionKey))
	gob.Register(*new(AppReceiptT))
	gob.Register(*new(ReceiptT))
	gob.Register(*new(FileChunk))
	gob.Register(*new(appMsgT))
	args := os.Args
	if len(args) != 3 {
		fmt.Println("There must be two command-line arguments.")
		return
	}
	port := args[1]
	dataDir := args[2]
	err := os.RemoveAll(dataDir + "/tmp")
	if err != nil {
		fmt.Println(err)
		return
	}
	err = os.Mkdir(dataDir+"/tmp", 0755)
	if err != nil {
		fmt.Println(err)
		return
	}
	state, err := initState(dataDir, port)
	if err != nil {
		fmt.Println(err)
		return
	}
	go tcpServer(
		state.tcpInChan,
		state.tcpOutChan,
		state.secretSign,
		state.publicSign)
	go httpServer(state.httpChan, state.homeCode, port)
	fmt.Print(state.homeCode)
	err = browser.OpenURL(fmt.Sprintf(
		"http://localhost:%s/getapp/%s/index.html",
		port,
		state.homeCode))
	if err != nil {
		fmt.Println(err)
		return
	}
	for {
		select {
		case h := <-state.httpChan:
			state = processHttpInput(state, h)
		case tcpIn := <-state.tcpInChan:
			state = processTcpInput(state, tcpIn)
		}
	}
}

func processTcpInput(s stateT, c common.ClientToClient) stateT {
	_, isMember := makeMemberList(s.invites, s.uninvites)[c.Author]
	if !isMember {
		return s
	}
	switch c.Msg.(type) {
	case common.Encrypted:
		return processEncrypted(
			(c.Msg).(common.Encrypted),
			c.Author,
			s)
	case common.GiveMeASymmetricKey:
		return processGiveMeKey(
			(c.Msg).(common.GiveMeASymmetricKey),
			c.Author,
			s)
	case common.HereIsAnEncryptionKey:
		return processHereIsAnEncryptionKey(
			(c.Msg).(common.HereIsAnEncryptionKey),
			c.Author,
			s)
	}
	return s
}

func (chunk FileChunk) process(author publicSignT, s stateT) stateT {
	fmt.Println("chunk.Counter:")
	fmt.Println(chunk.Counter)
	previousChunks, ok := s.chunksLoading[chunk.AppHash]
	fmt.Println("previousChunks:")
	fmt.Println(previousChunks)
	newChunksLoading := make(map[blake2bHash][]fileChunkPtrT)
	for appHash, chunkPtr := range s.chunksLoading {
		newChunksLoading[appHash] = chunkPtr
	}
	chunkHash, err := hash(chunk)
	if err != nil {
		fmt.Println("Bad chunk hash in process FileChunk func:")
		fmt.Println(err)
		return s
	}
	if !ok && chunk.Counter != 0 {
		fmt.Println("ok:")
		fmt.Println(ok)
		fmt.Println("chunk.Counter:")
		fmt.Println(chunk.Counter)
		fmt.Println("!ok && chunk.Counter")
		return s
	}
	if !ok {
		fmt.Println("!ok")
		newChunksLoading[chunk.AppHash] = []fileChunkPtrT{
			fileChunkPtrT{
				chunkHash: chunkHash,
				counter:   chunk.Counter,
				lastChunk: chunk.LastChunk,
				appHash: chunk.AppHash,
			}}
	} else {
		lastChunk := previousChunks[len(previousChunks)-1]
		if lastChunk.counter != chunk.Counter-1 {
			fmt.Println("lastChunk.counter:")
			fmt.Println(lastChunk.counter)
			fmt.Println("chunk.Counter:")
			fmt.Println(chunk.Counter)
			fmt.Println("lastChunk.counter != chunk.Counter-1")
			return s
		}
		newChunksLoading[chunk.AppHash] = append(
			newChunksLoading[chunk.AppHash],
			fileChunkPtrT{
				chunkHash: chunkHash,
				counter:   chunk.Counter,
				lastChunk: chunk.LastChunk,
			})
		fmt.Println("newChunksLoading:")
		fmt.Println(newChunksLoading)
	}
	newS := s
	newS.chunksLoading = newChunksLoading
	tmpFileName := base64.URLEncoding.EncodeToString(
		common.HashToSlice(chunkHash))
	symmetricKey, ok := s.symmetricKeys[author]
	if !ok {
		fmt.Println("Could not find symmetric key for author:")
		fmt.Println(author)
		return s
	}
	return writeAppToFileNew(writeAppToFileAndSendReceiptT{
		chunk.AppHash,
		s.dataDir + "/tmp/" + tmpFileName,
		chunk.Chunk,
		s.secretSign,
		symmetricKey,
		s.tcpOutChan,
		author,
		s.publicSign,
		}, newS)
}

func processGiveMeKey(keyRequest common.GiveMeASymmetricKey, author [32]byte, s stateT) stateT {

	m := makeSymmetricKeyT{
		s.publicSign,
		s.secretSign,
		keyRequest.MyPublicEncrypt,
		author,
		s.tcpOutChan,
	}

	pub, priv, err := box.GenerateKey(rand.Reader)
	if err != nil {
		fmt.Println("Error in generating keys:")
		fmt.Println(err)
		return s
	}
	symmetricKey, err := makeSymmetricKey()
	if err != nil {
		fmt.Println("Error in generating symmetric key:")
		fmt.Println(err)
		return s
	}
	nonce, err := makeNonce()
	if err != nil {
		fmt.Println("Error in making nonce for sending symmetricKey:")
		fmt.Println(err)
		return s
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

	newSymmetricKeys := make(map[publicSignT]symmetricEncrypt)
	for k, v := range s.symmetricKeys {
		newSymmetricKeys[k] = v
	}
	newSymmetricKeys[m.recipient] = symmetricKey
	newS := s
	newS.symmetricKeys = newSymmetricKeys
	return newS
}

func processEncrypted(encrypted common.Encrypted, author [32]byte, s stateT) stateT {
	decryptionKey, ok := s.symmetricKeys[author]
	if !ok {
		fmt.Println("Top of !ok.")
		fmt.Println(author)
		fmt.Println(s.symmetricKeys)
		fmt.Println("Could not find decryption key.")
		return s
	}
	decryptionKeyAs32Byte := [32]byte(decryptionKey)
	decrypted, ok := secretbox.Open(
		make([]byte, 0),
		encrypted.Msg,
		&encrypted.Nonce,
		&decryptionKeyAs32Byte)
	if !ok {
		fmt.Println("Could not decrypt message.")
		return s
	}
	decoded, err := decodeMsg(decrypted)
	if err != nil {
		fmt.Println(err)
		return s
	}
	return decoded.process(author, s)
}

func processHttpInput(s stateT, h httpInputT) stateT {
	securityCode := pat.Param(h.r, "securityCode")
	subRoute := ""
	if _, ok := subRouteApps[h.route]; ok {
		subRoute = pat.Param(h.r, "subRoute")
	}
	if h.route == "saveapp" {
		hash, tags, err := writeAppToFile(h.r, s.dataDir)
		if err != nil {
			fmt.Println(err)
			http.Error(h.w, err.Error(), 500)
			h.doneCh <- struct{}{}
			return s
		}
		return processNewApp(s, h, hash, tags)
	}
	body, err := ioutil.ReadAll(h.r.Body)
	if err != nil {
		http.Error(h.w, err.Error(), http.StatusInternalServerError)
		h.doneCh <- struct{}{}
		return s
	}
	return processNormalApiInput(
		normalApiInputT{
			h.w,
			securityCode,
			body,
			h.route,
			subRoute,
			h.doneCh,
		},
		s)
}

func processNormalApiInput(n normalApiInputT, s stateT) stateT {
	homeGuard := func(
		processor func(normalApiInputT, stateT) stateT) stateT {

		if !strEq(n.securityCode, s.homeCode) {
			http.Error(n.w, "Bad security code", 400)
			n.doneCh <- struct{}{}
			return s
		}
		return processor(n, s)
	}

	switch n.route {
	case "makeapproute":
		return processMakeAppRoute(n, s)
	case "getapp":
		return processGetApp(n, s)
	case "sendapp":
		return processSendApp(n, s)
	case "searchapps":
		return processSearchApps(n, s)
	case "invite":
		return processInvite(n, s)
	case "getmyid":
		return homeGuard(processGetMyId)
	case "getmembers":
		return homeGuard(processGetMembers)
	}
	return s
}

func memberMapToList(
	members map[publicSignT]struct{}) []publicSignT {

	memberList := make([]publicSignT, len(members))
	i := 0
	for k, _ := range members {
		memberList[i] = k
		i++
	}
	return memberList
}

func processGetMembers(n normalApiInputT, s stateT) stateT {
	encoded, err := json.Marshal(memberMapToList(s.members))
	if err != nil {
		http.Error(n.w, "Error encoding member list", 400)
		n.doneCh <- struct{}{}
		return s
	}
	n.w.Write(encoded)
	n.doneCh <- struct{}{}
	return s
}

func encodePubId(pubId publicSignT) []byte {
	return []byte(base64.URLEncoding.EncodeToString(
		common.HashToSlice(pubId)))
}

func processGetMyId(n normalApiInputT, s stateT) stateT {
	n.w.Write(encodePubId(s.publicSign))
	n.doneCh <- struct{}{}
	return s
}

func processInvite(n normalApiInputT, s stateT) stateT {
	sendErr := func(err error, code int) stateT {
		http.Error(n.w, err.Error(), code)
		n.doneCh <- struct{}{}
		return s
	}
	if !strEq(n.securityCode, s.homeCode) {
		err := errors.New("Bad security code")
		return sendErr(err, 400)
	}
	invitee, err := base64.URLEncoding.DecodeString(n.subRoute)
	if err != nil {
		return sendErr(err, 400)
	}
	m := makeInviteT{common.SliceToHash(invitee), time.Now().Unix(), n.w, n.doneCh}

	invite := inviteT{
		PosixTime: m.posixTime,
		Invitee:   m.invitee,
		Author:    s.publicSign,
		Signature: sliceToSig(sign.Sign(
			make([]byte, 0),
			common.HashToSlice(inviteHash(
				m.posixTime, m.invitee)),
			&s.secretSign)),
	}
	newInvites := copyInvites(s.invites)
	newInvites[invite] = struct{}{}
	encodedInvites, err := json.Marshal(invitesToSlice(newInvites))
	if err != nil {
		return sendErr(err, 500)
	}

	w := writeUpdatedInvitesT{
		invitesFile(s.dataDir),
		encodedInvites,
		m.w,
		m.doneCh,
	}

	err = ioutil.WriteFile(w.filepath, w.toWrite, 0600)
	if err != nil {
		http.Error(w.w, err.Error(), 500)
	}
	w.doneCh <- struct{}{}
	return s
}

func processSearchApps(n normalApiInputT, s stateT) stateT {
	sendErr := func(msg string) stateT {
		http.Error(n.w, msg, 400)
		n.doneCh <- struct{}{}
		return s
	}
	if !strEq(n.securityCode, s.homeCode) {
		return sendErr("Bad security code")
	}
	var searchQuery searchQueryT
	err := json.Unmarshal(n.body, &searchQuery)
	if err != nil {
		return sendErr("Could not decode Json.")
	}
	matchingApps, err := search(s.apps, searchQuery)
	if err != nil {
		return sendErr(err.Error())
	}
	encoded, err := json.Marshal(matchingApps)
	if err != nil {
		return sendErr("Couldn't encode search results.")
	}
	n.w.Write(encoded)
	n.doneCh <- struct{}{}
	return s
}

func processGetApp(n normalApiInputT, s stateT) stateT {
	if strEq(n.securityCode, s.homeCode) {
		serveDocNew(serveDocT{
			n.w,
			n.doneCh,
			"home/" + n.subRoute,
		})
		return s
	}
	docHash, err := getDocHash(n.securityCode, s.appCodes)
	if err != nil {
		http.Error(n.w, "Bad security code", 400)
		n.doneCh <- struct{}{}
		return s
	}
	serveDocNew(serveDocT{
		n.w,
		n.doneCh,
		s.dataDir + "/tmp/" + hashToStr(docHash) + "/" + n.subRoute,
	})
	return s
}

func serveDocNew(s serveDocT) {
	fileHandle, err := os.Open(s.filePath)
	if err != nil {
		http.Error(s.w, err.Error(), 500)
		s.doneCh <- struct{}{}
	}
	_, err = io.Copy(s.w, fileHandle)
	if err != nil {
		http.Error(s.w, err.Error(), 500)
		s.doneCh <- struct{}{}
	}
	s.doneCh <- struct{}{}
}

func processSendApp(n normalApiInputT, s stateT) stateT {
	sendErr := func(msg string) stateT {
		http.Error(n.w, msg, 400)
		n.doneCh <- struct{}{}
		return s
	}
	if !strEq(n.securityCode, s.homeCode) {
		return sendErr("Bad security code")
	}
	var sendAppJson sendAppJsonT
	err := json.Unmarshal(n.body, &sendAppJson)
	if err != nil {
		return sendErr(err.Error())
	}
	appHashSlice, err := base64.URLEncoding.DecodeString(
		sendAppJson.AppHash)
	if err != nil {
		return sendErr(err.Error())
	}
	appHash := common.SliceToHash(appHashSlice)
	recipientSlice, err := base64.URLEncoding.DecodeString(
		sendAppJson.Recipient)
	if err != nil {
		return sendErr(err.Error())
	}
	recipient := common.SliceToHash(recipientSlice)
	var app appMsgT
	for _, thisApp := range s.apps {
		if equalHashes(thisApp.AppHash, appHash) {
			app = thisApp
			break
		}
	}
	filepath := s.dataDir + "/apps/" + hashToStr(appHash)
	symmetricEncryptKey, ok := s.symmetricKeys[recipient]
	chunk := sendChunkT{
		s.publicSign,
		s.dataDir,
		app,
		s.tcpOutChan,
		filepath,
		0,
		recipient,
		symmetricEncryptKey,
		0,
	}
	if !ok {
		return requestEncryptionKey(chunk, s)
	}
	return sendChunk(s, chunk)
}

func processMakeAppRoute(n normalApiInputT, s stateT) stateT {
	if !strEq(n.securityCode, s.homeCode) {
		http.Error(n.w, "Bad security code", 400)
		n.doneCh <- struct{}{}
		return s
	}
	hashSlice, err := base64.URLEncoding.DecodeString(n.subRoute)
	if err != nil {
		http.Error(n.w, err.Error(), 400)
		n.doneCh <- struct{}{}
		return s
	}
	hash := common.SliceToHash(hashSlice)
	hashStr := hashToStr(hash)
	return unpackAppNew(
		unpackAppT{
			s.appCodes,
			n.w,
			hash,
			n.doneCh,
			s.dataDir + "/apps/" + hashStr,
			s.dataDir + "/tmp/" + hashStr,
			s.port,
		}, s)
}

func unpackAppNew(g unpackAppT, s stateT) stateT {
	sendErr := func(err error) {
		fmt.Println(err)
		http.Error(g.w, err.Error(), 500)
		g.doneCh <- struct{}{}
	}
	_, err := os.Stat(g.tmpPath)
	if err == nil {
		appCode, err := getHashSecurityCode(g.appCodes, g.appHash)
		if err != nil {
			sendErr(err)
			return s
		}
		err = browser.OpenURL(
			"http://localhost:" + g.port + "/getapp/" + appCode + "/index.html")
		if err != nil {
			sendErr(err)
			return s
		}
		g.doneCh <- struct{}{}
		return s
	}
	fileHandle, err := os.Open(g.appPath)
	if err != nil {
		fmt.Println("no file handle")
		sendErr(err)
		return s
	}
	err = os.Mkdir(g.tmpPath, 0755)
	if err != nil {
		fmt.Println("mkdir failed")
		sendErr(err)
		return s
	}
	tr := tar.NewReader(fileHandle)
	for {
		hdr, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			fmt.Println("not a tar archive")
			sendErr(err)
			return s
		}
		sourcePath := g.tmpPath + "/" + hdr.Name
		sourceHandle, err := os.Create(sourcePath)
		if err != nil {
			sendErr(err)
			return s
		}
		_, err = io.Copy(sourceHandle, tr)
		if err != nil {
			sendErr(err)
			return s
		}
	}
	newCode, err := genCode()
	if err != nil {
		sendErr(err)
		return s
	}
	err = browser.OpenURL("http://localhost:" + g.port + "/getapp/" + newCode + "/index.html")
	if err != nil {
		sendErr(err)
		return s
	}
	return newAppCodeNew(
		newAppCodeT{
			g.w,
			g.appHash,
			g.doneCh,
			newCode,
		}, s)
}

func newAppCodeNew(n newAppCodeT, s stateT) stateT {
	newState := s
	newAppCodes := make(map[string]blake2bHash)
	for code, hash := range s.appCodes {
		newAppCodes[code] = hash
	}
	newAppCodes[n.newCode] = n.appHash
	newState.appCodes = newAppCodes
	n.doneCh <- struct{}{}
	return newState
}

func processNewApp(
	s stateT,
	h httpInputT,
	hash string,
	tags map[string]struct{}) stateT {

	appHash, err := hashFromString(hash)
	if err != nil {
		http.Error(h.w, err.Error(), 400)
		h.doneCh <- struct{}{}
		return s
	}
	posixTime := time.Now().Unix()
	app := appMsgT{
		s.publicSign,
		tags,
		appHash,
		posixTime,
		sliceToSig(sign.Sign(
			make([]byte, 0),
			common.HashToSlice(hashApp(
				tags, appHash, posixTime)),
			&s.secretSign)),
	}
	newApps := make([]appMsgT, len(s.apps))
	for i, thisApp := range s.apps {
		newApps[i] = thisApp
	}
	newApps = append(newApps, app)
	newS := s
	newS.apps = newApps
	encodedApps, err := json.Marshal(newApps)
	if err != nil {
		http.Error(h.w, err.Error(), 500)
		h.doneCh <- struct{}{}
		return s
	}

	err = ioutil.WriteFile(s.dataDir + "/apps.txt", encodedApps, 0600)
	if err != nil {
		http.Error(h.w, err.Error(), 500)
		h.doneCh <- struct{}{}
		return s
	}
	h.w.Write(common.HashToSlice(appHash))
	h.doneCh <- struct{}{}
	return newS
}

type handlerT func(http.ResponseWriter, *http.Request)

func handler(route string, inputChan chan httpInputT) handlerT {
	return func(w http.ResponseWriter, r *http.Request) {
		doneCh := make(chan struct{})
		inputChan <- httpInputT{w, r, route, doneCh}
		<-doneCh
	}
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
	author [32]byte,
	s stateT) stateT {

	awaitingKey, ok := s.awaitingSymmetricKey[author]
	if !ok {
		fmt.Println("!ok at awaitingKey")
		return s
	}
	keyHash := hashHereIsKey(newKey)
	signed, ok := sign.Open(
		make([]byte, 0),
		common.SigToSlice(newKey.Sig),
		&author)
	if !ok {
		fmt.Println("!ok at checking signature.")
		return s
	}
	if !bytes.Equal(signed, common.HashToSlice([32]byte(keyHash))) {
		fmt.Println("!bytes.Equal in processHereIsAnEncryptionKey")
		return s
	}

	secretKey, ok := s.keyPairs[newKey.YourPublicEncrypt]
	secretKeyBytes := [32]byte(secretKey)
	if !ok {
		fmt.Println("!ok at getting out the private key.")
		return s
	}

	keySlice, ok := box.Open(
		make([]byte, 0),
		encKeyToSlice(newKey.EncryptedSymmetricKey),
		&newKey.Nonce,
		&newKey.MyPublicEncrypt,
		&secretKeyBytes)
	if !ok {
		fmt.Println("!ok at decrypting key")
		return s
	}
	newS := s
	newAwaitingKeys := make(map[publicSignT]sendChunkT)
	for k, v := range s.awaitingSymmetricKey {
		newAwaitingKeys[k] = v
	}
	delete(newAwaitingKeys, author)
	newS.awaitingSymmetricKey = newAwaitingKeys
	newSymmetricKeys := make(map[publicSignT]symmetricEncrypt)
	for k, v := range s.symmetricKeys {
		newSymmetricKeys[k] = v
	}
	symmetricKey := common.SliceToHash(keySlice)
	newSymmetricKeys[author] = symmetricKey
	newS.symmetricKeys = newSymmetricKeys
	awaitingKey.symmetricEncryptKey = symmetricKey
	return sendChunk(newS, sendChunkT(awaitingKey))
}

type makeSymmetricKeyT struct {
	myPubSign              [32]byte
	mySecretSign [64]byte
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

func makeChunkFilePath(chunkHash [32]byte, dataDir string) string {
	filename := base64.URLEncoding.EncodeToString(
		common.HashToSlice(chunkHash))
	return dataDir + "/tmp/" + filename
}

type assembleApp struct {
	symmetricKey [32]byte
	filePaths    []string
	appHash      [32]byte
	tmpPath      string
	finalPath    string
	appSender    [32]byte
	appMsg appMsgT
}

func assembleAppNew(a assembleApp, s stateT) stateT {
	tmpDestF, err := os.OpenFile(a.tmpPath, appendFlags, 0600)
	hasher, err := blake2b.New256(nil)
	if err != nil {
		return newChunksFinished(a.appHash, s)
	}
	for _, filePath := range a.filePaths {
		chunk, err := ioutil.ReadFile(filePath)
		if err != nil {
			return newChunksFinished(a.appHash, s)
		}
		nFile, err := tmpDestF.Write(chunk)
		if err != nil {
			return newChunksFinished(a.appHash, s)
		}
		nHash, err := hasher.Write(chunk)
		if err != nil {
			return newChunksFinished(a.appHash, s)
		}
		if nFile != len(chunk) || nFile != nHash {
			return newChunksFinished(a.appHash, s)
		}
	}
	finalHash := hasher.Sum(make([]byte, 0))
	if !bytes.Equal(
		finalHash,
		common.HashToSlice(a.appHash)) {
		return newChunksFinished(a.appHash, s)
	}
	_ = os.Rename(a.tmpPath, a.finalPath)
	var receipt Decrypted
	receipt = AppReceiptT{
		sliceToSig(sign.Sign(
			make([]byte, 0),
			receiptHash(a.appHash, appReceiptCode),
			&s.secretSign)),
		a.appHash,
	}
	encoded, err := common.EncodeData(&receipt)
	if err != nil {
		return newChunksFinished(a.appHash, s)
	}
	nonce, err := makeNonce()
	if err != nil {
		return newChunksFinished(a.appHash, s)
	}
	encrypted := secretbox.Seal(
		make([]byte, 0),
		encoded,
		&nonce,
		&a.symmetricKey)
	s.tcpOutChan <- common.ClientToClient{
		Msg:       common.Encrypted{encrypted, nonce},
		Recipient: a.appSender,
		Author:    s.publicSign,
	}
	return addNewAppNew(a.appMsg, s)
}

type writeAppToFileAndSendReceiptT struct {
	appHash  blake2bHash
	filePath string
	chunk    []byte
	secretSign secretSignT
	symmetricKey symmetricEncrypt
	tcpOutChan chan common.ClientToClient
	sender publicSignT
	myPublicSign publicSignT
}

func writeAppToFileNew(w writeAppToFileAndSendReceiptT, s stateT) stateT {
	fmt.Println("Top of writeAppToFileT send function.")
	fmt.Println(w.filePath)
	err := ioutil.WriteFile(w.filePath, w.chunk, 0600)
	if err != nil {
		fmt.Println("Error writing app to file:")
		fmt.Println(err)
		return newChunksFinished(w.appHash, s)
	}
	var receipt Decrypted
	chunkHash := blake2b.Sum256(w.chunk)
	secretSignAsBytes := [64]byte(w.secretSign)
	receipt = ReceiptT{
		sliceToSig(sign.Sign(
			make([]byte, 0),
			receiptHash(chunkHash, receiptCode),
			&secretSignAsBytes)),
		chunkHash}
	encoded, err := common.EncodeData(&receipt)
	if err != nil {
		fmt.Println(err)
		return newChunksFinished(w.appHash, s)
	}
	nonce, err := makeNonce()
	if err != nil {
		return newChunksFinished(w.appHash, s)
	}
	keyAsBytes := [32]byte(w.symmetricKey)
	encrypted := secretbox.Seal(
		make([]byte, 0),
		encoded,
		&nonce,
		&keyAsBytes)
	w.tcpOutChan <- common.ClientToClient{
		Msg: common.Encrypted{encrypted, nonce},
		Recipient: w.sender,
		Author: w.myPublicSign,
	}
	return s
}

func newChunksFinished(t blake2bHash, s stateT) stateT {
	newS := s
	newChunksLoading := make(map[blake2bHash][]fileChunkPtrT)
	for k, v := range s.chunksLoading {
		newChunksLoading[k] = v
	}
	delete(newChunksLoading, t)
	newS.chunksLoading = newChunksLoading
	return newS
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

func addNewAppNew(a appMsgT, s stateT) stateT {
	lenApps := len(s.apps)
	newApps := make([]appMsgT, lenApps + 1)
	for i, app := range s.apps {
		newApps[i] = app
	}
	newApps[lenApps] = appMsgT(a)
	newS := s
	newS.apps = newApps
	return newS
}

func httpServer(
	inputChan chan httpInputT,
	homeCode string,
	port string) {

	mux := goji.NewMux()
	mux.HandleFunc(
		pat.Get("/getapp/:securityCode/:subRoute"),
		handler("getapp", inputChan))
	mux.HandleFunc(
		pat.Get("/makeapproute/:securityCode/:subRoute"),
		handler("makeapproute", inputChan))
	mux.HandleFunc(
		pat.Post("/invite/:securityCode/:subRoute"),
		handler("invite", inputChan))
	mux.HandleFunc(
		pat.Get("/getmyid/:securityCode"),
		handler("getmyid", inputChan))
	mux.HandleFunc(
		pat.Get("/getmembers/:securityCode"),
		handler("getmembers", inputChan))
	mux.HandleFunc(
		pat.Post("/sendapp/:securityCode"),
		handler("sendapp", inputChan))
	mux.HandleFunc(
		pat.Post("/saveapp/:securityCode"),
		handler("saveapp", inputChan))
	mux.HandleFunc(
		pat.Post("/searchapps/:securityCode"),
		handler("searchapps", inputChan))
	http.ListenAndServe(":"+port, mux)
}

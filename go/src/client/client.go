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

type inputT interface {
	update(*stateT) (stateT, outputT)
}

type outputT interface {
	send() inputT
}

type stateT struct {
	apps           []appMsgT
	httpChan       chan httpInputT
	tcpInChan      chan envelopeT
	tcpOutChan     chan envelopeT
	getInvitesChan chan chan map[inviteT]dontCareT
	homeCode       string
	appCodes       map[string][32]byte
	publicSign     [32]byte
	secretSign     [64]byte
	secretEncrypt  [32]byte
	publicEncrypt  [32]byte
	conn           net.Conn
	online         bool
	cantGetOnline  error
	invites        map[inviteT]dontCareT
	uninvites      map[inviteT]dontCareT
	members        map[[32]byte]dontCareT
	isMember       bool
	isMemberCh     chan isMemberT
	chunksLoading  map[[32]byte][]fileChunkPtrT
	dataDir string
	port string
}

type isMemberT struct {
	returnCh  chan bool
	candidate [32]byte
}

func makeMemberList(
	invites map[inviteT]dontCareT,
	uninvites map[inviteT]dontCareT) map[[32]byte]dontCareT {

	members := make(map[[32]byte]dontCareT)
	members[common.TruesPubSign] = dontCareT{}
	addedMember := true
	for addedMember {
		addedMember = false
		for invite, _ := range invites {
			_, ok := members[invite.Author]
			if !ok {
				continue
			}
			for uninvite, _ := range uninvites {
				if uninviteCancels(uninvite, invite) {
					continue
				}
			}
			members[invite.Invitee] = dontCareT{}
			addedMember = true
		}
	}
	return members
}

func isMember(
	invites map[inviteT]dontCareT,
	uninvites map[inviteT]dontCareT,
	candidate [32]byte) bool {

	_, ok := makeMemberList(invites, uninvites)[candidate]
	return ok
}

type dontCareT struct{}

func readInvites(filePath string) (map[inviteT]dontCareT, error) {
	rawInvites, err := ioutil.ReadFile(filePath)
	var invites map[inviteT]dontCareT
	if err != nil {
		return invites, nil
	}
	err = json.Unmarshal(rawInvites, &invites)
	return invites, err
}

func readApps(dataDir string) ([]appMsgT, error) {
	rawApps, err := ioutil.ReadFile(dataDir + "/apps.txt")
	var apps []appMsgT
	if err != nil {
		return apps, nil
	}
	err = json.Unmarshal(rawApps, &apps)
	return apps, err
}

type readHttpInputT struct {
	httpChan   chan httpInputT
	tcpInChan  chan envelopeT
	homeCode   string
	invitesCh  chan isMemberT
	memberList map[[32]byte]dontCareT
	dataDir string
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

func getFilePart(bodyFileReader *multipart.Reader) (*multipart.Part, error) {
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
	dataDir string) (string, map[string]dontCareT, error) {

	bodyFileReader, err := r.MultipartReader()
	filepart, err := getFilePart(bodyFileReader)
	if err != nil {
		return "", *new(map[string]dontCareT), err
	}
	if err != nil {
		return "", *new(map[string]dontCareT), err
	}
	tmpFileName, err := genCode()
	if err != nil {
		return "", *new(map[string]dontCareT), err
	}
	tmpPath := dataDir + "/tmp/" + tmpFileName
	fileHandle, err := os.Create(tmpPath)
	defer fileHandle.Close()
	if err != nil {
		return "", *new(map[string]dontCareT), err
	}
	hasher, err := blake2b.New256(nil)
	if err != nil {
		return "", *new(map[string]dontCareT), err
	}
	tee := io.TeeReader(filepart, hasher)
	_, err = io.Copy(fileHandle, tee)
	_, err = io.Copy(fileHandle, filepart)
	if err != nil {
		return "", *new(map[string]dontCareT), err
	}
	hash := base64.URLEncoding.EncodeToString(hasher.Sum(nil))
	err = os.Rename(tmpPath, dataDir + "/apps/" + hash)
	if err != nil {
		return "", *new(map[string]dontCareT), err
	}
	tagBytes, err := getTagsPart(bodyFileReader)
	if err != nil {
		return "", *new(map[string]dontCareT), err
	}
	tags, err := parseTags(tagBytes)
	if err != nil {
		return "", *new(map[string]dontCareT), err
	}
	return hash, tags, nil
}

func tagOk(tag string) error {
	if len(tag) > 100 {
		return errors.New("Tag too long.")
	}
	return nil
}

func parseTags(bs []byte) (map[string]dontCareT, error) {
	f := func(c rune) bool {
		return c == ','
	}
	tagslice := strings.FieldsFunc(string(bs), f)
	tagmap := make(map[string]dontCareT)
	for _, tag := range tagslice {
		err := tagOk(tag)
		if err != nil {
			return tagmap, err
		}
		tagmap[tag] = dontCareT{}
	}
	return tagmap, nil
}

type newAppT struct {
	tags      map[string]dontCareT
	hash      string
	w         http.ResponseWriter
	doneCh    chan endRequest
	posixTime int64
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

func (n newAppT) update(s *stateT) (stateT, outputT) {
	appHash, err := hashFromString(n.hash)
	if err != nil {
		return *s, sendHttpErrorT{
			n.w,
			err.Error(),
			400,
			n.doneCh,
		}
	}
	app := appMsgT{
		s.publicSign,
		n.tags,
		appHash,
		n.posixTime,
		sliceToSig(sign.Sign(
			make([]byte, 0),
			common.HashToSlice(hashApp(
				n.tags, appHash, n.posixTime)),
			&s.secretSign)),
	}
	newApps := make([]appMsgT, len(s.apps))
	for i, thisApp := range s.apps {
		newApps[i] = thisApp
	}
	newApps = append(newApps, app)
	newS := *s
	newS.apps = newApps
	encodedApps, err := json.Marshal(newApps)
	if err != nil {
		return *s, sendHttpErrorT{
			w:      n.w,
			msg:    err.Error(),
			code:   500,
			doneCh: n.doneCh,
		}
	}
	return newS, writeNewAppsT{
		s.dataDir + "/apps.txt",
		encodedApps,
		n.w,
		n.doneCh,
		common.HashToSlice(appHash),
	}
}

func (w writeNewAppsT) send() inputT {
	err := ioutil.WriteFile(w.filePath, w.encodedApps, 0600)
	if err != nil {
		http.Error(w.w, err.Error(), 500)
		w.doneCh <- endRequest{}
		return noInputT{}
	}
	w.w.Write(w.appHash)
	w.doneCh <- endRequest{}
	return noInputT{}
}

func (r readHttpInputT) send() inputT {
	select {
	case h := <-r.httpChan:
		req := h.r
		securityCode := pat.Param(h.r, "securityCode")
		subRoute := ""
		if h.route == "getapp" || h.route == "makeapproute" {
			subRoute = pat.Param(h.r, "subRoute")
		}
		if h.route == "saveapp" {
			hash, tags, err := writeAppToFile(h.r, r.dataDir)
			if err != nil {
				fmt.Println(err)
				http.Error(h.w, err.Error(), 500)
				h.doneCh <- endRequest{}
				return noInputT{}
			}
			return newAppT{
				tags,
				hash,
				h.w,
				h.doneCh,
				time.Now().Unix()}
		}
		body, err := ioutil.ReadAll(req.Body)
		if err != nil {
			http.Error(
				h.w,
				err.Error(),
				http.StatusInternalServerError)
			h.doneCh <- endRequest{}
			return noInputT{}
		}
		return normalApiInputT{
			h.w,
			securityCode,
			body,
			h.route,
			subRoute,
			h.doneCh,
		}
	case tcpIn := <-r.tcpInChan:
		return tcpIn
	case request := <-r.invitesCh:
		_, ok := r.memberList[request.candidate]
		request.returnCh <- ok
		return noInputT{}
	}
	return noInputT{}
}

type stopListenT struct{}

type msgT interface {
	code() byte
}

func intToTwoBytes(i int) ([]byte, error) {
	if i < 0 {
		return *new([]byte), errors.New("Int less than zero.")
	}
	if i > 256*256 {
		return *new([]byte), errors.New(
			"Int greater than 256*256.")
	}
	u := uint(i)
	return []byte{
		(byte)(u & 0xff),
		(byte)((u & 0xff00) >> 8)}, nil
}

func encodeMsg(
	envelope envelopeT,
	publicSign [32]byte,
	secretEncrypt [32]byte,
	nonce [24]byte) ([]byte, error) {

	rawEncoded, err := encodeData(envelope.msg)
	if err != nil {
		return *new([]byte), err
	}
	encodedMsg := append(
		[]byte{envelope.msg.code()},
		rawEncoded...)
	encryptedMsg := box.Seal(
		make([]byte, 0),
		encodedMsg,
		&nonce,
		&envelope.correspondent,
		&secretEncrypt)
	outerMsg, err := encodeData(common.ClientToClient{
		encryptedMsg,
		envelope.correspondent,
		nonce,
		publicSign,
	})
	if err != nil {
		return *new([]byte), err
	}
	lenOuterMsg := len(outerMsg)
	if lenOuterMsg > 16000 {
		return *new([]byte), errors.New("Message too long.")
	}
	lenInBytes, err := intToTwoBytes(lenOuterMsg)
	if err != nil {
		return *new([]byte), err
	}
	return append(lenInBytes, outerMsg...), nil
}

type envelopeT struct {
	msg           msgT
	correspondent [32]byte
}

func makeConn(
	publicSign [32]byte,
	secretSign [64]byte) (net.Conn, error) {

	conn, connErr := net.Dial(
		"tcp",
		"localhost:4000")
	if connErr != nil {
		return conn, connErr
	}
	enc := gob.NewEncoder(conn)
	dec := gob.NewDecoder(conn)
	var authCode [common.AuthCodeLength]byte
	connErr = dec.Decode(&authCode)
	if connErr != nil {
		return conn, connErr
	}
	authSig := common.AuthSigT{
		publicSign,
		signedAuthToSlice(sign.Sign(
			make([]byte, 0),
			common.AuthCodeToSlice(
				authCode),
			&secretSign)),
	}
	connErr = enc.Encode(authSig)
	return conn, connErr
}

func tcpListen(
	conn net.Conn,
	isMemberCh chan isMemberT,
	secretEncrypt [32]byte,
	inChan chan envelopeT,
	stopListenChan chan stopListenT) {

	for {
		msgLenB := make([]byte, 2)
		n, err := conn.Read(msgLenB)
		if n != 2 {
			continue
		}
		if err != nil {
			return
		}
		mLen := twoBytesToInt(msgLenB)
		if mLen > 16000 {
			return
		}
		msg := make([]byte, mLen)
		n, err = conn.Read(msg)
		if n != mLen {
			continue
		}
		if err != nil {
			return
		}
		cToC, err := decodeClientToClient(msg)
		if err != nil {
			continue
		}
		var okCh chan bool
		isMemberCh <- isMemberT{
			okCh, cToC.Author}
		if !<-okCh {
			continue
		}
		decoded, author, err := decodeMsg(cToC, secretEncrypt)
		if err != nil {
			continue
		}
		inChan <- envelopeT{
			msg:           decoded,
			correspondent: author,
		}
		select {
		case <-stopListenChan:
			return
		default:
		}
	}
}

func tcpServer(
	inChan chan envelopeT,
	outChan chan envelopeT,
	isMemberCh chan isMemberT,
	secretSign [64]byte,
	secretEncrypt [32]byte,
	publicSign [32]byte) {

	var conn net.Conn
	connErr := errors.New("Not connected yet.")
	var stopListenChan chan stopListenT
	for {
		if connErr != nil {
			conn, connErr := makeConn(
				publicSign,
				secretSign)
			if connErr != nil {
				continue
			}
			go tcpListen(
				conn,
				isMemberCh,
				secretEncrypt,
				inChan,
				stopListenChan)
		}

		toSend := <-outChan
		nonce, err := makeNonce()
		if err != nil {
			panic(err.Error())
		}
		encoded, err := encodeMsg(
			toSend,
			publicSign,
			secretEncrypt,
			nonce)
		if err != nil {
			fmt.Println(err)
			continue
		}
		n, connErr := conn.Write(encoded)
		if connErr != nil {
			stopListenChan <- stopListenT{}
			continue
		}
		if n != len(encoded) {
			connErr = errors.New(
				"Didn't send whole message.")
			stopListenChan <- stopListenT{}
		}
	}
}

const (
	//appsDir     = "clientData/apps"
	//keysFile    = "clientData/TOP_SECRET_DONT_SHARE.txt"
	appendFlags = os.O_APPEND | os.O_CREATE | os.O_WRONLY
	//sentMsgPath = "clientData/sentMsgs.txt"
	homeDir     = "home"
)

func sentMsgPath(dataDir string) string {
	return dataDir + "/sentMsgPath"
}

type normalApiInputT struct {
	w            http.ResponseWriter
	securityCode string
	body         []byte
	route        string
	subRoute     string
	doneCh       chan endRequest
}

func (n normalApiInputT) update(s *stateT) (stateT, outputT) {
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
		return processGetMyId(n, s)
	}
	return *s, readHttpIn(s)
}

func processGetMyId(
	n normalApiInputT,
	s *stateT) (stateT, outputT) {

	if !strEq(n.securityCode, s.homeCode) {
		return *s, sendHttpErrorT{
			n.w,
			"Bad security code.",
			400,
			n.doneCh,
		}
	}

	return *s, httpOkResponseT{
		[]byte(base64.URLEncoding.EncodeToString(common.HashToSlice(s.publicSign))),
		n.w,
		n.doneCh,
	}
}

type makeAppRouteT struct {
	Apphash []byte
}

type sendHttpErrorT struct {
	w      http.ResponseWriter
	msg    string
	code   int
	doneCh chan endRequest
}

func (s sendHttpErrorT) send() inputT {
	http.Error(s.w, s.msg, s.code)
	s.doneCh <- endRequest{}
	return noInputT{}
}

func strEq(s1, s2 string) bool {
	eq := subtle.ConstantTimeCompare([]byte(s1), []byte(s2))
	return eq == 1
}

type unpackAppT struct {
	appCodes map[string][32]byte
	w        http.ResponseWriter
	appHash  [32]byte
	doneCh   chan endRequest
	appPath  string
	tmpPath  string
	port string
}

func (g unpackAppT) send() inputT {
	sendErr := func(err error) {
		fmt.Println(err)
		http.Error(g.w, err.Error(), 500)
		g.doneCh <- endRequest{}
	}
	_, err := os.Stat(g.tmpPath)
	if err == nil {
		appCode, err := getHashSecurityCode(g.appCodes, g.appHash)
		if err != nil {
			sendErr(err)
			return noInputT{}
		}
		err = browser.OpenURL(
			"http://localhost:" + g.port + "/getapp/" + appCode + "/index.html")
		if err != nil {
			sendErr(err)
			return noInputT{}
		}
		g.doneCh <- endRequest{}
		return noInputT{}
	}
	fileHandle, err := os.Open(g.appPath)
	if err != nil {
		fmt.Println("no file handle")
		sendErr(err)
		return noInputT{}
	}
	err = os.Mkdir(g.tmpPath, 0755)
	if err != nil {
		fmt.Println("mkdir failed")
		sendErr(err)
		return noInputT{}
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
			return noInputT{}
		}
		sourcePath := g.tmpPath + "/" + hdr.Name
		sourceHandle, err := os.Create(sourcePath)
		if err != nil {
			sendErr(err)
			return noInputT{}
		}
		_, err = io.Copy(sourceHandle, tr)
		if err != nil {
			sendErr(err)
			return noInputT{}
		}
	}
	newCode, err := genCode()
	if err != nil {
		sendErr(err)
		return noInputT{}
	}
	err = browser.OpenURL("http://localhost:" + g.port + "/getapp/" + newCode + "/index.html")
	if err != nil {
		sendErr(err)
		return noInputT{}
	}
	return newAppCodeT{
		g.w,
		g.appHash,
		g.doneCh,
		newCode,
	}
}

type newAppCodeT struct {
	w       http.ResponseWriter
	appHash [32]byte
	doneCh  chan endRequest
	newCode string
}

func (n newAppCodeT) update(s *stateT) (stateT, outputT) {
	newState := *s
	newAppCodes := make(map[string][32]byte)
	for code, hash := range s.appCodes {
		newAppCodes[code] = hash
	}
	newAppCodes[n.newCode] = n.appHash
	newState.appCodes = newAppCodes
	return newState, httpOkResponseT{
		[]byte{},
		n.w,
		n.doneCh,
	}
}

func getDocHash(
	securityCode string,
	appCodes map[string][32]byte) ([32]byte, error) {

	for sc, hash := range appCodes {
		if strEq(sc, securityCode) {
			return hash, nil
		}
	}
	var empty [32]byte
	return empty, errors.New("Could not find document hash.")
}

func hashToStr(h [32]byte) string {
	asSlice := common.HashToSlice(h)
	return base64.URLEncoding.EncodeToString(asSlice)
}

type sendAppJsonT struct {
	AppHash    [32]byte
	Recipients [][32]byte
}

type logSendErrT struct {
	posixTime int64
	appHash   [32]byte
	recipient [32]byte
	err       error
}

func sendErrFile(dataDir string) string {
	return dataDir + "sendErrors.txt"
}

func logSendErr(err error, appHash [32]byte, recipient [32]byte, dataDir string) {
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
		sendErrFile(dataDir),
		appendFlags,
		0600)
	if openErr != nil {
		fmt.Print(openErr)
		return
	}
	defer f.Close()
	_, writeErr := f.Write(append([]byte("\n"), encoded...))
	if writeErr != nil {
		fmt.Print(writeErr)
	}
}

type logSentSuccessT struct {
	appHash   [32]byte
	recipient [32]byte
}

func logSentSuccess(appHash [32]byte, recipient [32]byte, dataDir string) {
	msg := logSentSuccessT{appHash, recipient}
	encoded, jsonErr := json.Marshal(msg)
	if jsonErr != nil {
		logSendErr(jsonErr, appHash, recipient, dataDir)
		return
	}
	f, openErr := os.OpenFile(sentMsgPath(dataDir), appendFlags, 0600)
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

func sendMsg(
	envelope envelopeT,
	tcpInChan chan envelopeT,
	tcpOutChan chan envelopeT) error {

	tcpOutChan <- envelope
	select {
	case response := <-tcpInChan:
		receipt, ok := (response.msg).(receiptT)
		if !ok {
			return errors.New("Bad receipt.")
		}
		expectedHash, err := hash(envelope.msg)
		if err != nil {
			return err
		}
		givenReceiptHash, ok := sign.Open(
			make([]byte, 0),
			common.SigToSlice(receipt.hashSig),
			&receipt.author)
		if !ok {
			return errors.New("Bad receipt.")
		}
		if !bytes.Equal(
			givenReceiptHash,
			receiptHash(expectedHash, receiptCode)) {

			return errors.New("Bad receipt.")
		}
	case <-time.After(5 * time.Second):
		return errors.New("No receipt.")
	}
	return nil
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

var appSigMeaning = []byte{
	0x58, 0x46, 0x8d, 0x82, 0xa7, 0xfb, 0xe3, 0xe1, 0x33, 0xd6,
	0xbc, 0x25, 0x2e, 0x4c, 0x2c, 0xd5}

func concatTags(t map[string]dontCareT) []byte {
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

func hashApp(tags map[string]dontCareT, appHash [32]byte, posixTime int64) [32]byte {
	tagBytes := concatTags(tags)
	lenTags := len(tagBytes)
	concat := make([]byte, lenTags+32+8)
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
	return blake2b.Sum256(concat)
}

type appMsgT struct {
	Author    [32]byte
	Tags      map[string]dontCareT
	AppHash   [32]byte
	PosixTime int64
	Sig       [common.SigSize]byte
}

type searchResultT struct {
	Author    string
	Tags      []string
	Hash      string
	Posixtime int64
}

func (a appMsgT) code() byte {
	return appMsgB
}

func appSigHash(appHash [32]byte) []byte {
	return common.HashToSlice(blake2b.Sum256(append(
		common.HashToSlice(appHash),
		appSigMeaning...)))
}

func encodeData(a interface{}) ([]byte, error) {
	var buf bytes.Buffer
	enc := gob.NewEncoder(&buf)
	err := enc.Encode(a)
	if err != nil {
		return make([]byte, 0), err
	}
	return buf.Bytes(), nil
}

func processSendApp(n normalApiInputT, s *stateT) (stateT, outputT) {
	sendErr := func(msg string) sendHttpErrorT {
		return sendHttpErrorT{n.w, msg, 400, n.doneCh}
	}
	if !strEq(n.securityCode, s.homeCode) {
		return *s, sendErr("Bad security code.")
	}
	var sendAppJson sendAppJsonT
	err := json.Unmarshal(n.body, &sendAppJson)
	if err != nil {
		return *s, sendErr("Could not decode Json.")
	}
	if len(sendAppJson.Recipients) == 0 {
		return *s, sendErr("No recipients.")
	}
	var app appMsgT
	for _, thisApp := range s.apps {
		if equalHashes(thisApp.AppHash, sendAppJson.AppHash) {
			app = thisApp
			break
		}
	}
	filepath := s.dataDir + "/apps/" + hashToStr(sendAppJson.AppHash)
	return *s, sendAppT{
		app,
		s.tcpInChan,
		s.tcpOutChan,
		filepath,
		n.w,
		n.doneCh,
		sendAppJson.Recipients,
		sendAppJson.AppHash,
		s.secretSign,
		s.secretEncrypt,
		s.dataDir,
	}
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

type fileChunk struct {
	appHash   [32]byte
	chunk     []byte
	counter   int
	lastChunk bool
}

func sendFileToOne(
	s sendAppT,
	recipient [32]byte,
	fileHandle *os.File) error {

	sender := func(msg msgT) error {
		return sendMsg(
			envelopeT{msg, recipient},
			s.tcpInChan,
			s.tcpOutChan)
	}

	err := sender(s.appMsg)
	if err != nil {
		return err
	}

	counter := 0
	for {
		chunk := make([]byte, common.ChunkSize)
		n, err := fileHandle.Read(chunk)
		if err != nil {
			return err
		}
		lastChunk := n < common.ChunkSize
		err = sender(fileChunk{
			s.appHash,
			chunk,
			counter,
			lastChunk,
		})
		if err != nil {
			return err
		}
		if lastChunk {
			break
		}
		counter++
	}
	logSentSuccess(s.appHash, recipient, s.dataDir)
	return nil
}

func (s sendAppT) send() inputT {
	fileHandle, err := os.Open(s.filepath)
	errOut := func(err error) {
		logSendErr(err, s.appHash, s.recipients[0], s.dataDir)
		http.Error(s.w, err.Error(), 500)
		s.doneCh <- endRequest{}
	}
	if err != nil {
		errOut(err)
		return noInputT{}
	}
	for _, recipient := range s.recipients {
		err := sendFileToOne(s, recipient, fileHandle)
		if err != nil {
			errOut(err)
			return noInputT{}
		}
	}
	s.doneCh <- endRequest{}
	return noInputT{}
}

type searchQueryT struct {
	Tags         []string
	SearchString string
}

func sliceToSet(slice []string) map[string]dontCareT {
	var set map[string]dontCareT
	for _, s := range slice {
		set[s] = dontCareT{}
	}
	return set
}

func isSubset(sub []string, super map[string]dontCareT) bool {
	for _, s := range sub {
		_, ok := super[s]
		if !ok {
			return false
		}
	}
	return true
}

func matchesSearch(searchString string, tags map[string]dontCareT) bool {
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

func setToSlice(set map[string]dontCareT) []string {
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
	matchingTags := make(map[string]dontCareT)
	for i, app := range filtered {
		matchingApps[i] = appToSearchResult(app)
		for tag, _ := range app.Tags {
			if strings.Contains(tag, q.SearchString) {
				matchingTags[tag] = dontCareT{}
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

func processSearchApps(
	n normalApiInputT,
	s *stateT) (stateT, outputT) {

	sendErr := func(msg string) sendHttpErrorT {
		return sendHttpErrorT{n.w, msg, 400, n.doneCh}
	}
	if !strEq(n.securityCode, s.homeCode) {
		return *s, sendErr("Bad security code.")
	}
	var searchQuery searchQueryT
	err := json.Unmarshal(n.body, &searchQuery)
	if err != nil {
		return *s, sendErr("Could not decode Json.")
	}
	matchingApps, err := search(s.apps, searchQuery)
	if err != nil {
		return *s, sendErr(err.Error())
	}
	encoded, err := json.Marshal(matchingApps)
	if err != nil {
		return *s, sendErr("Couldn't encode search results.")
	}
	return *s, httpOkResponseT{encoded, n.w, n.doneCh}
}

func processGetApp(n normalApiInputT, s *stateT) (stateT, outputT) {
	if strEq(n.securityCode, s.homeCode) {
		return *s, serveDocT{
			n.w,
			n.doneCh,
			homeDir + "/" + n.subRoute,
		}
	}
	docHash, err := getDocHash(n.securityCode, s.appCodes)
	if err != nil {
		return *s, sendHttpErrorT{
			n.w,
			"Bad security code.",
			400,
			n.doneCh,
		}
	}
	return *s, serveDocT{
		n.w,
		n.doneCh,
		s.dataDir + "/tmp/" + hashToStr(docHash) + "/" + n.subRoute,
	}
}

type serveDocT struct {
	w        http.ResponseWriter
	doneCh   chan endRequest
	filePath string
}

func (s serveDocT) send() inputT {
	fileHandle, err := os.Open(s.filePath)
	if err != nil {
		http.Error(s.w, err.Error(), 500)
		s.doneCh <- endRequest{}
		return noInputT{}
	}
	_, err = io.Copy(s.w, fileHandle)
	if err != nil {
		http.Error(s.w, err.Error(), 500)
		s.doneCh <- endRequest{}
		return noInputT{}
	}
	s.doneCh <- endRequest{}
	return noInputT{}
}

func processInvite(
	n normalApiInputT,
	s *stateT) (stateT, outputT) {

	sendErr := func(err error) sendHttpErrorT {
		return sendHttpErrorT{
			n.w,
			err.Error(),
			400,
			n.doneCh,
		}
	}
	if !strEq(n.securityCode, s.homeCode) {
		err := errors.New("Bad security code.")
		return *s, sendErr(err)
	}
	invitee, err := base64.URLEncoding.DecodeString(n.subRoute)
	if err != nil {
		return *s, sendErr(err)
	}
	return *s, getTimeForInviteT{n.w, n.doneCh, common.SliceToHash(invitee)}
}

type getTimeForInviteT struct {
	w http.ResponseWriter
	doneCh chan endRequest
	invitee [32]byte
}

func (g getTimeForInviteT) send() inputT {
	return makeInviteT{g.invitee, time.Now().Unix(), g.w, g.doneCh}
}

type makeInviteT struct {
	invitee [32]byte
	posixTime int64
	w http.ResponseWriter
	doneCh chan endRequest
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

func copyInvites(invites map[inviteT]dontCareT) map[inviteT]dontCareT {
	newInvites := make(map[inviteT]dontCareT)
	for invite, _ := range invites {
		newInvites[invite] = dontCareT{}
	}
	return newInvites
}

func (m makeInviteT) update(s *stateT) (stateT, outputT) {
	invite := inviteT{
		PosixTime: m.posixTime,
		Invitee: m.invitee,
		Author: s.publicSign,
		Signature: sliceToSig(sign.Sign(
			make([]byte, 0),
			common.HashToSlice(inviteHash(
				m.posixTime, m.invitee)),
			&s.secretSign)),
	}
	newInvites := copyInvites(s.invites)
	newInvites[invite] = dontCareT{}
	encodedInvites, err := json.Marshal(newInvites)
	if err != nil {
		return *s, sendHttpErrorT{
			m.w,
			err.Error(),
			500,
			m.doneCh,
		}
	}
	return *s, writeUpdatedInvitesT{
		invitesFile(s.dataDir),
		encodedInvites,
		m.w,
		m.doneCh,
	}
}

type writeUpdatedInvitesT struct {
	filepath string
	toWrite []byte
	w http.ResponseWriter
	doneCh chan endRequest
}

func (w writeUpdatedInvitesT) send() inputT {
	err := ioutil.WriteFile(w.filepath, w.toWrite, 0600)
	if err != nil {
		http.Error(w.w, err.Error(), 500)
	}
	w.doneCh <- endRequest{}
	return noInputT{}
}

func processMakeAppRoute(
	n normalApiInputT,
	s *stateT) (stateT, outputT) {

	sendErr := func(err error) sendHttpErrorT {
		return sendHttpErrorT{
			n.w,
			err.Error(),
			400,
			n.doneCh,
		}
	}
	if !strEq(n.securityCode, s.homeCode) {
		err := errors.New("Bad security code.")
		return *s, sendErr(err)
	}
	hashSlice, err := base64.URLEncoding.DecodeString(n.subRoute)
	if err != nil {
		return *s, sendErr(err)
	}
	hash := common.SliceToHash(hashSlice)
	hashStr := hashToStr(hash)
	return *s, unpackAppT{
		s.appCodes,
		n.w,
		hash,
		n.doneCh,
		s.dataDir + "/apps/" + hashStr,
		s.dataDir + "/tmp/" + hashStr,
		s.port,
	}
}

func getHashSecurityCode(appCodes map[string][32]byte, hash [32]byte) (string, error) {
	for c, h := range appCodes {
		if equalHashes(h, hash) {
			return c, nil
		}
	}
	return "", errors.New("Could not find app.")
}

type httpOkResponseT struct {
	msg    []byte
	w      http.ResponseWriter
	doneCh chan endRequest
}

func (h httpOkResponseT) send() inputT {
	writer := h.w
	writer.Write(h.msg)
	h.doneCh <- endRequest{}
	return noInputT{}
}

type httpInputT struct {
	w      http.ResponseWriter
	r      *http.Request
	route  string
	doneCh chan endRequest
}

type noInputT struct{}

func (n noInputT) update(s *stateT) (stateT, outputT) {
	return *s, readHttpIn(s)
}

const pwlen = 5

func makePassword() ([]byte, error) {
	pw := make([]byte, pwlen)
	n, err := rand.Read(pw)
	if n != pwlen {
		return pw, errors.New("Wrong number of bytes.")
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

type sendAppT struct {
	appMsg        appMsgT
	tcpInChan     chan envelopeT
	tcpOutChan    chan envelopeT
	filepath      string
	w             http.ResponseWriter
	doneCh        chan endRequest
	recipients    [][32]byte
	appHash       [32]byte
	secretSign    [64]byte
	secretEncrypt [32]byte
	dataDir string
}

func pruneInvites(
	invites [][]common.InviteT,
	myId [32]byte,
	tNow int64) [][]common.InviteT {

	var result [][]common.InviteT
	for _, invite := range invites {
		if !common.IsMember(myId, invite, tNow) {
			continue
		}
		result = append(result, invite)
	}
	return result
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
	invites, err := readInvites(invitesFile(dataDir))
	if err != nil {
		return s, err
	}
	uninvites, err := readInvites(uninvitesFile(dataDir))
	if err != nil {
		return s, err
	}
	apps, err := readApps(dataDir)
	if err != nil {
		return s, err
	}
	memberList := makeMemberList(invites, uninvites)
	_, mem := memberList[keys.publicsign]
	return stateT{
		dataDir: dataDir,
		httpChan:      make(chan httpInputT),
		tcpInChan:     make(chan envelopeT),
		tcpOutChan:    make(chan envelopeT),
		homeCode:      homeCode,
		appCodes:      make(map[string][32]byte),
		publicSign:    keys.publicsign,
		secretSign:    keys.secretsign,
		secretEncrypt: keys.secretencrypt,
		publicEncrypt: keys.publicencrypt,
		conn:          *new(net.Conn),
		online:        false,
		cantGetOnline: nil,
		invites:       invites,
		uninvites:     uninvites,
		members:       memberList,
		isMember:      mem,
		apps:          apps,
		port: port,
	}, nil
}

func main() {
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
	err = os.Mkdir(dataDir + "/tmp", 0755)
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
		state.isMemberCh,
		state.secretSign,
		state.secretEncrypt,
		state.publicSign)
	go httpServer(state.httpChan, state.homeCode, port)
	fmt.Print(state.homeCode)
	err = browser.OpenURL(
		"http://localhost:" + port + "/getapp/" + state.homeCode + "/index.html")
	if err != nil {
		fmt.Println(err)
		return
	}
	var output outputT = readHttpIn(&state)
	for {
		input := output.send()
		state, output = input.update(&state)
	}
}

type handlerT func(http.ResponseWriter, *http.Request)

type endRequest struct{}

func handler(route string, inputChan chan httpInputT) handlerT {
	return func(w http.ResponseWriter, r *http.Request) {
		doneCh := make(chan endRequest)
		inputChan <- httpInputT{w, r, route, doneCh}
		<-doneCh
	}
}

func (e envelopeT) update(s *stateT) (stateT, outputT) {
	switch e.msg.code() {
	case receiptMsgB:
		return *s, readHttpIn(s)
	case fileChunkMsgB:
		return processNewFileChunk(e, s)
	case appMsgB:
		return processAppSigMsg(e, s)
	}
	return *s, readHttpIn(s)
}

func processAppSigMsg(e envelopeT, s *stateT) (stateT, outputT) {
	appSig, ok := (e.msg).(appMsgT)
	var newChunksLoading map[[32]byte][]fileChunkPtrT
	for appHash, chunkPtr := range s.chunksLoading {
		newChunksLoading[appHash] = chunkPtr
	}
	newS := *s
	delete(newChunksLoading, appSig.AppHash)
	newS.chunksLoading = newChunksLoading
	if !ok {
		return newS, readHttpIn(s)
	}
	signedHash, ok := sign.Open(
		make([]byte, 0),
		common.SigToSlice(appSig.Sig),
		&e.correspondent)
	hashOk := bytes.Equal(
		appSigHash(appSig.AppHash),
		signedHash)
	if !(ok && hashOk) {
		return newS, readHttpIn(s)
	}
	chunkPtrs, ok := s.chunksLoading[appSig.AppHash]
	if !ok {
		return newS, readHttpIn(s)
	}
	finalChunkPtr := chunkPtrs[len(chunkPtrs)-1]
	if !finalChunkPtr.lastChunk {
		return newS, readHttpIn(s)
	}
	filePaths := make([]string, len(chunkPtrs))
	for i, chunkPtr := range chunkPtrs {
		filePaths[i] = makeChunkFilePath(chunkPtr.chunkHash, s.dataDir)
	}
	tmpPath := makeChunkFilePath(appSig.AppHash, s.dataDir)
	finalName := base64.URLEncoding.EncodeToString(
		common.HashToSlice(appSig.AppHash))
	finalPath := s.dataDir + "/apps/" + finalName
	return newS, assembleApp{
		filePaths:  filePaths,
		appHash:    appSig.AppHash,
		tmpPath:    tmpPath,
		finalPath:  finalPath,
		appAuthor:  e.correspondent,
		tcpInChan:  s.tcpInChan,
		tcpOutChan: s.tcpOutChan,
		secretSign: s.secretSign,
	}
}

func makeChunkFilePath(chunkHash [32]byte, dataDir string) string {
	filename := base64.URLEncoding.EncodeToString(
		common.HashToSlice(chunkHash))
	return dataDir + "/tmp/" + filename
}

type assembleApp struct {
	filePaths  []string
	appHash    [32]byte
	tmpPath    string
	finalPath  string
	appAuthor  [32]byte
	tcpInChan  chan envelopeT
	tcpOutChan chan envelopeT
	secretSign [64]byte
}

func (a assembleApp) send() inputT {
	tmpDestF, err := os.OpenFile(a.tmpPath, appendFlags, 0600)
	for _, filePath := range a.filePaths {
		chunk, err := ioutil.ReadFile(filePath)
		if err != nil {
			return chunksFinishedT{a.appHash}
		}
		n, err := tmpDestF.Write(chunk)
		if n != len(chunk) || err != nil {
			return chunksFinishedT{a.appHash}
		}
	}
	hasher, err := blake2b.New256(nil)
	if err != nil {
		return chunksFinishedT{a.appHash}
	}
	finalHash := hasher.Sum(nil)
	if !bytes.Equal(
		finalHash,
		common.HashToSlice(a.appHash)) {

		return chunksFinishedT{a.appHash}
	}
	_ = os.Rename(a.tmpPath, a.finalPath)
	receipt := appReceiptT{sliceToSig(sign.Sign(
		make([]byte, 0),
		receiptHash(a.appHash, appReceiptCode),
		&a.secretSign))}
	_ = sendMsg(
		envelopeT{receipt, a.appAuthor},
		a.tcpInChan,
		a.tcpOutChan)
	return chunksFinishedT{a.appHash}

}

func processNewFileChunk(e envelopeT, s *stateT) (stateT, outputT) {
	chunk, ok := (e.msg).(fileChunk)
	if !ok {
		return *s, readHttpIn(s)
	}
	previousChunks, ok := s.chunksLoading[e.correspondent]
	lastChunk := previousChunks[len(previousChunks)-1]
	if lastChunk.lastChunk {
		return *s, readHttpIn(s)
	}
	var newChunksLoading map[[32]byte][]fileChunkPtrT
	for appHash, chunkPtr := range s.chunksLoading {
		newChunksLoading[appHash] = chunkPtr
	}
	chunkHash, err := hash(e.msg)
	if err != nil {
		return *s, readHttpIn(s)
	}
	if !ok && chunk.counter != 0 {
		return *s, readHttpIn(s)
	}
	if !ok {
		newChunksLoading[chunk.appHash] = []fileChunkPtrT{
			fileChunkPtrT{
				chunkHash: chunkHash,
				counter:   chunk.counter,
				lastChunk: chunk.lastChunk,
			}}
	} else {
		if lastChunk.counter != chunk.counter-1 {
			return *s, readHttpIn(s)
		}
		newChunksLoading[chunk.appHash] = append(
			newChunksLoading[chunk.appHash],
			fileChunkPtrT{
				chunkHash: chunkHash,
				counter:   chunk.counter,
				lastChunk: chunk.lastChunk,
			})
	}
	newS := *s
	newS.chunksLoading = newChunksLoading
	tmpFileName := base64.URLEncoding.EncodeToString(
		common.HashToSlice(chunkHash))
	return newS, writeAppToFileT{
		chunk.appHash,
		s.dataDir + "/tmp/" + tmpFileName,
		chunk.chunk}
}

type writeAppToFileT struct {
	appHash  [32]byte
	filePath string
	chunk    []byte
}

func (w writeAppToFileT) send() inputT {
	err := ioutil.WriteFile(w.filePath, w.chunk, 0600)
	if err != nil {
		return chunksFinishedT{w.appHash}
	}
	return noInputT{}
}

type writeNewAppsT struct {
	filePath    string
	encodedApps []byte
	w           http.ResponseWriter
	doneCh      chan endRequest
	appHash     []byte
}

type chunksFinishedT struct {
	appHash [32]byte
}

func readHttpIn(s *stateT) readHttpInputT {
	return readHttpInputT{
		s.httpChan,
		s.tcpInChan,
		s.homeCode,
		s.isMemberCh,
		s.members,
		s.dataDir}
}

func (t chunksFinishedT) update(s *stateT) (stateT, outputT) {
	newS := *s
	var newChunksLoading map[[32]byte][]fileChunkPtrT
	for k, v := range s.chunksLoading {
		newChunksLoading[k] = v
	}
	delete(newChunksLoading, t.appHash)
	newS.chunksLoading = newChunksLoading
	return newS, readHttpIn(s)
}

func decodeClientToClient(msg []byte) (common.ClientToClient, error) {
	var buf bytes.Buffer
	n, err := buf.Write(msg)
	var cToC common.ClientToClient
	if err != nil {
		return cToC, err
	}
	if n != len(msg) {
		return cToC, errors.New(
			"Could not write message to buffer.")
	}
	err = gob.NewDecoder(&buf).Decode(&cToC)
	if err != nil {
		return cToC, err
	}
	return cToC, nil
}

type receiptT struct {
	hashSig [common.SigSize]byte
	author  [32]byte
}

type appReceiptT struct {
	hashSig [common.SigSize]byte
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

func (r receiptT) code() byte {
	return receiptMsgB
}

func (f fileChunk) code() byte {
	return fileChunkMsgB
}

func (a appReceiptT) code() byte {
	return appReceiptMsgB
}

const (
	receiptMsgB    = 0x00
	appMsgB        = 0x01
	fileChunkMsgB  = 0x02
	appReceiptMsgB = 0x03
)

func decodeLow(bs []byte, result msgT) (msgT, error) {
	var buf bytes.Buffer
	n, err := buf.Write(bs)
	var msg msgT
	if n != len(bs) {
		return msg, errors.New("Could not read whole message.")
	}
	if err != nil {
		return msg, err
	}
	dec := gob.NewDecoder(&buf)
	err = dec.Decode(&result)
	if err != nil {
		return msg, err
	}
	return result, nil
}

func decodeMsg(
	cToC common.ClientToClient,
	secretEncrypt [32]byte) (msgT, [32]byte, error) {

	decrypted, ok := box.Open(
		make([]byte, 0),
		cToC.Msg,
		&cToC.Nonce,
		&cToC.Author,
		&secretEncrypt)
	if !ok {
		err := errors.New("Could not decrypt message.")
		return *new(msgT), *new([32]byte), err
	}
	typeByte := decrypted[0]
	msg := decrypted[1:]
	switch typeByte {
	case receiptMsgB:
		decoded, err := decodeLow(msg, *new(receiptT))
		return decoded, cToC.Author, err
	case appMsgB:
		decoded, err := decodeLow(msg, *new(appMsgT))
		return decoded, cToC.Author, err
	}
	err := errors.New("Message type byte unknown.")
	return *new(msgT), *new([32]byte), err
}

var routes = []string{"sendapp", "saveapp", "searchapps"}

func twoBytesToInt(bs []byte) int {
	return (int)(bs[0]) + (int)(bs[1])*256
}

func httpServer(inputChan chan httpInputT, homeCode string, port string) {
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
	for _, route := range routes {
		path := "/" + route + "/:securityCode"
		mux.HandleFunc(
			pat.Post(path),
			handler(route, inputChan))
	}
	http.ListenAndServe(":" + port, mux)
}

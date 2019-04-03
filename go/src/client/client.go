package main

import (
	"bytes"
	"common"
	"crypto/rand"
	"crypto/subtle"
	"encoding/base64"
	"encoding/gob"
	"encoding/json"
	"encoding/hex"
	"errors"
	"fmt"
	"goji.io"
	"goji.io/pat"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/nacl/sign"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/ssh/terminal"
	"golang.org/x/crypto/argon2"
	"syscall"
	"io"
	"io/ioutil"
	"mime/multipart"
	"net"
	"net/http"
	"os"
	"time"
)

type inputT interface {
	update(*stateT) (stateT, outputT)
}

type outputT interface {
	send(chan inputT)
}

type stateT struct {
	httpChan      chan httpInputT
	homeCode      string
	appCodes      map[string][32]byte
	publicSign    [32]byte
	secretSign    [64]byte
	secretEncrypt [32]byte
	publicEncrypt [32]byte
	conn          net.Conn
	online        bool
	cantGetOnline error
	invites       [][]common.InviteT
	isMember      bool
}

type readHttpInputT struct {
	ch       chan httpInputT
	homeCode string
}

func getPostFilePart(r *http.Request) (*multipart.Part, error) {
	var filepart *multipart.Part
	bodyFileReader, err := r.MultipartReader()
	if err != nil {
		return filepart, err
	}
	filepart, err = bodyFileReader.NextPart()
	if err != nil {
		return filepart, err
	}
	if filepart.FormName() != "upload" {
		msg := "Could not find form element \"upload\"."
		return filepart, errors.New(msg)
	}
	return filepart, nil
}

func writeAppToFile(r *http.Request) (string, error) {
	filepart, err := getPostFilePart(r)
	if err != nil {
		return "", err
	}
	tmpFileName, err := genCode()
	if err != nil {
		return "", err
	}
	tmpPath := docsDir + "/" + tmpFileName
	fileHandle, err := os.Create(tmpPath)
	defer fileHandle.Close()
	if err != nil {
		return "", err
	}
	hasher, err := blake2b.New256(nil)
	if err != nil {
		return "", err
	}
	tee := io.TeeReader(filepart, hasher)
	_, err = io.Copy(fileHandle, tee)
	if err != nil {
		return "", err
	}
	hash := base64.RawURLEncoding.EncodeToString(hasher.Sum(nil))
	err = os.Rename(tmpPath, docsDir+"/"+hash)
	if err != nil {
		return "", err
	}
	return hash, nil
}

func (r readHttpInputT) send(inputCh chan inputT) {
	select {
	case h := <-r.ch:
		req := h.r
		securityCode := pat.Param(h.r, "securitycode")
		if h.route == "saveapp" {
			hash, err := writeAppToFile(h.r)
			if err != nil {
				http.Error(h.w, err.Error(), 500)
				h.doneCh <- endRequest{}
				return
			}
			h.w.Write([]byte(hash))
			h.doneCh <- endRequest{}
			return
		}
		body, err := ioutil.ReadAll(req.Body)
		if err != nil {
			http.Error(
				h.w,
				err.Error(),
				http.StatusInternalServerError)
			h.doneCh <- endRequest{}
			inputCh <- noInputT{}
			return
		}
		inputCh <- normalApiInputT{
			h.w,
			securityCode,
			body,
			h.route,
			h.doneCh,
		}
		return
	default:
	}
	inputCh <- noInputT{}
}

const (
	invitesFile = "clientData/invitations.txt"
	keysFile    = "clientData/TOP_SECRET_DONT_SHARE.txt"
	docsDir     = "clientData/docs"
	wsUrl       = "ws://localhost:4000"
	blobLen     = 16000 - 32 - box.Overhead
	sendLogPath = "clientData/sendErrors.txt"
	appendFlags = os.O_APPEND | os.O_CREATE | os.O_WRONLY
	sentMsgPath = "clientData/sentMsgs.txt"
)

type normalApiInputT struct {
	w            http.ResponseWriter
	securityCode string
	body         []byte
	route        string
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
	}
	return *s, readHttpInputT{ch: s.httpChan}
}

type makeAppRouteT struct {
	apphash [32]byte
}

type sendHttpErrorT struct {
	w      http.ResponseWriter
	msg    string
	code   int
	doneCh chan endRequest
}

func (s sendHttpErrorT) send(inputCh chan inputT) {
	http.Error(s.w, s.msg, s.code)
	s.doneCh <- endRequest{}
	inputCh <- noInputT{}
	return
}

func strEq(s1, s2 string) bool {
	eq := subtle.ConstantTimeCompare([]byte(s1), []byte(s2))
	return eq == 1
}

type genCodeForAppT struct {
	w       http.ResponseWriter
	appHash [32]byte
	doneCh  chan endRequest
}

func (g genCodeForAppT) send(inputCh chan inputT) {
	newCode, err := genCode()
	if err != nil {
		http.Error(g.w, err.Error(), 500)
		g.doneCh <- endRequest{}
		inputCh <- noInputT{}
		return
	}
	inputCh <- newAppCodeT{
		g.w,
		g.appHash,
		g.doneCh,
		newCode,
	}
	return
}

type newAppCodeT struct {
	w       http.ResponseWriter
	appHash [32]byte
	doneCh  chan endRequest
	newCode string
}

func (n newAppCodeT) update(s *stateT) (stateT, outputT) {
	newState := *s
	var newAppCodes map[string][32]byte
	for code, hash := range s.appCodes {
		newAppCodes[code] = hash
	}
	newAppCodes[n.newCode] = n.appHash
	newState.appCodes = newAppCodes
	return newState, htmlOkResponseT{
		[]byte(n.newCode),
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
	return base64.RawURLEncoding.EncodeToString(asSlice)
}

type sendAppJsonT struct {
	appHash    [32]byte
	recipients [][32]byte
}

type logSendErrT struct {
	posixTime int64
	appHash   [32]byte
	recipient [32]byte
	err       error
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
	f, openErr := os.OpenFile(sendLogPath, appendFlags, 0600)
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

func logSentSuccess(appHash [32]byte, recipient [32]byte) {
	msg := logSentSuccessT{appHash, recipient}
	encoded, jsonErr := json.Marshal(msg)
	if jsonErr != nil {
		logSendErr(jsonErr, appHash, recipient)
		return
	}
	f, openErr := os.OpenFile(sentMsgPath, appendFlags, 0600)
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

var receiptMeaning = []byte{
	0x3f, 0x0e, 0x0e, 0x46, 0xf8, 0xaa, 0xac, 0xa6, 0xf2, 0x59,
	0xd8, 0x2d, 0xa7, 0x6f, 0x23, 0xd8}

func receiptHash(msgHash [32]byte) [32]byte {
	concat := make([]byte, 48)
	i := 0
	for i < 32 {
		concat[i] = msgHash[i]
		i++
	}
	for i < 48 {
		concat[i] = receiptMeaning[i-32]
		i++
	}
	return blake2b.Sum256(concat)
}

func sendMsg(
	msg interface{},
	conn net.Conn,
	secretEncrypt [32]byte,
	recipient [32]byte) error {

	encoded, err := encodeData(msg)
	if err != nil {
		return err
	}

	nonce, err := makeNonce()
	if err != nil {
		return err
	}

	encrypted := box.Seal(
		make([]byte, 0),
		encoded,
		&nonce,
		&recipient,
		&secretEncrypt)

	finalEncoded, err := encodeData(common.ClientToClient{
		encrypted,
		recipient,
		nil,
		nonce})
	if err != nil {
		return err
	}

	conn.Write(finalEncoded)

	dec := gob.NewDecoder(conn)
	var response common.ClientToClient
	err = dec.Decode(&response)
	if err != nil {
		return err
	}
	if response.Err != nil {
		return err
	}

	decryptedReceipt, ok := box.Open(
		make([]byte, 0),
		response.Msg,
		&response.Nonce,
		&recipient,
		&secretEncrypt)
	if !ok {
		return errors.New("Could not decrypt receipt.")
	}

	receipt, err := decodeReceipt(decryptedReceipt)
	if err != nil {
		return err
	}

	expectedReceipt := receiptHash(blake2b.Sum256(finalEncoded))
	if !equalHashes(receipt, expectedReceipt) {
		return errors.New("Bad receipt.")
	}

	return nil
}

func equalHashes(as [32]byte, bs [32]byte) bool {
	for i, a := range as {
		if a != bs[i] {
			return false
		}
	}
	return true
}

var appSigMeaning = []byte{
	0x58, 0x46, 0x8d, 0x82, 0xa7, 0xfb, 0xe3, 0xe1, 0x33, 0xd6,
	0xbc, 0x25, 0x2e, 0x4c, 0x2c, 0xd5}

type appSigMsgT struct {
	appHash [32]byte
	sig     [common.SigSize]byte
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

func decodeReceipt(bs []byte) ([32]byte, error) {
	var buf bytes.Buffer
	buf.Write(bs)
	dec := gob.NewDecoder(&buf)
	var receipt [32]byte
	err := dec.Decode(&receipt)
	if err != nil {
		return receipt, err
	}
	return receipt, nil
}

func processSendApp(n normalApiInputT, s *stateT) (stateT, outputT) {
	if !strEq(n.securityCode, s.homeCode) {
		return *s, sendHttpErrorT{
			n.w,
			"Bad security code.",
			400,
			n.doneCh,
		}
	}
	var sendAppJson sendAppJsonT
	err := json.Unmarshal(n.body, &sendAppJson)
	if err != nil {
		return *s, sendHttpErrorT{
			n.w,
			"Could not decode Json.",
			400,
			n.doneCh,
		}
	}
	if len(sendAppJson.recipients) == 0 {
		return *s, sendHttpErrorT{
			n.w,
			"No recipients.",
			400,
			n.doneCh,
		}
	}
	filepath := docsDir + "/" + hashToStr(sendAppJson.appHash)
	return *s, sendFileT{
		filepath,
		s.conn,
		n.w,
		n.doneCh,
		sendAppJson.recipients,
		sendAppJson.appHash,
		s.secretSign,
		s.secretEncrypt,
	}
}

func sliceToSig(bs []byte) [common.SigSize]byte {
	var result [common.SigSize]byte
	for i, b := range bs {
		result[i] = b
	}
	return result
}

type fileChunk struct {
	appHash   [32]byte
	chunk     []byte
	counter   int
	lastChunk bool
}

func sendFileToOne(
	s sendFileT,
	recipient [32]byte,
	fileHandle *os.File) error {

	sender := func(msg interface{}) error {
		return sendMsg(
			msg,
			s.conn,
			s.secretEncrypt,
			recipient)
	}

	err := sender(appSigMsgT{
		s.appHash,
		sliceToSig(sign.Sign(
			make([]byte, 0),
			appSigHash(s.appHash),
			&s.secretSign))})
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
	logSentSuccess(s.appHash, recipient)
	return nil
}

func (s sendFileT) send(inputCh chan inputT) {
	fileHandle, err := os.Open(s.filepath)
	errOut := func(err error) {
		logSendErr(err, s.appHash, s.recipients[0])
		http.Error(s.w, err.Error(), 500)
		s.doneCh <- endRequest{}
	}
	if err != nil {
		errOut(err)
		return
	}
	for _, recipient := range s.recipients {
		err := sendFileToOne(s, recipient, fileHandle)
		if err != nil {
			errOut(err)
		}
	}
	s.doneCh <- endRequest{}
}

func processGetApp(n normalApiInputT, s *stateT) (stateT, outputT) {
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
		docsDir + "/" + hashToStr(docHash),
	}
}

type serveDocT struct {
	w        http.ResponseWriter
	doneCh   chan endRequest
	filePath string
}

func (s serveDocT) send(inputCh chan inputT) {
	fileHandle, err := os.Open(s.filePath)
	if err != nil {
		http.Error(s.w, err.Error(), 500)
		s.doneCh <- endRequest{}
		return
	}
	_, err = io.Copy(s.w, fileHandle)
	if err != nil {
		http.Error(s.w, err.Error(), 500)
		s.doneCh <- endRequest{}
		return
	}
	s.doneCh <- endRequest{}
}

func processMakeAppRoute(
	n normalApiInputT,
	s *stateT) (stateT, outputT) {

	var makeAppRoute makeAppRouteT
	err := json.Unmarshal(n.body, &makeAppRoute)
	if err != nil {
		return *s, sendHttpErrorT{
			n.w,
			err.Error(),
			400,
			n.doneCh,
		}
	}
	if !strEq(n.securityCode, s.homeCode) {
		return *s, sendHttpErrorT{
			n.w,
			"Bad security code.",
			400,
			n.doneCh,
		}
	}
	return *s, genCodeForAppT{
		n.w,
		makeAppRoute.apphash,
		n.doneCh,
	}
}

type htmlOkResponseT struct {
	msg    []byte
	w      http.ResponseWriter
	doneCh chan endRequest
}

func (h htmlOkResponseT) send(inputCh chan inputT) {
	writer := h.w
	writer.Write(h.msg)
	h.doneCh <- endRequest{}
}

type httpInputT struct {
	w      http.ResponseWriter
	r      *http.Request
	route  string
	doneCh chan endRequest
}

type noInputT struct{}

func (n noInputT) update(s *stateT) (stateT, outputT) {
	return *s, readHttpInputT{s.httpChan, s.homeCode}
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
	return base64.RawURLEncoding.EncodeToString(authSlice), nil
}

type setupTcpConn struct {
	secretSign [64]byte
	publicSign [32]byte
	invites    []common.InviteT
}

type newTcpConn struct {
	conn net.Conn
}

type failedToGoOnline struct {
	err  error
	tNow int64
}

type sendFileT struct {
	filepath      string
	conn          net.Conn
	w             http.ResponseWriter
	doneCh        chan endRequest
	recipients    [][32]byte
	appHash       [32]byte
	secretSign    [64]byte
	secretEncrypt [32]byte
}

func (s setupTcpConn) send(inputCh chan inputT) {
	conn, err := net.Dial("tcp", "localhost:4000")
	if err != nil {
		inputCh <- failedToGoOnline{err, time.Now().Unix()}
		return
	}
	enc := gob.NewEncoder(conn)
	dec := gob.NewDecoder(conn)
	var authCode [common.AuthCodeLength]byte
	err = dec.Decode(&authCode)
	if err != nil {
		inputCh <- failedToGoOnline{err, time.Now().Unix()}
		return
	}
	authSig := common.AuthSigT{
		s.publicSign,
		s.invites,
		signedAuthToSlice(sign.Sign(
			make([]byte, 0),
			common.AuthCodeToSlice(authCode),
			&s.secretSign)),
	}
	err = enc.Encode(authSig)
	if err != nil {
		inputCh <- failedToGoOnline{err, time.Now().Unix()}
		return
	}
	inputCh <- newTcpConn{conn}
}

func (n newTcpConn) update(s *stateT) (stateT, outputT) {
	newS := *s
	newS.conn = n.conn
	return newS, readHttpInputT{s.httpChan, s.homeCode}
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

func (f failedToGoOnline) update(s *stateT) (stateT, outputT) {
	newS := *s
	newS.cantGetOnline = f.err
	newS.invites = pruneInvites(s.invites, s.publicSign, f.tNow)
	if len(newS.invites) == 0 {
		newS.isMember = false
		return newS, readHttpInputT{s.httpChan, s.homeCode}
	}
	return newS, setupTcpConn{
		s.secretSign,
		s.publicSign,
		newS.invites[0]}
}

func signedAuthToSlice(bs []byte) [common.AuthSigSize]byte {
	var result [common.AuthSigSize]byte
	for i, b := range bs {
		result[i] = b
	}
	return result
}

type secretsFileT struct {
	Publicsign [32]byte
	Publicencrypt [32]byte
	Nonce [24]byte
	Salt [32]byte
	Secretkeys []byte
}

type secretKeysT struct {
	Secretsign [64]byte
	Secretencrypt [32]byte
}

type keysT struct {
	publicsign [32]byte
	publicencrypt [32]byte
	secretsign [64]byte
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

func createKeys() error {
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
	fmt.Println("c")
	secretKey := slowHash(password, salt)
	encryptedSecrets := secretbox.Seal(
		make([]byte, 0),
		encodedSecrets,
		&nonce,
		&secretKey)
	secretsFile := secretsFileT{
		Publicsign: *pubSign,
		Publicencrypt: *pubEnc,
		Nonce: nonce,
		Salt: salt,
		Secretkeys: encryptedSecrets,
	}
	encodedFile, err := json.Marshal(secretsFile)
	if err != nil {
		fmt.Println("Could not encode.")
		return err
	}
	err = ioutil.WriteFile(keysFile, encodedFile, 0600)
	return err
}

func initState() (stateT, error) {
	homeCode, err := genCode()
	var s stateT
	if err != nil {
		return s, err
	}
	rawSecrets, err := ioutil.ReadFile(keysFile)
	if err != nil {
		err := createKeys()
		if err != nil {
			return s, err
		}
		rawSecrets, err = ioutil.ReadFile(keysFile)
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
	var invites [][]common.InviteT
	rawInvites, err := ioutil.ReadFile(invitesFile)
	if err == nil {
		err = json.Unmarshal(rawInvites, &invites)
		if err != nil {
			return s, err
		}
		invites := pruneInvites(
			invites,
			keys.publicsign,
			time.Now().Unix())
		encodedInvites, err := json.Marshal(invites)
		if err != nil {
			return s, err
		}
		err = ioutil.WriteFile(
			invitesFile,
			encodedInvites,
			0600)
		if err != nil {
			return s, err
		}
	}
	var conn net.Conn
	return stateT{
		httpChan: make(chan httpInputT),
		homeCode: homeCode,
		appCodes: make(map[string][32]byte),
		publicSign: keys.publicsign,
		secretSign: keys.secretsign,
		secretEncrypt: keys.secretencrypt,
		publicEncrypt: keys.publicencrypt,
		conn: conn,
		online: false,
		cantGetOnline: nil,
		invites: invites,
		isMember: len(invites) == 0,
	}, nil
}

func main() {
	state, err := initState()
	if err != nil {
		fmt.Println(err)
		return
	}
	go httpServer(state.httpChan)
	var input inputT = noInputT{}
	var output outputT = readHttpInputT{ch: state.httpChan}
	var inputCh chan inputT
	return
	for {
		go output.send(inputCh)
		select {
		case input = <-inputCh:
		default:
			input = noInputT{}
		}
		state, output = input.update(&state)
	}
}

type handlerT func(http.ResponseWriter, *http.Request)

type endRequest struct{}

func handler(route string, inputChan chan httpInputT) handlerT {
	return func(w http.ResponseWriter, r *http.Request) {
		var doneCh chan endRequest
		inputChan <- httpInputT{w, r, route, doneCh}
		<-doneCh
	}
}

var routes = []string{"makeapproute", "getapp", "sendapp"}

func httpServer(inputChan chan httpInputT) {
	mux := goji.NewMux()
	for _, route := range routes {
		path := "/" + route + "/:securityCode"
		mux.HandleFunc(
			pat.Post(path),
			handler(route, inputChan))
	}
	http.ListenAndServe(":3000", nil)
}

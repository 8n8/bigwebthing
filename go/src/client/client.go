package main

import (
	"crypto/rand"
	"bytes"
	"fmt"
	"crypto/subtle"
	"github.com/gorilla/websocket"
	"encoding/base64"
	"encoding/json"
	"errors"
	"goji.io"
	"goji.io/pat"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/nacl/sign"
	"io"
	"io/ioutil"
	"mime/multipart"
	"net/http"
	"os"
	"time"
	"common"
)

type inputT interface {
	update(*stateT) (stateT, outputT)
}

type outputT interface {
	send(chan inputT)
}

type stateT struct {
	httpChan chan httpInputT
	homeCode string
	appCodes map[string][32]byte
	publicSign [32]byte
	secretSign *[64]byte
	secretEncrypt *[32]byte
	publicEncrypt [32]byte
}

type readHttpInputT struct {
	ch chan httpInputT
}

type saveAppT struct {
	w            http.ResponseWriter
	r            *http.Request
	securityCode string
	doneCh       chan endRequest
}

func (a saveAppT) update(s *stateT) (stateT, outputT) {
	if !strEq(a.securityCode, s.homeCode) {
		return *s, sendHttpErrorT{
			w:      a.w,
			msg:    "Bad security code.",
			code:   400,
			doneCh: a.doneCh,
		}
	}
	return *s, writeAppToFileT{
		w:      a.w,
		r:      a.r,
		doneCh: a.doneCh,
	}
}

type writeAppToFileT struct {
	w      http.ResponseWriter
	r      *http.Request
	doneCh chan endRequest
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

func writeAppToFile(w writeAppToFileT) (string, error) {
	filepart, err := getPostFilePart(w.r)
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

func (w writeAppToFileT) send(inputCh chan inputT) {
	hash, err := writeAppToFile(w)
	if err != nil {
		http.Error(w.w, err.Error(), 500)
		w.doneCh <- endRequest{}
		return
	}
	writer := w.w
	writer.Write([]byte(hash))
	w.doneCh <- endRequest{}
	return
}

func (r readHttpInputT) send(inputCh chan inputT) {
	select {
	case h := <-r.ch:
		req := h.r
		securityCode := pat.Param(h.r, "securitycode")
		if h.route == "saveapp" {
			inputCh <- saveAppT{
				w:            h.w,
				r:            h.r,
				securityCode: securityCode,
				doneCh:       h.doneCh,
			}
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
			w:            h.w,
			securityCode: securityCode,
			body:         body,
			route:        h.route,
			doneCh:       h.doneCh,
		}
		return
	default:
	}
	inputCh <- noInputT{}
	return
}

type httpChansT struct {
	homeIn chan httpInputT
}

const (
	docsDir = "clientData/docs"
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
		w:       g.w,
		appHash: g.appHash,
		doneCh:  g.doneCh,
		newCode: newCode,
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
		msg:    []byte(n.newCode),
		w:      n.w,
		doneCh: n.doneCh,
	}
}

func getDocHash(securityCode string, appCodes map[string][32]byte) ([32]byte, error) {
	for sc, hash := range appCodes {
		if strEq(sc, securityCode) {
			return hash, nil
		}
	}
	var empty [32]byte
	return empty, nil
}

func byte32ToSlice(bs [32]byte) []byte {
	r := make([]byte, 32)
	for i, b := range bs {
		r[i] = b
	}
	return r
}

func hashToStr(h [32]byte) string {
	asSlice := byte32ToSlice(h)
	return base64.RawURLEncoding.EncodeToString(asSlice)
}

type sendAppJsonT struct {
	appHash [32]byte
	recipients [][32]byte
}

type makeSendHandles struct {
	filepath string
	w http.ResponseWriter
	doneCh chan endRequest
	recipients [][32]byte
	appHash [32]byte
}

const wsUrl = "ws://localhost:4000"

func (m makeSendHandles) send(inputCh chan inputT) {
	filehandle, err := os.Open(m.filepath)
	if err != nil {
		http.Error(m.w, err.Error(), 500)
		m.doneCh <- endRequest{}
		return
	}

	ws, _, err := websocket.DefaultDialer.Dial(wsUrl, nil)
	if err != nil {
		http.Error(m.w, err.Error(), 500)
		m.doneCh <- endRequest{}
		return
	}
	m.doneCh <- endRequest{}
	newNonce, err := makeNonce()
	if err != nil {
		http.Error(m.w, err.Error(), 500)
		m.doneCh <- endRequest{}
		return
	}
	inputCh <- newSendHandles{
		appHash: m.appHash,
		newNonce: newNonce,
		ws: ws,
		f: filehandle,
		recipients: m.recipients,
	}
}

type newSendHandles struct {
	appHash [32]byte
	ws *websocket.Conn
	f *os.File
	newNonce [24]byte
	recipients [][32]byte
}

type readBytesToSendT struct {
	ws *websocket.Conn
	f *os.File
	lastHash [32]byte
	appHash [32]byte
	recipients [][32]byte
}

const blobLen = 16000 - 32 - box.Overhead

type logSendErrT struct {
	posixTime int64
	appHash [32]byte
	recipient [32]byte
	err error
}

const sendLogPath = "clientData/sendErrors.txt"

const appendFlags = os.O_APPEND | os.O_CREATE | os.O_WRONLY

func logSendErr(err error, appHash [32]byte, recipient [32]byte) {
	msg := logSendErrT {
		posixTime: time.Now().Unix(),
		appHash: appHash,
		recipient: recipient,
		err: err,
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
	appHash [32]byte
	recipient [32]byte
}

const sentMsgPath = "clientData/sentMsgs.txt"

func logSentSuccess(appHash [32]byte, recipient [32]byte) {
	msg := logSentSuccessT{
		appHash: appHash,
		recipient: recipient,
	}
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

type readBytesErrT struct {
	err error
}

type blobReadyToGoT struct {
	blob []byte
	metadata []byte
	ws *websocket.Conn
	f *os.File
}

type headerMsgT struct {
	msgHash [32]byte
	signature [common.SigSize]byte
}

func signMsg(msg []byte, sKey *[64]byte) [common.SigSize]byte {
	hash := common.HashToSlice(blake2b.Sum256(msg))
	sig := sign.Sign(make([]byte, 0), hash, sKey)
	var sigSized [common.SigSize]byte
	for i, el := range sig {
		sigSized[i] = el
	}
	return sigSized
}

type writeSendErrT struct {
	appHash [32]byte
	err error
	recipient [32]byte
}

func (w writeSendErrT) send(inputCh chan inputT) {
	logSendErr(w.err, w.appHash, w.recipient)
}

var appSigCode = [16]byte{0xb3, 0x7b, 0x8d, 0x83, 0x9d, 0x6c, 0xd8, 0x6e, 0x52, 0x76, 0xb8, 0xf2, 0x2b, 0x0b, 0x9b, 0xc5}

func (n newSendHandles) update(s *stateT) (stateT, outputT) {
	sigSlice := sign.Sign(
		make([]byte, 0),
		common.MakeDigest(n.appHash, appSigCode),
		s.secretSign)
	var sigArr [common.SigSize]byte
	for i, sb := range sigSlice {
		sigArr[i] = sb
	}
	beforeEncoding := headerMsgT{
		msgHash: n.appHash,
		signature: sigArr,
	}
	plainMsg, err := json.Marshal(beforeEncoding)
	if err != nil {
		return *s, writeSendErrT{appHash: n.appHash, err: err}
	}
	encryptedMsg := box.Seal(
		make([]byte, 0),
		plainMsg,
		&n.newNonce,
		&n.recipients[0],
		s.secretEncrypt)
	blobHash := blake2b.Sum256(encryptedMsg)
	blobSigSlice := sign.Sign(
		make([]byte, 0),
		common.HashToSlice(blobHash),
		s.secretSign)
	var blobSigArr [common.SigSize]byte
	for i, sb := range blobSigSlice {
		blobSigArr[i] = sb
	}
	headerPreEnc := common.MetadataT{
		Blobhash: blake2b.Sum256(encryptedMsg),
		Author: s.publicSign,
		Recipient: n.recipients[0],
		Nonce: n.newNonce,
		Signature: blobSigArr,
	}
	headerEnc, err := json.Marshal(headerPreEnc)
	if err != nil {
		return *s, writeSendErrT{appHash: n.appHash, err:err}
	}
	return *s, sendMsgT{
		msg: append(common.EmptyHash, encryptedMsg...),
		header: headerEnc,
		appHash: n.appHash,
		ws: n.ws,
		f: n.f,
		recipients: n.recipients,
	}
}

// readBytesToSendT{
// 	ws: n.ws,
// 	f: n.f,
// 	lastHash: blake2b.Sum256([]byte{}),
// }


func checkReceipt(
	raw []byte,
	expectedHash [32]byte,
	sender *[32]byte) error {

	signed, validSig := sign.Open(make([]byte, 0), raw, sender)
	if !validSig {
		return errors.New("Bad signature.")
	}
	if !bytes.Equal(signed, common.MakeDigest(expectedHash, common.ReceiptCode)) {
		return errors.New("Bad receipt.")
	}
	return nil
}

func sendMsg(
	msg []byte,
	ws *websocket.Conn,
	recipient *[32]byte) error {

	err := ws.WriteMessage(websocket.BinaryMessage, msg)
	if err != nil {
		return err
	}
	msgType, response, err := ws.ReadMessage()
	if err != nil {
		return err
	}
	if msgType != websocket.BinaryMessage {
		return errors.New("Bad message type.")
	}
	return checkReceipt(response, blake2b.Sum256(msg), recipient)
}

type sendMsgT struct {
	msg []byte
	header []byte
	appHash [32]byte
	ws *websocket.Conn
	f *os.File
	recipients [][32]byte
	secretEncrypt *[32]byte
	secretSign *[64]byte
	publicSign [32]byte
}

func (s sendMsgT) send(inputCh chan inputT) {
	recipient := s.recipients[0]
	err := sendMsg(s.header, s.ws, &recipient)
	if err != nil {
		logSendErr(err, s.appHash, recipient)
		return
	}
	err = sendMsg(s.msg, s.ws, &recipient)
	if err != nil {
		logSendErr(err, s.appHash, recipient)
		return
	}
	lastChunkHash := common.HashToSlice(blake2b.Sum256(s.msg))

	for {
		fileHandle := s.f
		plainBlob := make([]byte, blobLen)
		n, err := fileHandle.Read(plainBlob)
		if err != nil {
			logSendErr(err, s.appHash, recipient)
			return
		}
		if n == 0 {
			logSentSuccess(s.appHash, recipient)
			return
		}
		nonce, err := makeNonce()
		if err != nil {
			logSendErr(err, s.appHash, recipient)
			return
		}
		encrBlob := box.Seal(
			make([]byte, 0),
			plainBlob,
			&nonce,
			&recipient,
			s.secretEncrypt)
		chunk := append(lastChunkHash, encrBlob...)
		chunkHash := blake2b.Sum256(chunk)
		lastChunkHash = common.HashToSlice(chunkHash)
		blobSigSlice := sign.Sign(
			make([]byte, 0),
			common.HashToSlice(chunkHash),
			s.secretSign)
		var blobSigArr [common.SigSize]byte
		for i, sb := range blobSigSlice {
			blobSigArr[i] = sb
		}
		header := common.MetadataT {
			Blobhash: chunkHash,
			Author: s.publicSign,
			Recipient: recipient,
			Nonce: nonce,
			Signature: blobSigArr,
		}
		headerEnc, err := json.Marshal(header)
		if err != nil {
			logSendErr(err, s.appHash, recipient)
			return
		}
		err = sendMsg(headerEnc, s.ws, &recipient)
		if err != nil {
			logSendErr(err, s.appHash, recipient)
			return
		}
		err = sendMsg(chunk, s.ws, &recipient)
		if err != nil {
			logSendErr(err, s.appHash, recipient)
			return
		}
	}
}

func processSendApp(n normalApiInputT, s *stateT) (stateT, outputT) {
	if !strEq(n.securityCode, s.homeCode) {
		return *s, sendHttpErrorT {
			w: n.w,
			msg: "Bad security code.",
			code: 400,
			doneCh: n.doneCh,
		}
	}
	var sendAppJson sendAppJsonT
	err := json.Unmarshal(n.body, &sendAppJson)
	if err != nil {
		return *s, sendHttpErrorT{
			w: n.w,
			msg: "Could not decode Json.",
			code: 400,
			doneCh: n.doneCh,
		}
	}
	if len(sendAppJson.recipients) == 0 {
		return *s, sendHttpErrorT{
			w: n.w,
			msg: "No recipients.",
			code: 400,
			doneCh: n.doneCh,
		}
	}
	filepath := docsDir + "/" + hashToStr(sendAppJson.appHash)
	return *s, makeSendHandles {
		appHash: sendAppJson.appHash,
		filepath: filepath,
		w: n.w,
		doneCh: n.doneCh,
		recipients: sendAppJson.recipients,
	}
}

func processGetApp(n normalApiInputT, s *stateT) (stateT, outputT) {
	docHash, err := getDocHash(n.securityCode, s.appCodes)
	if err != nil {
		return *s, sendHttpErrorT{
			w:      n.w,
			msg:    "Bad security code.",
			code:   400,
			doneCh: n.doneCh,
		}
	}
	return *s, serveDocT{
		w:        n.w,
		doneCh:   n.doneCh,
		filePath: docsDir + "/" + hashToStr(docHash),
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

func processMakeAppRoute(n normalApiInputT, s *stateT) (stateT, outputT) {
	var makeAppRoute makeAppRouteT
	err := json.Unmarshal(n.body, &makeAppRoute)
	if err != nil {
		return *s, sendHttpErrorT{
			w:      n.w,
			msg:    err.Error(),
			code:   400,
			doneCh: n.doneCh,
		}
	}
	if !strEq(n.securityCode, s.homeCode) {
		return *s, sendHttpErrorT{
			w:      n.w,
			msg:    "Bad security code.",
			code:   400,
			doneCh: n.doneCh,
		}
	}
	return *s, genCodeForAppT{
		w:       n.w,
		appHash: makeAppRoute.apphash,
		doneCh:  n.doneCh,
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
	return *s, readHttpInputT{s.httpChan}
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

func initState() (stateT, error) {
	homeCode, err := genCode()
	if err != nil {
		return stateT{}, err
	}
	return stateT{
		httpChan: make(chan httpInputT),
		homeCode: homeCode,
	}, nil
}

func main() {
	var state stateT
	// err := readFileData(&state)
	// if err != nil {
	// 	return
	// }
	go httpServer(state.httpChan)
	var input inputT = noInputT{}
	var output outputT = readHttpInputT{ch: state.httpChan}
	var inputCh chan inputT
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

func staticFileHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method == "GET" {
		f := http.FileServer(http.Dir("/home/t/bigwebthing"))
		f.ServeHTTP(w, r)
	}
}

type handlerT func(http.ResponseWriter, *http.Request)

type endRequest struct{}

func handler(route string, inputChan chan httpInputT) handlerT {
	return func(w http.ResponseWriter, r *http.Request) {
		var doneCh chan endRequest
		inputChan <- httpInputT{
			w:      w,
			r:      r,
			route:  route,
			doneCh: doneCh,
		}
		<-doneCh
	}
}

var routes = []string{"makeapproute", "getapp"}

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

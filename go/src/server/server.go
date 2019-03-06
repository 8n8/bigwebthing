package main

import (
	"encoding/hex"
	"encoding/json"
	"errors"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/nacl/sign"
	"net/http"
	"os"
	"github.com/gorilla/websocket"
)

type stateT struct {
	fatalErr      error
	httpInputChan chan httpInputT
	invitations   invitesT
	uninvitations invitesT
	memberList    map[[32]byte]bool
	expectedBlobs map[[32]byte]bool
}

type invitesT map[invitationT]bool

type uninviteeT struct {
	pubkey [32]byte
}

type inviteeT struct {
	pubkey [32]byte
}

type authorT struct {
	pubkey [32]byte
}

func initState() stateT {
	return stateT{
		fatalErr: nil,
	}
}

type outputT interface {
	send() inputT
}

type getHttpInputs struct {
	inputChan chan httpInputT
}

func (g getHttpInputs) send() inputT {
	return <-g.inputChan
}

type httpError struct {
	err     error
	errChan chan error
}

func (e httpError) send() inputT {
	e.errChan <- e.err
	return noInput{}
}

type sendHttpResponse struct {
	response     []byte
	responseChan chan []byte
}

type inputT interface {
	update(stateT) (stateT, outputT)
}

func main() {
	httpInputChan := make(chan httpInputT)
	go httpServer(httpInputChan)
	var input inputT = noInput{}
	var output outputT = getHttpInputs{inputChan: httpInputChan}
	state := initState()
	for state.fatalErr == nil {
		input = output.send()
		state, output = input.update(state)
	}
}

const (
	blob byte = 0x00
	invite byte = 0x01
	uninvite byte = 0x02
	metadata byte = 0x03
)

type httpInputT struct {
	body       []byte
	returnChan chan []byte
	errChan    chan error
}

func (h httpInputT) update(s stateT) (stateT, outputT) {
	switch h.body[0] {
	case invite:
		return processInvite(h, s)
	case uninvite:
		return processUninvite(h, s)
	case metadata:
		return processMetadata(h, s)
	case blob:
		return processBlob(h, s)
	}
	err := errors.New("Invalid message type.")
	return s, httpError{err: err, errChan: h.errChan}
}

func processBlob(h httpInputT, s stateT) (stateT, outputT) {
	if len(h.body) != 16032 {
		err := errors.New("Body not 16032 bytes long.")
		return s, httpError{err: err, errChan: h.errChan}
	}
	blobHash := blake2b.Sum256(h.body)
	_, blobExpected := s.expectedBlobs[blobHash]
	if !blobExpected {
		err := errors.New("Blob not expected.")
		return s, httpError{err: err, errChan: h.errChan}
	}
	filename := hex.EncodeToString(hashToSlice(blobHash))
	writeToFile := writeNewFileT{
		filepath:   chunkDir + "/" + filename,
		contents:   h.body,
		returnChan: h.returnChan,
		errChan:    h.errChan,
	}
	return s, writeToFile
}

func parseInviteLike(
	raw []byte,
	memberList map[[32]byte]bool,
	meaningCode [15]byte) (invitationT, error) {

	var uninvitation invitationT
	jsonErr := json.Unmarshal(raw, &uninvitation)
	if jsonErr != nil {
		return uninvitation, jsonErr
	}
	validSignature := verifyDetached(
		concatInviteAndCodes(
			uninvitation.invitee,
			uninvitation.uniqueID,
			meaningCode),
		uninvitation.signature,
		uninvitation.author)
	if !validSignature {
		err := errors.New("Could not verify signature.")
		return uninvitation, err
	}
	_, authorIsMember := memberList[uninvitation.author]
	if !authorIsMember {
		errStr := "The author is not a current member."
		return uninvitation, errors.New(errStr)
	}
	if uninvitation.author == uninvitation.invitee {
		errStr := "You can't uninvite yourself."
		return uninvitation, errors.New(errStr)
	}
	return uninvitation, nil
}

func processUninvite(h httpInputT, s stateT) (stateT, outputT) {
	uninvitation, err := parseInviteLike(
		h.body,
		s.memberList,
		pleaseUninviteX)
	if err != nil {
		return s, httpError{err: err, errChan: h.errChan}
	}
	newUninvites := s.uninvitations
	newUninvites[uninvitation] = true
	newState := stateT{
		fatalErr:      s.fatalErr,
		httpInputChan: s.httpInputChan,
		invitations:   s.invitations,
		uninvitations: newUninvites,
		memberList: makeMemberList(
			s.invitations,
			newUninvites),
	}
	goodUninvite := appendToFileT{
		filepath:   uninvitesFilePath,
		bytesToAdd: h.body,
		returnChan: h.returnChan,
		errChan:    h.errChan,
	}
	return newState, goodUninvite
}

type invitationT struct {
	author    [32]byte
	invitee   [32]byte
	signature [sigSize]byte
	uniqueID  [15]byte
}

const sigSize = sign.Overhead + blake2b.Size256

func hashToSlice(hash [32]byte) []byte {
	newHash := make([]byte, 32)
	for i, el := range hash {
		newHash[i] = el
	}
	return newHash
}

func signMsg(msg []byte, sKey *[64]byte) [sigSize]byte {
	hash := hashToSlice(blake2b.Sum256(msg))
	sig := sign.Sign(make([]byte, 0), hash, sKey)
	var sig96 [sigSize]byte
	for i, el := range sig {
		sig96[i] = el
	}
	return sig96
}

func convertSig(sig [sigSize]byte) []byte {
	newSig := make([]byte, sigSize)
	for i, el := range sig {
		newSig[i] = el
	}
	return newSig
}

var pleaseUninviteX = [15]byte{
	0x6e, 0xb9, 0xb4, 0x6f, 0xdc, 0x87, 0xf6, 0xbc,
	0xcf, 0xdf, 0x44, 0x22, 0xea, 0x78, 0xf4}

var pleaseInviteX = [15]byte{
	0xe8, 0xbf, 0x93, 0xd8, 0x39, 0xd3, 0x34, 0xe5,
	0xc0, 0x1f, 0xff, 0x2b, 0xc4, 0x30, 0xc4}

func concatInviteAndCodes(
	invite [32]byte,
	uniqueID [15]byte,
	meaningCode [15]byte) []byte {
	result := make([]byte, 62)
	i := 0
	for i < 32 {
		result[i] = invite[i]
		i++
	}
	for i < 47 {
		result[i] = uniqueID[i-32]
		i++
	}
	for i < 62 {
		result[i] = meaningCode[i-47]
		i++
	}
	return result
}

func verifyDetached(
	msg []byte,
	sig [sigSize]byte,
	author [32]byte) bool {

	actualHash, validSignature := sign.Open(
		make([]byte, 0),
		convertSig(sig),
		&author)
	if !validSignature {
		return false
	}
	if len(actualHash) != blake2b.Size256 {
		return false
	}
	var actualHash32 [32]byte
	for i, onebyte := range actualHash {
		actualHash32[i] = onebyte
	}
	expectedHash := blake2b.Sum256(msg)
	return expectedHash == actualHash32
}

var truesPubSign = [32]byte{
	0x22, 0x76, 0xf1, 0x1b, 0x62, 0xe6, 0x37, 0x55, 0x01, 0x24,
	0xa3, 0x68, 0x06, 0x20, 0xbb, 0x34, 0x4f, 0xcb, 0x7d, 0xe2,
	0xdc, 0x19, 0x6d, 0xa0, 0x98, 0x59, 0x12, 0xda, 0x54, 0x99,
	0xf1, 0x5e}

var truesPubEncrypt = [32]byte{
	0x0e, 0x62, 0x16, 0x83, 0x6f, 0x6e, 0xd8, 0xb3, 0x1b, 0x68,
	0x52, 0xec, 0xc1, 0xef, 0x70, 0x01, 0xaa, 0xcd, 0xdc, 0xba,
	0x3b, 0xe4, 0xcb, 0x81, 0x34, 0xfc, 0xa9, 0xa3, 0x0c, 0x3d,
	0x82, 0xa0}

func authorAndInviteeEqual(a invitationT, b invitationT) bool {
	return a.author == b.author && a.invitee == b.invitee
}

func countSimilarInvites(i invitationT, invites invitesT) int {
	counter := 0
	for invite, _ := range invites {
		if authorAndInviteeEqual(i, invite) {
			counter += 1
		}
	}
	return counter
}

func makeMemberList(i invitesT, u invitesT) map[[32]byte]bool {
	var m map[[32]byte]bool
	lenm := 0
	m[truesPubSign] = true
	for len(m) > lenm {
		for invite, _ := range i {
			_, authorIsMember := m[invite.author]
			if !authorIsMember {
				continue
			}
			icount := countSimilarInvites(invite, i)
			ucount := countSimilarInvites(invite, u)
			if ucount >= icount {
				continue
			}
			m[invite.invitee] = true
			lenm++
		}
	}
	return m
}

type appendToFileT struct {
	filepath   string
	bytesToAdd []byte
	returnChan chan []byte
	errChan    chan error
}

type writeNewFileT struct {
	filepath   string
	contents   []byte
	returnChan chan []byte
	errChan    chan error
}

func (w writeNewFileT) send() inputT {
	flags := os.O_CREATE | os.O_WRONLY
	f, openErr := os.OpenFile(w.filepath, flags, 0400)
	if openErr != nil {
		w.errChan <- openErr
		return noInput{}
	}
	defer f.Close()
	_, writeErr := f.Write(w.contents)
	if writeErr != nil {
		w.errChan <- writeErr
		return noInput{}
	}
	w.returnChan <- []byte("ok")
	return noInput{}
}

func (g appendToFileT) send() inputT {
	flags := os.O_APPEND | os.O_CREATE | os.O_WRONLY
	f, openErr := os.OpenFile(g.filepath, flags, 0600)
	if openErr != nil {
		g.errChan <- openErr
		return noInput{}
	}
	defer f.Close()
	_, writeErr := f.Write(append([]byte("\n"), g.bytesToAdd...))
	if writeErr != nil {
		g.errChan <- writeErr
		return noInput{}
	}
	g.returnChan <- []byte("ok")
	return noInput{}
}

const (
	serverDir         = "/home/t/bigwebthing/serverData"
	invitesFilePath   = serverDir + "/invites.txt"
	uninvitesFilePath = serverDir + "/uninvites.txt"
	metadataFilePath  = serverDir + "/metadata.txt"
	chunkDir          = serverDir + "/chunks"
)

func processInvite(h httpInputT, s stateT) (stateT, outputT) {
	invitation, err := parseInviteLike(
		h.body,
		s.memberList,
		pleaseInviteX)
	if err != nil {
		return s, httpError{err: err, errChan: h.errChan}
	}
	newInvites := s.invitations
	newInvites[invitation] = true
	newState := stateT{
		fatalErr:      s.fatalErr,
		httpInputChan: s.httpInputChan,
		invitations:   newInvites,
		uninvitations: s.uninvitations,
		memberList: makeMemberList(
			newInvites,
			s.uninvitations),
	}
	goodInvite := appendToFileT{
		filepath:   invitesFilePath,
		bytesToAdd: h.body,
		returnChan: h.returnChan,
		errChan:    h.errChan,
	}
	return newState, goodInvite
}

const encSymKeyLen = secretbox.Overhead + 32

type metadataT struct {
	chunkHeadHash         [32]byte
	author                [32]byte
	recipient             [32]byte
	nonceBase             [20]byte
	encryptedSymmetricKey [encSymKeyLen]byte
	signature             [sigSize]byte
}

const concatMdLen = 32 + 32 + 20 + encSymKeyLen

func concatMd(m metadataT) []byte {
	result := make([]byte, concatMdLen)
	i := 0
	for i < 32 {
		result[i] = m.chunkHeadHash[i]
		i++
	}
	for i < 64 {
		result[i] = m.recipient[i-32]
		i++
	}
	for i < 84 {
		result[i] = m.nonceBase[i-52]
		i++
	}
	for i < 84+encSymKeyLen {
		result[i] = m.encryptedSymmetricKey[i-encSymKeyLen-52]
		i++
	}
	return result
}

func parseMetadata(
	raw []byte,
	memberList map[[32]byte]bool) (metadataT, error) {

	var metadata metadataT
	jsonErr := json.Unmarshal(raw, &metadata)
	if jsonErr != nil {
		return metadataT{}, jsonErr
	}
	validSignature := verifyDetached(
		concatMd(metadata),
		metadata.signature,
		metadata.author)
	if !validSignature {
		err := errors.New("Could not verify signature.")
		return metadataT{}, err
	}
	_, authorIsMember := memberList[metadata.author]
	if !authorIsMember {
		errStr := "The author is not a member."
		return metadataT{}, errors.New(errStr)
	}
	_, recipientIsMember := memberList[metadata.recipient]
	if !recipientIsMember {
		errStr := "The recipient is not a member."
		return metadataT{}, errors.New(errStr)
	}
	if metadata.author == metadata.recipient {
		errStr := "You can't send messages to yourself."
		return metadataT{}, errors.New(errStr)
	}
	return metadata, nil
}

func processMetadata(h httpInputT, s stateT) (stateT, outputT) {
	metadata, err := parseMetadata(h.body, s.memberList)
	if err != nil {
		return s, httpError{err: err, errChan: h.errChan}
	}
	newExpectedBlobs := s.expectedBlobs
	newExpectedBlobs[metadata.chunkHeadHash] = true
	newState := stateT{
		fatalErr:      s.fatalErr,
		httpInputChan: s.httpInputChan,
		invitations:   s.invitations,
		uninvitations: s.uninvitations,
		memberList:    s.memberList,
		expectedBlobs: newExpectedBlobs,
	}
	goodMd := appendToFileT{
		filepath:   metadataFilePath,
		bytesToAdd: h.body,
		returnChan: h.returnChan,
		errChan:    h.errChan,
	}
	return newState, goodMd
}

type noInput struct{}

func (n noInput) update(s stateT) (stateT, outputT) {
	return s, getHttpInputs{inputChan: s.httpInputChan}
}

type handlerT = func(http.ResponseWriter, *http.Request)

// func makeHandler(ch chan httpInputT, route httpMsgType) handler {
// 	return func(w http.ResponseWriter, r *http.Request) {
// 		bodyBytes, err := ioutil.ReadAll(r.Body)
// 		if err != nil {
// 			return
// 		}
// 		returnChan := make(chan []byte)
// 		errChan := make(chan error)
// 		ch <- httpInputT{
// 			typeOf:     route,
// 			body:       bodyBytes,
// 			returnChan: returnChan,
// 			errChan:    errChan,
// 		}
// 		select {
// 		case <-errChan:
// 			w.WriteHeader(http.StatusInternalServerError)
// 		case response := <-returnChan:
// 			w.Write(response)
// 		}
// 	}
// }


var upgrader = websocket.Upgrader{
	ReadBufferSize: 1024,
	WriteBufferSize: 1024,
}

func handler(
	ch chan httpInputT,
	w http.ResponseWriter,
	r *http.Request) {

	ws, wsErr := upgrader.Upgrade(w, r, nil)
	if wsErr != nil {
		return
	}
	returnChan := make(chan []byte)
	for {
		msgType, rawMsg, readErr := ws.ReadMessage()
		if readErr != nil {
			return
		}
		if msgType != websocket.BinaryMessage {
			return
		}
		ch <- httpInputT{
			body: rawMsg,
			returnChan: returnChan,
		}
		select {
		case returnMsg := <-returnChan:
			sendErr := ws.WriteMessage(
				websocket.BinaryMessage, returnMsg)
			if sendErr != nil {
				return
			}
		default:
		}
	}
}

func makeHandler(ch chan httpInputT) handlerT {
	return func(w http.ResponseWriter, r *http.Request) {
		handler(ch, w, r)
	}
}

func httpServer(ch chan httpInputT) {
	http.HandleFunc("/", makeHandler(ch))
	// http.HandleFunc("/blob", makeHandler(ch, blob))
	// http.HandleFunc("/invite", makeHandler(ch, invite))
	// http.HandleFunc("/uninvite", makeHandler(ch, uninvite))
	// http.HandleFunc("/metadata", makeHandler(ch, metadata))
	http.ListenAndServe(":4000", nil)
}

package main

import (
	"encoding/json"
	"errors"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/sign"
	"io/ioutil"
	"net/http"
	"os"
)

type stateT struct {
	fatalErr      error
	httpInputChan chan httpInputT
	invitations   invitesT
	uninvitations invitesT
	memberList    map[[32]byte]bool
	authCodes     map[[15]byte]int
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

type httpMsgType int

const (
	blob httpMsgType = iota + 1
	invite
	uninvite
	metadata
)

type httpInputT struct {
	typeOf     httpMsgType
	body       []byte
	returnChan chan []byte
	errChan    chan error
}

func (h httpInputT) update(s stateT) (stateT, outputT) {
	switch h.typeOf {
	case invite:
		return processInvite(h, s)
	case uninvite:
		return processUninvite(h, s)
		// case metadata:
		// 	return processMetadata(h, s)
		// case blob:
		// 	return processBlob(h, s)
	}
	err := errors.New("Invalid message type.")
	return s, httpError{err: err, errChan: h.errChan}
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

var truesPubKey = [32]byte{
	0x22, 0x76, 0xf1, 0x1b, 0x62, 0xe6, 0x37, 0x55, 0x01, 0x24,
	0xa3, 0x68, 0x06, 0x20, 0xbb, 0x34, 0x4f, 0xcb, 0x7d, 0xe2,
	0xdc, 0x19, 0x6d, 0xa0, 0x98, 0x59, 0x12, 0xda, 0x54, 0x99,
	0xf1, 0x5e}

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
	m[truesPubKey] = true
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

func (g appendToFileT) send() inputT {
	flags := os.O_APPEND | os.O_CREATE | os.O_WRONLY
	f, openErr := os.OpenFile(g.filepath, flags, 0600)
	if openErr != nil {
		g.errChan <- openErr
	}
	defer f.Close()
	_, writeErr := f.Write(append([]byte("\n"), g.bytesToAdd...))
	if writeErr != nil {
		g.errChan <- writeErr
	}
	g.returnChan <- []byte("ok")
	return noInput{}
}

const (
	serverDir         = "/home/t/bigwebthing/serverData"
	invitesFilePath   = serverDir + "/invites.txt"
	uninvitesFilePath = serverDir + "/uninvites.txt"
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

type noInput struct{}

func (n noInput) update(s stateT) (stateT, outputT) {
	return s, getHttpInputs{inputChan: s.httpInputChan}
}

type handler = func(http.ResponseWriter, *http.Request)

func makeHandler(ch chan httpInputT, route httpMsgType) handler {
	return func(w http.ResponseWriter, r *http.Request) {
		bodyBytes, err := ioutil.ReadAll(r.Body)
		if err != nil {
			return
		}
		returnChan := make(chan []byte)
		errChan := make(chan error)
		ch <- httpInputT{
			typeOf:     route,
			body:       bodyBytes,
			returnChan: returnChan,
			errChan:    errChan,
		}
		select {
		case <-errChan:
			w.WriteHeader(http.StatusInternalServerError)
		case response := <-returnChan:
			w.Write(response)
		}
	}
}

func httpServer(ch chan httpInputT) {
	http.HandleFunc("/blob", makeHandler(ch, blob))
	http.HandleFunc("/invite", makeHandler(ch, invite))
	http.HandleFunc("/uninvite", makeHandler(ch, uninvite))
	http.HandleFunc("/metadata", makeHandler(ch, metadata))
	http.ListenAndServe(":4000", nil)
}

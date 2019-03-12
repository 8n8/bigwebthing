package main

import (
	"bytes"
	"crypto/rand"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/gorilla/websocket"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/nacl/sign"
	"io/ioutil"
	"net/http"
	"os"
	"time"
)

type stateT struct {
	mainChans      mainChansT
	connectedUsers map[[32]byte]userChansT
	invitations    invitesT
	uninvitations  invitesT
	memberList     map[[32]byte]bool
	expectedBlobs  map[[32]byte][32]byte
	authCodes      authCodesT
}

type authCodesT map[[authCodeLength]byte]int64

type invitesT map[invitationT]bool

func initState() stateT {
	return stateT{
		mainChans: mainChansT{
			getAuthCode:     make(chan authCodeChans),
			setupConnection: make(chan setupConnectionT),
		},
		connectedUsers: make(map[[32]byte]userChansT),
		invitations:    make(map[invitationT]bool),
		uninvitations:  make(map[invitationT]bool),
		memberList:     make(map[[32]byte]bool),
		expectedBlobs:  make(map[[32]byte][32]byte),
		authCodes:      make(map[[authCodeLength]byte]int64),
	}
}

type outputT interface {
	send() inputT
}

const (
	responseNoAuthCode            byte = 0x00
	responseAuthCode              byte = 0x01
	responseRecipientNotConnected byte = 0x02
)

type inputT interface {
	update(*stateT) (stateT, outputT)
}

type readChansT struct {
	chs            mainChansT
	connectedUsers map[[32]byte]userChansT
}

func (r readChansT) send() inputT {
	select {
	case authChans := <-r.chs.getAuthCode:
		authCode, err := genAuthCode()
		if err != nil {
			authChans.err <- err
			return noInputT{}
		}
		authChans.main <- authCode
		return noInputT{}
	case newConnection := <-r.chs.setupConnection:
		return newConnection
	default:
		for author, userChans := range r.connectedUsers {
			select {
			case msg := <-userChans.in:
				return userMsgT{
					author:  author,
					msg:     msg,
					outChan: userChans.out,
					errChan: userChans.err,
				}
			default:
			}
		}
		return noInputT{}
	}
}

type userMsgT struct {
	author  [32]byte
	msg     httpInputT
	outChan chan []byte
	errChan chan error
}

const (
	inInvitation   byte = 0x00
	inUninvitation byte = 0x01
	inMetadata     byte = 0x02
	inBlob         byte = 0x03
)

func (u userMsgT) update(s *stateT) (stateT, outputT) {
	switch u.msg.route {
	case inInvitation:
		return processInvitation(u, s)
	case inUninvitation:
		return processUninvitation(u, s)
	case inMetadata:
		return processMetadata(u, s)
	case inBlob:
		return processBlob(u, s)
	}
	err := errors.New("Bad router byte.")
	return *s, sendErrT{err: err, ch: u.errChan}
}

func removeBlobHash(s *stateT, blobHash [32]byte) stateT {
	var newExpectedBlobs map[[32]byte][32]byte
	for hash, recipient := range s.expectedBlobs {
		newExpectedBlobs[hash] = recipient
	}
	delete(newExpectedBlobs, blobHash)
	newState := *s
	newState.expectedBlobs = newExpectedBlobs
	return newState
}

func processBlob(umsg userMsgT, s *stateT) (stateT, outputT) {
	hash := blake2b.Sum256(umsg.msg.body)
	recipient, blobExpected := s.expectedBlobs[hash]
	if !blobExpected {
		return *s, sendErrT{
			err: errors.New("Blob not expected."),
			ch:  umsg.errChan}
	}

	recipientChans, recipientOnline := s.connectedUsers[recipient]
	if !recipientOnline {
		return *s, sendErrT{
			err: errors.New("Recipient offline."),
			ch:  umsg.errChan}
	}

	newState := removeBlobHash(s, hash)
	output := sendMsgT{
		msg: umsg.msg.body,
		ch:  recipientChans.out}
	return newState, output
}

type metadataT struct {
	blobHash              [32]byte
	author                [32]byte
	recipient             [32]byte
	encryptedSymmetricKey [encSymKeyLen]byte
	signature             [sigSize]byte
}

func addBlobHash(
	s *stateT,
	blobHash [32]byte,
	recipient [32]byte) stateT {

	var newExpectedBlobs map[[32]byte][32]byte
	for hash, recipient := range s.expectedBlobs {
		newExpectedBlobs[hash] = recipient
	}
	newExpectedBlobs[blobHash] = recipient
	newState := *s
	newState.expectedBlobs = newExpectedBlobs
	return newState
}

func processMetadata(umsg userMsgT, s *stateT) (stateT, outputT) {
	md, err := parseMetadata(umsg.msg.body, s.memberList)
	if err != nil {
		return *s, sendErrT{err: err, ch: umsg.errChan}
	}

	recipientChans, recipientOnline := s.connectedUsers[md.recipient]
	if !recipientOnline {
		return *s, sendMsgT{
			msg: []byte{responseRecipientNotConnected},
			ch:  umsg.outChan,
		}
	}

	newState := addBlobHash(s, md.blobHash, md.recipient)
	output := sendMsgT{
		msg: umsg.msg.body,
		ch:  recipientChans.out,
	}
	return newState, output
}

func addUninvite(s *stateT, uninvitation invitationT) stateT {
	var newUninvites map[invitationT]bool
	for u, _ := range s.uninvitations {
		newUninvites[u] = true
	}
	newUninvites[uninvitation] = true
	newState := *s
	newState.uninvitations = newUninvites
	return newState
}

func processUninvitation(umsg userMsgT, s *stateT) (stateT, outputT) {
	uninvitation, err := parseInviteLike(
		umsg.msg.body,
		s.memberList,
		pleaseUninviteX)
	if err != nil {
		return *s, sendErrT{err: err, ch: umsg.errChan}
	}
	newState := addUninvite(s, uninvitation)
	goodUninvite := appendToFileT{
		filepath:   uninvitesFilePath,
		bytesToAdd: umsg.msg.body,
		errChan:    umsg.errChan,
	}
	return newState, goodUninvite
}

func addInvite(s *stateT, invitation invitationT) stateT {
	var newInvites map[invitationT]bool
	for i, _ := range s.invitations {
		newInvites[i] = true
	}
	newInvites[invitation] = true
	newState := *s
	newState.invitations = newInvites
	return newState
}

func processInvitation(umsg userMsgT, s *stateT) (stateT, outputT) {
	invitation, err := parseInviteLike(
		umsg.msg.body,
		s.memberList,
		pleaseInviteX)
	if err != nil {
		return *s, sendErrT{err: err, ch: umsg.errChan}
	}
	newState := addInvite(s, invitation)
	goodInvite := appendToFileT{
		filepath:   invitesFilePath,
		bytesToAdd: umsg.msg.body,
		errChan:    umsg.errChan,
	}
	return newState, goodInvite
}

func (g appendToFileT) send() inputT {
	flags := os.O_APPEND | os.O_CREATE | os.O_WRONLY
	f, openErr := os.OpenFile(g.filepath, flags, 0600)
	if openErr != nil {
		g.errChan <- openErr
		return noInputT{}
	}
	defer f.Close()
	_, writeErr := f.Write(append([]byte("\n"), g.bytesToAdd...))
	if writeErr != nil {
		g.errChan <- writeErr
		return noInputT{}
	}
	return noInputT{}
}

type noInputT struct{}

func readChans(s *stateT) readChansT {
	return readChansT{
		chs:            s.mainChans,
		connectedUsers: s.connectedUsers,
	}
}

func (n noInputT) update(s *stateT) (stateT, outputT) {
	return *s, readChans(s)
}

func signedAuthToSlice(bs [signedAuthSize]byte) []byte {
	slice := make([]byte, signedAuthSize)
	for i, b := range bs {
		slice[i] = b
	}
	return slice
}

type sendMsgT struct {
	msg []byte
	ch  chan []byte
}

func (s sendMsgT) send() inputT {
	s.ch <- s.msg
	return noInputT{}
}

type sendErrT struct {
	err error
	ch  chan error
}

func (s sendErrT) send() inputT {
	s.ch <- s.err
	return noInputT{}
}

func authSliceToArray(authSlice []byte) [authCodeLength]byte {
	var arr [authCodeLength]byte
	for i, b := range authSlice {
		arr[i] = b
	}
	return arr
}

func addUserConn(s *stateT, user [32]byte, chans userChansT) stateT {
	var newConnUsers map[[32]byte]userChansT
	for u, ch := range s.connectedUsers {
		newConnUsers[u] = ch
	}
	newConnUsers[user] = chans
	newState := *s
	newState.connectedUsers = newConnUsers
	return newState
}

func validSetupConn(c setupConnectionT, authCodes authCodesT) error {
	authCode, validSignature := sign.Open(
		make([]byte, 0),
		signedAuthToSlice(c.signedAuthCode),
		&c.author)
	if !validSignature {
		return errors.New("Bad signature.")
	}
	if !authOk(authCode, authCodes, c.posixTime) {
		return errors.New("Bad auth code.")
	}
	return nil
}

func (c setupConnectionT) update(s *stateT) (stateT, outputT) {
	err := validSetupConn(c, s.authCodes)
	if err != nil {
		return *s, sendErrT{err: err, ch: c.errChan}
	}
	return addUserConn(s, c.author, c.chans), readChans(s)
}

func parseInvites(rawBytes []byte) (invitesT, error) {
	var invites invitesT
	lines := bytes.Split(rawBytes, []byte("\n"))
	var invite invitationT
	for _, line := range lines {
		jsonErr := json.Unmarshal(line, &invite)
		if jsonErr != nil {
			return invites, jsonErr
		}
		invites[invite] = true
	}
	return invites, nil
}

func readInvites(filePath string) (invitesT, error) {
	var invites invitesT
	f, openErr := os.Open(filePath)
	if openErr != nil {
		return invites, openErr
	}
	defer f.Close()

	rawContents, readErr := ioutil.ReadAll(f)
	if readErr != nil {
		return invites, readErr
	}

	return parseInvites(rawContents)
}

func readFileData(s *stateT) error {
	var err error
	s.invitations, err = readInvites(invitesFilePath)
	if err != nil {
		fmt.Print(err.Error())
		return err
	}
	s.uninvitations, err = readInvites(uninvitesFilePath)
	if err != nil {
		fmt.Print(err.Error())
		return err
	}
	return nil
}

func main() {
	state := initState()
	err := readFileData(&state)
	if err != nil {
		return
	}
	go httpServer(state.mainChans)
	var input inputT = noInputT{}
	var output outputT = readChans(&state)
	for {
		input = output.send()
		state, output = input.update(&state)
	}
}

type httpInputT struct {
	route byte
	body  []byte
}

func authOk(
	authSlice []byte,
	authCodes map[[authCodeLength]byte]int64,
	posixTime int64) bool {

	authCode := authSliceToArray(authSlice)
	authTime, authExists := authCodes[authCode]
	authAge := posixTime - authTime
	return authExists && (authAge <= 30) && (authAge >= 0)
}

type userChansT struct {
	in  chan httpInputT
	out chan []byte
	err chan error
}

func genAuthCode() ([authCodeLength]byte, error) {
	authSlice := make([]byte, authCodeLength)
	_, err := rand.Read(authSlice)
	var authCode [authCodeLength]byte
	if err != nil {
		return authCode, err
	}
	for i, b := range authSlice {
		authCode[i] = b
	}
	return authCode, nil
}

func inviteErr(
	invitation invitationT,
	memberList map[[32]byte]bool,
	meaningCode [15]byte) error {

	validSignature := verifyDetached(
		concatInviteAndCodes(
			invitation.invitee,
			invitation.uniqueID,
			meaningCode),
		invitation.signature,
		invitation.author)
	if !validSignature {
		return errors.New("Could not verify signature.")
	}

	_, authorIsMember := memberList[invitation.author]
	if !authorIsMember {
		return errors.New("The author is not a member.")
	}

	if invitation.author == invitation.invitee {
		return errors.New("You can't (un)invite yourself.")
	}
	return nil
}

func parseInviteLike(
	raw []byte,
	memberList map[[32]byte]bool,
	meaningCode [15]byte) (invitationT, error) {

	var invitation invitationT
	jsonErr := json.Unmarshal(raw, &invitation)
	if jsonErr != nil {
		return invitation, jsonErr
	}

	inviteErr := inviteErr(invitation, memberList, meaningCode)
	return invitation, inviteErr
}

type invitationT struct {
	author    [32]byte
	invitee   [32]byte
	signature [sigSize]byte
	uniqueID  [15]byte
}

const sigSize = sign.Overhead + blake2b.Size256
const signedAuthSize = sign.Overhead + authCodeLength

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
	errChan    chan error
}

const (
	serverDir         = "/home/t/bigwebthing/serverData"
	invitesFilePath   = serverDir + "/invites.txt"
	uninvitesFilePath = serverDir + "/uninvites.txt"
	metadataFilePath  = serverDir + "/metadata.txt"
	chunkDir          = serverDir + "/chunks"
)

const encSymKeyLen = secretbox.Overhead + 32

const concatMdLen = 32 + 32 + encSymKeyLen

func concatMd(m metadataT) []byte {
	result := make([]byte, concatMdLen)
	i := 0
	for i < 32 {
		result[i] = m.blobHash[i]
		i++
	}
	for i < 64 {
		result[i] = m.recipient[i-32]
		i++
	}
	for i < 64+encSymKeyLen {
		result[i] = m.encryptedSymmetricKey[i-encSymKeyLen-32]
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
		err := errors.New("Bad signature.")
		return metadataT{}, err
	}
	_, authorIsMember := memberList[metadata.author]
	if !authorIsMember {
		errStr := "Author is not a member."
		return metadataT{}, errors.New(errStr)
	}
	_, recipientIsMember := memberList[metadata.recipient]
	if !recipientIsMember {
		errStr := "Recipient is not a member."
		return metadataT{}, errors.New(errStr)
	}
	if metadata.author == metadata.recipient {
		errStr := "You can't send messages to yourself."
		return metadataT{}, errors.New(errStr)
	}
	return metadata, nil
}

type handlerT = func(http.ResponseWriter, *http.Request)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func readWebsocket(
	ws *websocket.Conn,
	inputChan chan httpInputT) {

	for {
		msgType, msgReader, readErr := ws.NextReader()
		if readErr != nil {
			return
		}
		if msgType != websocket.BinaryMessage {
			return
		}
		rawMsg := make([]byte, 16000)
		lenMsg, err := msgReader.Read(rawMsg)
		if lenMsg == 0 {
			return
		}
		if err != nil {
			return
		}
		inputChan <- httpInputT{
			route: rawMsg[0],
			body:  rawMsg[1:lenMsg],
		}
	}
}

func authToSlice(bs [authCodeLength]byte) []byte {
	slice := make([]byte, authCodeLength)
	for i, b := range bs {
		slice[i] = b
	}
	return slice
}

func handler(
	mainChans mainChansT,
	w http.ResponseWriter,
	r *http.Request) {

	ws, wsErr := upgrader.Upgrade(w, r, nil)
	if wsErr != nil {
		return
	}
	authChans := authCodeChans{
		main: make(chan [authCodeLength]byte),
		err:  make(chan error),
	}
	mainChans.getAuthCode <- authChans
	select {
	case err := <-authChans.err:
		sendErr := ws.WriteMessage(
			websocket.BinaryMessage,
			append(
				[]byte{responseNoAuthCode},
				[]byte(err.Error())...))
		if sendErr != nil {
			return
		}
	case authCode := <-authChans.main:
		sendErr := ws.WriteMessage(
			websocket.BinaryMessage,
			append(
				[]byte{responseAuthCode},
				authToSlice(authCode)...))
		if sendErr != nil {
			return
		}
	}
	msgType, rawMsg, readErr := ws.ReadMessage()
	if readErr != nil {
		return
	}
	if msgType != websocket.BinaryMessage {
		return
	}
	var auth authenticatorT
	jsonErr := json.Unmarshal(rawMsg, &auth)
	if jsonErr != nil {
		return
	}
	errChan := make(chan error)
	setup := setupConnectionT{
		chans: userChansT{
			in:  make(chan httpInputT),
			out: make(chan []byte),
		},
		author:         auth.author,
		signedAuthCode: auth.signedAuthCode,
		errChan:        errChan,
		posixTime:      time.Now().Unix(),
	}
	mainChans.setupConnection <- setup
	err := <-errChan
	if err != nil {
		return
	}

	go readWebsocket(ws, setup.chans.in)
	for {
		msgOut := <-setup.chans.out
		sendErr := ws.WriteMessage(
			websocket.BinaryMessage, msgOut)
		if sendErr != nil {
			return
		}
	}
}

const authCodeLength = 24

type setupConnectionT struct {
	chans          userChansT
	author         [32]byte
	signedAuthCode [signedAuthSize]byte
	errChan        chan error
	posixTime      int64
}

type authenticatorT struct {
	signedAuthCode [signedAuthSize]byte
	author         [32]byte
}

type authCodeChans struct {
	main chan [authCodeLength]byte
	err  chan error
}

type mainChansT struct {
	getAuthCode     chan authCodeChans
	setupConnection chan setupConnectionT
}

func makeHandler(ch mainChansT) handlerT {
	return func(w http.ResponseWriter, r *http.Request) {
		handler(ch, w, r)
	}
}

func httpServer(ch mainChansT) {
	http.HandleFunc("/", makeHandler(ch))
	http.ListenAndServe(":4000", nil)
}

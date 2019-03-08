package main

import (
	"crypto/rand"
	"encoding/json"
	"errors"
	"github.com/gorilla/websocket"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/nacl/sign"
	"net/http"
	"time"
)

type stateT struct {
	fatalErr       error
	mainChans      mainChansT
	connectedUsers map[[32]byte]userStateT
	invitations    invitesT
	uninvitations  invitesT
	memberList     map[[32]byte]bool
	expectedBlobs  map[[32]byte]bool
	authCodes      map[[authCodeLength]byte]int64
}

type userStateT struct {
	chans        userChansT
	blobExpected bool
	expectedBlob [32]byte
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

func errToBytes(e error) []byte {
	return []byte(e.Error())
}

const (
	responseErr        byte = 0x00
	responseNoAuthCode byte = 0x01
	responseOk         byte = 0x02
	responseAuthCode   byte = 0x03
	responseBlob       byte = 0x04
)

type inputT interface {
	update(stateT) (stateT, outputT)
}

type readMainChansT struct {
	chs mainChansT
}

func (r readMainChansT) send() inputT {
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
		return newSetupRequest{s: newConnection}
	default:
		return noInputT{}
	}
}

type noInputT struct{}

func (n noInputT) update(s stateT) (stateT, outputT) {
	return s, readMainChansT{chs: s.mainChans}
}

type newSetupRequest struct {
	s setupConnectionT
}

func signedAuthToSlice(bs [signedAuthSize]byte) []byte {
	slice := make([]byte, signedAuthSize)
	for i, b := range bs {
		slice[i] = b
	}
	return slice
}

type sendErr struct {
	err error
	ch  chan error
}

func (s sendErr) send() inputT {
	s.ch <- s.err
	return noInputT{}
}

func (n newSetupRequest) update(s stateT) (stateT, outputT) {
	authSlice, validSignature := sign.Open(
		make([]byte, 0),
		signedAuthToSlice(n.s.signedAuthCode),
		&n.s.author)
	if !validSignature {
		err := errors.New("Bad signature.")
		return s, sendErr{err: err, ch: n.s.errChan}
	}
	var authCode [authCodeLength]byte
	for i, b := range authSlice {
		authCode[i] = b
	}
	authOk := checkAuthCode(authCode, s.authCodes, n.s.posixTime)
	if !authOk {
		err := errors.New("Bad auth code.")
		return s, sendErr{err: err, ch: n.s.errChan}
	}
	var newConnUsers map[[32]byte]userStateT
	for user, userState := range s.connectedUsers {
		newConnUsers[user] = userState
	}
	var expectedBlob [32]byte
	newConnUsers[n.s.author] = userStateT{
		chans:        n.s.chans,
		blobExpected: false,
		expectedBlob: expectedBlob,
	}
	newState := s
	newState.connectedUsers = newConnUsers
	return newState, readMainChansT{chs: s.mainChans}
}

func main() {
	mainChans := mainChansT{
		getAuthCode:     make(chan authCodeChans),
		setupConnection: make(chan setupConnectionT),
	}
	go httpServer(mainChans)
	var input inputT = noInputT{}
	var output outputT = readMainChansT{chs: mainChans}
	state := initState()
	for state.fatalErr == nil {
		input = output.send()
		state, output = input.update(state)
	}
}

const (
	blob         byte = 0x00
	invite       byte = 0x01
	uninvite     byte = 0x02
	metadata     byte = 0x03
	authenticate byte = 0x04
)

type httpInputT struct {
	route byte
	body  []byte
}

type readFileT struct {
	filePath   string
	outputChan chan []byte
}

func getMyMsgs(
	allMsgs map[metadataT]bool,
	author [32]byte) map[metadataT]bool {

	var myMsgs map[metadataT]bool
	for msg, _ := range allMsgs {
		if msg.recipient == author {
			myMsgs[msg] = true
		}
	}
	return myMsgs
}

type sendMsgsToClient struct {
	msgs       map[metadataT]bool
	outputChan chan []byte
}

type msgDownloadT struct {
	author    [32]byte
	authCode  [15]byte
	signature [sigSize]byte
}

func concatMsgRequest(m msgDownloadT) []byte {
	result := make([]byte, 30)
	i := 0
	for i < 15 {
		result[i] = m.authCode[i]
	}
	for i < 30 {
		result[i] = pleaseGiveMeMsgs[i-15]
	}
	return result
}

func checkAuthCode(authCode [authCodeLength]byte, authCodes map[[authCodeLength]byte]int64, posixTime int64) bool {
	authTime, authExists := authCodes[authCode]
	if !authExists {
		return false
	}
	authAge := posixTime - authTime
	if authAge > 30 {
		return false
	}
	if authAge < 0 {
		return false
	}
	return true
}

type getAuthCodeT struct {
	chans userChansT
}

type userChansT struct {
	in  chan httpInputT
	out chan []byte
}

type newAuthCodeT struct {
	code      [15]byte
	posixTime int64
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

func byte15ToSlice(b15 [15]byte) []byte {
	slice := make([]byte, 15)
	for i, b := range b15 {
		slice[i] = b
	}
	return slice
}

type giveMeMsgsT struct {
	author    [32]byte
	authCode  [15]byte
	signature [sigSize]byte
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
	validSignature := verifyDetached(
		concatInviteAndCodes(
			invitation.invitee,
			invitation.uniqueID,
			meaningCode),
		invitation.signature,
		invitation.author)
	if !validSignature {
		err := errors.New("Could not verify signature.")
		return invitation, err
	}
	_, authorIsMember := memberList[invitation.author]
	if !authorIsMember {
		errStr := "The author is not a member."
		return invitation, errors.New(errStr)
	}
	if invitation.author == invitation.invitee {
		errStr := "You can't (un)invite yourself."
		return invitation, errors.New(errStr)
	}
	return invitation, nil
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

var pleaseGiveMeMsgs = [15]byte{
	0x23, 0x79, 0x01, 0xd1, 0xf0, 0x04, 0xe7, 0x1c,
	0x55, 0xba, 0x53, 0xae, 0xa3, 0x13, 0x94}

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
	outputChan chan []byte
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

type metadataT struct {
	chunkHeadHash         [32]byte
	author                [32]byte
	recipient             [32]byte
	encryptedSymmetricKey [encSymKeyLen]byte
	signature             [sigSize]byte
}

const concatMdLen = 32 + 32 + encSymKeyLen

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

type handlerT = func(http.ResponseWriter, *http.Request)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func readWebsocket(
	ws *websocket.Conn,
	inputChan chan httpInputT) {

	for {
		msgType, rawMsg, readErr := ws.ReadMessage()
		if readErr != nil {
			return
		}
		if msgType != websocket.BinaryMessage {
			return
		}
		inputChan <- httpInputT{
			route: rawMsg[0],
			body:  rawMsg[1:],
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

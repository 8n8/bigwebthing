package main

import (
	"bytes"
	"crypto/rand"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/gorilla/websocket"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/sign"
	"io/ioutil"
	"net/http"
	"os"
	"time"
	"common"
	"net"
)

type stateT struct {
	newConnChan chan tcpConnectionT
	connectedUsers map[[32]byte]tcpConnectionT
}

type outputT interface {
	send(chan inputT)
}

const (
	responseAuthCode              byte = 0x00
	responseRecipientNotConnected byte = 0x01
)

type inputT interface {
	update(*stateT) (stateT, outputT)
}

type readChansT struct {
	newConnChan chan tcpConnectionT
	conn
}

func (s stateT) send(inChan chan inputT) {
	select {
	case newConn := <-r.newConnChan:
		conn := newConn.conn
		authCode, err := genAuthCode()
		if err != nil {
			fmt.Print(err)
			encodedErr, encErr := common.EncodeErr(err)
			if encErr != nil {
				fmt.Print(encErr)
				return
			}
			conn.Write(encodedErr)
			conn.Close()
			newConn.killChan <- killConn{}
			return
		}
		conn.Write(authCode)
		authSig := make([]byte, common.AuthSigSize)
		n, err := conn.Read(authSig)
		if n != AuthSigSize {
			fmt.Print("Auth sig too short.")
			encodedErr, encErr := common.Encode("Auth sig too short.")
			if encErr != nil {
				fmt.Print(encErr)
				return
			}
			conn.Write(encodedErr)
			conn.Close()
			newConn.killChan <- killConn{}
			return
		}
		
		returnedAuth, okSig := sign.Open(
			make([]byte, 0),
			authSig,
			
	default:
	}
	for author, connection := range r.connectedUsers {
		select {
		case msg := <-userChans.in:
			return userMsgT{
				author:  author,
				msg:     msg,
				outChan: userChans.out,
				errChan: userChans.errOut,
			}
		case <-userChans.errIn:
			userChans.errOut <- errors.New(
				"WS reader has errored.")
			return endWsSessionT{author: author}
		default:
		}
	}
	return noInputT{}
}

type endWsSessionT struct {
	author [32]byte
}

func (k endWsSessionT) update(s *stateT) (stateT, outputT) {
	return removeBlobHash(s, k.author), readChans(s)
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
	// case inInvitation:
	// 	return processInvitation(u, s)
	// case inUninvitation:
	// 	return processUninvitation(u, s)
	// case inMetadata:
	// 	return processMetadata(u, s)
	// case inBlob:
	// 	return processBlob(u, s)
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

// func processMetadata(umsg userMsgT, s *stateT) (stateT, outputT) {
// 	md, err := parseMetadata(umsg.msg.body, s.memberList)
// 	if err != nil {
// 		return *s, sendErrT{err: err, ch: umsg.errChan}
// 	}
// 
// 	recipientChans, recipientOnline := s.connectedUsers[md.Recipient]
// 	if !recipientOnline {
// 		return *s, sendMsgT{
// 			msg: []byte{responseRecipientNotConnected},
// 			ch:  umsg.outChan,
// 		}
// 	}
// 
// 	newState := addBlobHash(s, md.Blobhash, md.Recipient)
// 	output := sendMsgT{
// 		msg: umsg.msg.body,
// 		ch:  recipientChans.out,
// 	}
// 	return newState, output
// }

// func addUninvite(s *stateT, uninvitation invitationT) stateT {
// 	var newUninvites map[invitationT]bool
// 	for u, _ := range s.uninvitations {
// 		newUninvites[u] = true
// 	}
// 	newUninvites[uninvitation] = true
// 	newState := *s
// 	newState.uninvitations = newUninvites
// 	newState.memberList = makeMemberList(
// 		s.invitations,
// 		newUninvites)
// 	return newState
// }

func sigToSlice(sig [sigSize]byte) []byte {
	slice := make([]byte, sigSize)
	for i, b := range sig {
		slice[i] = b
	}
	return slice
}

func metadataErr(
	metadata common.MetadataT,
	memberList map[[32]byte]bool) error {

	actual, okSig := sign.Open(
		make([]byte, 0),
		sigToSlice(metadata.Signature),
		&metadata.Author)
	if !okSig {
		return errors.New("Bad signature.")
	}
	if !bytes.Equal(common.MakeDigest(metadata.Blobhash, common.AppSigCode), actual) {
		return errors.New("Bad signature.")
	}
	_, authorIsMember := memberList[metadata.Author]
	if !authorIsMember {
		return errors.New("Author is not a member.")
	}
	_, recipientIsMember := memberList[metadata.Recipient]
	if !recipientIsMember {
		return errors.New("Recipient is not a member.")
	}
	if metadata.Author == metadata.Recipient {
		return errors.New("Can't send messages to yourself.")
	}
	return nil
}

// func processUninvitation(umsg userMsgT, s *stateT) (stateT, outputT) {
// 	uninvitation, err := parseInviteLike(
// 		umsg.msg.body,
// 		s.memberList,
// 		pleaseUninviteX)
// 	if err != nil {
// 		return *s, sendErrT{err: err, ch: umsg.errChan}
// 	}
// 	newState := addUninvite(s, uninvitation)
// 	goodUninvite := appendToFileT{
// 		filepath:   uninvitesFilePath,
// 		bytesToAdd: umsg.msg.body,
// 		errChan:    umsg.errChan,
// 	}
// 	return newState, goodUninvite
// }

// func addInvite(s *stateT, invitation invitationT) stateT {
// 	var newInvites map[invitationT]bool
// 	for i, _ := range s.invitations {
// 		newInvites[i] = true
// 	}
// 	newInvites[invitation] = true
// 	newState := *s
// 	newState.invitations = newInvites
// 	newState.memberList = makeMemberList(
// 		newInvites,
// 		s.uninvitations)
// 	return newState
// }

// func processInvitation(umsg userMsgT, s *stateT) (stateT, outputT) {
// 	invitation, err := parseInviteLike(
// 		umsg.msg.body,
// 		s.memberList,
// 		pleaseInviteX)
// 	if err != nil {
// 		return *s, sendErrT{err: err, ch: umsg.errChan}
// 	}
// 	newState := addInvite(s, invitation)
// 	goodInvite := appendToFileT{
// 		filepath:   invitesFilePath,
// 		bytesToAdd: umsg.msg.body,
// 		errChan:    umsg.errChan,
// 	}
// 	return newState, goodInvite
// }

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
	newState := addUserConn(s, c.author, c.chans)
	output := sendErrT{err: nil, ch: c.errChan}
	return newState, output
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

// func readFileData(s *stateT) error {
// 	var err error
// 	s.invitations, err = readInvites(invitesFilePath)
// 	if err != nil {
// 		fmt.Print(err.Error())
// 		return err
// 	}
// 	s.uninvitations, err = readInvites(uninvitesFilePath)
// 	if err != nil {
// 		fmt.Print(err.Error())
// 		return err
// 	}
// 	return nil
// }

func main() {
	var state stateT
	// err := readFileData(&state)
	// if err != nil {
	// 	return
	// }
	go tcpServer(state.mainChans)
	var input inputT = noInputT{}
	var output outputT = readChans(&state)
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
	in     chan httpInputT
	out    chan []byte
	errOut chan error
	errIn  chan error
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

func convertSig(sig [sigSize]byte) []byte {
	newSig := make([]byte, sigSize)
	for i, el := range sig {
		newSig[i] = el
	}
	return newSig
}

var pleaseUninviteX = [15]byte{
	0x6e, 0xb9, 0xb4, 0x6f, 0xdc, 0x87, 0xf6, 0xbc, 0xcf,
	0xdf, 0x44, 0x22, 0xea, 0x78, 0xf4}

var pleaseInviteX = [15]byte{
	0xe8, 0xbf, 0x93, 0xd8, 0x39, 0xd3, 0x34, 0xe5, 0xc0,
	0x1f, 0xff, 0x2b, 0xc4, 0x30, 0xc4}

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
)

func parseMetadata(
	raw []byte,
	memberList map[[32]byte]bool) (common.MetadataT, error) {

	var metadata common.MetadataT
	jsonErr := json.Unmarshal(raw, &metadata)
	if jsonErr != nil {
		return common.MetadataT{}, jsonErr
	}
	return metadata, metadataErr(metadata, memberList)
}

type handlerT = func(http.ResponseWriter, *http.Request)

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func readWebsocket(
	ws *websocket.Conn,
	errChanIn chan error,
	inputChan chan httpInputT) {

	for {
		msgType, msgReader, readErr := ws.NextReader()
		if readErr != nil {
			errChanIn <- readErr
			return
		}
		if msgType != websocket.BinaryMessage {
			errChanIn <- errors.New("Bad message type.")
			return
		}

		rawMsg := make([]byte, 16000)
		lenMsg, err := msgReader.Read(rawMsg)
		if err != nil {
			errChanIn <- err
			return
		}
		if lenMsg == 0 {
			errChanIn <- errors.New("Empty message.")
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

func genAuthAndSendToClient(
	ws *websocket.Conn,
	getAuthChan chan authCodeChans) error {

	authChans := authCodeChans{
		main: make(chan [authCodeLength]byte),
		err:  make(chan error),
	}
	getAuthChan <- authChans

	select {
	case err := <-authChans.err:
		return err
	case authCode := <-authChans.main:
		sendErr := ws.WriteMessage(
			websocket.BinaryMessage,
			append(
				[]byte{responseAuthCode},
				authToSlice(authCode)...))
		if sendErr != nil {
			return sendErr
		}
	}
	return nil
}

func readAuthFromClient(ws *websocket.Conn) (authenticatorT, error) {
	var auth authenticatorT
	msgType, rawMsg, readErr := ws.ReadMessage()
	if readErr != nil {
		return auth, readErr
	}
	if msgType != websocket.BinaryMessage {
		return auth, errors.New("Bad message type.")
	}
	jsonErr := json.Unmarshal(rawMsg, &auth)
	return auth, jsonErr
}

func authenticate(
	ws *websocket.Conn,
	mainChans mainChansT,
	sessionChans userChansT) error {

	genErr := genAuthAndSendToClient(ws, mainChans.getAuthCode)
	if genErr != nil {
		return genErr
	}

	authToken, authErr := readAuthFromClient(ws)
	if authErr != nil {
		return authErr
	}

	errChan := make(chan error)
	setup := setupConnectionT{
		chans:          sessionChans,
		author:         authToken.author,
		signedAuthCode: authToken.signedAuthCode,
		errChan:        errChan,
		posixTime:      time.Now().Unix(),
	}
	mainChans.setupConnection <- setup
	return <-errChan
}

func writeWebsocket(
	ws *websocket.Conn,
	sessionChans userChansT) error {

	for {
		select {
		case msgOut := <-sessionChans.out:
			sendErr := ws.WriteMessage(
				websocket.BinaryMessage, msgOut)
			if sendErr != nil {
				return sendErr
			}
		case err := <-sessionChans.errOut:
			return err
		default:
		}
	}
	return nil
}

func handler(
	mainChans mainChansT,
	w http.ResponseWriter,
	r *http.Request) {

	ws, wsErr := upgrader.Upgrade(w, r, nil)
	defer ws.Close()
	if wsErr != nil {
		return
	}

	var sessionChans userChansT

	authErr := authenticate(ws, mainChans, sessionChans)
	if authErr != nil {
		fmt.Print(authErr.Error())
		return
	}

	go readWebsocket(ws, sessionChans.errIn, sessionChans.in)
	writeErr := writeWebsocket(ws, sessionChans)
	if writeErr != nil {
		fmt.Print(writeErr.Error())
	}
}

const authCodeLength = 24

type setupConnectionT struct {
	conn net.Conn
}

type authenticatorT struct {
	signedAuthCode [signedAuthSize]byte
	author         [32]byte
}

type authCodeChans struct {
	main chan [authCodeLength]byte
	err  chan error
}


func makeHandler(ch mainChansT) handlerT {
	return func(w http.ResponseWriter, r *http.Request) {
		handler(ch, w, r)
	}
}

type killConn struct {}

type tcpConnectionT struct {
	dec *gob.Decoder
	enc *gob.Encoder
	conn chan net.Conn
	killChan chan killConn
}

func tcpServer(newConnChan chan tcpConnectionT) {
	ln, err := net.Listen("tcp", ":4000")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer ln.Close()
	for {
		conn, err := ln.Accept()
		if err != nil {
			fmt.Println(err)
			continue
		}
		go func() {
			var killChan chan killConn
			newConnChan <- newConnT{
				dec: gob.NewDecoder(conn),
				enc: gob.NewEncoder(conn),
				conn: conn,
				killChan: killChan,
			}
			<-killChan
		}()
	}
}

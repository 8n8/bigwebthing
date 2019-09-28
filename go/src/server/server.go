package main

import (
	"bytes"
	"encoding/base64"
	"os"
	"golang.org/x/crypto/blake2b"
	"common"
	"crypto/rand"
	"encoding/gob"
	"encoding/json"
	"fmt"
	"golang.org/x/crypto/nacl/sign"
	"io/ioutil"
	"net"
	"time"
	"errors"
	"net/http"
	"sync"
)

type stateT struct {
	newConnChan    chan tcpConnectionT
	errInChan chan errMsgT
	msgInChan chan common.ClientToClient
	connectedUsers map[[32]byte]tcpOutChansT
	isMember       chan isMemberT
	members        map[[32]byte]dontCare
}

func initState() (stateT, error) {
	members, err := readMembers()
	if err != nil {
		return *new(stateT), err
	}
	if len(members) < 1 {
		return *new(stateT), errors.New("No members.")
	}
	return stateT{
		newConnChan: make(chan tcpConnectionT),
		errInChan: make(chan errMsgT),
		msgInChan: make(chan common.ClientToClient),
		connectedUsers: make(map[[32]byte]tcpOutChansT),
		isMember: make(chan isMemberT),
		members: members,
	}, nil
}

type errMsgT struct {
	id [32]byte
	err error
}

type tcpOutChansT struct {
	err chan error
	msg chan common.ClientToClient
}

type dontCare struct{}

type isMemberT struct {
	id         [32]byte
	returnChan chan bool
}

type outputT interface {
	send() inputT
}

type inputT interface {
	update(*stateT) (stateT, outputT)
}

type readChansT struct {
	isMember       chan isMemberT
	errInChan chan errMsgT
	msgInChan chan common.ClientToClient
	newConnChan    chan tcpConnectionT
	members map[[32]byte]dontCare
	connectedUsers map[[32]byte]tcpOutChansT
}

type endConnT struct {
	id [32]byte
}

func (e endConnT) update(s *stateT) (stateT, outputT) {
	newConnUsers := make(map[[32]byte]tcpOutChansT)
	for id, conn := range s.connectedUsers {
		newConnUsers[id] = conn
	}
	delete(newConnUsers, e.id)
	newState := *s
	newState.connectedUsers = newConnUsers
	return newState, readChans(s)
}

func authSigToSlice(sig [common.AuthSigSize]byte) []byte {
	result := make([]byte, common.AuthSigSize)
	for i := 0; i < common.AuthSigSize; i++ {
		result[i] = sig[i]
	}
	return result
}

func authOk(
	a common.AuthSigT,
	code [common.AuthCodeLength]byte) bool {

	givenCode, sigOk := sign.Open(
		make([]byte, 0),
		authSigToSlice(a.Sig),
		&a.Author)
	okAuth := bytes.Equal(givenCode, common.AuthCodeToSlice(code))
	return sigOk && okAuth
}

func (r readChansT) send() inputT {
	select {
	case conn := <-r.newConnChan:
		return conn
	case isMember := <-r.isMember:
		_, ok := r.members[isMember.id]
		isMember.returnChan <- ok
	case errIn := <-r.errInChan:
		fmt.Println(errIn)
		return endConnT{errIn.id}
	case msgIn := <-r.msgInChan:
		fmt.Println("New message reached main thread.")
		fmt.Println(msgIn)
		recipCh, ok := r.connectedUsers[msgIn.Recipient]
		if !ok {
			fmt.Println("recipient not connected")
			return noInputT{}
		}
		fmt.Println("Found recipient.")
		recipCh.msg <- msgIn
		fmt.Println("Msg sent to recipient goroutine.")
	}
	return noInputT{}
}

type noInputT struct{}

func readChans(s *stateT) readChansT {
	return readChansT{
		isMember:       s.isMember,
		errInChan: s.errInChan,
		msgInChan: s.msgInChan,
		newConnChan:    s.newConnChan,
		members: s.members,
		connectedUsers: s.connectedUsers,
	}
}

func (n noInputT) update(s *stateT) (stateT, outputT) {
	return *s, readChans(s)
}

func readMembers() (map[[32]byte]dontCare, error) {
	result := make(map[[32]byte]dontCare)
	var members [][32]byte
	contents, err := ioutil.ReadFile("serverData/members.txt")
	if err != nil {
		return result, err
	}
	err = json.Unmarshal(contents, &members)
	if err != nil {
		return result, err
	}
	for _, member := range members {
		result[member] = dontCare{}
	}
	return result, nil
}

type publicSignKeyT [32]byte
type hashT [32]byte

type message struct {
	Body []byte
	Recipient publicSignKeyT
	Author publicSignKeyT
}

type textWithLinks struct {
	link *linkT
	text string
}

type linkT struct {
	displayName string
	contentHash []byte
}

type Msg interface {
	to() publicSigningKeyT
	checkSignature() error
}

func (k keyChangeMsg) to() publicSigningKeyT {
	return k.address.to
}

func (b blobMsg) to() publicSigningKeyT {
	return b.address.to
}

var AUTHCODES = make(map[[16]byte]int64)
var AUTHCODESMUX sync.Mutex

// The signature in the address of a keyChangeMsg must sign the
// blake2b hash of:
//
//     key of recipient + new key + authcode
//
// This ensures that the owner of the old key is the creator of this
// new one.
//
// The signature of the new key just needs to sign the auth code
// with the new key. This ensures that they own the new key and
// are not stealing someone elses.
func (k keyChangeMsg) checkSignature() error {
	expected := blake2b.Sum256(append(
		append(k.address.to[:], k.newKey[:]...),
		k.address.authCode[:]...))
	fromArr := [32]byte(k.address.from)
	actual, ok := sign.Open(
		make([]byte, 0),
		k.address.sig[:],
		&fromArr)
	if !ok {
		return errors.New("bad old key signature")
	}
	for i, e := range expected {
		if actual[i] != e {
			return errors.New("unexpected old key signature")
		}
	}

	err := validAuthCode(k.address.authCode)
	if err != nil {
		return err
	}

	newKeyArr := [32]byte(k.newKey)
	actual2, ok2 := sign.Open(
		make([]byte, 0),
		k.newKeySig[:],
		&newKeyArr)
	if !ok2 {
		return errors.New("bad new key signature")
	}
	if len(actual2) != authCodeLen {
		return errors.New("signed code wrong length")
	}
	for i, a := range actual2 {
		if k.address.authCode[i] != a {
			return errors.New("bad signed auth code")
		}
	}

	return nil
}

// It is encoded as:
//
//     address + newKey + newKeySig
//
// It has length keyChangeMsgLen.
//
type keyChangeMsg struct {
	address addressT
	newKey publicSigningKeyT
	newKeySig signatureT
}
const keyChangeMsgLen = addressLen + pubKeyLen + sigLen

// It is encoded as:
//
//     address + nonce + blob
//
// It has length:
//
//     addressLen + nonceLen + (whatever the length of the blob is)
//
type blobMsg struct {
	address addressT
	nonce nonceT
	blob []byte
}


// The signature is of the blake2b hash of:
//
//     key of recipient + blob + authcode
//
func (b blobMsg) checkSignature() error {
	expected := blake2b.Sum256(append(
		append(b.address.to[:], b.blob...),
		b.address.authCode[:]...))
	fromArr := [32]byte(b.address.from)
	actual, ok := sign.Open(
		make([]byte, 0),
		b.address.sig[:],
		&fromArr)
	if !ok {
		return errors.New("bad signature")
	}
	for i, e := range expected {
		if actual[i] != e {
			return errors.New("unexpected signature")
		}
	}

	return validAuthCode(b.address.authCode)
}

func validAuthCode(authCode authCodeT) error {
	AUTHCODESMUX.Lock()
	createdTime, ok := AUTHCODES[authCode]
	delete(AUTHCODES, authCode)
	AUTHCODESMUX.Unlock()

	if !ok {
		return errors.New("could not find auth code")
	}
	const fiveMinutes = 5 * 60
	if time.Now().Unix() - createdTime > fiveMinutes {
		return errors.New("auth code out of date")
	}
	return nil
}

func encodePubKey(p publicSigningKeyT) string {
	return base64.URLEncoding.EncodeToString(p[:])
}

const nonceLen = 24
type nonceT [nonceLen]byte

// It is encoded as:
//
//     from + to + sig + authCode
//
// It has length addressLen.
//
type addressT struct {
	from publicSigningKeyT
	to publicSigningKeyT
	sig signatureT
	authCode authCodeT
}
const addressLen = 2 * pubKeyLen + sigLen + authCodeLen
const authCodeLen = 16

type authCodeT = [authCodeLen]byte

const pubKeyLen = 32

type publicSigningKeyT [pubKeyLen]byte

type signatureT [sigLen]byte

const sigLen = 96

func decodeMsg(body []byte) (Msg, error) {
	if body[0] == 0 {
	    return decodeKeyChangeMsg(body[1:])
	}
	if body[0] == 1 {
		return decodeBlobMsg(body[1:])
	}
	return *new(Msg), errors.New(
		"first byte of message should be 0 or 1")
}

const inboxesPath = "inboxes"

func save(msg []byte, to publicSigningKeyT) error {
	dirPath := base64.URLEncoding.EncodeToString(to[:])
	err := os.MkdirAll(inboxesPath + "/" + dirPath, os.ModeDir)
	if err != nil {
		return err
	}

	hash := blake2b.Sum256(msg)
	filename := base64.URLEncoding.EncodeToString(hash[:])
	msgPath := dirPath + "/" + filename
	return ioutil.WriteFile(msgPath, msg, 0644)
}

func decodeBlobMsg(body []byte) (Msg, error) {
	address, err := decodeAddress(body[:addressLen])
	if err != nil {
		return *new(blobMsg), err
	}

	var nonce nonceT
	copy(nonce[:], body[addressLen:addressLen + nonceLen])
	blob := make([]byte, 0)
	copy(blob, body[addressLen + nonceLen :])
	return blobMsg{address, nonce, blob}, nil
}

func decodeKeyChangeMsg(body []byte) (keyChangeMsg, error) {
	if len(body) != keyChangeMsgLen {
		return *new(keyChangeMsg), errors.New(
			"key change message wrong length")
	}
	address, err := decodeAddress(body[:addressLen])
	if err != nil {
		return *new(keyChangeMsg), err
	}
	var newKey publicSigningKeyT
	copy(newKey[:], body[addressLen : addressLen + pubKeyLen])
	var newKeySig signatureT
	copy(newKeySig[:], body[addressLen + pubKeyLen:])
	var newKeyArr [32]byte
	copy(newKeyArr[:], newKey[:])
	return keyChangeMsg{address, newKey, newKeySig}, nil
}

func decodeAddress(body []byte) (addressT, error) {
	if len(body) != addressLen {
		return *new(addressT), errors.New(
			"address message wrong length")
	}

	// The boundaries in the encoded address are:
	const fromTo = pubKeyLen
	const toSig = fromTo + pubKeyLen
	const sigAuth = toSig + sigLen
	var from publicSigningKeyT
	copy(from[:], body[:fromTo])
	var to publicSigningKeyT
	copy(to[:], body[fromTo:toSig])
	var sig signatureT
	copy(sig[:], body[toSig : sigAuth])
	var authCode authCodeT
	copy(authCode[:], body[sigAuth:])
	return addressT{from, to, sig, authCode}, nil
}

func sendErr(w http.ResponseWriter, r *http.Request) (error, int) {
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		return err, 500
	}

	msg, err := decodeMsg(body)
	if err != nil {
		return err, 400
	}

	err = msg.checkSignature()
	if err != nil {
		return err, 400
	}

	err = save(body, msg.to())
	if err != nil {
		return err, 500
	}
	return nil, 0
}

func main() {
	http.HandleFunc(
		"/send",
		func(w http.ResponseWriter, r *http.Request) {
			err, errCode := sendErr(w, r)
			if err != nil {
				http.Error(w, err.Error(), errCode)
				return
			}
		})
	http.HandleFunc(
		"/receive",
		func(w http.ResponseWriter, r *http.Request) {
		})
	fmt.Println(http.ListenAndServe(":3000", nil))
}

func genAuthCode() ([common.AuthCodeLength]byte, error) {
	authSlice := make([]byte, common.AuthCodeLength)
	_, err := rand.Read(authSlice)
	var authCode [common.AuthCodeLength]byte
	if err != nil {
		return authCode, err
	}
	for i, b := range authSlice {
		authCode[i] = b
	}
	return authCode, nil
}

type tcpConnectionT struct {
	out    chan common.ClientToClient
	outErr chan error
	id     [32]byte
}

func (t tcpConnectionT) update(s *stateT) (stateT, outputT) {
	newConnUsers := make(map[[32]byte]tcpOutChansT)
	for k, v := range s.connectedUsers {
		newConnUsers[k] = v
	}
	newConnUsers[t.id] = tcpOutChansT{t.outErr, t.out}
	newS := *s
	newS.connectedUsers = newConnUsers
	return newS, readChans(s)
}

func handleConn(
	conn net.Conn,
	newConnChan chan tcpConnectionT,
	isMemberChan chan isMemberT,
	errInChan chan errMsgT,
	msgInChan chan common.ClientToClient) {

	fmt.Println("Top of handleConn.")
	conn.SetDeadline(time.Now().Add(time.Second * 30))
	authCode, err := genAuthCode()
	if err != nil {
		fmt.Print(err)
		conn.Close()
		return
	}
	enc := gob.NewEncoder(conn)
	dec := gob.NewDecoder(conn)
	err = enc.Encode(authCode)
	if err != nil {
		fmt.Print(err)
		conn.Close()
		return
	}
	var authSig common.AuthSigT
	err = dec.Decode(&authSig)
	if err != nil {
		fmt.Print(err)
		conn.Close()
		return
	}
	if !authOk(authSig, authCode) {
		fmt.Print(err)
		conn.Close()
		return
	}
	memberOkCh := make(chan bool)
	isMemberChan <- isMemberT{authSig.Author, memberOkCh}
	isMember := <-memberOkCh
	if !isMember {
		conn.Close()
		return
	}
	chs := tcpConnectionT{
		out:    make(chan common.ClientToClient),
		outErr: make(chan error),
		id:     authSig.Author,
	}
	newConnChan <- chs
	conn.SetDeadline(time.Now().Add(time.Minute * 30))
	go func() {
		for {
			//var clientToClient common.ClientToClient
			fmt.Println("C")
			//err = dec.Decode(&clientToClient)
			fmt.Println("B")
			clientToClient, err := common.ReadClientToClient(conn)
			fmt.Println("Just below reading in message from client.")
			if err != nil {
				fmt.Println("A")
				fmt.Println(err)
				errInChan <- errMsgT{authSig.Author, err}
				chs.outErr <- err
				fmt.Print(err)
				conn.Close()
				return
			}
			fmt.Println("XX")
			fmt.Println(msgInChan)
			fmt.Println("xx")
			msgInChan <- clientToClient
			fmt.Println("Y")
		}
	}()
	for {
		conn.SetDeadline(time.Now().Add(time.Minute * 30))
		select {
		case newMsg := <-chs.out:
			fmt.Println("New message out.")
			encoded, err := common.EncodeClientToClient(
				newMsg)
			if err != nil {
				errInChan <- errMsgT{
					authSig.Author,
					err,
					}
				fmt.Print(err)
				conn.Close()
				return
			}
			fmt.Println("Correctly encoded.")
			n, err := conn.Write(encoded)
			if err != nil {
				errInChan <- errMsgT{
					authSig.Author,
					err,
				}
				fmt.Println(err)
				conn.Close()
				return
			}
			if n != len(encoded) {
				errInChan <- errMsgT{authSig.Author, errors.New("Didn't sent whole message.")}
				fmt.Println("Did not send whole message.")
				conn.Close()
				return
			}
			fmt.Println("Message sent without errors.")
			fmt.Println("Recipient: ")
			fmt.Println(authSig.Author)
			fmt.Println(">>>>>>>>")
		case <-chs.outErr:
			conn.Close()
			return
		}
	}
}

func tcpServer(
	newConnChan chan tcpConnectionT,
	isMember chan isMemberT,
	errInChan chan errMsgT,
	msgInChan chan common.ClientToClient) {

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
		go handleConn(conn, newConnChan, isMember, errInChan, msgInChan)
	}
}

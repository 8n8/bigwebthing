package main

import (
	"bytes"
	"encoding/base64"
	"os"
	"golang.org/x/crypto/blake2b"
	"common"
	"crypto/rand"
	"encoding/json"
	"fmt"
	"golang.org/x/crypto/nacl/sign"
	"io/ioutil"
	"time"
	"errors"
	"net/http"
	"sync"
	"io"
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

// func (k keyChangeMsg) to() publicSigningKeyT {
// 	return k.address.to
// }

// func (b blobMsg) to() publicSigningKeyT {
// 	return b.address.to
// }

var AUTHCODES = make(map[[16]byte]int64)
var AUTHCODESMUX sync.Mutex

// // The signature in the address of a keyChangeMsg must sign the
// // blake2b hash of:
// //
// //     key of recipient + new key + authcode
// //
// // This ensures that the owner of the old key is the creator of this
// // new one.
// //
// // The signature of the new key just needs to sign the auth code
// // with the new key. This ensures that they own the new key and
// // are not stealing someone elses.
// func (k keyChangeMsg) checkSignature() error {
// 	expected := blake2b.Sum256(append(
// 		append(k.address.to[:], k.newKey[:]...),
// 		k.address.authCode[:]...))
// 	fromArr := [32]byte(k.address.from)
// 	actual, ok := sign.Open(
// 		make([]byte, 0),
// 		k.address.sig[:],
// 		&fromArr)
// 	if !ok {
// 		return errors.New("bad old key signature")
// 	}
// 	for i, e := range expected {
// 		if actual[i] != e {
// 			return errors.New("unexpected old key signature")
// 		}
// 	}
// 
// 	err := validAuthCode(k.address.authCode)
// 	if err != nil {
// 		return err
// 	}
// 
// 	newKeyArr := [32]byte(k.newKey)
// 	actual2, ok2 := sign.Open(
// 		make([]byte, 0),
// 		k.newKeySig[:],
// 		&newKeyArr)
// 	if !ok2 {
// 		return errors.New("bad new key signature")
// 	}
// 	if len(actual2) != authCodeLen {
// 		return errors.New("signed code wrong length")
// 	}
// 	for i, a := range actual2 {
// 		if k.address.authCode[i] != a {
// 			return errors.New("bad signed auth code")
// 		}
// 	}
// 
// 	return nil
// }

// It is encoded as:
//
//     address + newKey + newKeySig
//
// It has length keyChangeMsgLen.
//
type keyChangeMsg struct {
	newKey publicSigningKeyT
	newKeySig authSigT
}
const keyChangeMsgLen = pubKeyLen + authSigLen

// It is encoded as:
//
//     address + nonce + blob
//
// It has length:
//
//     addressLen + nonceLen + (whatever the length of the blob is)
//
type blobMsg struct {
	nonce nonceT
	blob []byte
}


// The signature is of the blake2b hash of:
//
//     key of recipient + blob + authcode
//
// func (b blobMsg) checkSignature() error {
// 	expected := blake2b.Sum256(append(
// 		append(b.address.to[:], b.blob...),
// 		b.address.authCode[:]...))
// 	fromArr := [32]byte(b.address.from)
// 	actual, ok := sign.Open(
// 		make([]byte, 0),
// 		b.address.sig[:],
// 		&fromArr)
// 	if !ok {
// 		return errors.New("bad signature")
// 	}
// 	for i, e := range expected {
// 		if actual[i] != e {
// 			return errors.New("unexpected signature")
// 		}
// 	}
// 
// 	return validAuthCode(b.address.authCode)
// }

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
const authCodeLen = 16

type authCodeT = [authCodeLen]byte

const pubKeyLen = 32

type publicSigningKeyT [pubKeyLen]byte

const msgSigLen = sign.Overhead + 32
type msgSigT = [msgSigLen]byte
const authSigLen = sign.Overhead + authCodeLen
type authSigT = [authSigLen]byte

// func decodeMsg(body []byte) (Msg, error) {
// 	if body[0] == 0 {
// 	    return decodeKeyChangeMsg(body[1:])
// 	}
// 	if body[0] == 1 {
// 		return decodeBlobMsg(body[1:])
// 	}
// 	return *new(Msg), errors.New(
// 		"first byte of message should be 0 or 1")
// }

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

// func decodeBlobMsg(body []byte) (Msg, error) {
// 	address, err := decodeAddress(body[:addressLen])
// 	if err != nil {
// 		return *new(blobMsg), err
// 	}
// 
// 	var nonce nonceT
// 	copy(nonce[:], body[addressLen:addressLen + nonceLen])
// 	blobLen := len(body) - addressLen - nonceLen
// 	if blobLen > maxBlobLen {
// 		return *new(blobMsg), errors.New("blob too long")
// 	}
// 	blob := make([]byte, blobLen)
// 	copy(blob, body[addressLen + nonceLen :])
// 	return blobMsg{address, nonce, blob}, nil
// }

func decodeKeyChangeMsg(body []byte) (keyChangeMsg, error) {
	if len(body) != keyChangeMsgLen {
		return *new(keyChangeMsg), errors.New(
			"key change message wrong length")
	}
	var newKey publicSigningKeyT
	copy(newKey[:], body[:pubKeyLen])
	var newKeySig authSigT
	copy(newKeySig[:], body[pubKeyLen:])
	var newKeyArr [32]byte
	copy(newKeyArr[:], newKey[:])
	return keyChangeMsg{newKey, newKeySig}, nil
}

// func checkRequestSig(r receiveMsg) error {
// 	idArr := [32]byte(r.myId)
// 	signed, ok := sign.Open(make([]byte, 0), r.signature[:], &myId)
// 	if !ok {
// 		return errors.New("bad signature")
// 	}
// 	for i, s := range signed {
// 		if r.authCode[i] != s {
// 			return errors.New("unexpected signature")
// 		}
// 	}
// 	return nil
// }

const maxBlobLen = 16000
const maxBodyLen = pubKeyLen + authCodeLen + msgSigLen + 1 +
	maxBlobMsgLen
const maxBlobMsgLen = pubKeyLen + nonceLen + maxBlobLen



// All messages that are restricted to members only have this
// structure:
//
//     sender public key + auth code + signature + meaning byte +
//         everything else
//
func protectedErr(
		w http.ResponseWriter, r *http.Request) (error, int) {
	body := make([]byte, maxBodyLen)
	n, err := r.Body.Read(body)
	if err != nil {
		return err, 500
	}
	body = body[:n]

	// Check credentials.
	const senderEnd = pubKeyLen
	const authCodeEnd = senderEnd + authCodeLen
	const sigEnd = authCodeEnd + msgSigLen
	const meaningEnd = sigEnd + 1
	var sender publicSigningKeyT
	copy(sender[:], body[:senderEnd])
	var authCode authCodeT
	copy(authCode[:], body[senderEnd : authCodeEnd])
	var signature msgSigT
	copy(signature[:], body[authCodeEnd : sigEnd])
	everythingElse := body[sigEnd:]
	if len(everythingElse) == 0 {
		return errors.New("no message body"), 400
	}
	expectedSigned := blake2b.Sum256(everythingElse)
	fromArr := [32]byte(sender)
	actualSigned, ok := sign.Open(
		make([]byte, 0),
		signature[:],
		&fromArr)
	if !ok {
		return errors.New("bad signature"), 400
	}
	if err != nil {
		return err, 400
	}
	for i, a := range actualSigned {
		if a != expectedSigned[i] {
			return errors.New("unexpected signature"), 400
		}
	}
	err = validAuthCode(authCode)
	if err != nil {
		return err, 400
	}

	ok, err = isMember(sender)
	if err != nil {
		return err, 500
	}
	if !ok {
		return errors.New("sender is not a member"), 400
	}

	meaning := everythingElse[0]

	afterMeaning := everythingElse[1:]
	switch meaning {
	case 0:
		return sendMsg(body, afterMeaning, sender, authCode)
	case 1:
		return receiveMsg(sender, w)
	case 2:
		return deleteMsg(sender, afterMeaning)
	}
	return errors.New("bad meaning code"), 400
}

func deleteMsg(sender publicSigningKeyT, body []byte) (error, int) {
	if len(body) != 32 {
		return errors.New("message hash must be 32 bytes"), 400
	}
	filename := base64.URLEncoding.EncodeToString(body)
	path := makeInboxPath(sender[:]) + "/" + filename
	err := os.Remove(path)
	if err != nil {
		return err, 400
	}
	return nil, 0
}

func receiveMsg(
	sender publicSigningKeyT, w http.ResponseWriter) (error, int) {

	inboxPath := makeInboxPath(sender[:])
	files, err := ioutil.ReadDir(inboxPath)
	if err != nil {
		return err, 500
	}
	if len(files) == 0 {
		return nil, 0
	}
	msgPath := inboxPath + "/" + files[0].Name()
	handle, err := os.Open(msgPath)
	if err != nil {
		return err, 500
	}
	_, err = io.Copy(w, handle)
	if err != nil {
		return err, 500
	}
	return nil, 0
}

func sendMsg(
	original []byte,
	body []byte,
	sender publicSigningKeyT,
	authCode authCodeT) (error, int) {

	// This type of message can be a blobMsg or a keyChangeMsg. It
	// is encoded as:
	//
	//     meaning byte + recipient public key + remainder
	//
	meaningByte := body[0]
	to := body[1:pubKeyLen]
	msgHash := blake2b.Sum256(original)
	newFileName := base64.URLEncoding.EncodeToString(msgHash[:])
	endPath := makeInboxPath(to[:]) + "/" + newFileName
	if meaningByte == 0 {
		keyChange, err := decodeKeyChangeMsg(body[1 + pubKeyLen:])
		if err != nil {
			return err, 400
		}
		fromArr := [32]byte(keyChange.newKey)
		signed, ok := sign.Open(
			make([]byte, 0),
			keyChange.newKeySig[:],
			&fromArr)
		if !ok {
			return errors.New("bad signature"), 400
		}
		if len(signed) != authCodeLen {
			return errors.New("signed message wrong length"), 400
		}
		for i, s := range signed {
			if s != authCode[i] {
				return errors.New("unexpected signature"), 400
			}
		}
		oldInboxPath := makeInboxPath(sender[:])
		newInboxPath := makeInboxPath(keyChange.newKey[:])
		err = os.Rename(oldInboxPath, newInboxPath)
		if err != nil {
			return err, 500
		}
	}
	err := ioutil.WriteFile(endPath, original, 0600)
	if err != nil {
		return err, 500
	}
	return nil, 0
}

func makeInboxPath(b []byte) string {
	dirName := base64.URLEncoding.EncodeToString(b)
	return inboxesPath + "/" + dirName
}

func main() {
	http.HandleFunc(
		"/protected",
		func(w http.ResponseWriter, r *http.Request) {
			err, errCode := protectedErr(w, r)
			if err != nil {
				http.Error(w, err.Error(), errCode)
				return
			}
		})
	// http.HandleFunc(
	// 	"/receive",
	// 	func(w http.ResponseWriter, r *http.Request) {
	// 		err, errCode := receiveErr(w, r)
	// 		if err != nil {
	// 			http.Error(w, err.Error(), errCode)
	// 			return
	// 		}
	// 	})
	// http.HandleFunc(
	// 	"/delete",
	// 	func(w http.ResponseWriter, r *http.Request) {
	// 		err, errCode := deleteErr(w, r)
	// 		if err != nil {
	// 			http.Error(w, err.Error(), errCode)
	// 			return
	// 		}
	// 	})
	fmt.Println(http.ListenAndServe(":3000", nil))
}

// func receiveErr(w http.ResponseWriter, r *http.Request) (error, int) {
// 	var body [receiveMsgLen]byte
// 	n, err := r.Body.Read(body[:])
// 	if err != nil {
// 		return err, 400
// 	}
// 	if n != receiveMsgLen {
// 		return errors.New("message has wrong length"), 400
// 	}
// 	receive := decodeReceiveMsg(body)
// 	err = checkRequestSig(receive)
// 	if err != nil {
// 		return err, 400
// 	}
// 	err = validAuthCode(receive.authCode)
// 	if err != nil {
// 		return err, 400
// 	}
// 	dirPath := base64.URLEncoding.EncodeToString(receive.myId)
// 	msgs, err := ioutil.ReadDir(inboxesPath + "/" + dirPath)
// 	if len(msgs) == 0 {
// 		return nil, 0
// 	}
// 	filePath := inboxesDir + "/" + dirPath + "/" + msgs[0].Name()
// 	msgHandle, err := os.Open(filePath)
// 	if err != nil {
// 		return err, 500
// 	}
// 	_, err = io.Copy(w, msgHandle)
// 	if err != nil {
// 		return err, 500
// 	}
// 	return nil, 0
// }

func isMember(p publicSigningKeyT) (bool, error) {
	inboxes, err := ioutil.ReadDir(inboxesPath)
	if err != nil {
		return false, err
	}
	encodedKey := base64.URLEncoding.EncodeToString(p[:])
	for _, inbox := range inboxes {
		if inbox.Name() == encodedKey {
			return true, nil
		}
	}
	return false, nil
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

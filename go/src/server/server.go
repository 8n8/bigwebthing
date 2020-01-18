package main

import (
	"crypto/rand"
	"encoding/base64"
	"errors"
	"fmt"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/sign"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"sync"
	"time"
)

var AUTHCODES = make(map[[16]byte]int64)
var AUTHCODESMUX sync.Mutex

// It is encoded as:
//
//     address + newKey + newKeySig
//
// It has length keyChangeMsgLen.
//
type keyChangeMsg struct {
	newKey    publicSigningKeyT
	newKeySig authSigT
}

const keyChangeMsgLen = pubKeyLen + authSigLen

const authTimeOut = 5 * 60

func validAuthCode(authCode authCodeT) error {
	AUTHCODESMUX.Lock()
	createdTime, ok := AUTHCODES[authCode]
	delete(AUTHCODES, authCode)
	AUTHCODESMUX.Unlock()

	if !ok {
		return errors.New("could not find auth code")
	}
	if time.Now().Unix()-createdTime > authTimeOut {
		return errors.New("auth code out of date")
	}
	return nil
}

const nonceLen = 24
const authCodeLen = 16

type authCodeT = [authCodeLen]byte

const pubKeyLen = 32

type publicSigningKeyT [pubKeyLen]byte

const msgSigLen = sign.Overhead + 32

type msgSigT = [msgSigLen]byte

const authSigLen = sign.Overhead + authCodeLen

type authSigT = [authSigLen]byte

const inboxesPath = "inboxes"

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
	copy(authCode[:], body[senderEnd:authCodeEnd])
	var signature msgSigT
	copy(signature[:], body[authCodeEnd:sigEnd])
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
		keyChange, err := decodeKeyChangeMsg(body[1+pubKeyLen:])
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

const csp =
	"default-src 'none'; " +
	"script-src 'self'; " +
	"style-src 'unsafe-inline'; " +
	"img-src 'self'; " +
	"report-uri http://localhost:3001/cspreport;"

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
	http.HandleFunc(
		"/authcode",
		func(w http.ResponseWriter, r *http.Request) {
			authCode, err := genAuthCode()
			if err != nil {
				http.Error(w, err.Error(), 500)
				return
			}

			_, err = w.Write(authCode[:])
			if err != nil {
				http.Error(w, err.Error(), 500)
				return
			}

			now := time.Now().Unix()
			newCodes := make(map[authCodeT]int64)
			newCodes[authCode] = now
			AUTHCODESMUX.Lock()
			for a, t := range AUTHCODES {
				if t-now < authTimeOut {
					newCodes[a] = t
				}
			}
			AUTHCODES = newCodes
			AUTHCODESMUX.Unlock()
		})
	http.HandleFunc(
		"/",
		func(w http.ResponseWriter, r *http.Request) {
			if r.URL.Path != "/" {
				http.NotFound(w, r)
				return
			}
			w.Header().Add("Content-Security-Policy", csp)
			handle, err := os.Open("index.html")
			if err != nil {
				http.Error(w, err.Error(), 500)
				return
			}
			_, err = io.Copy(w, handle)
			if err != nil {
				http.Error(w, err.Error(), 500)
			}
		})
	http.HandleFunc(
		"/cspreport",
		func(w http.ResponseWriter, r *http.Request) {
			body, _ := ioutil.ReadAll(r.Body)
			fmt.Println(string(body))
		})
	serveFile("main.js", "application/javascript")
	serveFile("localforage.min.js", "application/javascript")
	serveFile("styles.css", "text/css")
        serveFile("favicon.ico", "image/ico")
        serveFile("elm.js", "application/javascript")
        serveFile("base64js.min.js", "application/javascript")
        serveFile("nacl-fast.min.js", "application/javascript")
	fmt.Println(http.ListenAndServe(":3001", nil))
}


func serveFile(filename, contentType string) {
	http.HandleFunc(
		"/" + filename,
		func(w http.ResponseWriter, r *http.Request) {
			handle, err := os.Open(filename)
			if err != nil {
				http.Error(w, err.Error(), 500)
				return
			}
			io.Copy(w, handle)
			w.Header().Add("Content-Type", contentType)
		})
}

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

func genAuthCode() ([authCodeLen]byte, error) {
	authSlice := make([]byte, authCodeLen)
	_, err := rand.Read(authSlice)
	var authCode [authCodeLen]byte
	if err != nil {
		return authCode, err
	}
	for i, b := range authSlice {
		authCode[i] = b
	}
	return authCode, nil
}

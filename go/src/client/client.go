package main

import (
	"constants"
	"crypto/rand"
	"encoding/base64"
	"errors"
	"fmt"
	"github.com/gorilla/websocket"
	"golang.org/x/crypto/argon2"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/nacl/sign"
	"io/ioutil"
	"net"
	"net/http"
	"path/filepath"
	"time"
)

type UiInput struct {
	w    http.ResponseWriter
	r    *http.Request
	done chan struct{}
}

const serverDomain = "http://localhost"

const serverHttpUrl = serverDomain + ":" + constants.ServerHttpPort

const serverTcpUrl = serverDomain + ":" + constants.ServerTcpPort

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

type MyKeys struct {
	sign struct {
		public *[32]byte
		secret *[64]byte
	}
	encrypt struct {
		public *[32]byte
		secret *[32]byte
	}
}

var STOP = make(chan error)
var RAWINPUT = make(chan RawInput)
var TOWEBSOCKET = make(chan []byte)
var AUTHCODE = make(chan []byte, 1)
var MYKEYS = make(chan MyKeys, 1)
var MYID = make(chan []byte, 1)

func makeTcpAuth(
	myId, authCode []byte, secretSign *[64]byte) []byte {

	auth := make([]byte, 13+sign.Overhead+16+16)
	copy(auth, myId)
	toSign := make([]byte, 32)
	copy(toSign, constants.TcpAuth)
	copy(toSign[32:], authCode)
	copy(auth[13:], sign.Sign([]byte{}, toSign, secretSign))
	return auth
}

func cachePath(filename string) string {
	return filepath.Join("clientDataDir", filename)
}

const keysFileName = "keys"

func readKeysFromFile() (MyKeys, error) {
	raw, err := ioutil.ReadFile(cachePath(keysFileName))
	if err != nil {
		return *new(MyKeys), err
	}

	return parseKeys(raw)
}

func parseKeys(raw []byte) (MyKeys, error) {
	var keys MyKeys

	if len(raw) != 32+64+32+32 {
		return keys, errors.New("keys file is the wrong length")
	}

	copy(keys.sign.public[:], raw)
	copy(keys.sign.secret[:], raw[32:])
	copy(keys.encrypt.public[:], raw[32+64:])
	copy(keys.encrypt.secret[:], raw[32+64+32:])

	return keys, nil
}

func makeNewKeys() (MyKeys, error) {
	var keys MyKeys
	var err error
	keys.sign.public, keys.sign.secret, err = sign.GenerateKey(
		rand.Reader)
	if err != nil {
		return keys, err
	}

	keys.encrypt.public, keys.encrypt.secret, err = box.GenerateKey(
		rand.Reader)
	if err != nil {
		return keys, err
	}

	err = ioutil.WriteFile(
		cachePath(keysFileName),
		encodeKeys(keys),
		0500)

	return keys, err
}

func encodeKeys(keys MyKeys) []byte {
	encoded := make([]byte, 160)
	copy(encoded, keys.sign.public[:])
	copy(encoded[32:], keys.sign.secret[:])
	copy(encoded[32+64:], keys.encrypt.public[:])
	copy(encoded[32+64+32:], keys.encrypt.secret[:])
	return encoded
}

var STATE = make(chan State)

// func main() {
// 	initKeys()
//
// 	return
//

func (StartUiServer) output() {
	http.HandleFunc(
		"/websocket",
		func(w http.ResponseWriter, r *http.Request) {
			conn, err := upgrader.Upgrade(w, r, nil)
			if err != nil {
				fmt.Println(err)
				return
			}

			for {
				err := conn.WriteMessage(
					websocket.TextMessage, <-TOWEBSOCKET)
				if err != nil {
					fmt.Println(err)
					return
				}
			}
		})
	http.HandleFunc(
		"/",
		func(w http.ResponseWriter, r *http.Request) {
			req := UiInput{
				w:    w,
				r:    r,
				done: make(chan struct{}),
			}
			RAWINPUT <- req
			<-req.done
		})
	STOP <- http.ListenAndServe(":11833", nil)
}

const networkSleep = 30 * time.Second

func (StartTcpListener) output() {
	bad := func() {
		RAWINPUT <- BadNetwork{}
		time.Sleep(networkSleep)
	}
	for {
		conn, err := net.Dial("tcp", serverTcpUrl)
		if err != nil {
			bad()
			continue
		}

		RAWINPUT <- GoodNetwork{}

		auth := makeTcpAuth(
			<-MYID, <-AUTHCODE, (<-MYKEYS).sign.secret)
		n, err := conn.Write(auth)
		if n != len(auth) {
			bad()
			continue
		}
		if err != nil {
			bad()
			continue
		}

		for {
			rawLen := make([]byte, 4)
			n, err := conn.Read(rawLen)
			if n != 4 {
				bad()
				break
			}
			if err != nil {
				bad()
				break
			}

			msgLen := decodeInt(rawLen)
			msg := make([]byte, msgLen)
			n, err = conn.Read(msg)
			if n != msgLen {
				bad()
				break
			}
			if err != nil {
				bad()
				break
			}

			RAWINPUT <- MsgFromServer(msg)
		}
	}
}

func main() {
	// initKeys()
	// go uiServer()
	// go tcpListener()

	STATE <- Start{}

	go func() {
		for {
			STATE <- ((<-RAWINPUT).update(<-STATE))
		}
	}()

	go func() {
		for {
			go (<-STATE).output()
		}
	}()

	fmt.Println(<-STOP)
}

type Start struct{}

func (Start) output() {
	STATE <- StartTcpListener{}
	STATE <- StartUiServer{}
	STATE <- GetUserId{}
	STATE <- GetCryptoKeys{}
	STATE <- GetAuthCode{}
}

type GetAuthCode struct{}

func (GetAuthCode) output() {
	bad := func() {
		RAWINPUT <- BadNetwork{}
		time.Sleep(networkSleep)
	}
	for {
		resp, err := http.Get(serverHttpUrl + "/authcode")
		if err != nil {
			bad()
			continue
		}

		authCode := make([]byte, constants.AuthCodeLen)
		n, err := resp.Body.Read(authCode)
		if n != constants.AuthCodeLen {
			bad()
			continue
		}
		if err != nil {
			bad()
			continue
		}
		AUTHCODE <- authCode
	}
}

type StartTcpListener struct{}

type StartUiServer struct{}

type GetUserId struct{}

const myIdPath = "myId"

func (GetUserId) output() {
	myId, err := ioutil.ReadFile(cachePath(myIdPath))
	if err != nil {
		keys := <-MYKEYS
		myId = argon2.IDKey(
			append(keys.sign.public[:], keys.encrypt.public[:]...),
			[]byte{},
			60,
			256*1024,
			4,
			13)
		err = ioutil.WriteFile(cachePath(myIdPath), myId, 0500)
		if err != nil {
			STOP <- err
		}
	}
	for {
		MYID <- myId
	}
}

type GetCryptoKeys struct{}

func (GetCryptoKeys) output() {
	keys, err := readKeysFromFile()
	if err != nil {
		keys, err = makeNewKeys()
	}
	if err != nil {
		STOP <- err
	}
	for {
		MYKEYS <- keys
	}
}

func intPower(base, power int) int {
	result := 1
	for i := 0; i < power; i++ {
		result = result * base
	}
	return result
}

func decodeInt(bs []byte) int {
	// Most significant byte should be the last one (Little-Endian).
	result := 0
	for i, b := range bs {
		result += int(b) * intPower(256, i)
	}
	return result
}

type MsgFromServer []byte

type State interface {
	output()
}

type RawInput interface {
	update(State) State
}

func (UiInput) update(state State) State {
	return Todo{}
}

func (MsgFromServer) update(state State) State {
	return Todo{}
}

type Todo struct{}

func (Todo) output() {
}

type GoodNetwork struct{}

type BadNetwork struct{}

func (GoodNetwork) update(state State) State {
	msg := base64.StdEncoding.EncodeToString([]byte{4})
	return ToWebsocket(msg)
}

type ToWebsocket []byte

func (t ToWebsocket) output() {
	TOWEBSOCKET <- []byte(t)
}

func (BadNetwork) update(state State) State {
	msg := base64.StdEncoding.EncodeToString([]byte{3})
	return ToWebsocket(msg)
}

package main

import (
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"github.com/gorilla/websocket"
	"golang.org/x/crypto/argon2"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/nacl/sign"
	"net"
	"net/http"
	"time"
)

type UiInput struct {
	w    http.ResponseWriter
	r    *http.Request
	done chan struct{}
}

const serverUrl = "http://localhost:8002"

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

var stop = make(chan error)
var rawInputS = make(chan RawInput)
var toWebsocketS = make(chan []byte)
var authCodeS = make(chan []byte)

var publicSign *[32]byte
var secretSign *[64]byte
var publicEncrypt *[32]byte
var secretEncrypt *[32]byte
var myId []byte

func initKeys() {
	var err error

	publicSign, secretSign, err = sign.GenerateKey(rand.Reader)
	if err != nil {
		stop <- err
	}

	publicEncrypt, secretEncrypt, err = box.GenerateKey(rand.Reader)
	if err != nil {
		stop <- err
	}

	myId = argon2.IDKey(
		append(publicSign[:], publicEncrypt[:]...),
		[]byte{},
		60,
		256*1024,
		4,
		13)

	fmt.Println(myId)
}

func main() {
	initKeys()

	return

	go func() {
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
						websocket.TextMessage, <-toWebsocketS)
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
				rawInputS <- req
				<-req.done
			})
		stop <- http.ListenAndServe(":11833", nil)
	}()

	go func() {
		bad := func() {
			rawInputS <- BadNetwork{}
			time.Sleep(30 * time.Second)
		}
		for {
			conn, err := net.Dial("tcp", serverUrl)
			if err != nil {
				bad()
				continue
			}

			rawInputS <- GoodNetwork{}

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

				rawInputS <- MsgFromServer(msg)
			}
		}
	}()

	stateS := make(chan State)

	go func() {
		state := InitState{}
		for {
			stateS <- (<-rawInputS).update(state)
		}
	}()

	go func() {
		for {
			(<-stateS).output()
		}
	}()

	fmt.Println(<-stop)
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

type InitState struct{}

func (InitState) output() {
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
	toWebsocketS <- []byte(t)
}

func (BadNetwork) update(state State) State {
	msg := base64.StdEncoding.EncodeToString([]byte{3})
	return ToWebsocket(msg)
}

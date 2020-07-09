package main

import (
	"encoding/base64"
	"errors"
	"fmt"
	"github.com/gorilla/websocket"
	"github.com/zserge/webview"
	"golang.org/x/crypto/argon2"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"time"
)

type stateT struct {
	fatalErr         error
	websocketOutChan chan string
	toServerChan     chan []byte
}

func initState() stateT {
	return stateT{
		fatalErr: nil,
	}
}

type startWebViewT struct{}

const port = "17448"
const baseUrl = "http://localhost:" + port

func (startWebViewT) io(ch chan inputT) {
	go startTcpConn(ch)
	go runWebserver(ch)
	w := webview.New(true)
	defer w.Destroy()
	w.SetTitle("BigWebThing")
	w.SetSize(800, 600, webview.HintNone)
	w.Navigate(baseUrl + "/static/index.html")
	w.Run()
}

func initOutputs() []outputT {
	return []outputT{
		startWebViewT{},
	}
}

const serverUrl = "http://localhost:3001"

type BadTcpT struct {
	err error
}

type ToFrontendT struct {
	msg string
	ch  chan string
}

func encodeString(s string) []byte {
	asBytes := []byte(s)
	length := encodeInt32(len(asBytes))
	return append(length, asBytes...)
}

type restartTcpT struct{}

func (restartTcpT) io(ch chan inputT) {
	time.Sleep(time.Second * 30)
	startTcpConn(ch)
}

func (b BadTcpT) update(state stateT) (stateT, []outputT) {
	errAsBytes := encodeString(b.err.Error())
	encoded := make([]byte, len(errAsBytes)+1)
	encoded[0] = 6
	copy(encoded[1:], errAsBytes)
	asString := base64.StdEncoding.EncodeToString(encoded)
	return state, []outputT{
		ToFrontendT{msg: asString, ch: state.websocketOutChan},
		restartTcpT{}}
}

func (t ToFrontendT) io(ch chan inputT) {
	t.ch <- t.msg
}

type toServerChanT chan []byte

func (t toServerChanT) update(state stateT) (stateT, []outputT) {
	state.toServerChan = chan []byte(t)
	return state, []outputT{}
}

func startTcpConn(ch chan inputT) {
	conn, err := net.Dial("tcp", serverUrl)
	if err != nil {
		ch <- BadTcpT{err}
		return
	}

	toServerChan := make(chan []byte)
	ch <- toServerChanT(toServerChan)

	killChan := make(chan struct{})

	var kill = func(err error) {
		ch <- BadTcpT{err}
		killChan <- struct{}{}
	}

	go func() {
		for {
			toServer := <-toServerChan
			n, err := conn.Write(toServer)
			if n != len(toServer) {
				kill(errors.New("wrong number of bytes written"))
			}
			if err != nil {
				kill(err)
			}
		}
	}()

	go func() {
		for {
			rawLen := make([]byte, 4)
			n, err := conn.Read(rawLen)
			if n != 4 {
				kill(errors.New("couldn't read length bytes"))
			}
			if err != nil {
				kill(err)
			}

			msgLen := decodeInt(rawLen)
			msg := make([]byte, msgLen)
			n, err = conn.Read(msg)
			if n != msgLen {
				kill(errors.New("couldn't read message bytes"))
			}
			if err != nil {
				kill(err)
			}

			ch <- msgFromServerT(msg)
		}
	}()

	<-killChan
}

type msgFromServerT []byte

func (m msgFromServerT) update(state stateT) (stateT, []outputT) {
	asBytes := []byte(m)
	encoded := make([]byte, len(asBytes)+1)
	encoded[0] = 1

	copy(encoded[1:], asBytes)
	return state, []outputT{
		ToFrontendT{
			msg: base64.StdEncoding.EncodeToString(encoded),
			ch:  state.websocketOutChan,
		},
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

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func websocketHandler(
	w http.ResponseWriter, r *http.Request, ch chan inputT) {

	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		ch <- fatalErrT{err}
		return
	}

	outChan := make(chan string)

	ch <- websocketOpenT(outChan)

	go func() {
		for {
			msg := <-outChan
			err := conn.WriteMessage(
				websocket.TextMessage, []byte(msg))
			if err != nil {
				ch <- fatalErrT{err}
				return
			}
		}
	}()

	for {
		_, p, err := conn.ReadMessage()
		if err != nil {
			ch <- fatalErrT{err}
			return
		}
		ch <- fromWebsocketT(string(p))
	}
}

type fromWebsocketT string

func (f fromWebsocketT) update(state stateT) (stateT, []outputT) {
	bytes, err := base64.StdEncoding.DecodeString(string(f))
	if err != nil {
		state.fatalErr = errors.New(
			"bad base64 from frontend: " + string(f))
		return state, []outputT{}
	}
	if len(bytes) == 0 {
		state.fatalErr = errors.New("zero bytes from frontend")
		return state, []outputT{}
	}

	switch bytes[0] {
	case 0:
		return sendToServer(bytes[1:], state)
	case 1:
		return getPow(bytes[1:], state)
	case 2:
		return cacheGet(bytes[1:], state)
	case 3:
		return cacheSet(bytes[1:], state)
	case 4:
		return cacheDelete(bytes[1:], state)
	}

	state.fatalErr = errors.New(
		"bad message indicator from frontend")
	return state, []outputT{}
}

const clientDataDir = "clientData"

func cacheDelete(raw []byte, state stateT) (stateT, []outputT) {
	err := os.Remove(clientDataDir + "/" + string(raw))
	if err != nil {
		state.fatalErr = err
	}
	return state, []outputT{}
}

func cacheSet(raw []byte, state stateT) (stateT, []outputT) {
	keyLen := decodeInt(raw[:4])
	keyBytes := raw[4 : 4+keyLen]
	key := string(keyBytes)
	blob := raw[4+keyLen:]
	err := ioutil.WriteFile(clientDataDir+"/"+key, blob, 0600)
	if err != nil {
		return state, []outputT{ToFrontendT{
			msg: badCache(keyBytes, err),
			ch:  state.websocketOutChan}}
	}
	return state, []outputT{}
}

func badCache(rawKey []byte, err error) string {
	errBytes := []byte(err.Error())
	rawLen := len(rawKey)
	errLen := len(errBytes)
	encoded := make([]byte, rawLen+errLen+1)
	copy(encoded[1:rawLen+1], rawKey)
	copy(encoded[rawLen+1:], errBytes)
	return base64.StdEncoding.EncodeToString(encoded)
}

func cacheGet(raw []byte, state stateT) (stateT, []outputT) {
	filepath := clientDataDir + "/" + string(raw)

	blob, err := ioutil.ReadFile(filepath)
	if os.IsNotExist(err) {
		encoded := make([]byte, len(raw)+1)
		encoded[0] = 5
		copy(encoded[1:], raw)
		return state, []outputT{ToFrontendT{
			msg: base64.StdEncoding.EncodeToString(encoded),
			ch:  state.websocketOutChan}}
	}

	if err != nil {
		return state, []outputT{ToFrontendT{
			msg: badCache(raw, err),
			ch:  state.websocketOutChan}}
	}

	encoded := make([]byte, len(blob)+1)
	encoded[0] = 0
	copy(encoded[1:], blob)
	return state, []outputT{ToFrontendT{
		msg: base64.StdEncoding.EncodeToString(encoded),
		ch:  state.websocketOutChan}}
}

type PowInfo struct {
	difficulty byte
	unique     []byte
}

func decodeGetPow(raw []byte) (PowInfo, error) {
	var powInfo PowInfo
	if len(raw) != 9 {
		return powInfo, errors.New(
			"raw POW info is not 9 bytes long")
	}

	powInfo.difficulty = raw[0]
	powInfo.unique = raw[1:]
	return powInfo, nil
}

func encodeInt32(theInt int) []byte {
	result := make([]byte, 4)
	for i, _ := range result {
		result[i] = byte((theInt >> (i * 8)) & 0xFF)
	}
	return result
}

func encodeInt64(theInt int) []byte {
	// Most significant byte is the last one. It only works
	// for up to about 10^14, but this is enough for id numbers,
	// since I'm not too bothered about having more than that
	// number of users.
	result := make([]byte, 8)
	for i, _ := range result {
		result[i] = byte((theInt >> (i * 8)) & 0xFF)
	}
	return result
}

func isDifficult(hash []byte, difficulty byte) bool {
	for _, b := range hash {
		if b < difficulty {
			return false
		}
	}
	return true
}

func getPow(raw []byte, state stateT) (stateT, []outputT) {
	powInfo, err := decodeGetPow(raw)
	if err != nil {
		state.fatalErr = err
		return state, []outputT{}
	}

	counter := 0

	for {
		candidate := encodeInt64(counter)
		hash := argon2.IDKey(
			candidate, powInfo.unique, 1, 64*1024, 4, 32)
		if isDifficult(hash, powInfo.difficulty) {
			pow := append(powInfo.unique, candidate...)
			encoded := make([]byte, 17)
			encoded[0] = 2
			copy(encoded[1:], pow)
			toFrontend := ToFrontendT{
				msg: base64.StdEncoding.EncodeToString(encoded),
				ch:  state.websocketOutChan,
			}
			return state, []outputT{toFrontend}
		}
		counter += 1
	}
}

func sendToServer(raw []byte, state stateT) (stateT, []outputT) {
	return state, []outputT{}
}

func runWebserver(ch chan inputT) {
	http.Handle("/static/", http.FileServer(http.Dir("")))
	http.HandleFunc(
		"/websocket",
		func(w http.ResponseWriter, r *http.Request) {
			websocketHandler(w, r, ch)
		})
	fmt.Println(http.ListenAndServe(":"+port, nil))
}

type fatalErrT struct {
	err error
}

func (f fatalErrT) update(state stateT) (stateT, []outputT) {
	state.fatalErr = f.err
	return state, []outputT{}
}

type websocketOpenT chan string

func (w websocketOpenT) update(state stateT) (stateT, []outputT) {
	state.websocketOutChan = chan string(w)
	return state, []outputT{}
}

type inputT interface {
	update(stateT) (stateT, []outputT)
}

type outputT interface {
	io(chan inputT)
}

func main() {
	state := initState()
	outputs := initOutputs()
	inputChannel := make(chan inputT)
	for state.fatalErr == nil {
		for _, output := range outputs {
			go output.io(inputChannel)
		}
		input := <-inputChannel
		state, outputs = input.update(state)
	}
	fmt.Println(state.fatalErr)
}

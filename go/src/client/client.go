package main

import (
	"encoding/base64"
	"errors"
	"fmt"
	"github.com/gorilla/websocket"
	"github.com/zserge/webview"
	"golang.org/x/crypto/argon2"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"strings"
)

type KeysForName struct {
	sign    []byte
	encrypt []byte
	id      []byte
}

func (r KeysForName) routerS(ch fromServerChansT) {
	ch.keysForName <- r
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

func parseRetrievedKeys(raw []byte) (KeysForName, error) {
	if len(raw) != 74 {
		return KeysForName{}, errors.New(
			"Raw retrieived keys is not 74 bytes")
	}

	return KeysForName{
		sign:    raw[:32],
		encrypt: raw[32:64],
		id:      raw[64:],
	}, nil
}

type ProofOfWorkInfo struct {
	difficulty byte
	unique     []byte
}

func (p ProofOfWorkInfo) routerS(ch fromServerChansT) {
	ch.proofOfWorkInfo <- p
}

func (p ProofOfWorkInfo) routerU(ch fromUiChansT) {
	ch.getProofOfWork <- p
}

func parseProofOfWorkInfo(raw []byte) (ProofOfWorkInfo, error) {
	if len(raw) != 17 {
		return ProofOfWorkInfo{},
			errors.New("Raw proof of work info is not 17 bytes.")
	}

	return ProofOfWorkInfo{
		difficulty: raw[0],
		unique:     raw[1:],
	}, nil
}

type AuthCode []byte

func (a AuthCode) routerS(ch fromServerChansT) {
	ch.authCode <- []byte(a)
}

func parseAuthCode(raw []byte) (AuthCode, error) {
	if len(raw) != 16 {
		return nil, errors.New("Raw auth code is not 16 bytes.")
	}

	return AuthCode(raw), nil
}

type FromServer interface {
	routerS(fromServerChansT)
}

func parseMsg(raw []byte) (FromServer, error) {
	length := len(raw)
	if length == 0 {
		return nil, errors.New("empty message")
	}

	switch raw[0] {
	case 1:
		return parseRetrievedKeys(raw[1:])
	case 2:
		return parseProofOfWorkInfo(raw[1:])
	case 3:
		return parseAuthCode(raw[1:])
	}

	return nil, errors.New(
		"Bad indicator in message from server.")
}

func cacher(c cacheChansT, kill chan error) {
	for {
		(<-c.to).run(c, kill)
	}
}

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func websocketHandler(
	w http.ResponseWriter, r *http.Request, c uiChansT) {

	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println(err)
		return
	}

	go func() {
		for {
			msg := <-c.to
			err := conn.WriteMessage(
				websocket.TextMessage, []byte(msg))
			if err != nil {
				fmt.Println(err)
				return
			}
		}
	}()

	for {
		_, p, err := conn.ReadMessage()
		if err != nil {
			fmt.Println(err)
			return
		}

		msg, err := parseFromUi(string(p))
		if err != nil {
			fmt.Println(err)
			return
		}
		msg.routerU(c.from)
	}
}

type FromUi interface {
	routerU(fromUiChansT)
}

func parseFromUi(raw string) (FromUi, error) {
	bytes, err := base64.StdEncoding.DecodeString(raw)
	if err != nil {
		return nil, err
	}
	if len(bytes) == 0 {
		return nil, errors.New("no bytes in message")
	}

	switch bytes[0] {
	case 0:
		return ToServer(bytes[1:]), nil
	case 1:
		return parseProofOfWorkInfo(bytes[1:])
	case 2:
		return CacheGet(string(bytes[1:])), nil
	case 3:
		return parseCacheSet(bytes[1:])
	case 4:
		return CacheDelete(string(bytes[1:])), nil
	case 5:
		return SendMessage(string(bytes[1:])), nil
	}

	return nil, errors.New("Bad indicator byte in message form UI.")
}

type SendMessage string

func (s SendMessage) routerU(ch fromUiChansT) {
	ch.sendMessage <- string(s)
}

type ToServer []byte

func (t ToServer) routerU(ch fromUiChansT) {
	ch.toServer <- []byte(t)
}

type CacheGet string

func (c CacheGet) routerU(ch fromUiChansT) {
	ch.cache.get <- string(c)
}

type CacheDelete string

func (c CacheDelete) routerU(ch fromUiChansT) {
	ch.cache.remove <- string(c)
}

type CacheSet struct {
	key   string
	value []byte
}

func (c CacheSet) routerU(ch fromUiChansT) {
	ch.cache.set <- c
}

func parseCacheSet(raw []byte) (CacheSet, error) {
	lenRaw := len(raw)
	if lenRaw < 4 {
		return CacheSet{}, errors.New("Raw Cache Set is less than 4 bytes.")
	}
	keyLen := decodeInt(raw[:4])
	if lenRaw < 4+keyLen {
		return CacheSet{}, errors.New("Raw Cache Set is too short.")
	}
	key := string(raw[4 : 4+keyLen])
	value := raw[4+keyLen:]
	return CacheSet{
		key:   key,
		value: value,
	}, nil
}

func ui(c uiChansT, kill chan error) {
	http.Handle("/static/", http.FileServer(http.Dir("")))
	http.HandleFunc(
		"/uploadFile/",
		func(w http.ResponseWriter, r *http.Request) {
			c.from.cache.setHandle <- setHandleT{
				handle: r.Body,
				name:   strings.Split(r.URL.Path, "/")[2],
			}
		})
	http.HandleFunc(
		"/websocket",
		func(w http.ResponseWriter, r *http.Request) {
			websocketHandler(w, r, c)
		})
	fmt.Println(http.ListenAndServe(":"+port, nil))
}

type cacheCmdT interface {
	run(cacheChansT, chan error)
}

const clientDataDir = "clientData"

func (c CacheGet) run(ch cacheChansT, crash chan error) {
	filepath := clientDataDir + "/" + string(c)
	contents, err := ioutil.ReadFile(filepath)
	if err != nil {
		ch.from.bad <- badCacheT{
			key: string(c),
			err: err}
		return
	}

	ch.from.got <- cacheGotT{
		key:   string(c),
		value: contents}
}

func (c CacheSet) run(ch cacheChansT, crash chan error) {
	filepath := clientDataDir + "/" + c.key
	err := ioutil.WriteFile(filepath, c.value, 0600)
	if err != nil {
		ch.from.bad <- badCacheT{
			key: c.key,
			err: err}
	}
}

func (c CacheDelete) run(ch cacheChansT, crash chan error) {
	filepath := clientDataDir + "/" + string(c)
	err := os.Remove(filepath)
	if err != nil {
		ch.from.bad <- badCacheT{
			key: string(c),
			err: err}
	}
}

const port = "17448"

const baseUrl = "http://localhost:" + port

func window() {
	w := webview.New(true)
	defer w.Destroy()
	w.SetTitle("BigWebThing")
	w.SetSize(800, 600, webview.HintNone)
	w.Navigate(baseUrl + "/static/index.html")
}

type cacheGotT struct {
	key   string
	value []byte
}

type badCacheT struct {
	key string
	err error
}

type fromCacheChansT struct {
	got chan cacheGotT
	bad chan badCacheT
}

type fromServerChansT struct {
	keysForName     chan KeysForName
	proofOfWorkInfo chan ProofOfWorkInfo
	authCode        chan []byte
	bad             chan error
}

type serverChansT struct {
	from fromServerChansT
	to   chan []byte
}

type cacheChansT struct {
	from fromCacheChansT
	to   chan cacheCmdT
}

type setHandleT struct {
	handle io.ReadCloser
	name   string
}

type fromUiCacheChansT struct {
	get       chan string
	set       chan CacheSet
	remove    chan string
	setHandle chan setHandleT
}

type fromUiChansT struct {
	toServer       chan []byte
	getProofOfWork chan ProofOfWorkInfo
	cache          fromUiCacheChansT
	sendMessage    chan string
}

type uiChansT struct {
	from fromUiChansT
	to   chan []byte
}

type chansT struct {
	server serverChansT
	cache  cacheChansT
	ui     uiChansT
}

func initChans() chansT {
	return chansT{
		server: serverChansT{
			from: fromServerChansT{
				keysForName:     make(chan KeysForName, 1),
				proofOfWorkInfo: make(chan ProofOfWorkInfo, 1),
				authCode:        make(chan []byte, 1),
				bad:             make(chan error, 1),
			},
			to: make(chan []byte, 1),
		},
		cache: cacheChansT{
			from: fromCacheChansT{
				got: make(chan cacheGotT, 1),
				bad: make(chan badCacheT, 1),
			},
			to: make(chan cacheCmdT, 1),
		},
		ui: uiChansT{
			from: fromUiChansT{
				toServer:       make(chan []byte, 1),
				getProofOfWork: make(chan ProofOfWorkInfo, 1),
				cache: fromUiCacheChansT{
					get:    make(chan string, 1),
					set:    make(chan CacheSet, 1),
					remove: make(chan string, 1),
				},
				sendMessage: make(chan string, 1),
			},
			to: make(chan []byte, 1),
		},
	}
}

const serverUrl = "http://localhost:3001"

func tcpConn(ch serverChansT, crash chan error) {
	conn, err := net.Dial("tcp", serverUrl)
	if err != nil {
		ch.from.bad <- err
		return
	}

	go func() {
		for {
			toServer := <-ch.to
			n, err := conn.Write(toServer)
			if n != len(toServer) {
				ch.from.bad <- errors.New(
					"wrong number of bytes written")
				return
			}
			if err != nil {
				ch.from.bad <- err
				return
			}
		}
	}()

	go func() {
		for {
			rawLen := make([]byte, 4)
			n, err := conn.Read(rawLen)
			if n != 4 {
				ch.from.bad <- errors.New(
					"couldn't read length bytes")
				return
			}
			if err != nil {
				ch.from.bad <- err
				return
			}

			msgLen := decodeInt(rawLen)
			msg := make([]byte, msgLen)
			n, err = conn.Read(msg)
			if n != msgLen {
				ch.from.bad <- errors.New(
					"couldn't read message bytes")
				return
			}
			if err != nil {
				ch.from.bad <- err
				return
			}

			parsed, err := parseMsg(msg)
			if err != nil {
				ch.from.bad <- err
				return
			}

			parsed.routerS(ch.from)
		}
	}()

	<-crash
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

func isDifficult(hash []byte, difficulty byte) bool {
	for _, b := range hash {
		if b < difficulty {
			return false
		}
	}
	return true
}

func makeProofOfWork(info ProofOfWorkInfo) []byte {
	counter := 0
	for {
		candidate := encodeInt64(counter)
		hash := argon2.IDKey(
			candidate, info.unique, 1, 64*1024, 4, 32)
		if isDifficult(hash, info.difficulty) {
			pow := make([]byte, 24)
			copy(pow, info.unique)
			copy(pow[16:], candidate)
			return pow
		}
	}
}

func sendMessage(draftId string, ch chansT, crash chan error) {
    ch.cache.to <-CacheGet(draftId)
    rawDraft := <-ch.cache.from.got
}

func main() {
	ch := initChans()
	crash := make(chan error)

	go tcpConn(ch.server, crash)
	go cacher(ch.cache, crash)
	go ui(ch.ui, crash)

	go window()

	for {
		select {
		case toServer := <-ch.ui.from.toServer:
			ch.server.to <- toServer

		case proofOfWorkInfo := <-ch.ui.from.getProofOfWork:
			ch.ui.to <- makeProofOfWork(proofOfWorkInfo)

		case cacheGet := <-ch.ui.from.cache.get:
			ch.cache.to <- CacheGet(cacheGet)

		case cacheSet := <-ch.ui.from.cache.set:
			ch.cache.to <- CacheSet(cacheSet)

		case cacheRemove := <-ch.ui.from.cache.remove:
			ch.cache.to <- CacheDelete(cacheRemove)

        case draftId := <-ch.ui.from.sendMessage:
            go sendMessage(draftId, ch)
		}
	}

	fmt.Println(<-crash)
}

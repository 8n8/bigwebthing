package main

import (
	"bytes"
	"goji.io"
	"goji.io/pat"
	"constants"
	"crypto/rand"
	"crypto/sha256"
	"encoding/base64"
	"errors"
	"fmt"
	"github.com/gorilla/websocket"
	"golang.org/x/crypto/argon2"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/nacl/sign"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
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
var INPUT = make(chan RawInput)
var TOWEBSOCKET = make(chan []byte)
var AUTHCODE = make(chan []byte, 1)
var MYKEYS = make(chan MyKeys, 1)
var MYID = make(chan []byte, 1)
var CACHELOCK sync.Mutex
var POWINFO = make(chan PowInfo, 1)
var CONTACTS = make(chan map[string]struct{}, 1)
var LOG = make(chan string)
var UNIQUEID chan string

type bytesliceSet interface {
	insert([]byte)
	remove([]byte)
	contains([]byte) bool
}

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
	file, err := os.Op
	return filepath.Join(frontendDir, filename)
}

var frontendDir = filepath.Join(homeDir, "frontend")

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

func (StartUiServer) output() {
	mux := goji.NewMux()
	mux.HandleFunc(
		pat.Get("/websocket"),
		func(w http.ResponseWriter, r *http.Request) {
			conn, err := upgrader.Upgrade(w, r, nil)
			if err != nil {
				panic(err)
			}

			for {
				err := conn.WriteMessage(
					websocket.TextMessage, <-TOWEBSOCKET)
				if err != nil {
					panic(err)
				}
			}
		})
	mux.HandleFunc(
		pat.Get("/cache/get/:key"),
		func(w http.ResponseWriter, r *http.Request) {
			path := cachePath(pat.Param(r, "key"))
			CACHELOCK.Lock()
			defer CACHELOCK.Unlock()
			file, err := os.Open(path)
			if err != nil {
				panic(err)
			}
			n, err := io.Copy(w, file)
			if n == 0 {
				panic("did not read any bytes from file", path)
			}
			if err != nil {
				panic(err)
			}
		})

	mux.HandleFunc(
		pat.Post("/cache/set/:key"),
		func(w http.ResponseWriter, r *http.Request) {
			path := cachePath(pat.Param(r, "key"))
			CACHELOCK.Lock()
			defer CACHELOCK.Unlock()
			file, err := os.Create(path)
			if err != nil {
				panic(err)
			}
			n, err := io.Copy(file, r.Body)
			if n == 0 {
				panic("no bytes in request body")
			}
			if err != nil {
				panic(err)
			}
		})

	mux.HandleFunc(
		pat.Post("/cache/delete/:key"),
		func(w http.ResponseWriter, r *http.Request) {
			path := cachePath(pat.Param(r, "key"))
			CACHELOCK.Lock()
			defer CACHELOCK.Unlock()
			err := os.Remove(path)
			if err != nil {
				panic(err)
			}
		})

	mux.HandleFunc(
		pat.Post("/sendmessage/:draftid"),
		func(w http.ResponseWriter, r *http.Request) {
		})

	mux.HandleFunc(
		pat.Get("/getunique"),
		func(w http.ResponseWriter, _ *http.Request) {
			n, err := w.Write(<-UNIQUEID)
			if n == 0 {
				panic("didn't write any bytes to unique ID request")
			}
			if err != nil {
				panic(err)
			}
		})

	panic(http.ListenAndServe(":11833", nil))
}

const networkSleep = 30 * time.Second

func tcpListenTillFail(conn net.Conn) {
	auth := makeTcpAuth(
		<-MYID, <-AUTHCODE, (<-MYKEYS).sign.secret)
	n, err := conn.Write(auth)
	if n != len(auth) {
		return
	}
	if err != nil {
		return
	}

	for {
		rawLen := make([]byte, 4)
		n, err := conn.Read(rawLen)
		if n != 4 {
			return
		}
		if err != nil {
			return
		}

		msgLen := decodeInt(rawLen)
		msg := make([]byte, msgLen)
		n, err = conn.Read(msg)
		if n != msgLen {
			return
		}
		if err != nil {
			return
		}

		INPUT <- MsgFromServer(msg)
	}
}

func (StartTcpListener) output() {
	for {
		conn, err := net.Dial("tcp", serverTcpUrl)
		if err != nil {
			INPUT <- BadNetwork{}
			time.Sleep(networkSleep)
			continue
		}

		INPUT <- GoodNetwork{}

		tcpListenTillFail(conn)
		INPUT <- BadNetwork{}
		time.Sleep(networkSleep)
	}
}

func main() {
	STATE <- Start{}

	go func() {
		for {
			STATE <- ((<-INPUT).update(<-STATE))
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
	STATE <- GetPowInfo{}
	STATE <- StartLogger{}
	STATE <- StartUniqueId{}
}

type StartLogger struct{}

func (StartLogger) output() {
	f, err := os.OpenFile(logPath, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0600)
	if err != nil {
		panic(err)
	}

	for {
		msg := fmt.Sprintf("%v   %s\n", time.Now(), <-LOG)
		n, err := f.Write([]byte(msg))
		if n == 0 {
			panic("log message was empty")
		}
		if err != nil {
			panic(err)
		}
	}
}

type GetPowInfo struct{}

func (GetPowInfo) output() {
	bad := func() {
		INPUT <- BadNetwork{}
		time.Sleep(networkSleep)
	}
	for {
		resp, err := http.Get(serverHttpUrl + "/proofofworkinfo")
		if err != nil {
			bad()
			continue
		}

		raw := make([]byte, 17)
		n, err := resp.Body.Read(raw)
		if n != 17 {
			bad()
			continue
		}
		if err != nil {
			bad()
			continue
		}

		POWINFO <- PowInfo{
			difficulty: raw[0],
			unique:     raw[1:],
		}
	}
}

type GetAuthCode struct{}

func (GetAuthCode) output() {
	bad := func() {
		INPUT <- BadNetwork{}
		time.Sleep(networkSleep)
	}
	for {
		resp, err := http.Get(serverHttpUrl + "/authcode")
		if err != nil {
			bad()
			continue
		}

		authCode := make([]byte, constants.AuthCodeLength)
		n, err := resp.Body.Read(authCode)
		if n != constants.AuthCodeLength {
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

func (u UiInput) update(state State) State {
	return u
}

type UiInWithUrl struct {
	w    http.ResponseWriter
	r    *http.Request
	done chan struct{}
	path string
}

func (u UiInput) output() {
	INPUT <- UiInWithUrl{
		w:    u.w,
		r:    u.r,
		done: u.done,
		path: u.r.URL.Path,
	}
}

type HttpFail struct {
	w      http.ResponseWriter
	status int
	msg    []byte
	done   chan struct{}
}

func (h HttpFail) output() {
	h.w.WriteHeader(h.status)
	h.w.Write(h.msg)
	h.done <- struct{}{}
}

func routeErr(msg string) (Route, error) {
	return *new(Route), errors.New(msg)
}

func cacheGetP(raw []string) (Route, error) {
	if len(raw) != 3 {
		return routeErr("path not 3 elements")
	}

	if !sliceEq(raw[:2], "cache", "get") {
		return routeErr("path should begin \"cache/get\"")
	}

	if len(raw[2]) == 0 {
		return routeErr("key is empty")
	}

	return CacheGetRequest(raw[2]), nil
}

func cacheSetP(raw []string) (Route, error) {
	if len(raw) != 3 {
		return routeErr("path not 3 elements")
	}

	if !sliceEq(raw[:2], "cache", "set") {
		return routeErr("path should begin \"cache/set\"")
	}

	if len(raw[2]) == 0 {
		return routeErr("key is empty")
	}

	return CacheSetRequest(raw[2]), nil
}

func cacheDeleteP(raw []string) (Route, error) {
	if len(raw) != 3 {
		return routeErr("path not 3 elements")
	}

	if !sliceEq(raw[:2], "cache", "delete") {
		return routeErr("path should begin \"cache/delete\"")
	}

	if len(raw[2]) == 0 {
		return routeErr("key is empty")
	}

	return CacheDeleteRequest(raw[2]), nil
}

func whitelistRemoveP(raw []string) (Route, error) {
	if len(raw) != 2 {
		return routeErr("path not 2 elements")
	}

	if !sliceEq(raw[:2], "whitelist", "remove") {
		return routeErr("path should begin \"whitelist/remove\"")
	}

	return WhitelistRemove{}, nil
}

func whitelistAddP(raw []string) (Route, error) {
	if len(raw) != 2 {
		return routeErr("path not 2 elements")
	}

	if !sliceEq(raw[:2], "whitelist", "add") {
		return routeErr("path should begin \"whitelist/add\"")
	}

	return WhitelistAdd{}, nil
}

func sliceEq(s1 []string, s2 ...string) bool {
	if len(s1) != len(s2) {
		return false
	}

	for i, s := range s1 {
		if s != s2[i] {
			return false
		}
	}

	return true
}

func sendMessageP(raw []string) (Route, error) {
	if len(raw) != 2 {
		return routeErr("path not 2 elements")
	}

	if raw[0] != "sendmessage" {
		return routeErr("first item is not \"sendmessage\"")
	}

	if len(raw[1]) == 0 {
		return routeErr("draftId is empty")
	}

	return SendMessageRequest(raw[1]), nil
}

type SendMessageRequest string

func (s SendMessageRequest) handle(u UiInWithUrl) State {
	return ReadDraftAndRecipient{
		w:       u.w,
		r:       u.r,
		done:    u.done,
		draftId: string(s),
	}
}

func (w WhitelistRemove) handle(u UiInWithUrl) State {
	return ReadUnwhitelistee{
		w:    u.w,
		r:    u.r,
		done: u.done,
	}
}

type ReadUnwhitelistee struct {
	w    http.ResponseWriter
	r    *http.Request
	done chan struct{}
}

func (r ReadUnwhitelistee) output() {
	unwhitelistee := make([]byte, constants.IdLength)
	n, err := r.r.Body.Read(unwhitelistee)
	INPUT <- TriedReadingUnwhitelistee{
		w:             r.w,
		done:          r.done,
		n:             n,
		err:           err,
		myId:          <-MYID,
		authCode:      <-AUTHCODE,
		secretSign:    *(<-MYKEYS).sign.secret,
		unwhitelistee: unwhitelistee,
	}
}

type TriedReadingUnwhitelistee struct {
	w             http.ResponseWriter
	done          chan struct{}
	n             int
	err           error
	myId          []byte
	authCode      []byte
	secretSign    [64]byte
	unwhitelistee []byte
}

func (t TriedReadingUnwhitelistee) update(state State) State {
	fail := func(msg string) HttpFail {
		return HttpFail{
			w:      t.w,
			status: 400,
			msg:    []byte(msg),
			done:   t.done,
		}
	}
	if t.n != constants.IdLength {
		return fail("couldn't read unwhitelistee from body")
	}
	if t.err != nil {
		return fail(t.err.Error())
	}

	toSign := make([]byte, constants.MeaningLength+constants.AuthCodeLength+constants.IdLength)
	copy(toSign, constants.WhitelistRemove)
	copy(toSign[constants.MeaningLength:], t.authCode)
	copy(
		toSign[constants.MeaningLength+constants.AuthCodeLength:],
		t.unwhitelistee)
	signed := sign.Sign(t.myId, toSign, &t.secretSign)
	return WhitelistRequest{
		w:    t.w,
		body: signed,
		done: t.done,
		path: serverHttpUrl + "/whitelist/remove",
	}
}

type PowInfo struct {
	unique     []byte
	difficulty byte
}

type TriedReadingWhitelistee struct {
	authCode    []byte
	myId        []byte
	secretSign  [64]byte
	powInfo     PowInfo
	whitelistee []byte
	w           http.ResponseWriter
	done        chan struct{}
	n           int
	err         error
}

func (t TriedReadingWhitelistee) update(state State) State {
	fail := func(msg string) HttpFail {
		return HttpFail{
			w:      t.w,
			status: 400,
			msg:    []byte(msg),
			done:   t.done,
		}
	}
	if t.n != constants.IdLength {
		return fail("couldn't read whitelistee from body")
	}
	if t.err != nil {
		return fail(t.err.Error())
	}

	pow := makePow(t.powInfo)

	toSign := make([]byte, constants.MeaningLength+constants.AuthCodeLength+constants.IdLength)
	copy(toSign, constants.WhitelistAdd)
	copy(toSign[constants.MeaningLength:], t.authCode)
	copy(
		toSign[constants.MeaningLength+constants.AuthCodeLength:],
		t.whitelistee)
	signed := sign.Sign(append(pow, t.myId...), toSign, &t.secretSign)
	return WhitelistRequest{
		w:    t.w,
		body: signed,
		done: t.done,
		path: serverHttpUrl + "/whitelist/add",
	}
}

type WhitelistRequest struct {
	w    http.ResponseWriter
	body []byte
	done chan struct{}
	path string
}

func (w WhitelistRequest) output() {
	_, err := http.Post(
		w.path,
		"application/octet-stream",
		bytes.NewBuffer(w.body))
	INPUT <- SentWhitelistRequest{
		w:    w.w,
		err:  err,
		done: w.done,
	}
}

type SentWhitelistRequest struct {
	w    http.ResponseWriter
	err  error
	done chan struct{}
}

func (s SentWhitelistRequest) update(state State) State {
	if s.err != nil {
		return FailedRelay{
			w:      s.w,
			status: 500,
			msg:    []byte{},
			done:   s.done,
		}
	}
	return HttpOk(s.done)
}

type HttpOk chan struct{}

func (h HttpOk) output() {
	h <- struct{}{}
}

type FailedRelay struct {
	w      http.ResponseWriter
	status int
	msg    []byte
	done   chan struct{}
}

func (f FailedRelay) output() {
	f.w.WriteHeader(f.status)
	f.w.Write(f.msg)
	f.done <- struct{}{}
	INPUT <- BadNetwork{}
}

func makePow(info PowInfo) []byte {
	counter := 0
	toHash := make([]byte, 24)
	copy(toHash, info.unique)
	for {
		copy(toHash[16:], encodeUint32(counter))
		hash := argon2.IDKey(toHash, []byte{}, 1, 64*1024, 4, 32)
		counter += 1
		for _, b := range hash {
			if b < info.difficulty {
				continue
			}
		}
		return toHash
	}
}

func (c CacheDeleteRequest) handle(u UiInWithUrl) State {
	return UiCacheDelete{
		done: u.done,
		path: cachePath(string(c)),
	}
}

func (c CacheGetRequest) handle(u UiInWithUrl) State {
	return UiCacheGet{
		path: cachePath(string(c)),
		w:    u.w,
		done: u.done,
	}
}

type UiCacheGet struct {
	done chan struct{}
	path string
	w    http.ResponseWriter
}

func (c CacheSetRequest) handle(u UiInWithUrl) State {
	return UiCacheSet{
		done: u.done,
		path: cachePath(string(c)),
		r:    u.r.Body,
	}
}

type UiCacheSet struct {
	done chan struct{}
	path string
	r    io.Reader
}

func (u UiCacheSet) output() {
	CACHELOCK.Lock()
	file, err := os.Create(u.path)
	if err != nil {
		STOP <- err
	}
	n, err := io.Copy(file, u.r)
	CACHELOCK.Unlock()

	if n == 0 {
		STOP <- errors.New("did not write any bytes to new file")
	}
	if err != nil {
		STOP <- err
	}

	u.done <- struct{}{}
}

func (u UiCacheGet) output() {
	CACHELOCK.Lock()
	file, err := os.Open(u.path)
	if err != nil {
		STOP <- err
	}
	n, err := io.Copy(u.w, file)
	CACHELOCK.Unlock()

	if n == 0 {
		STOP <- errors.New("no bytes read from file: " + u.path)
	}
	if err != nil {
		STOP <- err
	}

	u.done <- struct{}{}
}

type UiCacheDelete struct {
	done chan struct{}
	path string
}

func (u UiCacheDelete) output() {
	CACHELOCK.Lock()
	err := os.Remove(u.path)
	CACHELOCK.Unlock()

	if err != nil {
		STOP <- err
	}

	u.done <- struct{}{}
}

func (w WhitelistAdd) handle(u UiInWithUrl) State {
	return ReadWhitelistee{
		w:    u.w,
		r:    u.r,
		done: u.done,
	}
}

type ReadWhitelistee struct {
	w    http.ResponseWriter
	r    *http.Request
	done chan struct{}
}

func (r ReadWhitelistee) output() {
	whitelistee := make([]byte, constants.IdLength)
	n, err := r.r.Body.Read(whitelistee)
	INPUT <- TriedReadingWhitelistee{
		authCode:    <-AUTHCODE,
		myId:        <-MYID,
		secretSign:  *(<-MYKEYS).sign.secret,
		powInfo:     <-POWINFO,
		whitelistee: whitelistee,
		w:           r.w,
		done:        r.done,
		n:           n,
		err:         err,
	}
}

type ReadDraftAndRecipient struct {
	w       http.ResponseWriter
	r       *http.Request
	done    chan struct{}
	draftId string
}

func (r ReadDraftAndRecipient) output() {
	recipient := make([]byte, constants.IdLength)
	n, httpErr := r.r.Body.Read(recipient)
	draft, fileErr := ioutil.ReadFile(cachePath(r.draftId))
	INPUT <- TriedReadingDraftAndRecipient{
		w:         r.w,
		r:         r.r,
		done:      r.done,
		n:         n,
		httpErr:   httpErr,
		draft:     draft,
		fileErr:   fileErr,
		recipient: recipient,
		draftId:   r.draftId,
	}
}




type TriedReadingDraftAndRecipient struct {
	w         http.ResponseWriter
	r         *http.Request
	done      chan struct{}
	n         int
	httpErr   error
	draft     []byte
	fileErr   error
	recipient []byte
	draftId   string
}

type Draft struct {
	id        string
	subject   string
	to        []byte
	time      string
	userInput string
	code      MaybeCode
	blobs     []Blob
}

type MaybeCode interface {
	f()
}

type NoCode struct{}

type Code struct {
	contents []byte
	filename string
}

type Blob struct {
	id       string
	mime     string
	filename string
	size     int
}

func parseString(raw []byte, pos int) (string, int, error) {
	bs, pos, err := parseBytes(raw, pos)
	if err != nil {
		return "", pos, err
	}
	return string(bs), pos, nil
}

func parseBytes(raw []byte, pos int) ([]byte, int, error) {
	length, pos, err := parseUint32(raw, pos)
	if err != nil {
		return []byte{}, pos, err
	}

	return raw[4:length], pos + length, nil
}

func parseDraft(raw []byte) (Draft, error) {
	var draft Draft
	pos := 0

	id, pos, err := parseString(raw, pos)
	if err != nil {
		return draft, err
	}

	subject, pos, err := parseString(raw, pos)
	if err != nil {
		return draft, err
	}

	to, pos, err := parseBytes(raw, pos)
	if err != nil {
		return draft, err
	}

	time_, pos, err := parseString(raw, pos)
	if err != nil {
		return draft, err
	}

	userInput, pos, err := parseString(raw, pos)
	if err != nil {
		return draft, err
	}

	code, pos, err := parseCode(raw, pos)
	if err != nil {
		return draft, err
	}

	blobs, pos, err := parseBlobs(raw, pos)
	if err != nil {
		return draft, err
	}

	return Draft{
		id:        id,
		subject:   subject,
		to:        to,
		time:      time_,
		userInput: userInput,
		code:      code,
		blobs:     blobs,
	}, nil
}

func parseBlob(raw []byte, pos int) (Blob, int, error) {
	var blob Blob

	id, pos, err := parseString(raw, pos)
	if err != nil {
		return blob, pos, err
	}

	mime, pos, err := parseString(raw, pos)
	if err != nil {
		return blob, pos, err
	}

	filename, pos, err := parseString(raw, pos)
	if err != nil {
		return blob, pos, err
	}

	size, pos, err := parseUint32(raw, pos)
	if err != nil {
		return blob, pos, err
	}

	return Blob{
		id:       id,
		mime:     mime,
		filename: filename,
		size:     size,
	}, pos, nil
}

func parseUint32(raw []byte, pos int) (int, int, error) {
	if len(raw) < 4 {
		return 0, pos, errors.New("raw is less than 4 bytes long")
	}
	return decodeInt(raw[pos : pos+4]), pos + 4, nil
}

func parseBlobs(raw []byte, pos int) ([]Blob, int, error) {

	length, pos, err := parseUint32(raw, pos)
	if err != nil {
		return []Blob{}, pos, err
	}

	blobs := make([]Blob, length)

	for i, _ := range blobs {
		blob, pos, err := parseBlob(raw, pos)
		if err != nil {
			return blobs, pos, err
		}

		blobs[i] = blob
	}

	return blobs, pos, nil
}

func parseCode(raw []byte, pos int) (MaybeCode, int, error) {
	if len(raw) == 0 {
		return *new(MaybeCode), pos, errors.New("empty byte slice")
	}

	indicator := raw[0]
	pos += 1

	if indicator == 0 {
		return NoCode{}, pos, nil
	}

	if indicator != 1 {
		return *new(MaybeCode), pos, errors.New("indicator is not 0 or 1")
	}

	contents, pos, err := parseBytes(raw, pos)
	if err != nil {
		return *new(MaybeCode), pos, err
	}

	filename, pos, err := parseString(raw, pos)
	return Code{
		contents: contents,
		filename: filename,
	}, pos, nil
}

func (Code) f() {}

func (NoCode) f() {}

const maxChunkSize = 15500

func encodeUint32(theInt int) []byte {
	result := make([]byte, 4)
	for i, _ := range result {
		result[i] = byte((theInt >> (i * 8)) & 0xFF)
	}
	return result
}

func min(a, b int) int {
	if a < b {
		return a
	}
	return b
}

func ceilDiv(a, b int) int {
	if a == b {
		return 1
	}
	return (a / b) + 1
}

func chunkUpDraft(raw []byte) [][]byte {
	rawLen := len(raw)
	if rawLen <= maxChunkSize {
		return [][]byte{append([]byte{0x00}, raw...)}
	}

	hash := sha256.Sum256(raw)
	numChunks := ceilDiv(rawLen, maxChunkSize)
	chunks := make([][]byte, numChunks)
	for i, _ := range chunks {
		chunkStart := i * maxChunkSize
		chunkEnd := min((i+1)*maxChunkSize, rawLen)
		chunk := make([]byte, 1+32+4+chunkEnd-chunkStart)
		chunk[0] = 1
		copy(chunk[1:], hash[:])
		copy(chunk[1+32:], encodeUint32(i))
		copy(chunk[1+32+4:], raw[chunkStart:chunkEnd])
		chunks[i] = chunk
	}
	return chunks
}

func (t TriedReadingDraftAndRecipient) update(state State) State {
	fail := func(msg string) HttpFail {
		return HttpFail{
			w:      t.w,
			status: 400,
			msg:    []byte(msg),
			done:   t.done,
		}
	}
	if t.n != constants.IdLength {
		return fail("couldn't read recipient ID from body")
	}
	if t.httpErr != nil {
		return fail(t.httpErr.Error())
	}
	if t.fileErr != nil {
		return fail(t.fileErr.Error())
	}
	draft, err := parseDraft(t.draft)
	if err != nil {
		return fail(err.Error())
	}
	return StartSendingDraft{
		draftChunks: chunkUpDraft(t.draft),
		blobs:       draft.blobs,
		recipient:   t.recipient,
		w:           t.w,
		done:        t.done,
		draftId:     t.draftId,
	}
}

type StartSendingDraft struct {
	draftChunks [][]byte
	blobs       []Blob
	recipient   []byte
	w           http.ResponseWriter
	done        chan struct{}
	draftId     string
}

func (s StartSendingDraft) output() {
	errs := make(chan error)
	for _, chunk := range s.draftChunks {
		INPUT <- ChunkToSend{
			chunk:     chunk,
			recipient: s.recipient,
			errs:      errs,
		}
	}

	for _, blob := range s.blobs {
		INPUT <- BlobToSend{
			blob:      blob,
			recipient: s.recipient,
			errs:      errs,
		}
	}

	for _, _ = range s.draftChunks {
		for _, _ = range s.blobs {
			err := <-errs
			if err != nil {
				INPUT <- FailedSend{
					recipient: s.recipient,
					w:         s.w,
					done:      s.done,
					err:       err,
				}
				return
			}
		}
	}

	s.done <- struct{}{}
}

func (c ChunkToSend) update(state State) State {
	firstPart := make([]byte, 16+16+13)
	copy(firstPart, constants.SendMessage)
	copy(firstPart[16:], c.authCode)
	copy(firstPart[16+16:], c.recipient)
	encrypted := box.Seal(
		firstPart,
		c.chunk,
		&c.nonce,
		&c.recipientKey,
		c.keys.encrypt.secret)
	signed := sign.Sign(c.myId, encrypted, c.keys.sign.secret)
	return SendChunk{
		chunk: signed,
		errs:  c.errs,
	}
}

type SendChunk struct {
	chunk []byte
	errs  chan error
}

func (s SendChunk) output() {
	_, err := http.Post(
		serverHttpUrl+"/message/send",
		"application/octet-stream",
		bytes.NewBuffer(s.chunk))
	s.errs <- err
}

type ChunkToSend struct {
	chunk        []byte
	recipient    []byte
	recipientKey [32]byte
	keys         MyKeys
	myId         []byte
	authCode     []byte
	errs         chan error
	nonce        [24]byte
}

type BlobToSend struct {
	blob      Blob
	recipient []byte
	errs      chan error
}

func (b BlobToSend) update(state State) State {
	if b.blob.size <= maxChunkSize {
		return SendSmallBlob{
			path:      cachePath(b.blob.id),
			recipient: b.recipient,
			errs:      b.errs,
		}
	}

	return SendLargeBlob{
		id:        b.blob.id,
		recipient: b.recipient,
		errs:      b.errs,
	}
}

type SendSmallBlob struct {
	path      string
	recipient []byte
	errs      chan error
}

func (s SendSmallBlob) output() {
	CACHELOCK.Lock()
	contents, err := ioutil.ReadFile(s.path)
	CACHELOCK.Unlock()
	if err != nil {
		STOP <- err
	}
	INPUT <- ChunkToSend{
		recipient: s.recipient,
		errs:      s.errs,
		chunk:     append([]byte{0, 2}, contents...),
	}
}

type SendLargeBlob struct {
	id        string
	recipient []byte
	errs      chan error
}

func (s SendLargeBlob) output() {
}

type FailedSend struct {
	recipient []byte
	w         http.ResponseWriter
	done      chan struct{}
	err       error
}

func (f FailedSend) update(state State) State {
	return HttpFail{
		w:      f.w,
		status: 500,
		msg:    []byte(f.err.Error()),
		done:   f.done,
	}
}

type CacheGetRequest string

type CacheSetRequest string

type CacheDeleteRequest string

type WhitelistAdd struct{}

type WhitelistRemove struct{}

type Route interface {
	handle(UiInWithUrl) State
}

func parsePath(raw string) (Route, error) {
	parts := strings.Split(raw, "/")

	cacheGet, err := cacheGetP(parts)
	if err == nil {
		return cacheGet, nil
	}

	cacheSet, err := cacheSetP(parts)
	if err == nil {
		return cacheSet, nil
	}

	cacheDelete, err := cacheDeleteP(parts)
	if err == nil {
		return cacheDelete, nil
	}

	sendMessage, err := sendMessageP(parts)
	if err == nil {
		return sendMessage, nil
	}

	whitelistAdd, err := whitelistAddP(parts)
	if err == nil {
		return whitelistAdd, nil
	}

	whitelistRemove, err := whitelistRemoveP(parts)
	if err == nil {
		return whitelistRemove, nil
	}

	return *new(Route), errors.New("bad path: " + raw)
}

func (u UiInWithUrl) update(state State) State {
	route, err := parsePath(u.path)
	if err != nil {
		return HttpFail{
			w:      u.w,
			status: 400,
			msg:    []byte(err.Error()),
			done:   u.done,
		}
	}

	return route.handle(u)
}

func parseMessageFromServer(raw []byte) (EncryptedAndSigned, error) {
	if len(raw) < constants.MessageSendLength {
		return *new(EncryptedAndSigned), errors.New("message from server is too short")
	}

	return EncryptedAndSigned{
		fromId: raw[:constants.IdLength],
		signed: raw[constants.IdLength:],
	}
}

type EncryptedAndSigned struct {
	fromId []byte
	signed []byte
}

func (m MsgFromServer) update(state State) State {
	signed, err := parseMessageFromServer([]byte(m))
	if err != nil {
		return Stop{err}
	}
	return signed
}

func (e EncryptedAndSigned) output() {
	theirKeys, err := getTheirKeys(e.fromId)
	INPUT <- MsgFromServerContext{
		myId:          <-MYID,
		signed:        e.signed,
		fromId:        e.fromId,
		contacts:      <-CONTACTS,
		theirKeys:     theirKeys,
		theirKeysErr:  err,
		secretSign:    *(<-MYKEYS).sign.secret,
		secretEncrypt: *(<-MYKEYS).encrypt.secret,
	}
}

type TheirKeys struct {
	encrypt [32]byte
	sign    [32]byte
}

type MsgFromServerContext struct {
	myId          []byte
	signed        []byte
	fromId        []byte
	contacts      bytesliceSet
	theirKeys     TheirKeys
	theirKeysErr  error
	secretSign    [64]byte
	secretEncrypt [32]byte
}

type Encrypted struct {
	meaning   []byte
	recipient []byte
	encrypted []byte
	nonce     [constants.NonceLength]byte
}

func parseUnsigned(raw []byte) (Encrypted, error) {
	const afterMeaning = constants.MeaningLength
	const afterAuth = afterMeaning + constants.AuthCodeLength
	const afterId = afterAuth + constants.IdLength
	const afterNonce = afterId + constants.NonceLength

	if len(raw) < afterNonce+1 {
		return *new(Encrypted), errors.New("raw unsigned was too short")
	}

	var nonce [constants.NonceLength]byte
	copy(nonce[:], raw[afterId:])

	return Encrypted{
		meaning:   raw[:afterMeaning],
		recipient: raw[afterAuth:afterId],
		encrypted: raw[afterId:],
		nonce:     nonce,
	}, nil
}

func (m MsgFromServerContext) update(state State) State {
	if !m.contacts.contains(m.fromId) {
		return Log(fmt.Sprintf(
			"%v is not in my contacts",
			m.fromId))
	}

	if m.theirKeysErr != nil {
		return Log(fmt.Sprintf(
			"receiving message from %v failed because could not get their encryption keys: %v",
			m.fromId,
			m.theirKeysError))
	}

	unsigned, ok := sign.Open([]byte{}, m.signed, &m.theirKeys.sign)
	if !ok {
		return Panic("received bad signature from server")
	}

	encrypted, err := parseUnsigned(unsigned)
	if err != nil {
		return Log(fmt.Sprintf(
			"message from %v is not in the right format",
			m.fromId))
	}

	if !bytes.Equal(encrypted.meaning, constants.SendMessage) {
		return Log(fmt.Sprintf(
			"message from %v does not have the right meaning",
			m.fromId))
	}

	decrypted, ok := box.Open([]byte{}, encrypted.encrypted, &encrypted.nonce, &m.theirKeys.encrypt, &m.secretEncrypt)
	if !ok {
		return Log(fmt.Sprintf(
			"could not decrypt message from %v",
			m.fromId))
	}

	clientChunk, err := parseClientToClient(decrypted)
	if err != nil {
		return Log(fmt.Sprintf("error parsing client chunk: ", err))
	}

	return clientChunk.handle()
}

func parseClientToClient(raw []byte) (ClientChunk, error) {
	lenRaw := len(raw)
	if lenRaw == 0 {
		return *new(ClientChunk), errors.New("empty client chunk")
	}

	switch raw[0] {
	case 0:
		if lenRaw < 2 {
			return *new(ClientChunk),
				errors.New("empty small message")
		}
		return SmallClientChunk(raw[1:]), nil
	case 1:
		if lenRaw < 1+32+4 {
			return *new(ClientChunk),
				errors.New("large message segment too short")
		}
		return MessageSegment{
			hash:    raw[1:33],
			counter: decodeInt(raw[33:37]),
			chunk:   raw[37:],
		}, nil
	}
	return *new(ClientChunk),
		errors.New("bad indicator in client message chunk")
}

type ClientChunk interface {
	handle() State
}

const minAcknowledge = sign.Overhead + constants.MeaningLength + 8 + 32

func parseAcknowledgement(raw []byte) (ParsedSmall, error) {
	if len(raw) < minAcknowledge {
		return *new(ParsedSmall), errors.New("raw acknowledgement too small")
	}

	return ParsedAcknowledgement(raw), nil
}

func parseSmallBlob(raw []byte) (ParsedSmall, error) {

}

func parseSmallChunk(raw []byte) (ParsedSmall, error) {
	rawLen := len(raw)
	if rawLen == 0 {
		return *new(ParsedSmall), errors.New("empty raw small chunk")
	}

	switch raw[0] {
	case 1:
		return parseSmallMessage(raw[1:])

	case 2:
		return parseSmallBlob(raw[1:])

	case 3:
		return parseAcknowledgement(raw[1:])

	}

	return *new(ParsedSmall), errors.New("bad indicator byte on small chunk from server")
}

type ParsedSmall interface {
	handle() State
}

func (s SmallClientChunk) handle() State {
	parsed, err := parseSmallChunk([]byte(s))
	if err != nil {
		return Log(fmt.Sprintf(
			"problem parsing small chunk from server: %v",
			err))
	}
	return parsed.handle()
}

type MessageSegment struct {
	hash    []byte
	counter int
	chunk   []byte
}

func (m MessageSegment) handle() State {
	return m
}

type SmallClientChunk []byte

type Log string

var backendDir = filepath.Join(homeDir, "backend")

var logPath = filepath.Join(backendDir, "log")

var homeDir = "clientData"

func (log Log) output() {
	LOG <- string(log)
}

type Panic string

func (p Panic) output() {
	panic(string(p))
}

type DoNothing struct{}

func (DoNothing) output() {}

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

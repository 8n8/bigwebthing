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
	"strconv"
	"strings"
	"time"
	//     "crypto/sha256"
	//     "math"
)

type stateT struct {
	fatalErr         error
	waiters          []waitableT
	cacherChan       chan cacheCmdT
	websocketOutChan chan string
	toServerChan     chan []byte
}

type inT interface {
	router(waitableT) inputStateT
	update(stateT) (stateT, []outT)
}

type waitableT interface {
	expected(inT) (bool, inputStateT)
}

type inputStateT interface {
	update(state stateT) (stateT, []outT)
}

type outT interface {
	io(chan inT)
	fast() bool
}

func main() {
	state := stateT{
		fatalErr: nil,
		waiters:  []waitableT{},
	}

	outputs := []outT{
		startWebserverT{},
		startTcpConnT{},
		startCacherT{},
		startWebViewT{},
	}

	inputChannel := make(chan inT, 1)

	for state.fatalErr == nil {
		for _, output := range outputs {
			if output.fast() {
				output.io(inputChannel)
			} else {
				go output.io(inputChannel)
			}
		}

		in := <-inputChannel

		state, outputs = update(state, in)
	}

	fmt.Println(state.fatalErr)
}

func update(state stateT, in inT) (stateT, []outT) {
	for _, waiter := range state.waiters {
		relevant, inputState := waiter.expected(in)
		if relevant {
			return inputState.update(state)
		}
	}
	return in.update(state)
}

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

//
// type waitT struct {
//     pubKeys pubKeysWaitT
//     authCode authCodeWaitT
// }
//
// type authCodeWaitT interface {
//     update(stateT, authCodeT) (stateT, []outputT)
// }
//
// type pubKeysWaitT interface {
//     update(stateT, pubKeysT) (stateT, outputT)
// }
//
// type publicKeysT struct {
//     sign []byte
//     encrypt []byte
// }
//
// func initState() stateT {
// 	return stateT{
// 		fatalErr: nil,
// 	}
// }
//
type startWebViewT struct{}

func (startWebViewT) fast() bool { return false }

//
const baseUrl = "http://localhost:" + port

//
func (startWebViewT) io(ch chan inT) {
	w := webview.New(true)
	defer w.Destroy()
	w.SetTitle("BigWebThing")
	w.SetSize(800, 600, webview.HintNone)
	w.Navigate(baseUrl + "/static/index.html")
	w.Run()
}

//
// func initOutputs() []outputT {
// 	return []outputT{
// 		startCacherT{},
// 		startTcpConnT{},
// 		startWebserverT{},
// 		startWebViewT{},
// 	}
// }
//
type startCacherT struct{}

//

func (startCacherT) fast() bool { return false }

type startTcpConnT struct{}

func (startTcpConnT) fast() bool { return false }

type startWebserverT struct{}

func (startWebserverT) fast() bool { return false }

type cacherChanT struct {
	ch chan cacheCmdT
}

func (c cacherChanT) router(w waitableT) inputStateT {
	expected, inputState := w.expected(c)
	if !expected {
		return notExpectedT{}
	}

	return inputState
}

type cacheCmdT interface {
	run(ch chan inT)
}

func (startCacherT) io(ch chan inT) {
	cmdCh := make(chan cacheCmdT)
	ch <- cacherChanT{cmdCh}

	for {
		cmd := <-cmdCh
		cmd.run(ch)
	}
}

func (c cacherChanT) update(state stateT) (stateT, []outT) {
	state.cacherChan = c.ch
	return state, []outT{}
}

const serverUrl = "http://localhost:3001"

//
type BadTcpT struct {
	err error
}

func (b BadTcpT) router(w waitableT) inputStateT {
	expected, inputState := w.expected(b)
	if !expected {
		return notExpectedT{}
	}

	return inputState
}

type ToFrontendT struct {
	msg string
	ch  chan string
}

func (ToFrontendT) fast() bool { return true }

func encodeString(s string) []byte {
	asBytes := []byte(s)
	length := encodeInt32(len(asBytes))
	return append(length, asBytes...)
}

//
type restartTcpT struct{}

func (restartTcpT) fast() bool { return true }

func (restartTcpT) io(ch chan inT) {
	time.Sleep(time.Second * 30)
	startTcpConnT{}.io(ch)
}

func (b BadTcpT) update(state stateT) (stateT, []outT) {
	errAsBytes := encodeString(b.err.Error())
	encoded := make([]byte, len(errAsBytes)+1)
	encoded[0] = 6
	copy(encoded[1:], errAsBytes)
	asString := base64.StdEncoding.EncodeToString(encoded)
	return state, []outT{
		ToFrontendT{msg: asString, ch: state.websocketOutChan},
		restartTcpT{}}
}

func (t ToFrontendT) io(ch chan inT) {
	t.ch <- t.msg
}

type toServerChanT chan []byte

func (t toServerChanT) router(w waitableT) inputStateT {
	expected, inputState := w.expected(t)
	if !expected {
		return notExpectedT{}
	}

	return inputState
}

func (t toServerChanT) update(state stateT) (stateT, []outT) {
	state.toServerChan = chan []byte(t)
	return state, []outT{}
}

func (startTcpConnT) io(ch chan inT) {
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

//
type msgFromServerT []byte

func (m msgFromServerT) router(w waitableT) inputStateT {
	expected, inputState := w.expected(m)
	if !expected {
		return notExpectedT{}
	}

	return inputState
}

//
func (m msgFromServerT) update(state stateT) (stateT, []outT) {
	asBytes := []byte(m)
	encoded := make([]byte, len(asBytes)+1)
	encoded[0] = 1

	copy(encoded[1:], asBytes)
	return state, []outT{
		ToFrontendT{
			msg: base64.StdEncoding.EncodeToString(encoded),
			ch:  state.websocketOutChan,
		},
	}
}

//
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

func websocketHandler(
	w http.ResponseWriter, r *http.Request, ch chan inT) {

	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println(err)
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
		ch <- fromWebsocketT(string(p))
	}
}

func (f fromWebsocketT) router(w waitableT) inputStateT {
	expected, inputState := w.expected(f)
	if !expected {
		return notExpectedT{}
	}

	return inputState
}

type notExpectedT struct{}

func (notExpectedT) update(state stateT) (stateT, []outT) {
	return state, []outT{}
}

// //
type fromWebsocketT string

//
func (f fromWebsocketT) update(state stateT) (stateT, []outT) {
	bytes, err := base64.StdEncoding.DecodeString(string(f))
	if err != nil {
		state.fatalErr = errors.New(
			"bad base64 from frontend: " + string(f))
		return state, []outT{}
	}
	if len(bytes) == 0 {
		state.fatalErr = errors.New("zero bytes from frontend")
		return state, []outT{}
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
	case 5:
		return sendMessage(bytes[1:], state)
	}

	state.fatalErr = errors.New(
		"bad message indicator from frontend")
	return state, []outT{}
}

func sendMessage(raw []byte, state stateT) (stateT, []outT) {
	state.waiters = append(state.waiters, draftToSendT(raw))
	return state, []outT{
		cacheJobT{
			ch:  state.cacherChan,
			job: cacheGetT(raw),
		},
	}
}

type draftToSendT string

func (d draftToSendT) expected(in inT) (bool, inputStateT) {
	cacheGot, ok := in.(cacheGotT)
	if !ok {
		return false, nil
	}

	if string(d) != cacheGot.key {
		return false, nil
	}

	return true, gotDraftToSendT(cacheGot.value)
}

type gotDraftToSendT []byte

func (g gotDraftToSendT) update(state stateT) (stateT, []outT) {
    waiter := draftForAuthCodeT([]byte(g))
    state.waiters = append(state.waiters, waiter)



	return state, []outT{toServerT{
        ch: state.toServerChan,
        msg: []byte{7},
        }}
}

type toServerT struct {
    ch chan []byte
    msg []byte
}

func (toServerT) fast() bool { return true }

func (t toServerT) io(ch chan inT) { t.ch <- t.msg }


// func (getDraftToSendT) fast() bool {return false}
//
// func (g getDraftToSendT) io(ch chan inT) {
//     draft, err := ioutil.ReadFile(clientDataDir + "/" + string(g))
//     if err != nil {
//         ch <-fatalErrT{err}
//         return
//     }
//
//     ch <-draftToSendT(draft)
// }
//
// type draftToSendT []byte
//
// type draftSendingT struct {
//     to string
//     draft []byte
//     filesToGo []string
// }
//
// // func (d draftToSendT) update(state stateT) (stateT, []outputT) {
// //     draft, err := parseDraft([]byte(d))
// //     if err != nil {
// //         state.fatalErr = err
// //         return state, []outputT{}
// //     }
// //
// //     sending := draftSendingT{
// //         to: draft.to,
// //         draft: []byte(d),
// //         filesToGo: []string,
// //     }
// //
// //     for _, blob := range draft.blobs {
// //         sending.filesToGo = append(sending.filesToGo, blob.id)
// //     }
// //
// //     publicKeys, ok := state.publicKeys[to]
// //     if !ok {
// //         state.wait.pubKeys = sending
// //         return state, []outputT{getPublicKeys(draft.to)}
// //     }
// //
// //     state.wait.authCode = sending
// //     return state, []outputT{getAuthCode
// //
// //     outputs := []outputT{sendBigBlobT{
// //         to: draft.to,
// //         bytes: []byte(d),
// //     }}
// //
// //
// //
// //
// //
// //         sendBlob(blob
// //         outputs = append(outputs, sendBlobT(blob))
// //         outputs = append(outputs, sendFileT{
// //             path: clientDataDir + "/" + blob.id,
// //             to: draft.to})
// //     }
// //
// //     outputs = append(outputs, sendCodeT(draft.code))
// //
// //     outputs = append(outputs, sendDrafT(draft))
// //     return state, outputs
// // }
//
// type sendFileT struct {
//     path string
//     to string
// }
//
// type sendCodeT Code
//
// type sendBlobT Blob
//
// type sendDrafT Draft
//
// func hashFile(path string) ([]byte, error) {
//     hasher := sha256.New()
//     f, err := os.Open(path)
//     if err != nil {
//         return []byte{}, err
//     }
//
//     _, err = io.Copy(hasher, f)
//     if err != nil {
//         return []byte{}, err
//     }
//
//     return hasher.Sum([]byte{}), nil
// }
//
// const chunkLength = 15000
//
// func sendFile(s sendFileT, ch chan inputT) error {
//
//     hash, err := hashFile(s.path)
//     if err != nil {
//         return err
//     }
//
//     f, err := os.Open(s.path)
//     if err != nil {
//         return err
//     }
//
//     fileInfo, err := f.Stat()
//     if err != nil {
//         return err
//     }
//
//     numChunks := int(
//         math.Ceil(float64(fileInfo.Size()) / float64(chunkLength)))
//
//     encNumChunks := encodeInt32(numChunks)
//
//     for i := 0; i < numChunks; i++ {
//         baseChunk := make([]byte, chunkLength)
//         n, err := f.Read(baseChunk)
//         if err != nil && err != io.EOF {
//             return err
//         }
//
//         baseChunk = chunk[:n]
//         chunkNum := encodeInt32(i)
//
//         msg := make([]byte, n + 32 + 4 + 4)
//         copy(msg[:4], chunkNum)
//         copy(msg[4:8], encNumChunks)
//         copy(msg[8:40], hash)
//         copy(msg[40:], baseChunk)
//
//         ch <-sendChunkT{chunk: msg, to: s.to}
//     }
//
//     return nil
// }
//
// type sendChunkT struct {
//     chunk []byte
//     to string
// }
//
// func (s sendChunkT) update(state stateT) (stateT, []outputT) {
//     return state, []outputT{}
// }
//
// func (s sendFileT) io(ch chan inputT) {
//     // err := sendFile(s, ch)
//     // if err != nil {
//     //     ch <-fatalErrT{err}
//     //     return
//     // }
//
//
// }
//
func parseString(raw []byte, i int) (string, int) {
	length := decodeInt(raw[i : i+4])
	i += 4
	str := string(raw[i : i+length])
	i += length
	return str, i
}

type Code struct {
	contents []byte
	mime     string
	filename string
}

type Blob struct {
	id       string
	mime     string
	filename string
	size     int
}

type Draft struct {
	id        string
	subject   string
	to        string
	time      int
	userInput string
	code      Code
	blobs     []Blob
}

func parseBytes(raw []byte, i int) ([]byte, int) {
	length := decodeInt(raw[i : i+4])
	i += 4
	bytes := raw[i : i+length]
	i += length
	return bytes, i
}

func parseCode(raw []byte, i int) (Code, int) {
	contents, i := parseBytes(raw, i)
	mime, i := parseString(raw, i)
	filename, i := parseString(raw, i)
	return Code{
		contents: contents,
		mime:     mime,
		filename: filename,
	}, i
}

func parseBlob(raw []byte, i int) (Blob, int) {
	id, i := parseString(raw, i)
	mime, i := parseString(raw, i)
	filename, i := parseString(raw, i)
	size := decodeInt(raw[i : i+4])
	i += 4
	return Blob{
		id:       id,
		mime:     mime,
		filename: filename,
		size:     size,
	}, i
}

func parseBlobs(raw []byte, i int) ([]Blob, int) {
	length := decodeInt(raw[i : i+4])
	i += 4
	blobs := make([]Blob, length)
	for j := 0; j < length; j++ {
		blobs[j], i = parseBlob(raw, i)
	}
	return blobs, i
}

func parseDraft(raw []byte) (Draft, error) {
	var draft Draft
	id, i := parseString(raw, 0)
	subject, i := parseString(raw, i)
	to, i := parseString(raw, i)
	timeString, i := parseString(raw, i)
	time, err := strconv.Atoi(timeString)
	if err != nil {
		return draft, err
	}
	userInput, i := parseString(raw, i)
	code, i := parseCode(raw, i)
	blobs, i := parseBlobs(raw, i)
	return Draft{
		id:        id,
		subject:   subject,
		to:        to,
		time:      time,
		userInput: userInput,
		code:      code,
		blobs:     blobs,
	}, nil
}

const clientDataDir = "clientData"

//
func cacheDelete(raw []byte, state stateT) (stateT, []outT) {
	return state, []outT{
		cacheJobT{
			ch:  state.cacherChan,
			job: cacheDeleteT(string(raw))}}
}

type cacheDeleteT string

//
func (c cacheDeleteT) run(ch chan inT) {
	err := os.Remove(clientDataDir + "/" + string(c))
	if err != nil {
		ch <- fatalErrT{err}
	}
}

//
type cacheJobT struct {
	ch  chan cacheCmdT
	job cacheCmdT
}

type fatalErrT struct {
	err error
}

func (f fatalErrT) update(state stateT) (stateT, []outT) {
	state.fatalErr = f.err
	return state, []outT{}
}

func (f fatalErrT) router(w waitableT) inputStateT {
	expected, inputState := w.expected(f)
	if !expected {
		return notExpectedT{}
	}

	return inputState
}

func (cacheJobT) fast() bool { return false }

//
func (c cacheJobT) io(ch chan inT) {
	c.ch <- c.job
}

//
type cacheSetT struct {
	key   string
	value []byte
}

func (c cacheSetT) run(ch chan inT) {
	err := ioutil.WriteFile(
		clientDataDir+"/"+c.key,
		c.value,
		0600)
	if err != nil {
		ch <- fatalErrT{err}
	}
}

//
func cacheSet(raw []byte, state stateT) (stateT, []outT) {
	keyLen := decodeInt(raw[:4])
	keyBytes := raw[4 : 4+keyLen]
	key := string(keyBytes)
	blob := raw[4+keyLen:]
	return state, []outT{
		cacheJobT{
			ch:  state.cacherChan,
			job: cacheSetT{key: key, value: blob}}}
}

//
func cacheGet(raw []byte, state stateT) (stateT, []outT) {
	key := string(raw)
	return state, []outT{
		cacheJobT{
			ch:  state.cacherChan,
			job: cacheGetT(key)}}
}

//
type cacheGetT string

//
func (c cacheGetT) run(ch chan inT) {
	filepath := clientDataDir + "/" + string(c)

	blob, err := ioutil.ReadFile(filepath)
	if os.IsNotExist(err) {
		ch <- nullCacheT(c)
		return
	}

	if err != nil {
		ch <- fatalErrT{err}
		return
	}

	ch <- cacheGotT{
		key:   string(c),
		value: blob}
}

type cacheGotT struct {
	key   string
	value []byte
}

func (c cacheGotT) router(w waitableT) inputStateT {
	expected, inputState := w.expected(c)
	if !expected {
		return notExpectedT{}
	}

	return inputState
}

func (c cacheGotT) update(state stateT) (stateT, []outT) {
	blobLen := encodeInt32(len(c.value))
	encoded := append(
		[]byte{0},
		append(
			encodeString(c.key),
			append(blobLen, c.value...)...)...)
	return state, []outT{
		ToFrontendT{
			msg: base64.StdEncoding.EncodeToString(encoded),
			ch:  state.websocketOutChan}}
}

type nullCacheT string

func (n nullCacheT) update(state stateT) (stateT, []outT) {
	encoded := append([]byte{5}, encodeString(string(n))...)
	return state, []outT{
		ToFrontendT{
			msg: base64.StdEncoding.EncodeToString(encoded),
			ch:  state.websocketOutChan},
	}
}

func (n nullCacheT) router(w waitableT) inputStateT {
	expected, inputState := w.expected(n)
	if !expected {
		return notExpectedT{}
	}

	return inputState
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

func getPow(raw []byte, state stateT) (stateT, []outT) {
	powInfo, err := decodeGetPow(raw)
	if err != nil {
		state.fatalErr = err
		return state, []outT{}
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
			return state, []outT{toFrontend}
		}
		counter += 1
	}
}

//
func sendToServer(raw []byte, state stateT) (stateT, []outT) {
	return state, []outT{}
}

//
func fileUpload(w http.ResponseWriter, r *http.Request) {
	blobId := strings.Split(r.URL.Path, "/")[2]
	file, err := os.Create(clientDataDir + "/" + blobId)
	if err != nil {
		fmt.Println("couldn't write file upload: " + err.Error())
		return
	}

	io.Copy(file, r.Body)
}

func (startWebserverT) io(ch chan inT) {
	http.Handle("/static/", http.FileServer(http.Dir("")))
	http.HandleFunc("/uploadFile/", fileUpload)
	http.HandleFunc(
		"/websocket",
		func(w http.ResponseWriter, r *http.Request) {
			websocketHandler(w, r, ch)
		})
	fmt.Println(http.ListenAndServe(":"+port, nil))
}

type websocketOpenT chan string

func (open websocketOpenT) router(w waitableT) inputStateT {
	expected, inputState := w.expected(open)
	if !expected {
		return notExpectedT{}
	}

	return inputState
}

//
func (w websocketOpenT) update(state stateT) (stateT, []outT) {
	state.websocketOutChan = chan string(w)
	return state, []outT{}
}

//
// type inputT interface {
// 	update(stateT) (stateT, []outputT)
// }
//
// type outputT interface {
// 	io(chan inputT)
// }
//
// type memCacheT interface {
//     update () (msgT, error)
// }
//
// type fileSystemT interface {
//     update() (msgT, error)
// }
//
// type msgT interface {
//     send(chansT)
// }
//
// // func main() {
// // 	state := initState()
// // 	outputs := initOutputs()
// // 	inputChannel := make(chan inputT, 1)
// //
// // 	for state.fatalErr == nil {
// // 		for _, output := range outputs {
// // 			go output.io(inputChannel)
// // 		}
// // 		input := <-inputChannel
// // 		state, outputs = input.update(state)
// // 	}
// // 	fmt.Println(state.fatalErr)
// // }

const port = "17448"

func tcp(outChs chansT) {
}

func fileSystem(outChs chansT) {
}

type chansT struct {
	websocket  chan toWebsocketT
	tcp        chan toTcpT
	fileSystem chan toFileSystemT
	stop       chan error
}

type toWebsocketT interface{}
type toTcpT interface{}
type toFileSystemT interface{}

// func main() {
//     websocketCh := make(chan toWebsocketT)
//     tcpCh := make(chan toTcpT)
//     fileSystemCh := make(chan toFileSystemT)
//     stopCh := make(chan error)
//
//     chs := chansT{
//         websocket: websocketCh,
//         tcp: tcpCh,
//         fileSystem: fileSystemCh,
//         stop: stopCh,
//     }
//
//     go httpServer(chs)
//     go tcp(chs)
//     go fileSystem(chs)
//
//     fmt.Println(<-stopCh)
// }

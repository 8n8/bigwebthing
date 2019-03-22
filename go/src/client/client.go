package main

import (
	"crypto/rand"
	"crypto/subtle"
	"encoding/base64"
	"encoding/json"
	"errors"
	"goji.io"
	"goji.io/pat"
	"golang.org/x/crypto/blake2b"
	"io"
	"io/ioutil"
	"mime/multipart"
	"net/http"
	"os"
)

type inputT interface {
	update(*stateT) (stateT, outputT)
}

type outputT interface {
	send() inputT
}

type stateT struct {
	httpChan chan httpInputT
	homeCode string
	appCodes map[string][32]byte
}

type readHttpInputT struct {
	ch chan httpInputT
}

type saveAppT struct {
	w            http.ResponseWriter
	r            *http.Request
	securityCode string
	doneCh       chan endRequest
}

func (a saveAppT) update(s *stateT) (stateT, outputT) {
	if !strEq(a.securityCode, s.homeCode) {
		return *s, sendHttpErrorT{
			w:      a.w,
			msg:    "Bad security code.",
			code:   400,
			doneCh: a.doneCh,
		}
	}
	return *s, writeAppToFileT{
		w:      a.w,
		r:      a.r,
		doneCh: a.doneCh,
	}
}

type writeAppToFileT struct {
	w      http.ResponseWriter
	r      *http.Request
	doneCh chan endRequest
}

func getPostFilePart(r *http.Request) (*multipart.Part, error) {
	var filepart *multipart.Part
	bodyFileReader, err := r.MultipartReader()
	if err != nil {
		return filepart, err
	}
	filepart, err = bodyFileReader.NextPart()
	if err != nil {
		return filepart, err
	}
	if filepart.FormName() != "upload" {
		msg := "Could not find form element \"upload\"."
		return filepart, errors.New(msg)
	}
	return filepart, nil
}

func writeAppToFile(w writeAppToFileT) (string, error) {
	filepart, err := getPostFilePart(w.r)
	if err != nil {
		return "", err
	}
	tmpFileName, err := genCode()
	if err != nil {
		return "", err
	}
	tmpPath := docsDir + "/" + tmpFileName
	fileHandle, err := os.Create(tmpPath)
	defer fileHandle.Close()
	if err != nil {
		return "", err
	}
	hasher, err := blake2b.New256(nil)
	if err != nil {
		return "", err
	}
	tee := io.TeeReader(filepart, hasher)
	_, err = io.Copy(fileHandle, tee)
	if err != nil {
		return "", err
	}
	hash := base64.RawURLEncoding.EncodeToString(hasher.Sum(nil))
	err = os.Rename(tmpPath, docsDir+"/"+hash)
	if err != nil {
		return "", err
	}
	return hash, nil
}

func (w writeAppToFileT) send() inputT {
	hash, err := writeAppToFile(w)
	if err != nil {
		http.Error(w.w, err.Error(), 500)
		w.doneCh <- endRequest{}
		return noInputT{}
	}
	writer := w.w
	writer.Write([]byte(hash))
	w.doneCh <- endRequest{}
	return noInputT{}
}

func (r readHttpInputT) send() inputT {
	select {
	case h := <-r.ch:
		req := h.r
		securityCode := pat.Param(h.r, "securitycode")
		if h.route == "saveapp" {
			return saveAppT{
				w:            h.w,
				r:            h.r,
				securityCode: securityCode,
				doneCh:       h.doneCh,
			}
		}
		body, err := ioutil.ReadAll(req.Body)
		if err != nil {
			http.Error(
				h.w,
				err.Error(),
				http.StatusInternalServerError)
			h.doneCh <- endRequest{}
			return noInputT{}
		}
		return normalApiInputT{
			w:            h.w,
			securityCode: securityCode,
			body:         body,
			route:        h.route,
			doneCh:       h.doneCh,
		}
	default:
	}
	return noInputT{}
}

type httpChansT struct {
	homeIn chan httpInputT
}

const (
	docsDir = "clientData/docs"
)

type normalApiInputT struct {
	w            http.ResponseWriter
	securityCode string
	body         []byte
	route        string
	doneCh       chan endRequest
}

func (n normalApiInputT) update(s *stateT) (stateT, outputT) {
	switch n.route {
	case "makeapproute":
		return processMakeAppRoute(n, s)
	case "getapp":
		return processGetApp(n, s)
	}
	return *s, readHttpInputT{ch: s.httpChan}
}

type makeAppRouteT struct {
	apphash [32]byte
}

type sendHttpErrorT struct {
	w      http.ResponseWriter
	msg    string
	code   int
	doneCh chan endRequest
}

func (s sendHttpErrorT) send() inputT {
	http.Error(s.w, s.msg, s.code)
	s.doneCh <- endRequest{}
	return noInputT{}
}

func strEq(s1, s2 string) bool {
	eq := subtle.ConstantTimeCompare([]byte(s1), []byte(s2))
	return eq == 1
}

type genCodeForAppT struct {
	w       http.ResponseWriter
	appHash [32]byte
	doneCh  chan endRequest
}

func (g genCodeForAppT) send() inputT {
	newCode, err := genCode()
	if err != nil {
		http.Error(g.w, err.Error(), 500)
		g.doneCh <- endRequest{}
		return noInputT{}
	}
	return newAppCodeT{
		w:       g.w,
		appHash: g.appHash,
		doneCh:  g.doneCh,
		newCode: newCode,
	}
}

type newAppCodeT struct {
	w       http.ResponseWriter
	appHash [32]byte
	doneCh  chan endRequest
	newCode string
}

func (n newAppCodeT) update(s *stateT) (stateT, outputT) {
	newState := *s
	var newAppCodes map[string][32]byte
	for code, hash := range s.appCodes {
		newAppCodes[code] = hash
	}
	newAppCodes[n.newCode] = n.appHash
	newState.appCodes = newAppCodes
	return newState, htmlOkResponseT{
		msg:    []byte(n.newCode),
		w:      n.w,
		doneCh: n.doneCh,
	}
}

func getDocHash(securityCode string, appCodes map[string][32]byte) ([32]byte, error) {
	for sc, hash := range appCodes {
		if strEq(sc, securityCode) {
			return hash, nil
		}
	}
	var empty [32]byte
	return empty, nil
}

func byte32ToSlice(bs [32]byte) []byte {
	r := make([]byte, 32)
	for i, b := range bs {
		r[i] = b
	}
	return r
}

func hashToStr(h [32]byte) string {
	asSlice := byte32ToSlice(h)
	return base64.RawURLEncoding.EncodeToString(asSlice)
}

func processGetApp(n normalApiInputT, s *stateT) (stateT, outputT) {
	docHash, err := getDocHash(n.securityCode, s.appCodes)
	if err != nil {
		return *s, sendHttpErrorT{
			w:      n.w,
			msg:    "Bad security code.",
			code:   400,
			doneCh: n.doneCh,
		}
	}
	return *s, serveDocT{
		w:        n.w,
		doneCh:   n.doneCh,
		filePath: docsDir + "/" + hashToStr(docHash),
	}
}

type serveDocT struct {
	w        http.ResponseWriter
	doneCh   chan endRequest
	filePath string
}

func (s serveDocT) send() inputT {
	fileHandle, err := os.Open(s.filePath)
	if err != nil {
		http.Error(s.w, err.Error(), 500)
		s.doneCh <- endRequest{}
		return noInputT{}
	}
	_, err = io.Copy(s.w, fileHandle)
	if err != nil {
		http.Error(s.w, err.Error(), 500)
		s.doneCh <- endRequest{}
		return noInputT{}
	}
	s.doneCh <- endRequest{}
	return noInputT{}
}

func processMakeAppRoute(n normalApiInputT, s *stateT) (stateT, outputT) {
	var makeAppRoute makeAppRouteT
	err := json.Unmarshal(n.body, &makeAppRoute)
	if err != nil {
		return *s, sendHttpErrorT{
			w:      n.w,
			msg:    err.Error(),
			code:   400,
			doneCh: n.doneCh,
		}
	}
	if !strEq(n.securityCode, s.homeCode) {
		return *s, sendHttpErrorT{
			w:      n.w,
			msg:    "Bad security code.",
			code:   400,
			doneCh: n.doneCh,
		}
	}
	return *s, genCodeForAppT{
		w:       n.w,
		appHash: makeAppRoute.apphash,
		doneCh:  n.doneCh,
	}
}

type htmlOkResponseT struct {
	msg    []byte
	w      http.ResponseWriter
	doneCh chan endRequest
}

func (h htmlOkResponseT) send() inputT {
	writer := h.w
	writer.Write(h.msg)
	h.doneCh <- endRequest{}
	return noInputT{}
}

type httpInputT struct {
	w      http.ResponseWriter
	r      *http.Request
	route  string
	doneCh chan endRequest
}

type noInputT struct{}

func (n noInputT) update(s *stateT) (stateT, outputT) {
	return *s, readHttpInputT{s.httpChan}
}

func genCode() (string, error) {
	authSlice := make([]byte, 16)
	_, err := rand.Read(authSlice)
	if err != nil {
		return "", err
	}
	return base64.RawURLEncoding.EncodeToString(authSlice), nil
}

func initState() (stateT, error) {
	homeCode, err := genCode()
	if err != nil {
		return stateT{}, err
	}
	return stateT{
		httpChan: make(chan httpInputT),
		homeCode: homeCode,
	}, nil
}

func main() {
	var state stateT
	// err := readFileData(&state)
	// if err != nil {
	// 	return
	// }
	go httpServer(state.httpChan)
	var input inputT = noInputT{}
	var output outputT = readHttpInputT{ch: state.httpChan}
	for {
		input = output.send()
		state, output = input.update(&state)
	}
}

func staticFileHandler(w http.ResponseWriter, r *http.Request) {
	if r.Method == "GET" {
		f := http.FileServer(http.Dir("/home/t/bigwebthing"))
		f.ServeHTTP(w, r)
	}
}

type handlerT func(http.ResponseWriter, *http.Request)

type endRequest struct{}

func handler(route string, inputChan chan httpInputT) handlerT {
	return func(w http.ResponseWriter, r *http.Request) {
		var doneCh chan endRequest
		inputChan <- httpInputT{
			w:      w,
			r:      r,
			route:  route,
			doneCh: doneCh,
		}
		<-doneCh
	}
}

var routes = []string{"makeapproute", "getapp"}

func httpServer(inputChan chan httpInputT) {
	mux := goji.NewMux()
	for _, route := range routes {
		path := "/" + route + "/:securityCode"
		mux.HandleFunc(
			pat.Post(path),
			handler(route, inputChan))
	}
	http.ListenAndServe(":3000", nil)
}

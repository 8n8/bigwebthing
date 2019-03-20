package main

import (
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"io/ioutil"
	"net/http"
)

type inputT interface {
	update(*stateT) (stateT, outputT)
}

type outputT interface {
	send() inputT
}

type stateT struct {
	httpChans httpChansT
	homeCode  string
}

type httpChansT struct {
	homeIn chan homeInputT
}

const (
	docsDir = "clientData/docs"
)

func (h httpChansT) send() inputT {
	select {
	case input := <-h.homeIn:
		return homeInputT{postBody: input.postBody}
	default:
	}
	return noInputT{}
}

func (h homeInputT) update(s *stateT) (stateT, outputT) {
	return stateT{}, httpChansT{}
}

type homeInputT struct {
	route      string
	postBody   []byte
	returnChan chan []byte
	errChan    chan error
}

type noInputT struct{}

func (n noInputT) update(s *stateT) (stateT, outputT) {
	return *s, s.httpChans
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
	var httpChans httpChansT
	homeCode, err := genCode()
	if err != nil {
		return stateT{}, err
	}
	return stateT{
		httpChans: httpChans,
		homeCode:  homeCode,
	}, nil
}

func main() {
	var state stateT
	// err := readFileData(&state)
	// if err != nil {
	// 	return
	// }
	go httpServer(state.httpChans, state.homeCode)
	var input inputT = noInputT{}
	var output outputT = state.httpChans
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

func makeHomeHandler(route string, inputChan chan homeInputT) handlerT {
	return func(w http.ResponseWriter, r *http.Request) {
		body, bodyReadErr := ioutil.ReadAll(r.Body)
		if bodyReadErr != nil {
			fmt.Print(bodyReadErr.Error())
			return
		}
		var returnChan chan []byte
		var errChan chan error
		inputChan <- homeInputT{
			route:      route,
			postBody:   body,
			returnChan: returnChan,
			errChan:    errChan,
		}
		select {
		case response := <-returnChan:
			w.Write(response)
			return
		case err := <-errChan:
			fmt.Print(err.Error())
			return
		}
	}
}

func httpServer(ch httpChansT, homeCode string) {
	homeBase := "/" + homeCode
	http.HandleFunc(homeBase, staticFileHandler)
	http.HandleFunc(
		homeBase+"/launchapp",
		makeHomeHandler("launchapp", ch.homeIn))
	http.ListenAndServe(":3000", nil)
}

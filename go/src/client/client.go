package main

import (
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
	route string
	postBody []byte
	returnChan chan []byte
	errChan chan error
}

type noInputT struct{}

func (n noInputT) update(s *stateT) (stateT, outputT) {
	return *s, s.httpChans
}

func main() {
	var state stateT
	// err := readFileData(&state)
	// if err != nil {
	// 	return
	// }
	go homeServer(state.httpChans)
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
			route: route,
			postBody: body,
			returnChan: returnChan,
			errChan: errChan,
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

func homeServer(ch httpChansT) {
	http.HandleFunc("/", staticFileHandler)
	http.ListenAndServe(":3000", nil)
}

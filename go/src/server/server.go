package main

import (
	"io/ioutil"
	"net/http"
)

type stateT struct {
	fatalErr      error
	httpInputChan chan httpInputT
}

func initState() stateT {
	return stateT{
		fatalErr: nil,
	}
}

type outputT interface {
	io() inputT
}

type getHttpInputs struct {
	inputChan chan httpInputT
}

func (g getHttpInputs) io() inputT {
	return <-g.inputChan
}

type sendHttpResponse struct {
	response     []byte
	responseChan chan []byte
}

type inputT interface {
	update(stateT) (stateT, outputT)
}

func main() {
	httpInputChan := make(chan httpInputT)
	go httpServer(httpInputChan)
	var input inputT = noInput{}
	var output outputT = getHttpInputs{inputChan: httpInputChan}
	state := initState()
	for state.fatalErr == nil {
		input = output.io()
		state, output = input.update(state)
	}
}

type httpMsgType int

const (
	blob httpMsgType = iota + 1
	invite
	uninvite
	metadata
)

type httpInputT struct {
	typeOf     httpMsgType
	body       []byte
	returnChan chan []byte
}

func (h httpInputT) update(s stateT) (stateT, outputT) {
	return s, getHttpInputs{inputChan: s.httpInputChan}
}

type noInput struct{}

func (n noInput) update(s stateT) (stateT, outputT) {
	return s, getHttpInputs{inputChan: s.httpInputChan}
}

type handler = func(http.ResponseWriter, *http.Request)

func makeHandler(ch chan httpInputT, route httpMsgType) handler {
	return func(w http.ResponseWriter, r *http.Request) {
		bodyBytes, err := ioutil.ReadAll(r.Body)
		if err != nil {
			return
		}
		returnChan := make(chan []byte)
		ch <- httpInputT{
			typeOf:     route,
			body:       bodyBytes,
			returnChan: returnChan,
		}
		w.Write(<-returnChan)
	}
}

func httpServer(ch chan httpInputT) {
	http.HandleFunc("/blob", makeHandler(ch, blob))
	http.HandleFunc("/invite", makeHandler(ch, invite))
	http.HandleFunc("/uninvite", makeHandler(ch, uninvite))
	http.HandleFunc("/metadata", makeHandler(ch, metadata))
	http.ListenAndServe(":4000", nil)
}

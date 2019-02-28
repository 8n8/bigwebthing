package main

import (
	"io/ioutil"
	"net/http"
)

type stateT struct {
	fatalErr error
}

func initState() stateT {
	return stateT{
		fatalErr: nil,
	}
}

type outputT struct {
	response     []byte
	responseChan chan []byte
	inputChan    chan httpInputT
}

func initOutput() outputT {
	return outputT{
		response:     make([]byte, 0),
		responseChan: make(chan []byte),
		inputChan:    make(chan httpInputT),
	}
}

type inputT struct {
	httpInput httpInputT
}

func initInput() inputT {
	return inputT{
		httpInput: httpInputT{
			typeOf:     blob,
			body:       make([]byte, 0),
			returnChan: make(chan []byte),
		},
	}
}

func io(output outputT) inputT {
	output.responseChan <- output.response
	return inputT{
		httpInput: <-output.inputChan,
	}
}

func update(s stateT, i inputT) (stateT, outputT) {
	return stateT{fatalErr: nil}, initOutput()
}

func main() {
	output := initOutput()
	go httpServer(output.inputChan)
	state := initState()
	input := initInput()
	for state.fatalErr == nil {
		input = io(output)
		state, output = update(state, input)
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

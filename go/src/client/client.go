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
	in chan []byte
	out chan []byte
}

func (h httpChansT) send() inputT {
	select {
	case body := <-h.in:
		return httpInputT{postBody: body}
	default:
	}
	return noInputT{}
}

func (h httpInputT) update(s *stateT) (stateT, outputT) {
	return stateT{}, httpChansT{}
}

type httpInputT struct {
	postBody []byte
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
	go httpServer(state.httpChans)
	var input inputT = noInputT{}
	var output outputT = state.httpChans
	for {
		input = output.send()
		state, output = input.update(&state)
	}
}

func handler(w http.ResponseWriter, r *http.Request) {
	switch r.Method {
	case "GET":
		f := http.FileServer(http.Dir("/home/t/bigwebthing"))
		f.ServeHTTP(w, r)
	case "POST":
		bodyBytes, err := ioutil.ReadAll(r.Body)
		if err != nil {
			return
		}
		bodyStr := string(bodyBytes)
		fmt.Fprintf(w, bodyStr+" world!")
	}
}

func httpServer(ch httpChansT) {
	http.HandleFunc("/", handler)
	http.ListenAndServe(":3000", nil)
}

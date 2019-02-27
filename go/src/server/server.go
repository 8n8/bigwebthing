package main

import (
	"net/http"
	"io/ioutil"
)

func main () {
	httpCh := make(chan httpInput)
	go httpServer(httpCh)
}

type httpMsgType int

const (
	blob httpMsgType = iota + 1
	invite
	uninvite
)

type httpInput struct {
	typeOf httpMsgType
	body []byte
	returnChan chan []byte
}

type handler = func(http.ResponseWriter, *http.Request)

func makeHandler(ch chan httpInput, route httpMsgType) handler {
	return func(w http.ResponseWriter, r *http.Request) {
		bodyBytes, err := ioutil.ReadAll(r.Body)
		if err != nil {
			return
		}
		returnChan := make(chan []byte)
		ch <- httpInput{
			typeOf: route,
			body: bodyBytes,
			returnChan: returnChan,
		}
		w.Write(<-returnChan)
	}
}

func httpServer(ch chan httpInput) {
	http.HandleFunc("/blob", makeHandler(ch, blob))
	http.HandleFunc("/invite", makeHandler(ch, invite))
	http.HandleFunc("/uninvite", makeHandler(ch, uninvite))
	http.ListenAndServe(":4000", nil)
}

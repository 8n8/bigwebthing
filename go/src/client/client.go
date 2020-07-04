package main

import (
	"fmt"
	"github.com/gorilla/websocket"
	"github.com/zserge/webview"
	"net/http"
)

type stateT struct {
	fatalErr         error
	websocketOutChan chan string
}

func initState() stateT {
	return stateT{
		fatalErr: nil,
	}
}

type startWebViewT struct{}

const port = "17448"
const baseUrl = "http://localhost:" + port

func (startWebViewT) io(ch chan inputT) {
	go runWebserver(ch)
	w := webview.New(true)
	defer w.Destroy()
	w.SetTitle("BigWebThing")
	w.SetSize(800, 600, webview.HintNone)
	w.Navigate(baseUrl + "/static/index.html")
	w.Run()
}

func initOutputs() []outputT {
	return []outputT{
		startWebViewT{},
	}
}

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func websocketHandler(
	w http.ResponseWriter, r *http.Request, ch chan inputT) {

	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		ch <- fatalErrT{err}
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
				ch <- fatalErrT{err}
				return
			}
		}
	}()

	for {
		_, p, err := conn.ReadMessage()
		if err != nil {
			ch <- fatalErrT{err}
			return
		}
		ch <- fromWebsocketT(string(p))
	}
}

type fromWebsocketT string

func (f fromWebsocketT) update(state stateT) (stateT, []outputT) {
	return state, []outputT{}
}

func runWebserver(ch chan inputT) {
	http.Handle("/static/", http.FileServer(http.Dir("")))
	http.HandleFunc(
		"/websocket",
		func(w http.ResponseWriter, r *http.Request) {
			websocketHandler(w, r, ch)
		})
	fmt.Println(http.ListenAndServe(":"+port, nil))
}

type fatalErrT struct {
	err error
}

func (f fatalErrT) update(state stateT) (stateT, []outputT) {
	state.fatalErr = f.err
	return state, []outputT{}
}

type websocketOpenT chan string

func (w websocketOpenT) update(state stateT) (stateT, []outputT) {
	state.websocketOutChan = chan string(w)
	return state, []outputT{}
}

type inputT interface {
	update(stateT) (stateT, []outputT)
}

type outputT interface {
	io(chan inputT)
}

func main() {
	state := initState()
	outputs := initOutputs()
	inputChannel := make(chan inputT)
	for state.fatalErr == nil {
		for _, output := range outputs {
			go output.io(inputChannel)
		}
		input := <-inputChannel
		state, outputs = input.update(state)
	}
	fmt.Println(state.fatalErr)
}

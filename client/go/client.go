package main

import (
	"github.com/webview/webview"
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"net"
	"net/http"
)

func main() {
	listener, err := net.Listen("tcp", "127.0.0.1:")
	if err != nil {
		panic(fmt.Sprintf("bad listener: %s", err))
	}

	code := makeRandomRoot()

	root := listener.Addr().String() + "/" + code

	go func() {
		http.HandleFunc("/"+code, httpHandler)
        http.HandleFunc("/"+code+"/websockets", websocketHandler)
		panic(http.Serve(listener, nil))
	}()

	debug := true
	w := webview.New(debug)
	defer w.Destroy()
	w.SetTitle("BigWebThing")
	w.SetSize(800, 600, webview.HintNone)
	w.Navigate("http://" + root)
	w.Run()
}

func makeRandomRoot() string {
	buffer := make([]byte, 10)
	n, err := rand.Reader.Read(buffer)
	if err != nil {
		panic(err)
	}
	if n != 10 {
		panic("not enough bytes for random root")
	}
	return base64.RawURLEncoding.EncodeToString(buffer)
}

func parsePath(path string) (Path, error) {
	return Root{}, nil
}

type Root struct{}

const indexHtml = `
<!DOCTYPE html>
<html lang=en>
<head>
    <meta charset="UTF-8">
    <title>BigWebThing</title>
    <style>
    </style>
</head>
<body>
<script>
const rawApp = await fetch('page')
let app = rawApp.json()
</script>
</body>
</html>
`

var upgrader = websocket.Upgrader{
	ReadBufferSize: 1024,
	WriteBufferSize: 1024,
}

func websocketsHandler(w http.ResponseWriter, r *http.Request) {
	guiConn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		panic(err)
	}

	fromGui := make(chan []byte)
	go func() {
		for {
			_, message, err := guiConn.ReadMessage()
			if err != nil {
				panic(err)
			}
			fromFrontend <- message
		}
	}()

	serverConn, err := net.Dial("tcp", serverUrl)
	if err != nil {
		panic(err)
	}

	fromServer := make(chan []byte)
	go func() {
		size := make([]byte, 2)
		for {
			n, err := serverConn.Read(size)
		}
	}()
}

func (Root) handle(w http.ResponseWriter, r *http.Request) {
	_, err := w.Write([]byte(indexHtml))
	if err != nil {
		panic("didn't reach end of index.html: " + err.Error())
	}
}

type Path interface {
	handle(http.ResponseWriter, *http.Request)
}

func httpHandler(w http.ResponseWriter, r *http.Request) {
	path, err := parsePath(r.URL.Path)
	if err != nil {
		return
	}

	path.handle(w, r)
}

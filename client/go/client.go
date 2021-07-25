package main

import (
	"crypto/rand"
	"encoding/base64"
	"fmt"
	"net"
	"net/http"

	"github.com/gorilla/websocket"
	"github.com/webview/webview"
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
	ReadBufferSize:  1024,
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

	toServer := make(chan []byte)
	fromServer := make(chan []byte)
	go handleServerConn(toServer, fromServer)


	db, err := setupDb()
	if err != nil {
		panic("no database: " + err.Error())
	}

	for {
		select {
		case msg := <-fromGui:
			parseFromGui(msg).processFromGui(db)
		}
}

func handleServerConn(to chan []byte, from chan []byte) {
	for {
		conn, err := net.Dial("tcp", serverUrl)
		if err == nil {
			useOneConn(conn, to, from)
		}
		time.Sleep(serverRetry)
	}
}

func useOneConn(conn net.Conn, to chan []byte, from chan []byte) {
	go readConn(conn, from)
	writeConn(conn, to)
}

func readConn(conn net.Conn, from chan []byte) {
	for {
		size := make([]byte, 2)
		n, err := conn.Read(size)
		if err != nil || n != 2 {
			return
		}

		buf := make([]byte, size)
		n, err = conn.Read(buf)
		if err != nil || n != size {
			return
		}

		from <- buf
	}
}

func writeConn(conn net.Conn, to chan []byte) {
}


func useOneConn(conn net.Conn, to chan []byte, from chan []byte) {
	go func() {
		for {
			live <- struct{}{}
			time.Sleep(serverRetry)
			<-dead
		}
	}()

	go func() {
		for {
			<-live

			size := make([]byte, 2)
			n, err := serverConn.Read(size)
			if err != nil || n != 2 {
				dead <-struct{}{}
				continue
			}

			buf := make([]byte, size)
			n, err = serverConn.Read(buf)
			if err != nil || n != size {
				dead <-struct{}{}
				continue
			}

			from <- buf
		}
	}()

	for {
		<-
	}

	serverConn, err := net.Dial("tcp", serverUrl)
	if err != nil {
		panic(err)
	}

	fromServer := make(chan []byte)
	serverFail := make(chan error)
	go func() {
		for {
			size := make([]byte, 2)
			n, err := serverConn.Read(size)
			if n == size & err == nil {
				continue
			}
			if n != size {
				serverFail <- ServerMessageTooShort{n, size}
			}
			if err != nil {
				serverFail <- err
			}
			time.Sleep(serverRetry)
		}
	}()

	toServer := make(chan []byte)
	go func() {
		for {
			msg := <-toServer
			n, err := serverConn.Write(msg)
			if err != nil {
				
			}
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

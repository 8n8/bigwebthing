package main

import (
	"net/http"
	"os"
	"io"
	"crypto/rand"
	"encoding/hex"
	"github.com/gorilla/websocket"
    "fmt"
)

func makeHtmlCode() string {
    bs := make([]byte, 10)
    _, err := rand.Reader.Read(bs)
    if err != nil {
        panic(err)
    }
    return hex.EncodeToString(bs)
}

func main() {
	htmlCode := makeHtmlCode()
    fmt.Println(htmlCode)
    http.Handle(
        "/static/",
        http.StripPrefix("/static/", http.FileServer(http.Dir("."))))
    http.Handle("/"+htmlCode+"/", 
    http.HandleFunc("/"+htmlCode+"/websocket", websocketRoute)

	panic(http.ListenAndServe(":3333", nil))
}

var upgrader = websocket.Upgrader{
	ReadBufferSize: 1024,
	WriteBufferSize: 1024,
}

func websocketRoute(w http.ResponseWriter, r *http.Request) {
	_, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		panic(err)
	}
}

func loadFavicon(w http.ResponseWriter, r *http.Request) {
	f, err := os.Open("../frontend/favicon.ico")
	if err != nil {
		panic(err)
	}
	io.Copy(w, f)
	f.Close()
}

func loadHomePage(w http.ResponseWriter, r *http.Request) {
	f, err := os.Open("../frontend/index.html")
	if err != nil {
		panic(err)
	}
	io.Copy(w, f)
	f.Close()
}

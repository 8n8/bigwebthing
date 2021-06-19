package main

import (
	"net/http"
	"os"
	"io"
	"crypto/rand"
	"encoding/base64"
)

func main() {
	htmlCode := makeHtmlCode()
	http.HandleFunc("/"+htmlCode, loadIndex)

	panic(http.ListenAndServe(":3333", nil))
}

func loadHomePage(w http.ResponseWriter, r *http.Request) {
	f, err := os.Open("index.html")
	if err != nil {
		panic(err)
	}
	io.Copy(w, f)
	f.Close()
}

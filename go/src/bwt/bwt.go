package main

import (
	"net/http"
)

func main() {
	http.Handle("/", http.FileServer(http.Dir("/home/t/bigwebthing")))
	http.ListenAndServe(":3000", nil)
}

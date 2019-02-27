package main

import (
	"fmt"
	"net/http"
	"io/ioutil"
)

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
		fmt.Fprintf(w, bodyStr + " world!")
	}
}

func main() {
	http.HandleFunc("/", handler)
	http.ListenAndServe(":3000", nil)
}

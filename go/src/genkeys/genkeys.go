package main

import (
	"crypto/rand"
	"golang.org/x/crypto/nacl/sign"
	"io/ioutil"
	"fmt"
)

func main() {
	public, private, err := sign.GenerateKey(rand.Reader)
	if err != nil {
		fmt.Println(err)
		return
	}
	err = ioutil.WriteFile("private", private[:], 0600)
	if err != nil {
		fmt.Println(err)
		return
	}
	err = ioutil.WriteFile("public", public[:], 0600)
	if err != nil {
		fmt.Println(err)
	}
}

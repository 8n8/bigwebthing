package main

import (
	"os"
	"fmt"
	"io/ioutil"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/sign"
	"io"
)


var filenames []string = []string{
	"name.txt", "description.txt", "icon.webp", "app.tar"}

const badArgs = "There must be one argument: " +
	"the name of the file containing the private key."

func main() {
	args := os.Args
	if len(args) != 2 {
		fmt.Println(badArgs)
		return
	}
	privateKeySlice, err := ioutil.ReadFile(args[1])
	if err != nil {
		fmt.Println(err)
		return
	}
	var privateKey [64]byte
	copy(privateKey[:], privateKeySlice)

	hasher, err := blake2b.New256(nil)
	if err != nil {
		fmt.Println(err)
		return
	}
	for _, filename := range filenames {
		handle, err := os.Open(filename)
		if err != nil {
			fmt.Println(err)
			return
		}
		_, err = io.Copy(hasher, handle)
		if err != nil {
			fmt.Println(err)
			return
		}
	}

	appHash := hasher.Sum(nil)
	signature := sign.Sign(make([]byte, 0), appHash, &privateKey)
	err = ioutil.WriteFile("signature", signature, 0600)
	if err != nil {
		fmt.Println(err)
	}
}

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
	"app.tar", "icon.webp", "description.txt", "name.txt"}

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
		fmt.Println(filename)
		fmt.Println(hasher.Sum(nil))
		if err != nil {
			fmt.Println(err)
			return
		}
	}

	appHash := hasher.Sum(nil)
	fmt.Println(appHash)
	signature := sign.Sign(make([]byte, 0), appHash, &privateKey)
	fmt.Println("signature: %v", signature)
	err = ioutil.WriteFile("signature", signature, 0600)
	if err != nil {
		fmt.Println(err)
	}
}

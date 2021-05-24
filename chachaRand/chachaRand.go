package main

import (
	"golang.org/x/crypto/chacha20"
	"io"
	"fmt"
)

var testSecret []byte = []byte{
	169, 173, 48, 139, 34, 72, 39, 195, 74, 81, 193, 111, 81, 142, 125, 244, 29, 212, 8, 6, 37, 204, 237, 189, 56, 6, 189, 227, 153, 105, 215, 211, 192, 250, 170, 169, 219, 65, 42, 102, 131, 243, 113, 158,
}

func main() {
	csprng, err := makeNewCsprng(testSecret)
	if err != nil {
		panic(err)
	}

	for x := 0; x < 10; x++ {
		value := make([]byte, 10)
		n, err := csprng.Read(value)
		if n != 10 {
			panic("bad random length")
		}
		if err != nil {
			panic(err)
		}
		fmt.Println(value)
	}
}

type Csprng struct {
	cipher *chacha20.Cipher
}

func (c Csprng) Read(p []byte) (int, error) {
	c.cipher.XORKeyStream(p, p)
	return len(p), nil
}

func makeNewCsprng(s []byte) (io.Reader, error) {
	cipher, err := chacha20.NewUnauthenticatedCipher(s[:32], s[32:])
	return Csprng{cipher}, err
}

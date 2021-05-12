package main

import (
	"fmt"
	"crypto/rand"
	"github.com/flynn/noise"
)

func main() {
	err := mainErr()
	if err != nil {
		panic(err)
	}
}

func mainErr() error {
	myStatic, err := noise.DH25519.GenerateKeypair(rand.Reader)
	if err != nil {
		return err
	}

	theirStatic, err := noise.DH25519.GenerateKeypair(rand.Reader)
	if err != nil {
		return err
	}

	me, err := makeMyInitNoise(myStatic, theirStatic)
	if err != nil {
		return err
	}

	them, err := makeTheirInitNoise(theirStatic, myStatic)
	if err != nil {
		return err
	}

	kk1, _, _, err := me.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return err
	}

	_, _, _, err = them.ReadMessage([]byte{}, kk1)
	if err != nil {
		return err
	}

	kk2, _, _, err := them.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return err
	}

	_, tx, mRx, err := me.ReadMessage([]byte{}, kk2)
	if err != nil {
		return err
	}

	transport, err := tx.Encrypt([]byte{}, []byte{}, plaintext)
	if err != nil {
		return err
	}

	_, err = mRx.Decrypt([]byte{}, []byte{}, transport)
	if err != nil {
		fmt.Println("it didn't work")
		return err
	}

	return nil
}

var plaintext = []byte("hello")

func makeMyInitNoise(myStatic, theirStatic noise.DHKey) (*noise.HandshakeState, error) {
	var c noise.Config
	c.CipherSuite = noise.NewCipherSuite(
		noise.DH25519,
		noise.CipherChaChaPoly,
		noise.HashBLAKE2s)
	c.Pattern = noise.HandshakeKK
	c.Initiator = true
	c.StaticKeypair = myStatic
	c.PeerStatic = theirStatic.Public
	return noise.NewHandshakeState(c)
}

func makeTheirInitNoise(theirStatic, myStatic noise.DHKey) (*noise.HandshakeState, error) {
	var c noise.Config
	c.CipherSuite = noise.NewCipherSuite(
		noise.DH25519,
		noise.CipherChaChaPoly,
		noise.HashBLAKE2s)
	c.Pattern = noise.HandshakeKK
	c.Initiator = false
	c.StaticKeypair = theirStatic
	c.PeerStatic = myStatic.Public
	return noise.NewHandshakeState(c)
}

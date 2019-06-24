package common

import (
	"bytes"
	"fmt"
	"bufio"
	"encoding/gob"
	"errors"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/nacl/sign"
	"net"
)

func AuthCodeToSlice(bs [AuthCodeLength]byte) []byte {
	result := make([]byte, AuthCodeLength)
	for i, b := range bs {
		result[i] = b
	}
	return result
}

const (
	SigSize        = sign.Overhead + blake2b.Size256
	AuthSigSize    = sign.Overhead + AuthCodeLength
	AuthCodeLength = 24
	ChunkSize      = 16000
	ChunkContentSize = 15000
)

var TruesPubSign = [32]byte{203,110,35,30,25,232,75,93,6,122,45,161,239,56,4,150,110,153,254,157,151,179,125,35,121,47,194,145,70,242,129,163}

type MsgT interface {
	msgTplaceholderFunc()
}

type Encrypted struct {
	Msg   []byte
	Nonce [24]byte
}

type GiveMeASymmetricKey struct {
	MyPublicEncrypt [32]byte
	Sig [SigSize]byte
}

const EncryptedKeyLen = secretbox.Overhead + 32

type HereIsAnEncryptionKey struct {
	MyPublicEncrypt       [32]byte
	EncryptedSymmetricKey [EncryptedKeyLen]byte
	Nonce                 [24]byte
	Sig                   [SigSize]byte
}

func (e Encrypted) msgTplaceholderFunc() { return }

func (w GiveMeASymmetricKey) msgTplaceholderFunc() { return }

func (h HereIsAnEncryptionKey) msgTplaceholderFunc() { return }

type ClientToClient struct {
	Msg       MsgT
	Recipient [32]byte
	Author    [32]byte
}

func EncodeData(a interface{}) ([]byte, error) {
	var buf bytes.Buffer
	enc := gob.NewEncoder(&buf)
	err := enc.Encode(a)
	if err != nil {
		return make([]byte, 0), err
	}
	return buf.Bytes(), nil
}

func intToTwoBytes(i int) ([]byte, error) {
	if i < 0 {
		return *new([]byte), errors.New("Int less than zero.")
	}
	if i > 256*256 {
		return *new([]byte), errors.New(
			"Int greater than 256*256.")
	}
	u := uint(i)
	return []byte{
		(byte)(u & 0xff),
		(byte)((u & 0xff00) >> 8)}, nil
}

func EncodeClientToClient(cToC ClientToClient) ([]byte, error) {
	outerMsg, err := EncodeData(cToC)
	if err != nil {
		return *new([]byte), err
	}
	lenOuterMsg := len(outerMsg)
	if lenOuterMsg > ChunkSize {
		return *new([]byte), errors.New("Message too long.")
	}
	lenInBytes, err := intToTwoBytes(lenOuterMsg)
	if err != nil {
		return *new([]byte), err
	}
	return append(lenInBytes, outerMsg...), nil
}

type AuthSigT struct {
	Author [32]byte
	Sig    [AuthSigSize]byte
}

func ReadClientToClient(conn net.Conn) (ClientToClient, error) {
	msgLenB := make([]byte, 2)
	fmt.Println("Above conn.Read in ReadClientToClient.")
	bufferedConn := bufio.NewReaderSize(conn, ChunkSize)
	n, err := bufferedConn.Read(msgLenB)
	if err != nil {
		fmt.Println("It all went wrong.")
		fmt.Println(err)
		return *new(ClientToClient), err
	}
	fmt.Println("Below conn.Read error check.")
	if n != 2 {
		return *new(ClientToClient), errors.New(
			"Message length bytes wrong length.")
	}
	mLen := twoBytesToInt(msgLenB)
	if mLen > ChunkSize {
		return *new(ClientToClient), errors.New(
			"Message too big.")
	}
	fmt.Println("below message length check")
	msg := make([]byte, mLen)
	n, err = bufferedConn.Read(msg)
	if err != nil {
		return *new(ClientToClient), err
	}
	fmt.Println("Below message read.")
	if n != mLen {
		return *new(ClientToClient), errors.New(
			"Message wrong length.")
	}
	fmt.Println("Just above decodeClientToClient call.")
	return decodeClientToClient(msg)
}

func twoBytesToInt(bs []byte) int {
	return (int)(bs[0]) + (int)(bs[1])*256
}

func decodeClientToClient(msg []byte) (ClientToClient, error) {
	var buf bytes.Buffer
	n, err := buf.Write(msg)
	var cToC ClientToClient
	if err != nil {
		return cToC, err
	}
	if n != len(msg) {
		return cToC, errors.New(
			"Could not write message to buffer.")
	}
	err = gob.NewDecoder(&buf).Decode(&cToC)
	if err != nil {
		return cToC, err
	}
	return cToC, nil
}

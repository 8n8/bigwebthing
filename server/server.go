package main

import (
	"github.com/flynn/noise"
	"fmt"
	"crypto/rand"
	"net"
	"sync"
	"os"
	"io"
)

func main() {
	staticKeys := getStaticKeys()
	messages := getMessagesFromDisk()
	var lock sync.Mutex

	listener, err := net.Listen("tcp", ":4040")
	if err != nil {
		panic(err)
	}

	for {
		conn, err := listener.Accept()
		if err != nil {
			continue
		}
		go handleConnection(conn, &messages, &lock, &staticKeys)
	}
}

func getMessagesFromDisk() Messages {
	var messages Messages
	f, err := os.Open(messagesPath)
	if os.IsNotExist(err) {
		return messages
	}
	if err != nil {
		panic(err)
	}
	defer f.Close()

	for parseMessageDisk(f, &messages) {}

	return messages
}

func getStaticKeys() noise.DHKey {
	var keys noise.DHKey

	raw, err := os.ReadFile(staticKeysPath)

	if os.IsNotExist(err)  {
		keys, err := noise.DH25519.GenerateKeypair(rand.Reader)
		if err != nil {
			panic(err)
		}
		err = os.WriteFile(staticKeysPath, encodeKeys(keys), 0400)
		if err != nil {
			panic(err)
		}
		return keys
	}

	if err != nil {
		panic(err)
	}
	return noise.DHKey{
		Public: raw[:dhlen],
		Private: raw[dhlen: dhlen*2],
	}
}

func encodeKeys(keys noise.DHKey) []byte {
	encoded := make([]byte, 2*dhlen)
	copy(encoded, keys.Public)
	copy(encoded, keys.Private)
	return encoded
}

const dhlen = 32

const staticKeysPath = "staticKeys"

func serializeKeys(keys noise.DHKey) []byte {
	result := make([]byte, 2 * idlen, 2 * idlen)
	copy(result, keys.Private)
	copy(result[idlen:], keys.Public)
	return result
}

const messagesPath = "messages"

const maxInt32 = 2147483647

func initMessages() Messages {
	return Messages {
		kk1s: make(map[[idlen]byte]Kk1),
		kk2s: make(map[[idlen]byte]Kk2),
		kkTransports: make(map[[idlen]byte]KkTransport),
		blobs: make(map[[idlen]byte]map[uint32]Blob),
		addContact: make(map[Contact]uint32),
		removeContact: make(map[Contact]uint32),
		payment: make(map[[idlen]byte]Payment),
		getDataOf: make(map[[idlen]byte]uint32),
		getBlob: make(map[[idlen]byte]uint32),
	}
}

func parseMessageDisk(f io.Reader, messages *Messages) bool {
	return (
		parseKk1Disk(f, messages) ||
		parseKk2Disk(f, messages) ||
		parseKkTransportDisk(f, messages) ||
		parseBlobDisk(f, messages) ||
		parseAddContactDisk(f, messages) ||
		parseRemoveContactDisk(f, messages) ||
		parseGetDataOfDisk(f, messages) ||
		parseGetBlobDisk(f, messages))
}

func parseKk1Disk(f io.Reader, messages *Messages) bool {
	indicator := make([]byte, 1)
	n, err := f.Read(indicator)
	if err != nil {
		panic(err)
	}
	if n != 1 {
		return false
	}
	if indicator[0] != 0 {
		return false
	}
}

func parseId(raw []byte, pos int) ([idlen]byte, int, error) {
	var id [idlen]byte
	if copy(id[:], raw[pos:]) < idlen {
		return id, pos, fmt.Errorf("not enough bytes for user ID")
	}
	pos += idlen
	return id, pos, nil
}

func intPow(base, power int) int {
	result := 1
	for p := 0; p < power; p++ {
		result = result * base
	}
	return result
}

func decodeUint32(b []byte) uint32 {
	b0 := uint32(b[0])
	b1 := uint32(b[1])
	b2 := uint32(b[2])
	b3 := uint32(b[3])
	return b0 + b1 * 256 + b2 * 256 * 256 + b3 * 256 * 256 * 256
}

func parseAmount(raw []byte, pos int) (uint32, int, error) {
	if len(raw) < pos + 4 {
		return 0, pos, fmt.Errorf("not enough bytes for amount")
	}

	amount := decodeUint32(raw[pos: pos+4])
	return amount, pos+4, nil
}

func parseTimestamp(raw []byte, pos int) (uint32, int, error) {
	if len(raw) < pos + 4 {
		return 0, pos, fmt.Errorf("not enough bytes for timestamp")
	}

	timestamp := decodeUint32(raw[pos: pos+4])
	return timestamp, pos+4, nil
}

func parseBlobCounter(raw []byte, pos int) (uint32, int, error) {
	if len(raw) < pos + 4 {
		return 0, pos, fmt.Errorf("not enough bytes for blob counter")
	}

	counter := decodeUint32(raw[pos: pos+4])
	return counter, pos+4, nil
}

const (
	idlen = 32
	kk1len = 48
	kk2len = 48
	kktransportlen = 48
	hashlen = 32
)

type Messages struct {
	kk1s map[[idlen]byte]Kk1
	kk2s map[[idlen]byte]Kk2
	kkTransports map[[idlen]byte]KkTransport
	blobs map[[idlen]byte]map[uint32]Blob
	addContact map[Contact]uint32
	removeContact map[Contact]uint32
	payment map[[idlen]byte]Payment
	getDataOf map[[idlen]byte]uint32
	getBlob map[[idlen]byte]uint32
}

type Blob struct {
	timestamp uint32
	author [idlen]byte
	hash [hashlen]byte
}

type Payment struct {
	timestamp uint32
	amount uint32
}

type Contact struct {
	contacter [idlen]byte
	contactee [idlen]byte
}

type KkTransport struct {
	timestamp uint32
	sender [idlen]byte
	sessionId [idlen]byte
	blobId [idlen]byte
	kkTransport [kktransportlen]byte
}

type Kk2 struct {
	timestamp uint32
	sender [idlen]byte
	sessionId [idlen]byte
	kk2 [kk2len]byte
}

type Kk1 struct {
	timestamp uint32
	sender [idlen]byte
	kk1 [kk1len]byte
}

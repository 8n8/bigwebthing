package main

import (
	"github.com/flynn/noise"
	"io/ioutil"
	"fmt"
	"crypto/rand"
)

var go_ = make(chan func())

func main() {
	go_ <- init_
	for {
		(<-go_)()
	}
}

var in = make(chan In)

type Start struct{}

func initState() State {
	return State{
	}
}

type State struct {
	staticKeys MaybeStaticKeys
	messages Messages
}

type MaybeStaticKeys interface {
}

type Panic string

func (p Panic) io(*State) {
	panic(string(p))
}

type In interface {
	pure(*State) Out
}

type Out interface {
	io(*State)
}

func init_() {
	in <- Start{}
	state := initState()
	for {
		(<-in).pure(&state).io(&state)
	}
}

func (Start) pure(state *State) Out {
	return ReadKeysFile{}
}

type ReadKeysFile struct{}

const staticKeysPath = "staticKeys"

func (ReadKeysFile) io(*State) {
	raw, err := ioutil.ReadFile(staticKeysPath)
	in <- StaticKeysFile{raw, err}
}

type StaticKeysFile struct {
	raw []byte
	err error
}

func (s StaticKeysFile) pure(state *State) Out {
	if s.err != nil {
		return GenerateStaticKeys{}
	}

	if len(s.raw) != 2 * dhlen {
		return Panic(fmt.Sprintf("bad static keys file length: expecting %d but got %d", 2 * dhlen, len(s.raw)))
	}

	return MemCacheStaticKeys(
		noise.DHKey{
			Private: s.raw[:dhlen],
			Public: s.raw[dhlen:],
		})
}

type GenerateStaticKeys struct{}

func (GenerateStaticKeys) io(*State) {
	keys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	in <- NewKeypair{keys, err}
}

type NewKeypair struct {
	keys noise.DHKey
	err error
}

const dhlen = 32

type MemCacheStaticKeys noise.DHKey

func (c MemCacheStaticKeys) io(state *State) {
	state.staticKeys = GotKeys(c)
}

type GotKeys noise.DHKey

type Outs []Out

func (outs Outs) io(state *State) {
	for _, out := range []Out(outs) {
		out.io(state)
	}
}

func (n NewKeypair) pure(state *State) Out {
	if n.err != nil {
		return Panic("couldn't generate static keys: " + n.err.Error())
	}

	return Outs([]Out{
		MemCacheStaticKeys(n.keys),
		DiskCacheStaticKeys(serializeKeys(n.keys)),
		FetchMessagesFromFile{},
	})
}

func serializeKeys(keys noise.DHKey) []byte {
	result := make([]byte, 2 * dhlen, 2 * dhlen)
	copy(result, keys.Private)
	copy(result[dhlen:], keys.Public)
	return result
}

type FetchMessagesFromFile struct{}

const messagesPath = "messages"

func (FetchMessagesFromFile) io(*State) {
	raw, err := ioutil.ReadFile(messagesPath)
	in <- RawMessagesFile{raw, err}
}

type RawMessagesFile struct {
	raw []byte
	err error
}

func (r RawMessagesFile) pure(state *State) Out {
	if r.err != nil {
		return DoNothing{}
	}

	parsed, err := parseMessages(r.raw)
	if err != nil {
		return Panic("corrupted messages file: " + err.Error())
	}

	return MemCacheMessages(parsed)
}

const maxInt32 = 2147483647

func initMessages() Messages {
	return Messages {
		kk1s: make(map[[dhlen]byte]Kk1),
		kk2s: make(map[[dhlen]byte]Kk2),
		kkTransports: make(map[[dhlen]byte]KkTransport),
		blobs: make(map[[idlen]byte]Blob),
		addContact: make(map[[dhlen]byte]Contact),
		removeContact: make(map[[dhlen]byte]Contact),
		payment: make(map[[dhlen]byte]Payment),
		getDataOf: make(map[[dhlen]byte]int),
		getBlob: make(map[[idlen]byte]int),
	}
}

func parseMessages(raw []byte) (Messages, error) {
	messages := initMessages()

	for position := 0; position < len(raw); {
		message, position, err := parseMessage(raw, position)
		if err != nil {
			return messages, err
		}
		message.store(messages)
	}

	return messages, nil
}

func parseMessage(raw []byte, position int) (Message, int, error) {
	indicator, position, err := parseIndicator(raw, position)
	if err != nil {
		return *new(Message), position, fmt.Errorf("bad indicator: %s", err)
	}

	return indicator.parser(raw, position)
}

func parseIndicator(raw []byte, position int) (Indicator, int, error) {
	rawIndicator := raw[position]
	position++

	indicator, ok := indicators[rawIndicator]

	if !ok {
		indicator, position, fmt.Errorf("expected one of %v, but got %d", mapKeys(indicators), rawIndicator)
	}

	return indicator, position, nil
}

type Message interface {
	store(Messages) Messages
}

const idlen = 32

type MemCacheMessages Messages

func (m MemCacheMessages) io(state *State) {
	state.messages = Messages(m)
}

type Messages struct {
	kk1s map[[dhlen]byte]Kk1
	kk2s map[[dhlen]byte]Kk2
	kkTransports map[[dhlen]byte]KkTransport
	blobs map[[idlen]byte]Blob
	addContact map[[dhlen]byte]Contact
	removeContact map[[dhlen]byte]Contact
	payment map[[dhlen]byte]Payment
	getDataOf map[[dhlen]byte]int
	getBlob map[[idlen]byte]int
}

type Blob struct {
	timestamp int
	author []byte
	id []byte
	hash []byte
}

type Payment struct {
	timestamp int
	amount int
}

type Contact struct {
	timestamp int
	contactee []byte
}

type KkTransport struct {
	timestamp int
	sender []byte
	firstPartKk1 []byte
	kkTransport []byte
}

type Kk2 struct {
	timestamp int
	sender []byte
	firstPartKk1 []byte
	kk2 []byte
}

type Kk1 struct {
	timestamp int
	sender []byte
	kk1 []byte
}

type DiskCacheStaticKeys []byte

func (d DiskCacheStaticKeys) io(state *State) {
	err := ioutil.WriteFile(staticKeysPath, []byte(d), 0400)
	in <- ErrWritingStaticKeys{err}
}

type ErrWritingStaticKeys struct {
	err error
}

func (e ErrWritingStaticKeys) pure(*State) Out {
	if e.err != nil {
		return Panic("couldn't write static keys to file: " + e.err.Error())
	}

	return DoNothing{}
}

type DoNothing struct{}

func (DoNothing) io(*State) {}

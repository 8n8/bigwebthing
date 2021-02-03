package main

import (
	"crypto/aes"
	"crypto/cipher"
	"crypto/rand"
	"encoding/base64"
	"errors"
	"fmt"
	"github.com/flynn/noise"
	"io/ioutil"
	"os"
)

func main() {
	inputChan := make(chan Input)
	state := initState()
	output := initInput().update(state)
	for state.fatal == nil {
		go output.run(inputChan)
		output = (<-inputChan).update(state)
	}
	fmt.Println(state.fatal.Error())
}

func initState() State {
	return State{
		fatal: nil,
		staticKeys: make(map[[dhlen]byte][dhlen]byte),
		contacts: make(map[[dhlen]byte]struct{}),
		sending: make(map[[SessionIdSize]byte][SecretSize]byte),
		receiving: make(map[[SessionIdSize]byte][SecretSize]byte),
	}
}

func initInput() Input {
	return Start{}
}

type Start struct{}

const SessionIdSize = 24

type DoNothing struct{}

func (DoNothing) run(_ chan Input) {}

type State struct {
	fatal error
	staticKeys noise.DHKey
	contacts map[[dhlen]byte]struct{}
	sending map[[SessionIdSize]byte][SecretSize]byte
	receiving map[[SessionIdSize]byte][SecretSize]byte
}

func (Start) update(state State) (State, Output) {
	return state, ReadSecretsFile{}
}

type ReadSecretsFile struct{}

const secretsPath = "state"

func (ReadSecretsFile) run(inCh chan Input) {
	raw, err := ioutil.ReadFile(secretsPath)
	inCh <- RawState{raw, err}
}

type RawState struct {
	raw []byte
	err error
}

type GenerateStaticKeys struct{}

func (GenerateStaticKeys) run(inCh chan Input) {
	keys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	inCh <- NewStaticKeys{keys, err}
}

type NewStaticKeys struct {
	keys noise.DHKey
	err error
}

func (n NewStaticKeys) update(state State) (Output, State) {
	if n.err != nil {
		state.fatal = fmt.Errorf("couldn't generate static keys: %s", err.Error())
		return DoNothing{}, state
	}
}

func (r RawState) update(state State) (Output, State) {
	var pathError *os.PathError
	if errors.As(r.err, &pathError) {
		return GenerateStaticKeys{}, initState()
	}

	state, err := parseState(r.raw)
	if err != nil {
		state.fatal = fmt.Errorf("couldn't parse secrets file: %s", err.Error())
		return DoNothing{}, state
	}

	return DoNothing{}, state
}

type Input interface {
	update(State) (Output, State)
}

type Output interface {
	run(chan Input)
}

type Args interface {
	run() error
}

const SecretSize = aes.BlockSize * 2

const dhlen = 32

func encodeUint32(n int) []byte {
	result := make([]byte, 4)
	for i := 0; i < 4; i++ {
		result[i] = byte((n >> (i * 8)) & 0xFF)
	}
	return result
}

func parseStaticKeys(raw []byte, pos int) (noise.DHKey, int, error) {
	secret, pos, err := parseDhlen(raw, pos)
	if err != nil {
		return *new(noise.DHKey), pos, err
	}
	public, pos, err := parseDhlen(raw, pos)
	return noise.DHKey{
		Private: secret[:],
		Public: public[:],
	}, pos, err
}

func parseDhlen(raw []byte, pos int) ([dhlen]byte, int, error) {
	var id [dhlen]byte
	n := copy(id[:], raw[pos:])
	if n != dhlen {
		return id, pos, NotEnoughBytesForDhKey(n)
	}
	pos += dhlen
	return id, pos, nil
}

type NotEnoughBytesForDhKey int

func (n NotEnoughBytesForDhKey) Error() string {
	return fmt.Sprintf("not enough bytes for DH key: %d", n)
}

func parseState(raw []byte) (State, error) {
	pos := 0
	var state State

	staticKeys, pos, err := parseStaticKeys(raw, pos)
	if err != nil {
		return state, err
	}

	contacts, pos, err := parseContacts(raw, pos)
	if err != nil {
		return state, err
	}

	sending, pos, err := parseSessionSecrets(raw, pos)
	if err != nil {
		return state, err
	}

	receiving, pos, err := parseSessionSecrets(raw, pos)
	if err != nil {
		return state, err
	}

	if pos != len(raw) {
		return state, ExpectStateEnd{pos, len(raw)}
	}

	return State{
		fatal: nil,
		staticKeys: staticKeys,
		contacts: contacts,
		sending: sending,
		receiving: receiving,
	}, nil
}

func parseSessionId(
	raw []byte, pos int) ([SessionIdSize]byte, int, error) {

	var id [SessionIdSize]byte
	n := copy(id[:], raw[pos:])
	if n < SessionIdSize {
		return id, pos, TooShortForSessionId(n)
	}

	return id, pos + SessionIdSize, nil
}

type TooShortForSessionId int

func (t TooShortForSessionId) Error() string {
	return fmt.Sprintf("not enough bytes for session ID: %d", t)
}

type ExpectStateEnd struct {
	pos int
	lenraw int
}

func (e ExpectStateEnd) Error() string {
	return fmt.Sprintf("expected end of state at position %d, but raw length is %d", e.pos, e.lenraw)
}

func parseContacts(
	raw []byte,
	pos int) (map[[dhlen]byte]struct{}, int, error) {

	n, pos, err := parseUint32(raw, pos)
	if err != nil {
		return *new(map[[dhlen]byte]struct{}), pos, err
	}

	contacts := make(map[[dhlen]byte]struct{}, n)
	var contact [dhlen]byte
	for i := 0; i < n; i++ {
		contact, pos, err = parseDhlen(raw, pos)
		if err != nil {
			return contacts, pos, err
		}
		contacts[contact] = struct{}{}
	}
	return contacts, pos, nil
}

func parseSessionSecrets(
	raw []byte,
	pos int) (
	map[[SessionIdSize]byte][SecretSize]byte, int, error) {

	n, pos, err := parseUint32(raw, pos)
	if err != nil {
		return nil, pos, err
	}

	secrets := make(map[[SessionIdSize]byte][SecretSize]byte, n)
	var id [SessionIdSize]byte
	var secret [SecretSize]byte
	for i := 0; i < n; i++ {
		id, pos, err = parseSessionId(raw, pos)
		if err != nil {
			return secrets, pos, err
		}

		secret, pos, err = parseSecret(raw, pos)
		if err != nil {
			return secrets, pos, err
		}

		secrets[id] = secret
	}
	return secrets, pos, nil
}

func parseSecret(raw []byte, pos int) ([SecretSize]byte, int, error) {
	var secret [SecretSize]byte
	n := copy(secret[:], raw[pos:])
	if n != SecretSize {
		return secret, pos, TooShortForSecret(n)
	}

	return secret, pos + SecretSize, nil
}

type TooShortForSecret int

func (t TooShortForSecret) Error() string {
	return fmt.Sprintf("not enough bytes for secret: got %d, expecting %d", t, SecretSize)
}

type TooShortForUint32 int

func (t TooShortForUint32) Error() string {
	return fmt.Sprintf("not enough bytes for uint32: got %d, expected 4", t)
}

func parseUint32(raw []byte, pos int) (int, int, error) {
	const size = 4
	var bytes [size]byte
	copied := copy(bytes[:], raw[pos:])
	if copied < size {
		return 0, pos, TooShortForUint32(copied)
	}

	pos += size

	n := 0
	for i := 0; i < size; i++ {
		n += int(bytes[i] << (i * 8))
	}
	return n, pos, nil
}


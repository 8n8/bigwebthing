package main

import (
	"errors"
	"github.com/flynn/noise"
	"testing"
)

func TestStart(t *testing.T) {
	in := Start{}
	expected := GetArgs{}
	var state State
	got := in.update(&state)
	if got != expected {
		t.Errorf("expected %v but got %v", expected, got)
	}
}

func TestArgumentsUpdate(t *testing.T) {
	in := Arguments([]string{"bwt", "update"})
	expected := ReadStaticKeysFile{}
	var state State
	got := in.update(&state)
	if got != expected {
		t.Errorf("expected %v but got %v", expected, got)
	}

	if state.mode != UpdateCrypto {
		t.Errorf("expected state.mode == %d but got %d", UpdateCrypto, state.mode)
	}
}

func TestArgumentsEmpty(t *testing.T) {
	in := Arguments([]string{})
	var state State
	got := in.update(&state)
	seq := got.(Sequence)
	_, printed := seq[0].(Print)
	_, ended := seq[1].(End)
	if !(printed && ended) {
		t.Errorf("expected printed and ended, but got %v", got)
	}
}

func TestNewStaticKeysErr(t *testing.T) {
	var in NewStaticKeys
	in.err = errors.New("error")

	var state State
	got := in.update(&state)

	_, ok := got.(Panic)
	if !ok {
		t.Errorf("expecting panic but got %v", got)
	}
}

func TestNewStaticKeysOk(t *testing.T) {
	in := NewStaticKeys{
		err: nil,
		keys: noise.DHKey{
			Private: make([]byte, dhlen),
			Public:  make([]byte, dhlen),
		},
	}
	in.keys.Private[0] = 55

	var state State
	got := in.update(&state)

	if state.staticKeys.Private[0] != 55 {
		t.Errorf("failed to copy new static keys")
	}

	seq := got.(Sequence)
	_, write := seq[0].(WriteStaticKeys)
	_, connect := seq[1].(ConnectToServer)
	if !(write && connect) {
		t.Errorf("expected write and connect, but got %v", got)
	}
}

func TestStaticKeysFileErr(t *testing.T) {
	in := StaticKeysFile{
		raw: make([]byte, dhlen*2),
		err: errors.New("not nil"),
	}

	got := in.update(new(State))
	_, ok := got.(MakeStaticKeys)
	if !ok {
		t.Errorf("expected make static keys, but got %v", got)
	}
}

func TestStaticKeysFileOk(t *testing.T) {
	in := StaticKeysFile{
		raw: make([]byte, dhlen*2),
		err: nil,
	}
	in.raw[32] = 45

	var state State
	state.mode = UpdateCrypto
	got := in.update(&state)

	_, ok := got.(ConnectToServer)
	if !ok {
		t.Errorf("expected ConnectToServer, but got %v", got)
	}

	if state.staticKeys.Public[0] != 45 {
		t.Errorf("failed to copy static keys from file into state")
	}
}

func TestServerConnectionErr(t *testing.T) {
	var in ServerConnection
	in.err = errors.New("not nil")

	var state State
	got := in.update(&state).(Sequence)

	expected := badServer.(Sequence)

	if got[0] != expected[0] {
		t.Errorf("expected %v, but got %v", expected[0], got[0])
	}

	if got[1] != expected[1] {
		t.Errorf("expected %v, but got %v", expected[1], got[1])
	}
}

func TestServerConnectionOk(t *testing.T) {
	var in ServerConnection
	var state State
	got := in.update(&state)

	read, ok := got.(Read)
	if !ok {
		t.Errorf("expected Read, but got %v", got)
	}

	if read.size != SecretSize {
		t.Errorf("expected %d, but got %d", SecretSize, read.size)
	}
}

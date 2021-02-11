package main

import (
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

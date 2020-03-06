package main

import (
	"testing"
	"github.com/google/go-cmp/cmp"
	"github.com/google/gofuzz"
	"encoding/binary"
)


func TestMakeFriendlyNameParserSimpleOk(t *testing.T) {
	powServer := []byte{2,2,2,2,2,2,2,2}
	powClient := []byte{3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3}
	powflat := append(powServer, powClient...)
	newKey := make([]byte, 32)
	for i, _ := range newKey {
		newKey[i] = 4
	}
	request := append(append([]byte{1}, powflat...), newKey...)
	pow := proofOfWorkT{
		Server: powServer,
		Client: powClient,
	}
	expected := makeFriendlyNameRequest{
		ProofOfWork: pow,
		NewKey: newKey,
	}
	actual := parseRequest(request)
	diff := cmp.Diff(expected, actual)
	if diff != "" {
		t.Errorf("parseRequest (make friendly name): %v", diff)
	}
}

func TestMakeFriendlyNameParserFuzz(t *testing.T) {
	f := fuzz.New()
	for i := 0; i < 100; i++ {
		var requestMost []byte
		f.Fuzz(&requestMost)
		parseRequest(append([]byte{1}, requestMost...))
	}
}

func TestLookupNameSimple(t *testing.T) {
	f := fuzz.New()
	for i := 0; i < 100; i++ {
		var name uint64
		f.Fuzz(&name)
		buf := make([]byte, 8)
		_ = binary.PutUvarint(buf, name)
		request := append([]byte{2}, buf...)
		actual := parseRequest(request)
		expected := lookupNameT(name)
		if actual != expected {
			t.Errorf("expected %v, but got %v", expected, actual)
		}
	}
}

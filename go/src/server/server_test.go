package main

import (
	"testing"
	"github.com/google/go-cmp/cmp"
)


func TestMakeFriendlyNameParserSimpleOk(t *testing.T) {
	request := append([]byte{1}, make([]byte, 56)...)
	pow := proofOfWorkT{
		Server: make([]byte, 8),
		Client: make([]byte, 16),
	}
	expected := makeFriendlyNameRequest{
		ProofOfWork: pow,
		NewKey: make([]byte, 32),
	}
	actual := parseRequest(request)
	diff := cmp.Diff(expected, actual)
	if diff != "" {
		t.Errorf("parseRequest (make friendly name): %v", diff)
	}
}

package main

import (
	"github.com/google/go-cmp/cmp"
	"github.com/google/gofuzz"
	"testing"
)

func TestMakeFriendlyNameParserSimpleOk(t *testing.T) {
	powServer := []byte{2, 2, 2, 2, 2, 2, 2, 2}
	powClient := []byte{3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}
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
		NewKey:      newKey,
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

// func TestLookupNameSimple(t *testing.T) {
// 	f := fuzz.New()
// 	for i := 0; i < 100; i++ {
// 		var name uint64
// 		f.Fuzz(&name)
// 		buf := make([]byte, 8)
// 		_ = binary.PutUvarint(buf, name)
// 		request := append([]byte{2}, buf...)
// 		actual := parseRequest(request)
// 		expected := lookupNameT(name)
// 		if actual != expected {
// 			t.Errorf("expected %v, but got %v", expected, actual)
// 		}
// 	}
// }

func TestEncodeInt0(t *testing.T) {
	got := encodeInt(0)
	want := []byte{0, 0, 0, 0, 0, 0}
	diff := cmp.Diff(got, want)
	if diff != "" {
		t.Errorf("diff %v", diff)
	}
}

func TestEncodeInt1(t *testing.T) {
	got := encodeInt(1)
	want := []byte{1, 0, 0, 0, 0, 0}
	diff := cmp.Diff(got, want)
	if diff != "" {
		t.Errorf("got %v, but want %v", got, want)
	}
}

func TestDecodeInt0(t *testing.T) {
	got := decodeInt([]byte{0, 0, 0, 0, 0, 0})
	want := 0
	if got != want {
		t.Errorf("got %v, but want %v", got, want)
	}
}

func TestDecodeInt1(t *testing.T) {
	got := decodeInt([]byte{1, 0, 0, 0, 0, 0})
	want := 1
	if got != want {
		t.Errorf("got %v, but want %v", got, want)
	}
}

func TestDecodeInt257(t *testing.T) {
	got := decodeInt([]byte{1, 1, 0, 0, 0, 0})
	want := 257
	if got != want {
		t.Errorf("got %v, but want %v", got, want)
	}
}

func TestIntEncodeDecode(t *testing.T) {
	cases := []int{0, 55555, 10, 82, 9999999999999}
	for _, theInt := range cases {
		encoded := encodeInt(theInt)
		decoded := decodeInt(encoded)
		if decoded != theInt {
			t.Errorf("expected %v, but got %v. Encoded is %v", theInt, decoded, encoded)
			return
		}
	}
}

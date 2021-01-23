package main

import "testing"

func TestEncodeUint320(t *testing.T) {
	got := encodeUint32(0)
	if !bytesEqual(got, []byte{0, 0, 0, 0}) {
		t.Errorf("encodeUint32(0) = %v; want [0, 0, 0, 0]", got)
	}
}

func TestEncodeUint321(t *testing.T) {
	got := encodeUint32(1)
	if !bytesEqual(got, []byte{1, 0, 0, 0}) {
		t.Errorf("encodeUint32(1) = %v; want [1, 0, 0, 0]", got)
	}
}

func TestEncodeUint32_256(t *testing.T) {
	got := encodeUint32(256)
	if !bytesEqual(got, []byte{0, 1, 0, 0}) {
		t.Errorf("encodeUint32(256) = %v; want [0, 1, 0, 0]", got)
	}
}

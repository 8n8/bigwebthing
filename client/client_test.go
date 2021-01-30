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

func bytesEqual(as []byte, bs []byte) bool {
	if len(as) != len(bs) {
		return false
	}
	for i, a := range as {
		if a != bs[i] {
			return false
		}
	}
	return true
}

func TestRandomDifferent(t *testing.T) {
	const n = 10000
	const bufsize = 6
	rands := make(map[[bufsize]byte]struct{})
	secret, err := makeSessionSecret()
	if err != nil {
		t.Errorf("couldn't make session secret")
	}
	random, err := initRandomGen(secret)
	if err != nil {
		t.Errorf("couldn't initiate random")
	}
	for i := 0; i < n; i++ {
		var buf [bufsize]byte
		n, err := random.Read(buf[:])
		if n != bufsize {
			t.Errorf("couldn't write enough bytes to random buffer")
		}
		if err != nil {
			t.Errorf("error when filling random buffer: %v", err)
		}
		_, ok := rands[buf]
		if ok {
			t.Errorf("duplicate random: %v", buf)
		}
		rands[buf] = struct{}{}
	}
}

func TestRandomSame(t *testing.T) {
	secret, err := makeSessionSecret()
	if err != nil {
		t.Errorf("couldn't make session secret")
	}
	r1, err := initRandomGen(secret)
	if err != nil {
		t.Errorf("couldn't initiate random 1")
	}

	r2, err := initRandomGen(secret)
	if err != nil {
		t.Errorf("couldn't initiate random 2")
	}

	const bufsize = 10

	var buf1 [bufsize]byte
	n, err := r1.Read(buf1[:])
	if err != nil {
		t.Errorf("couldn't fill buf1")
	}
	if n != bufsize {
		t.Errorf("not enough bytes for buf1")
	}

	var buf2 [bufsize]byte
	n, err = r2.Read(buf2[:])
	if err != nil {
		t.Errorf("couldn't fill buf2")
	}
	if n != bufsize {
		t.Errorf("not enough bytes for buf2")
	}

	if buf1 != buf2 {
		t.Errorf("buffers should be equal, but got %v and %v", buf1, buf2)
	}
}

func TestParseUserIdEmpty(t *testing.T) {
	_, err := parseUserId("")
	if err == nil {
		t.Errorf("succeeded on empty string, expected fail")
	}
}

func TestParseUserIdJunk(t *testing.T) {
	_, err := parseUserId("asdflaksdjf;lkh ereriuerpqwer ")
	if err == nil {
		t.Errorf("succeeded on junk input, expected fail")
	}
}

func TestParseValidId(t *testing.T) {
	got, err := parseUserId("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
	if err != nil {
		t.Errorf("failed on good input")
	}

	if len(got) != 32 {
		t.Errorf("expected 32 byte output, got %d", len(got))
	}

	for _, g := range got {
		if g != 0 {
			t.Errorf("expected a zero, but got %d", g)
		}
	}
}

func TestParseArgsBwt(t *testing.T) {
	got, err := parseArgs([]string{})
	if err != nil {
		t.Errorf("error on valid args: %v", err)
	}
	_, ok := got.(Bwt)
	if !ok {
		t.Errorf("expected Bwt, but got %v", got)
	}
}

func TestParseArgsHelp(t *testing.T) {
	got, err := parseArgs([]string{"help"})
	if err != nil {
		t.Errorf("error on valid args: %v", err)
	}
	_, ok := got.(Help)
	if !ok {
		t.Errorf("expected Help, but got %v", got)
	}
}

func TestParseArgsMyId(t *testing.T) {
	got, err := parseArgs([]string{"myid"})
	if err != nil {
		t.Errorf("error on valid args: %v", err)
	}
	_, ok := got.(MyId)
	if !ok {
		t.Errorf("expected MyId, but got %v", got)
	}
}

func TestParseArgsRead(t *testing.T) {
	got, err := parseArgs([]string{"read"})
	if err != nil {
		t.Errorf("error on valid args: %v", err)
	}
	_, ok := got.(Read_)
	if !ok {
		t.Errorf("expected Read, but got %v", got)
	}
}

func TestParseArgsWrite(t *testing.T) {
	got, err := parseArgs([]string{"write", "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA", "hi"})
	if err != nil {
		t.Errorf("error on valid args: %v", err)
	}
	_, ok := got.(Write_)
	if !ok {
		t.Errorf("expected Write_, but got %v", got)
	}
}

func TestParseArgsAddContact(t *testing.T) {
	got, err := parseArgs([]string{"addcontact", "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"})
	if err != nil {
		t.Errorf("error on valid args: %v", err)
	}
	_, ok := got.(AddContact)
	if !ok {
		t.Errorf("expected AddContact, but got %v", got)
	}
}

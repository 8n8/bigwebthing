package main

import (
	// "github.com/flynn/noise"
	// "crypto/rand"
	// "fmt"
	// "io"
	// "os"
	// "io/ioutil"
	// "net"
)

var goChan chan func() = make(chan func())
var killCh chan struct{} = make(chan struct{})

func main() {
	goChan <- main_

	go func() {
		for {
			go (<-goChan)()
		}
	}()

	<-killCh
}

var inCh chan In = make(chan In)

func main_() {
	inCh <- Start{}
	for {
		(<-inCh).update().run()
	}
}

type In interface {
	update() Out
}

/*
type In interface {
	update(*State) Out
}

type Status int

const dhlen = 32

const (
	NoKeys Status = iota
	ReadingKeys
	NewKeys
	GotKeys
)

type ConnStatus int

const (
	ReadingXk1 ConnStatus = iota
	ReadingXk3
	ReadingTransport
)

type Conn struct {
	status ConnStatus
	conn net.Conn
	readingSize bool
	handshake *noise.HandshakeState
	tx *noise.CipherState
	rx *noise.CipherState
}

type State struct {
	staticKeys noise.DHKey
	keysStatus Status
	listener net.Listener
	gotListener bool
	conns map[int]Conn
	awaitingConn bool
	unique int
}

type Start struct{}

func (Start) update(s *State) (Update, Out) {
	return next(s)
}

type Outs map[Out]struct{}

func (outs Outs) run() {
	for out := range outs {
		inCh <- Run{out}
	}
}

type Run struct {
	out Out
}

const maxConns = 1000

func next(s *State) Out {
	outs := make(map[Out]struct{})

	switch s.keysStatus {
	case NoKeys:
		outs[GetStaticKeysFile{}] = struct{}{}
	case NewKeys:
		outs[WriteKeys(encodeKeys(s.staticKeys))] = struct{}{}
	case GotKeys:
		if !s.gotListener {
			outs[GetListener{}] = struct{}{}
		}
	}

	return Outs(outs)
}

type GetListener struct{}

func (GetListener) run() {
	listener, err := net.Listen("tcp", ":59688")
	inCh <- Listener{listener, err}
}

type Listener struct {
	listener net.Listener
	err error
}

func (l Listener) update(s *State) Out {
	if l.err != nil {
		return Panic{l.err}
	}

	s.listener = l.listener
	s.gotListener = true
	return ListenToListener{l.listener}
}

type ListenToListener struct {
	listener net.Listener
}

func (l ListenToListener) run() {
	conn, err := l.listener.Accept()
	inCh <- NewConn{conn, err}
}

type NewConn struct {
	conn net.Conn
	err error
}

func (c NewConn) update(s *State) Out {
	if c.err == nil {
		id := s.unique
		s.unique++
		s.conns[id] = Conn{
			status: ReadingXk1,
			conn: c.conn,
			readingSize: true,
		}

		return Read{c.conn, 2, id}
	}

	return ListenToListener{s.listener}
}

type WriteKeys []byte

func (keys WriteKeys) run() {
	inCh <- WriteKeysErr{ioutil.WriteFile(staticKeysFile, keys, 0400)}
}

type WriteKeysErr struct {
	err error
}

func (w WriteKeysErr) update(s *State) (Update, Out) {
	if w.err != nil {
		return Panic{w.err}
	}

	return next(s)
}

func encodeKeys(keys noise.DHKey) []byte {
	encoded := make([]byte, 0, 2 * dhlen)
	encoded = append(encoded, keys.Private...)
	encoded = append(encoded, keys.Public...)
	return encoded
}

type GetStaticKeysFile struct{}

const staticKeysFile = "staticKeys"

func (GetStaticKeysFile) run() {
	const NotUsed = 0400
	f, err := os.OpenFile(staticKeysFile, os.O_RDONLY, NotUsed)
	inCh <- StaticKeysFile{f, err}
}

type StaticKeysFile struct {
	f io.Reader
	err error
}

type DoNothing struct{}

func (DoNothing) run() {
}

func parseStaticKeys(raw []byte) (noise.DHKey, error) {
	var keys noise.DHKey

	n := copy(keys.Private, raw) + copy(keys.Public, raw[dhlen:])
	if n != 2 * dhlen {
		return keys, fmt.Errorf("expecting %d bytes but got %d", 2 * dhlen, n)
	}

	return keys, nil
}

type MakeStaticKeys struct{}

func (MakeStaticKeys) run() {
	keys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	inCh <- NewStaticKeys{keys, err}
}

func (k NewStaticKeys) update(s *State) Out {
	if k.err != nil {
		return Panic{k.err}
	}

	s.staticKeys = k.keys
	s.keysStatus = NewKeys
	return next(s)
}

type NewStaticKeys struct {
	keys noise.DHKey
	err error
}

func (kf StaticKeysFile) update(s *State) Out {
	if kf.err != nil {
		return DoNothing{}, MakeStaticKeys{}
	}

	s.keysStatus = ReadingKeys
	return Read{kf.f, 2*dhlen, ReadingStaticKeys}
}

const ReadingStaticKeys = -1

type Read struct {
	f io.Reader
	size int
	context int
}

func (r Read) run() {
	buf := make([]byte, r.size)
	n, err := r.f.Read(buf)
	inCh <- ReadResult{buf, n, err, r.context}
}

type ReadResult struct {
	buf []byte
	n int
	err error
	context int
}

func handleConnRead(conn Conn, r ReadResult, s *State) Out {
	if conn.readingSize {
		if r.n != 2 {
			return DeleteConn(r.context)
		}

		size := int(r.buf[0]) + int((r.buf[1] << 8))
		outs := make(map[Out]struct{})
		outs[Read{conn.conn, size, r.context}] = struct{}{}
		outs[NotReadingSize(r.context)] = struct{}{}
		return Outs(outs)
	}
}

type NotReadingSize int

func (n NotReadingSize) run(s *State) {
	conn, _ := s.conns[n]
	conn.notReadingSize = true
	s.conns[n] = conn
}

type Update interface {
	run(*State)
}

func (r ReadResult) update(s *State) (Update, Out) {
	if r.context == ReadingStaticKeys {
		if r.err != io.EOF && r.err != nil {
			return Panic{r.err}
		}

		if r.n != 2 * dhlen {
			return Panic{fmt.Errorf("expecting %d bytes in static keys file, but got %d", 2 * dhlen, r.n)}
		}

		keys, err := parseStaticKeys(r.buf)
		if err != nil {
			return Panic{fmt.Errorf("couldn't parse static keys: %s", err)}
		}

		s.staticKeys = keys
		s.keysStatus = GotKeys

		return next(s)
	}

	conn, ok := s.conns[r.context]
	if ok {
		return handleConnRead(conn, r)
	}

	return Panic{fmt.Errorf("unknown read result %+v", r)}
}

type Out interface {
	run()
}

type Panic struct {
	err error
}

func (p Panic) run() {
	panic(p)
}
*/

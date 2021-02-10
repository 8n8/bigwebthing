package main

import (
	"github.com/flynn/noise"
	"os"
	"fmt"
	"io"
	"net"
)

// modes
const UpdateCrypto = 1

// status
const ListeningForXk2 = 1
const ListeningForNewKk1s = 2
const MakingSessionSecret = 3

const Kk1Size = 48

type State struct {
	mode interface{}
	staticKeys noise.DHKey
	status int
	kk1 [Kk1Size]byte
}

type In interface {
	update(*State) Out
}

type Out interface {
	run(chan In)
}

type Sequence []Out

func (s Sequence) run(in chan In) {
	for _, out := range s {
		out.run(in)
	}
}

type GetStaticKeysFile struct{}

const staticKeysPath = "staticKeys"

func (GetStaticKeysFile) run(in chan In) {
	f, err := os.Open(staticKeysPath)
	in <- StaticKeysFile{f, err}
}

type StaticKeysFile struct {
	f io.Reader
	err error
}

func (s StaticKeysFile) update(state *State) Out {
	if s.err != nil {
		return MakeStaticKeys{}
	}

	if state.mode == UpdateCrypto {
		keys, err := parseStaticKeys(s.f)
		if err != nil {
			panic(err)
		}
		return ConnectToServer{}
	}

	panic("bad mode")
}

type ConnectToServer struct{}

func (ConnectToServer) run(in chan In) {
	conn, err := net.Dial("tcp", serverUrl)
	in <- ServerConn{conn, err}
}

func (c ServerConn) update(state *State) Out {
	if c.err ! = nil {
		return badServer
	}

	var err error

	staticKeys := parseStaticKeys(c.conn)

	if state.mode == UpdateCrypto {
		state.status = ListeningForXk2
		state.conn = c.conn
		state.handshake, err = noise.NewHandshakeState(noiseServerConfig(
	}
}

const serverUrl = "localhost:3001"

type ServerConn struct {
	conn net.Conn
	err error
}

type End struct{}

func (End) run(chan In) {}

type GetArgs struct{}

type Args []string

func (GetArgs) run(in chan In) {
	in <- Args(os.Args)
}

func (a Args) update(state *State) Out {
	if len(a) == 2 && a[1] == "update" {
		state.mode = UpdateCrypto
		return GetStaticKeysFile{}
	}

	return Sequence([]Out{Print("bad arguments"), End{}})
}

type Print string

func (p Print) run(chan In) {
	fmt.Print(p)
}

func main() {
	var state State
	var out Out = GetArgs{}
	in := make(chan In)
	for _, end := out.(End); !end; {
		go out.run(in)
		out = (<-in).update(&state)
	}
}

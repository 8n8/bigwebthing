package main

import (
	"github.com/webview/webview"
	"github.com/mitchellh/go-homedir"
	"github.com/flynn/noise"
	"fmt"
	"path"
	"os"
	"io"
	"sync"
	"net"
	"crypto/aes"
	"crypto/rand"
	"net/http"
)

type State struct {
	homeDir string
	staticKeys noise.DHKey
	gotServerConn bool
	serverConn net.Conn
	serverShake *noise.HandshakeState
	gotServerShake bool
	httpConn HttpRequest
}

var stateLock sync.Mutex

type Panic string

func initState() State {
	return State{
	homeDir: "",
	}
}

var goCh chan func() = make(chan func())
var inCh chan In = make(chan In)

func start() {
	inCh <- Start{}
	state := initState()
	for {
		(<-inCh).update(&state).run(&state)
	}
}

type In interface {
	// Don't edit the state and don't do any IO in this function.
	update(*State) Out
}

type Out interface {
	// Be sure to aquire stateLock before editing State.
	// Don't do any logic. It's just for doing IO.
	run(*State)
}

type Start struct{}

func (Start) update(*State) Out {
	return GetDataDir{}
}

type GetDataDir struct{}

func (GetDataDir) run(*State) {
	home, err := homedir.Dir()
	inCh <- HomeDir{home, err}
}

type HomeDir struct {
	home string
	err error
}

func staticKeysPath(home string) string {
	return path.Join(home, "statickeys")
}

func (h HomeDir) update(*State) Out {
	if h.err != nil {
		return Panic(fmt.Sprintf("can't get home directory: %v", h.err))
	}

	return Outs([]Out{
		SetHomeDir(h.home),
		ReadFile(staticKeysPath(h.home)),
	})
}

type Outs []Out

func (outs Outs) run(state *State) {
	for _, out := range outs {
		out.run(state)
	}
}

func (p Panic) run(*State) {
	panic(p)
}

type ReadFile string

func (path ReadFile) run(*State) {
	f, err := os.Open(string(path))
	inCh <- File{string(path), f, err}
}

type File struct {
	path string
	f *os.File
	err error
}

const dhlen = 32

func (f File) update(s *State) Out {
	if f.path == staticKeysPath(s.homeDir) {
		return ReadCloser{f.path, f.f, 2*dhlen}
	}

	return Panic("bad path: " + f.path)
}

type ReadReader struct {
	context int
	f io.Reader
	size int
}

type ReadCloser struct {
	context string
	f io.ReadCloser
	size int
}

func (r ReadReader) run(*State) {
	goCh <- func() {
		buf := make([]byte, r.size)
		n, err := r.f.Read(buf)
		inCh <- ReaderResult{r.context, n, err, buf}
	}
}

func (r ReaderResult) update(s *State) Out {
	if r.context == serverSeedContext {
		if r.err != nil {
			return Panic("couldn't generate server random seed: " + r.err.Error())
		}
		if r.n != 2*dhlen {
			return Panic(fmt.Sprintf("not enough random bytes for server seed: expecting %d, got %d", 2*dhlen, r.n))
		}

		shake, err := initServerShake(r.buf)
		if err != nil {
			return Panic("couldn't initialise server handshake: " + err.Error())
		}

		xk1, _, _, err := shake.WriteMessage([]byte{}, []byte{})
		if err != nil {
			return Panic("couldn't make XK1: " + err.Error())
		}

		return Outs([]Out{
			SetServerShake(shake),
			WriteWriter{
				context: sendXk1Context,
				writer: s.serverConn,
				message: xk1,
			},
			ReadReader{
				context: fromServerContext,
				f: s.serverConn,
				size: XK2Size,
			},
		})
	}
}

type WriteWriter struct {
	context int
	writer io.Writer
	message []byte
}

func (w WriteWriter) run(*State) {
	goCh <- func() {
		n, err := w.writer.Write(w.message)
		inCh <- WriterResult{
			context: w.context,
			n: n,
			err: err,
			size: len(w.message),
		}
	}
}

type WriterResult struct {
	context int
	n int
	err error
	size int
}

func (w WriterResult) update(state *State) Out {
	if w.context == sendXk1Context {
		if w.err != nil {
			return badServer(state)
		}
		if w.n != XK1Size {
			return badServer(state)
		}
		return DoNothing{}
	}
	return Panic("unknown WriterResult: " + showContext(w.context))
}

func showContext(context int) string {
	switch context {
	case serverSeedContext:
		return "server seed"
	case staticKeysContext:
		return "static keys"
	case fromServerContext:
		return "from server"
	case sendXk1Context:
		return "send XK1"
	}
	return "unknown"
}

func badServer(*State) Out {
	return SetNotGotServerConn{}
}

type SetNotGotServerConn struct{}

func (SetNotGotServerConn) run(s *State) {
	stateLock.Lock()
	defer stateLock.Unlock()
	s.gotServerConn = false
}

const XK2Size = 64

type ReaderResult struct {
	context int
	n int
	err error
	buf []byte
}

type CloserResult struct {
	context int
	n int
	err error
	buf []byte
	f io.ReadCloser
}

type CloseHandle struct {
	closer io.Closer
}

func (handle CloseHandle) run(*State) {
	handle.closer.Close()
}



func (r CloserResult) update(s *State) Out {
	if r.context == staticKeysContext {
		if r.err != nil {
			return Panic("couldn't read static keys file: " + r.err.Error())
		}

		if r.n != 2 * dhlen {
			return Panic(fmt.Sprintf("bad static keys file read: expecting %d bytes but got %d bytes", 2 * dhlen, r.n))
		}

		return Outs([]Out{
			CloseHandle{r.f},
			SetStaticKeys(noise.DHKey{
				Private: r.buf[:dhlen],
				Public: r.buf[dhlen:],
			}),
			StartHttpServer{},
			GetServerConn{},
			StartGui{},
		})
	}

	return Panic("unknown ReadResult: " + string(r.context))
}

type StartHttpServer struct{}

func (StartHttpServer) run(*State) {
	goCh <- func() {
		http.HandleFunc(
			"/",
			func(w http.ResponseWriter, r *http.Request) {
				ch := make(chan struct{})
				inCh <- HttpRequest{w, r, ch}
				<-ch
			})
		http.ListenAndServe(clientPort, nil)
	}
}

type HttpRequest struct {
	w http.ResponseWriter
	r *http.Request
	ch chan struct{}
}

type SetHttpConn HttpRequest

func (s SetHttpConn) run(state *State) {
	stateLock.Lock()
	defer stateLock.Unlock()
	state.httpConn = HttpRequest(s)
}

type SetStaticKeys noise.DHKey

func (keys SetStaticKeys) run(state *State) {
	stateLock.Lock()
	defer stateLock.Unlock()
	state.staticKeys = noise.DHKey(keys)
}

func (h HttpRequest) update(s *State) Out {
	switch h.r.URL.Path {
	case "/":
		return Outs([]Out{
			SetHttpConn(h),
			ReadFile(indexHtmlPath(s.homeDir)),
		})
	}
	return Panic("bad request to HTTP server: " + h.r.URL.Path)
}

func indexHtmlPath(home string) string {
	return path.Join(home, "index.html")
}

func main() {
	goCh <- start
	for {
		go (<-goCh)()
	}
}

type GetServerConn struct{}

func (GetServerConn) run(*State) {
	goCh <- func() {
		conn, err := net.Dial("tcp", serverUrl)
		inCh <- ServerConn{conn, err}
	}
}

type ServerConn struct {
	conn net.Conn
	err error
}

type DoNothing struct{}

func (DoNothing) run(*State) {
}

func (c ServerConn) update(*State) Out {
	if c.err != nil {
		return DoNothing{}
	}

	return Outs([]Out{
		SetServerConn{c.conn},
		ReadReader{
			context: serverSeedContext,
			f: rand.Reader,
			size: 2*aes.BlockSize,
		},
	})

}

type SetServerConn struct {
	conn net.Conn
}

func (c SetServerConn) run(state *State) {
	stateLock.Lock()
	defer stateLock.Unlock()
	state.serverConn = c.conn
}

const (
	serverSeedContext = iota
	staticKeysContext
	fromServerContext
	sendXk1Context
)

const serverUrl = "localhost:8001"

const clientPort = ":18508"

type StartGui struct{}

func (StartGui) run(*State) {
	goCh <- func() {
		w := webview.New(false)
		defer w.Destroy()
		w.SetTitle("BigWebThing")
		w.SetSize(800, 600, webview.HintNone)
		w.Navigate("https://whatthestudentsthink.uk")
		w.Run()
	}
}

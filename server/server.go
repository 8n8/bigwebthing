package main

import (
	"net"
	"crypto/rand"
	"crypto/aes"
	"time"
	"fmt"
)

var toGo = make(chan func())

func main() {
	toGo <- updateLoop
	for {
		go (<-toGo)()
	}
}

var in = make(chan In)

type In interface {
	update() Out
}

type Out interface {
	io()
}

func updateLoop() {
	in <- Start{}
	for {
		(<-in).update().io()
	}
}

type Start struct{}

func (Start) update() Out {
	return Outs([]Out{
		StartCache{},
		StartTcpServer{},
	})
}

func (StartCache) io() {
	toGo <- cacheManager
}

type StartTcpServer struct{}

type StartCache struct{}

func (StartTcpServer) io() {
	listener, err := net.Listen("tcp", "1010")
	in <- TcpListener{listener, err}
}

type TcpListener struct {
	listener net.Listener
	err error
}

func (t TcpListener) update() Out {
	if t.err != nil {
		return Panic("couldn't start TCP server: " + t.err.Error())
	}

	return Outs([]Out{
		GetConn{t.listener},
		CacheRequest{CacheListener{t.listener}},
	})
}

type CacheListener struct {
	listener net.Listener
}

func (c CacheListener) update(cache *Cache) {
	cache.listener = ListenerExists{c.listener}
}

type GetListener struct{}

func (GetListener) update(cache *Cache) {
	in <- cache.listener.makeInput()
}

type ListenerExists struct {
	listener net.Listener
}

func (l ListenerExists) update() Out {
	return GetConn{l.listener}
}

func (l ListenerExists) makeInput() In {
	return l
}

type MaybeListener interface {
	makeInput() In
}

type Panic string

type Outs []Out

func (outs Outs) io() {
	for _, out := range outs {
		out.io()
	}
}

func (p Panic) io() {
	panic(string(p))
}

type GetConn struct {
	listener net.Listener
}

func (g GetConn) io() {
	conn, err := g.listener.Accept()
	in <- NewConn{conn, err}
}

type NewConn struct {
	conn net.Conn
	err error
}

func (n NewConn) update() Out {
	if n.err != nil {
		return CacheRequest{GetListener{}}
	}

	return Outs{
		SetDeadline{n.conn},
		Go{ReadXk1{n.conn}},
	}
}

type SetDeadline struct {
	conn net.Conn
}

func (s SetDeadline) io() {
	err := s.conn.SetDeadline(time.Now().Add(time.Second * 5))
	in <- DeadlineError{s.conn, err}
}

type DeadlineError struct {
	conn net.Conn
	err error
}

func (d DeadlineError) update() Out {
	if d.err != nil {
		return CloseConn{d.conn}
	}

	return DoNothing{}
}

type CloseConn struct {
	conn net.Conn
}

func (c CloseConn) io() {
	c.conn.Close()
}

type DoNothing struct{}

func (DoNothing) io() {}

type Go struct {
	out Out
}

func (g Go) io() {
	toGo <- func() {g.out.io()}
}

type ReadXk1 struct {
	conn net.Conn
}

const xk1Size = 48

func (r ReadXk1) io() {
	raw := make([]byte, xk1Size)
	r.conn.SetDeadline(time.Now().Add(time.Second * 5))
	n, err := r.conn.Read(raw)
	in <- NewXk1{r.conn, raw, n, err}
}

type NewXk1 struct {
	conn net.Conn
	raw []byte
	n int
	err error
}

func (n NewXk1) update() Out {
	if n.err != nil {
		return CloseConn{n.conn}
	}

	if n.n != xk1Size {
		return CloseConn{n.conn}
	}

	return Outs([]Out{
		CacheRequest{Xk1AwaitingRand{n.conn, n.raw}},
		GetRandomForNoise{},
	})
}

func (x Xk1AwaitingRand) update(cache *Cache) {
	cache.xk1sAwaitingRand = append(cache.xk1sAwaitingRand, x)
}

type GetRandomForNoise struct{}

func (GetRandomForNoise) io() {
	buf := make([]byte, 2 * aes.BlockSize)
	n, err := rand.Read(buf)
	in <- RandomForNoise{n, err, buf}
}

type RandomForNoise struct {
	n int
	err error
	buf []byte
}

func notEnoughRandBytes(n int) Out {
	return Panic(fmt.Sprintf(
		"not enough random bytes generated for Noise: expecting %d but got %d",
		2 * aes.BlockSize,
		n))
}

func badRandBytes(err error) Out {
	return Panic(fmt.Sprintf(
		"couldn't generate random bytes for Noise: %s", err))
}

func (r RandomForNoise) update() Out {
	if r.n != 2 * aes.BlockSize {
		return notEnoughRandBytes(r.n)
	}

	if r.err != nil {
		return badRandBytes(r.err)
	}

	return Outs([]Out{
		CacheRequest{CacheRandomForNoise(r.buf)},
		CacheRequest{GetXk1AndRand{}},
	})
}

type CacheRandomForNoise []byte

func (c CacheRandomForNoise) update(cache *Cache) {
	cache.randsForNoise = append(cache.randsForNoise, []byte(c))
}

type CacheRequest struct {
	request ToCache
}

func (c CacheRequest) io() {
	toCache <- c.request
}

type GetXk1AndRand struct{}

type BadCache string

func (b BadCache) update() Out {
	return Panic(string(b))
}

func (GetXk1AndRand) update(cache *Cache) {
	if len(cache.xk1sAwaitingRand) == 0 {
		in <- BadCache("empty xk1sAwaitingRand")
		return
	}

	if len(cache.randsForNoise) == 0 {
		in <- BadCache("empty randsForNoise")
		return
	}

	result := Xk1AndRandAndStatic{
		xk1: make([]byte, xk1Size),
		random: make([]byte, 2 * aes.BlockSize),
		staticKeys: cache.staticKeys.copyKeys(),
	}

	for i, x := range cache.xk1sAwaitingRand[0].xk1 {
		result.xk1[i] = x
	}

	for i, r := range cache.randsForNoise[0] {
		result.random[i] = r
	}

	result.conn = cache.xk1sAwaitingRand[0].conn

	cache.xk1sAwaitingRand = cache.xk1sAwaitingRand[1:]
	cache.randsForNoise = cache.randsForNoise[1:]

	in <- result
}

type Xk1AndRandAndStatic struct {
	conn net.Conn
	xk1 []byte
	random []byte
	staticKeys MaybeStaticKeys
}

func (x Xk1AndRandAndStatic) update() Out {
	config := makeConfig(x.random, x.staticKeys)
	shake, err := noise.NewHandshakeState(config)
	if err != nil {
		return Panic(
			"couldn't make new handshake state: " +
			err.Error())
	}

	_, _, _, err := shake.ReadMessage(x.xk1)
	if err != nil {
		return CloseConn{x.conn}
	}

	xk2, _, _, err :=
}

type MaybeStaticKeys interface {
	copyKeys() MaybeStaticKeys
}

var toCache = make(chan ToCache)

type ToCache interface {
	update(*Cache)
}

func cacheManager() {
	cache := initCache()
	for {
		(<-toCache).update(&cache)
	}
}

func initCache() Cache {
	return Cache{
		kk1s: make([]Kk1, 0),
		kk2s: make([]Kk2, 0),
		kkTransports: make([]KkTransport, 0),
		addContacts: make([]Contact, 0),
		removeContacts: make([]Contact, 0),
		payments: make([]Payment, 0),
	}
}

type NoDhKey struct{}

type Cache struct {
	kk1s []Kk1
	kk2s []Kk2
	kkTransports []KkTransport
	addContacts []Contact
	removeContacts []Contact
	payments []Payment
	xk1sAwaitingRand []Xk1AwaitingRand
	randsForNoise [][]byte
	listener MaybeListener
	staticKeys MaybeStaticKeys
}

type Xk1AwaitingRand struct {
	conn net.Conn
	xk1 []byte
}

type Kk1 struct {
	timestamp int
	recipient []byte
	sender []byte
	kk1 []byte
}

type Kk2 struct {
	timestamp int
	recipient []byte
	sender []byte
	firstHalfKk1 []byte
	kk2 []byte
}

type KkTransport struct {
	timestamp int
	recipient []byte
	sender []byte
	firstHalfKk1 []byte
	bodyPath string
}

type Contact struct {
	timestamp int
	contacter []byte
	contactee []byte
}

type Payment struct {
	timestamp int
	amount int
	payer []byte
}

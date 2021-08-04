package main

import (
	"database/sql"
	"fmt"
	"github.com/flynn/noise"
	_ "github.com/mattn/go-sqlite3"
	"net"
	"os"
)

// MAIN

var goCh = make(chan func())

func main() {
	go func() {
		in <- Start{}
		for {
			(<-in).pure().io()
		}
	}()
	for {
		go (<-goCh)()
	}
}

var in = make(chan I)

type I interface {
	pure() O
}

type O interface {
	io()
}

// INPUTS

type ConnInfo struct {
	address string
	result  Cache
}

type ConnReadResult struct {
	address string
	buf     []byte
	n       int
	err     error
}

type Start struct{}

type NewDbHandle struct {
	db  *sql.DB
	err error
}

type DbExecErr struct {
	err  error
	sql_ string
}

type FileContents struct {
	path     string
	contents []byte
	err      error
}

type NewTcpListener struct {
	listener net.Listener
	err      error
}

type NewConn struct {
	conn net.Conn
	err  error
}

// OUTPUTS

type CacheReading struct {
	conn net.Conn
	size int
}

type CloseConn struct {
	conn net.Conn
}

type RemoveConnFromCache string

type FetchConnInfo string

type CacheReadingXx1 struct {
	conn net.Conn
}

type ReadConn struct {
	conn net.Conn
	size int
}

type WaitForConn struct {
	listener net.Listener
}

type GetDbHandle struct{}

type InitCache struct{}

type SaveDbHandle struct {
	db *sql.DB
}

type Os []O

type Panic string

type DbExecPlain struct {
	db   *sql.DB
	sql_ string
}

type DoNothing struct{}

type ReadFile string

type CacheCertificate []byte

type CacheStaticKeys noise.DHKey

type StartTcpServer struct{}

type CacheListener struct {
	listener net.Listener
}

type Go struct {
	output O
}

// IO

func (GetDbHandle) io() {
	db, err := sql.Open("sqlite3", "database.sqlite")
	in <- NewDbHandle{db, err}
}

var toCache = make(chan ToCache)

type Cache struct {
	db          []*sql.DB
	staticKeys  []noise.DHKey
	certificate []byte
	listener    []net.Listener
	readingXx1  map[string]net.Conn
}

type ToCache interface {
	update(*Cache)
}

func (InitCache) io() {
	goCh <- func() {
		cache := Cache{
			db: []*sql.DB{},
		}
		for {
			(<-toCache).update(&cache)
		}
	}
}

func (s SaveDbHandle) update(cache *Cache) {
	cache.db = []*sql.DB{s.db}
}

func (os_ Os) io() {
	for _, o := range os_ {
		o.io()
	}
}

func (d DbExecPlain) io() {
	_, err := d.db.Exec(d.sql_)
	in <- DbExecErr{err, d.sql_}
}

func (DoNothing) io() {
}

func (p Panic) io() {
	panic(p)
}

func (s SaveDbHandle) io() {
	toCache <- s
}

func (r ReadFile) io() {
	contents, err := os.ReadFile(string(r))
	in <- FileContents{string(r), contents, err}
}

func (c CacheCertificate) io() {
	toCache <- c
}

func (c CacheCertificate) update(cache *Cache) {
	cache.certificate = c
}

func (c CacheStaticKeys) io() {
	toCache <- c
}

func (c CacheStaticKeys) update(cache *Cache) {
	cache.staticKeys = []noise.DHKey{noise.DHKey(c)}
}

func (StartTcpServer) io() {
	listener, err := net.Listen("tcp", ":8080")
	in <- NewTcpListener{listener, err}
}

func (c CacheListener) io() {
	toCache <- c
}

func (c CacheListener) update(cache *Cache) {
	cache.listener = []net.Listener{c.listener}
}

func (w WaitForConn) io() {
	conn, err := w.listener.Accept()
	in <- NewConn{conn, err}
}

func (r ReadConn) io() {
	buf := make([]byte, r.size)
	n, err := r.conn.Read(buf)
	in <- ConnReadResult{r.conn.RemoteAddr().String(), buf, n, err}
}

func (g Go) io() {
	goCh <- func() {
		g.io()
	}
}

func (f FetchConnInfo) io() {
	toCache <- f
}

func emptyCache() Cache {
	return Cache{
		db:          make([]*sql.DB, 0),
		staticKeys:  make([]noise.DHKey, 0),
		certificate: make([]byte, 0),
		listener:    make([]net.Listener, 0),
	}
}

func (f FetchConnInfo) update(cache *Cache) {
	address := string(f)

	result := emptyCache()

	for _, candidate := range cache.readingSize {
		if candidate.RemoteAddr().String() == address {
			result.readingSize = append(result.readingSize, candidate)
		}
	}

	for _, candidate := range cache.readResult {
		if candidate.address == address {
			result.readResult = append(result.readResult, candidate)
		}
	}

	in <- ConnInfo{address, result}
}

func (c CloseConn) io() {
	c.conn.Close()
}

func (r RemoveConnFromCache) io() {
	toCache <- r
}

func (r RemoveConnFromCache) update(c *Cache) {
	delete(c.readingXx1, string(r))
}

// PURE

const keysPath = "secretKeys"

const certificatePath = "certificate"

func (Start) pure() O {
	return sequence(
		GetDbHandle{},
		InitCache{},
		ReadFile(certificatePath),
		ReadFile(keysPath),
		StartTcpServer{})
}

const createUsersTable = `
CREATE TABLE IF NOT EXISTS users (
        userid INTEGER NOT NULL PRIMARY KEY,
        publickey BLOB NOT NULL UNIQUE,
        timestamp INTEGER NOT NULL
);
`

const createBadUsersTable = `
CREATE TABLE IF NOT EXISTS badusers (
        publickey BLOB NOT NULL PRIMARY KEY,
);
`

const createUnreadTable = `
CREATE TABLE IF NOT EXISTS unread (
        sendid INTEGER NOT NULL PRIMARY KEY,
        sender BLOB NOT NULL,
        recipient BLOB NOT NULL,
        message BLOB NOT NULL,
        timestamp INTEGER NOT NULL
);`

const createReadTable = `
CREATE TABLE IF NOT EXISTS read (
        sendid INTEGER NOT NULL PRIMARY KEY,
        sender BLOB NOT NULL,
        recipient BLOB NOT NULL,
        size INTEGER NOT NULL,
        timesent INTEGER NOT NULL,
        timereceived INTEGER NOT NULL
);`

const createPaymentTable = `
CREATE TABLE IF NOT EXISTS payment (
        paymentid INTEGER NOT NULL PRIMARY KEY,
        payer BLOB NOT NULL,
        amount INTEGER NOT NULL,
        timestamp INTEGER NOT NULL
);
`

const createContactTable = `
CREATE TABLE IF NOT EXISTS contact (
        contacter BLOB NOT NULL,
        contactee BLOB NOT NULL,
        PRIMARY KEY (contacter, contactee)
);
`

func (n NewDbHandle) pure() O {
	if n.err != nil {
		return Panic(
			"couldn't set up database: " + n.err.Error())
	}

	makeTable := func(makeSql string) O {
		return DbExecPlain{n.db, makeSql}
	}

	return sequence(
		makeTable(createUsersTable),
		makeTable(createBadUsersTable),
		makeTable(createUnreadTable),
		makeTable(createReadTable),
		makeTable(createPaymentTable),
		makeTable(createContactTable),
		SaveDbHandle{n.db})
}

func (d DbExecErr) pure() O {
	if d.err != nil {
		return Panic(
			"could not execute SQL: " +
				d.sql_ +
				d.err.Error())
	}

	return DoNothing{}
}

const dhlen = 32

func (f FileContents) pure() O {
	if f.err != nil {
		return Panic(fmt.Sprintf(
			"error reading file: %s: %s", f.path, f.err))
	}

	switch f.path {
	case keysPath:
		return CacheStaticKeys(noise.DHKey{
			Private: f.contents[:dhlen],
			Public:  f.contents[dhlen : 2*dhlen],
		})

	case certificatePath:
		return CacheCertificate(f.contents)
	}

	return Panic("unexpected file contents: %s" + f.path)
}

func sequence(outputs ...O) O {
	return Os(outputs)
}

func (t NewTcpListener) pure() O {
	if t.err != nil {
		return Panic("couldn't start TCP listener: " + t.err.Error())
	}

	return sequence(
		CacheListener{t.listener},
		WaitForConn{t.listener})
}

const xx1len = 48

func (n NewConn) pure() O {
	if n.err != nil {
		return DoNothing{}
	}

	return sequence(
		Go{ReadConn{n.conn, xx1len}},
		CacheReadingXx1{n.conn})
}

func (c CacheReadingXx1) io() {
	toCache <- c
}

func (c CacheReadingXx1) update(cache *Cache) {
	cache.readingXx1[c.conn.RemoteAddr().String()] = c.conn
}

func (c ConnReadResult) pure() O {
	return sequence(
		c,
		FetchConnInfo(c.address))
}

func (c ConnReadResult) io() {
	toCache <- c
}

func (c ConnReadResult) update(cache *Cache) {
	cache.readResult[c.address] = c
}

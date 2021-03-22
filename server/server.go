package main

import (
	"net"
	"database/sql"
	_ "github.com/mattn/go-sqlite3"
	"io"
	"fmt"
	"github.com/flynn/noise"
	"crypto/rand"
)

var GO = make(chan func())
var QUIT = make(chan struct{})

func main() {
	GO <- main_
	go func() {
		for {
			go (<-GO)()
		}
	}()
	<-QUIT
}

var IN = make(chan In)

func main_() {
	IN <- Start{}
	for {
		(<-IN).pure().io()
	}
}

type In interface {
	pure() Out
}

type Out interface {
	io()
}

type Start struct{}

func (Start) pure() Out {
	return sequence(
		SetDbNotSetup{},
		MakeDbHandle{},
		InitCacher{},
		MakeTcpListener{})
}

type Panic string

func (msg Panic) io() {
	panic(msg)
}

type MakeTcpListener struct{}

func (MakeTcpListener) io() {
	listener, err := net.Listen("tcp", "3001")
	IN <- NewTcpListener{listener, err}
}

type NewTcpListener struct {
	listener net.Listener
	err error
}

func (t NewTcpListener) pure() Out {
	if t.err != nil {
		return Panic("couldn't start TCP server: " + t.err.Error())
	}

	return sequence(CacheListener{t.listener}, GetConn{t.listener})
}

type GetConn struct{
	listener net.Listener
}

func (listener GetConn) io() {
	conn, err := listener.listener.Accept()
	IN <- TcpConn{conn, err}
}

type TcpConn struct {
	conn net.Conn
	err error
}

func (conn TcpConn) pure() Out {
	return sequence(
		Go{ReadConn{conn.conn, Xk1Size, ReadXk1Context}},
		GetListener{})
}

type Go struct {
	out Out
}

func (g Go) io() {
	GO <- g.out.io
}

type ReadConn struct {
	conn net.Conn
	size int
	context int
}

func (r ReadConn) io() {
	buf := make([]byte, r.size)
	n, err := r.conn.Read(buf)
	return ConnMsg{n, err, buf, r.context, r.conn}
}

type ConnMsg struct {
	n int
	err error
	buf []byte
	context int
	conn net.Conn
}

func (m ConnMsg) pure() Out {
	if m.context == ReadXk1Context {
		return handleXk1(m)
	}
}

func handleXk1(m ConnMsg) Out {
	if m.err != nil {
		return CloseCloser{m.conn}
	}

	if m.n != Xk1Size {
		return CloseCloser{m.conn}
	}

	return sequence(
		SetXk1(Xk1AwaitingNoise{m.conn, m.buf}),
		GetStaticKeysAndXk1{})
}

type GetStaticKeysAndXk1 struct{}

func (GetStaticKeysAndXk1) io() {
	GET <- func(c *Cache) In {
		return StaticKeys(c.staticKeys)
	}
}

type StaticKeys noise.DHKey

func (s StaticKeys) pure() Out {
	
}

type SetXk1 Xk1AwaitingNoise

type Xk1AwaitingNoise struct {
	conn net.Conn
	xk1 []byte
}

func (s SetXk1) io() {
	SET <- func(c *Cache) {
		c.xk1Waiting = append(
			c.xk1Waiting,
			Xk1AwaitingNoise(s))
	}
}

func noiseConfig(staticKeys noise.DHKey) noise.Config {
	return noise.Config{
		CipherSuite: noise.NewCipherSuite(
			noise.DH25519,
			noise.CipherChaChaPoly,
			noise.HashBLAKE2s),
		Random: rand.Reader,
		Pattern: noise.HandshakeXK,
		Initiator: false,
		StaticKeypair: staticKeys,
	}

}

type NewHandshakeState struct {
	shake *noise.HandshakeState
	err error
}

type CloseCloser struct {
	closer io.Closer
}

func (c CloseCloser) io() {
	c.closer.Close()
}

type GetListener struct{}

var GET = make(chan func(*Cache) In)
var SET = make(chan func(*Cache))

func (GetListener) io() {
	GET <- func(c *Cache) In {return Listener{c.listener}}
}

type Listener struct {
	listener net.Listener
}

type Cache struct {
	listener net.Listener
	staticKeys noise.DHKey
	db *sql.DB
	dbSetup bool
	xk1Waiting []Xk1AwaitingNoise
}

func (listener Listener) pure() Out {
	return GetConn{listener.listener}
}

type Sequence []Out

func (s Sequence) io() {
	for _, out := range []Out(s) {
		out.io()
	}
}

type CacheListener struct {
	listener net.Listener
}

func (listener CacheListener) io() {
	SET <- func(c *Cache) {c.listener = listener.listener}
}

type InitCacher struct{}

func (InitCacher) io() {
	GO <- cacher
}

func cacher() {
	var cache Cache
	for {
		select {
		case lookup := <-GET:
			IN <- lookup(&cache)
		case set := <-SET:
			set(&cache)
		}
	}
}

const (
	topPath = "bigwebthing"
	dbPath = topPath + "/database.sqlite"
)

type MakeDbHandle struct{}

func (MakeDbHandle) io() {
	db, err := sql.Open("sqlite3", dbPath)
	IN <- Db{db, err}
}

type Db struct {
	db *sql.DB
	err error
}

func (db Db) pure() Out {
	if db.err != nil {
		return Panic("couldn't get DB handle: " + db.err.Error())
	}

	return sequence(SetDb{db.db}, GetDbAndSetup{})
}

type SetDb struct {
	db *sql.DB
}

func (db SetDb) io() {
	SET <- func(c *Cache) {c.db = db.db}
}

type SetDbNotSetup struct{}

func (SetDbNotSetup) io() {
	SET <- func(c *Cache) {c.dbSetup = false}
}

type GetDbAndSetup struct{}

func (GetDbAndSetup) io() {
	GET <- func(c *Cache) In {
		return DbAndStatus{
			db: c.db,
			setup: c.dbSetup,
		}
	}
}

type DbAndStatus struct {
	db *sql.DB
	setup bool
}

func (d DbAndStatus) pure() Out {
	if !d.setup {
		return sequence(setupDb(d.db), afterDbSetup(d.db))
	}

	return afterDbSetup(d.db)
}

func afterDbSetup(db *sql.DB) Out {
	return sequence(CloseCloser{db}, MakeTcpListener{})
}

const makeContactsTableSql = `
CREATE TABLE IF NOT EXISTS contacts (
	contacter BLOB NOT NULL,
	contactee BLOB NOT NULL,
	PRIMARY KEY (contacter, contactee));
`

const makeKk1sTableSql = `
CREATE TABLE IF NOT EXISTS kk1s (
	sessionid BLOB NOT NULL PRIMARY KEY,
	kk12ndhalf BLOB NOT NULL UNIQUE,
	sender BLOB NOT NULL,
	recipient BLOB NOT NULL);
`

const makeKk2sTableSql = `
CREATE TABLE IF NOT EXISTS kk2s (
	sessionid BLOB NOT NULL PRIMARY KEY,
	kk2 BLOB NOT NULL UNIQUE,
	sender BLOB NOT NULL,
	recipient BLOB NOT NULL);
`

const makeKkTransportTableSql = `
CREATE TABLE IF NOT EXISTS kktransports (
	sessionid BLOB NOT NULL PRIMARY KEY,
	kktransport BLOB NOT NULL UNIQUE,
	sender BLOB NOT NULL,
	recipient BLOB NOT NULL,
	timestamp INTEGER NOT NULL);
`

const makePaymentsTableSql = `
CREATE TABLE IF NOT EXISTS payments (
	id INTEGER NOT NULL PRIMARY KEY,
	amount INTEGER NOT NULL,
	timestamp INTEGER NOT NULL,
	payer BLOB NOT NULL);
`

const makeBlobUploadsTableSql = `
CREATE TABLE IF NOT EXISTS blobuploads (
	timestamp INTEGER NOT NULL,
	blobid BLOB NOT NULL PRIMARY KEY,
	uploader BLOB NOT NULL);
`

func setupDb(db *sql.DB) Out {
	x := func(sql string) Out {
		return DbExec_{db, sql}
	}
	return sequence(
		x(makeContactsTableSql),
		x(makeKk1sTableSql),
		x(makeKk2sTableSql),
		x(makeKkTransportTableSql),
		x(makePaymentsTableSql),
		x(makeBlobUploadsTableSql))
}

func sequence(outs ...Out) Out {
	return Sequence(outs)
}

type DbExec_ struct {
	db *sql.DB
	sql string
}

func (x DbExec_) io() {
	_, err := x.db.Exec(x.sql)
	IN <- DbExec_Result{err, x.sql}
}

type DbExec_Result struct {
	err error
	sql string
}

func (err DbExec_Result) pure() Out {
	if err.err != nil {
		return Panic(fmt.Sprintf("couldn't execute SQL: %s: %s", err.sql, err.err))
	}
	return DoNothing{}
}

type DoNothing struct{}

func (DoNothing) io() {
}

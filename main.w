\documentclass{article}
\usepackage{listings}
\usepackage[hidelinks]{hyperref}
\usepackage{microtype}
\usepackage{amsmath}

\title{BigWebThing}
\author{True Ghiassi}
\begin{document}
\maketitle
\tableofcontents

BigWebThing is a social network for sharing apps. This document contains a user specification and a technical specification.

The user specification just defines what is essential from a user point of view, but doesn't bother about technical issues that the user doesn't care or know about. With regard to performance, this section just assumes that the hardware is so good that performance issues do not exist.

The technical specification is about how to implement the user specification, and is greatly concerned with performance issues of all kinds, such as memory, disk and network usage.

\section{User specification}
It should be possible for a very large number of people to use the system.
\subsection{Hardware support}
The system should work on:
\begin{itemize}
\item Android phones and tablets
\item Apple phones and tablets
\item Apple desktops and laptops
\item Windows desktops and laptops
\item Linux desktops
\item Linux servers
\end{itemize}
\subsection{Mutable data}
The data stored in the system is specified here in relational form, that is as sets of unique tuples.

\subsubsection{File author}
\begin{itemize}
\item the file (bytes)
\item author ID (uint64)
\item creation timestamp (uint32)
\end{itemize}

\subsubsection{Permission for user to view a file}
\begin{itemize}
\item the file (bytes)
\item user ID (uint64)
\item timestamp (uint32)
\end{itemize}

\subsubsection{App}
\begin{itemize}
\item the app file (bytes)
\end{itemize}

\subsubsection{Permission for app to view a file}
\begin{itemize}
\item the file (bytes)
\item the app (bytes)
\item timestamp (uint32)
\end{itemize}

\subsubsection{Alias}
This is an alias for the user ID, chosen by the user.
\begin{itemize}
\item ID of the aliased user (uint64)
\item ID of the user creating the alias (uint64)
\item alias (UTF-8 string)
\item timestamp (uint32)
\end{itemize}

\subsection{Static data}
This data is constant and built into the system.

\begin{itemize}
\item files must be smaller than or equal to 10GB
\item users are not able to view files unless they have permission
\item apps are not able to access files unless they have permission
\item apps are not able to edit the data
\end{itemize}

\section{Technical specification}
\subsection{Public data on single server}
\subsubsection{User}
\begin{itemize}
\item short user ID (uint64)
\item user's public static Noise key
\item created timestamp (uint32)
\end{itemize}

@d serverCreateUsersTable @{
const createUsersTable = `
CREATE TABLE IF NOT EXISTS users (
	userid INTEGER NOT NULL PRIMARY KEY,
	publickey BLOB NOT NULL UNIQUE,
	timestamp INTEGER NOT NULL
);
`
@}

\subsubsection{Bad user}
These are users who shouldn't be contacted, probably because their account is compromised and their secret key has leaked.
\begin{itemize}
\item user's public static Noise key
\end{itemize}

@d serverCreateBadUsersTable @{
const createBadUsersTable = `
CREATE TABLE IF NOT EXISTS badusers (
	publickey BLOB NOT NULL PRIMARY KEY,
);
`
@}

\subsection{Public data on each user's server}
\subsubsection{Unread message}
A message that has not yet been downloaded by the recipient.
\begin{itemize}
\item send ID
\item sender public static Noise key (bytes)
\item recipient public static Noise key (bytes)
\item message (bytes)
\item timestamp when sent (uint32)
\end{itemize}
@d serverCreateUnreadTable @{
const createUnreadTable = `
CREATE TABLE IF NOT EXISTS unread (
	sendid INTEGER NOT NULL PRIMARY KEY,
	sender BLOB NOT NULL,
	recipient BLOB NOT NULL,
	message BLOB NOT NULL,
	timestamp INTEGER NOT NULL
);`
@}
\subsubsection{Read message}
A message that has been downloaded by the recipient. The body is thrown away, but it's size and metadata are kept. This is useful data for working out the sender's usage, and for making the social network more interesting. Users can see who sent messages, and who received them.
\begin{itemize}
\item sender public static Noise key (bytes)
\item recipient public static Noise key (bytes)
\item message size (uint32)
\item timestamp when sent (uint32)
\item timestamp when read (uint32)
\end{itemize}
@d serverCreateReadTable @{
const createReadTable = `
CREATE TABLE IF NOT EXISTS read (
	sendid INTEGER NOT NULL PRIMARY KEY,
	sender BLOB NOT NULL,
	recipient BLOB NOT NULL,
	size INTEGER NOT NULL,
	timesent INTEGER NOT NULL,
	timereceived INTEGER NOT NULL
);`
@}

\subsubsection{Payment}
A payment to me.
\begin{itemize}
\item payment ID (uint64)
\item payer public static Noise key (bytes)
\item amount in GBP (uint32)
\item timestamp (uint32)
\end{itemize}
@d serverCreatePaymentTable @{
const createPaymentTable = `
CREATE TABLE IF NOT EXISTS payment (
	paymentid INTEGER NOT NULL PRIMARY KEY,
	payer BLOB NOT NULL,
	amount INTEGER NOT NULL,
	timestamp INTEGER NOT NULL
);
`
@}

\subsubsection{Contact}
\begin{itemize}
\item contacter public static Noise key (bytes)
\item contactee public static Noise key (bytes)
\end{itemize}
@d serverCreateContactTable @{
const createContactTable = `
CREATE TABLE IF NOT EXISTS contact (
	contacter BLOB NOT NULL,
	contactee BLOB NOT NULL,
	PRIMARY KEY (contacter, contactee)
);
`
@}

\subsection{Data on each user's computer}

\subsubsection{Static Noise key pair}
\begin{itemize}
\item secret key (bytes)
\item public key (bytes)
\end{itemize}

\subsection{Public root sigining key for BigWebThing}
This is the public key that is used to create server certificates. It is embedded in the program.

\subsubsection{Noise session seed}
\begin{itemize}
\item session ID (bytes)
\item seed (bytes)
\end{itemize}

\subsubsection{Noise KK1 received}
\begin{itemize}
\item session ID (bytes)
\item KK1 (bytes)
\item sender ID (uint64)
\end{itemize}

\subsubsection{Noise KK2 received}
\begin{itemize}
\item session ID (bytes)
\item KK2 (bytes)
\item sender ID (uint64)
\end{itemize}

\subsubsection{File}
\begin{itemize}
\item file hash (bytes)
\item file (bytes)
\end{itemize}

\subsubsection{App}
\begin{itemize}
\item file hash
\end{itemize}

\subsubsection{File author}
\begin{itemize}
\item file hash (bytes)
\item author public static Noise key (bytes)
\end{itemize}

\subsubsection{Permission for app to view a file}
\begin{itemize}
\item app file hash (bytes)
\item file hash (bytes)
\end{itemize}

\subsubsection{Send file}
\begin{itemize}
\item sending ID (uint32)
\item timestamp (uint32)
\item file hash (bytes)
\item recipient public static Noise key (bytes)
\end{itemize}

\subsubsection{Error sending file}
\begin{itemize}
\item sending ID (uint32)
\item UTF-8 error message (string)
\end{itemize}

\subsubsection{Contact}
\begin{itemize}
\item their public static Noise key (bytes)
\item a friendly name
\end{itemize}

\section{Build}
To build everything, run the bash script at \texttt{generated/build.sh}:

@o generated/build.sh @{
set -e

pushd server
bash build.sh
popd
@}

\section{Server build}
The Go module file specifies the Go dependencies. I have tried to keep them to a minimum, but do need the SQLITE3 driver. I chose SQLITE because it will be robust enough to start with, and I can use a more heavy-duty DB if there gets to be a lot of users. The downside of SQLITE3 is that it does use \texttt{cgo}, so I will run into issues when cross-compiling.

@o generated/server/go.mod @{
module server

go 1.16

require (
	github.com/flynn/noise v1.0.0 // indirect
	github.com/mattn/go-sqlite3 v1.14.8 // indirect
)
@}
(I don't know why it says \texttt{// indirect}. Is this a comment? If so, why? This is a direct dependency, that is, is explicitly imported in the Go code, not a dependency of a dependency.)

And this file contains the checksums of the dependencies:

@o generated/server/go.sum @{
github.com/flynn/noise v1.0.0 h1:DlTHqmzmvcEiKj+4RYo/imoswx/4r6iBlCMfVtrMXpQ=
github.com/flynn/noise v1.0.0/go.mod h1:xbMo+0i6+IGbYdJhF31t2eR1BIU0CYc12+BNAKwUTag=
github.com/kr/pretty v0.2.1/go.mod h1:ipq/a2n7PKx3OHsz4KJII5eveXtPO4qwEXGdVfWzfnI=
github.com/kr/pty v1.1.1/go.mod h1:pFQYn66WHrOpPYNljwOMqo10TkYh1fy3cYio2l3bCsQ=
github.com/kr/text v0.1.0/go.mod h1:4Jbv+DJW3UT/LiOwJeYQe1efqtUx/iVham/4vfdArNI=
github.com/mattn/go-sqlite3 v1.14.8 h1:gDp86IdQsN/xWjIEmr9MF6o9mpksUgh0fu+9ByFxzIU=
github.com/mattn/go-sqlite3 v1.14.8/go.mod h1:NyWgC/yNuGj7Q9rpYnZvas74GogHl5/Z4A/KQRfk6bU=
golang.org/x/crypto v0.0.0-20210322153248-0c34fe9e7dc2 h1:It14KIkyBFYkHkwZ7k45minvA9aorojkyjGk9KJ5B/w=
golang.org/x/crypto v0.0.0-20210322153248-0c34fe9e7dc2/go.mod h1:T9bdIzuCu7OtxOm1hfPfRQxPLYneinmdGuTeoZ9dtd4=
golang.org/x/net v0.0.0-20210226172049-e18ecbb05110/go.mod h1:m0MpNAwzfU5UDzcl9v0D8zg8gWTRqZa9RBIspLL5mdg=
golang.org/x/sys v0.0.0-20201119102817-f84b799fce68 h1:nxC68pudNYkKU6jWhgrqdreuFiOQWj1Fs7T3VrH4Pjw=
golang.org/x/sys v0.0.0-20201119102817-f84b799fce68/go.mod h1:h1NjWce9XRLGQEsW7wpKNCjG9DtNlClVuFLEZdDNbEs=
golang.org/x/term v0.0.0-20201126162022-7de9c90e9dd1/go.mod h1:bj7SfCRtBDWHUb9snDiAeCFNEtKQo2Wmx5Cou7ajbmo=
golang.org/x/text v0.3.3/go.mod h1:5Zoc/QRtKVWzQhOtBMvqHzDpF6irO9z98xDceosuGiQ=
golang.org/x/tools v0.0.0-20180917221912-90fa682c2a6e/go.mod h1:n7NCudcB/nEzxVGmLbDWY5pfWTLqBcC2KZ6jyYvM4mQ=
gopkg.in/check.v1 v1.0.0-20201130134442-10cb98267c6c/go.mod h1:JHkPIbrfpd72SG/EVd6muEfDQjcINNoR0C8j2r3qZ4Q=
@}

(I don't know why there are two entries for \texttt{go-sqlite3}. This was generated when I ran \texttt{go get github.com/mattn/go-sqlite3}.)

The script for building the server is as follows. \texttt{golangci-lint} is a runner for lots of linters. The \texttt{\&\&} is to tell it to abort if \texttt{go install} fails.

@o generated/server/build.sh @{
go install &&
golangci-lint run
@}

\section{Server code}

The idea with the structure the server is that as much as possible of the logic is contained in pure functions, that is, functions that have no side effects, and always give the same output for the same input. Anything put into the channel \texttt{goCh} gets run in a separate goroutine.

The interface \texttt{I} represents inputs from doing IO, and \texttt{O} represents outputs, that is, commands to do some IO. All inputs must have a pure function as a method that calculates an output from the input. All outputs must have a method that executes the IO. If the IO produces any input, it should put it into the \texttt{in} channel.

The purpose of the \texttt{goCh} channel is so that output execution functions can start off goroutines and quit, without the goroutine ending. Functions sent to the \texttt{goCh} are run in a separate goroutine until they are finished.

Ideally, output execution functions should be so short that they are obviously correct. All the logic should be in pure functions so that it can be easily tested.

@d serverMain @{
var goCh = make(chan func())

func main() {
	goCh <- func() {
		in <- Start{}
		for {
			(<-in).pure().io()
		}
	}
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
@}

@o generated/server/server.go @{
package main

import (
	"database/sql"
	_ "github.com/mattn/go-sqlite3"
	"os"
	"github.com/flynn/noise"
	"fmt"
)

// MAIN
@<serverMain@>
// INPUTS
@<serverInputs@>
// OUTPUTS
@<serverOutputs@>
// IO
@<serverIO@>
// PURE
@<serverPure@>
@}

\subsection{Inputs}

@d serverInputs @{
type Start struct{}

type NewDbHandle struct {
	db *sql.DB
	err error
}

type DbExecErr struct {
	err error
	sql_ string
}

type FileContents struct {
	path string
	contents []byte
	err error
}
@}

\subsection{Outputs}

@d serverOutputs @{
type GetDbHandle struct{}

type InitCache struct{}

type SaveDbHandle struct {
	db *sql.DB
}

type Os []O

type Panic string

type DbExecPlain struct {
	db *sql.DB
	sql_ string
}

type DoNothing struct {}

type ReadFile string

type CacheCertificate []byte

type CacheStaticKeys noise.DHKey
@}

\subsection{IO}

@d serverIO @{
@<serverGetDbHandle@>
@<serverSetUpCache@>
@<serverSaveDbHandleCache@>
@<serverManyOuts@>
@<serverDbExec@>
@<serverDoNothing@>
@<serverPanic@>
@<serverSaveDbHandle@>
@<serverReadFile@>
@<serverCacheCertificate@>
@<serverCacheStaticKeys@>
@}

@d serverCacheStaticKeys @{
func (c CacheStaticKeys) io() {
	toCache <- c
}

func (c CacheStaticKeys) update(cache *Cache) {
	cache.staticKeys = noise.DHKey(c)
}
@}

@d serverCacheCertificate @{
func (c CacheCertificate) io() {
	toCache <- c
}

func (c CacheCertificate) update(cache *Cache) {
	cache.certificate = c
}
@}

@d serverReadFile @{
func (r ReadFile) io() {
	contents, err := os.ReadFile(string(r))
	in <- FileContents{string(r), contents, err}
}
@}

@d serverGetDbHandle @{
func (GetDbHandle) io() {
	db, err := sql.Open("sqlite3", "database.sqlite")
	in <- NewDbHandle{db, err}
}
@}

@d serverSetUpCache @{
var toCache = make(chan ToCache)

type Cache struct {
	db []*sql.DB
	staticKeys noise.DHKey
	certificate []byte
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
@}

@d serverSaveDbHandleCache @{
func (s SaveDbHandle) update(cache *Cache) {
	cache.db = []*sql.DB{s.db}
}
@}

@d serverManyOuts @{
func (os_ Os) io() {
	for _, o := range os_ {
		o.io()
	}
}
@}

@d serverDbExec @{
func (d DbExecPlain) io() {
	_, err := d.db.Exec(d.sql_)
	in <- DbExecErr{err, d.sql_}
}
@}

@d serverDoNothing @{
func (DoNothing) io() {
}
@}

@d serverPanic @{
func (p Panic) io() {
	panic(p)
}
@}

@d serverSaveDbHandle @{
func (s SaveDbHandle) io() {
	toCache <- s
}
@}

\subsection{Pure}

@d serverPure @{
@<serverOnStart@>
@<serverSetUpDb@>
@<serverHandleDbError@>
@<serverOnFileContents@>
@}

@d serverOnStart @{

const keysPath = "secretKeys"

const certificatePath = "certificate"

func (Start) pure() O {
	return Os([]O{
		GetDbHandle{},
		ReadFile(certificatePath),
		ReadFile(keysPath)})
}
@}

@d serverOnFileContents @{
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
			Public: f.contents[dhlen:2*dhlen],
		})

	case certificatePath:
		return CacheCertificate(f.contents)
	}

	return Panic("unexpected file contents: %s" + f.path)
}
@}

@d serverSetUpDb @{
@<serverCreateUsersTable@>
@<serverCreateBadUsersTable@>
@<serverCreateUnreadTable@>
@<serverCreateReadTable@>
@<serverCreatePaymentTable@>
@<serverCreateContactTable@>

func (n NewDbHandle) pure() O {
	if n.err != nil {
		return Panic(
			"couldn't set up database: " + n.err.Error())
	}

	makeTable := func(makeSql string) O {
		return DbExecPlain{n.db, makeSql}
	}

	return Os([]O{
		makeTable(createUsersTable),
		makeTable(createBadUsersTable),
		makeTable(createUnreadTable),
		makeTable(createReadTable),
		makeTable(createPaymentTable),
		makeTable(createContactTable),
		InitCache{},
		SaveDbHandle{n.db},
	})
}
@}

@d serverHandleDbError @{
func (d DbExecErr) pure() O {
	if d.err != nil {
		return Panic(
			"could not execute SQL: " +
			d.sql_ +
			d.err.Error())
	}

	return DoNothing{}
}
@}

\end{document}

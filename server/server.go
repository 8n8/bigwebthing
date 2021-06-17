package main

import (
	"crypto/rand"
	"database/sql"
	"encoding/base64"
	"errors"
	"fmt"
	"github.com/flynn/noise"
	_ "github.com/mattn/go-sqlite3"
	"io/ioutil"
	"net"
	"os"
	"sort"
	"time"
)

func main() {
	keys := getStaticKeys()
	db := setupDb()
	defer db.Close()

	listener, err := net.Listen("tcp", ":4040")
	if err != nil {
		panic(err)
	}

	newSessions := make(chan Session)
	go setupSessions(listener, keys, newSessions)

	toSends := make(chan ToSend)
	go sendOutMessages(toSends)

	rawMessages := make(chan RawMessage)
	sessions := make(map[[dhlen]byte]Session)
	for {
		select {
		case s := <-newSessions:
			sessions[s.theirId] = s
			go listenToConn(s.theirId, s.conn, rawMessages)
			err := sendThemAllTheirStuff(s, db, toSends)
			if err != nil {
				s.conn.Close()
				delete(sessions, s.theirId)
			}

		case raw := <-rawMessages:
			respond(raw, sessions, db, toSends)
		}
	}
}

func sendThemAllTheirStuff(s Session, db *sql.DB, toSends chan ToSend) error {

	err := sendThemAllTheirKk1s(s, db, toSends)
	if err != nil {
		return err
	}

	err = sendThemAllTheirKk2s(s, db, toSends)
	if err != nil {
		return err
	}

	err = sendThemAllTheirKkTransports(s, db, toSends)
	if err != nil {
		return err
	}

	err = sendThemAllTheirAddContacts(s, db, toSends)
	if err != nil {
		return err
	}

	err = sendThemAllTheirRemoveContacts(s, db, toSends)
	if err != nil {
		return err
	}

	return sendThemAllTheirPayments(s, db, toSends)
}

const getAllTheirKk1sSql = `
SELECT (timestamp, recipient, sender, encrypted)
FROM kk1
WHERE recipient=?
OR sender=?;
`

func sendThemAllTheirKk1s(
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(getAllTheirKk1sSql, s.theirId, s.theirId)
	if err != nil {
		panic(err)
	}

	for rows.Next() {
		var k Kk1
		err = rows.Scan(
			&k.timestamp,
			&k.recipient,
			&k.sender,
			&k.encrypted)
		if err != nil {
			panic(err)
		}

		ciphertext, err := encrypt(encodeKk1(k), s)
		if err != nil {
			return err
		}

		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

const getAllTheirKk2sSql = `
SELECT (timestamp, recipient, sender, encrypted)
FROM kk2
WHERE recipient=?
OR sender=?;
`

func sendThemAllTheirKk2s(
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(getAllTheirKk2sSql, s.theirId, s.theirId)
	if err != nil {
		panic(err)
	}

	for rows.Next() {
		var k Kk2
		err = rows.Scan(
			&k.timestamp,
			&k.recipient,
			&k.sender,
			&k.sessionId,
			&k.encrypted)
		if err != nil {
			panic(err)
		}

		ciphertext, err := encrypt(encodeKk2(k), s)
		if err != nil {
			return err
		}

		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

const getAllTheirKkTransports = `
SELECT (timestamp, recipient, sender, sessionid, blobid, encrypted)
FROM kktransport
WHERE recipient=?
OR sender=?;
`

func sendThemAllTheirKkTransports(
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(getAllTheirKkTransports, s.theirId, s.theirId)
	if err != nil {
		panic(err)
	}

	for rows.Next() {
		var k KkTransport
		err = rows.Scan(
			&k.timestamp,
			&k.recipient,
			&k.sender,
			&k.sessionId,
			&k.blobId,
			&k.encrypted)
		if err != nil {
			panic(err)
		}

		ciphertext, err := encrypt(encodeKkTransport(k), s)
		if err != nil {
			return err
		}

		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

const getAllTheirPaymentsSql = `
SELECT (timestamp, amount)
FROM payment
WHERE payer=?;
`

func sendThemAllTheirPayments(
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(getAllTheirPaymentsSql, s.theirId)
	if err != nil {
		panic(err)
	}

	for rows.Next() {
		var p Payment
		p.payer = s.theirId
		err = rows.Scan(&p.timestamp, &p.amount)
		if err != nil {
			panic(err)
		}

		ciphertext, err := encrypt(encodePayment(p), s)
		if err != nil {
			return err
		}

		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

const getAllTheirRemoveContactSql = `
SELECT (timestamp, contacter)
FROM removecontact
WHERE contactee=?;
`

func sendThemAllTheirRemoveContacts(
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(getAllTheirRemoveContactSql, s.theirId)
	if err != nil {
		panic(err)
	}

	for rows.Next() {
		var r RemoveContact
		r.contactee = s.theirId
		err = rows.Scan(&r.timestamp, &r.contacter)
		if err != nil {
			panic(err)
		}

		ciphertext, err := encrypt(encodeRemoveContact(r), s)
		if err != nil {
			return err
		}

		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

const getAllTheirAddContactSql = `
SELECT (timestamp, contacter)
FROM addcontact
WHERE contactee=?;
`

func sendThemAllTheirAddContacts(
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(getAllTheirAddContactSql, s.theirId)
	if err != nil {
		panic(err)
	}

	for rows.Next() {
		var a AddContact
		a.contactee = s.theirId
		err = rows.Scan(&a.timestamp, &a.contacter)
		if err != nil {
			panic(err)
		}

		ciphertext, err := encrypt(encodeAddContact(a), s)
		if err != nil {
			return err
		}

		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

func sendOutMessages(toSends chan ToSend) {
	for {
		s := <-toSends
		go func() {
			deadline := time.Now().Add(tcpDeadline)
			err := s.conn.SetDeadline(deadline)
			if err != nil {
				return
			}
			_, err = s.conn.Write(s.message)
			if err != nil {
				return
			}
		}()
	}
}

var ad = []byte{
	248, 0, 56, 173, 54, 51, 61, 107, 170, 15, 53, 205, 36, 4, 67, 156, 186, 159, 44, 196, 114, 221, 191, 251, 143, 167, 73, 252, 139, 144, 60, 66,
}

func respond(
	raw RawMessage,
	sessions map[[32]byte]Session,
	db *sql.DB,
	toSends chan ToSend) {

	session, ok := sessions[raw.theirId]
	if !ok {
		panic(fmt.Sprintf("there should be a session for %v", raw.theirId))
	}

	plain, err := session.receive.Decrypt([]byte{}, ad, raw.raw)
	if err != nil {
		session.conn.Close()
		delete(sessions, raw.theirId)
		return
	}

	parsed, ok := parseMessage(plain)
	if !ok {
		session.conn.Close()
		delete(sessions, raw.theirId)
		return
	}

	parsed.respond(raw.theirId, sessions, db, toSends)
}

func (k Kk2) respond(
	theirId [idlen]byte,
	sessions map[[idlen]byte]Session,
	db *sql.DB,
	toSends chan ToSend) {

	k.sender = theirId
	k.timestamp = posixNow()

	s, ok := sessions[theirId]
	if !ok {
		return
	}

	if getBalance(db, theirId) < price {
		s.conn.Close()
		delete(sessions, theirId)
		return
	}

	err := cacheKk2(k, db)
	if err != nil {
		s.conn.Close()
		delete(sessions, theirId)
		return
	}

	if !inContacts(k.sender, k.recipient, db) {
		return
	}

	rs, ok := sessions[k.recipient]
	if !ok {
		return
	}

	ciphertext, err := encrypt(encodeKk2(k), rs)
	if err != nil {
		rs.conn.Close()
		delete(sessions, k.recipient)
		return
	}

	toSends <- ToSend{conn: rs.conn, message: ciphertext}
}

func encodeUint32(u uint32) []byte {
	encoded := make([]byte, 4)

	encoded[0] = byte(u & 0xFF)
	encoded[1] = byte((u >> 8) & 0xFF)
	encoded[2] = byte((u >> 16) & 0xFF)
	encoded[3] = byte((u >> 24) & 0xFF)

	return encoded
}

func encodeKk1(k Kk1) []byte {
	encoded := make([]byte, kk1Len)
	encoded[0] = kk1Indicator

	copy(encoded[1:], encodeUint32(k.timestamp))
	copy(encoded[1+4:], k.recipient[:])
	copy(encoded[1+4+32:], k.sender[:])
	copy(encoded[1+4+32+32:], k.encrypted[:])

	return encoded
}

func encodeKk2(k Kk2) []byte {
	encoded := make([]byte, kk2Len)
	encoded[0] = kk2Indicator

	copy(encoded[1:], encodeUint32(k.timestamp))
	copy(encoded[1+4:], k.recipient[:])
	copy(encoded[1+4+32:], k.sender[:])
	copy(encoded[1+4+32+32:], k.encrypted[:])

	return encoded
}

const contactRemovedSql = `
SELECT timestamp
FROM addcontact
WHERE contacter=?;
AND contactee=?;
`

const contactAddedSql = `
SELECT timestamp
FROM removecontact
WHERE contacter=?
AND contactee=?;
`

func inContacts(sender, recipient [idlen]byte, db *sql.DB) bool {
	rows, err := db.Query(contactAddedSql, sender, recipient)
	if err != nil {
		panic(err)
	}
	addedTimes := []uint32{}
	for rows.Next() {
		var t uint32
		err = rows.Scan(t)
		if err != nil {
			panic(err)
		}
		addedTimes = append(addedTimes, t)
	}
	if len(addedTimes) == 0 {
		return false
	}
	sort.Slice(addedTimes, func(i, j int) bool {
		return addedTimes[i] > addedTimes[j]
	})
	latestAdded := addedTimes[0]

	rows, err = db.Query(contactRemovedSql, sender, recipient)
	if err != nil {
		panic(err)
	}
	removedTimes := []uint32{}
	for rows.Next() {
		var t uint32
		err = rows.Scan(t)
		if err != nil {
			panic(err)
		}
		removedTimes = append(removedTimes, t)
	}
	if len(removedTimes) == 0 {
		return true
	}
	sort.Slice(removedTimes, func(i, j int) bool {
		return removedTimes[i] > removedTimes[j]
	})
	latestRemoved := removedTimes[0]

	return latestAdded > latestRemoved
}

const cacheKk2Sql = `
INSERT INTO kk2 (timestamp, recipient, sender, sessionid, encrypted) 
VALUES (?, ?, ?, ?, ?);
`

func cacheKk2(k Kk2, db *sql.DB) error {
	_, err := db.Exec(
		cacheKk2Sql,
		k.timestamp,
		k.recipient,
		k.sender,
		k.sessionId,
		k.encrypted)
	return err
}

type Parsed interface {
	respond([idlen]byte, map[[idlen]byte]Session, *sql.DB, chan ToSend)
}

func parseMessage(raw []byte) (Parsed, bool) {
	var msg Parsed
	var ok bool

	msg, ok = parseKk1(raw)
	if ok {
		return msg, true
	}

	msg, ok = parseKk2(raw)
	if ok {
		return msg, true
	}

	msg, ok = parseKkTransport(raw)
	if ok {
		return msg, true
	}

	msg, ok = parseBlob(raw)
	if ok {
		return msg, true
	}

	msg, ok = parseAddContact(raw)
	if ok {
		return msg, true
	}

	msg, ok = parseRemoveContact(raw)
	if ok {
		return msg, true
	}

	msg, ok = parsePayment(raw)
	if ok {
		return msg, true
	}

	msg, ok = parseGetDataOf(raw)
	if ok {
		return msg, true
	}

	msg, ok = parseGetBlob(raw)
	if ok {
		return msg, true
	}

	msg, ok = parseGetBlobRequestsOf(raw)
	if ok {
		return msg, true
	}

	return nil, false
}

const insertKkTransportSql = `
INSERT INTO kktransport
(timestamp, recipient, sender, sessionid, blobid, kktransport)
VALUES (?, ?, ?, ?, ?, ?);
`

func cacheKkTransport(k KkTransport, db *sql.DB) error {
	_, err := db.Exec(
		insertKkTransportSql,
		k.timestamp,
		k.recipient,
		k.sender,
		k.sessionId,
		k.blobId,
		k.encrypted)
	return err
}

func encodeKkTransport(k KkTransport) []byte {
	encoded := make([]byte, kkTransportLen)

	encoded[0] = kkTransportIndicator
	copy(encoded[1:], encodeUint32(k.timestamp))
	copy(encoded[1+4:], k.recipient[:])
	copy(encoded[1+4+32:], k.sender[:])
	copy(encoded[1+4+32+32:], k.sessionId[:])
	copy(encoded[1+4+32+32+32:], k.blobId[:])
	copy(encoded[1+4+32+32+32+32:], k.encrypted[:])

	return encoded
}

func (k KkTransport) respond(
	theirId [idlen]byte,
	sessions map[[idlen]byte]Session,
	db *sql.DB,
	toSends chan ToSend) {

	k.sender = theirId
	k.timestamp = posixNow()

	s, ok := sessions[theirId]
	if !ok {
		return
	}

	if getBalance(db, theirId) < price {
		s.conn.Close()
		delete(sessions, theirId)
		return
	}

	err := cacheKkTransport(k, db)
	if err != nil {
		s.conn.Close()
		delete(sessions, theirId)
		return
	}

	if !inContacts(k.sender, k.recipient, db) {
		return
	}

	rs, ok := sessions[k.recipient]
	if !ok {
		return
	}

	ciphertext, err := encrypt(encodeKkTransport(k), rs)
	if err != nil {
		rs.conn.Close()
		delete(sessions, k.recipient)
		return
	}

	toSends <- ToSend{conn: rs.conn, message: ciphertext}
}

var paymentIssuerId = [dhlen]byte{
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
}

const cachePaymentSql = `
INSERT INTO payment (timestamp, amount, payer)
VALUES (?, ?, ?);
`

func (p Payment) respond(
	theirId [idlen]byte,
	sessions map[[idlen]byte]Session,
	db *sql.DB,
	toSends chan ToSend) {

	p.timestamp = posixNow()

	s, ok := sessions[theirId]
	if !ok {
		return
	}

	if theirId != paymentIssuerId {
		s.conn.Close()
		delete(sessions, theirId)
		return
	}

	_, err := db.Exec(
		cachePaymentSql,
		p.timestamp,
		p.amount,
		p.payer)
	if err != nil {
		panic(err)
	}
}

const (
	getBlobRequestsOfLen = 33
	getBlobLen           = 37
	getDataOfLen         = 37
	paymentLen           = 41
	contactLen           = 69
	minBlobLen           = 58
	kkTransportLen       = 181
	kk2Len               = 149
	kk1Len               = 117
	blobStubLen          = 78
	xk1len               = 48
	xk3len               = 64

	blobStubIndicator          = 7
	getBlobIndicator           = 9
	getBlobRequestsOfIndicator = 10
	getDataOfIndicator         = 8
	removeContactIndicator     = 5
	addContactIndicator        = 4
	paymentIndicator           = 6
	blobIndicator              = 3
	kkTransportIndicator       = 2
	kk2Indicator               = 1
	kk1Indicator               = 0

	price = 0.1
)

func (b Blob) respond(
	theirId [idlen]byte,
	sessions map[[idlen]byte]Session,
	db *sql.DB,
	toSends chan ToSend) {

	b.timestamp = posixNow()

	s, ok := sessions[theirId]
	if !ok {
		return
	}

	if getBalance(db, theirId) < price {
		s.conn.Close()
		delete(sessions, theirId)
		return
	}

	if cacheBlob(b, theirId, db) != nil {
		s.conn.Close()
		delete(sessions, theirId)
	}
}

const addBlobSql = `
INSERT INTO blob (timestamp, author, id, finalchunk, counter)
VALUES (?, ?, ?, ?, ?);
`

func makeBlobPath(blobId [idlen]byte, counter uint32) string {
	bs := make([]byte, idlen+4)
	copy(bs, blobId[:])
	copy(bs[idlen:], encodeUint32(counter))
	return base64.URLEncoding.EncodeToString(bs)
}

func cacheBlob(b Blob, theirId [idlen]byte, db *sql.DB) error {
	_, err := db.Exec(
		addBlobSql,
		b.timestamp,
		theirId,
		b.blobId,
		b.finalChunk,
		b.counter)
	if err != nil {
		return err
	}

	path := makeBlobPath(b.blobId, b.counter)
	err = os.WriteFile(path, b.blob, 0400)
	if err != nil {
		panic(err)
	}
	return nil
}

func getBalance(db *sql.DB, theirId [idlen]byte) float64 {
	return getPayments(db, theirId) - getSpending(db, theirId)
}

const getTheirPaymentsSql = `
SELECT amount FROM payment WHERE payer=?;
`

func getPayments(db *sql.DB, theirId [dhlen]byte) float64 {
	rows, err := db.Query(getTheirPaymentsSql, theirId)
	var total uint32 = 0
	for rows.Next() {
		var a uint32
		err = rows.Scan(&a)
		if err != nil {
			panic(err)
		}
		total += a
	}
	return float64(total)
}

const getNumKk1sSql = `
SELECT COUNT(*) FROM kk1 WHERE sender=?;
`

func getNumKk1s(db *sql.DB, theirId [dhlen]byte) int64 {
	rows, err := db.Query(getNumKk1sSql, theirId)
	if err != nil {
		panic(err)
	}
	if !rows.Next() {
		return 0
	}
	var n int64 = 0
	err = rows.Scan(&n)
	if err != nil {
		panic(err)
	}
	return n
}

func getNumKk2s(db *sql.DB, theirId [dhlen]byte) int64 {
	rows, err := db.Query(
		"SELECT COUNT(*) FROM kk2 WHERE sender=?;",
		theirId)
	if err != nil {
		panic(err)
	}
	if !rows.Next() {
		return 0
	}
	var n int64 = 0
	err = rows.Scan(&n)
	if err != nil {
		panic(err)
	}
	return n
}

func getNumKkTransports(db *sql.DB, theirId [dhlen]byte) int64 {
	rows, err := db.Query(
		"SELECT COUNT(*) FROM kktransport WHERE sender=?;",
		theirId)
	if err != nil {
		panic(err)
	}
	if !rows.Next() {
		return 0
	}
	var n int64 = 0
	err = rows.Scan(&n)
	if err != nil {
		panic(err)
	}
	return n
}

func getNumBlobs(db *sql.DB, theirId [dhlen]byte) int64 {
	rows, err := db.Query(
		"SELECT COUNT (*) FROM blob WHERE author=?;",
		theirId)

	if err != nil {
		panic(err)
	}
	if !rows.Next() {
		return 0
	}
	var n int64 = 0
	err = rows.Scan(&n)
	if err != nil {
		panic(err)
	}
	return n
}

func getNumAddContacts(db *sql.DB, theirId [dhlen]byte) int64 {
	rows, err := db.Query(
		"SELECT COUNT (*) FROM addcontact WHERE contactee=?;",
		theirId)
	if err != nil {
		panic(err)
	}
	if !rows.Next() {
		return 0
	}
	var n int64 = 0
	err = rows.Scan(&n)
	if err != nil {
		panic(err)
	}
	return n
}

func getNumRemoveContacts(db *sql.DB, theirId [dhlen]byte) int64 {
	rows, err := db.Query(
		"SELECT COUNT (*) FROM removecontact WHERE contactee=?",
		theirId)
	if err != nil {
		panic(err)
	}
	if !rows.Next() {
		return 0
	}
	var n int64 = 0
	err = rows.Scan(&n)
	if err != nil {
		panic(err)
	}
	return n
}

func getNumGetDataOf(db *sql.DB, theirId [dhlen]byte) int64 {
	rows, err := db.Query(
		"SELECT COUNT (*) FROM getdataof WHERE asker=?;",
		theirId)
	if err != nil {
		panic(err)
	}
	if !rows.Next() {
		return 0
	}
	var n int64 = 0
	err = rows.Scan(&n)
	if err != nil {
		panic(err)
	}
	return n
}

func getNumGetBlob(db *sql.DB, theirId [dhlen]byte) int64 {
	rows, err := db.Query(
		"SELECT COUNT (*) FROM getblob WHERE asker=?;",
		theirId)
	if err != nil {
		panic(err)
	}
	if !rows.Next() {
		return 0
	}
	var n int64 = 0
	err = rows.Scan(&n)
	if err != nil {
		panic(err)
	}
	return n
}

func getSpending(db *sql.DB, theirId [dhlen]byte) float64 {
	ns := []int64{
		getNumKk1s(db, theirId),
		getNumKk2s(db, theirId),
		getNumKkTransports(db, theirId),
		getNumBlobs(db, theirId),
		getNumAddContacts(db, theirId),
		getNumRemoveContacts(db, theirId),
		getNumGetDataOf(db, theirId),
		getNumGetBlob(db, theirId)}

	var total int64 = 0
	for _, n := range ns {
		total += n
	}

	return float64(total) * price
}

func (r RemoveContact) respond(
	theirId [idlen]byte,
	sessions map[[idlen]byte]Session,
	db *sql.DB,
	toSends chan ToSend) {

	r.timestamp = posixNow()
	s, ok := sessions[theirId]
	if !ok {
		return
	}

	r.contactee = theirId

	if getBalance(db, theirId) < price {
		s.conn.Close()
		delete(sessions, theirId)
		return
	}

	cacheRemoveContact(r, db)
}

const cacheRemoveContactSql = `
INSERT INTO removecontact (timestamp, contacter, contactee)
VALUES (?, ?, ?);
`

func cacheRemoveContact(r RemoveContact, db *sql.DB) {
	_, err := db.Exec(
		cacheRemoveContactSql,
		r.timestamp,
		r.contacter,
		r.contactee)
	if err != nil {
		panic(err)
	}
}

func (a AddContact) respond(
	theirId [idlen]byte,
	sessions map[[idlen]byte]Session,
	db *sql.DB,
	toSends chan ToSend) {

	a.timestamp = posixNow()
	s, ok := sessions[theirId]
	if !ok {
		return
	}

	a.contactee = theirId

	if getBalance(db, theirId) < price {
		s.conn.Close()
		delete(sessions, theirId)
		return
	}

	cacheAddContact(a, db)
}

const cacheAddContactSql = `
INSERT INTO addcontact (timestamp, contacter, contactee)
VALUES (?, ?, ?);
`

func cacheAddContact(a AddContact, db *sql.DB) {
	_, err := db.Exec(
		cacheAddContactSql,
		a.timestamp,
		a.contacter,
		a.contactee)
	if err != nil {
		panic(err)
	}
}

func (g GetDataOf) respond(
	theirId [idlen]byte,
	sessions map[[idlen]byte]Session,
	db *sql.DB,
	toSends chan ToSend) {

	g.timestamp = posixNow()

	s, ok := sessions[theirId]
	if !ok {
		return
	}

	if getBalance(db, theirId) < price {
		s.conn.Close()
		delete(sessions, theirId)
		return
	}

	cacheGetDataOf(g, db)

	if sendDataOfTo(g.theirId, s, db, toSends) != nil {
		s.conn.Close()
		delete(sessions, theirId)
	}
}

const selectTheirKk1sSql = `
SELECT (timestamp, recipient, sender, kk1)
FROM kk1
WHERE recipient=?
OR sender=?;
`

const selectTheirKk2sSql = `
SELECT (timestamp, recipient, sender, sessionid, encrypted)
FROM kk2
WHERE recipient=?
OR sender=?;
`

func sendDataOfTo(
	of [idlen]byte,
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	err := sendKk1OfTo(of, s, db, toSends)
	if err != nil {
		return err
	}

	err = sendKk2OfTo(of, s, db, toSends)
	if err != nil {
		return err
	}

	err = sendKkTransportOfTo(of, s, db, toSends)
	if err != nil {
		return err
	}

	err = sendBlobOfTo(of, s, db, toSends)
	if err != nil {
		return err
	}

	err = sendAddContactOfTo(of, s, db, toSends)
	if err != nil {
		return err
	}

	err = sendRemoveContactOfTo(of, s, db, toSends)
	if err != nil {
		return err
	}

	err = sendPaymentsOfTo(of, s, db, toSends)
	if err != nil {
		return err
	}

	return sendGetDataOfOfTo(of, s, db, toSends)
}

const selectTheirBlobsSql = `
SELECT (timestamp, author, id, finalchunk, counter, size)
FROM blob
WHERE author=?;
`

const selectTheirAddContactsSql = `
SELECT (timestamp, contacter, contactee)
FROM addcontact
WHERE contacter=?
OR contactee=?;
`

const selectTheirRemoveContactsSql = `
SELECT (timestamp, contacter, contactee)
FROM removecontact
WHERE contacter=?
OR contactee=?;
`

const selectTheirPaymentsSql = `
SELECT (timestamp, amount, payer)
FROM payment
WHERE payer=?;
`

const selectTheirGetDataOfsSql = `
SELECT (timestamp, asker, theirid)
FROM getdataof
WHERE asker=?
OR theirid=?;
`

func sendGetDataOfOfTo(
	of [idlen]byte,
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(selectTheirGetDataOfsSql, of, of)
	if err != nil {
		panic(err)
	}
	for rows.Next() {
		var g GetDataOf
		err = rows.Scan(&g.timestamp, &g.asker, &g.theirId)
		if err != nil {
			panic(err)
		}
		ciphertext, err := encrypt(encodeGetDataOf(g), s)
		if err != nil {
			return err
		}
		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

func encodeGetDataOf(g GetDataOf) []byte {
	encoded := make([]byte, getDataOfLen)
	encoded[0] = getDataOfIndicator

	copy(encoded[1:], encodeUint32(g.timestamp))
	copy(encoded[1+4:], g.asker[:])
	copy(encoded[1+4+32:], g.theirId[:])

	return encoded
}

func sendPaymentsOfTo(
	of [idlen]byte,
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(selectTheirPaymentsSql, of)
	if err != nil {
		panic(err)
	}
	for rows.Next() {
		var p Payment
		err = rows.Scan(&p.timestamp, &p.amount, &p.payer)
		if err != nil {
			panic(err)
		}
		ciphertext, err := encrypt(encodePayment(p), s)
		if err != nil {
			return err
		}
		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

func encodeBlobStub(b BlobStub) []byte {
	encoded := make([]byte, blobStubLen)

	encoded[0] = blobStubIndicator
	copy(encoded[1:], encodeUint32(b.timestamp))
	copy(encoded[1+4:], b.author[:])
	copy(encoded[1+4+32:], b.blobId[:])
	if b.finalChunk {
		encoded[1+4+32+32] = 1
	} else {
		encoded[1+4+32+32] = 0
	}
	copy(encoded[1+4+32+32+1:], encodeUint32(b.counter))
	copy(encoded[1+4+32+32+1+4:], encodeUint32(b.size))

	return encoded
}

func encodeAddContact(a AddContact) []byte {
	encoded := make([]byte, contactLen)

	encoded[0] = addContactIndicator
	copy(encoded[1:], encodeUint32(a.timestamp))
	copy(encoded[1+4:], a.contacter[:])
	copy(encoded[1+4+32:], a.contactee[:])

	return encoded
}

func encodeRemoveContact(r RemoveContact) []byte {
	encoded := make([]byte, contactLen)

	encoded[0] = removeContactIndicator
	copy(encoded[1:], encodeUint32(r.timestamp))
	copy(encoded[1+4:], r.contacter[:])
	copy(encoded[1+4+32:], r.contactee[:])

	return encoded
}

func encodePayment(p Payment) []byte {
	encoded := make([]byte, paymentLen)

	encoded[0] = paymentIndicator
	copy(encoded[1:], encodeUint32(p.timestamp))
	copy(encoded[1+4:], encodeUint32(p.amount))
	copy(encoded[1+4+4:], p.payer[:])

	return encoded
}

func sendRemoveContactOfTo(
	of [idlen]byte,
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(selectTheirRemoveContactsSql, of, of)
	if err != nil {
		panic(err)
	}
	for rows.Next() {
		var r RemoveContact
		err = rows.Scan(
			&r.timestamp,
			&r.contacter,
			&r.contactee)
		if err != nil {
			panic(err)
		}
		ciphertext, err := encrypt(encodeRemoveContact(r), s)
		if err != nil {
			return err
		}
		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

func sendAddContactOfTo(
	of [idlen]byte,
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(selectTheirAddContactsSql, of, of)
	if err != nil {
		panic(err)
	}
	for rows.Next() {
		var a AddContact
		err = rows.Scan(
			&a.timestamp,
			&a.contacter,
			&a.contactee)
		if err != nil {
			panic(err)
		}
		ciphertext, err := encrypt(encodeAddContact(a), s)
		if err != nil {
			return err
		}
		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

func sendBlobOfTo(
	of [idlen]byte,
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(selectTheirBlobsSql, of)
	if err != nil {
		panic(err)
	}
	for rows.Next() {
		var b BlobStub
		err = rows.Scan(
			&b.timestamp,
			&b.author,
			&b.blobId,
			&b.finalChunk,
			&b.counter,
			&b.size)
		if err != nil {
			panic(err)
		}
		ciphertext, err := encrypt(encodeBlobStub(b), s)
		if err != nil {
			return err
		}

		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

const selectTheirKkTransportsSql = `
SELECT (timestamp, recipient, sender, sessionid, blobid, kktransport)
FROM kktransport
WHERE recipient=?
OR sender=?;
`

func encodeKkTranport(k KkTransport) []byte {
	encoded := make([]byte, kkTransportLen)

	encoded[0] = kkTransportIndicator
	copy(encoded[1:], encodeUint32(k.timestamp))
	copy(encoded[1+4:], k.recipient[:])
	copy(encoded[1+4+32:], k.sender[:])
	copy(encoded[1+4+32+32:], k.sessionId[:])
	copy(encoded[1+4+32+32+32:], k.blobId[:])
	copy(encoded[1+4+32+32+32+32:], k.encrypted[:])

	return encoded
}

func sendKkTransportOfTo(
	of [idlen]byte,
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(selectTheirKkTransportsSql, of, of)
	if err != nil {
		panic(err)
	}
	for rows.Next() {
		var k KkTransport
		err = rows.Scan(
			&k.timestamp,
			&k.recipient,
			&k.sender,
			&k.sessionId,
			&k.blobId,
			&k.encrypted)
		if err != nil {
			panic(err)
		}
		ciphertext, err := encrypt(encodeKkTranport(k), s)
		if err != nil {
			return err
		}
		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

func sendKk1OfTo(
	of [idlen]byte,
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(selectTheirKk1sSql, of, of)
	if err != nil {
		panic(err)
	}
	for rows.Next() {
		var k Kk1
		err = rows.Scan(
			&k.timestamp,
			&k.recipient,
			&k.sender,
			&k.encrypted)
		if err != nil {
			panic(err)
		}
		ciphertext, err := encrypt(encodeKk1(k), s)
		if err != nil {
			return err
		}
		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
	return nil
}

func sendKk2OfTo(
	of [idlen]byte,
	s Session,
	db *sql.DB,
	toSends chan ToSend) error {

	rows, err := db.Query(selectTheirKk2sSql, of, of)
	if err != nil {
		panic(err)
	}
	for rows.Next() {
		var k Kk2
		err = rows.Scan(
			&k.timestamp,
			&k.recipient,
			&k.sender,
			&k.sessionId,
			&k.encrypted)
		if err != nil {
			panic(err)
		}
		ciphertext, err := encrypt(encodeKk2(k), s)
		if err != nil {
			return err
		}
		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}

	return nil
}

const cacheGetDataOfSql = `
INSERT INTO getdataof (timestamp, asker, theirid)
VALUES (?, ?, ?);
`

func cacheGetDataOf(g GetDataOf, db *sql.DB) {
	_, err := db.Exec(
		cacheGetDataOfSql,
		g.timestamp,
		g.asker,
		g.theirId)
	if err != nil {
		panic(err)
	}
}

func posixNow() uint32 {
	return uint32(time.Now().Unix())
}

const getBlobRequestsOfSql = `
SELECT (timestamp, asker, id)
FROM getblob
WHERE asker=?;
`

func encodeGetBlob(g GetBlob) []byte {
	encoded := make([]byte, getBlobLen)

	encoded[0] = getBlobIndicator
	copy(encoded[1:], encodeUint32(g.timestamp))
	copy(encoded[1+4:], g.asker[:])
	copy(encoded[1+4+32:], g.blobId[:])

	return encoded
}

func (g GetBlobRequestsOf) respond(
	theirId [idlen]byte,
	sessions map[[idlen]byte]Session,
	db *sql.DB,
	toSends chan ToSend) {

	s, ok := sessions[theirId]
	if !ok {
		return
	}

	rows, err := db.Query(getBlobRequestsOfSql, g)
	if err != nil {
		panic(err)
	}

	for rows.Next() {
		var gb GetBlob
		err = rows.Scan(&gb.timestamp, &gb.asker)
		gb.blobId = g
		plain := encodeGetBlob(gb)
		const macLen = 16
		const lenlen = 2
		out := make([]byte, lenlen, len(plain)+macLen+lenlen)
		out[0] = byte(len(plain) & 0xFF)
		out[1] = byte(len(plain) >> 8)

		ciphertext, err := s.send.Encrypt(out, ad, plain)
		if err != nil {
			s.conn.Close()
			delete(sessions, theirId)
			return
		}

		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
}

const cacheGetBlobSql = `
INSERT INTO getblob (timestamp, asker, id)
VALUES (?, ?, ?);
`

func cacheGetBlob(g GetBlob, db *sql.DB) {
	_, err := db.Exec(
		cacheGetBlobSql,
		g.timestamp,
		g.asker,
		g.blobId)
	if err != nil {
		panic(err)
	}
}

const getBlobSql = `
SELECT (timestamp, author, blobid, finalchunk, counter)
FROM blob
WHERE blobid=?;
`

func (g GetBlob) respond(
	theirId [idlen]byte,
	sessions map[[idlen]byte]Session,
	db *sql.DB,
	toSends chan ToSend) {

	g.timestamp = posixNow()
	g.asker = theirId

	s, ok := sessions[theirId]
	if !ok {
		return
	}

	if getBalance(db, theirId) < price {
		s.conn.Close()
		delete(sessions, theirId)
		return
	}

	cacheGetBlob(g, db)

	rows, err := db.Query(getBlobSql, g.blobId)
	if err != nil {
		panic(err)
	}

	for rows.Next() {
		var b Blob
		err = rows.Scan(
			&b.timestamp,
			&b.author,
			&b.blobId,
			&b.finalChunk,
			&b.counter)
		if err != nil {
			panic(err)
		}

		blobPath := makeBlobPath(b.blobId, b.counter)
		b.blob, err = os.ReadFile(blobPath)
		if err != nil {
			panic(err)
		}

		ciphertext, err := encrypt(encodeBlob(b), s)
		if err != nil {
			s.conn.Close()
			delete(sessions, theirId)
			return
		}

		toSends <- ToSend{conn: s.conn, message: ciphertext}
	}
}

func encodeBlob(b Blob) []byte {
	encoded := make([]byte, 1+4+32+32+1+4+len(b.blob))

	encoded[0] = blobIndicator
	copy(encoded[1:], encodeUint32(b.timestamp))
	copy(encoded[1+4:], b.author[:])
	copy(encoded[1+4+32:], b.blobId[:])
	if b.finalChunk {
		encoded[1+4+32+32] = 1
	} else {
		encoded[1+4+32+32] = 0
	}
	copy(encoded[1+4+32+32+1:], encodeUint32(b.counter))
	copy(encoded[1+4+32+32+1+4:], b.blob)

	return encoded
}

type Kk1 struct {
	timestamp uint32
	recipient [idlen]byte
	sender    [idlen]byte
	encrypted [48]byte
}

const cacheKk1Sql = `
INSERT INTO kk1 (timestamp, recipient, sender, kk1)
VALUES (?, ?, ?, ?);
`

func cacheKk1(k Kk1, db *sql.DB) {
	_, err := db.Exec(
		cacheKk1Sql,
		k.timestamp,
		k.recipient,
		k.sender,
		k.encrypted)
	if err != nil {
		panic(err)
	}
}

func (k Kk1) respond(
	theirId [idlen]byte,
	sessions map[[idlen]byte]Session,
	db *sql.DB,
	toSends chan ToSend) {

	k.sender = theirId
	k.timestamp = posixNow()

	s, ok := sessions[theirId]
	if !ok {
		s.conn.Close()
		return
	}

	if getBalance(db, theirId) < price {
		s.conn.Close()
		delete(sessions, theirId)
		return
	}

	cacheKk1(k, db)

	if !inContacts(k.sender, k.recipient, db) {
		return
	}

	rs, ok := sessions[k.recipient]
	if !ok {
		return
	}

	ciphertext, err := encrypt(encodeKk1(k), rs)
	if err != nil {
		rs.conn.Close()
		delete(sessions, k.recipient)
		return
	}

	toSends <- ToSend{conn: rs.conn, message: ciphertext}
}

func encrypt(plain []byte, s Session) ([]byte, error) {
	const macLen = 16
	const lenlen = 2
	out := make([]byte, lenlen, len(plain)+macLen+lenlen)
	out[0] = byte(len(plain) & 0xFF)
	out[1] = byte(len(plain) >> 8)

	return s.send.Encrypt(out, ad, plain)
}

func parseKk1(raw []byte) (Kk1, bool) {
	var k Kk1
	if len(raw) != kk1Len {
		return k, false
	}

	if raw[0] != kk1Indicator {
		return k, false
	}

	copy(k.recipient[:], raw[5:])
	copy(k.sender[:], raw[37:])
	copy(k.encrypted[:], raw[69:])

	return k, true
}

type Kk2 struct {
	timestamp uint32
	recipient [idlen]byte
	sender    [idlen]byte
	sessionId [idlen]byte
	encrypted [48]byte
}

func parseKk2(raw []byte) (Kk2, bool) {
	var k Kk2
	if len(raw) != kk2Len {
		return k, false
	}

	if raw[0] != kk2Indicator {
		return k, false
	}

	copy(k.recipient[:], raw[5:])
	copy(k.sender[:], raw[37:])
	copy(k.sessionId[:], raw[69:])
	copy(k.encrypted[:], raw[101:])

	return k, true
}

type KkTransport struct {
	timestamp uint32
	recipient [idlen]byte
	sender    [idlen]byte
	sessionId [idlen]byte
	blobId    [idlen]byte
	encrypted [48]byte
}

func parseKkTransport(raw []byte) (KkTransport, bool) {
	var k KkTransport
	if len(raw) != kkTransportLen {
		return k, false
	}

	if raw[0] != kkTransportIndicator {
		return k, false
	}

	copy(k.recipient[:], raw[5:])
	copy(k.sender[:], raw[37:])
	copy(k.sessionId[:], raw[69:])
	copy(k.blobId[:], raw[101:])
	copy(k.encrypted[:], raw[133:])

	return k, true
}

type Blob struct {
	timestamp  uint32
	author     [idlen]byte
	blobId     [idlen]byte
	finalChunk bool
	counter    uint32
	blob       []byte
}

type BlobStub struct {
	timestamp  uint32
	author     [idlen]byte
	blobId     [idlen]byte
	finalChunk bool
	counter    uint32
	size       uint32
}

func parseBlob(raw []byte) (Blob, bool) {
	var b Blob
	if len(raw) != minBlobLen {
		return b, false
	}

	if raw[0] != blobIndicator {
		return b, false
	}

	copy(b.blobId[:], raw[5:])
	b.finalChunk = raw[37] == 1

	b.counter = parseUint32(raw[38:42])
	b.blob = raw[42:]
	return b, true
}

type Contact struct {
	timestamp uint32
	contacter [idlen]byte
	contactee [idlen]byte
}

func parseContact(raw []byte) (Contact, bool) {
	var c Contact
	if len(raw) != contactLen-1 {
		return c, false
	}

	copy(c.contacter[:], raw[5:])
	copy(c.contactee[:], raw[37:])
	return c, true
}

type AddContact Contact

type RemoveContact Contact

func parseAddContact(raw []byte) (AddContact, bool) {
	var a AddContact
	if len(raw) != contactLen {
		return a, false
	}

	if raw[0] != addContactIndicator {
		return a, false
	}

	contact, ok := parseContact(raw[1:])
	if !ok {
		return a, false
	}

	return AddContact(contact), true
}

func parseRemoveContact(raw []byte) (RemoveContact, bool) {
	var r RemoveContact
	if len(raw) != contactLen {
		return r, false
	}

	if raw[0] != removeContactIndicator {
		return r, false
	}

	contact, ok := parseContact(raw[1:])
	if !ok {
		return r, false
	}

	return RemoveContact(contact), true
}

type GetBlobRequestsOf [32]byte

func parseUint32(raw []byte) uint32 {
	var result uint32 = 0
	for i, b := range raw {
		result += uint32(b) >> i
	}
	return result
}

type Payment struct {
	timestamp uint32
	amount    uint32
	payer     [idlen]byte
}

func parsePayment(raw []byte) (Payment, bool) {
	var p Payment

	if len(raw) != paymentLen {
		return p, false
	}

	if raw[0] != paymentIndicator {
		return p, false
	}

	p.timestamp = parseUint32(raw[1:5])

	p.amount = parseUint32(raw[5:9])

	copy(p.payer[:], raw[9:])

	return p, true
}

func parseGetBlobRequestsOf(raw []byte) (GetBlobRequestsOf, bool) {
	var g GetBlobRequestsOf
	if len(raw) != getBlobRequestsOfLen {
		return g, false
	}
	if raw[0] != getBlobRequestsOfIndicator {
		return g, false
	}
	copy(g[:], raw[1:])
	return g, true
}

type GetBlob struct {
	timestamp uint32
	asker     [idlen]byte
	blobId    [idlen]byte
}

func parseGetBlob(raw []byte) (GetBlob, bool) {
	var g GetBlob
	if len(raw) != getBlobLen {
		return g, false
	}
	if raw[0] != getBlobIndicator {
		return g, false
	}
	copy(g.asker[:], raw[1+4:])
	copy(g.blobId[:], raw[1+4+32:])
	return g, true
}

func parseGetDataOf(raw []byte) (GetDataOf, bool) {
	var g GetDataOf
	if len(raw) != getDataOfLen {
		return g, false
	}
	if raw[0] != getDataOfIndicator {
		return g, false
	}
	copy(g.theirId[:], raw[1+4:])
	return g, true
}

type GetDataOf struct {
	timestamp uint32
	asker     [idlen]byte
	theirId   [idlen]byte
}

func setupSessions(
	listener net.Listener,
	keys noise.DHKey,
	newSessions chan Session) {

	for {
		conn, err := listener.Accept()
		if err != nil {
			continue
		}
		go func() {
			s, err := doHandshake(conn, keys)
			if err != nil {
				return
			}
			newSessions <- Session{
				conn:    conn,
				receive: s.receive,
				send:    s.send,
				theirId: s.theirId,
			}
		}()
	}
}

func listenToConn(
	theirId [dhlen]byte,
	conn net.Conn,
	rawMessages chan RawMessage) {

	for {
		raw, err := readRaw(conn)
		if err != nil {
			return
		}

		rawMessages <- RawMessage{
			theirId: theirId,
			raw:     raw,
		}
	}
}

type ToSend struct {
	conn    net.Conn
	message []byte
}

const databasePath = "database.sqlite3"

func setupDb() *sql.DB {
	db, err := sql.Open("sqlite3", databasePath)
	if err != nil {
		panic(err)
	}

	for _, createTable := range createTables {
		_, err = db.Exec(createTable)
		if err != nil {
			panic(err)
		}
	}

	return db
}

type Sender struct {
	send    *noise.CipherState
	theirId [dhlen]byte
	conn    net.Conn
}

func handleConn(
	conn net.Conn,
	keys noise.DHKey,
	messages chan RawMessage,
	senders chan Sender) {

	s, err := doHandshake(conn, keys)
	if err != nil {
		return
	}

	senders <- Sender{
		send:    s.send,
		theirId: s.theirId,
		conn:    conn,
	}

	for {
		message, err := readRaw(conn)
		if err != nil {
			return
		}

		messages <- RawMessage{
			theirId: s.theirId,
			raw:     message,
		}
	}
}

func readRaw(conn net.Conn) ([]byte, error) {
	err := conn.SetDeadline(time.Now().Add(tcpDeadline))
	if err != nil {
		return []byte{}, err
	}

	rawlen := make([]byte, 2)
	n, err := conn.Read(rawlen)
	if err != nil {
		return []byte{}, err
	}
	if n != 2 {
		return []byte{}, errors.New("not enough bytes for message length")
	}

	len_ := uint16(rawlen[0]) + uint16(rawlen[1])*256
	if len_ > 15998 {
		return []byte{}, errors.New("message length is greater than 15998 bytes")
	}

	message := make([]byte, len_)
	n, err = conn.Read(message)
	if err != nil {
		return []byte{}, err
	}
	if n != int(len_) {
		return []byte{}, errors.New("not enough bytes for message")
	}

	return message, nil
}

type RawMessage struct {
	theirId [idlen]byte
	raw     []byte
}

const messagesDir = "messages"

func loadMessages() [][]byte {
	fileInfos, err := ioutil.ReadDir(messagesDir)
	if err != nil {
		panic(err)
	}

	messages := make([][]byte, len(fileInfos))
	for i, fileInfo := range fileInfos {
		path := messagesDir + "/" + fileInfo.Name()
		message, err := os.ReadFile(path)
		if err != nil {
			panic(err)
		}
		messages[i] = message
	}

	return messages
}

func noiseConfig(staticKeys noise.DHKey) noise.Config {
	return noise.Config{
		CipherSuite: noise.NewCipherSuite(
			noise.DH25519,
			noise.CipherChaChaPoly,
			noise.HashBLAKE2s),
		Pattern:       noise.HandshakeXK,
		Initiator:     false,
		StaticKeypair: staticKeys,
	}
}

func makeHandshakeState(
	staticKeys noise.DHKey) *noise.HandshakeState {

	config := noiseConfig(staticKeys)
	handshake, err := noise.NewHandshakeState(config)
	if err != nil {
		panic(err)
	}
	return handshake
}

type Session struct {
	conn    net.Conn
	receive *noise.CipherState
	send    *noise.CipherState
	theirId [idlen]byte
}

func send(s ToSend) {
	_, err := s.conn.Write(s.message)
	if err != nil {
		s.conn.Close()
	}
}

const tcpDeadline = 5 * time.Second

func readXk1(conn net.Conn, handshake *noise.HandshakeState) error {
	err := conn.SetDeadline(time.Now().Add(tcpDeadline))
	if err != nil {
		return err
	}

	xk1 := make([]byte, xk1len)
	n, err := conn.Read(xk1)
	if err != nil {
		return err
	}
	if n != xk1len {
		return errors.New("XK1 too short")
	}

	_, _, _, err = handshake.ReadMessage([]byte{}, xk1)
	return err
}

func sendXk2(conn net.Conn, handshake *noise.HandshakeState) error {
	err := conn.SetDeadline(time.Now().Add(tcpDeadline))
	if err != nil {
		return err
	}

	xk2, _, _, err := handshake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return err
	}

	_, err = conn.Write(xk2)
	return err
}

type CryptoState struct {
	send    *noise.CipherState
	receive *noise.CipherState
	theirId [dhlen]byte
}

func readXk3(
	conn net.Conn,
	handshake *noise.HandshakeState) (CryptoState, error) {

	var s CryptoState

	err := conn.SetDeadline(time.Now().Add(tcpDeadline))
	if err != nil {
		return s, err
	}

	xk3 := make([]byte, xk3len)
	n, err := conn.Read(xk3)
	if err != nil {
		return s, err
	}
	if n != xk3len {
		return s, errors.New("XK3 too short")
	}

	_, s.send, s.receive, err = handshake.ReadMessage(
		[]byte{},
		xk3)
	if err != nil {
		return s, err
	}

	copy(s.theirId[:], handshake.PeerStatic())
	return s, nil
}

func doHandshake(
	conn net.Conn,
	staticKeys noise.DHKey) (
	CryptoState,
	error) {

	handshake := makeHandshakeState(staticKeys)

	var s CryptoState

	err := readXk1(conn, handshake)
	if err != nil {
		return s, err
	}

	err = sendXk2(conn, handshake)
	if err != nil {
		return s, err
	}

	s, err = readXk3(conn, handshake)
	if err != nil {
		return s, err
	}

	return s, nil
}

const getKk1sFromOthersSql = `
SELECT timestamp, recipient, sender, kk1
FROM kk1
WHERE recipient=?;
`

const createKk1Table = `
CREATE TABLE IF NOT EXISTS kk1 (
	timestamp INTEGER NOT NULL,
	recipient BLOB NOT NULL,
	sender BLOB NOT NULL,
	encrypted BLOB UNIQUE NOT NULL
);
`

const createKk2Table = `
CREATE TABLE IF NOT EXISTS kk2 (
	timestamp INTEGER NOT NULL,
	recipient BLOB NOT NULL,
	sender BLOB NOT NULL,
	sessionid BLOB UNIQUE NOT NULL,
	encrypted BLOB UNIQUE NOT NULL
);
`

const createKkTransportTable = `
CREATE TABLE IF NOT EXISTS kktransport (
	timestamp INTEGER NOT NULL,
	recipient BLOB NOT NULL,
	sender BLOB NOT NULL,
	sessionid BLOB UNIQUE NOT NULL,
	blobid BLOB NOT NULL,
	encrypted BLOB UNIQUE NOT NULL
);
`

const createBlobTable = `
CREATE TABLE IF NOT EXISTS blob (
	timestamp INTEGER NOT NULL,
	author BLOB NOT NULL,
	blobid BLOB NOT NULL,
	finalchunk INTEGER NOT NULL,
	counter INTEGER NOT NULL,
	size INTEGER,
	PRIMARY KEY (id, counter)
);
`

const createAddContactTable = `
CREATE TABLE IF NOT EXISTS addcontact (
	timestamp INTEGER NOT NULL,
	contacter BLOB NOT NULL,
	contactee BLOB NOT NULL
);
`

const createRemoveContactTable = `
CREATE TABLE IF NOT EXISTS removecontact (
	timestamp INTEGER NOT NULL,
	contacter BLOB NOT NULL,
	contactee BLOB NOT NULL
);
`

const createPaymentTable = `
CREATE TABLE IF NOT EXISTS payment (
	timestamp INTEGER NOT NULL,
	amount INTEGER NOT NULL,
	payer BLOB NOT NULL
);
`

const createGetDataOfTable = `
CREATE TABLE IF NOT EXISTS getdataof (
	timestamp INTEGER NOT NULL,
	asker BLOB NOT NULL,
	theirid BLOB NOT NULL
);
`

const createGetBlobTable = `
CREATE TABLE IF NOT EXISTS getblob (
	timestamp INTEGER NOT NULL,
	asker BLOB NOT NULL,
	id BLOB NOT NULL
);
`

var createTables = []string{
	createKk1Table,
	createKk2Table,
	createKkTransportTable,
	createBlobTable,
	createAddContactTable,
	createRemoveContactTable,
	createPaymentTable,
	createGetDataOfTable,
	createGetBlobTable,
}

func getStaticKeys() noise.DHKey {
	raw, err := os.ReadFile(staticKeysPath)
	if err == nil {
		return noise.DHKey{
			Public:  raw[:dhlen],
			Private: raw[dhlen : dhlen*2],
		}
	}

	if !os.IsNotExist(err) {
		panic(err)
	}

	keys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	if err != nil {
		panic(err)
	}
	err = os.WriteFile(staticKeysPath, encodeKeys(keys), 0400)
	if err != nil {
		panic(err)
	}
	return keys
}

func encodeKeys(keys noise.DHKey) []byte {
	encoded := make([]byte, 2*dhlen)
	copy(encoded, keys.Public)
	copy(encoded, keys.Private)
	return encoded
}

const dhlen = 32

const staticKeysPath = "staticKeys"

func serializeKeys(keys noise.DHKey) []byte {
	result := make([]byte, 2*idlen, 2*idlen)
	copy(result, keys.Private)
	copy(result[idlen:], keys.Public)
	return result
}

const messagesPath = "messages"

const maxInt32 = 2147483647

func intPow(base, power int) int {
	result := 1
	for p := 0; p < power; p++ {
		result = result * base
	}
	return result
}

func decodeUint32(b []byte) uint32 {
	b0 := uint32(b[0])
	b1 := uint32(b[1])
	b2 := uint32(b[2])
	b3 := uint32(b[3])
	return b0 + b1*256 + b2*256*256 + b3*256*256*256
}

func parseAmount(raw []byte, pos int) (uint32, int, error) {
	if len(raw) < pos+4 {
		return 0, pos, fmt.Errorf("not enough bytes for amount")
	}

	amount := decodeUint32(raw[pos : pos+4])
	return amount, pos + 4, nil
}

func parseBlobCounter(raw []byte, pos int) (uint32, int, error) {
	if len(raw) < pos+4 {
		return 0, pos, fmt.Errorf("not enough bytes for blob counter")
	}

	counter := decodeUint32(raw[pos : pos+4])
	return counter, pos + 4, nil
}

const (
	idlen = 32
)

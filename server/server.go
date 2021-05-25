package main

import (
	"github.com/flynn/noise"
	"io/ioutil"
	"fmt"
	"crypto/rand"
)

var go_ = make(chan func())

func main() {
	go_ <- init_
	for {
		(<-go_)()
	}
}

var in = make(chan In)

type Start struct{}

func initState() State {
	return State{
	}
}

type State struct {
	staticKeys MaybeStaticKeys
	messages Messages
}

type MaybeStaticKeys interface {
}

type Panic string

func (p Panic) io(*State) {
	panic(string(p))
}

type In interface {
	pure(*State) Out
}

type Out interface {
	io(*State)
}

func init_() {
	in <- Start{}
	state := initState()
	for {
		(<-in).pure(&state).io(&state)
	}
}

func (Start) pure(state *State) Out {
	return ReadKeysFile{}
}

type ReadKeysFile struct{}

const staticKeysPath = "staticKeys"

func (ReadKeysFile) io(*State) {
	raw, err := ioutil.ReadFile(staticKeysPath)
	in <- StaticKeysFile{raw, err}
}

type StaticKeysFile struct {
	raw []byte
	err error
}

func (s StaticKeysFile) pure(state *State) Out {
	if s.err != nil {
		return GenerateStaticKeys{}
	}

	if len(s.raw) != 2 * idlen {
		return Panic(fmt.Sprintf("bad static keys file length: expecting %d but got %d", 2 * idlen, len(s.raw)))
	}

	return MemCacheStaticKeys(
		noise.DHKey{
			Private: s.raw[:idlen],
			Public: s.raw[idlen:],
		})
}

type GenerateStaticKeys struct{}

func (GenerateStaticKeys) io(*State) {
	keys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	in <- NewKeypair{keys, err}
}

type NewKeypair struct {
	keys noise.DHKey
	err error
}


type MemCacheStaticKeys noise.DHKey

func (c MemCacheStaticKeys) io(state *State) {
	state.staticKeys = GotKeys(c)
}

type GotKeys noise.DHKey

type Outs []Out

func (outs Outs) io(state *State) {
	for _, out := range []Out(outs) {
		out.io(state)
	}
}

func (n NewKeypair) pure(state *State) Out {
	if n.err != nil {
		return Panic("couldn't generate static keys: " + n.err.Error())
	}

	return Outs([]Out{
		MemCacheStaticKeys(n.keys),
		DiskCacheStaticKeys(serializeKeys(n.keys)),
		FetchMessagesFromFile{},
	})
}

func serializeKeys(keys noise.DHKey) []byte {
	result := make([]byte, 2 * idlen, 2 * idlen)
	copy(result, keys.Private)
	copy(result[idlen:], keys.Public)
	return result
}

type FetchMessagesFromFile struct{}

const messagesPath = "messages"

func (FetchMessagesFromFile) io(*State) {
	raw, err := ioutil.ReadFile(messagesPath)
	in <- RawMessagesFile{raw, err}
}

type RawMessagesFile struct {
	raw []byte
	err error
}

func (r RawMessagesFile) pure(state *State) Out {
	if r.err != nil {
		return DoNothing{}
	}

	parsed, err := parseMessages(r.raw)
	if err != nil {
		return Panic("corrupted messages file: " + err.Error())
	}

	return MemCacheMessages(parsed)
}

const maxInt32 = 2147483647

func initMessages() Messages {
	return Messages {
		kk1s: make(map[[idlen]byte]Kk1),
		kk2s: make(map[[idlen]byte]Kk2),
		kkTransports: make(map[[idlen]byte]KkTransport),
		blobs: make(map[[idlen]byte]map[uint32]Blob),
		addContact: make(map[Contact]uint32),
		removeContact: make(map[Contact]uint32),
		payment: make(map[[idlen]byte]Payment),
		getDataOf: make(map[[idlen]byte]uint32),
		getBlob: make(map[[idlen]byte]uint32),
	}
}

func parseMessages(raw []byte) (Messages, error) {
	messages := initMessages()

	var message Message
	var err error
	for position := 0; position < len(raw); {
		message, position, err = parseMessage(raw, position)
		if err != nil {
			return messages, err
		}
		message.store(messages)
	}

	return messages, nil
}

func parseKkTransportMessage(raw []byte, pos int) (KkTransportMessage, int, error) {
	oldPos := pos
	var kk KkTransportMessage
	if raw[pos] != 2 {
		return kk, oldPos, fmt.Errorf("not a KK transport message: expected 2 at start position (position %d), but got %d", pos, raw[pos])
	}

	var err error
	kk.timestamp, pos, err = parseTimestamp(raw, pos)
	if err != nil {
		return kk, oldPos, err
	}

	kk.recipient, pos, err = parseId(raw, pos)
	if err != nil {
		return kk, oldPos, err
	}

	kk.sender, pos, err = parseId(raw, pos)
	if err != nil {
		return kk, oldPos, err
	}

	kk.sessionId, pos, err = parseId(raw, pos)
	if err != nil {
		return kk, oldPos, err
	}

	kk.blobId, pos, err = parseBlobId(raw, pos)
	if err != nil {
		return kk, oldPos, err
	}

	kk.kkTransport, pos, err = parseKkTransport(raw, pos)
	if err != nil {
		return kk, oldPos, err
	}

	return kk, pos, nil
}

func parseKkTransport(raw []byte, pos int) ([kktransportlen]byte, int, error) {
	var kkTransport [kktransportlen]byte
	if copy(kkTransport[:], raw[pos:]) < kktransportlen {
		return kkTransport, pos, fmt.Errorf("not enough bytes for KK transport")
	}
	pos += kktransportlen
	return kkTransport, pos, nil
}

type KkTransportMessage struct {
	timestamp uint32
	recipient [idlen]byte
	sender [idlen]byte
	sessionId [idlen]byte
	blobId [idlen]byte
	kkTransport [kktransportlen]byte
}

func (k KkTransportMessage) store(messages Messages) Messages {
	messages.kkTransports[k.recipient] = KkTransport{
		timestamp: k.timestamp,
		sender: k.sender,
		sessionId: k.sessionId,
		blobId: k.blobId,
		kkTransport: k.kkTransport,
	}
	return messages
}

func parseKk2Message(raw []byte, pos int) (Kk2Message, int, error) {
	oldPos := pos
	var kk2 Kk2Message
	if raw[pos] != 1 {
		return kk2, oldPos, fmt.Errorf("not a KK2 message: expected 1 at start position (position %d), but got %d", pos, raw[pos])
	}
	pos++

	var err error
	kk2.timestamp, pos, err = parseTimestamp(raw, pos)
	if err != nil {
		return kk2, oldPos, err
	}

	kk2.recipient, pos, err = parseId(raw, pos)
	if err != nil {
		return kk2, oldPos, err
	}

	kk2.sender, pos, err = parseId(raw, pos)
	if err != nil {
		return kk2, oldPos, err
	}

	kk2.sessionId, pos, err = parseId(raw, pos)
	if err != nil {
		return kk2, oldPos, err
	}

	kk2.kk2, pos, err = parseKk2(raw, pos)
	if err != nil {
		return kk2, oldPos, err
	}

	return kk2, pos, nil
}

func parseKk2(raw []byte, pos int) ([kk2len]byte, int, error) {
	var kk2 [kk2len]byte
	if copy(kk2[:], raw[pos:]) < kk2len {
		return kk2, pos, fmt.Errorf("not enough bytes for KK2")
	}
	pos += kk2len
	return kk2, pos, nil
}

func parseMessage(raw []byte, pos int) (Message, int, error) {
	var msg Message

	msg, pos, err := parseKk1Message(raw, pos)
	if err == nil {
		return msg, pos, err
	}

	msg, pos, err = parseKk2Message(raw, pos)
	if err == nil {
		return msg, pos, err
	}

	msg, pos, err = parseKkTransportMessage(raw, pos)
	if err == nil {
		return msg, pos, err
	}

	msg, pos, err = parseBlobMessage(raw, pos)
	if err == nil {
		return msg, pos, err
	}

	msg, pos, err = parseAddContact(raw, pos)
	if err == nil {
		return msg, pos, err
	}

	msg, pos, err = parseRemoveContact(raw, pos)
	if err == nil {
		return msg, pos, err
	}

	msg, pos, err = parsePayment(raw, pos)
	if err == nil {
		return msg, pos, err
	}

	msg, pos, err = parseGetPublicDataOf(raw, pos)
	if err == nil {
		return msg, pos, err
	}

	msg, pos, err = parseGetBlob(raw, pos)
	if err == nil {
		return msg, pos, err
	}

	return msg, pos, fmt.Errorf("no parsers for message: %v", raw[pos:])
}

func parseBlobMessage(raw []byte, pos int) (BlobMessage, int, error) {
	oldPos := pos
	var blob BlobMessage
	if raw[pos] != 3 {
		return blob, oldPos, fmt.Errorf("not a blob message: expected 3 at start position (position %d), but got %d", pos, raw[pos])
	}
	pos++

	var err error
	blob.timestamp, pos, err = parseTimestamp(raw, pos)
	if err != nil {
		return blob, oldPos, err
	}

	blob.author, pos, err = parseId(raw, pos)
	if err != nil {
		return blob, oldPos, err
	}

	blob.id, pos, err = parseBlobId(raw, pos)
	if err != nil {
		return blob, oldPos, err
	}

	blob.counter, pos, err = parseBlobCounter(raw, pos)
	if err != nil {
		return blob, oldPos, err
	}

	blob.hash, pos, err = parseBlobHash(raw, pos)
	if err != nil {
		return blob, oldPos, err
	}

	return blob, pos, nil
}

type BlobMessage struct {
	timestamp uint32
	author [idlen]byte
	id [idlen]byte
	counter uint32
	hash [hashlen]byte
}

func (b BlobMessage) store(messages Messages) Messages {
	chunks, ok := messages.blobs[b.id]
	if !ok {
		chunks = make(map[uint32]Blob)
	}
	chunks[b.counter] = Blob{
		timestamp: b.timestamp,
		author: b.author,
		hash: b.hash,
	}
	messages.blobs[b.id] = chunks
	return messages
}

func parseBlobHash(raw []byte, pos int) ([hashlen]byte, int, error) {
	var hash [hashlen]byte
	if copy(hash[:], raw[pos:]) < hashlen {
		return hash, pos, fmt.Errorf("not enough bytes for hash")
	}
	pos += hashlen
	return hash, pos, nil
}

func parseAddContact(raw []byte, pos int) (AddContact, int, error) {
	oldPos := pos
	var contact AddContact
	if raw[pos] != 4 {
		return contact, oldPos, fmt.Errorf("not an add contact message: expected 4 at start position (position %d), but got %d", pos, raw[pos])
	}
	pos++

	var err error
	contact.timestamp, pos, err = parseTimestamp(raw, pos)
	if err != nil {
		return contact, oldPos, err
	}

	contact.contacter, pos, err = parseId(raw, pos)
	if err != nil {
		return contact, oldPos, err
	}

	contact.contactee, pos, err = parseId(raw, pos)
	if err != nil {
		return contact, oldPos, err
	}

	return contact, pos, nil
}

type AddContact struct {
	timestamp uint32
	contacter [idlen]byte
	contactee [idlen]byte
}

func (a AddContact) store(messages Messages) Messages {
	contact := Contact{
		contacter: a.contacter,
		contactee: a.contactee,
	}
	messages.addContact[contact] = a.timestamp
	return messages
}

func parseRemoveContact(raw []byte, pos int) (RemoveContact, int, error) {
	oldPos := pos
	var contact RemoveContact
	if raw[pos] != 5 {
		return contact, oldPos, fmt.Errorf("not a remove contact message: expected 5 at start (position %d), but got %d", pos, raw[pos])
	}
	pos++

	var err error
	contact.timestamp, pos, err = parseTimestamp(raw, pos)
	if err != nil {
		return contact, oldPos, err
	}

	contact.contacter, pos, err = parseId(raw, pos)
	if err != nil {
		return contact, oldPos, err
	}

	contact.contactee, pos, err = parseId(raw, pos)
	if err != nil {
		return contact, oldPos, err
	}

	return contact, pos, nil
}

type RemoveContact struct {
	timestamp uint32
	contacter [idlen]byte
	contactee [idlen]byte
}

func (r RemoveContact) store(messages Messages) Messages {
	contact := Contact{
		contacter: r.contacter,
		contactee: r.contactee,
	}
	messages.removeContact[contact] = r.timestamp
	return messages
}

func parsePayment(raw []byte, pos int) (PaymentMessage, int, error) {
	oldPos := pos
	var payment PaymentMessage
	if raw[pos] != 6 {
		return payment, oldPos, fmt.Errorf("not a payment message: expected 6 at start (position %d), but got %d", pos, raw[pos])
	}
	pos++

	var err error
	payment.timestamp, pos, err = parseTimestamp(raw, pos)
	if err != nil {
		return payment, oldPos, err
	}

	payment.amount, pos, err = parseAmount(raw, pos)
	if err != nil {
		return payment, oldPos, err
	}

	payment.payer, pos, err = parseId(raw, pos)
	if err != nil {
		return payment, oldPos, err
	}

	return payment, pos, nil
}

func parseGetBlob(raw []byte, pos int) (GetBlob, int, error) {
	oldPos := pos
	var getBlob GetBlob
	if raw[pos] != 9 {
		return getBlob, oldPos, fmt.Errorf("not a get blob message: expected 9 at start (position %d), but got %d", pos, raw[pos])
	}
	pos++

	var err error
	getBlob.timestamp, pos, err = parseTimestamp(raw, pos)
	if err != nil {
		return getBlob, oldPos, err
	}

	getBlob.blobId, pos, err = parseBlobId(raw, pos)
	if err != nil {
		return getBlob, oldPos, err
	}

	return getBlob, pos, nil
}

func parseBlobId(raw []byte, pos int) ([idlen]byte, int, error) {
	var blobId [idlen]byte
	if copy(blobId[:], raw[pos:]) != idlen {
		return blobId, pos, fmt.Errorf("not enough bytes for blobId")
	}
	pos += idlen
	return blobId, pos, nil
}

func parseGetPublicDataOf(raw []byte, pos int) (GetDataOf, int, error) {
	oldPos := pos
	var getData GetDataOf

	if raw[pos] != 8 {
		return getData, oldPos, fmt.Errorf("not a get data of: expected 8 at start (position %d), but got %d", pos, raw[pos])
	}
	pos++

	var err error
	getData.timestamp, pos, err = parseTimestamp(raw, pos)
	if err != nil {
		return getData, oldPos, err
	}

	getData.theirId, pos, err = parseId(raw, pos)
	if err != nil {
		return getData, oldPos, err
	}

	return getData, pos, nil
}

type GetDataOf struct {
	timestamp uint32
	theirId [idlen]byte
}

func (g GetDataOf) store(messages Messages) Messages {
	messages.getDataOf[g.theirId] = g.timestamp
	return messages
}

func parseKk1Message(raw []byte, pos int) (Kk1Message, int, error) {
	oldPos := pos
	var kk1 Kk1Message

	if raw[pos] != 0 {
		return kk1, pos, fmt.Errorf("not a KK1: expected 0 at start (position %d), but got %d", oldPos, raw[pos])
	}
	pos++

	var err error
	kk1.timestamp, pos, err = parseTimestamp(raw, pos)
	if err != nil {
		return kk1, oldPos, err
	}

	kk1.recipient, pos, err = parseId(raw, pos)
	if err != nil {
		return kk1, oldPos, err
	}

	kk1.sender, pos, err = parseId(raw, pos)
	if err != nil {
		return kk1, oldPos, err
	}

	kk1.kk1, pos, err = parseKk1(raw, pos)
	if err != nil {
		return kk1, oldPos, err
	}

	return kk1, pos, nil
}

func parseKk1(raw []byte, pos int) ([kk1len]byte, int, error) {
	var kk1 [kk1len]byte
	if copy(kk1[:], raw[pos:]) < kk1len {
		return kk1, pos, fmt.Errorf("not enough bytes for KK1")
	}
	pos += kk1len
	return kk1, pos, nil
}

func parseId(raw []byte, pos int) ([idlen]byte, int, error) {
	var id [idlen]byte
	if copy(id[:], raw[pos:]) < idlen {
		return id, pos, fmt.Errorf("not enough bytes for user ID")
	}
	pos += idlen
	return id, pos, nil
}

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
	return b0 + b1 * 256 + b2 * 256 * 256 + b3 * 256 * 256 * 256
}

type PaymentMessage struct {
	timestamp uint32
	amount uint32
	payer [idlen]byte
}

func (p PaymentMessage) store(messages Messages) Messages {
	messages.payment[p.payer] = Payment{
		timestamp: p.timestamp,
		amount: p.amount,
	}
	return messages
}

func parseAmount(raw []byte, pos int) (uint32, int, error) {
	if len(raw) < pos + 4 {
		return 0, pos, fmt.Errorf("not enough bytes for amount")
	}

	amount := decodeUint32(raw[pos: pos+4])
	return amount, pos+4, nil
}

func parseTimestamp(raw []byte, pos int) (uint32, int, error) {
	if len(raw) < pos + 4 {
		return 0, pos, fmt.Errorf("not enough bytes for timestamp")
	}

	timestamp := decodeUint32(raw[pos: pos+4])
	return timestamp, pos+4, nil
}

func parseBlobCounter(raw []byte, pos int) (uint32, int, error) {
	if len(raw) < pos + 4 {
		return 0, pos, fmt.Errorf("not enough bytes for blob counter")
	}

	counter := decodeUint32(raw[pos: pos+4])
	return counter, pos+4, nil
}

type Message interface {
	store(Messages) Messages
}

func (kk1 Kk1Message) store(messages Messages) Messages {
	messages.kk1s[kk1.recipient] = Kk1{
		timestamp: kk1.timestamp,
		sender: kk1.sender,
		kk1: kk1.kk1,
	}
	return messages
}

func (g GetBlob) store(messages Messages) Messages {
	messages.getBlob[g.blobId] = g.timestamp
	return messages
}

const (
	idlen = 32
	kk1len = 48
	kk2len = 48
	kktransportlen = 48
	hashlen = 32
)

type MemCacheMessages Messages

func (m MemCacheMessages) io(state *State) {
	state.messages = Messages(m)
}

type GetBlob struct {
	timestamp uint32
	blobId [idlen]byte
}

type GetBlobRequestsOf struct {
	timestamp uint32
	blobId [idlen]byte
}

type Messages struct {
	kk1s map[[idlen]byte]Kk1
	kk2s map[[idlen]byte]Kk2
	kkTransports map[[idlen]byte]KkTransport
	blobs map[[idlen]byte]map[uint32]Blob
	addContact map[Contact]uint32
	removeContact map[Contact]uint32
	payment map[[idlen]byte]Payment
	getDataOf map[[idlen]byte]uint32
	getBlob map[[idlen]byte]uint32
}

type Blob struct {
	timestamp uint32
	author [idlen]byte
	hash [hashlen]byte
}

type Payment struct {
	timestamp uint32
	amount uint32
}

type Contact struct {
	contacter [idlen]byte
	contactee [idlen]byte
}

type KkTransport struct {
	timestamp uint32
	sender [idlen]byte
	sessionId [idlen]byte
	blobId [idlen]byte
	kkTransport [kktransportlen]byte
}

type Kk2Message struct {
	timestamp uint32
	sender [idlen]byte
	recipient [idlen]byte
	sessionId [idlen]byte
	kk2 [kk2len]byte
}

func (k Kk2Message) store(messages Messages) Messages {
	messages.kk2s[k.recipient] = Kk2{
		timestamp: k.timestamp,
		sender: k.sender,
		sessionId: k.sessionId,
		kk2: k.kk2,
	}
	return messages
}

type Kk2 struct {
	timestamp uint32
	sender [idlen]byte
	sessionId [idlen]byte
	kk2 [kk2len]byte
}

type Kk1 struct {
	timestamp uint32
	sender [idlen]byte
	kk1 [kk1len]byte
}

type Kk1Message struct {
	timestamp uint32
	recipient [idlen]byte
	sender [idlen]byte
	kk1 [kk1len]byte
}

type DiskCacheStaticKeys []byte

func (d DiskCacheStaticKeys) io(state *State) {
	err := ioutil.WriteFile(staticKeysPath, []byte(d), 0400)
	in <- ErrWritingStaticKeys{err}
}

type ErrWritingStaticKeys struct {
	err error
}

func (e ErrWritingStaticKeys) pure(*State) Out {
	if e.err != nil {
		return Panic("couldn't write static keys to file: " + e.err.Error())
	}

	return DoNothing{}
}

type DoNothing struct{}

func (DoNothing) io(*State) {}

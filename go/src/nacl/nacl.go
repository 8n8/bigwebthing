package main

import (
	"crypto/rand"
	"errors"
	"fmt"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/nacl/sign"
	"io"
	"io/ioutil"
	"net"
)

func intPower(base, power int) int {
	result := 1
	for i := 0; i < power; i++ {
		result = result * base
	}
	return result
}

func decodeInt(bs []byte) int {
	// Most significant byte should be the last one (Little-Endian).
	result := 0
	for i, b := range bs {
		result += int(b) * intPower(256, i)
	}
	return result
}

func encodeInt32(theInt int) []byte {
	result := make([]byte, 4)
	for i, _ := range result {
		result[i] = byte((theInt >> (i * 4)) & 0xFF)
	}
	return result
}

type Msg interface {
	respond(Keys) (Response, error)
}

type GetKeys struct{}

func (GetKeys) respond(keys Keys) (Response, error) {
	return PublicKeys{
		sign:    keys.signPublic,
		encrypt: keys.encryptPublic,
	}, nil
}

type PublicKeys struct {
	sign    []byte
	encrypt []byte
}

func (p PublicKeys) encode() []byte {
	result := make([]byte, 1+32+32)
	result[0] = 5
	copy(result[1:], p.sign)
	copy(result[1+32:], p.encrypt)
	return result
}

func parseGetKeys(raw []byte) (Msg, error) {
	var null Msg
	if len(raw) != 0 {
		return null, errors.New("get keys request isn't empty")
	}

	return GetKeys{}, nil
}

type Unsign struct {
	from   []byte
	signed []byte
}

func (u Unsign) respond(keys Keys) (Response, error) {
	var from [32]byte
	copy(from[:], u.from)
	unsigned, ok := sign.Open([]byte{}, u.signed, &from)
	if !ok {
		return BadUnsign{
			from:   u.from,
			signed: u.signed,
		}, nil
	}

	return Unsigned{
		from:     u.from,
		signed:   u.signed,
		unsigned: unsigned,
	}, nil
}

type BadUnsign struct {
	from   []byte
	signed []byte
}

func (b BadUnsign) encode() []byte {
	result := make([]byte, 1+1+32+len(b.signed))
	result[0] = 4
	result[1] = 0
	copy(result[1+1:], b.from)
	copy(result[1+1+32:], b.signed)
	return result
}

type Unsigned struct {
	from     []byte
	signed   []byte
	unsigned []byte
}

func (u Unsigned) encode() []byte {
	signedLen := len(u.signed)
	unsignedLen := len(u.unsigned)
	result := make([]byte, 1+1+32+4+signedLen+unsignedLen)
	result[0] = 4
	result[1] = 1
	copy(result[1+1:], u.from)
	copy(result[1+1+32:], encodeInt32(signedLen))
	copy(result[1+1+32+4:], u.signed)
	copy(result[32+4+signedLen:], u.unsigned)
	return result
}

func parseUnsign(raw []byte) (Msg, error) {
	var null Msg
	if len(raw) < 33 {
		return null, errors.New("raw unsign is less than 33 bytes")
	}

	return Unsign{
		from:   raw[:32],
		signed: raw[32:],
	}, nil
}

type Sign []byte

func parseSign(raw []byte) (Msg, error) {
	return Sign(raw), nil
}

type Signed struct {
	msg    []byte
	signed []byte
}

func (s Sign) respond(keys Keys) (Response, error) {
	var secretSign [64]byte
	copy(secretSign[:], keys.signSecret)
	signed := sign.Sign([]byte{}, []byte(s), &secretSign)
	return Signed{
		msg:    []byte(s),
		signed: signed,
	}, nil
}

func (s Signed) encode() []byte {
	lenMsg := len(s.msg)
	lenSigned := len(s.signed)
	result := make([]byte, 1+4+lenMsg+lenSigned)
	result[0] = 3
	copy(result[1:], encodeInt32(lenMsg))
	copy(result[1+4:], s.msg)
	copy(result[1+4+lenMsg:], s.signed)
	return result
}

type Decrypt struct {
	from []byte
	msg  []byte
}

func parseDecrypt(raw []byte) (Msg, error) {
	var null Msg
	if len(raw) < 33 {
		return null, errors.New("raw decrypt is less than 33 bytes")
	}

	return Decrypt{
		from: raw[:32],
		msg:  raw[32:],
	}, nil
}

func (d Decrypt) respond(keys Keys) (Response, error) {
	var null Response
	if len(d.msg) < 25 {
		return null, errors.New("message to decrypt is less than 25 bytes")
	}

	var nonce [24]byte
	copy(nonce[:], d.msg)

	msg := d.msg[24:]

	var from [32]byte
	copy(from[:], d.from)

	var mySecretKey [32]byte
	copy(mySecretKey[:], keys.encryptSecret)

	decrypted, ok := box.Open(
		[]byte{}, msg, &nonce, &from, &mySecretKey)

	if !ok {
		return BadDecrypt{
			from: d.from,
			msg:  d.msg,
		}, nil
	}

	return GoodDecrypt{
		from:      d.from,
		encrypted: d.msg,
		decrypted: decrypted,
	}, nil
}

type BadDecrypt struct {
	from []byte
	msg  []byte
}

func (b BadDecrypt) encode() []byte {
	result := make([]byte, 1+1+32+len(b.msg))
	result[0] = 2
	result[1] = 0
	copy(result[1+1:], b.from)
	copy(result[1+1+32:], b.msg)
	return result
}

type GoodDecrypt struct {
	from      []byte
	encrypted []byte
	decrypted []byte
}

func (g GoodDecrypt) encode() []byte {
	lenEnc := len(g.encrypted)
	lenDec := len(g.decrypted)
	result := make([]byte, 1+1+32+4+lenEnc+lenDec)
	result[0] = 2
	result[1] = 1
	copy(result[1+1:], g.from)
	copy(result[1+1+32:], encodeInt32(lenEnc))
	copy(result[1+1+32+4:], g.encrypted)
	copy(result[1+1+32+4+lenEnc:], g.decrypted)
	return result
}

func parseMsg(raw []byte) (Msg, error) {
	var msg Msg
	if len(raw) == 0 {
		return msg, errors.New("empty message")
	}

	switch raw[0] {
	case 1:
		return parseEncrypt(raw[1:])
	case 2:
		return parseDecrypt(raw[1:])
	case 3:
		return parseSign(raw[1:])
	case 4:
		return parseUnsign(raw[1:])
	case 5:
		return parseGetKeys(raw[1:])
	}

	return msg, errors.New("bad indicator byte")
}

type Encrypt struct {
	to  []byte
	msg []byte
}

func getNonce() ([24]byte, error) {
	var nonce [24]byte
	n, err := io.ReadFull(rand.Reader, nonce[:])
	if n != 24 {
		return nonce, errors.New("not enough random bytes for nonce")
	}
	return nonce, err
}

const maxChunkLen = 16000

type Response interface {
	encode() []byte
}

func (e Encrypt) respond(keys Keys) (Response, error) {
	var null Encrypted
	if len(e.msg) > maxChunkLen {
		return null, errors.New("message is too long")
	}

	nonce, err := getNonce()
	if err != nil {
		return null, err
	}

	var encryptSecret [32]byte
	copy(encryptSecret[:], keys.encryptSecret)

	var to [32]byte
	copy(to[:], e.to)

	encrypted := box.Seal(
		nonce[:], e.msg, &nonce, &to, &encryptSecret)

	return Encrypted{
		to:        e.to,
		msg:       e.msg,
		encrypted: encrypted,
	}, nil
}

func (e Encrypted) encode() []byte {
	msgLen := len(e.msg)
	encLen := len(e.encrypted)

	result := make([]byte, 1+32+4+msgLen+encLen)

	result[0] = 1
	copy(result[1:], e.to)
	copy(result[1+32:], encodeInt32(msgLen))
	copy(result[1+32+4:], e.msg)
	copy(result[1+32+4+msgLen:], e.encrypted)

	return result
}

type Encrypted struct {
	to        []byte
	msg       []byte
	nonce     []byte
	encrypted []byte
}

func parseEncrypt(raw []byte) (Msg, error) {
	var msg Msg
	if len(raw) < 33 {
		return msg, errors.New(
			"raw to encrypt is less than 33 bytes")
	}

	return Encrypt{
		to:  raw[:32],
		msg: raw[32:],
	}, nil
}

type Keys struct {
	signPublic    []byte
	signSecret    []byte
	encryptPublic []byte
	encryptSecret []byte
}

const clientDataDir = "clientData"

func initKeys() (Keys, error) {
	var keys Keys
	raw, err := ioutil.ReadFile(clientDataDir + "/keys")
	if err != nil {
		encryptPublic, encryptSecret, err := box.GenerateKey(
			rand.Reader)
		if err != nil {
			return keys, err
		}

		signPublic, signSecret, err := sign.GenerateKey(rand.Reader)
		if err != nil {
			return keys, err
		}

		keys = Keys{
			signPublic:    signPublic[:],
			signSecret:    signSecret[:],
			encryptPublic: encryptPublic[:],
			encryptSecret: encryptSecret[:],
		}

		encoded := make([]byte, 160)
		copy(encoded, signPublic[:])
		copy(encoded[32:], signSecret[:])
		copy(encoded[32+64:], encryptPublic[:])
		copy(encoded[32+64+32:], encryptSecret[:])
		err = ioutil.WriteFile(clientDataDir+"/keys", encoded, 0400)
		if err != nil {
			return keys, err
		}

		return keys, nil
	}

	keys = Keys{
		signPublic:    raw[:32],
		signSecret:    raw[32 : 32+64],
		encryptPublic: raw[32+64 : 32+64+32],
		encryptSecret: raw[32+64+32:],
	}
	return keys, nil
}

const port = "59285"

func main() {
	endChan := make(chan error)

	keys, err := initKeys()
	if err != nil {
		fmt.Println(err)
		return
	}

	outCh := make(chan []byte)

	listener, err := net.Listen("tcp", ":"+port)
	if err != nil {
		endChan <- err
	}

	conn, err := listener.Accept()
	if err != nil {
		endChan <- err
	}

	go func() {
		for {
			rawLen := make([]byte, 4)
			n, err := conn.Read(rawLen)
			if n != 4 {
				endChan <- errors.New("couldn't read length bytes")
			}

			if err != nil {
				endChan <- err
			}

			msgLen := decodeInt(rawLen)
			msg := make([]byte, msgLen)
			n, err = conn.Read(msg)
			if n != msgLen {
				endChan <- errors.New("couldn't read message bytes")
			}
			if err != nil {
				endChan <- err
			}

			parsed, err := parseMsg(msg)
			if err != nil {
				endChan <- err
			}

			response, err := parsed.respond(keys)
			if err != nil {
				endChan <- err
			}

			encoded := response.encode()
			outCh <- append(encodeInt32(len(encoded)), encoded...)
		}
	}()

	go func() {
		for {
			msg := <-outCh
			length := len(msg)
			out := make([]byte, 4+length)
			copy(out, encodeInt32(length))
			copy(out[4:], msg)
			n, err := conn.Write(out)
			if n != length+4 {
				endChan <- errors.New("couldn't write the message")
			}
			if err != nil {
				endChan <- err
			}
		}
	}()

	fmt.Println(<-endChan)
}

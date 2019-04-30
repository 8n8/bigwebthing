package common

import (
	"bytes"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/sign"
	"errors"
	"net"
	"encoding/gob"
)


var ReceiptCode = [16]byte{0xfb, 0x68, 0x66, 0xe0, 0xa3, 0x35,
	0x46, 0x5e, 0x02, 0x49, 0xb9, 0x4b, 0x69, 0xd0, 0x93, 0x4d}

func MakeDigest(hash [32]byte, contextCode [16]byte) []byte {
	digest := make([]byte, 48)
	i := 0
	for i < 32 {
		digest[i] = hash[i]
	}
	for i < 48 {
		digest[i] = contextCode[i-32]
	}
	return HashToSlice(blake2b.Sum256(digest))
}

func AuthCodeToSlice(bs [AuthCodeLength]byte) []byte {
	result := make([]byte, AuthCodeLength)
	for i, b := range bs {
		result[i] = b
	}
	return result
}

type MetadataT struct {
	Blobhash  [32]byte
	Author    [32]byte
	Recipient [32]byte
	Nonce     [24]byte
	Signature [SigSize]byte
}

var EmptyHash = HashToSlice(blake2b.Sum256([]byte("")))

const (
	SigSize        = sign.Overhead + blake2b.Size256
	AuthSigSize    = sign.Overhead + AuthCodeLength
	AuthCodeLength = 24
	ChunkSize = 16000
)

var TruesPubSign = [32]byte{34,148,5,96,91,208,18,233,215,66,198,30,191,49,109,202,113,163,219,102,111,179,146,74,188,147,152,18,53,120,234,249}

func AuthSigToSlice(sig [AuthSigSize]byte) []byte {
	result := make([]byte, AuthSigSize)
	for i := 0; i < AuthSigSize; i++ {
		result[i] = sig[i]
	}
	return result
}

func SigToSlice(bs [SigSize]byte) []byte {
	result := make([]byte, SigSize)
	for i, b := range bs {
		result[i] = b
	}
	return result
}

var AppSigCode = [16]byte{0xb3, 0x7b, 0x8d, 0x83, 0x9d, 0x6c,
	0xd8, 0x6e, 0x52, 0x76, 0xb8, 0xf2, 0x2b, 0x0b, 0x9b, 0xc5}

func HashToSlice(hash [32]byte) []byte {
	newHash := make([]byte, 32)
	for i, el := range hash {
		newHash[i] = el
	}
	return newHash
}

func SliceToHash(sl []byte) [32]byte {
	var newHash [32]byte
	for i, el := range sl {
		newHash[i] = el
	}
	return newHash
}

type Msg interface {
	Encode() ([]byte, error)
	Decode() (Msg, error)
}

type ClientToClient struct {
	Msg       []byte
	Recipient [32]byte
	Nonce [24]byte
	Author [32]byte
}

const (
	ClientToClientR = 0x00
	ErrorR          = 0x01
	AuthCodeR       = 0x02
	AuthSigR        = 0x03
)

func Int64ToBytes(i int64) [8]byte {
	u := uint64(i)
	return [8]byte{
		(byte)(u & 0xff),
		(byte)((u & 0xff00) >> 1),
		(byte)((u & 0xff0000) >> 2),
		(byte)((u & 0xff000000) >> 3),
		(byte)((u & 0xff00000000) >> 4),
		(byte)((u & 0xff0000000000) >> 5),
		(byte)((u & 0xff000000000000) >> 6),
		(byte)((u & 0xff00000000000000) >> 7)}
}

var inviteMeaning = []byte{0x1d, 0x4f, 0xc1, 0x13, 0x0e, 0xd7, 0x94, 0xae, 0x2a, 0x74, 0x9e, 0x49, 0xd0, 0xd2, 0x1b, 0x68}

func EncodeData(a interface{}) ([]byte, error) {
	var buf bytes.Buffer
	enc := gob.NewEncoder(&buf)
	err := enc.Encode(a)
	if err != nil {
		return make([]byte, 0), err
	}
	return buf.Bytes(), nil
}

func intToTwoBytes(i int) ([]byte, error) {
	if i < 0 {
		return *new([]byte), errors.New("Int less than zero.")
	}
	if i > 256*256 {
		return *new([]byte), errors.New(
			"Int greater than 256*256.")
	}
	u := uint(i)
	return []byte{
		(byte)(u & 0xff),
		(byte)((u & 0xff00) >> 8)}, nil
}

func EncodeClientToClient(cToC ClientToClient) ([]byte, error) {
	outerMsg, err := EncodeData(cToC)
	if err != nil {
		return *new([]byte), err
	}
	lenOuterMsg := len(outerMsg)
	if lenOuterMsg > 16000 {
		return *new([]byte), errors.New("Message too long.")
	}
	lenInBytes, err := intToTwoBytes(lenOuterMsg)
	if err != nil {
		return *new([]byte), err
	}
	return append(lenInBytes, outerMsg...), nil
}

func InviteHash(invite InviteT) []byte {
	// 0-8 is ExpiryPosix
	// 8-40 is Invitee
	// 40-56 is InviteMeaning
	result := make([]byte, 56)
	i := 0
	expiryBytes := Int64ToBytes(invite.ExpiryPosix)
	for i < 8 {
		result[i] = expiryBytes[i]
		i++
	}
	for i < 40 {
		result[i] = invite.Invitee[i-8]
		i++
	}
	for i < 56 {
		result[i] = inviteMeaning[i-40]
	}
	return HashToSlice(blake2b.Sum256(result))
}

type InviteT struct {
	ExpiryPosix int64
	Invitee     [32]byte
	Author      [32]byte
	Signature   [SigSize]byte
}

type AuthSigT struct {
	Author  [32]byte
	Sig     [AuthSigSize]byte
}

func IsMember(
	author [32]byte,
	invites []InviteT,
	tNow int64) bool {

	for _, invite := range invites {
		if invite.ExpiryPosix < tNow || !inviteSigOk(invite) {
			return false
		}
	}
	if !bytes.Equal(
		HashToSlice(invites[0].Author),
		HashToSlice(TruesPubSign)) {

		return false
	}
	for i := 1; i < len(invites); i++ {
		linkOk := bytes.Equal(
			HashToSlice(invites[i].Author),
			HashToSlice(invites[i-1].Invitee))
		if !linkOk {
			return false
		}
	}
	return true
}

func inviteSigOk(i InviteT) bool {
	untrusted, sigOk := sign.Open(
		make([]byte, 0),
		SigToSlice(i.Signature),
		&i.Author)
	return bytes.Equal(untrusted, InviteHash(i)) && sigOk
}

func ReadClientToClient(conn net.Conn) (ClientToClient, error) {
	msgLenB := make([]byte, 2)
	n, err := conn.Read(msgLenB)
	if err != nil {
		return *new(ClientToClient), err
	}
	if n != 2 {
		return *new(ClientToClient), errors.New(
			"Message length bytes wrong length.")
	}
	mLen := twoBytesToInt(msgLenB)
	if mLen > 16000 {
		return *new(ClientToClient), errors.New(
			"Message more than 16kB.")
	}
	msg := make([]byte, mLen)
	n, err = conn.Read(msg)
	if err != nil {
		return *new(ClientToClient), err
	}
	if n != mLen {
		return *new(ClientToClient), errors.New(
			"Message wrong length.")
	}
	return decodeClientToClient(msg)
}

func twoBytesToInt(bs []byte) int {
	return (int)(bs[0]) + (int)(bs[1])*256
}

func decodeClientToClient(msg []byte) (ClientToClient, error) {
	var buf bytes.Buffer
	n, err := buf.Write(msg)
	var cToC ClientToClient
	if err != nil {
		return cToC, err
	}
	if n != len(msg) {
		return cToC, errors.New(
			"Could not write message to buffer.")
	}
	err = gob.NewDecoder(&buf).Decode(&cToC)
	if err != nil {
		return cToC, err
	}
	return cToC, nil
}

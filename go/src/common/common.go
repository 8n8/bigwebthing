package common

import (
	"golang.org/x/crypto/nacl/sign"
	"golang.org/x/crypto/blake2b"
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

type MetadataT struct {
	Blobhash [32]byte
	Author [32]byte
	Recipient [32]byte
	Nonce [24]byte
	Signature [SigSize]byte
}

var EmptyHash = HashToSlice(blake2b.Sum256([]byte("")))

const (
	AuthLen = 16
	SigSize = sign.Overhead + blake2b.Size256
	AuthSigSize = sign.Overhead + AuthLen
)

var AppSigCode = [16]byte{0xb3, 0x7b, 0x8d, 0x83, 0x9d, 0x6c,
	0xd8, 0x6e, 0x52, 0x76, 0xb8, 0xf2, 0x2b, 0x0b, 0x9b, 0xc5}


func HashToSlice(hash [32]byte) []byte {
	newHash := make([]byte, 32)
	for i, el := range hash {
		newHash[i] = el
	}
	return newHash
}

type Msg interface {
	Encode() ([]byte, error)
	Decode() (Msg, error)
}

type ClientToClient struct {
	msg []byte
	recipient [32]byte
}

const (
	ClientToClientR = 0x00
	ErrorR = 0x01
	AuthCodeR = 0x02
	AuthSigR = 0x03
)

func EncodeErr(err error) ([]byte, error) {
	errBytes := []byte(err.Error())
	lenErr := len(errBytes)
	if lenErr > 256*256 {
		return make([]byte, 0), errors.New("Error too long.")
	}
	result := make([]byte, lenErr + 3)
	result[0] = lenErr & 0xff00
	result[1] = lenErr & 0xff
	result[2] = errorR
	for i := 3; i < lenErr + 3; i++ {
		result[i] = errBytes[i-3]
	}
	return result, nil
}

type AuthSigT struct {
	author [32]byte
	sig [AuthSigSize]byte
}

const encAuthLen = 3 + 32 + AuthSigSize

func EncAuthSig(a AuthSig) []byte {
	result := make([]byte, encAuthLen)
	result[0] = 0x00
	result[1] = 32 + AuthSigSize
	result[2] = AuthSigR
	for i := 3; i < 35; i++ {
		result[i] = a.author[i-3]
	}
	for i := 35; i < encAuthLen; i++ {
		result[i] = a.sig[i - 33]
	}
	return result
}

func DecAuthSig(bs []byte) (AuthSigT, error) {
	var authSig AuthSig
	lenBs := len(bs)
	rightLen := 32 + AuthSigSize
	if lenBs != rightLen {
		return authSig, errors.New("Wrong length.")
	}
	var author [32]byte
	for i := 0; i < 32; i++ {
		author[i] = bs[i]
	}
	var sig [AuthSigSize]byte
	for i := 0; i < AuthSigSize; i++ {
	}
	return AuthSigT{
		author: bs[:32],
		sig:
	}
}

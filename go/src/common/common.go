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

type Msg interface {
	Encode() ([]byte, error)
	Decode() (Msg, error)
}

type ClientToClient struct {
	Msg []byte
	Recipient [32]byte
	Err error
}

const (
	ClientToClientR = 0x00
	ErrorR = 0x01
	AuthCodeR = 0x02
	AuthSigR = 0x03
)

func int64ToBytes(i int64) [8]byte {
	u := uint64(i)
	return [8]byte{
		(byte)(u & 0xff),
		(byte)(u & 0xff00),
		(byte)(u & 0xff0000),
		(byte)(u & 0xff000000),
		(byte)(u & 0xff00000000),
		(byte)(u & 0xff0000000000),
		(byte)(u & 0xff000000000000),
		(byte)(u & 0xff00000000000000)}
}

var inviteMeaning = []byte{0x1d, 0x4f, 0xc1, 0x13, 0x0e, 0xd7, 0x94, 0xae, 0x2a, 0x74, 0x9e, 0x49, 0xd0, 0xd2, 0x1b, 0x68}


func InviteHash(invite InviteT) []byte {
	// 0-8 is ExpiryPosix
	// 8-40 is Invitee
	// 40-72 is InviteMeaning
	result := make([]byte, 72)
	i := 0
	expiryBytes := int64ToBytes(invite.ExpiryPosix)
	for i < 8 {
		result[i] = expiryBytes[i]
		i++
	}
	for i < 40 {
		result[i] = invite.Invitee[i-8]
		i++
	}
	for i < 72 {
		result[i] = inviteMeaning[i-40]
	}
	return HashToSlice(blake2b.Sum256(result))
}

type InviteT struct {
	ExpiryPosix int64
	Invitee [32]byte
	Author [32]byte
	Signature [SigSize]byte
}

type AuthSigT struct {
	Author [32]byte
	Invites []InviteT
	Sig [AuthSigSize]byte
}

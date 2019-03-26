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

const SigSize = sign.Overhead + blake2b.Size256

var AppSigCode = [16]byte{0xb3, 0x7b, 0x8d, 0x83, 0x9d, 0x6c, 0xd8, 0x6e, 0x52, 0x76, 0xb8, 0xf2, 0x2b, 0x0b, 0x9b, 0xc5}


func HashToSlice(hash [32]byte) []byte {
	newHash := make([]byte, 32)
	for i, el := range hash {
		newHash[i] = el
	}
	return newHash
}

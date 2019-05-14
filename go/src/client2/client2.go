package main

import (
	"archive/tar"
	"bytes"
	"common"
	"crypto/rand"
	"crypto/subtle"
	"crypto/sha256"
	"encoding/base64"
	"encoding/binary"
	"encoding/gob"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"github.com/pkg/browser"
	"goji.io"
	"goji.io/pat"
	"golang.org/x/crypto/argon2"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/nacl/sign"
	"golang.org/x/crypto/ssh/terminal"
	"io"
	"io/ioutil"
	"mime/multipart"
	"net"
	"net/http"
	"os"
	"sort"
	"strings"
	"syscall"
	"time"
)

func main() {
	args := os.Args
	if len(args) != 3 {
		fmt.Println("Args should be port then data directory.")
		return
	}
	port := args[1]
	dataDir := args[2]
	publicSign, secretSign, err := getCryptoKeys(dataDir)
	tcpConn, tcpErr = makeTcpConn(secretSign, publicSign)
	homeCode, err := genHomeCode()
	if err != nil {
		fmt.Println(err)
		return
	}
	addAppCodeChan := make(chan struct{
		securityCode string
		appHash sha256hash
	})
	readAppChan := make(chan struct{
		returnChan chan
	appCodes := make(map[string]sha256hash)
	mux := goji.NewMux()
	mux.HandleFunc(
		pat.Get("/getapp/:securitycode/:filename"),
		func(w http.ResponseWriter, r *http.Request) {
			securityCode := pat.Param(r, "securitycode")
			filename := pat.Param(r, "filename")
			if strEq(securityCode, homeCode) {
				f, err := os.Open("/home/" + filename)
				if err != nil {
					http.Error(w, err.Error(), 400)
					return
				}
				_, err = io.Copy(w, f)
				if err != nil {
					http.Error(w, err.Error(), 500)
					return
				}
				return
			}
			docHash, ok := appCodes[securityCode]
			if !ok {
				http.Error(w, "Bad security code", 400)
				return
			}
			f, err := os.Open(fmt.Sprintf(
				"%s/tmp/%s/%s",
				datatDir,
				hashToStr(dochash),
				filename))
			if err != nil {
				http.Error(w, err.Error(), 500)
				return
			}
			_, err := io.Copy(w, f)
			if err != nil {
				http.Error(w, err.Error(), 500)
			}
		})
	mux.HandleFunc(
		pat.Get("/makeapproute/:securitycode/:apphash"),
		func(w http.ResponseWriter, r *http.Request) {
			securityCode := pat.Pararm(r, securitycode)
			if !strEq(securityCode, homeCode) {
				http.Error(w, "Bad security code.", 400)
				return
			}
			hashSlice, err := base64.URLEncoding.DecodeString(
				pat.Param(r, apphash))
			if err != nil {
				http.Error(w, err.Error(), 400)
				return
			}
			appHash := common.SliceToHash(hashSlice)
			tmpPath = dataDir + "/tmp/" + appHash
			_, err := os.Stat(tmpPath)
			if err == nil {
				appCode, err := getHashSecurityCode(appCodes, appHash)
				if err != nil {
					http.Error(w, err.Error(), 400)
					return
				}
				err = browser.OpenURL(fmt.Sprintf(
					"http://localhost:%s/getapp/%s/index.html",
					port,
					appCode)
				if err != nil {
					http.Error(w, err.Error(), 500)
				}
				return
			}
			fileHandle, err := os.Open(dataDir + "/apps/" + appHash)
			if err != nil {
				http.Error(w, err.Error(), 500)
				return
			}
			err = os.Mkdir(tmpPath, 0755)
			if err != nil {
				http.Error(w, err.Error(), 500)
				return
			}
			tr := tar.NewReader(fileHandle)
			for {
				hdr, err := tr.Next()
				if err == io.EOF {
					break
				}
				if err != nil {
					http.Error(w, err.Error(), 500)
					return
				}
				sourcePath := tmpPath + "/" + hdr.Name
				sourceHandle, err := os.Create(sourcePath)
				if err != nil {
					http.Error(w, err.Error(), 500)
					return
				}
				_, err = io.Copy(sourceHandle, tr)
				if err != nil {
					http.Error(w, err.Error(), 500)
					return
				}
			}
			newCode, err := genCode()
			if err != nil {
				http.Error(w, err.Error(), 500)
				return
			}
			err = browser.OpenURL(fmt.Sprintf(
				"http://localhost:%s/getapp/%s/index.html",
				port,
				newCode))
			if err != nil {
				http.Error(w, err.Error(), 500)
			}
		})
}

func genCode() (string, error) {
	authSlice := make([]byte, 16)
	_, err := rand.Read(authSlice)
	if err != nil {
		return "", err
	}
	return base64.URLEncoding.EncodeToString(authSlice), nil
}

func getHashSecurityCode(appCodes map[string][32]byte, hash [32]byte) (string, error) {
	for c, h := range appCodes {
		if equalHashes(h, hash) {
			return c, nil
		}
	}
	return "", errors.New("Could not find app.")
}

func hashToStr(h [32]byte) string {
	asSlice := common.HashToSlice(h)
	return base64.URLEncoding.EncodeToString(asSlice)
}

func getDocHash(
	securityCode string,
	appCodes map[string][32]byte) ([32]byte, error) {

	for sc, hash := range appCodes {
		if strEq(sc, securityCode) {
			return hash, nil
		}
	}
	var empty [32]byte
	return empty, errors.New("Could not find document hash.")
}


type sha256hash [32]byte

func strEq(s1, s2 string) bool {
	eq := subtle.ConstantTimeCompare([]byte(s1), []byte(s2))
	return eq == 1
}

func makeTcpConn(
	secretSign secretSignT,
	publicSign publicSignT) (net.Conn, error) {

	conn, err := net.Dial("tcp", "localhost:4000")
	if err != nil {
		return conn, err
	}
	var authCode [common.AuthCodeLength]byte
	err = gob.NewDecoder(conn).Decode(&authCode)
	if err != nil {
		return conn, err
	}
	err = gob.NewEncoder(conn).Encode(common.AuthSigT{
		publicSign,
		signedAuthToSlice(sign.Sign(
			make([]byte, 0),
			common.AuthCodeToSlice(authCode),
			&secretSign)),
	}
	return conn, err
}

type publicSignT [32]byte
type secretSignT [32]byte

func keysFile(dataDir string) string {
	return dataDir + "/TOP_SECRET_DONT_SHARE.txt"
}

func createKeys(dataDir string) error {
	publicSign, secretSign, err := sign.GenerateKey(rand.Reader)
	if err != nil {
		return err
	}
	nonce, err := makeNonce()
	if err != nil {
		return err
	}
	salt, err := make32RandomBytes()
	if err != nil {
		return err
	}
	password, err := makePassword()
	if err != nil {
		return err
	}
	secretKey := slowHash(password, salt)
	encryptedSecret := secretbox.Seal(
		make([]byte, 0),
		secretSign,
		&nonce,
		&secretKey)
	secretsFile := secretsFileT{
		PublicSign: *publicSign,
		Nonce: nonce,
		Salt: salt,
		EncryptedSecretSign: encryptedSecret,
	}
	encodedFile, err := json.Marshal(secretsFile)
	if err != nil {
		return err
	}
	err = ioutil.WriteFile(keysFile(dataDir), encodedFile, 0600)
	return err
}

type secretsFileT struct {
	PublicSign publicSignT
	Nonce [24]byte
	Salt [32]byte
	EncryptedSecretSign []byte
}

func slowHash(pw []byte, salt [32]byte) [32]byte {
	return common.SliceToHash(argon2.IDKey(
		pw,
		common.HashToSlice(salt),
		300,
		64*1024,
		4,
		32))
}

func make32RandomBytes() ([32]byte, error) {
	slice := make([]byte, 32)
	var result [32]byte
	n, err := rand.Read(slice)
	if err != nil {
		return result, err
	}
	if n != 32 {
		return result, errors.New(
			"Faulty random bytes reader.")
	}
	for i, b := range slice {
		result[i] = b
	}
	return result, nil
}

func makeNonce() ([24]byte, error) {
	nonceSlice := make([]byte, 24)
	var nonce [24]byte
	n, err := rand.Read(nonceSlice)
	if err != nil {
		return nonce, err
	}
	if n != 24 {
		return nonce, errors.New("Faulty random bytes reader.")
	}
	for i, b := range nonceSlice {
		nonce[i] = b
	}
	return nonce, nil
}

func getArgs() (string, string, error) {
	args := os.Args
	if len(args) != 3 {
		return 0, "", errors.New("Bad arguments.")
	}
	return args[1], args[2], nil
}

func getCryptoKeys(dataDir string) (publicSignT, secretSignT, error) (
	rawSecrets, err := ioutil.ReadFile(keysFile(dataDir))
	if err != nil {
		err := createKeys(dataDir)
		if err != nil {
			return s, err
		}
		rawSecrets, err = ioutil.ReadFile(keysFile(dataDir))
		if err != nil {
			return s, err
		}
	}
	fmt.Println("Please enter your password:")
	passwordtxt, err := terminal.ReadPassword(int(syscall.Stdin))
	password := make([]byte, hex.DecodedLen(len(passwordtxt)))
	_, err = hex.Decode(password, passwordtxt)
	if err != nil {
		return *new(publicSignT), *new(secretSignT), err
	}
	return extractKeys(password, rawSecrets)
}

func extractKeys(
	password []byte,
	secretsFile []byte) (publicSignT, secretSignT, error) {

	var decoded secretsFileT
	err := json.Unmarshal(secretsFile, &decoded)
	if err != nil {
		return *new(publicSignT), *new(secretSignT), err
	}
	symmetricKey := slowHash(password, decoded.Salt)
	secretSign, ok := secretbox.Open(
		make([]byte, 0),
		decoded.EncryptedSecretSign,
		&decoded.Nonce,
		&symmetricKey)
	if !ok {
		err = errors.New("Could not decrypt secret key.")
		return *new(publicSignT), *new(secretSignT), err
	}
	return decoded.PublicSign, secretSign, nil
}

func genHomeCode() (string, error) {
	authSlice := make([]byte, 16)
	_, err := rand.Read(authSlice)
	if err != nil {
		return "", err
	}
	return base64.URLEncoding.EncodeToString(authSlice), nil
}

package main

import (
	"archive/tar"
	"bytes"
	"common"
	"crypto/rand"
	"crypto/subtle"
	"encoding/base64"
	"encoding/gob"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"math/big"
	"net/http"
	"os"
	"sync"
	"syscall"

	"github.com/pkg/browser"
	"goji.io"
	"goji.io/pat"
	"golang.org/x/crypto/argon2"
	"golang.org/x/crypto/blake2b"
	"golang.org/x/crypto/nacl/box"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/nacl/sign"
	"golang.org/x/crypto/ssh/terminal"
)

var homeCode string
var appCodes = make(map[string]blake2bHash)
var appCodesMux sync.Mutex
var publicSign publicSignT
var secretSign [64]byte
var publicEncrypt publicEncryptT
var secretEncrypt secretEncryptT
var dataDir string
var port string
var globalMetadata []metadataT
var metadataMux sync.Mutex

type publicSignT [32]byte
type publicEncryptT [32]byte
type secretEncryptT [32]byte
type blake2bHash [32]byte
type sigT [96]byte

func equalHashes(as [32]byte, bs [32]byte) bool {
	for i, b := range bs {
		if as[i] != b {
			return false
		}
	}
	return true
}

func httpMakeAppErr(r *http.Request) (error, int) {
	appHash := pat.Param(r, "apphash")
	hashSlice, err := base64.URLEncoding.DecodeString(appHash)
	if err != nil {
		return err, 400
	}
	hash := sliceToHash(hashSlice)
	hashStr := hashToStr(hash)
	tmpPath := tmpDir() + hashStr
	_, err = os.Stat(tmpPath)
	appPath := dataDir + "/apps/" + hashStr
	appCodesMux.Lock()
	defer appCodesMux.Unlock()
	if err == nil {
		appCode, err := getHashSecurityCode(hash)
		if err != nil {
			return err, 400
		}
		err = browser.OpenURL(appURL(appCode))
		if err != nil {
			return err, 500
		}
		return nil, 0
	}
	err = unpackTarArchive(appPath, tmpPath)
	if err != nil {
		return err, 500
	}
	newCode, err := genCode()
	if err != nil {
		return err, 500
	}
	err = browser.OpenURL(appURL(newCode))
	if err != nil {
		return err, 500
	}
	appCodes[newCode] = hash
	return nil, 0
}

func hashToSlice(hash [32]byte) []byte {
	newHash := make([]byte, 32)
	for i, el := range hash {
		newHash[i] = el
	}
	return newHash
}

func sliceToHash(sl []byte) [32]byte {
	var newHash [32]byte
	for i, el := range sl {
		newHash[i] = el
	}
	return newHash
}

func strEq(s1, s2 string) bool {
	eq := subtle.ConstantTimeCompare([]byte(s1), []byte(s2))
	return eq == 1
}

func getDocHash(securityCode string) (blake2bHash, error) {
	appCodesMux.Lock()
	defer appCodesMux.Unlock()
	for sc, hash := range appCodes {
		if strEq(sc, securityCode) {
			return hash, nil
		}
	}
	return *new(blake2bHash), errors.New("no document hash found")
}

func hashToStr(h [32]byte) string {
	return base64.URLEncoding.EncodeToString(hashToSlice(h))
}

func getHashSecurityCode(hash blake2bHash) (string, error) {
	for c, h := range appCodes {
		if equalHashes(h, hash) {
			return c, nil
		}
	}
	return "", errors.New("could not find app")
}

var passwordChars = []rune("abcdefghjkmnpqrstuvwxyz23456789")

func makePassword() (string, error) {
	password := make([]rune, 10)
	numChars := big.NewInt(int64(len(passwordChars)))
	for i := range password {
		bigI, err := rand.Int(rand.Reader, numChars)
		if err != nil {
			return "", err
		}
		password[i] = passwordChars[int(bigI.Int64())]
	}
	return string(password), nil
}

func sliceToSecretKey(secret []byte) [64]byte {
	var result [64]byte
	for i, b := range secret {
		result[i] = b
	}
	return result
}

func secretKeyToSlice(secret [64]byte) []byte {
	result := make([]byte, 64)
	for i, b := range secret {
		result[i] = b
	}
	return result
}

func makeSalt() ([32]byte, error) {
	nonceSlice := make([]byte, 32)
	var nonce [32]byte
	n, err := rand.Read(nonceSlice)
	if err != nil {
		return nonce, err
	}
	if n != 32 {
		return nonce, errors.New("faulty random bytes reader")
	}
	for i, b := range nonceSlice {
		nonce[i] = b
	}
	return nonce, nil
}

func makeNonce() ([24]byte, error) {
	nonceSlice := make([]byte, 24)
	var nonce [24]byte
	n, err := rand.Read(nonceSlice)
	if err != nil {
		return nonce, err
	}
	if n != 24 {
		return nonce, errors.New("faulty random bytes reader")
	}
	for i, b := range nonceSlice {
		nonce[i] = b
	}
	return nonce, nil
}

func genCode() (string, error) {
	authSlice := make([]byte, 16)
	_, err := rand.Read(authSlice)
	if err != nil {
		return "", err
	}
	return base64.URLEncoding.EncodeToString(authSlice), nil
}

type secretsFileT struct {
	Publicsign          [32]byte
	Nonce               [24]byte
	Salt                [32]byte
	EncryptedSecretSign []byte
}

type keysT struct {
	publicsign [32]byte
	secretsign [64]byte
}

func slowHash(pw []byte, salt [32]byte) [32]byte {
	return sliceToHash(argon2.IDKey(
		pw,
		hashToSlice(salt),
		100,
		64*1024,
		4,
		32))
}

func extractKeys(password []byte, secretsFile []byte) (keysT, error) {
	var decoded secretsFileT
	err := json.Unmarshal(secretsFile, &decoded)
	var keys keysT
	if err != nil {
		return keys, err
	}
	symmetricKey := slowHash(password, decoded.Salt)
	secretSignBytes, ok := secretbox.Open(
		make([]byte, 0),
		decoded.EncryptedSecretSign,
		&decoded.Nonce,
		&symmetricKey)
	if !ok {
		return keys, errors.New("could not decrypt keys")
	}
	return keysT{
		decoded.Publicsign,
		sliceToSecretKey(secretSignBytes)}, nil
}

func createKeys() error {
	pubSign, secretSign, err := sign.GenerateKey(rand.Reader)
	if err != nil {
		return err
	}
	nonce, err := makeNonce()
	if err != nil {
		return err
	}
	salt, err := makeSalt()
	if err != nil {
		return err
	}
	password, err := makePassword()
	if err != nil {
		return err
	}
	fmt.Println(password)
	secretKey := slowHash([]byte(password), salt)
	encryptedSecrets := secretbox.Seal(
		make([]byte, 0),
		secretKeyToSlice(*secretSign),
		&nonce,
		&secretKey)
	secretsFile := secretsFileT{
		Publicsign:          *pubSign,
		Nonce:               nonce,
		Salt:                salt,
		EncryptedSecretSign: encryptedSecrets,
	}
	encodedFile, err := json.Marshal(secretsFile)
	if err != nil {
		fmt.Println("Could not encode.")
		return err
	}
	err = ioutil.WriteFile(keysFile(), encodedFile, 0600)
	return err
}

func keysFile() string {
	return dataDir + "/TOP_SECRET_DONT_SHARE.txt"
}

func gobRegister() {
	gob.Register(*new(common.ClientToClient))
	gob.Register(*new(common.Encrypted))
	gob.Register(*new(common.GiveMeASymmetricKey))
	gob.Register(*new(common.HereIsAnEncryptionKey))
}

func getCryptoKeys() error {
	rawSecrets, err := ioutil.ReadFile(keysFile())
	if err != nil {
		err := createKeys()
		if err != nil {
			return err
		}
		rawSecrets, err = ioutil.ReadFile(keysFile())
		if err != nil {
			return err
		}
	}
	fmt.Println("Please enter your password:")
	password, err := terminal.ReadPassword(int(syscall.Stdin))
	keys, err := extractKeys(password, rawSecrets)
	if err != nil {
		return err
	}
	secretSign = keys.secretsign
	publicSign = keys.publicsign
	pub, priv, err := box.GenerateKey(rand.Reader)
	if err != nil {
		return err
	}
	secretEncrypt = *priv
	publicEncrypt = *pub
	return nil
}

const badArgs = `Required arguments:
1. port
2. directory containing the data
`

func readArgs() error {
	args := os.Args
	if len(args) != 3 {
		return errors.New(badArgs)
	}
	port = args[1]
	dataDir = args[2]
	return nil
}

func setup() error {
	gobRegister()
	err := readArgs()
	if err != nil {
		return fmt.Errorf("readArgs: %v", err)
	}
	err = os.RemoveAll(tmpDir())
	if err != nil {
		return err
	}
	err = os.Mkdir(tmpDir(), 0755)
	if err != nil {
		return err
	}
	homeCode, err = genCode()
	if err != nil {
		return err
	}
	err = getCryptoKeys()
	if err != nil {
		return err
	}
	err = browser.OpenURL(appURL(homeCode))
	return err
}

func main() {
	err := setup()
	if err != nil {
		fmt.Println(err)
		return
	}
	httpServer()
}

func httpServer() {
	mux := goji.NewMux()
	mux.HandleFunc(pat.Get("/getapp/:pass/:filename"), httpGetApp)
	mux.HandleFunc(pat.Get("/makeapp/:pass/:apphash"), h(httpMakeApp))
	mux.HandleFunc(pat.Post("/install/:pass"), h(httpInstall))
	http.ListenAndServe(":"+port, mux)
}

func httpInstall(w http.ResponseWriter, r *http.Request) {
	err, code := httpInstallErr(r)
	if err != nil {
		http.Error(w, err.Error(), code)
	}
}

func readApp(tarReader *tar.Reader, hasher io.Writer) error {

	tarHeader, err := tarReader.Next()
	if err != nil {
		return err
	}
	if tarHeader.Name != "app.tar" {
		return errors.New("no app.tar file")
	}
	_, err = io.Copy(hasher, tarReader)
	return err
}

func readTxt(
	tarReader *tar.Reader,
	hasher io.Writer,
	filename string) (string, error) {

	tarHeader, err := tarReader.Next()
	if err != nil {
		return "", err
	}
	if tarHeader.Name != filename {
		return "", errors.New("no " + filename + " file")
	}
	description, err := ioutil.ReadAll(tarReader)
	if err != nil {
		return "", err
	}
	_, err = hasher.Write(description)
	return string(description), err
}

func httpInstallErr(r *http.Request) (error, int) {
	urlBytes, err := ioutil.ReadAll(r.Body)
	if err != nil {
		return err, 400
	}
	urlString := string(urlBytes)
	resp, err := http.Get(urlString)
	defer resp.Body.Close()
	if err != nil {
		return err, 400
	}
	tmpFileName, err := genCode()
	if err != nil {
		return err, 500
	}
	tmpPath := tmpDir() + "/" + tmpFileName
	tmpHandle, err := os.Open(tmpPath)
	if err != nil {
		return err, 500
	}
	metadata, err := checkAppSig(io.TeeReader(resp.Body, tmpHandle))
	if err != nil {
		return err, 500
	}
	metadataMux.Lock()
	globalMetadata = append(globalMetadata, metadata)
	encoded, err := json.Marshal(globalMetadata)
	metadataMux.Unlock()
	if err != nil {
		return err, 500
	}
	err = ioutil.WriteFile(metadataPath(), encoded, 0600)
	if err != nil {
		return err, 500
	}
	err = os.Rename(
		tmpPath, appsPath()+"/"+hashToStr(metadata.appHash))
	if err != nil {
		return err, 500
	}
	return nil, 0
}

func appsPath() string {
	return dataDir + "/apps"
}

func metadataPath() string {
	return dataDir + "/metadata.json"
}

func readAppSig(tarReader *tar.Reader) (sigT, error) {
	var sig sigT
	tarHeader, err := tarReader.Next()
	if err != nil {
		return sig, err
	}
	if tarHeader.Name != "signature" {
		return sig, errors.New("no signature file")
	}
	n, err := tarReader.Read(sig[:])
	if n != 96 {
		return sig, errors.New("could not read from tarReader")
	}
	return sig, err
}

func readAppAuthor(tarReader *tar.Reader) (publicSignT, error) {
	var author publicSignT
	tarHeader, err := tarReader.Next()
	if err != nil {
		return author, err
	}
	if tarHeader.Name != "author" {
		return author, errors.New("no author file")
	}
	n, err := tarReader.Read(author[:])
	if n != 32 {
		return author, errors.New("could not read from tarReader")
	}
	return author, err
}

func readIcon(tarReader *tar.Reader, hasher io.Writer) ([]byte, error) {
	tarHeader, err := tarReader.Next()
	if err != nil {
		return []byte{}, err
	}
	if tarHeader.Name != "icon.webp" {
		return []byte{}, errors.New("no icon.webp file")
	}
	icon, err := ioutil.ReadAll(tarReader)
	if err != nil {
		return []byte{}, err
	}
	_, err = hasher.Write(icon)
	return icon, err
}

type metadataT struct {
	name        string
	description string
	icon        []byte
	appHash     blake2bHash
}

func checkAppSig(appReader io.Reader) (metadataT, error) {
	tarReader := tar.NewReader(appReader)
	signature, err := readAppSig(tarReader)
	var md metadataT
	if err != nil {
		return md, err
	}
	author, err := readAppAuthor(tarReader)
	if err != nil {
		return md, err
	}
	hasher, err := blake2b.New256(nil)
	if err != nil {
		return md, err
	}
	err = readApp(tarReader, hasher)
	if err != nil {
		return md, err
	}
	icon, err := readIcon(tarReader, hasher)
	if err != nil {
		return md, err
	}
	md.icon = icon
	description, err := readTxt(tarReader, hasher, "description.txt")
	if err != nil {
		return md, err
	}
	md.description = description

	name, err := readTxt(tarReader, hasher, "name.txt")
	if err != nil {
		return md, err
	}
	md.name = name
	sigBytes := make([]byte, 96)
	copy(sigBytes, signature[:])
	var authArr [32]byte
	copy(authArr[:], author[:])
	signedHash, ok := sign.Open(make([]byte, 0), sigBytes, &authArr)
	if !ok {
		return md, errors.New("bad signature")
	}
	appHash := hasher.Sum(nil)
	copy(md.appHash[:], appHash)
	if !bytes.Equal(signedHash, appHash) {
		return md, errors.New("bad app hash")
	}
	return md, nil
}

func serveDoc(w http.ResponseWriter, filePath string) error {
	fileHandle, err := os.Open(filePath)
	if err != nil {
		return err
	}
	defer fileHandle.Close()
	_, err = io.Copy(w, fileHandle)
	return err
}

func appURL(appCode string) string {
	return fmt.Sprintf(
		"http://localhost:%s/getapp/%s/index.html",
		port,
		appCode)
}

func unpackTarArchive(source string, dest string) error {
	fileHandle, err := os.Open(source)
	if err != nil {
		return err
	}
	err = unpackTarReader(fileHandle, dest)
	return err
}

func unpackTarReader(fileHandle io.Reader, dest string) error {
	err := os.Mkdir(dest, 0755)
	if err != nil {
		return err
	}

	tarReader := tar.NewReader(fileHandle)
	for {
		tarHeader, err := tarReader.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return err
		}
		sourcePath := dest + "/" + tarHeader.Name
		sourceHandle, err := os.Create(sourcePath)
		if err != nil {
			return err
		}
		_, err = io.Copy(sourceHandle, tarReader)
		if err != nil {
			return err
		}
	}
	return nil
}

func httpGetApp(w http.ResponseWriter, r *http.Request) {
	err := httpGetAppErr(w, r)
	if err != nil {
		http.Error(w, err.Error(), 500)
	}
}

func httpGetAppErr(w http.ResponseWriter, r *http.Request) error {
	securityCode := pat.Param(r, "pass")
	filename := pat.Param(r, "filename")
	if strEq(securityCode, homeCode) {
		err := serveDoc(w, "home/"+filename)
		if err != nil {
			return err
		}
		return nil
	}
	docHash, err := getDocHash(securityCode)
	if err != nil {
		return err
	}
	filePath := fmt.Sprintf(
		"%s/%s/%s",
		tmpDir(),
		hashToStr(docHash),
		filename)
	err = serveDoc(w, filePath)
	if err != nil {
		return err
	}
	return nil
}

func httpMakeApp(w http.ResponseWriter, r *http.Request) {
	err, code := httpMakeAppErr(r)
	if err != nil {
		http.Error(w, err.Error(), code)
	}
}

type handler func(w http.ResponseWriter, r *http.Request)

func h(f handler) handler {
	return func(w http.ResponseWriter, r *http.Request) {
		if !strEq(pat.Param(r, "pass"), homeCode) {
			http.Error(w, "bad security code", 400)
			return
		}
		f(w, r)
	}
}

func tmpDir() string { return dataDir + "/tmp" }

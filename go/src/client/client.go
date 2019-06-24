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
	"net"
	"net/http"
	"os"
	"sort"
	"sync"
	"syscall"
	"time"
	"unicode/utf8"

	"github.com/fsnotify/fsnotify"
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
var symmetricKeys = make(map[publicSignT]symmetricEncrypt)
var symmetricKeysMux sync.Mutex
var awaitingKey = make(map[publicSignT]filePathT)
var awaitingKeyMux sync.Mutex
var tcpOutChan = make(chan common.ClientToClient)

type filePathT string
type publicSignT [32]byte
type publicEncryptT [32]byte
type secretEncryptT [32]byte
type blake2bHash [32]byte
type symmetricEncrypt [32]byte

func inboxPath() string { return dataDir + "/inbox" }

func outboxPath() string { return dataDir + "/outbox" }

func equalHashes(as [32]byte, bs [32]byte) bool {
	for i, b := range bs {
		if as[i] != b {
			return false
		}
	}
	return true
}

func writeAppToFile(r *http.Request) error {
	bodyFileReader, err := r.MultipartReader()
	if err != nil {
		return err
	}
	filepart, err := bodyFileReader.NextPart()
	if err != nil {
		return err
	}
	if filepart.FormName() != "file" {
		return errors.New("could not find form element \"file\"")
	}
	tmpFileName, err := genCode()
	if err != nil {
		return err
	}
	tmpPath := dataDir + "/tmp/" + tmpFileName
	fileHandle, err := os.Create(tmpPath)
	defer fileHandle.Close()
	if err != nil {
		return err
	}
	hasher, err := blake2b.New256(nil)
	if err != nil {
		return err
	}
	tee := io.TeeReader(filepart, hasher)
	_, err = io.Copy(fileHandle, tee)
	if err != nil {
		return err
	}
	hash := base64.URLEncoding.EncodeToString(hasher.Sum(nil))
	err = os.Rename(tmpPath, dataDir+"/apps/"+hash)
	return err
}

func hashFromString(s string) ([32]byte, error) {
	hashSlice, err := base64.URLEncoding.DecodeString(s)
	var hash [32]byte
	if err != nil {
		return hash, err
	}
	if len(hashSlice) != 32 {
		return hash, errors.New("hash wrong length")
	}
	for i, b := range hashSlice {
		hash[i] = b
	}
	return hash, nil
}

func makeConn() (net.Conn, error) {
	conn, err := net.Dial("tcp", "localhost:4000")
	if err != nil {
		return conn, err
	}
	var authCode [common.AuthCodeLength]byte
	err = gob.NewDecoder(conn).Decode(&authCode)
	if err != nil {
		return conn, err
	}
	authSig := common.AuthSigT{
		publicSign,
		signedAuthToSlice(sig(common.AuthCodeToSlice(authCode))),
	}
	err = gob.NewEncoder(conn).Encode(authSig)
	return conn, err
}

type chunkT struct {
	Body     []byte
	CakeHash blake2bHash
	Counter  int
	Final    bool
}

type chunkPtrT struct {
	Hash     string
	CakeHash blake2bHash
	Counter  int
	Final    bool
}

type plain struct {
	Bin           []byte
	Correspondent publicSignT
}

const txtMsgCodeLen = 16

var txtMsgCode = [txtMsgCodeLen]byte{
	244, 108, 174, 239, 63, 159, 201, 117, 90, 254, 165, 34, 118,
	208, 130, 224}

func sigToSlice(bs [common.SigSize]byte) []byte {
	result := make([]byte, common.SigSize)
	for i, b := range bs {
		result[i] = b
	}
	return result
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

func processCToC(c common.ClientToClient) error {
	switch payload := c.Msg.(type) {
	case common.Encrypted:
		symmetricKeysMux.Lock()
		symmetricKey, ok := symmetricKeys[c.Author]
		symmetricKeysMux.Unlock()
		if !ok {
			return errors.New("could not find symmetric key")
		}
		symmetricKeySlice := [32]byte(symmetricKey)
		decrypted, ok := secretbox.Open(
			make([]byte, 0),
			payload.Msg,
			&payload.Nonce,
			&symmetricKeySlice)
		if !ok {
			return errors.New("could not decrypt message")
		}
		if !equalHashes(c.Recipient, publicSign) {
			return errors.New("ClientToClient wrongly addressed")
		}
		fileName := hashToStr(blake2b.Sum256(decrypted))
		path := inboxPath() + "/" + fileName
		err := ioutil.WriteFile(path, decrypted, 0600)
		return err
	case common.GiveMeASymmetricKey:
		signed, ok := sign.Open(
			make([]byte, 0),
			sigToSlice(payload.Sig),
			&c.Author)
		if !ok {
			return errors.New("bad signature")
		}
		if !equalHashes(
			sliceToHash(signed),
			hashPubEncrypt(payload.MyPublicEncrypt)) {

			return errors.New("signature doesn't match public key")
		}
		symmetricKey, err := makeSymmetricKey()
		if err != nil {
			return err
		}
		nonce, err := makeNonce()
		if err != nil {
			return err
		}
		secretEncBytes := [32]byte(secretEncrypt)
		encryptedKey := box.Seal(
			make([]byte, 0),
			hashToSlice(symmetricKey),
			&nonce,
			&payload.MyPublicEncrypt,
			&secretEncBytes)
		hereIs := common.HereIsAnEncryptionKey{
			publicEncrypt,
			encryptedKeyToArr(encryptedKey),
			nonce,
			*new([common.SigSize]byte),
		}
		signature := sliceToSig(sig(hashToSlice(
			hashHereIsKey(hereIs))))
		hereIs.Sig = signature
		tcpOutChan <- common.ClientToClient{
			hereIs, c.Author, publicSign}
		symmetricKeysMux.Lock()
		symmetricKeys[c.Author] = symmetricKey
		symmetricKeysMux.Unlock()
		return nil
	case common.HereIsAnEncryptionKey:
		awaitingKeyMux.Lock()
		filePath, ok := awaitingKey[c.Author]
		awaitingKeyMux.Unlock()
		if !ok {
			return errors.New("symmetric key not expected")
		}
		keyHash := hashHereIsKey(payload)
		signed, ok := sign.Open(
			make([]byte, 0),
			sigToSlice(payload.Sig),
			&c.Author)
		if !ok {
			return errors.New("bad signature on new key")
		}
		if !equalHashes(sliceToHash(signed), keyHash) {
			return errors.New("bad key hash on new key")
		}
		secretKeyBytes := [32]byte(secretEncrypt)
		keySlice, ok := box.Open(
			make([]byte, 0),
			encKeyToSlice(payload.EncryptedSymmetricKey),
			&payload.Nonce,
			&payload.MyPublicEncrypt,
			&secretKeyBytes)
		if !ok {
			return errors.New("could not decrypt new key")
		}
		symmetricKey := sliceToHash(keySlice)
		awaitingKeyMux.Lock()
		delete(awaitingKey, c.Author)
		awaitingKeyMux.Unlock()
		symmetricKeysMux.Lock()
		symmetricKeys[c.Author] = symmetricKey
		symmetricKeysMux.Unlock()
		err := sendFileTcp(string(filePath))
		return err
	}
	return errors.New("bad pattern match")
}

var pubEncHashCode = [16]byte{
	128, 174, 215, 253, 100, 130, 29, 105, 25, 9, 193, 255, 3, 66,
	215, 111}

func hashPubEncrypt(pub publicEncryptT) blake2bHash {
	const resultLen = 32 + 16
	result := make([]byte, resultLen)
	i := 0
	for ; i < 32; i++ {
		result[i] = pub[i]
	}
	for ; i < resultLen; i++ {
		result[i] = pubEncHashCode[i-32]
	}
	return blake2b.Sum256(result)
}

func sendFileTcp(filePath string) error {
	fileHandle, err := os.Open(filePath)
	if err != nil {
		return err
	}
	var unencrypted plain
	err = gob.NewDecoder(fileHandle).Decode(&unencrypted)
	fileHandle.Close()
	if err != nil {
		return err
	}
	symmetricKeysMux.Lock()
	symmetricKey, ok := symmetricKeys[unencrypted.Correspondent]
	symmetricKeysMux.Unlock()
	if !ok {
		awaitingKeyMux.Lock()
		awaitingKey[unencrypted.Correspondent] = filePathT(filePath)
		awaitingKeyMux.Unlock()
		tcpOutChan <- common.ClientToClient{
			common.GiveMeASymmetricKey{
				publicEncrypt,
				sliceToSig(sig(hashToSlice(hashPubEncrypt(
					publicEncrypt)))),
			},
			unencrypted.Correspondent,
			publicSign,
		}
		return nil
	}
	nonce, err := makeNonce()
	if err != nil {
		return err
	}
	symKeyArr := [32]byte(symmetricKey)
	encrypted := secretbox.Seal(
		make([]byte, 0),
		unencrypted.Bin,
		&nonce,
		&symKeyArr)
	tcpOutChan <- common.ClientToClient{
		common.Encrypted{encrypted, nonce},
		unencrypted.Correspondent,
		publicSign}
	err = os.Remove(filePath)
	return err
}

func tcpServer() {
	stop := make(chan struct{})
	outboxWatcher, err := fsnotify.NewWatcher()
	outboxWatcher.Add(outboxPath())
	if err != nil {
		panic(err)
	}
	for {
		conn, err := makeConn()
		if err != nil {
			time.Sleep(time.Second)
			continue
		}
		go func() {
			for {
				select {
				case event := <-outboxWatcher.Events:
					if event.Op != fsnotify.Create {
						continue
					}
					_ = sendFileTcp(event.Name)
				}
			}
		}()
		func() {
			go func() {
				for {
					msg, err := common.ReadClientToClient(conn)
					if err != nil {
						stop <- struct{}{}
					}
					_ = processCToC(msg)
				}
			}()
			go func() {
				for {
					msg, err := common.EncodeClientToClient(
						<-tcpOutChan)
					if err != nil {
						continue
					}
					n, err := conn.Write(msg)
					if err != nil {
						stop <- struct{}{}
					}
					if n != len(msg) {
						stop <- struct{}{}
					}
				}
			}()
			<-stop
		}()
	}
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

var hereIsCode = [16]byte{223, 237, 163, 168, 233, 246, 38, 223, 196, 115, 89, 99, 226, 11, 157, 155}

func hashHereIsKey(h common.HereIsAnEncryptionKey) blake2bHash {
	result := make([]byte, 32+common.EncryptedKeyLen+24+16)
	i := 0
	for ; i < 32; i++ {
		result[i] = h.MyPublicEncrypt[i]
	}
	for ; i < 32+common.EncryptedKeyLen; i++ {
		result[i] = h.EncryptedSymmetricKey[i-32]
	}
	for ; i < 32+common.EncryptedKeyLen+24; i++ {
		result[i] = h.Nonce[i-32-common.EncryptedKeyLen]
	}
	for ; i < 32+common.EncryptedKeyLen+24+16; i++ {
		result[i] = hereIsCode[i-32-common.EncryptedKeyLen-24]
	}
	return blake2b.Sum256(result)
}

func sliceToSig(bs []byte) [common.SigSize]byte {
	var result [common.SigSize]byte
	for i, b := range bs {
		result[i] = b
	}
	return result
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

func makeSymmetricKey() ([32]byte, error) {
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

func signedAuthToSlice(bs []byte) [common.AuthSigSize]byte {
	var result [common.AuthSigSize]byte
	for i, b := range bs {
		result[i] = b
	}
	return result
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
		10,
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
	homeCode, err := genCode()
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

func masterPath() string { return dataDir + "/master.bwt" }

func httpLoadMaster(w http.ResponseWriter, r *http.Request) {
	f, err := os.Open(masterPath())
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	_, err = io.Copy(w, f)
	if err != nil {
		http.Error(w, err.Error(), 500)
	}
}

func httpSaveMaster(w http.ResponseWriter, r *http.Request) {
	f, err := os.Open(masterPath())
	defer f.Close()
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	_, err = io.Copy(f, r.Body)
	if err != nil {
		http.Error(w, err.Error(), 500)
	}
}

func main() {
	err := setup()
	if err != nil {
		fmt.Println(err)
		return
	}
	go tcpServer()
	go httpServer()
}

func encodePubID(pubID publicSignT) []byte {
	return []byte(base64.URLEncoding.EncodeToString(
		hashToSlice(pubID)))
}

func serveDoc(w http.ResponseWriter, filePath string) error {
	fmt.Println("Top of serveDoc.")
	fileHandle, err := os.Open(filePath)
	if err != nil {
		return err
	}
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

	err = os.Mkdir(dest, 0755)
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

func encKeyToSlice(key [common.EncryptedKeyLen]byte) []byte {
	result := make([]byte, common.EncryptedKeyLen)
	for i, b := range key {
		result[i] = b
	}
	return result
}

func encryptedKeyToArr(slice []byte) [common.EncryptedKeyLen]byte {
	var result [common.EncryptedKeyLen]byte
	for i, b := range slice {
		result[i] = b
	}
	return result
}

func httpGetApp(w http.ResponseWriter, r *http.Request) {
	securityCode := pat.Param(r, "pass")
	filename := pat.Param(r, "filename")
	if strEq(securityCode, homeCode) {
		err := serveDoc(w, "home/"+filename)
		if err != nil {
			http.Error(w, err.Error(), 500)
			return
		}
	}
	docHash, err := getDocHash(securityCode)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	filePath := fmt.Sprintf(
		"%s/%s/%s",
		tmpDir(),
		hashToStr(docHash),
		filename)
	err = serveDoc(w, filePath)
	if err != nil {
		http.Error(w, err.Error(), 500)
	}
}

func httpMakeApp(w http.ResponseWriter, r *http.Request) {
	appHash := pat.Param(r, "apphash")
	hashSlice, err := base64.URLEncoding.DecodeString(appHash)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
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
			http.Error(w, err.Error(), 400)
			return
		}
		err = browser.OpenURL(appURL(appCode))
		if err != nil {
			http.Error(w, err.Error(), 500)
		}
		return
	}
	err = unpackTarArchive(appPath, tmpPath)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	newCode, err := genCode()
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	err = browser.OpenURL(appURL(newCode))
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	appCodes[newCode] = hash
}

func sig(msg []byte) []byte {
	return sign.Sign(make([]byte, 0), msg, &secretSign)
}

func httpSaveApp(w http.ResponseWriter, r *http.Request) {
	err := writeAppToFile(r)
	if err != nil {
		http.Error(w, err.Error(), 500)
	}
}

func httpGetMyID(w http.ResponseWriter, r *http.Request) {
	w.Write(encodePubID(publicSign))
}

type handler func(w http.ResponseWriter, r *http.Request)

func p(f handler) handler {
	return func(w http.ResponseWriter, r *http.Request) {
		if !strEq(pat.Param(r, "pass"), homeCode) {
			http.Error(w, "bad security code", 400)
			return
		}
		f(w, r)
	}
}

func getInboxPtrs() (map[chunkPtrT]struct{}, error) {
	ptrs := make(map[chunkPtrT]struct{})
	files, err := ioutil.ReadDir(inboxPath())
	if err != nil {
		return ptrs, err
	}
	for _, f := range files {
		rawBin, err := ioutil.ReadFile(inboxPath() + "/" + f.Name())
		if err != nil {
			return ptrs, err
		}
		var ptr chunkPtrT
		ptr.Hash = f.Name()
		var buf bytes.Buffer
		n, err := buf.Write(rawBin)
		if err != nil {
			return ptrs, err
		}
		if n != len(rawBin) {
			err = errors.New("could not write plain.Bin to buffer")
			return ptrs, err
		}
		var chnk chunkT
		err = gob.NewDecoder(&buf).Decode(&chnk)
		if err != nil {
			return ptrs, err
		}
		ptr.CakeHash = chnk.CakeHash
		ptr.Counter = chnk.Counter
		ptr.Final = chnk.Final
		ptrs[ptr] = struct{}{}
	}
	return ptrs, nil
}

type ptrMap map[chunkPtrT]struct{}

func bunchPtrs(ptrs ptrMap) bunchMap {
	bunched := make(bunchMap)
	for ptr := range ptrs {
		bunch, ok := bunched[ptr.Hash]
		if ok {
			bunched[ptr.Hash] = append(bunch, ptr)
			continue
		}
		bunched[ptr.Hash] = []chunkPtrT{ptr}
	}
	return bunched
}

type bunchMap map[string][]chunkPtrT

func sortAndCheckBunchPtrs(m bunchMap) bunchMap {
	result := make(bunchMap)
	for hash, ptrs := range m {
		sorted := ptrs[:]
		sortBunchPtrs(sorted)
		err := checkBunchPtrs(sorted)
		if err != nil {
			continue
		}
		result[hash] = sorted
	}
	return result
}

func checkBunchPtrs(ptrs []chunkPtrT) error {
	lenPtrs := len(ptrs)
	if lenPtrs == 0 {
		return errors.New("no pointers in slice")
	}
	firstHash := ptrs[0].Hash
	for i, ptr := range ptrs {
		if i != ptr.Counter {
			return errors.New("bad counter")
		}
		if ptr.Hash != firstHash {
			return errors.New("bad hash")
		}
	}
	for _, ptr := range ptrs[:lenPtrs-1] {
		if ptr.Final {
			return errors.New("non-final pointer has 'Final' set")
		}
	}
	if !ptrs[lenPtrs].Final {
		return errors.New("final pointer doesn't have 'Final' set")
	}
	return nil
}

func sortBunchPtrs(ptrs []chunkPtrT) {
	f := func(i, j int) bool {
		return ptrs[i].Counter < ptrs[j].Counter
	}
	sort.Slice(ptrs, f)
}

func docsDir() string { return dataDir + "/docs" }

const maxTxtFileSize = 1e7

func httpPull(w http.ResponseWriter, r *http.Request) {
	rawPtrs, err := getInboxPtrs()
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	ptrs := sortAndCheckBunchPtrs(bunchPtrs(rawPtrs))
	for _, msgPtrs := range ptrs {
		_ = stitchChunks(msgPtrs)
	}
	files, err := ioutil.ReadDir(stitchedDir())
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	txtMsgs := make([]string, 0)
	txtFilePaths := make([]string, 0)
	for _, f := range files {
		fsize := f.Size()
		if fsize > maxTxtFileSize || fsize < txtMsgCodeLen {
			continue
		}
		path := stitchedDir() + "/" + f.Name()
		txtErr := func(msg string) {
			err = os.Remove(path)
			if err != nil {
				msg += "; " + err.Error()
			}
			http.Error(w, msg, 500)
		}
		h, err := os.Create(path)
		defer h.Close()
		if err != nil {
			txtErr(err.Error())
			return
		}
		codeCandidate := make([]byte, txtMsgCodeLen)
		n, err := h.Read(codeCandidate)
		if n != txtMsgCodeLen {
			txtErr("could not read text message code")
			return
		}
		txtCodeArr := txtCodeSliceToArr(codeCandidate)
		if !equalTxtCodes(txtCodeArr, txtMsgCode) {
			continue
		}
		txtBytes, err := ioutil.ReadAll(h)
		if err != nil {
			txtErr("could not read text file")
			return
		}
		h.Close()
		if !utf8.Valid(txtBytes) {
			err := os.Remove(path)
			if err != nil {
				http.Error(w, err.Error(), 500)
				return
			}
			continue
		}
		txtMsgs = append(txtMsgs, string(txtBytes))
		txtFilePaths = append(txtFilePaths, path)
	}
	encodedTxt, err := json.Marshal(txtMsgs)
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	n, err := w.Write(encodedTxt)
	if n != len(encodedTxt) {
		http.Error(w, "could not send all messages", 500)
		return
	}
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	for _, txtFilePath := range txtFilePaths {
		err := os.Remove(txtFilePath)
		if err != nil {
			http.Error(w, err.Error(), 500)
		}
	}
	binFiles, err := ioutil.ReadDir(stitchedDir())
	if err != nil {
		http.Error(w, err.Error(), 500)
		return
	}
	for _, file := range binFiles {
		oldPath := stitchedDir() + "/" + file.Name()
		newPath := docsDir() + "/" + file.Name()
		err = os.Rename(oldPath, newPath)
		if err != nil {
			http.Error(w, err.Error(), 500)
			return
		}
	}
}

func txtCodeSliceToArr(xs []byte) [txtMsgCodeLen]byte {
	var result [txtMsgCodeLen]byte
	for i, x := range xs {
		result[i] = x
	}
	return result
}

func equalTxtCodes(as, bs [txtMsgCodeLen]byte) bool {
	for i, a := range as {
		if a != bs[i] {
			return false
		}
	}
	return true
}

func tmpDir() string { return dataDir + "/tmp" }

func addChunkToBin(
	ptr chunkPtrT, dest io.Writer, hasher io.Writer) error {

	chunkPath := inboxPath() + "/" + ptr.Hash
	chunkHandle, err := os.Create(chunkPath)
	defer chunkHandle.Close()
	if err != nil {
		return err
	}
	var msg plain
	err = gob.NewDecoder(chunkHandle).Decode(&msg)
	if err != nil {
		return err
	}
	lenBin := len(msg.Bin)
	n, err := hasher.Write(msg.Bin)
	if n != lenBin {
		return errors.New("could not write whole chunk to hasher")
	}
	if err != nil {
		return err
	}
	n, err = dest.Write(msg.Bin)
	if n != lenBin {
		return errors.New("could not write whole chunk to tmp")
	}
	if err != nil {
		return err
	}
	chunkHandle.Close()
	return nil
}

func stitchChunks(ptrs []chunkPtrT) error {
	hasher, err := blake2b.New256(nil)
	if err != nil {
		return err
	}
	tmpFileName, err := genCode()
	if err != nil {
		return err
	}
	tmpPath := tmpDir() + "/" + tmpFileName
	tmpDest, err := os.Create(tmpPath)
	defer tmpDest.Close()
	if err != nil {
		return err
	}
	for _, ptr := range ptrs {
		err = addChunkToBin(ptr, tmpDest, hasher)
		if err != nil {
			return err
		}
	}
	hash := base64.URLEncoding.EncodeToString(hasher.Sum(nil))
	if hash != ptrs[0].Hash {
		return errors.New("stitched app has bad hash")
	}
	err = os.Rename(tmpPath, stitchedDir() + "/" + hash)
	return err
}

func stitchedDir() string { return dataDir + "/stitched" }

type push struct {
	Txts []txtMsg
	Bins []binMsg
}

type binMsg struct {
	FileName  string
	Recipient string
}

type txtMsg struct {
	Msg       string
	Recipient string
}

func httpPushErr(r *http.Request) error {
	body, err := ioutil.ReadAll(r.Body)
	if err != nil {
		return err
	}
	var msgs push
	err = json.Unmarshal(body, &msgs)
	if err != nil {
		return err
	}
	for _, msg := range msgs.Txts {
		recipient, err := hashFromString(msg.Recipient)
		if err != nil {
			return err
		}
		msgReader := bytes.NewReader([]byte(msg.Msg))
		err = chunkAndSend(msgReader, recipient)
		if err != nil {
			return err
		}
	}
	for _, msg := range msgs.Bins {
		recipient, err := hashFromString(msg.Recipient)
		if err != nil {
			return err
		}
		handle, err := os.Open(msg.FileName)
		if err != nil {
			return err
		}
		err = chunkAndSend(handle, recipient)
		if err != nil {
			return err
		}
	}
	return nil
}

func httpPush(w http.ResponseWriter, r *http.Request) {
	err := httpPushErr(r)
	if err != nil {
		http.Error(w, err.Error(), 400)
	}
}

func chunkAndSend(r io.Reader, recipient publicSignT) error {
	for {
		chunk := make([]byte, common.ChunkContentSize)
		n, err := r.Read(chunk)
		if n == 0 {
			return errors.New("no bytes in reader")
		}
		if err != nil {
			return err
		}
		msg := plain{chunk[:n], recipient}
		var buf bytes.Buffer
		err = gob.NewEncoder(&buf).Encode(msg)
		if err != nil {
			return err
		}
		encoded := buf.Bytes()
		path := inboxPath() + "/" + hashToStr(blake2b.Sum256(encoded))
		err = ioutil.WriteFile(path, encoded, 0600)
		if err != nil {
			return err
		}
	}
	return nil
}

func httpServer() {
	mux := goji.NewMux()
	mux.HandleFunc(pat.Get("/getapp/:pass/:filename"), httpGetApp)
	mux.HandleFunc(pat.Get("/makeapp/:pass/:apphash"), p(httpMakeApp))
	mux.HandleFunc(pat.Get("/getmyid/:pass"), p(httpGetMyID))
	mux.HandleFunc(pat.Post("/saveapp/:pass"), p(httpSaveApp))
	mux.HandleFunc(pat.Post("/push/:pass"), p(httpPush))
	mux.HandleFunc(pat.Post("/pull/:pass"), p(httpPull))
	mux.HandleFunc(pat.Post("/savemaster/:pass"), p(httpSaveMaster))
	mux.HandleFunc(pat.Get("/loadmaster/:pass"), p(httpLoadMaster))
	http.ListenAndServe(":" + port, mux)
}

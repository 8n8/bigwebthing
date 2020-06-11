package main

import (
	"crypto/sha256"
	"crypto/sha512"
	"database/sql"
	"encoding/base64"
	"encoding/binary"
	"errors"
	"fmt"
	"github.com/gorilla/websocket"
	_ "github.com/mattn/go-sqlite3"
	"golang.org/x/crypto/nacl/sign"
	"io"
	"io/ioutil"
	"net/http"
	"os"
	"strconv"
)

const csp = "default-src 'none'; " +
	"script-src 'self'; " +
	"style-src 'self' 'unsafe-inline'; " +
	"img-src 'self'; " +
	"font-src 'self'; " +
	"connect-src 'self'; " +
	"report-uri http://localhost:3001/cspreport;"

func encodeInt(theInt int) []byte {
	// Most significant byte is the last one. It only works
	// for up to about 10^14, but this is enough for id numbers,
	// since I'm not too bothered about having more than that
	// number of users.
	result := make([]byte, 8)
	for i, _ := range result {
		result[i] = byte((theInt >> (i * 8)) & 0xFF)
	}
	return result
}

func intPower(base, power int) int {
	result := 1
	for i := 0; i < power; i++ {
		result = result * base
	}
	return result
}

func decodeInt(bs []byte) int {
	// Most significant byte should be the last one.
	result := 0
	for i, b := range bs {
		result += int(b) * intPower(256, i)
	}
	return result
}

type stateT struct {
	toChans        map[int]websocketChans
	fatalErr       error
	proofOfWork    proofOfWorkState
	friendlyNames  [][]byte
	members        map[int]struct{}
	authCodes      []int
	authUnique     int
	whitelists     map[int]map[int]struct{}
	encryptionKeys map[int][]byte
}

type proofOfWorkState struct {
	difficulty uint8
	unique     int
	unused     []int
}

const powDifficulty = 80

func initState() stateT {
	pow := proofOfWorkState{
		difficulty: powDifficulty,
		unique:     0,
		unused:     []int{},
	}
	return stateT{
		fatalErr:       nil,
		proofOfWork:    pow,
		friendlyNames:  [][]byte{},
		members:        make(map[int]struct{}),
		authCodes:      []int{},
		authUnique:     0,
		encryptionKeys: make(map[int][]byte),
	}
}

type outputT interface {
	io(chan inputT)
}

func initOutputs() []outputT {
	outputs := []outputT{startHttpServer{}, loadData{}}
	return outputs
}

const createMembers = `
	CREATE TABLE IF NOT EXISTS members (name INTEGER UNIQUE NOT NULL);`

const createFriendlyNames = `
	CREATE TABLE IF NOT EXISTS friendlynames (keys BLOB UNIQUE NOT NULL);`

const createWhitelist = `
	CREATE TABLE IF NOT EXISTS whitelist (owner INTEGER NOT NULL, sender INTEGER NOT NULL);`

func createDatabase() error {
	database, err := sql.Open("sqlite3", dbFileName)
	if err != nil {
		err = fmt.Errorf("could not open database: %v", err)
		return err
	}
	defer database.Close()
	for _, command := range []string{createMembers, createFriendlyNames, createWhitelist} {
		_, err = database.Exec(command)
		if err != nil {
			return err
		}
	}
	return nil
}

type loadData struct{}

func loadMembers(database *sql.DB) (map[int]struct{}, error) {
	rows, err := database.Query("SELECT name FROM members")
	members := make(map[int]struct{})
	if err != nil {
		return members, err
	}
	for rows.Next() {
		var member int
		rows.Scan(&member)
		members[member] = struct{}{}
	}
	return members, nil
}

func loadFriendlyNames(database *sql.DB) ([][]byte, error) {
	rows, err := database.Query("SELECT keys FROM friendlynames")
	var friendlyNames [][]byte
	if err != nil {
		err = fmt.Errorf("could not load friendlynames: %v", err)
		return friendlyNames, err
	}
	for rows.Next() {
		var name []byte
		rows.Scan(&name)
		friendlyNames = append(friendlyNames, name)
	}
	return friendlyNames, nil
}

func loadWhitelists(database *sql.DB) (map[int]map[int]struct{}, error) {
	rows, err := database.Query("SELECT owner, sender FROM whitelist")
	whitelists := make(map[int]map[int]struct{})
	if err != nil {
		return whitelists, err
	}
	for rows.Next() {
		var owner, sender int
		rows.Scan(&owner, &sender)
		whitelist, ok := whitelists[owner]
		if !ok {
			whitelist = make(map[int]struct{})
		}
		whitelist[sender] = struct{}{}
		whitelists[owner] = whitelist
	}
	return whitelists, nil
}

func loadInt(filename string) (int, error) {
	raw, err := ioutil.ReadFile(filename)
	if err != nil {
		return 0, err
	}
	if len(raw) != 8 {
		return 0, errors.New("proof of work counter is not 8 byte")
	}
	return decodeInt(raw), nil
}

type sendMessages struct {
	id  int
	chs websocketChans
}

func (s sendMessages) io(inputChan chan inputT) {
	messagesDir := userMessagePath(s.id)
	messageFileNames, err := ioutil.ReadDir(messagesDir)
	if err != nil {
		return
	}
	for _, fileInfo := range messageFileNames {
		filename := fileInfo.Name()
		filepath := messagesDir + "/" + filename

		message, err := ioutil.ReadFile(filepath)
		if err != nil {
			return
		}
		s.chs.good <- goodHttpResponse(message)
	}
}

func (loadData) io(inputChannel chan inputT) {
	err := createDatabase()
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}

	database, err := sql.Open("sqlite3", dbFileName)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	defer database.Close()

	members, err := loadMembers(database)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}

	friendlyNames, err := loadFriendlyNames(database)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}

	whitelists, err := loadWhitelists(database)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}

	powCounter, err := loadInt(uniquePowFileName)
	if err != nil {
		powCounter = 0
	}

	authUnique, err := loadInt(uniqueAuthFileName)
	if err != nil {
		authUnique = 0
	}

	loaded := loadedData{
		members:       members,
		friendlyNames: friendlyNames,
		powCounter:    powCounter,
		authUnique:    authUnique,
		whitelists:    whitelists,
	}
	inputChannel <- loaded
}

type loadedData struct {
	members       map[int]struct{}
	friendlyNames [][]byte
	powCounter    int
	authUnique    int
	whitelists    map[int]map[int]struct{}
}

func (l loadedData) update(state stateT) (stateT, []outputT) {
	state.members = l.members
	state.friendlyNames = l.friendlyNames
	state.proofOfWork.unique = l.powCounter
	state.authUnique = l.authUnique
	state.whitelists = l.whitelists
	return state, []outputT{}
}

type startHttpServer struct{}

type inputT interface {
	update(stateT) (stateT, []outputT)
}

func main() {
	state := initState()
	outputs := initOutputs()
	inputChannel := make(chan inputT)
	for state.fatalErr == nil {
		for _, output := range outputs {
			go output.io(inputChannel)
		}
		input := <-inputChannel
		state, outputs = input.update(state)
	}
	fmt.Println(state.fatalErr)
}

const maxBodyLength = 16000

type httpRequestT struct {
	body         []byte
	responseChan chan httpResponseT
}

type httpResponseT interface {
	respond(http.ResponseWriter)
}

func parseRequest(body []byte) parsedRequestT {
	if len(body) == 0 {
		return badRequest{"empty body", 400}
	}

	switch body[0] {
	case 1:
		return parseMakeFriendlyName(body)
	case 2:
		return parseLookupName(body)
	case 3:
		return getProofOfWorkInfoRequest{}
	case 4:
		return parseAddAMember(body)
	case 5:
		return parseRemoveAMember(body)
	case 6:
		return parseChangeKey(body)
	case 7:
		return getAuthCodeRequest{}
	case 8:
		return parseSendMessage(body)
	case 9:
		return parseDeleteMessage(body)
	case 10:
		return parseWhitelistSomeone(body)
	case 11:
		return parseUnwhitelistSomeone(body)
	case 12:
		return parseUploadEncryptionKey(body)
	}

	return badRequest{"bad route", 400}
}

type uploadEncryptionKeyRequest struct {
	Owner     int
	SignedKey []byte
}

type deleteMessageT struct {
	idToken     idTokenT
	messageHash []byte
}

func parseDeleteMessage(body []byte) parsedRequestT {
	if len(body) != 145 {
		return badRequest{"length of body is not 145", 400}
	}
	idToken := parseIdToken(body)
	return deleteMessageT{
		idToken:     idToken,
		messageHash: body[113:145],
	}
}

func (d deleteMessageT) updateOnRequest(state stateT, responseChan chan httpResponseT) (stateT, []outputT) {
	newAuthCodes, err := validToken(d.idToken, state.authCodes, state.friendlyNames)
	if err != nil {
		return state, []outputT{badResponse{"bad ID token: " + err.Error(), 400, responseChan}}
	}
	state.authCodes = newAuthCodes
	filename := base64.URLEncoding.EncodeToString(d.messageHash)
	filepath := userMessagePath(d.idToken.senderId) + "/" + filename
	return state, []outputT{deleteFileT(filepath)}
}

type deleteFileT string

func (d deleteFileT) io(inputChannel chan inputT) {
	os.Remove(string(d))
}

func parseUploadEncryptionKey(body []byte) parsedRequestT {
	if len(body) != 105 {
		return badRequest{"length of body is not 105", 400}
	}
	name := decodeInt(body[1:9])
	return uploadEncryptionKeyRequest{
		Owner:     name,
		SignedKey: body[9:],
	}
}

func (u uploadEncryptionKeyRequest) updateOnRequest(state stateT, responseChan chan httpResponseT) (stateT, []outputT) {

	if u.Owner >= len(state.friendlyNames) {
		return state, []outputT{badResponse{"who are you?", 400, responseChan}}
	}

	signingKey := state.friendlyNames[u.Owner][:32]
	var signingKeyArr [32]byte
	copy(signingKeyArr[:], signingKey)
	_, okSignature := sign.Open(make([]byte, 0), u.SignedKey, &signingKeyArr)
	if !okSignature {
		return state, []outputT{badResponse{"bad signature", 400, responseChan}}
	}
	_, keyExists := state.encryptionKeys[u.Owner]
	if keyExists {
		return state, []outputT{sendHttpResponse{responseChan, goodHttpResponse([]byte("key already uploaded"))}}
	}
	state.encryptionKeys[u.Owner] = u.SignedKey
	return state, []outputT{cacheNewEncryptionKey(u), sendHttpResponse{responseChan, goodHttpResponse(make([]byte, 0))}}
}

type cacheNewEncryptionKey uploadEncryptionKeyRequest

func (c cacheNewEncryptionKey) io(inputChannel chan inputT) {
	database, err := sql.Open("sqlite3", dbFileName)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	defer database.Close()
	statement, err := database.Prepare("INSERT INTO encryptionkeys (owner, key) VALUES (?, ?);")
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	_, err = statement.Exec(c.Owner, c.SignedKey)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
}

func parseUnwhitelistSomeone(body []byte) parsedRequestT {
	if len(body) != 145 {
		return badRequest{"length of body is not 145", 400}
	}
	idToken := parseIdToken(body)

	name := decodeInt(body[137:])
	return unwhitelistRequest{
		Name:    name,
		IdToken: idToken,
	}
}

type unwhitelistRequest struct {
	Name    int
	IdToken idTokenT
}

func (u unwhitelistRequest) updateOnRequest(state stateT, responseChan chan httpResponseT) (stateT, []outputT) {
	bad := func(message string, code int) []outputT {
		return []outputT{badResponse{message, code, responseChan}}
	}
	newAuthCodes, err := validToken(u.IdToken, state.authCodes, state.friendlyNames)
	if err != nil {
		return state, bad("bad ID token: "+err.Error(), 400)
	}
	state.authCodes = newAuthCodes

	whitelist, ok := state.whitelists[u.IdToken.senderId]
	if !ok {
		return state, bad("already not whitelisted", 400)
	}
	_, ok = whitelist[u.Name]
	if !ok {
		return state, bad("already not whitelisted", 400)
	}
	delete(whitelist, u.Name)
	state.whitelists[u.IdToken.senderId] = whitelist
	response := deleteWhitelistee{
		Owner:    u.IdToken.senderId,
		ToRemove: u.Name,
		Channel:  responseChan,
	}
	return state, []outputT{response}
}

type deleteWhitelistee struct {
	Owner    int
	ToRemove int
	Channel  chan httpResponseT
}

const removeFromWhitelist = `
	DELETE FROM whitelist WHERE owner=? AND sender=?;`

func (d deleteWhitelistee) io(inputChannel chan inputT) {
	database, err := sql.Open("sqlite3", dbFileName)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	defer database.Close()
	_, err = database.Exec(removeFromWhitelist, d.Owner, d.ToRemove)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	d.Channel <- goodHttpResponse([]byte{})
}

func parseWhitelistSomeone(body []byte) parsedRequestT {
	if len(body) != 137 {
		return badRequest{"length of body is not 137", 400}
	}
	idToken := parseIdToken(body)

	proofOfWork := proofOfWorkT{
		Server: body[113:121],
		Client: body[121:129],
	}
	name := decodeInt(body[129:])
	return whitelistRequest{
		ProofOfWork: proofOfWork,
		Name:        name,
		IdToken:     idToken,
	}
}

type whitelistRequest struct {
	ProofOfWork proofOfWorkT
	Name        int
	IdToken     idTokenT
}

func (w whitelistRequest) updateOnRequest(state stateT, responseChan chan httpResponseT) (stateT, []outputT) {
	bad := func(message string, code int) []outputT {
		return []outputT{badResponse{message, code, responseChan}}
	}
	newPow, ok := checkProofOfWork(w.ProofOfWork, state.proofOfWork)
	if !ok {
		return state, bad("bad proof of work", 400)
	}
	state.proofOfWork = newPow

	newAuthCodes, err := validToken(w.IdToken, state.authCodes, state.friendlyNames)
	if err != nil {
		return state, bad("bad ID token: "+err.Error(), 400)
	}
	state.authCodes = newAuthCodes

	if w.Name >= len(state.friendlyNames) {
		return state, bad("unknown invitee", 400)
	}

	whitelist, ok := state.whitelists[w.IdToken.senderId]
	if !ok {
		whitelist = make(map[int]struct{})
	}

	whitelist[w.Name] = struct{}{}
	state.whitelists[w.IdToken.senderId] = whitelist
	response := addToWhitelist{
		Owner:   w.IdToken.senderId,
		ToAdd:   w.Name,
		Channel: responseChan,
	}
	return state, []outputT{response}
}

type addToWhitelist struct {
	Owner   int
	ToAdd   int
	Channel chan httpResponseT
}

const cacheNewWhiteList = `
	INSERT INTO whitelist (owner, sender) VALUES (?, ?);`

func (a addToWhitelist) io(inputChannel chan inputT) {
	database, err := sql.Open("sqlite3", dbFileName)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	defer database.Close()
	_, err = database.Exec(cacheNewWhiteList, a.Owner, a.ToAdd)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	a.Channel <- goodHttpResponse([]byte{})
}

func parseRemoveAMember(body []byte) parsedRequestT {
	if len(body) < 138 {
		return badRequest{"length of body is less than 138", 400}
	}
	idToken := parseIdToken(body)
	memberToRemove := decodeInt(body[137:])
	if memberToRemove == 0 {
		return badRequest{"can't delete admin member", 400}
	}
	return removeMemberRequest{idToken, memberToRemove}
}

type removeMemberRequest struct {
	idToken idTokenT
	member  int
}

func (r removeMemberRequest) updateOnRequest(state stateT, httpResponseChan chan httpResponseT) (stateT, []outputT) {
	newAuthCodes, err := validToken(r.idToken, state.authCodes, state.friendlyNames)
	if err != nil {
		return state, []outputT{badResponse{"bad ID token: " + err.Error(), 400, httpResponseChan}}
	}
	state.authCodes = newAuthCodes
	if r.idToken.senderId != 0 {
		return state, []outputT{badResponse{"unauthorised", 401, httpResponseChan}}
	}
	delete(state.members, r.member)
	return state, []outputT{deleteMember{r.member, httpResponseChan}}
}

type deleteMember struct {
	member  int
	channel chan httpResponseT
}

func (d deleteMember) io(inputChannel chan inputT) {
	database, err := sql.Open("sqlite3", dbFileName)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	defer database.Close()
	statement, err := database.Prepare("DELETE FROM members WHERE name=?")
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	_, err = statement.Exec(int(d.member))
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	d.channel <- goodHttpResponse([]byte{})
}

func parseChangeKey(body []byte) parsedRequestT {
	if len(body) != 169 {
		return badRequest{"length of body is not 169", 400}
	}
	idToken := parseIdToken(body)
	newKey := body[137:]
	return changeKeyRequest{idToken, newKey}
}

type changeKeyRequest struct {
	idToken idTokenT
	newKey  []byte
}

func (c changeKeyRequest) updateOnRequest(state stateT, responseChan chan httpResponseT) (stateT, []outputT) {
	newAuthCodes, err := validToken(c.idToken, state.authCodes, state.friendlyNames)
	if err != nil {
		return state, []outputT{badResponse{"bad ID token: " + err.Error(), 400, responseChan}}
	}
	state.authCodes = newAuthCodes
	state.friendlyNames[c.idToken.senderId] = c.newKey
	return state, []outputT{cacheNewKey{c.newKey, responseChan, c.idToken.senderId}}
}

type cacheNewKey struct {
	newKey   []byte
	channel  chan httpResponseT
	senderId int
}

func (c cacheNewKey) io(inputChannel chan inputT) {
	database, err := sql.Open("sqlite3", dbFileName)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	defer database.Close()
	statement, err := database.Prepare("UPDATE friendlynames SET key=? WHERE rowid=?")
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	_, err = statement.Exec(c.newKey, c.senderId)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	c.channel <- goodHttpResponse([]byte{})
}

type getAuthCodeRequest struct{}

func (getAuthCodeRequest) updateOnRequest(state stateT, httpResponseChan chan httpResponseT) (stateT, []outputT) {
	state.authUnique++
	encoded := encodeInt(state.authUnique)
	outputs := []outputT{
		cacheNewAuthUnique(encoded),
		sendAuthCode{encoded, httpResponseChan}}
	state.authCodes = append(state.authCodes, state.authUnique)
	return state, outputs
}

type sendAuthCode struct {
	code    []byte
	channel chan httpResponseT
}

type cacheNewAuthUnique []byte

const uniqueAuthFileName = dataDir + "/uniqueAuthCounter"

func (c cacheNewAuthUnique) io(inputChannel chan inputT) {
	err := ioutil.WriteFile(uniqueAuthFileName, c, 0644)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
}

func (s sendAuthCode) io(inputChannel chan inputT) {
	s.channel <- goodHttpResponse(s.code)
}

func parseSendMessage(body []byte) parsedRequestT {
	if len(body) < 122 {
		return badRequest{"body less than 122 bytes", 400}
	}
	idToken := parseIdToken(body)
	recipient := decodeInt(body[113:121])
	return sendMessageRequest{idToken, recipient, body}
}

type sendMessageRequest struct {
	idToken   idTokenT
	recipient int
	message   []byte
}

func (s sendMessageRequest) updateOnRequest(state stateT, responseChan chan httpResponseT) (stateT, []outputT) {
	bad := func(message string, code int) []outputT {
		return []outputT{badResponse{message, code, responseChan}}
	}
	newAuthCodes, err := validToken(s.idToken, state.authCodes, state.friendlyNames)
	if err != nil {
		return state, bad("bad ID token: "+err.Error(), 400)
	}
	state.authCodes = newAuthCodes
	_, senderIsMember := state.members[s.idToken.senderId]
	if s.recipient >= len(state.friendlyNames) {
		return state, bad("unknown recipient", 400)
	}
	recipientWhitelist, ok := state.whitelists[s.recipient]
	if !ok {
		recipientWhitelist = make(map[int]struct{})
	}
	_, ok = recipientWhitelist[s.idToken.senderId]
	if !ok {
		return state, []outputT{hiddenError(responseChan)}
	}
	_, recipientIsMember := state.members[s.recipient]
	if (!senderIsMember) && (!recipientIsMember) {
		return state, bad("neither sender nor recipient are members", 400)
	}
	recipientChannels, recipientConnected := state.toChans[s.recipient]
	return state, []outputT{sendMessage{
		recipient:          s.recipient,
		message:            s.message,
		channel:            responseChan,
		recipientConnected: recipientConnected,
		recipientChannel:   recipientChannels.good}}
}

type hiddenError chan httpResponseT

func (h hiddenError) io(inputChannel chan inputT) {
	h <- goodHttpResponse([]byte{})
}

type sendMessage struct {
	recipient          int
	message            []byte
	channel            chan httpResponseT
	recipientConnected bool
	recipientChannel   chan []byte
}

func (s sendMessage) io(inputChannel chan inputT) {
	messagesDir := userMessagePath(s.recipient)
	err := os.MkdirAll(messagesDir, 0755)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	hash := sha256.Sum256(s.message)
	filename := base64.URLEncoding.EncodeToString(hash[:])
	filepath := messagesDir + "/" + filename
	err = ioutil.WriteFile(filepath, s.message, 0644)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	s.channel <- goodHttpResponse([]byte{})
	if s.recipientConnected {
		s.recipientChannel <- s.message
	}
}

const inboxesDir = dataDir + "/inboxes"

func userMessagePath(name int) string {
	nameString := strconv.FormatInt(int64(name), 10)
	return inboxesDir + "/" + nameString
}

type addAMemberRequest struct {
	idToken idTokenT
	name    int
}

func equalBytes(k1 []byte, k2 []byte) bool {
	if len(k1) != len(k2) {
		return false
	}
	for i, k := range k1 {
		if k != k2[i] {
			return false
		}
	}
	return true
}

func validToken(token idTokenT, unused []int, friendlyNames [][]byte) ([]int, error) {
	if token.senderId >= len(friendlyNames) {
		return unused, errors.New("unknown sender")
	}
	senderKey := friendlyNames[token.senderId][:32]
	var keyArr [32]byte
	copy(keyArr[:], senderKey)
	signed, ok := sign.Open([]byte{}, token.signature, &keyArr)
	if !ok {
		return unused, errors.New("bad signature")
	}
	if !equalBytes(signed, token.messageHash) {
		return unused, errors.New("bad signature hash")
	}
	if !isAmember(token.authCode, unused) {
		return unused, errors.New("bad auth code")
	}
	newUnused := removeItem(unused, token.authCode)
	return newUnused, nil
}

func (a addAMemberRequest) updateOnRequest(state stateT, responseChan chan httpResponseT) (stateT, []outputT) {
	newAuthCodes, err := validToken(a.idToken, state.authCodes, state.friendlyNames)
	if err != nil {
		return state, []outputT{badResponse{"bad ID token :" + err.Error(), 400, responseChan}}
	}
	state.authCodes = newAuthCodes
	if len(state.friendlyNames) == 0 {
		state.fatalErr = errors.New("no \"admin\" user")
		return state, []outputT{}
	}
	if a.idToken.senderId != 0 {
		return state, []outputT{badResponse{"unauthorised", 401, responseChan}}
	}
	state.members[a.name] = struct{}{}
	response := sendHttpResponse{
		channel:  responseChan,
		response: goodHttpResponse([]byte{}),
	}
	outputs := []outputT{cacheNewMember(a.name), response}
	return state, outputsT(outputs)
}

type cacheNewMember string

func (c cacheNewMember) io(inputChannel chan inputT) {
	database, err := sql.Open("sqlite3", dbFileName)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	defer database.Close()
	statement, err := database.Prepare("INSERT INTO members (member) VALUES (?);")
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	_, err = statement.Exec(c)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
}

type idTokenT struct {
	senderId    int
	authCode    int
	signature   []byte
	messageHash []byte
}

func safeCombine(route byte, message, authBytes []byte) []byte {
	messageLength := len(message)
	result := make([]byte, messageLength+9)
	result[0] = route
	for i := 1; i < messageLength+1; i++ {
		result[i] = message[i-1]
	}
	for i := 1 + messageLength; i < messageLength+9; i++ {
		result[i] = authBytes[i-1-messageLength]
	}
	return result
}

func parseIdToken(body []byte) idTokenT {
	authBytes := body[9:17]
	powAndWhitelistee := body[113:]
	authCode := decodeInt(authBytes)
	message := safeCombine(body[0], powAndWhitelistee, authBytes)
	hash := sha512.Sum512(message)
	hashSlice := hash[:32]
	senderId := decodeInt(body[1:9])
	return idTokenT{
		senderId:    senderId,
		authCode:    authCode,
		signature:   body[17:113],
		messageHash: hashSlice,
	}
}

func parseAddAMember(body []byte) parsedRequestT {
	if len(body) < 138 {
		return badRequest{"body less than 138 bytes", 400}
	}
	idToken := parseIdToken(body)
	nameToAdd := decodeInt(body[137:])
	return addAMemberRequest{
		idToken: idToken,
		name:    nameToAdd,
	}
}

type getProofOfWorkInfoRequest struct{}

type outputsT []outputT

type cachePowUniqueT int

const uniquePowFileName = dataDir + "/uniqueProofOfWorkCounter"

func (c cachePowUniqueT) io(inputChannel chan inputT) {
	err := ioutil.WriteFile(uniquePowFileName, encodeInt(int(c)), 0644)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
}

func trim(unused []int) []int {
	length := len(unused)
	const maxUnusedPowCodesToKeep = 5000
	tooMuch := length - maxUnusedPowCodesToKeep
	if tooMuch > 0 {
		return unused[tooMuch:]
	}
	return unused
}

func (getProofOfWorkInfoRequest) updateOnRequest(state stateT, httpResponseChan chan httpResponseT) (stateT, []outputT) {
	response := goodHttpResponse(append([]byte{state.proofOfWork.difficulty}, encodeInt(state.proofOfWork.unique)...))
	sendResponse := sendHttpResponse{
		channel:  httpResponseChan,
		response: response,
	}
	cachePow := cachePowUniqueT(state.proofOfWork.unique + 1)
	outputs := []outputT{sendResponse, cachePow}
	state.proofOfWork.unused = append(state.proofOfWork.unused, state.proofOfWork.unique)
	state.proofOfWork.unique += 1
	state.proofOfWork.unused = trim(state.proofOfWork.unused)
	return state, outputsT(outputs)
}

func (g goodHttpResponse) respond(w http.ResponseWriter) {
	w.Write([]byte(g))
}

type parsedRequestT interface {
	updateOnRequest(stateT, chan httpResponseT) (stateT, []outputT)
}

type badRequest struct {
	message string
	code    int
}

type sendHttpResponse struct {
	channel  chan httpResponseT
	response httpResponseT
}

func (s sendHttpResponse) io(inputChan chan inputT) {
	s.channel <- s.response
}

type goodHttpResponse []byte

func (b badRequest) updateOnRequest(state stateT, responseChan chan httpResponseT) (stateT, []outputT) {
	response := sendHttpResponse{
		channel:  responseChan,
		response: b,
	}
	return state, []outputT{response}
}

func (b badRequest) respond(w http.ResponseWriter) {
	http.Error(w, b.message, b.code)
}

type lookupNameT int

func (name lookupNameT) updateOnRequest(state stateT, httpResponseChan chan httpResponseT) (stateT, []outputT) {
	if len(state.friendlyNames) <= int(name) {
		return state, []outputT{badResponse{"unknown name", 400, httpResponseChan}}
	}
	keys := state.friendlyNames[int(name)]
	return state, []outputT{nameLookupResponse{keys, httpResponseChan}}
}

type nameLookupResponse struct {
	key     []byte
	channel chan httpResponseT
}

func (n nameLookupResponse) io(inputChannel chan inputT) {
	n.channel <- goodHttpResponse(n.key)
}

func parseLookupName(body []byte) parsedRequestT {
	name, _ := binary.Uvarint(body[1:])
	return lookupNameT(name)
}

type proofOfWorkT struct {
	Server []byte
	Client []byte
}

type makeFriendlyNameRequest struct {
	ProofOfWork proofOfWorkT
	NewKey      []byte
}

func removeItem(items []int, item int) []int {
	newItems := make([]int, 0, len(items))
	for _, oldItem := range items {
		if oldItem == item {
			continue
		}
		newItems = append(newItems, oldItem)
	}
	return newItems
}

func firstXareOk(bs []byte) bool {
	for i := 0; i < 32; i++ {
		if bs[i] < powDifficulty {
			return false
		}
	}
	return true
}

func isAmember(candidate int, of []int) bool {
	for _, o := range of {
		if candidate == o {
			return true
		}
	}
	return false
}

func keyExists(key []byte, keys [][]byte) bool {
	for _, k := range keys {
		if equalBytes(k, key) {
			return true
		}
	}
	return false
}

func checkProofOfWork(pow proofOfWorkT, s proofOfWorkState) (proofOfWorkState, bool) {
	serverCandidate := decodeInt(pow.Server)
	if !isAmember(serverCandidate, s.unused) {
		return s, false
	}
	hash := sha512.Sum512(append(pow.Server, pow.Client...))
	if !firstXareOk(hash[0:32]) {
		return s, false
	}
	s.unused = removeItem(s.unused, serverCandidate)
	return s, true
}

func (m makeFriendlyNameRequest) updateOnRequest(state stateT, httpResponseChan chan httpResponseT) (stateT, []outputT) {
	newPow, ok := checkProofOfWork(m.ProofOfWork, state.proofOfWork)
	if !ok {
		return state, []outputT{badResponse{"bad proof of work", 400, httpResponseChan}}
	}

	if keyExists(m.NewKey, state.friendlyNames) {
		return state, []outputT{badResponse{"key already used", 400, httpResponseChan}}
	}

	state.friendlyNames = append(state.friendlyNames, m.NewKey)
	state.proofOfWork = newPow
	return state, []outputT{cacheNewKeyT{httpResponseChan, m.NewKey}}
}

type cacheNewKeyT struct {
	channel chan httpResponseT
	key     []byte
}

type fatalError struct {
	err error
}

func (err fatalError) update(state stateT) (stateT, []outputT) {
	state.fatalErr = err.err
	return state, []outputT{}
}

const dataDir = "data"

const dbFileName = dataDir + "/database.db"

func (c cacheNewKeyT) io(inputChannel chan inputT) {
	database, err := sql.Open("sqlite3", dbFileName)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	defer database.Close()
	statement, err := database.Prepare("INSERT INTO friendlynames (key) VALUES (?);")
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	result, err := statement.Exec(c.key)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	username, err := result.LastInsertId()
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	// The -1 is because the database rowid starts from 1, not 0.
	c.channel <- newUsernameT(username - 1)
}

type newUsernameT int

func (n newUsernameT) respond(w http.ResponseWriter) {
	w.Write(encodeInt(int(n)))
}

type badResponse struct {
	message string
	code    int
	channel chan httpResponseT
}

func (b badResponse) io(inputChannel chan inputT) {
	b.channel <- badRequest{b.message, b.code}
}

func parseMakeFriendlyName(body []byte) parsedRequestT {
	if len(body) != 81 {
		return badRequest{"body is not 81 bytes", 400}
	}

	proofOfWork := proofOfWorkT{
		Server: body[1:9],
		Client: body[9:17],
	}

	return makeFriendlyNameRequest{
		ProofOfWork: proofOfWork,
		NewKey:      body[17:81],
	}
}

func (h httpRequestT) update(state stateT) (stateT, []outputT) {
	return parseRequest(h.body).updateOnRequest(state, h.responseChan)
}

func httpApiHandler(inputChan chan inputT, w http.ResponseWriter, r *http.Request) {
	readCloser := http.MaxBytesReader(w, r.Body, maxBodyLength)
	body, err := ioutil.ReadAll(readCloser)
	if err != nil {
		http.Error(w, err.Error(), 400)
		return
	}
	responseChan := make(chan httpResponseT)
	inputChan <- httpRequestT{
		body:         body,
		responseChan: responseChan,
	}
	response := <-responseChan
	response.respond(w)
}

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func websocketsHandler(
	inputChan chan inputT, w http.ResponseWriter, r *http.Request) {

	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		return
	}
	conn.SetReadLimit(16000)

	_, maybeIdToken, err := conn.ReadMessage()
	if err != nil {
		return
	}
	chans := websocketChans{
		good: make(chan []byte),
		kill: make(chan struct{}),
	}
	inputChan <- websocketIdToken{
		maybeIdToken: maybeIdToken,
		chans:        chans,
	}

	go func() {
		for {
			err := conn.WriteMessage(
				websocket.BinaryMessage, <-chans.good)
			if err != nil {
				return
			}
		}
	}()

	<-chans.kill
}

type websocketChans struct {
	good chan []byte
	kill chan struct{}
}

type websocketIdToken struct {
	chans        websocketChans
	maybeIdToken []byte
}

func (w websocketIdToken) update(state stateT) (stateT, []outputT) {
	if len(w.maybeIdToken) != 112 {
		return state, []outputT{badWebsocketAuth(w.chans.kill)}
	}
	idToken := parseIdToken(w.maybeIdToken)
	newAuthCodes, err := validToken(
		idToken, state.authCodes, state.friendlyNames)
	if err != nil {
		return state, []outputT{badWebsocketAuth(w.chans.kill)}
	}
	state.authCodes = newAuthCodes
	state.toChans[idToken.senderId] = w.chans
	sender := sendMessages{
		id:  idToken.senderId,
		chs: w.chans,
	}
	return state, []outputT{sender}
}

type badWebsocketAuth chan struct{}

func (b badWebsocketAuth) io(inputChan chan inputT) {
	b <- struct{}{}
}

func (startHttpServer) io(inputChan chan inputT) {
	http.HandleFunc(
		"/downloadmessages",
		func(w http.ResponseWriter, r *http.Request) {
			websocketsHandler(inputChan, w, r)
		})
	http.HandleFunc(
		"/api",
		func(w http.ResponseWriter, r *http.Request) {
			httpApiHandler(inputChan, w, r)
		})
	http.HandleFunc(
		"/",
		func(w http.ResponseWriter, r *http.Request) {
			// Turned off in development, because webpack uses eval.
			// w.Header().Add("Content-Security-Policy", csp)
			w.Header().Add("Cache-Control", "no-cache")
			handle, err := os.Open("index.html")
			if err != nil {
				http.Error(w, err.Error(), 500)
				return
			}
			_, err = io.Copy(w, handle)
			if err != nil {
				http.Error(w, err.Error(), 500)
			}
		})
	http.HandleFunc(
		"/cspreport",
		func(w http.ResponseWriter, r *http.Request) {
			body, _ := ioutil.ReadAll(r.Body)
			fmt.Println(string(body))
		})
	http.Handle("/static/", http.FileServer(http.Dir("")))
	serveFile("favicon.ico", "image/ico")
	fmt.Println(http.ListenAndServe(":3001", nil))
}

func serveFile(filename, contentType string) {
	http.HandleFunc(
		"/"+filename,
		func(w http.ResponseWriter, r *http.Request) {
			handle, err := os.Open(filename)
			if err != nil {
				http.Error(w, err.Error(), 500)
				return
			}
			io.Copy(w, handle)
			w.Header().Add("Content-Type", contentType)
		})
}

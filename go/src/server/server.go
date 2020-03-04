package main

import (
	"crypto/sha256"
	"database/sql"
	"encoding/base64"
	"encoding/binary"
	"errors"
	"fmt"
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
	"report-uri http://localhost:3001/cspreport;"

type stateT struct {
	fatalErr      error
	proofOfWork   proofOfWorkState
	friendlyNames [][]byte
	members       map[uint64]struct{}
	authCodes     []uint64
	authUnique    uint64
}

type proofOfWorkState struct {
	difficulty uint8
	unique     uint64
	unused     []uint64
}

func initState() stateT {
	pow := proofOfWorkState{
		difficulty: 15,
		unique:     0,
		unused:     []uint64{},
	}
	return stateT{
		fatalErr:      nil,
		proofOfWork:   pow,
		friendlyNames: [][]byte{},
		members:       make(map[uint64]struct{}),
		authCodes:     []uint64{},
		authUnique:    0,
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
	CREATE TABLE IF NOT EXISTS friendlynames (key BLOB UNIQUE NOT NULL);`

func createDatabase() error {
	database, err := sql.Open("sqlite3", dbFileName)
	if err != nil {
		err = fmt.Errorf("could not open database: %v", err)
		return err
	}
	defer database.Close()
	_, err = database.Exec(createMembers)
	if err != nil {
		return err
	}
	_, err = database.Exec(createFriendlyNames)
	if err != nil {
		return err
	}
	return nil
}

type loadData struct{}

func loadMembers(database *sql.DB) (map[uint64]struct{}, error) {
	rows, err := database.Query("SELECT name FROM members")
	members := make(map[uint64]struct{})
	if err != nil {
		return members, err
	}
	for rows.Next() {
		var member uint64
		rows.Scan(&member)
		members[member] = struct{}{}
	}
	return members, nil
}

func loadFriendlyNames(database *sql.DB) ([][]byte, error) {
	rows, err := database.Query("SELECT key FROM friendlynames")
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

func loadUint64(filename string) (uint64, error) {
	raw, err := ioutil.ReadFile(filename)
	if err != nil {
		return 0, err
	}
	if len(raw) != 8 {
		return 0, errors.New("proof of work counter is not 8 byte")
	}
	asInt, n := binary.Uvarint(raw)
	if n != 8 {
		return 0, errors.New("did not read all of proof of work counter")
	}
	return asInt, nil
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

	powCounter, err := loadUint64(uniquePowFileName)
	if err != nil {
		powCounter = 0
	}

	authUnique, err := loadUint64(uniqueAuthFileName)
	if err != nil {
		authUnique = 0
	}

	loaded := loadedData{
		members:       members,
		friendlyNames: friendlyNames,
		powCounter:    powCounter,
		authUnique:    authUnique,
	}
	inputChannel <- loaded
}

type loadedData struct {
	members       map[uint64]struct{}
	friendlyNames [][]byte
	powCounter    uint64
	authUnique    uint64
}

func (l loadedData) update(state stateT) (stateT, []outputT) {
	state.members = l.members
	state.friendlyNames = l.friendlyNames
	state.proofOfWork.unique = l.powCounter
	state.authUnique = l.authUnique
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
		return parseRetrieveMessage(body)
	}

	return badRequest{"bad route", 400}
}

func parseRemoveAMember(body []byte) parsedRequestT {
	if len(body) < 138 {
		return badRequest{"length of body is less than 138", 400}
	}
	idToken := parseIdToken(body)
	memberToRemove, _ := binary.Uvarint(body[137:])
	if memberToRemove == 0 {
		return badRequest{"can't delete admin member", 400}
	}
	return removeMemberRequest{idToken, memberToRemove}
}

type removeMemberRequest struct {
	idToken idTokenT
	member  uint64
}

func (r removeMemberRequest) updateOnRequest(state stateT, httpResponseChan chan httpResponseT) (stateT, []outputT) {
	newAuthCodes, ok := validToken(r.idToken, state.authCodes)
	if !ok {
		return state, []outputT{badResponse{"bad ID token", 400, httpResponseChan}}
	}
	state.authCodes = newAuthCodes
	adminUser := state.friendlyNames[0]
	if !equalBytes(r.idToken.publicSignKey, adminUser) {
		return state, []outputT{badResponse{"unauthorised", 401, httpResponseChan}}
	}
	delete(state.members, r.member)
	return state, []outputT{deleteMember{r.member, httpResponseChan}}
}

type deleteMember struct {
	member  uint64
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
	newAuthCodes, ok := validToken(c.idToken, state.authCodes)
	if !ok {
		return state, []outputT{badResponse{"bad ID token", 400, responseChan}}
	}
	state.authCodes = newAuthCodes
	senderId, ok := getMemberId(c.idToken.publicSignKey, state.friendlyNames)
	if !ok {
		return state, []outputT{badResponse{"unknown sender", 400, responseChan}}
	}
	state.friendlyNames[senderId] = c.newKey
	return state, []outputT{cacheNewKey{c.newKey, responseChan, senderId}}
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
	buf := make([]byte, 8)
	_ = binary.PutUvarint(buf, state.authUnique)
	outputs := []outputT{
		cacheNewAuthUnique{buf, httpResponseChan},
		sendAuthCode{buf, httpResponseChan}}
	return state, outputs
}

type sendAuthCode struct {
	code    []byte
	channel chan httpResponseT
}

type cacheNewAuthUnique struct {
	auth    []byte
	channel chan httpResponseT
}

const uniqueAuthFileName = dataDir + "/uniqueAuthCounter"

func (c cacheNewAuthUnique) io(inputChannel chan inputT) {
	err := ioutil.WriteFile(uniqueAuthFileName, c.auth, 0644)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	c.channel <- goodHttpResponse([]byte{})
}

func (s sendAuthCode) io(inputChannel chan inputT) {
	s.channel <- goodHttpResponse(s.code)
}

func parseSendMessage(body []byte) parsedRequestT {
	if len(body) < 178 {
		return badRequest{"body less than 178 bytes", 400}
	}
	idToken := parseIdToken(body)
	recipient := body[137:169]
	return sendMessageRequest{idToken, recipient, body}
}

type sendMessageRequest struct {
	idToken   idTokenT
	recipient []byte
	message   []byte
}

func getMemberId(key []byte, members [][]byte) (int, bool) {
	for i, member := range members {
		if equalBytes(member, key) {
			return i, true
		}
	}
	return 0, false
}

func (s sendMessageRequest) updateOnRequest(state stateT, responseChan chan httpResponseT) (stateT, []outputT) {
	bad := func(message string, code int) []outputT {
		return []outputT{badResponse{message, code, responseChan}}
	}
	newAuthCodes, ok := validToken(s.idToken, state.authCodes)
	if !ok {
		return state, bad("bad ID token", 400)
	}
	state.authCodes = newAuthCodes
	senderId, ok := getMemberId(s.idToken.publicSignKey, state.friendlyNames)
	if !ok {
		return state, bad("unknown sender", 400)
	}
	_, senderIsMember := state.members[uint64(senderId)]
	recipientId, ok := getMemberId(s.recipient, state.friendlyNames)
	if !ok {
		return state, bad("unknown recipient", 400)
	}
	_, recipientIsMember := state.members[uint64(recipientId)]
	if (!senderIsMember) && (!recipientIsMember) {
		return state, bad("neither sender nor recipient are members", 400)
	}
	return state, []outputT{sendMessage{recipientId, s.message, responseChan}}
}

type sendMessage struct {
	recipient int
	message   []byte
	channel   chan httpResponseT
}

func (s sendMessage) io(inputChannel chan inputT) {
	messagesDir := userMessagePath(int64(s.recipient))
	err := os.MkdirAll(messagesDir, os.ModeDir)
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
}

func parseRetrieveMessage(body []byte) parsedRequestT {
	if len(body) != 137 {
		return badRequest{"body not 137 bytes", 400}
	}
	idToken := parseIdToken(body)
	return retrieveMessageRequest(idToken)
}

type retrieveMessageRequest idTokenT

func (r retrieveMessageRequest) updateOnRequest(state stateT, responseChan chan httpResponseT) (stateT, []outputT) {
	token := idTokenT(r)
	newAuthCodes, ok := validToken(token, state.authCodes)
	if !ok {
		return state, []outputT{badResponse{"bad ID token", 400, responseChan}}
	}
	state.authCodes = newAuthCodes
	return state, []outputT{collectMessage{token.publicSignKey, responseChan}}
}

type collectMessage struct {
	addressee []byte
	channel   chan httpResponseT
}

const inboxesDir = "inboxes"

func userMessagePath(name int64) string {
	nameString := strconv.FormatInt(name, 10)
	return inboxesDir + "/" + nameString
}

func (c collectMessage) io(inputChannel chan inputT) {
	database, err := sql.Open("sqlite3", dbFileName)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	defer database.Close()
	rows, err := database.Query("SELECT rowid FROM friendlynames WHERE key=?", c.addressee)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	if !rows.Next() {
		c.channel <- badRequest{"no such key", 400}
		return
	}
	var name int64
	err = rows.Scan(&name)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}

	messagesDir := userMessagePath(name)
	messageFileNames, err := ioutil.ReadDir(messagesDir)
	if err != nil {
		c.channel <- goodHttpResponse([]byte{0})
		return
	}
	if len(messageFileNames) == 0 {
		c.channel <- goodHttpResponse([]byte{0})
		return
	}
	fileInfo := messageFileNames[0]
	filename := fileInfo.Name()
	filepath := messagesDir + "/" + filename

	message, err := ioutil.ReadFile(filepath)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
	c.channel <- goodHttpResponse(append([]byte{0}, message...))
	err = os.Remove(filepath)
	if err != nil {
		inputChannel <- fatalError{err}
	}
}

type addAMemberRequest struct {
	idToken idTokenT
	name    uint64
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

func validToken(token idTokenT, unused []uint64) ([]uint64, bool) {
	var keyArr [32]byte
	copy(keyArr[:], token.publicSignKey)
	signed, ok := sign.Open([]byte{}, token.signature, &keyArr)
	if !ok {
		return unused, false
	}
	if !equalBytes(signed, token.messageHash) {
		return unused, false
	}
	if !isAmember(token.authCode, unused) {
		return unused, false
	}
	newUnused := removeItem(unused, token.authCode)
	return newUnused, true
}

func (a addAMemberRequest) updateOnRequest(state stateT, responseChan chan httpResponseT) (stateT, []outputT) {
	newAuthCodes, ok := validToken(a.idToken, state.authCodes)
	if !ok {
		return state, []outputT{badResponse{"bad ID token", 400, responseChan}}
	}
	state.authCodes = newAuthCodes
	if len(state.friendlyNames) == 0 {
		state.fatalErr = errors.New("no \"admin\" user")
		return state, []outputT{}
	}
	adminUser := state.friendlyNames[0]
	if !equalBytes(a.idToken.publicSignKey, adminUser) {
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
	statement, err := database.Prepare("INSERT INTO members (member) VALUES (?)")
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
	publicSignKey []byte
	authCode      uint64
	signature     []byte
	messageHash   []byte
}

func parseIdToken(body []byte) idTokenT {
	authBytes := body[33:41]
	authCode, _ := binary.Uvarint(authBytes)
	message := append(
		append(body[0:1], authBytes...), body[137:]...)
	hash := sha256.Sum256(message)
	hashSlice := hash[:]
	return idTokenT{
		publicSignKey: body[1:33],
		authCode:      authCode,
		signature:     body[41:137],
		messageHash:   hashSlice,
	}
}

func parseAddAMember(body []byte) parsedRequestT {
	if len(body) < 138 {
		return badRequest{"body less than 138 bytes", 400}
	}
	idToken := parseIdToken(body)
	nameToAdd, _ := binary.Uvarint(body[137:])
	return addAMemberRequest{
		idToken: idToken,
		name:    nameToAdd,
	}
}

type getProofOfWorkInfoRequest struct{}

type outputsT []outputT

func encodeCounter(counter uint64) []byte {
	buf := make([]byte, 8)
	_ = binary.PutUvarint(buf, counter)
	return buf
}

type cachePowUniqueT uint64

const uniquePowFileName = dataDir + "/uniqueProofOfWorkCounter"

func (c cachePowUniqueT) io(inputChannel chan inputT) {
	buf := make([]byte, 8)
	_ = binary.PutUvarint(buf, uint64(c))
	err := ioutil.WriteFile(uniquePowFileName, buf, 0644)
	if err != nil {
		inputChannel <- fatalError{err}
		return
	}
}

func trim(unused []uint64) []uint64 {
	length := len(unused)
	const maxUnusedPowCodesToKeep = 5000
	tooMuch := length - maxUnusedPowCodesToKeep
	if tooMuch > 0 {
		return unused[tooMuch:]
	}
	return unused
}

func (getProofOfWorkInfoRequest) updateOnRequest(state stateT, httpResponseChan chan httpResponseT) (stateT, []outputT) {
	response := goodHttpResponse(encodeCounter(state.proofOfWork.unique))
	sendResponse := sendHttpResponse{
		channel:  httpResponseChan,
		response: response,
	}
	cachePow := cachePowUniqueT(state.proofOfWork.unique + 1)
	outputs := []outputT{sendResponse, cachePow}
	state.proofOfWork.unique += 1
	state.proofOfWork.unused = append(state.proofOfWork.unused, state.proofOfWork.unique)
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

type lookupNameT uint64

func (name lookupNameT) updateOnRequest(state stateT, httpResponseChan chan httpResponseT) (stateT, []outputT) {
	if len(state.friendlyNames) < int(name) {
		return state, []outputT{badResponse{"unknown name", 400, httpResponseChan}}
	}
	key := state.friendlyNames[int(name)]
	return state, []outputT{nameLookupResponse{key, httpResponseChan}}
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
	server []byte
	client []byte
}

type makeFriendlyNameRequest struct {
	proofOfWork proofOfWorkT
	newKey      []byte
}

func removeItem(items []uint64, item uint64) []uint64 {
	newItems := make([]uint64, 0, len(items))
	for _, oldItem := range items {
		if oldItem == item {
			continue
		}
		newItems = append(newItems, oldItem)
	}
	return newItems
}

func firstXareZero(bs [32]byte, x uint8) bool {
	for i := 0; i < int(x); i++ {
		if bs[i] != 0 {
			return false
		}
	}
	return true
}

func isAmember(candidate uint64, of []uint64) bool {
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
	serverCandidate, _ := binary.Uvarint(pow.server)
	if !isAmember(serverCandidate, s.unused) {
		return s, false
	}
	hash := sha256.Sum256(append(pow.server, pow.client...))
	if !firstXareZero(hash, s.difficulty) {
		return s, false
	}
	s.unused = removeItem(s.unused, serverCandidate)
	return s, true
}

func (m makeFriendlyNameRequest) updateOnRequest(state stateT, httpResponseChan chan httpResponseT) (stateT, []outputT) {
	newPow, ok := checkProofOfWork(m.proofOfWork, state.proofOfWork)
	if !ok {
		return state, []outputT{badResponse{"bad proof of work", 400, httpResponseChan}}
	}

	if keyExists(m.newKey, state.friendlyNames) {
		return state, []outputT{badResponse{"key already used", 400, httpResponseChan}}
	}

	state.friendlyNames = append(state.friendlyNames, m.newKey)
	state.proofOfWork = newPow
	return state, []outputT{cacheNewKeyT{httpResponseChan, m.newKey}}
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
	statement, err := database.Prepare("INSERT INTO friendlynames (key) VALUES (?)")
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
	c.channel <- newUsernameT(username)
}

type newUsernameT int64

func (n newUsernameT) respond(w http.ResponseWriter) {
	buf := make([]byte, 8)
	_ = binary.PutUvarint(buf, uint64(n))
	w.Write(buf)
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
	if len(body) != 56 {
		return badRequest{"body is not 56 bytes", 400}
	}

	proofOfWork := proofOfWorkT{
		server: body[1:9],
		client: body[9:25],
	}

	return makeFriendlyNameRequest{
		proofOfWork: proofOfWork,
		newKey:      body[25:57],
	}
}

func (h httpRequestT) update(state stateT) (stateT, []outputT) {
	return parseRequest(h.body).updateOnRequest(state, h.responseChan)
}

func (startHttpServer) io(inputChan chan inputT) {
	http.HandleFunc(
		"/api",
		func(w http.ResponseWriter, r *http.Request) {
			body := make([]byte, maxBodyLength)
			_, err := r.Body.Read(body)
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
		})
	http.HandleFunc(
		"/",
		func(w http.ResponseWriter, r *http.Request) {
			w.Header().Add("Content-Security-Policy", csp)
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

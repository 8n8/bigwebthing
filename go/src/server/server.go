package main

import (
	"crypto/sha256"
	"crypto/sha512"
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
	fatalErr      error
	proofOfWork   proofOfWorkState
	friendlyNames [][]byte
	members       map[int]struct{}
	authCodes     []int
	authUnique    int
	whitelists    map[int]map[int]struct{}
}

type proofOfWorkState struct {
	difficulty uint8
	unique     int
	unused     []int
}

const powDifficulty = 10

func initState() stateT {
	pow := proofOfWorkState{
		difficulty: powDifficulty,
		unique:     0,
		unused:     []int{},
	}
	return stateT{
		fatalErr:      nil,
		proofOfWork:   pow,
		friendlyNames: [][]byte{},
		members:       make(map[int]struct{}),
		authCodes:     []int{},
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
		return parseRetrieveMessage(body)
	case 10:
		return parseWhitelistSomeone(body)
	case 11:
		return parseUnwhitelistSomeone(body)
	}

	return badRequest{"bad route", 400}
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
	newAuthCodes, ok := validToken(u.IdToken, state.authCodes)
	if !ok {
		return state, bad("bad ID token", 400)
	}
	state.authCodes = newAuthCodes

	senderId, ok := getMemberId(u.IdToken.publicSignKey, state.friendlyNames)
	if !ok {
		return state, bad("unknown sender", 400)
	}

	whitelist, ok := state.whitelists[senderId]
	if !ok {
		return state, bad("already not whitelisted", 400)
	}
	_, ok = whitelist[u.Name]
	if !ok {
		return state, bad("already not whitelisted", 400)
	}
	delete(whitelist, u.Name)
	state.whitelists[senderId] = whitelist
	response := deleteWhitelistee{
		Owner:    senderId,
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
	if len(body) != 161 {
		return badRequest{"length of body is not 161", 400}
	}
	idToken := parseIdToken(body)

	proofOfWork := proofOfWorkT{
		Server: body[137:145],
		Client: body[145:153],
	}
	name := decodeInt(body[153:])
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

	newAuthCodes, ok := validToken(w.IdToken, state.authCodes)
	if !ok {
		return state, bad("bad ID token", 400)
	}
	state.authCodes = newAuthCodes

	senderId, ok := getMemberId(w.IdToken.publicSignKey, state.friendlyNames)
	if !ok {
		return state, bad("unknown sender", 400)
	}

	if w.Name >= len(state.friendlyNames) {
		return state, bad("unknown invitee", 400)
	}

	whitelist, ok := state.whitelists[senderId]
	if !ok {
		whitelist = make(map[int]struct{})
	}

	whitelist[w.Name] = struct{}{}
	state.whitelists[senderId] = whitelist
	response := addToWhitelist{
		Owner:   senderId,
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
	_, senderIsMember := state.members[senderId]
	recipientId, ok := getMemberId(s.recipient, state.friendlyNames)
	if !ok {
		return state, bad("unknown recipient", 400)
	}
	recipientWhitelist, ok := state.whitelists[recipientId]
	if !ok {
		recipientWhitelist = make(map[int]struct{})
	}
	_, ok = recipientWhitelist[senderId]
	if !ok {
		return state, []outputT{hiddenError(responseChan)}
	}
	_, recipientIsMember := state.members[recipientId]
	if (!senderIsMember) && (!recipientIsMember) {
		return state, bad("neither sender nor recipient are members", 400)
	}
	return state, []outputT{sendMessage{recipientId, s.message, responseChan}}
}

type hiddenError chan httpResponseT

func (h hiddenError) io(inputChannel chan inputT) {
	h <- goodHttpResponse([]byte{})
}

type sendMessage struct {
	recipient int
	message   []byte
	channel   chan httpResponseT
}

func (s sendMessage) io(inputChannel chan inputT) {
	messagesDir := userMessagePath(int(s.recipient))
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
	senderId, ok := getMemberId(token.publicSignKey, state.friendlyNames)
	if !ok {
		return state, []outputT{badResponse{"not a member", 400, responseChan}}
	}
	return state, []outputT{collectMessage{senderId, responseChan}}
}

type collectMessage struct {
	addressee int
	channel   chan httpResponseT
}

const inboxesDir = "inboxes"

func userMessagePath(name int) string {
	nameString := strconv.FormatInt(int64(name), 10)
	return inboxesDir + "/" + nameString
}

func (c collectMessage) io(inputChannel chan inputT) {
	messagesDir := userMessagePath(c.addressee)
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

func validToken(token idTokenT, unused []int) ([]int, bool) {
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
	publicSignKey []byte
	authCode      int
	signature     []byte
	messageHash   []byte
}

func safeCombine(route byte, authBytes, message []byte) []byte {
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
	authBytes := body[33:41]
	authCode := decodeInt(authBytes)
	message := safeCombine(body[0], authBytes, body[137:])
	hash := sha512.Sum512(message)
	hashSlice := hash[:32]
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

const powMax = 75

func firstXareOk(bs [64]byte, x uint8) bool {
	for i := 0; i < int(x); i++ {
		if bs[i] > powMax {
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
	if !firstXareOk(hash, s.difficulty) {
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
	if len(body) != 49 {
		return badRequest{"body is not 49 bytes", 400}
	}

	proofOfWork := proofOfWorkT{
		Server: body[1:9],
		Client: body[9:17],
	}

	return makeFriendlyNameRequest{
		ProofOfWork: proofOfWork,
		NewKey:      body[17:49],
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
			n, err := r.Body.Read(body)
			if err != nil && err != io.EOF {
				http.Error(w, err.Error(), 400)
				return
			}
			body = body[:n]
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

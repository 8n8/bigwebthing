package main

import (
	"encoding/base64"
	"fmt"
	"github.com/flynn/noise"
	"os"
	"io/ioutil"
)

func main() {
	err := mainErr()
	if (err != nil) {
		fmt.Println(err.Error())
	}
}

func mainErr() error {
	args, err := parseArgs(os.Args[1:])
	if err != nil {
		return err
	}
	return args.run()
}

type Args interface {
	run() error
}

const dhlen = 32

func parseUserId(raw string) ([]byte, error) {
	decoded, err := base64.RawURLEncoding.DecodeString(raw)
	if err != nil {
		return []byte{}, err
	}
	if len(decoded) != dhlen {
		return []byte{}, BadUserIdLength(len(decoded))
	}
	return decoded, nil
}

func parseArgs(args []string) (Args, error) {
	argsLength := len(args)
	if argsLength == 1 {
		return parseOneArg(args[0])
	}
	if argsLength == 2 {
		userId, err := parseUserId(args[1])
		if err != nil {
			return nil, err
		}
		return parseTwoArgs(args[0], userId)
	}
	return nil, BadArgs{}
}

func parseOneArg(arg string) (Args, error) {
	switch arg {
	case "help":
		return Help{}, nil
	case "myid":
		return MyId{}, nil
	case "get":
		return Read_{}, nil
	}
	return nil, BadArgs{}
}

type BadArgs struct{}

const usage = `
Get usage

    $ bwt help

Get my ID

    $ bwt myid

Read messages

    $ bwt read

Write a new message from STDIN

    $ bwt write <recipient ID>

Add contact

    $ bwt addcontact <contact ID>`


const badArgsMessage = "bad arguments: usage:\n" + usage

func (BadArgs) Error() string {
	return badArgsMessage
}

func parseTwoArgs(arg1 string, userId []byte) (Args, error) {
	switch arg1 {
	case "send":
		return Write_(userId), nil
	case "addcontact":
		return AddContact(userId), nil
	}
	return nil, BadArgs{}
}

type AddContact []byte

func (a AddContact) run() error {
	fmt.Println("addcontact not implemented yet")
	return nil
}

type Write_ []byte

func (s Write_) run() error {
	fmt.Println("send not implemented yet")
	return nil
}

type Read_ struct{}

type UserId []byte

type Ephemeral struct {
	id UserId
	keys noise.DHKey
}

type Cache struct {
	staticKeys noise.DHKey
	contacts []UserId
	ephemerals []Ephemeral
}

func readCache() (Cache, error) {
	fmt.Println("readCache not implemented yet")
	var cache Cache
	return cache, nil
}

type Noise interface {
}

type KK interface {
	read_(UserId, []Noise, Secret) ([]Noise, error)
}

func requestMessage(cache Cache) error {
	fmt.Println("requestMessage not implemented yet")
	return nil
}

func downloadMessage(cache Cache) ([]byte, error) {
	fmt.Println("downloadMessage not implemented yet")
	return []byte{}, nil
}

const kk1_size = 48

func parseKk1(raw []byte, lenraw int, pos int) (KK, int, error) {
	if lenraw - pos < kk1_size {
		return nil, pos, TooShortForKk1{lenraw: lenraw, pos: pos}
	}

	return Kk1(raw[pos: pos + kk1_size]), pos + kk1_size, nil
}

const kk2_size = 48

func parseKk2(raw []byte, lenraw int, pos int) (KK, int, error) {
	if lenraw - pos < kk2_size {
		return nil, pos, TooShortForKk2{lenraw: lenraw, pos: pos}
	}

	return Kk2(raw[pos: pos + kk2_size]), pos + kk2_size, nil
}

const kk_transport_size = 40

type KkTransport []byte

type Kk1 []byte

// A KK1 is one of:
//
// 1. new, sent to me
//     + make a new KK2 in response
//
// 2. old, sent to me
//     + make the Noise state
//
// 3. sent by me
//     + make the Noise state
//
// 4. sent by someone else
//	+ ignore
//
func (k Kk1) read_(contact UserId, noises []Noise, secret Secret) ([]Noise, error) {
	session, known_kk := secret.receiving.get(k)
	if known_kk {
		// So it is an old one I have responded to.
		replica_kk1, noise := makeKk1(
			session.secret, contact, secret.staticKeys)
		if replica_kk1 != k {
			return noises, CouldntReplicateKk1{}
		}
	}
}

func makeKk1(secret []byte, contact UserId) (Kk1, Noise) {
	config := makeConfig(secret, contact)
}

type CouldntReplicateKk1 struct{}

func (CouldntReplicateKk1) Error() string {
	return "couldn't replicate KK1"
}

type Kk2 []byte

type TooShortForKk1 struct {
	lenraw int
	pos int
}

type TooShortForKk2 struct {
	lenraw int
	pos int
}

func (t TooShortForKk2) Error() string {
	return fmt.Sprintf("bad 'public' file: failed parsing KK2: parser position %d, but length is %d", t.pos, t.lenraw)
}

type TooShortForTransport struct {
	lenraw int
	pos int
}

func (t TooShortForTransport) Error() string {
	return fmt.Sprintf("bad 'public' file: failed parsing KK transport: parser position &d, but length is &d", t.pos, t.lenraw)
}

func (t TooShortForKk1) Error() string {
	return fmt.Sprintf("bad 'public' file: failed parsing KK1: parser position %d, but length is %d", t.pos, t.lenraw)
}

func parseKkTransport(raw []byte, lenraw int, pos int) (KK, int, error) {
	if lenraw - pos < kk_transport_size {
		return nil, pos, TooShortForTransport{lenraw: lenraw, pos: pos}
	}
	return KkTransport(raw[pos: pos + kk_transport_size]), pos + kk_transport_size, nil
}

func parseKk(raw []byte, lenraw int, pos int) (KK, int, error) {
	indicator := raw[pos]
	pos++

	switch indicator {
	case 0:
		return parseKk1(raw, lenraw, pos)
	case 1:
		return parseKk2(raw, lenraw, pos)
	case 2:
		return parseKkTransport(raw, lenraw, pos)
	}
	return nil, pos, BadKkIndicator(indicator)
}

type BadKkIndicator int

func (b BadKkIndicator) Error() string {
	return "bad KK indicator"
}

const publicPath = "public"

type NoPublicFile struct{}

func (NoPublicFile) Error() string {
	return "need file named 'public'"
}

func readPublic() ([]KK, error) {
	raw, err := ioutil.ReadFile(publicPath)
	if os.IsNotExist(err) {
		return []KK{}, NoPublicFile{}
	}
	if err != nil {
		return []KK{}, err
	}

	kks := make([]KK, 0)
	pos := 0
	lenraw := len(raw)
	for pos < lenraw {
		kk, pos, err := parseKk(raw, lenraw, pos)
		if err != nil {
			return kks, err
		}
		kks = append(kks, kk)
	}
	return kks, nil
}

const kk1Size = 48

type Sessions interface {
	insert(Kk1, Session)
	remove(Kk1)
	get(Kk1) (Session, bool)
}

type Session struct {
	theirId UserId
	secret []byte
}

type Secret struct {
	staticKeys noise.DHKey
	contacts []UserId
	sending Sessions
	receiving Sessions
}

const secretPath = "secret"

func parseSessions(raw []byte, pos int) (Sessions, int, error) {
	panic("parseSessions not implemented yet")
	return *new(Sessions), 0, nil
}

func parseContacts(raw []byte, pos int) ([]UserId, int, error) {
	panic("parseContacts not implemented yet")
	return *new([]UserId), 0, nil
}

func parseStaticKeys(raw []byte, pos int) (noise.DHKey, int, error) {
	panic("parseStaticKeys not implemented yet")
	return *new(noise.DHKey), 0, nil
}

func parseSecret(raw []byte) (Secret, error) {
	pos := 0
	var secret Secret

	staticKeys, pos, err := parseStaticKeys(raw, pos)
	if err != nil {
		return secret, err
	}

	contacts, pos, err := parseContacts(raw, pos)
	if err != nil {
		return secret, err
	}

	sending, pos, err := parseSessions(raw, pos)
	if err != nil {
		return secret, err
	}

	receiving, pos, err := parseSessions(raw, pos)
	if err != nil {
		return secret, err
	}

	if pos != len(raw) {
		return secret, ExpectSecretEnd{
			pos: pos,
			lenraw: len(raw)}
	}

	return Secret {
		staticKeys: staticKeys,
		contacts: contacts,
		sending: sending,
		receiving: receiving,
	}, nil
}

type ExpectSecretEnd struct {
	pos int
	lenraw int
}

func (e ExpectSecretEnd) Error() string {
	return fmt.Sprintf("bad secret file: expecting file end at position %d, but length is %d", e.pos, e.lenraw)
}

func makeSecret() (Secret, error) {
	panic("makeSecret not done yet")
	return *new(Secret), nil
}

func readSecret() (Secret, error) {
	raw, err := ioutil.ReadFile(secretPath)
	if os.IsNotExist(err) {
		return makeSecret()
	}
	if err != nil {
		return *new(Secret), err
	}
	return parseSecret(raw)
}

func processKk(kk KK, noises []Noise, secret Secret) ([]Noise, error) {
	for _, contact := range secret.contacts {
		noises, err := kk.read_(contact, noises, secret)
		if err != nil {
			return noises, err
		}
	}
	return noises, nil
}

func (Read_) run() error {
	secret, err := readSecret()
	if err != nil {
		return err
	}

	public, err := readPublic()
	if err != nil {
		return err
	}

	noises := make([]Noise, 0)
	for _, kk := range public {
		noises, err = processKk(kk, noises, secret)
		if err != nil {
			return err
		}
	}

	return nil
}

type MyId struct{}

func (MyId) run() error {
	fmt.Println("myid not implemented yet")
	return nil
}

type Help struct{}

func (Help) run() error {
	fmt.Println("help not implmented yet")
	return nil
}

type BadUserIdLength int

func (b BadUserIdLength) Error() string {
	return fmt.Sprintf("wrong length: expected 43, got %d", int(b))
}

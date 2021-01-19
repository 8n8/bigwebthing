package main

import (
	"encoding/base64"
	"fmt"
	"github.com/flynn/noise"
	"os"
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
		return Get{}, nil
	}
	return nil, BadArgs{}
}

type BadArgs struct{}

const usage = `
Get usage

    $ bwt help

Get my ID

    $ bwt myid

Get messages

    $ bwt get

Send a new message from STDIN

    $ bwt send <recipient ID>

Add contact

    $ bwt addcontact <contact ID>`


const badArgsMessage = "bad arguments: usage:\n" + usage

func (BadArgs) Error() string {
	return badArgsMessage
}

func parseTwoArgs(arg1 string, userId []byte) (Args, error) {
	switch arg1 {
	case "send":
		return Send(userId), nil
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

type Send []byte

func (s Send) run() error {
	fmt.Println("send not implemented yet")
	return nil
}

type Get struct{}

type UserId []byte

type Contacts interface {
	member(UserId) bool
	add(UserId)
	remove(UserId)
}

type Ephemeral struct {
	id UserId
	keys noise.DHKey
}

type Cache struct {
	staticKeys noise.DHKey
	contacts Contacts
	ephemerals []Ephemeral
}

func readCache() (Cache, error) {
	fmt.Println("readCache not implemented yet")
	var cache Cache
	return cache, nil
}

type NoiseState interface {
}

type KK interface {
	update(Cache, []NoiseState) error
}

func requestMessage(cache Cache) error {
	fmt.Println("requestMessage not implemented yet")
	return nil
}

func downloadMessage(cache Cache) ([]byte, error) {
	fmt.Println("downloadMessage not implemented yet")
	return []byte{}, nil
}

const KK12size = 49

const KK3size = 41

func parseMessage(raw []byte) (KK, error) {
	length := len(raw)
	if length == KK12size {
		if raw[0] == 0 {
			return KK1(raw[1:]), nil
		}
		if raw[0] == 1 {
			return KK2(raw[1:]), nil
		}
		return nil, BadKK{}
	}
	if length == KK3size {
		if raw[0] != 2 {
			return nil, BadKK{}
		}
		return KK3(raw[1:]), nil
	}
	return nil, BadKK{}
}

type BadKK struct{}

func (BadKK) Error() string {
	return "bad message from server"
}

type KK1 []byte

type KK2 []byte

type KK3 []byte

func getMessage(cache Cache) (KK, error) {
	err := requestMessage(cache)
	var kk KK
	if err != nil {
		return kk, err
	}

	raw, err := downloadMessage(cache)
	if err != nil {
		return kk, err
	}

	return parseMessage(raw)
}

func processMessage(cache Cache, noises []NoiseState) error {
	message, err := getMessage(cache)
	if err != nil {
		return err
	}

	return message.update(cache, noises)
}

func (Get) run() error {
	cache, err := readCache()
	if err != nil {
		return err
	}

	noises := make([]NoiseState, 0)
	for {
		err = processMessage(cache, noises)
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

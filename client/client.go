package main

import (
	"encoding/base64"
	"fmt"
	"os"
)

func main() {
	var state State = Empty{}
	var input In = Start{}
	var output Out
	inputChan := make(chan In)
	for _, end := output.(End); !end; {
		state, output = input.update(state)
		go output.run(inputChan)
		input = <-inputChan
	}
}

//
//
//
// Inputs
//
//
//

type Start struct{}

type RawArgs []string

//
//
//
// States
//
//
//

type Empty struct{}

type ReadingArgs struct{}

//
//
//
// Outputs
//
//
//

type ReadArgs struct{}

type End struct{}

type Print string

type OutList []Out

//
//
//
// update implementations
//
//
//

type In interface {
	update(State) (State, Out)
}

func (Start) update(state State) (State, Out) {
	return ReadingArgs{}, ReadArgs{}
}

func (args RawArgs) update(state State) (State, Out) {
	return state.onArgs(args)
}

//
//
//
// state actions implementations
//
//
//

type State interface {
	onArgs(RawArgs) (State, Out)
}

func (ReadingArgs) onArgs(rawArgs RawArgs) (State, Out) {
	args, err := parseArgs(rawArgs[1:])
	if err != nil {
		return Empty{}, OutList([]Out{Print(usage), End{}})
	}

	return args.update()
}

func (Empty) onArgs(_ RawArgs) (State, Out) {
	panic("Empty.onArgs can't happen")
}

//
//
//
// run implementations
//
//
//

type Out interface {
	run(chan In)
}

func (ReadArgs) run(in chan In) {
	in <- RawArgs(os.Args)
}

func (End) run(_ chan In) {}

func (p Print) run(_ chan In) {
	fmt.Print(p)
}

func (outputs OutList) run(in chan In) {
	for _, output := range outputs {
		output.run(in)
	}
}

//
//
//
// pure simple helpers
//
//
//

type BadArgs struct{}

type Bwt struct{}

type Args interface {
	update() (State, Out)
}

func parseOneArg(arg string) (Args, error) {
	switch arg {
	case "help":
		return Help{}, nil
	case "myid":
		return MyId{}, nil
	case "read":
		return Read_{}, nil
	}
	return nil, BadArgs{}
}

type MessageTooLong struct{}

func (MessageTooLong) Error() string {
	return "message too long"
}

type BadChar int

func (b BadChar) Error() string {
	return fmt.Sprintf("character not allowed: %d", b)
}

func messageOk(msg [plaintextSize]byte) error {
	for i := 0; i < int(msg[0]); i++ {
		if msg[i+1] < 32 || msg[i+1] > 126 {
			return BadChar(msg[i+1])
		}
	}
	return nil
}

const plaintextSize = 24

func parseMessage(raw string) ([plaintextSize]byte, error) {
	asBytes := []byte(raw)
	var plain [plaintextSize]byte
	// The -1 is because the first byte is used for storing the
	// length.
	if len(asBytes) > plaintextSize-1 {
		return plain, MessageTooLong{}
	}
	copy(plain[1:], asBytes)
	plain[0] = byte(len(asBytes))
	return plain, messageOk(plain)
}

type MyId struct{}

func (MyId) update() (State, Out) {
	panic("MyId.update not implemented yet")
}

type Help struct{}

func (Help) update() (State, Out) {
	panic("Help.update not implemented yet")
}

type Read_ struct{}

func (Read_) update() (State, Out) {
	panic("Read_.update not implemented yet")
}

func (Bwt) update() (State, Out) {
	panic("Bwt.update not implemented yet")
}

func parseArgs(args []string) (Args, error) {
	argsLength := len(args)
	if argsLength == 0 {
		return Bwt{}, nil
	}
	if argsLength == 1 {
		return parseOneArg(args[0])
	}
	userId, err := parseUserId(args[1])
	if err != nil {
		return nil, err
	}
	if argsLength == 3 && args[0] == "write" {
		msg, err := parseMessage(args[2])
		if err != nil {
			return nil, err
		}
		return Write_{to: userId, msg: msg}, nil
	}
	return nil, BadArgs{}
}

const dhlen = 32

func parseUserId(raw string) ([dhlen]byte, error) {
	var userid [dhlen]byte
	decoded, err := base64.RawURLEncoding.DecodeString(raw)
	if err != nil {
		return userid, err
	}
	if len(decoded) != dhlen {
		return userid, BadUserIdLength(len(decoded))
	}
	copy(userid[:], decoded)
	return userid, nil
}

func (BadArgs) Error() string {
	return badArgsMessage
}

type Write_ struct {
	to  [dhlen]byte
	msg [plaintextSize]byte
}

func (Write_) update() (State, Out) {
	panic("Write_.update not implemented yet")
}

type BadUserIdLength int

func (b BadUserIdLength) Error() string {
	return fmt.Sprintf(
		"wrong length: expected 43, got %d", int(b))
}

const badArgsMessage = "bad arguments"

const usage = `Update crypto

    $ bwt

Get usage

    $ bwt help

Get my ID

    $ bwt myid

Read messages

    $ bwt read

Write a new message

    $ bwt write <recipient ID> "hi"

Add contact

    $ bwt addcontact <contact ID>

Make a dummy £1 payment to the server - TESTING ONLY

    $ bwt pay
`

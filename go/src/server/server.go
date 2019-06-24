package main

import (
	"bytes"
	"common"
	"crypto/rand"
	"encoding/gob"
	"encoding/json"
	"fmt"
	"golang.org/x/crypto/nacl/sign"
	"io/ioutil"
	"net"
	"time"
	"errors"
	// "github.com/pkg/profile"
)

type stateT struct {
	newConnChan    chan tcpConnectionT
	errInChan chan errMsgT
	msgInChan chan common.ClientToClient
	connectedUsers map[[32]byte]tcpOutChansT
	isMember       chan isMemberT
	members        map[[32]byte]dontCare
}

func initState() (stateT, error) {
	members, err := readMembers()
	if err != nil {
		return *new(stateT), err
	}
	if len(members) < 1 {
		return *new(stateT), errors.New("No members.")
	}
	return stateT{
		newConnChan: make(chan tcpConnectionT),
		errInChan: make(chan errMsgT),
		msgInChan: make(chan common.ClientToClient),
		connectedUsers: make(map[[32]byte]tcpOutChansT),
		isMember: make(chan isMemberT),
		members: members,
	}, nil
}

type errMsgT struct {
	id [32]byte
	err error
}

type tcpOutChansT struct {
	err chan error
	msg chan common.ClientToClient
}

type dontCare struct{}

type isMemberT struct {
	id         [32]byte
	returnChan chan bool
}

type outputT interface {
	send() inputT
}

type inputT interface {
	update(*stateT) (stateT, outputT)
}

type readChansT struct {
	isMember       chan isMemberT
	errInChan chan errMsgT
	msgInChan chan common.ClientToClient
	newConnChan    chan tcpConnectionT
	members map[[32]byte]dontCare
	connectedUsers map[[32]byte]tcpOutChansT
}

type endConnT struct {
	id [32]byte
}

func (e endConnT) update(s *stateT) (stateT, outputT) {
	newConnUsers := make(map[[32]byte]tcpOutChansT)
	for id, conn := range s.connectedUsers {
		newConnUsers[id] = conn
	}
	delete(newConnUsers, e.id)
	newState := *s
	newState.connectedUsers = newConnUsers
	return newState, readChans(s)
}

func authSigToSlice(sig [common.AuthSigSize]byte) []byte {
	result := make([]byte, common.AuthSigSize)
	for i := 0; i < common.AuthSigSize; i++ {
		result[i] = sig[i]
	}
	return result
}

func authOk(
	a common.AuthSigT,
	code [common.AuthCodeLength]byte) bool {

	givenCode, sigOk := sign.Open(
		make([]byte, 0),
		authSigToSlice(a.Sig),
		&a.Author)
	okAuth := bytes.Equal(givenCode, common.AuthCodeToSlice(code))
	return sigOk && okAuth
}

func (r readChansT) send() inputT {
	select {
	case conn := <-r.newConnChan:
		return conn
	case isMember := <-r.isMember:
		_, ok := r.members[isMember.id]
		isMember.returnChan <- ok
	case errIn := <-r.errInChan:
		fmt.Println(errIn)
		return endConnT{errIn.id}
	case msgIn := <-r.msgInChan:
		fmt.Println("New message reached main thread.")
		fmt.Println(msgIn)
		recipCh, ok := r.connectedUsers[msgIn.Recipient]
		if !ok {
			fmt.Println("recipient not connected")
			return noInputT{}
		}
		fmt.Println("Found recipient.")
		recipCh.msg <- msgIn
		fmt.Println("Msg sent to recipient goroutine.")
	}
	return noInputT{}
}

type noInputT struct{}

func readChans(s *stateT) readChansT {
	return readChansT{
		isMember:       s.isMember,
		errInChan: s.errInChan,
		msgInChan: s.msgInChan,
		newConnChan:    s.newConnChan,
		members: s.members,
		connectedUsers: s.connectedUsers,
	}
}

func (n noInputT) update(s *stateT) (stateT, outputT) {
	return *s, readChans(s)
}

func readMembers() (map[[32]byte]dontCare, error) {
	result := make(map[[32]byte]dontCare)
	var members [][32]byte
	contents, err := ioutil.ReadFile("serverData/members.txt")
	if err != nil {
		return result, err
	}
	err = json.Unmarshal(contents, &members)
	if err != nil {
		return result, err
	}
	for _, member := range members {
		result[member] = dontCare{}
	}
	return result, nil
}

func main() {
	// defer profile.Start().Stop()
	gob.Register(*new(common.ClientToClient))
	gob.Register(*new(common.Encrypted))
    gob.Register(*new(common.GiveMeASymmetricKey))
	gob.Register(*new(common.HereIsAnEncryptionKey))
	state, err := initState()
	if err != nil {
		fmt.Println(err)
		return
	}
	go tcpServer(
		state.newConnChan,
		state.isMember,
		state.errInChan,
		state.msgInChan)
	var output outputT = readChans(&state)
	for {
		fmt.Println(state.connectedUsers)
		input := output.send()
		state, output = input.update(&state)
	}
}

func genAuthCode() ([common.AuthCodeLength]byte, error) {
	authSlice := make([]byte, common.AuthCodeLength)
	_, err := rand.Read(authSlice)
	var authCode [common.AuthCodeLength]byte
	if err != nil {
		return authCode, err
	}
	for i, b := range authSlice {
		authCode[i] = b
	}
	return authCode, nil
}

type tcpConnectionT struct {
	out    chan common.ClientToClient
	outErr chan error
	id     [32]byte
}

func (t tcpConnectionT) update(s *stateT) (stateT, outputT) {
	newConnUsers := make(map[[32]byte]tcpOutChansT)
	for k, v := range s.connectedUsers {
		newConnUsers[k] = v
	}
	newConnUsers[t.id] = tcpOutChansT{t.outErr, t.out}
	newS := *s
	newS.connectedUsers = newConnUsers
	return newS, readChans(s)
}

func handleConn(
	conn net.Conn,
	newConnChan chan tcpConnectionT,
	isMemberChan chan isMemberT,
	errInChan chan errMsgT,
	msgInChan chan common.ClientToClient) {

	fmt.Println("Top of handleConn.")
	conn.SetDeadline(time.Now().Add(time.Second * 30))
	authCode, err := genAuthCode()
	if err != nil {
		fmt.Print(err)
		conn.Close()
		return
	}
	enc := gob.NewEncoder(conn)
	dec := gob.NewDecoder(conn)
	err = enc.Encode(authCode)
	if err != nil {
		fmt.Print(err)
		conn.Close()
		return
	}
	var authSig common.AuthSigT
	err = dec.Decode(&authSig)
	if err != nil {
		fmt.Print(err)
		conn.Close()
		return
	}
	if !authOk(authSig, authCode) {
		fmt.Print(err)
		conn.Close()
		return
	}
	memberOkCh := make(chan bool)
	isMemberChan <- isMemberT{authSig.Author, memberOkCh}
	isMember := <-memberOkCh
	if !isMember {
		conn.Close()
		return
	}
	chs := tcpConnectionT{
		out:    make(chan common.ClientToClient),
		outErr: make(chan error),
		id:     authSig.Author,
	}
	newConnChan <- chs
	conn.SetDeadline(time.Now().Add(time.Minute * 30))
	go func() {
		for {
			//var clientToClient common.ClientToClient
			fmt.Println("C")
			//err = dec.Decode(&clientToClient)
			fmt.Println("B")
			clientToClient, err := common.ReadClientToClient(conn)
			fmt.Println("Just below reading in message from client.")
			if err != nil {
				fmt.Println("A")
				fmt.Println(err)
				errInChan <- errMsgT{authSig.Author, err}
				chs.outErr <- err
				fmt.Print(err)
				conn.Close()
				return
			}
			fmt.Println("XX")
			fmt.Println(msgInChan)
			fmt.Println("xx")
			msgInChan <- clientToClient
			fmt.Println("Y")
		}
	}()
	for {
		conn.SetDeadline(time.Now().Add(time.Minute * 30))
		select {
		case newMsg := <-chs.out:
			fmt.Println("New message out.")
			encoded, err := common.EncodeClientToClient(
				newMsg)
			if err != nil {
				errInChan <- errMsgT{
					authSig.Author,
					err,
					}
				fmt.Print(err)
				conn.Close()
				return
			}
			fmt.Println("Correctly encoded.")
			n, err := conn.Write(encoded)
			if err != nil {
				errInChan <- errMsgT{
					authSig.Author,
					err,
				}
				fmt.Println(err)
				conn.Close()
				return
			}
			if n != len(encoded) {
				errInChan <- errMsgT{authSig.Author, errors.New("Didn't sent whole message.")}
				fmt.Println("Did not send whole message.")
				conn.Close()
				return
			}
			fmt.Println("Message sent without errors.")
			fmt.Println("Recipient: ")
			fmt.Println(authSig.Author)
			fmt.Println(">>>>>>>>")
		case <-chs.outErr:
			conn.Close()
			return
		}
	}
}

func tcpServer(
	newConnChan chan tcpConnectionT,
	isMember chan isMemberT,
	errInChan chan errMsgT,
	msgInChan chan common.ClientToClient) {

	ln, err := net.Listen("tcp", ":4000")
	if err != nil {
		fmt.Println(err)
		return
	}
	defer ln.Close()
	for {
		conn, err := ln.Accept()
		if err != nil {
			fmt.Println(err)
			continue
		}
		go handleConn(conn, newConnChan, isMember, errInChan, msgInChan)
	}
}

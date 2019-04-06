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
)

type stateT struct {
	newConnChan    chan tcpConnectionT
	connectedUsers map[[32]byte]tcpConnectionT
	isMember       chan isMemberT
	members        map[[32]byte]dontCare
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
	newConnChan    chan tcpConnectionT
	connectedUsers map[[32]byte]tcpConnectionT
	members        map[[32]byte]dontCare
}

type endConnT struct {
	id [32]byte
}

func (e endConnT) update(s *stateT) (stateT, outputT) {
	var newConnUsers map[[32]byte]tcpConnectionT
	for id, conn := range s.connectedUsers {
		newConnUsers[id] = conn
	}
	delete(newConnUsers, e.id)
	newState := *s
	newState.connectedUsers = newConnUsers
	return newState, readChans(s)
}

func authOk(
	a common.AuthSigT,
	code [common.AuthCodeLength]byte) bool {

	givenCode, sigOk := sign.Open(
		make([]byte, 0),
		common.AuthSigToSlice(a.Sig),
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
	default:
	}
	for author, conn := range r.connectedUsers {
		select {
		case msg := <-conn.in:
			recip, ok := r.connectedUsers[msg.Recipient]
			if !ok {
				continue
			}
			recip.out <- msg
		case <-conn.inErr:
			return endConnT{author}
		default:
		}
	}
	return noInputT{}
}

type noInputT struct{}

func readChans(s *stateT) readChansT {
	return readChansT{
		isMember:       s.isMember,
		newConnChan:    s.newConnChan,
		connectedUsers: s.connectedUsers,
		members:        s.members,
	}
}

func (n noInputT) update(s *stateT) (stateT, outputT) {
	return *s, readChans(s)
}

func readMembers() (map[[32]byte]dontCare, error) {
	var result map[[32]byte]dontCare
	var members [][32]byte
	contents, err := ioutil.ReadFile("serverData/member.txt")
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
	var state stateT
	members, err := readMembers()
	if err != nil {
		fmt.Println(err)
		return
	}
	if len(members) < 1 {
		fmt.Println("No members.")
		return
	}
	state.members = members
	go tcpServer(state.newConnChan, state.isMember)
	var output outputT = readChans(&state)
	for {
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
	in     chan common.ClientToClient
	out    chan common.ClientToClient
	inErr  chan error
	outErr chan error
	id     [32]byte
}

func (t tcpConnectionT) update(s *stateT) (stateT, outputT) {
	var newConnUsers map[[32]byte]tcpConnectionT
	for k, v := range s.connectedUsers {
		newConnUsers[k] = v
	}
	newConnUsers[t.id] = t
	newS := *s
	newS.connectedUsers = newConnUsers
	return newS, readChans(s)
}

func handleConn(conn net.Conn, newConnChan chan tcpConnectionT, isMemberChan chan isMemberT) {
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
	var memberOkCh chan bool
	isMemberChan <- isMemberT{authSig.Author, memberOkCh}
	if !<-memberOkCh {
		conn.Close()
		return
	}
	chs := tcpConnectionT{
		in:     make(chan common.ClientToClient),
		out:    make(chan common.ClientToClient),
		inErr:  make(chan error),
		outErr: make(chan error),
		id:     authSig.Author,
	}
	newConnChan <- chs
	conn.SetDeadline(time.Now().Add(time.Minute * 30))
	go func() {
		for {
			var clientToClient common.ClientToClient
			err = dec.Decode(&clientToClient)
			if err != nil {
				chs.inErr <- err
				chs.outErr <- err
				fmt.Print(err)
				conn.Close()
				return
			}
			chs.in <- clientToClient
		}
	}()
	for {
		conn.SetDeadline(time.Now().Add(time.Minute * 30))
		select {
		case newMsg := <-chs.out:
			err = enc.Encode(newMsg)
			if err != nil {
				chs.inErr <- err
				fmt.Print(err)
				conn.Close()
				return
			}
		case <-chs.outErr:
			conn.Close()
			return
		default:
		}
	}
}

func tcpServer(newConnChan chan tcpConnectionT, isMember chan isMemberT) {
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
		go handleConn(conn, newConnChan, isMember)
	}
}

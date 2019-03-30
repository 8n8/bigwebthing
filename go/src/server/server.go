package main

import (
	"bytes"
	"common"
	"crypto/rand"
	"encoding/gob"
	"errors"
	"fmt"
	"golang.org/x/crypto/nacl/sign"
	"net"
	"time"
)

type stateT struct {
	newConnChan    chan tcpConnectionT
	connectedUsers map[[32]byte]tcpConnectionT
}

type outputT interface {
	send(chan inputT)
}

type inputT interface {
	update(*stateT) (stateT, outputT)
}

type readChansT struct {
	newConnChan    chan tcpConnectionT
	connectedUsers map[[32]byte]tcpConnectionT
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

func inviteSigOk(i common.InviteT) bool {
	untrusted, sigOk := sign.Open(
		make([]byte, 0),
		common.SigToSlice(i.Signature),
		&i.Author)
	return bytes.Equal(untrusted, common.InviteHash(i)) && sigOk
}

func isMember(
	author [32]byte,
	invites []common.InviteT,
	tNow int64) bool {

	for _, invite := range invites {
		if invite.ExpiryPosix < tNow || !inviteSigOk(invite) {
			return false
		}
	}
	if !bytes.Equal(
		common.HashToSlice(invites[0].Author),
		common.HashToSlice(common.TruesPubSign)) {

		return false
	}
	for i := 1; i < len(invites); i++ {
		linkOk := bytes.Equal(
			common.HashToSlice(invites[i].Author),
			common.HashToSlice(invites[i-1].Invitee))
		if !linkOk {
			return false
		}
	}
	return true
}

func authOk(
	a common.AuthSigT,
	code [common.AuthCodeLength]byte,
	tNow int64) bool {

	givenCode, sigOk := sign.Open(
		make([]byte, 0),
		common.AuthSigToSlice(a.Sig),
		&a.Author)
	okAuth := bytes.Equal(givenCode, authCodeToSlice(code))
	return sigOk && okAuth && isMember(a.Author, a.Invites, tNow)
}

func (r readChansT) send(inChan chan inputT) {
	select {
	case conn := <-r.newConnChan:
		inChan <- conn
	default:
	}
	for author, conn := range r.connectedUsers {
		select {
		case msg := <-conn.in:
			recip, ok := r.connectedUsers[msg.Recipient]
			if !ok {
				var errMsg common.ClientToClient
				errMsg.Err = errors.New(
					"Recipient not connected.")
				conn.out <- errMsg
				continue
			}
			recip.out <- msg
		case <-conn.inErr:
			inChan <- endConnT{author}
		default:
		}
	}
}

type noInputT struct{}

func readChans(s *stateT) readChansT {
	return readChansT{
		newConnChan:    s.newConnChan,
		connectedUsers: s.connectedUsers,
	}
}

func (n noInputT) update(s *stateT) (stateT, outputT) {
	return *s, readChans(s)
}

func main() {
	var state stateT
	go tcpServer(state.newConnChan)
	var input inputT = noInputT{}
	var output outputT = readChans(&state)
	var inputCh chan inputT
	for {
		go output.send(inputCh)
		select {
		case input = <-inputCh:
		default:
			input = noInputT{}
		}
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
}

func (t tcpConnectionT) update(s *stateT) (stateT, outputT) {
	var newConnUsers map[[32]byte]tcpConnectionT
	for k, v := range s.connectedUsers {
		newConnUsers[k] = v
	}
	newS := *s
	newS.connectedUsers = newConnUsers
	return newS, readChans(s)
}

func handleConn(conn net.Conn, newConnChan chan tcpConnectionT) {
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
	timeNow := time.Now().Unix()
	if !authOk(authSig, authCode, timeNow) {
		fmt.Print(err)
		conn.Close()
		return
	}
	chs := tcpConnectionT{
		in:     make(chan common.ClientToClient),
		out:    make(chan common.ClientToClient),
		inErr:  make(chan error),
		outErr: make(chan error),
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

func authCodeToSlice(bs [common.AuthCodeLength]byte) []byte {
	result := make([]byte, common.AuthCodeLength)
	for i, b := range bs {
		result[i] = b
	}
	return result
}

func tcpServer(newConnChan chan tcpConnectionT) {
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
		go handleConn(conn, newConnChan)
	}
}

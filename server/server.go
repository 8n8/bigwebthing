package main

import (
	"fmt"
	"net"
)

func main() {
	err := mainErr()
	if err != nil {
		fmt.Println(err)
	}
}

func mainErr() error {
	staticKeys, err := getStaticKeys()
	if err != nil {
		return fmt.Errorf("couldn't get static keys: %s", err)
	}

	ln, err := net.Listen("tcp", ":3001")
	if err != nil {
		return fmt.Errorf("couldn't set up TCP server: %s", err)
	}

	for {
		conn, err := ln.Accept()
		if err != nil {
			return fmt.Errorf("couldn't accept connection: %s", err)
		}
		go handleConn(conn)
	}
}

func handleConn(conn net.Conn, staticKeys noise.DHKey) {
	err := handleConnErr(conn, staticKeys)
	if err != nil {
		fmt.Println(err)
		return
	}
	conn.Close()
}

func serverCipherSuite() noise.CipherSuite {
	return noise.NewCipherSuite(
		noise.DH25519,
		noise.CipherAESGCM,
		noise.HashSHA256)

func noiseServerConfig(staticKeys noise.DHKey) noise.Config {
	return noise.Config{
		CipherSuite: serverCipherSuite(),
		Random: rand.Reader,
		Pattern: noise.HandshakeXK,
		Initiator: false,
		StaticKeypair: staticKeys,
	}

}

func handleConnErr(conn net.Conn, staticKeys noise.DHKey) error {
	encrypted, err := parseRawMessage(conn)
	if err != nil {
		return fmt.Errorf("couldn't parse first raw message: %s", err)
	}

	config := noiseServerConfig(staticKeys)
	shake, err := noise.NewHandshakeState(config)
	if err != nil {
		return fmt.Errorf("couldn't make new Noise handshake: %s", err)
	}

	_, _, _, err = shake.ReadMessage([]byte{}, encrypted)
	if err != nil {
		return fmt.Errorf("couldn't read KK1: %s", err)
	}

	kk2, _, _, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		return fmt.Errorf("couldn't make KK2: %s", err)
	}

	n, err := conn.Write(kk2)
	if n != len(kk2) {
		return fmt.Errorf("couldn't write KK2: expecting %d bytes, got %d", len(kk2), n)
	}
	if err != nil {
		return fmt.Errorf("error on writing KK2: %s", err)
	}

	encrypted, err = parseRawMessage(conn)
	if err != nil {
		return fmt.Errorf("couldn't parse second raw message: %%s", err)
	}

	payload, tx, rx, err := shake.ReadMessage([]byte{}, encrypted)
	if err != nil {
		return fmt.Errorf("couldn't read first payload: %s", err)
	}

	parsed, err := parsePayload(payload)
	if err != nil {
		return fmt.Errorf("couldn't parse payload: %s", err)
	}

	tx, err = parsed.handle(tx)
	if err != nil {
		return fmt.Errorf("couldn't handle message: %s", err)
	}

	for {
		encrypted, err = parseRawMessage(conn)
		if err != nil {
			return fmt.Errorf("couldn't read message from client: %s", err)
		}

		payload, err = rx.Decrypt([]byte{}, encrypted)
		if err != nil {
			return fmt.Errorf("couldn't decrypt payload: %s", err)
		}

		parsed, err = parsePayload(payload)

	}
}

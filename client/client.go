package main

import (
	"crypto/rand"
	"fmt"
	"github.com/flynn/noise"
	"github.com/gorilla/websocket"
	"github.com/mitchellh/go-homedir"
	"github.com/webview/webview"
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"path"
	"time"
)

const Xk2Size = 64

func main() {
	homeDir := getHomeDir()
	staticKeys := getStaticKeys(homeDir)
	go httpServer(homeDir)
	go gui()
	tcpConn(staticKeys)
}

func connSleep() {
	time.Sleep(10 * time.Second)
}

func tcpConn(staticKeys noise.DHKey) {
	for {
		oneTcpConn(staticKeys)
		time.Sleep(10 * time.Second)
	}
}

var serverPk = []byte{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

const serverUrl = "localhost:8080"

func serverConfig(staticKeys noise.DHKey) noise.Config {
	return noise.Config{
		CipherSuite:   noise.NewCipherSuite(noise.DH25519, noise.CipherAESGCM, noise.HashSHA256),
		Random:        rand.Reader,
		Pattern:       noise.HandshakeXK,
		Initiator:     true,
		StaticKeypair: staticKeys,
		PeerStatic:    serverPk,
	}
}

func oneTcpConn(staticKeys noise.DHKey) {
	conn, err := net.Dial("tcp", serverUrl)
	if err != nil {
		return
	}

	defer conn.Close()

	shake, err := noise.NewHandshakeState(serverConfig(staticKeys))
	if err != nil {
		panic("couldn't initialize Noise handshake: " + err.Error())
	}

	xk1, _, _, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		panic("couldn't make XK1: " + err.Error())
	}

	_, err = conn.Write(xk1)
	if err != nil {
		return
	}

	xk2 := make([]byte, Xk2Size)
	n, err := conn.Read(xk2)
	if n != Xk2Size {
		return
	}
	if err != nil {
		return
	}

	_, _, _, err = shake.ReadMessage([]byte{}, xk2)
	if err != nil {
		panic("couldn't read XK2: " + err.Error())
	}

	xk3, tx, rx, err := shake.WriteMessage([]byte{}, []byte{})
	if err != nil {
		panic("couldn't make XK3: " + err.Error())
	}

	_, err = conn.Write(xk3)
	if err != nil {
		return
	}

	end := make(chan struct{})
	go func() {
		tcpReceiver(rx, conn)
		end<-struct{}{}
	}()
	go func() {
		tcpSender(tx, conn)
		end<-struct{}{}
	}
	<-end
}

func tcpReceiver(rx *noise.CipherState, conn net.Conn) {
	rawSize := make([]byte, 2)
	n, err := conn.Read(rawSize)
	if n != 2 {
		return
	}
	if err != nil {
		return
	}

	size := int(rawSize[0]) + (int(rawSize[1]) << 8)

	encrypted := make([]byte, size)
	n, err = conn.Read(encrypted)
	if n != size {
		return
	}
	if err != nil {
		return
	}

	plain, err := rx.Decrypt([]byte{}, serverAd, encrypted)
	if err != nil {
		panic("bad decryption of message from server: " + err.Error())
	}

	fromServerCh <- plain
}

var toServerCh chan []byte = make(chan []byte)

var fromServerCh chan []byte = make(chan []byte)

const authSize = 16

var serverAd = []byte{235, 77, 23, 199, 11, 162, 118, 80, 65, 84, 165, 211, 116, 185, 150, 149}

func tcpSender(tx *noise.CipherState, conn net.Conn) {
	for {
		if oneTcpSend(tx, conn) != nil {
			return
		}
	}
}

func oneTcpSend(tx *noise.CipherState, conn net.Conn) error {
	message := <-toServerCh
	encryptedSize := authSize + len(message)
	toServer := make([]byte, 2, 2+encryptedSize)
	toServer[0] = byte(encryptedSize & 0xFF)
	toServer[1] = byte((encryptedSize >> 8) & 0xFF)
	toServer = tx.Encrypt(toServer, serverAd, message)
	_, err := conn.Write(toServer)
	return err
}

func gui() {
	w := webview.New(false)
	defer w.Destroy()
	w.SetTitle("BigWebThing")
	w.SetSize(800, 600, webview.HintNone)
	w.Navigate("http://localhost" + clientPort)
	w.Run()
}

const clientPort = ":9724"

var upgrader = websocket.Upgrader{
	ReadBufferSize:  1024,
	WriteBufferSize: 1024,
}

func indexPath(homeDir string) string {
	return path.Join(homeDir, "index.html")
}

func httpServer(homeDir string) {
	http.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		f, err := os.Open(indexPath(homeDir))
		if err != nil {
			panic("couldn't read index.html: " + err.Error())
		}
		_, err = io.Copy(w, f)
		if err != nil {
			panic("couldn't send index.html: " + err.Error())
		}
	})
	http.HandleFunc("/websocket", func(w http.ResponseWriter, r *http.Request) {
		conn, err := upgrader.Upgrade(w, r, nil)
		if err != nil {
			panic("couldn't upgrade websocket: " + err.Error())
		}
		handleWebsockets(conn, homeDir)
	})
	http.ListenAndServe(":9001", nil)
}

func handleWebsockets(conn *websocket.Conn, homeDir string) {

}

func getHomeDir() string {
	home, err := homedir.Dir()
	if err != nil {
		panic("couldn't get home dir: " + err.Error())
	}
	return home
}

func staticKeysPath(homePath string) string {
	return path.Join(homePath, "staticKeys")
}

func makeKeys(homePath string) noise.DHKey {
	keys, err := noise.DH25519.GenerateKeypair(rand.Reader)
	if err != nil {
		panic("couldn't generate static keys: " + err.Error())
	}

	encoded := make([]byte, 2*dhlen)
	copy(encoded, keys.Private)
	copy(encoded[:dhlen], keys.Public)
	err = ioutil.WriteFile(staticKeysPath(homePath), encoded, 0400)
	if err != nil {
		panic("couldn't write static keys file: " + err.Error())
	}
	return keys
}

func getStaticKeys(homePath string) noise.DHKey {
	raw, err := ioutil.ReadFile(staticKeysPath(homePath))
	if err != nil {
		return makeKeys(homePath)
	}

	if len(raw) != 2*dhlen {
		panic(fmt.Sprintf("bad static keys file: expecting %d bytes, but got %d", 2*dhlen, len(raw)))
	}

	return noise.DHKey{
		Private: raw[:dhlen],
		Public:  raw[dhlen:],
	}
}

const dhlen = 32

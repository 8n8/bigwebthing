package main

import (
	"github.com/zserge/webview"
    "github.com/gorilla/websocket"
    "io"
    "net/http"
    "fmt"
    "strings"
)

func cacher(c cacheChansT, kill chan error) {
    locks := make(map[string]struct{})
    for { (<-c.to).run(c, locks, kill) }
}

var upgrader = websocket.Upgrader{
    ReadBufferSize: 1024,
    WriteBufferSize: 1024,
}

func websocketHandler(
	w http.ResponseWriter, r *http.Request, c uiChansT) {

	conn, err := upgrader.Upgrade(w, r, nil)
	if err != nil {
		fmt.Println(err)
		return
	}

	go func() {
		for {
			msg := <-c.to
			err := conn.WriteMessage(
				websocket.TextMessage, []byte(msg))
			if err != nil {
				fmt.Println(err)
				return
			}
		}
	}()

	for {
		_, p, err := conn.ReadMessage()
		if err != nil {
			fmt.Println(err)
			return
		}

        msg, err := parseFromUi(string(p))
        if err != nil {
            fmt.Println(err)
            return
        }
        msg.router(c)
	}
}

func ui(c uiChansT, kill chan error) {
	http.Handle("/static/", http.FileServer(http.Dir("")))
	http.HandleFunc(
        "/uploadFile/",
        func(w http.ResponseWriter, r *http.Request) {
            c.from.cache.setHandle <- setHandleT{
                handle: r.Body,
                name: strings.Split(r.URL.Path, "/")[2],
            }
        })
	http.HandleFunc(
		"/websocket",
		func(w http.ResponseWriter, r *http.Request) {
			websocketHandler(w, r, c)
		})
	fmt.Println(http.ListenAndServe(":"+port, nil))
}

type cacheCmdT interface {
    run(cacheChansT, map[string]struct{}, chan error)
}

const port = "17448"

const baseUrl = "http://localhost:" + port

func window() {
    w := webview.New(true)
    defer w.Destroy()
    w.SetTitle("BigWebThing")
    w.SetSize(800, 600, webview.HintNone)
    w.Navigate(baseUrl + "/static/index.html")
}

type cacheSetT struct {
    key string
    value []byte
}

type cacheGotT struct {
    key string
    value []byte
}

type fromCacheChansT struct {
    got chan cacheGotT
    bad chan string
}

type keysForNameT struct {
    sign []byte
    encrypt []byte
    name string
}

type proofOfWorkInfoT struct {
    difficulty byte
    unique []byte
}

type fromServerChansT struct {
    newName chan string
    keysForName chan keysForNameT
    proofOfWorkInfo chan proofOfWorkInfoT
    authCode chan []byte
}

type serverChansT struct {
    from fromServerChansT
    to chan []byte
}

type cacheChansT struct {
    from fromCacheChansT
    to chan cacheCmdT
}

type setHandleT struct {
    handle io.ReadCloser
    name string
}

type fromUiCacheChansT struct {
    get chan string
    set chan cacheSetT
    remove chan string
    setHandle chan setHandleT
}

type fromUiChansT struct {
    toServer chan []byte
    getProofOfWork chan proofOfWorkInfoT
    cache fromUiCacheChansT
    sendMessage chan string
}

type uiChansT struct {
    from fromUiChansT
    to chan []byte
}

type chansT struct {
    server serverChansT
    cache cacheChansT
    ui uiChansT
}

func initChans() chansT {
    return chansT {
        server: serverChansT{
            from: fromServerChansT{
                newName: make(chan string, 1),
                keysForName: make(chan keysForNameT, 1),
                proofOfWorkInfo: make(chan proofOfWorkInfoT, 1),
                authCode: make(chan []byte, 1),
                },
            to: make(chan []byte, 1),
            },
        cache: cacheChansT{
            from: fromCacheChansT{
                got: make(chan cacheGotT, 1),
                bad: make(chan string, 1),
                },
            to: make(chan cacheCmdT, 1),
            },
        ui: uiChansT{
            from: fromUiChansT{
                toServer: make(chan []byte, 1),
                getProofOfWork: make(chan proofOfWorkInfoT, 1),
                cache: fromUiCacheChansT{
                    get: make(chan string, 1),
                    set: make(chan cacheSetT, 1),
                    remove: make(chan string, 1),
                    },
                sendMessage: make(chan string, 1),
                },
            to: make(chan []byte, 1),
            },
        }
}

func main() {

    chans := initChans()
    crash := make(chan error)

    // Start generators and consumers.
    go tcpConn(chans.server, crash)
    go cacher(chans.cache, crash)
    go ui(chans.ui, crash)

    go window()

    // Start processors.


    fmt.Println(<-crash)
}

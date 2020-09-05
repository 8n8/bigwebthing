package main

var in = make(chan In, 1)

type DoNothing struct{}

type Empty struct{}

type Output interface {
    io()
}

func tcpClient() {}

func ui() {}

type In interface {
    update(State) (State, Output)
}

func (DoNothing) io() {}

type State interface {
    
}

func main() {
    go httpServer()
    go ui()
    go tcpClient()

    var state State
    state = Empty{}

    var output Output
    output = DoNothing{}

    for {
        state, output = (<-in).update(state)
        output.io()
    }
}

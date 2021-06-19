const app = Elm.Main.init({
    node: document.getElementById("elmapp")
});

const socket = new WebSocket("../websocket")

app.ports.toJs.subscribe(function(message) {
    socket.send(message)
})

let wasms = {}

socket.addEventListener("message", function(event) {
    const message = event.data

    switch (message[0]) {
        case 0:
            wasmToCompile(message)
            return

        case 1:
            inputForWasm(message)
            return

        case 2:
            newGui(message)
    }
})

function wasmToCompile(message) {
    const cacheId = decodeUint32(message[1:5])
    const wasmSize = decodeUint32(message[5:9])
    const rawWasm = message[9:9+wasmSize]

    Webassembly.instantiate(rawWasm, {})
    .then(function(module, funs) {
        if funs.bigwebthing === null {
            socket.send(badWasm())
            return
        }
        wasms.cacheId = funs.bigwebthing
    })
    .catch(function(err) {
        socket.send(badWasm())
    })
}

function inputForWasm(message) {
    const cacheId = decodeUint32(message[1:5])
    const inputSize = decodeInt32(message[5:9])
    const input = message[9+inputSize:]

    if (wasms.cacheId === null) {
        return
    }

    const output = wasms.cacheId(input)

    socket.send(wasmOutput(cacheId, input, output))
}

function newGui(message) {
    const boxSize = decodeUint32(message[1:5])
    const box = new TextDecoder().decode(message[5:5+boxSize])
    const belows = []
    const start = 5 + boxSize;
    for (let i = start; i < message.length() - start;) {
        if message[i] === 0 {
            i++
            const size = decodeUint32(message[i: i+4])
            i += 4
            const raw = message[i: i+size]
            i += size
            const text = new TextDecoder().decode(raw)
            belows.push({"type": "text", "value": text})
            continue
        }

        if message[i] === 1 {
            i++
            const urlSize = decodeUint32(message[i: i+4])
            i += 4
            const rawUrl = message[i:i+size]
            i += size
            const url = new TextDecoder().decode(rawUrl)
            const altSize = decodeUint32(message[i: i+4])
            i += 4
            const rawAlt = message[i:i+size]
            const alt = new TextDecoder().decode(rawAlt)
            const image = {"url": url, "alt": alt}
            belows.push({"type": "image", "value": image})
            continue
        }
        return
    }
    app.ports.fromJs.send({"box": box, "below": below})
}

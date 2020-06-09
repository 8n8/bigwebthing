const app = Elm.Main.init({node: document.getElementById("main")});

const toElm = app.ports.jsToElm.send;

function initState() {
    
}

function main(fromElm) {
    let s = initState();
    let o = initOutput();
    while (true) {
        const i = io(o);
        [s, o] = update(s, i);
    }
}

app.ports.elmToJs.subscribe(main);

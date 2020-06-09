"use strict"

const app = Elm.Main.init({ node: document.getElementById("main") });

app.ports.elmToJs.subscribe(from => PS.Lib.elmHandler(from));

let CACHE = {};
exports.cacheSet = key => value => CACHE[key] = value;
exports.cacheGet = key => CACHE[key];
exports.toElm = to => app.ports.jsToElm.send(to);

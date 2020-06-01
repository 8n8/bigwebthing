"use strict";

function inboxMenuItem(message, inputs) {
    const container = document.createElement("button");
    container.type = "button";
    container.classList.add("inboxItem");
    container.appendChild(makeSubjectDom(message));
    container.appendChild(makeFromDom(message));
    container.onclick = function () {
        inputs.push({ key: "inboxMenuClick", value: message.id });
    };
    return container;
}

function initOnClicks(inputs) {
    const buttons = [
        "write",
        "contacts",
        "inbox",
        "outbox",
        "drafts",
        "pricing",
        "account",
        "help"];
    let outputs = [];
    for (const button of buttons) {
        const clickMessage = {key: "topButtonClick", value: button};
        const onclick = function() {inputs.push(clickMessage)};
        outputs.push({
            key: "addOnClick",
            value: {id: button + "Button", onclick: onclick}});
    }
    return outputs;
}

function initOutputs(inputs) {
    return [
        { key: "cache query", value: "page" },
        { key: "cache query", value: "inbox" },
        { key: "cache query", value: "drafts" },
        { key: "cache query", value: "my name" },
        ].concat(initOnClicks(inputs));
}

function makeSubjectDom(message) {
    const subject = document.createElement("p");
    if ("subject" in message) {
        subject.textContent = "Subject: " + message.subject;
    } else {
        subject.textContent = "No subject";
        subject.classList.add("noneMessage");
    }
    return subject;
}

function makeFromDom(message) {
    const from = document.createElement("p");
    from.textContent = "From: " + message.from;
    return from;
}

function combine(a, b) {
    const lena = a.length;
    const lenb = b.length;
    let buf = new ArrayBuffer(lena + lenb);
    let combined = new Uint8Array(buf);
    for (let i = 0; i < lena; i++) {
        combined[i] = a[i];
    }
    for (let i = lena; i < lena + lenb; i++) {
        const bval = b[i - lena];
        combined[i] = bval;
    }
    return combined;
}

function decodeInt(eightBytes) {
    let result = 0;
    for (let i = 0; i < 8; i++) {
        result += eightBytes[i] * Math.pow(256, i);
    }
    return result;
}

function isDifficult(hash, difficulty) {
    for (let i = 0; i < 32; i++) {
        if (hash[i] < difficulty) {
            return false;
        }
    }
    return true;
}

function proofOfWork(powInfo) {
    let buffer = new ArrayBuffer(8);
    let bufferView = new Uint8Array(buffer);
    let counter = new Int32Array(buffer);
    while (true) {
        const combined = combine(powInfo.unique, bufferView);
        const hash = nacl.hash(combined).slice(0, 32);
        if (isDifficult(hash, powInfo.difficulty)) {
            return combined;
        }
        counter[0] = counter[0] + 1;
    }
}

function uint8Array(length) {
    let buffer = new ArrayBuffer(length);
    return new Uint8Array(buffer);
}

function makeMyNameRequest(pow, publicSigningKey) {
    let request = uint8Array(49);
    request[0] = 1;
    for (let i = 0; i < 16; i++) {
        request[i + 1] = pow[i];
    }
    for (let i = 0; i < 32; i++) {
        request[i + 17] = publicSigningKey[i];
    }
    return request;
}

function turnButtonOn(id) {
    return [
        {key: "addCssClass",
         value: {id: id, cssClass: "selectedButton"}},
        {key: "removeCssClass",
         value: {id: id, cssClass: "notSelectedButton"}}]
}

function turnButtonOff(id) {
    return [
        {key: "addCssClass",
         value: {id: id, cssClass "notSelectedButton"}},
        {key: "removeCssClass",
         value: {id: id, cssClass "selectedButton"}}];
}

function drawInboxItem(message, inputs) {
    const button = document.createElement("button");
    button.type = "button";
    button.classList.add("inboxItem");
    button.appendChild(makeSubjectDom(message));
    button.appendChild(makeFromDom(message));
    button.onclick = function () {
        inputs.push({ key: "inboxMenuClick", value: message.id });
    };
    return button;
}

function drawInbox(state) {
    let menu = [];
    for (message of state.messages) {
        menu.push(drawInboxItem(message, state.inputs);
    }
    return [{
        key: "newChildren",
        value: {
            parentId: "page",
            children: menu}}]
}

const drawFunc = {
    "inbox": drawInbox,
    "write": drawWrite,
    "contacts": drawContacts,
    "outbox": drawOutbox,
    "drafts": drawDrafts,
    "pricing": drawPricing,
    "account": drawAccount,
    "help": drawHelp};


function drawPage(page, oldPage, state) {
    let buttonOn = [];
    let buttonOff = [];
    if (page !== maybeOldPage) {
        buttonOff =
            oldPage === undefined ?
            [] :
            turnButtonOff(oldPage + "Button");

        buttonOn = turnButtonOn(page + "Button");
    }
    const drawJobs = drawFunc[page](state);
    return drawJobs.concat(buttonOn).concat(buttonOff);
}

function updateOnCacheResponse(response, state) {
    switch (response.key) {
        case "page":
            if (response.value === null) {
                state.page = "inbox";
            }
            const oldPage = state.page;
            state.page = response.value;
            return [drawPage(response.value, oldPage, state), state];

        case "my name":
            if (response.value === null) {
                return [
                    [
                        {
                            key: "request name from server",
                            value: state.cryptoKeys,
                        },
                    ],
                    state,
                ];
            }
            state.myName = response.value;
            return [[{ key: "draw", value: state }], state];

        case "inbox":
            if (response.value === null) {
                state.inbox = [];
            }
            state.inbox = response.value;
            return [[{ key: "draw", value: state }], state];

        case "drafts":
            if (response.value === null) {
                state.drafts = [];
            }
            state.drafts = response.value;
            return [[{ key: "draw", value: state }], state];
    }
}

function oneByte(route) {
    let buffer = new ArrayBuffer(1);
    let view = new Uint8Array(buffer);
    view[0] = route;
    return view;
}

function update(input, state) {
    switch (input.key) {
        case "cache response":
            return updateOnCacheResponse(input.value, state);

        case "error":
            state.error = input.value;
            return [[{ key: "draw", value: state }], state];

        case "new name":
            state.myName = input.value;
            return [[{ key: "draw", value: state }], state];
    }
}

function formatHttpError(body, statusCode) {
    return;
    "bad response: " +
        response.status +
        ": " +
        String.fromCharChode.apply(null, arrToNums(body));
}

function noMessagesDom() {
    const p = document.createElement("p");
    p.textContent = "You have no messages yet.";
    p.classList.add("noneMessage");
    return p;
}

async function getKeys() {
    let keys = await localforage.getItem("crypto keys");
    if (keys === null) {
        keys = {
            signing: nacl.sign.keyPair(),
            box: nacl.box.keyPair(),
        };
        await localforage.setItem("crypto keys", keys);
    }
    return keys;
}

async function getPowInfo() {
    const [response, responseErr] = await apiRequest(oneByte(3));
    if (responseErr !== "") {
        return [{}, responseErr];
    }
    return [{ difficulty: response[0], unique: response.slice(1) }, ""];
}

async function apiRequest(requestBody) {
    const response: Response = await fetch("/api", {
        method: "POST",
        headers: { "Content-Type": "application/octet-stream" },
        body: requestBody,
    });

    const body = await response.arrayBuffer();
    const bodyArray = new Uint8Array(body);

    if (!response.ok) {
        return [{}, formatHttpError(bodyArray, response.status)];
    }

    return [bodyArray, ""];
}

async function requestName(maybeKeys, inputs) {
    const keys = maybeKeys === undefined ? await getKeys() : maybeKeys;

    let [powInfo, err] = await getPowInfo();
    if (err !== "") {
        inputs.push({ key: "error", value: err });
        return;
    }
    const pow = proofOfWork(powInfo);

    const request = makeMyNameRequest(pow, keys.signing.publicKey);
    const [response, responseErr] = await apiRequest(request);
    if (responseErr !== "") {
        return [0, err];
    }
    return [decodeInt(response), ""];
}

async function io(output, inputs) {
    switch (output.key) {
        case "cache query":
            const value = await localforage.getItem(output.value);
            inputs.push({
                key: "cache response",
                value: { key: output.value, value: value },
            });
            return;

        case "request name from server":
            requestName(output.value, inputs);
            return;

        case "addCssClass":
            const el = document.getElementById(output.value.id);
            el.classList.add(output.value.cssClass);
            return;

        case "removeCssClass":
            const el = document.getElementById(output.value.id);
            el.classList.remove(output.value.cssClass);
            return;

        case "init draw":
            initDraw(inputs);
            return;

        case "newChildren":
            const x = output.value;
            const parentEl = document.getElementById(x.parentId);
            while (parentEl.firstChild) {
                parentEl.removeChild(parentEl.lastChild);
            };
            for (child of x.children) {
                parentEl.appendChild(child);
            };
            return;

        case "addOnClick":
            const el = document.getElementById(output.value.id);
            el.onclick = output.value.onclick;
            return;
    }
}

function callback(input) {
    
}

let state = {};
let inputs = [];
let outputs = initOutputs(inputs);
let promises = [];
state.inputs = inputs;
while (true) {
    for (const output of outputs) {
        const numPromises = promises.length;
        async function ioHelp() {
            await io(output, inputs);
            return numPromises - 1;
        }
        promises.push(ioHelp());
    }
    await Promise.race(promises).then((i) => promises.splice(i, 1));
    for (const input of inputs) {
        let newOutputs;
        [newOutputs, state] = update(input, state);
        outputs.concat(newOutputs);
    }
    inputs = [];
}

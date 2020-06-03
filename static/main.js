"use strict";

function inboxMenuItem(message, inputs) {
    const container = document.createElement("button");
    container.type = "button";
    container.classList.add("inboxItem");
    container.appendChild(makeSubjectDom(message.subject));
    container.appendChild(makeFromDom(message.from));
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
        "help",
    ];
    let outputs = [];
    for (const button of buttons) {
        const clickMessage = { key: "topButtonClick", value: button };
        const onclick = function () {
            inputs.push(clickMessage);
        };
        outputs.push({
            key: "addOnclick",
            value: { id: button + "Button", onclick: onclick },
        });
    }
    return outputs;
}

function initOutputs(inputs) {
    return [
        { key: "cacheQuery", value: "iota" },
        { key: "cacheQuery", value: "page" },
        { key: "cacheQuery", value: "inboxIds" },
        { key: "cacheQuery", value: "draftIds" },
        { key: "cacheQuery", value: "outboxIds" },
        { key: "cacheQuery", value: "myName" },
        { key: "cacheQuery", value: "contacts" },
    ].concat(initOnClicks(inputs));
}

function makeSubjectDom(subject) {
    const p = document.createElement("p");
    if (subject !== undefined) {
        p.textContent = "Subject: " + subject;
        return p;
    }
    p.textContent = "No subject";
    p.classList.add("noneMessage");
    return p;
}

function makeToDom(to) {
    const p = document.createElement("p");
    p.textContent = "To: " + to;
    return p;
}

function makeFromDom(from) {
    const p = document.createElement("p");
    p.textContent = "From: " + from;
    return p;
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
        { key: "addCssClass", value: { id: id, cssClass: "selectedButton" } },
        {
            key: "removeCssClass",
            value: { id: id, cssClass: "notSelectedButton" },
        },
    ];
}

function turnButtonOff(id) {
    return [
        {
            key: "addCssClass",
            value: { id: id, cssClass: "notSelectedButton" },
        },
        {
            key: "removeCssClass",
            value: { id: id, cssClass: "selectedButton" },
        },
    ];
}

function drawInboxItem(message, inputs) {
    const button = document.createElement("button");
    button.type = "button";
    button.classList.add("messageButton");
    button.appendChild(makeSubjectDom(message.subject));
    button.appendChild(makeFromDom(message.from));
    button.onclick = function () {
        inputs.push({ key: "inboxMenuClick", value: message.id });
    };
    return button;
}

function drawInbox(state) {
    const inbox = [];
    for (message of state.inboxSummary) {
        inbox.push(drawInboxItem(message, state.inputs));
    }
    return [
        {
            key: "newChildren",
            value: { parentId: "page", children: inbox },
        },
    ];
}

function drawOutboxItem(message, inputs) {
    const button = document.createElement("button");
    button.type = "button";
    button.classList.add("messageButton");
    button.appendChild(makeSubjectDom(message.subject));
    button.appendChild(makeToDom(message.to));
    button.onclick = function () {
        inputs.push({ key: "outboxMenuClick", value: message.id });
    };
    return button;
}

// after here

function drawOutbox(state) {
    const outbox = [];
    for (message of state.outboxSummary) {
        outbox.push(drawOutboxItem(message, state.inputs));
    }
    return [
        {
            key: "newChildren",
            value: { parentId: "page", children: outbox },
        },
    ];
}

function makeDraftToDom(to) {
    const p = document.createElement("p");
    if (to !== undefined) {
        p.textContent = "To: " + to;
        return p;
    }
    p.textContent = "No recipient";
    p.classList.add("noneMessage");
    return p;
}

function drawDraftsItem(draft, inputs) {
    const button = document.createElement("button");
    button.type = "button";
    button.classList.add("messageButton");
    button.appendChild(makeSubjectDom(draft.subject));
    button.appendChild(makeDraftToDom(message.to));
    button.onclick = function () {
        inputs.push({ key: "draftsMenuClick", value: draft.id });
    };
    return button;
}

function drawDrafts(state) {
    const drafts = [];
    for (draft of state.draftsSummary) {
        drafts.push(drawDraftsItem(draftSummary, state.inputs));
    }
    return [
        {
            key: "newChildren",
            value: { parentId: "page", children: drafts },
        },
    ];
}

function makeSubjectBox(subject, inputs) {
    const id = "writerSubjectBox";
    const container = document.createElement("div");
    const label = document.createElement("label");
    label.setAttribute("for", id);
    label.innerHTML = "Subject";
    container.appendChild(label);

    const box = document.createElement("input");
    box.type = "text";
    box.value = subject;
    box.oninput = (e) => callback("updatedSubjectBox", e.target.value, inputs);
    box.id = id;
    container.appendChild(box);
    return container;
}

function makeToBox(to, inputs) {
    const id = "writerToBox";
    const container = document.createElement("div");

    const label = document.createElement("label");
    label.setAttribute("for", id);
    label.innerHTML = "To";
    container.appendChild(label);

    const box = document.createElement("input");
    box.type = "text";
    box.value = to;
    box.oninput = (e) => callback("updatedToBox", e.target.value, inputs);
    box.id = id;
    container.appendChild(box);
    return container;
}

function addContactBox(boxContents, inputs) {
    const id = "addContactBox";
    const container = document.createElement("div");

    const label = document.createElement("label");
    label.setAttribute("for", id);
    label.innerHTML = "Add a new contact";
    container.appendChild(label);

    const box = document.createElement("input");
    box.type = "text";
    box.value = boxContents;
    box.oninput = (e) =>
        callback("updatedAddContactBox", e.target.value, inputs);
    box.id = id;
    container.appendChild(box);
    return container;
}

function longestRow(rows) {
    let longest = 0;
    for (row of rows) {
        const length = row.length;
        if (length > longest) {
            longest = length;
        }
    }
    return longest;
}

function makeUserInputBox(userInput, inputs) {
    const id = "writerUserInputBox";
    const container = document.createElement("div");
    const label = document.createElement("label");
    label.setAttribute("for", id);
    label.innerHTML = "Message";
    container.appendChild(label);

    const box = document.createElement("textarea");
    const rows = userInput.split("\n");
    box.cols = longestRow(rows);
    box.rows = rows.length;
    box.oninput = (e) => callback("updatedUserInput", e.target.value, inputs);
    box.id = id;
    container.appendChild(box);
    return container;
}

function codeUploaderHelp(inputs) {
    const id = "writerCodeUploader";
    const container = document.createElement("div");
    const label = document.createElement("label");
    label.setAttribute("for", id);
    label.innerHTML = "Upload code";
    container.appendChild(label);

    const browse = document.createElement("input");
    browse.type = "file";
    browse.id = id;
    browse.addEventListener(
        "change",
        () => callback("codeFilesUpload", this.files, inputs),
        false
    );
    container.appendChild(browse);
    return container;
}

function prettyBytes(n) {
    if (n < 1000) {
        return n + "B";
    }

    if (n < 1000000) {
        return Math.round(n / 1000) + "KB";
    }

    if (n < 1000000000) {
        return Math.round(n / 1000000) + "MB";
    }
}

function makeCodeUploader(code, inputs) {
    if (code === undefined) {
        return codeUploaderHelp(state.inputs);
    }

    const div = document.createElement("div");
    div.id = "codeUploader";

    const title = document.createElement("h1");
    title.textContent("Message program");
    div.appendChild(title);

    const filename = document.createElement("span");
    filename.textContent = code.filename;
    div.appendChild(filename);

    const size = document.createElement("span");
    size.textContent = "Size: " + prettyBytes(code.size);
    div.appendChild(size);

    const deleteButton = document.createElement("button");
    deleteButton.type = "button";
    deleteButton.onclick = function () {
        callback("deleteCode", code.draftId, inputs);
    };
    deleteButton.textContent = "Delete";
    div.appendChild(deleteButton);

    return div;
}

function drawWrite(state) {
    let draft;
    if (state.openedDraft === undefined) {
        state.openedDraft = {
            to: "",
            subject: "",
            userInput: "",
            blobIds: [],
        };
    } else {
        draft = state.openedDraft;
    }

    const children = [];

    const subjectBox = makeSubjectBox(draft.subject, state.inputs);
    children.push(subjectBox);

    const toBox = makeToBox(draft.to, state.inputs);
    children.push(toBox);

    const userInput = makeUserInputBox(draft.userInput, state.inputs);
    children.push(userInput);

    const codeUploader = makeCodeUploader(draft.code, state.inputs);
    children.push(codeUploader);

    return [
        {
            key: "newChildren",
            value: { parentId: "page", children: children },
        },
    ];
}

function drawContact(contact, inputs) {
    const div = document.createElement("div");
    div.classList.add("contactView");

    const name = document.createElement("span");
    name.textContent = contact;
    div.appendChild(name);

    const deleteButton = document.createElement("button");
    deleteButton.textContent = "Delete";
    deleteButton.onclick = function () {
        callback("deleteContact", contact, inputs);
    };
    div.append(deleteButton);

    return div;
}

function drawContacts(state) {
    const children = [];

    const myName = state.myName === undefined ? "Requesting..." : state.myName;
    children.push(myNameDom(myName));

    children.push(addContactBox(state.addContactBox, state.inputs));

    const h1 = document.createElement("h1");
    h1.textContent = "My contacts";
    children.append(h1);

    for (contact of state.contacts) {
        children.push(drawContact(contact, state.inputs));
    }
    return [
        {
            key: "newChildren",
            value: { parentId: "page", children: children },
        },
    ];
}

function drawPricing(state) {
    const span = document.createElement("span");
    span.textContent = "TODO";
    return [
        { key: "newChildren", value: { parentId: "page", children: [span] } },
    ];
}

function drawAccount(state) {
    const span = document.createElement("span");
    span.textContent = "TODO";
    return [
        { key: "newChildren", value: { parentId: "page", children: [span] } },
    ];
}

function drawHelp(state) {
    const span = document.createElement("span");
    span.textContent = "TODO";
    return [
        { key: "newChildren", value: { parentId: "page", children: [span] } },
    ];
}

const drawFunc = {
    inbox: drawInbox,
    write: drawWrite,
    contacts: drawContacts,
    outbox: drawOutbox,
    drafts: drawDrafts,
    pricing: drawPricing,
    account: drawAccount,
    help: drawHelp,
};

function drawPage(page, oldPage, state) {
    let buttonOn = [];
    let buttonOff = [];
    if (page !== maybeOldPage) {
        buttonOff =
            oldPage === undefined ? [] : turnButtonOff(oldPage + "Button");

        buttonOn = turnButtonOn(page + "Button");
    }
    const drawJobs = drawFunc[page](state);
    return drawJobs.concat(buttonOn).concat(buttonOff);
}

function oneByte(route) {
    let buffer = new ArrayBuffer(1);
    let view = new Uint8Array(buffer);
    view[0] = route;
    return view;
}

function myNameDom(myName) {
    const p = document.createElement("p");
    p.textContent = "My username is: " + myName;
    return p;
}

function myNameFromCache(maybeMyName, state) {
    if (maybeMyName === null) {
        return [[{ key: "requestMyName", value: state.cryptoKeys }], state];
    }
    state.myName = maybeMyName;
    if (state.page === "contacts") {
        const outputs = [
            {
                key: "newChildren",
                value: { parentId: "myName", children: [maybeMyName] },
            },
        ];
        return [outputs, state];
    }
    return [[], state];
}

function inboxIdsFromCache(inboxIds, state) {
    if (inbox === null) {
        state.inbox = [];
    }
    state.inboxIds = inbox;
    return [[{ key: "draw", value: state }], state];
}

function draftIdsFromCache(draftsIds, state) {
    if (draftIds === null) {
        state.draftIds = [];
    }
    state.draftIds = draftIds;
    return [[{ key: "draw", value: state }], state];
}

function outboxIdsFromCache(outboxIds, state) {
    if (outboxIds === null) {
        state.outboxIds = [];
    }
    state.outboxIds = outboxIds;
    return [[{ key: "draw", value: state }], state];
}

function pageFromCache(page, state) {
    state.page = page;
    if (page === "inbox" || page === null) {
        if (
            state.openedMessage === undefined &&
            state.inboxSummary === undefined
        ) {
            return [[{ key: "getInboxSummary", value: state.inboxIds }], state];
        }
    }
    if (
        page === "drafts" &&
        state.openedDraft === undefined &&
        state.draftsSummary === undefined
    ) {
        return [[{ key: "getDraftsSummary", value: state.draftIds }], state];
    }
    if (
        page === "outbox" &&
        state.openedSent === undefined &&
        state.outboxSummary === undefined
    ) {
        return [[{ key: "getOutboxSummary", value: state.outboxIds }], state];
    }
    const oldPage = state.page;
    return [drawPage(page, oldPage, state), state];
}

function iotaFromCache(iota, state) {
    state.iota = iota === null ? 0 : iota;
    return [[], state];
}

function contactsFromCache(contacts, state) {
    if (contacts === null) {
        return [[], state];
    }
    state.contacts = contacts;
    return [[], state];
}

const updateOnCacheResponseSwitch = {
    page: pageFromCache,
    myName: myNameFromCache,
    inboxIds: inboxIdsFromCache,
    draftIds: draftIdsFromCache,
    outboxIds: outboxIdsFromCache,
    iotaIds: iotaFromCache,
    contacts: contactsFromCache,
};

function updateError(error, state) {
    state.error = error;
    return [[{ key: "draw", value: state }], state];
}

function updateNewName(newName, state) {
    state.myName = newName;
    return [[{ key: "draw", value: state }], state];
}

function setItem(key, value) {
    return {
        key: "cacheValue",
        value: { key: key, value: value },
    };
}

function newSubject(draftId, draftsSummary, subject) {
    let draftSummary;
    for (draft of draftsSummary) {
        if (draft.id === draftId) {
            draft.subject = subject;
            break;
        }
    }
    return draftsSummary;
}


function updatedSubjectBox(subject, state) {
    if (state.openDraft === undefined) {
        return [[], state];
    }
    if (state.openDraft.id === undefined) {
        state.openDraft.id = state.iota.toString();
        state.iota += 1;
    }
    state.openDraft.subject = subject;
    state.draftsSummary = newSubject(
        state.openDraft.id, state.draftsSummary, subject);
    const ioJobs = [
        {
            key: "updateTextBox",
            value: { id: "writerSubjectBox", value: subject },
        },
        setItem("iota", state.iota),
        setItem(state.openDraft.id, state.openDraft),
    ];
    return [ioJobs, state];
}

function validRecipient(recipient) {
    if (recipient === "") {
        return false;
    }
    if (recipient === "0") {
        return true;
    }
    if (recipient[0] === "0") {
        return false;
    }
    const digits = Set(["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]);
    for (const c of recipient) {
        if (!digits.has(c)) {
            return false;
        }
    }
    return true;
}

function updatedToBox(to, state) {
    if (state.openDraft === undefined) {
        return [[], state];
    }
    if (!validRecipient(to)) {
        return [
            [{ key: "updateTextBox", value: { id: "writerToBox", value: "" } }],
            state,
        ];
    }
    if (state.openDraft.id === undefined) {
        state.openDraft.id = state.iota.toString();
        state.iota += 1;
    }
    state.openDraft.to = to;
    const ioJobs = [
        { key: "updateTextBox", value: { id: "writerToBox", value: to } },
        setItem("iota", state.iota),
        setItem(state.openDraft.id, state.openDraft),
    ];
    return [ioJobs, state];
}

function updateOnDraftsSummary(draftsSummary, state) {
    if (state.page !== "drafts" || state.openedDraft !== undefined) {
        return [[], state];
    }
    state.draftsSummary = draftsSummary;
    return [drawDrafts(state), state];
}

function updateOnOutboxSummary(outboxSummary, state) {
    if (state.page !== "outbox" || state.openedSent !== undefined) {
        return [[], state];
    }
    state.outboxSummary = outboxSummary;
    return [drawOutbox(state), state];
}

function updateOnInboxSummary(inboxSummary, state) {
    if (state.page !== "inbox" || state.openedMessageIn !== undefined) {
        return [[], state];
    }
    state.inboxSummary = inboxSummary;
    return [drawInbox(state), state];
}

function updateOnCacheResponse(response, state) {
    return updateOnCacheResponseSwitch[response.key](response.value, state);
}

function updateOnAddContactButtonClick(dontCare, state) {
    const contact = state.addContactBox;
    if (state.contacts.has(contact)) {
        const err = contact + " is already in your contacts";
        state.addContactError = err;
        return [[{key: "addContactError", value: err}], state];
    }
    state.contacts.add(contact);
    state.addContactBox = "";
    return [
        [{key: "updateTextBox",
          value: {id: "addContactBox", value: ""}}],
        state];
}

function updatedAddContactBox(contact, state) {
    if (!validRecipient(contact)) {
        return [
            [{ key: "updateTextBox", value: { id: "addContactBox", value: ""}}], state]
    }
    state.addContactBox = contact;
    return [{key: "updateTextBox", value: { id: "addContactBox", value: contact}}], state];
}

function updatedUserInput(userInput, state) {
    if (state.openDraft === undefined) {
        return [[], state];
    }
    if (state.openDraft.id === undefined) {
        state.openDraft.id = state.iota;
        state.iota +=1 ;
    }
    state.openDraft.userInput = userInput;
    const ioJobs =[
        {
            key: "updatedTextBox",
            value: {id: "writerUserInputBox", value: userInput}},
        setItem("iota", state.iota),
        setItem(state.openDraft.id, state.openDraft)];
    return [ioJobs, state];
}

function updateOnCodeUpload(code, state) {
    if (state.openedDraft === undefined) {
        return [[], state];
    }
    if (state.openDraft.id === undefined) {
        state.openDraft.id = state.iota;
        state.iota += 1;
    }
    state.openDraft.code = code;
    const ioJobs = [
        { key: "replaceDomWith",
          value: {
            id: "codeUploader",
            newDom: makeCodeUploader(code, state.inputs)}},
        setItem("iota", state.iota),
        setItem(state.openDraft.id, state.openDraft)];
    return [ioJobs, state];
}

function updateOnDeleteCode(draftId, state) {
    if (state.openDraft === undefined) {
        return [[], state];
    }
    const openDraft = state.openDraft;
    delete openDraft.code;
    const ioJobs = [
        {key: "replaceDomWith",
         value: {
            id: "codeUploader",
            newDom: makeCodeUploader(undefined, state.inputs)}},
        setItem(state.openDraft.id, state.openDraft)];
    return [ioJobs, state];
}

function updateOnDeleteContact(contact, state) {
    state.contacts.delete(contact);
    return [
        drawContacts(state).push(
            setItem("contacts", state.contacts)),
        state];
}

const update = {
    cacheResponse: updateOnCacheResponse,
    error: updateError,
    myNewName: updateNewName,
    updatedSubjectBox: updatedSubjectBox,
    updatedUserInput: updatedUserInput,
    updatedToBox: updatedToBox,
    draftsSummary: updateOnDraftsSummary,
    outboxSummary: updateOnOutboxSummary,
    inboxSummary: updateOnInboxSummary,
    addContactButtonClick: updateOnAddContactButtonClick,
    updatedAddContactBox: updatedAddContactBox,
    uploadedCodeFile: updateOnCodeUpload,
    deleteCode: updateOnDeleteCode,
    deleteContact: updateOnDeleteContact,
};

function formatHttpError(body, statusCode) {
    return (
        "bad response: " +
        response.status +
        ": " +
        String.fromCharChode.apply(null, arrToNums(body))
    );
}

function noMessagesDom() {
    const p = document.createElement("p");
    p.textContent = "You have no messages yet.";
    p.classList.add("noneMessage");
    return p;
}

async function getKeys() {
    let keys = await localforage.getItem("cryptoKeys");
    if (keys === null) {
        keys = {
            signing: nacl.sign.keyPair(),
            box: nacl.box.keyPair(),
        };
        await localforage.setItem("cryptoKeys", keys);
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
    const response = await fetch("/api", {
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

function addCssClass(toAdd, inputs) {
    const el = document.getElementById(toAdd.id);
    el.classList.add(toAdd.cssClass);
}

function removeCssClass(toRemove, inputs) {
    const el = document.getElementById(toRemove.id);
    el.classList.remove(toRemove.cssClass);
}

async function requestMyName(maybeKeys, inputs) {
    const keys = maybeKeys === undefined ? await getKeys() : maybeKeys;

    let [powInfo, err] = await getPowInfo();
    if (err !== "") {
        callback("error", err, inputs);
        return;
    }
    const pow = proofOfWork(powInfo);

    const request = makeMyNameRequest(pow, keys.signing.publicKey);
    const [response, responseErr] = await apiRequest(request);
    if (responseErr !== "") {
        callback("error", responseErr, inputs);
        return;
    }
    callback("myNewName", decodeInt(response), inputs);
}

async function cacheQuery(key, inputs) {
    const value = await localforage.getItem(key);
    callback("cacheResponse", { key: key, value: value }, inputs);
}

function newChildren(key, dontCare) {
    const parentEl = document.getElementById(key.parentId);
    while (parentEl.firstChild) {
        parentEl.removeChild(parentEl.lastChild);
    }
    for (child of key.children) {
        parentEl.appendChild(child);
    }
}

function addOnclick(key, dontCare) {
    const el = document.getElementById(key.id);
    el.onclick = key.onclick;
}

function cacheValue(toCache, dontCare) {
    localforage.setItem(toCache.key, toCache.value);
}

function updateTextBox(toAdd, dontCare) {
    const box = document.getElementById(toAdd.id);
    box.value = toAdd.value;
}

async function getInboxSummary(inboxIds, inputs) {
    const summaries = [];
    for (const id of inboxIds) {
        const message = await localforage.getItem(id);
        const summary = {
            subject: message.subject,
            id: id,
            from: message.from,
            time: message.time,
        };
        summaries.push(summary);
    }
    callback("inboxSummary", summaries, inputs);
}

async function getDraftsSummary(draftIds, inputs) {
    const summaries = [];
    for (const id of draftIds) {
        const draft = await localforage.getItem(id);
        const summary = {
            subject: draft.subject,
            id: id,
            to: draft.to,
        };
        summaries.push(summary);
    }
    callback("draftsSummary", summaries, inputs);
}

async function getOutboxSummary(outboxIds, inputs) {
    const summaries = [];
    for (const id of outboxIds) {
        const message = await localforage.getItem(id);
        const summary = {
            subject: message.subject,
            id: id,
            to: message.to,
            time: message.time,
        };
        summaries.push(summary);
    }
    callback("outboxSummary", summaries, inputs);
}

async function codeFilesUpload(files, inputs) {
    const file = files[0];
    const contents = await file.arrayBuffer();
    callback(
        "uploadedCodeFile",
        {contents: contents,
         name: file.name,
         size: file.size,
         mime: file.type},
        inputs);
}

function replaceDomWith(newDom, dontCare) {
    const old = document.getElementById(newDom.id);
    old.replaceWith(newDom.newDom);
}

const io = {
    cacheQuery: cacheQuery,
    requestMyName: requestMyName,
    addCssClass: addCssClass,
    removeCssClass: removeCssClass,
    newChildren: newChildren,
    addOnclick: addOnclick,
    cacheValue: cacheValue,
    updateTextBox: updateTextBox,
    getInboxSummary: getInboxSummary,
    getDraftsSummary: getDraftsSummary,
    getOutboxSummary: getOutboxSummary,
    codeFilesUpload: codeFilesUpload,
    replaceDomWith: replaceDomWith,
};

function callback(key, value, inputs) {
    inputs.push({ key: key, value: value });
    mainTick();
}

let mainTick;
{
    let state = {};
    const inputs = [];
    const outputs = initOutputs(inputs);
    state.inputs = inputs;

    mainTick = () => {
        for (const output of outputs) {
            debugger;
            const iof = io[output.key];
            iof(output.value, inputs);
        }
        for (const input of inputs) {
            let newOutputs;
            [newOutputs, state] = update[input.key](input.value, state);
            outputs.concat(newOutputs);
        }
        inputs.length = 0;
    };
}
mainTick();

"use strict";

function initOnClick(button) {
    return {
        key: "addOnclick",
        value: {
            id: button + "Button",
            onclick: () => tick("topButtonClick", button),
        },
    };
}

function initOnClicks() {
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
    const outputs = [];
    for (const button of buttons) {
        outputs.push(initOnClick(button));
    }
    return outputs;
}

function initOutputs() {
    return [
        { key: "cacheQuery", value: "iota" },
        { key: "cacheQuery", value: "page" },
        { key: "cacheQuery", value: "inboxIds" },
        { key: "cacheQuery", value: "draftIds" },
        { key: "cacheQuery", value: "outboxIds" },
        { key: "cacheQuery", value: "myName" },
        { key: "cacheQuery", value: "contacts" },
    ].concat(initOnClicks());
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
    const buf = new ArrayBuffer(lena + lenb);
    const combined = new Uint8Array(buf);
    for (let i = 0; i < lena; i++) {
        combined[i] = a[i];
    }
    for (let i = lena; i < lena + lenb; i++) {
        const bval = b[i - lena];
        combined[i] = bval;
    }
    return combined;
}

function decodeInt32(fourBytes) {
    let result = 0;
    for (let i = 0; i < 4; i++) {
        result += fourBytes[i] * Math.pow(256, i);
    }
    return result;
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
    const buffer = new ArrayBuffer(8);
    const bufferView = new Uint8Array(buffer);
    const counter = new Int32Array(buffer);
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
    const buffer = new ArrayBuffer(length);
    return new Uint8Array(buffer);
}

function makeMyNameRequest(pow, publicSigningKey) {
    const request = uint8Array(49);
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

function makeSubjectView(subject) {
    const p = document.createElement("p");
    if (subject === undefined) {
        p.textContent = "No subject";
        p.classList.add("noneMessage");
        return p;
    }
    p.textContent = subject;
    return p;
}

function makeFromView(from) {
    const p = document.createElement("p");
    p.textContent = from;
    return p;
}

function decodeSmallString(raw, i) {
    const rawLen = raw.length;
    if (rawLen < 4 + i) {
        return [
            null,
            "smallstring at position " +
                i +
                " is only " +
                (rawLen - i) +
                " bytes long",
        ];
    }
    const stringLength = decodeInt32(raw.slice(i, i + 4));
    i += 4;
    const stringBytes = raw.slice(i, i + stringLength);
    const decoded = new TextDecoder().decode(stringBytes);
    return [{ key: "smallString", value: decoded }, ""];
}

function decodeOrdering(raw, i) {
    const rawLen = raw.length;
    if (rawLen < 4 + i) {
        return [
            {},
            "ordering at position " +
                i +
                " is only " +
                (rawLen - i) +
                " bytes long",
        ];
    }
    const numElements = decodeInt32(raw.slice(i, i + 4));
    i += 4;
    const ordering = [];
    for (let _ = 0; _ < numElements; _++) {
        let doc, err;
        [doc, err, i] = decodeDocumentHelp(raw, i);
        if (err !== "") {
            return [null, err, i];
        }
        ordering.push(doc);
    }
    return [{ key: "ordering", value: ordering }, "", i];
}

function decodeDocument(raw) {
    const [doc, err, i] = decodeDocumentHelp(raw, 0);
    if (err !== "") {
        return [null, err + ": " + i];
    }
    const rawLength = raw.length;
    if (i < rawLength - 1) {
        return [
            null,
            "could not decode whole document: stopped at position " + i,
        ];
    }
    return [doc, ""];
}

function decodeDocumentHelp(raw, i) {
    if (raw.length === 0) {
        return [{}, "empty"];
    }
    const indicator = raw[0];
    switch (indicator) {
        case 0:
            return decodeOrdering(raw, 1);
        case 1:
            return decodeSmallString(raw, 1);
    }
    return [null, "bad indicator: " + indicator, i];
}

function makeOutputErr(err) {
    const p = document.createElement("p");
    p.textContent = "Internal error in message program: " + err;
    return p;
}

function makeSmallStringView(smallString) {
    const pre = document.createElement("pre");
    pre.textContent = smallString;
    return pre;
}

function makeOrderingView(ordering) {
    const div = document.createElement("div");
    for (const o of ordering) {
        const el = makeOutputViewHelp(o);
        div.appendChild(el);
    }
    return div;
}

function makeOutputViewHelp(doc) {
    switch (doc.key) {
        case "smallString":
            return makeSmallStringView(doc.value);
        case "ordering":
            return makeOrderingView(doc.value);
    }
}

function makeOutputView(output) {
    const [doc, decodeErr] = decodeDocument(output, 0);
    if (decodeErr !== "") {
        return makeOutputErr(decodeErr);
    }
    return makeOutputViewHelp(doc);
}

function makeUserInputView(userInput) {
    const pre = document.createElement("pre");
    pre.textContent = userInput;
    return pre;
}

function makeBlobView(blob) {
    const div = document.createElement("div");

    const name = document.createElement("p");
    name.textContent = blob.name;

    const size = document.createElement("p");
    size.textContent = prettyBytes(blob.size);

    const mime = document.createElement("p");
    mime.textContent = "File type: " + blob.mime;

    return div;
}

function makeCodeView(code) {
    const div = document.createElement("div");

    const name = document.createElement("p");
    name.textContent = code.name;

    const size = document.createElement("p");
    size.textContent = prettyBytes(code.size);

    return div;
}

function makeBlobsView(blobs) {
    const div = document.createElement("div");
    for (const blob of blobs) {
        div.appendChild(makeBlobView(blob));
    }
    return div;
}

function drawInboxItemView(message) {
    const children = [
        makeSubjectView(message.subject),
        makeFromView(message.from),
        makeOutputView(message.output),
        makeUserInputView(message.userInput),
        makeCodeView(message.code),
    ];

    if (message.blobs !== undefined) {
        children.push(makeBlobsView(message.blobs));
    }

    return [
        { key: "newChildren", value: { parentId: "page", children: children } },
    ];
}

function drawInboxMenuItem(message) {
    const button = document.createElement("button");
    button.type = "button";
    button.classList.add("messageButton");
    button.appendChild(makeSubjectDom(message.subject));
    button.appendChild(makeFromDom(message.from));
    button.onclick = () => tick("inboxMenuClick", message.id);
    return button;
}

function drawInbox(state) {
    if (state.inboxSummary.length === 0) {
        return [
            {
                key: "newChildren",
                value: { parentId: "page", children: [noMessagesDom()] },
            },
        ];
    }
    if (state.inboxItem !== undefined) {
        return drawInboxItemView(state.openedInboxItem);
    }
    const inbox = [];
    for (const message of state.inboxSummary) {
        inbox.push(drawInboxMenuItem(message));
    }
    return [
        {
            key: "newChildren",
            value: { parentId: "page", children: inbox },
        },
    ];
}

function drawOutboxItem(message) {
    const button = document.createElement("button");
    button.type = "button";
    button.classList.add("messageButton");
    button.appendChild(makeSubjectDom(message.subject));
    button.appendChild(makeToDom(message.to));
    button.onclick = () => tick("outboxMenuClick", message.id);
    return button;
}

function drawOutbox(state) {
    const outbox = [];
    for (const message of state.outboxSummary) {
        outbox.push(drawOutboxItem(message));
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

function drawDraftsItem(draft) {
    const button = document.createElement("button");
    button.type = "button";
    button.classList.add("messageButton");
    button.appendChild(makeSubjectDom(draft.subject));
    button.appendChild(makeDraftToDom(draft.to));
    button.onclick = () => tick("draftsMenuClick", draft.id);
    return button;
}

function drawDrafts(state) {
    const drafts = [];
    for (const draftSummary of state.draftsSummary) {
        drafts.push(drawDraftsItem(draftSummary));
    }
    return [
        {
            key: "newChildren",
            value: { parentId: "page", children: drafts },
        },
    ];
}

function makeSubjectBox(subject) {
    const id = "writerSubjectBox";
    const container = document.createElement("div");
    const label = document.createElement("label");
    label.setAttribute("for", id);
    label.innerHTML = "Subject";
    container.appendChild(label);

    const box = document.createElement("input");
    box.type = "text";
    box.value = subject;
    box.oninput = (e) => tick("updatedSubjectBox", e.target.value);
    box.id = id;
    container.appendChild(box);
    return container;
}

function makeToBox(to) {
    const id = "writerToBox";
    const container = document.createElement("div");

    const label = document.createElement("label");
    label.setAttribute("for", id);
    label.innerHTML = "To";
    container.appendChild(label);

    const box = document.createElement("input");
    box.type = "text";
    box.value = to;
    box.oninput = (e) => tick("updatedToBox", e.target.value);
    box.id = id;
    container.appendChild(box);
    return container;
}

function addContactBox(boxContents) {
    const id = "addContactBox";
    const container = document.createElement("div");

    const label = document.createElement("label");
    label.setAttribute("for", id);
    label.innerHTML = "Add a new contact";
    container.appendChild(label);

    const box = document.createElement("input");
    box.type = "text";
    box.value = boxContents;
    box.oninput = (e) => tick("updatedAddContactBox", e.target.value);
    box.id = id;
    container.appendChild(box);
    return container;
}

function longestRow(rows) {
    let longest = 0;
    for (const row of rows) {
        const length = row.length;
        if (length > longest) {
            longest = length;
        }
    }
    return longest;
}

function makeUserInputBox(userInput) {
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
    box.oninput = (e) => tick("updatedUserInput", e.target.value);
    box.id = id;
    container.appendChild(box);
    return container;
}

function codeUploaderHelp() {
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
        () => tick("codeFilesUpload", this.files),
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

function makeCodeUploader(code) {
    if (code === undefined) {
        return codeUploaderHelp();
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
    deleteButton.onclick = () => tick("deleteCode", code.draftId);
    deleteButton.textContent = "Delete";
    div.appendChild(deleteButton);

    return div;
}

function makeBlobUploader(blobs) {
    const id = "writerBlobUploader";
    const container = document.createElement("div");

    const label = document.createElement("label");
    label.setAttribute("for", id);
    label.innerHTML = "Attach a file";
    container.appendChild(label);

    const browse = document.createElement("input");
    browse.type = "file";
    browse.id = id;
    browse.multiple = true;
    browse.addEventListener(
        "change",
        () => tick("blobFilesUpload", this.files),
        false
    );
    container.appendChild(browse);

    return container;
}

function drawWrite(state) {
    let draft = state.openDraft;
    if (draft === undefined) {
        draft = {
            to: "",
            subject: "",
            userInput: "",
            blobIds: [],
        };
    }

    const children = [
        makeSubjectBox(draft.subject),
        makeToBox(draft.to),
        makeUserInputBox(draft.userInput),
        makeCodeUploader(draft.code),
        makeBlobUploader(draft.blobs),
    ];

    return [
        {
            key: "newChildren",
            value: { parentId: "page", children: children },
        },
    ];
}

function drawContact(contact) {
    const div = document.createElement("div");
    div.classList.add("contactView");

    const name = document.createElement("span");
    name.textContent = contact;
    div.appendChild(name);

    const deleteButton = document.createElement("button");
    deleteButton.textContent = "Delete";
    deleteButton.onclick = () => tick("deleteContact", contact);
    div.append(deleteButton);

    return div;
}

function drawContacts(state) {
    const children = [];

    const myName = state.myName === undefined ? "Requesting..." : state.myName;
    children.push(myNameDom(myName));

    children.push(addContactBox(state.addContactBox));

    const h1 = document.createElement("h1");
    h1.textContent = "My contacts";
    children.append(h1);

    for (const contact of state.contacts) {
        children.push(drawContact(contact));
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

function drawPage(oldPage, state) {
    let buttonOn = [];
    let buttonOff = [];
    if (state.page !== oldPage) {
        buttonOff =
            oldPage === undefined ? [] : turnButtonOff(oldPage + "Button");

        buttonOn = turnButtonOn(state.page + "Button");
    }
    const drawJobs = drawFunc[state.page](state);
    return drawJobs.concat(buttonOn).concat(buttonOff);
}

function oneByte(route) {
    const buffer = new ArrayBuffer(1);
    const view = new Uint8Array(buffer);
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
    if (inboxIds === null) {
        state.inboxIds = [];
    }
    state.inboxIds = inboxIds;
    return [[{ key: "draw", value: state }], state];
}

function draftIdsFromCache(draftIds, state) {
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
            state.openedInboxItem === undefined &&
            state.inboxSummary === undefined
        ) {
            return [[{ key: "getInboxSummary", value: state.inboxIds }], state];
        }
    }
    if (
        page === "drafts" &&
        state.openDraft === undefined &&
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
    state.page = page;
    return [drawPage(oldPage, state), state];
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
    for (const draft of draftsSummary) {
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
        state.openDraft.id,
        state.draftsSummary,
        subject
    );
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
    if (state.page !== "drafts" || state.openDraft !== undefined) {
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
    if (state.page !== "inbox" || state.openedInboxItem !== undefined) {
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
        return [[{ key: "addContactError", value: err }], state];
    }
    state.contacts.add(contact);
    state.addContactBox = "";
    return [
        [{ key: "updateTextBox", value: { id: "addContactBox", value: "" } }],
        state,
    ];
}

function updatedAddContactBox(contact, state) {
    if (!validRecipient(contact)) {
        return [
            [
                {
                    key: "updateTextBox",
                    value: { id: "addContactBox", value: "" },
                },
            ],
            state,
        ];
    }
    state.addContactBox = contact;
    return [
        [
            {
                key: "updateTextBox",
                value: { id: "addContactBox", value: contact },
            },
        ],
        state,
    ];
}

function updatedUserInput(userInput, state) {
    if (state.openDraft === undefined) {
        return [[], state];
    }
    if (state.openDraft.id === undefined) {
        state.openDraft.id = state.iota;
        state.iota += 1;
    }
    state.openDraft.userInput = userInput;
    const ioJobs = [
        {
            key: "updatedTextBox",
            value: { id: "writerUserInputBox", value: userInput },
        },
        setItem("iota", state.iota),
        setItem(state.openDraft.id, state.openDraft),
    ];
    return [ioJobs, state];
}

function updateOnCodeUpload(code, state) {
    if (state.openDraft === undefined) {
        return [[], state];
    }
    if (state.openDraft.id === undefined) {
        state.openDraft.id = state.iota;
        state.iota += 1;
    }
    state.openDraft.code = code;
    const ioJobs = [
        {
            key: "replaceDomWith",
            value: {
                id: "codeUploader",
                newDom: makeCodeUploader(code),
            },
        },
        setItem("iota", state.iota),
        setItem(state.openDraft.id, state.openDraft),
    ];
    return [ioJobs, state];
}

function updateOnDeleteCode(draftId, state) {
    if (state.openDraft === undefined) {
        return [[], state];
    }
    const openDraft = state.openDraft;
    delete openDraft.code;
    const ioJobs = [
        {
            key: "replaceDomWith",
            value: {
                id: "codeUploader",
                newDom: makeCodeUploader(undefined),
            },
        },
        setItem(state.openDraft.id, state.openDraft),
    ];
    return [ioJobs, state];
}

function updateOnDeleteContact(contact, state) {
    state.contacts.delete(contact);
    return [
        drawContacts(state).push(setItem("contacts", state.contacts)),
        state,
    ];
}

function updateOnTopButtonClick(button, state) {
    if (state.page === button) {
        return [[], state];
    }

    const oldPage = state.page;
    state.page = button;

    return [drawPage(oldPage, state).push(setItem("page", button)), state];
}

function updateOnInboxMenuClick(messageId, state) {
    return [[{ key: "lookupInboxMessage", value: messageId }], state];
}

function updateOnInit(dontCare, state) {
    return [initOutputs(), state];
}

function updateOnLookedUpInboxMessage(message, state) {
    if (state.page !== "inbox") {
        return [[], state];
    }
    state.openedInboxItem = message;
    return [drawInbox(state), state];
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
    topButtonClick: updateOnTopButtonClick,
    inboxMenuClick: updateOnInboxMenuClick,
    init: updateOnInit,
    lookedUpInboxMessage: updateOnLookedUpInboxMessage,
};

function arrToNums(arr) {
    const numbers = [];
    const lenArr = arr.length;
    for (let i = 0; i < lenArr; i++) {
        numbers.push(arr[i]);
    }
    return numbers;
}

function formatHttpError(body, statusCode) {
    return (
        "bad response: " +
        statusCode +
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

function addCssClass(toAdd) {
    const el = document.getElementById(toAdd.id);
    el.classList.add(toAdd.cssClass);
}

function removeCssClass(toRemove) {
    const el = document.getElementById(toRemove.id);
    el.classList.remove(toRemove.cssClass);
}

async function requestMyName(maybeKeys) {
    const keys = maybeKeys === undefined ? await getKeys() : maybeKeys;

    const [powInfo, err] = await getPowInfo();
    if (err !== "") {
        tick("error", err);
        return;
    }
    const pow = proofOfWork(powInfo);

    const request = makeMyNameRequest(pow, keys.signing.publicKey);
    const [response, responseErr] = await apiRequest(request);
    if (responseErr !== "") {
        tick("error", responseErr);
        return;
    }
    tick("myNewName", decodeInt(response));
}

async function cacheQuery(key) {
    const value = await localforage.getItem(key);
    tick("cacheResponse", { key: key, value: value });
}

function newChildren(key) {
    const parentEl = document.getElementById(key.parentId);
    while (parentEl.firstChild) {
        parentEl.removeChild(parentEl.lastChild);
    }
    for (const child of key.children) {
        parentEl.appendChild(child);
    }
}

function addOnclick(key) {
    const el = document.getElementById(key.id);
    el.onclick = key.onclick;
}

function cacheValue(toCache) {
    localforage.setItem(toCache.key, toCache.value);
}

function updateTextBox(toAdd, dontCare) {
    const box = document.getElementById(toAdd.id);
    box.value = toAdd.value;
}

async function getInboxSummary(inboxIds) {
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
    tick("inboxSummary", summaries);
}

async function getDraftsSummary(draftIds) {
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
    tick("draftsSummary", summaries);
}

async function getOutboxSummary(outboxIds) {
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
    tick("outboxSummary", summaries);
}

async function codeFilesUpload(files) {
    const file = files[0];
    const contents = await file.arrayBuffer();
    tick("uploadedCodeFile", {
        contents: contents,
        name: file.name,
        size: file.size,
        mime: file.type,
    });
}

function replaceDomWith(newDom) {
    const old = document.getElementById(newDom.id);
    old.replaceWith(newDom.newDom);
}

// For interacting with the WASM generated by Rust. It just wraps
// up a slightly tidied-up version of the code generated by
// wasm-pack.
class Wasm {
    async init(codeBytes) {
        const module_ = await WebAssembly.compile(this.codeBytes);
        this.wasm = await WebAssembly.instantiate(module_, {});
        this.WASM_VECTOR_LEN = 0;
        this.mem8 = new Uint8Array(this.wasm.exports.memory.buffer);
        this.mem32 = new Int32Array(this.wasm.exports.memory.buffer);
    }

    _getMem8() {
        if (this.mem8 !== this.wasm.exports.memory.buffer) {
            this.mem8 = new Uint8Array(this.wasm.exports.memory.buffer);
        }
        return this.mem8;
    }

    _passStringToWasm(arg, malloc, realloc) {
        const cachedTextEncoder = new TextEncoder();
        if (realloc === undefined) {
            const buf = cachedTextEncoder.encode(arg);
            const ptr = malloc(buf.length);
            this._getMem8()
                .subarray(ptr, ptr + buf.length)
                .set(buf);
            this.WASM_VECTOR_LEN = buf.length;
            return ptr;
        }

        let len = arg.length;
        let ptr = malloc(len);

        const mem = this._getMem8();

        let offset = 0;

        for (; offset < len; offset++) {
            const code = arg.charCodeAt(offset);
            if (code > 0x7f) break;
            mem[ptr + offset] = code;
        }

        if (offset !== len) {
            if (offset !== 0) {
                arg = arg.slice(offset);
            }
            ptr = realloc(ptr, len, (len = offset + arg.length * 3));

            const view = this._getMem8().subarray(ptr + offset, ptr + len);
            const ret = cachedTextEncoder.encodeInto(arg, view);
            offset += ret.written ? ret.written : 0;
        }

        this.WASM_VECTOR_LEN = offset;
        return ptr;
    }

    _getMem32() {
        if (this.mem32.buffer !== this.wasm.exports.memory.buffer) {
            this.mem32 = new Int32Array(this.wasm.exports.memory.buffer);
        }
        return this.mem32;
    }

    _getArrayU8FromWasm0(ptr, len) {
        return this._getMem8().subarray(ptr / 1, ptr / 1 + len);
    }

    _run(s) {
        const ptr0 = this._passStringToWasm(
            s,
            this.wasm.exports.__wbindgen_malloc,
            this.wasm.exports.__wbindgen_realloc
        );
        const len0 = this.WASM_VECTOR_LEN;
        this.wasm.exports.big_web_thing(8, ptr0, len0);
        const r0 = this._getMem32()[8 / 4 + 0];
        const r1 = this._getMem32()[8 / 4 + 1];
        const v1 = this._getArrayU8FromWasm0(r0, r1).slice();
        this.wasm.exports.__wbindgen_free(r0, r1 * 1);
        return v1;
    }

    bigWebThing(s) {
        try {
            return this._run(s);
        } catch (err) {
            tick("error", err);
        }
    }
}

async function lookupInboxMessage(id) {
    const message = localforage.getItem(id);
    const compiled = new Wasm();
    await compiled.init(message.code.contents);
    message.output = compiled.bigWebThing(message.userInput);
    tick("lookedUpInboxMessage", message);
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
    lookupInboxMessage: lookupInboxMessage,
};

let tick;
{
    let state = {};
    const outputs = [];

    tick = (inputKey, inputValue) => {
        for (const output of outputs) {
            io[output.key](output.value);
        }
        let newOutputs;
        [newOutputs, state] = update[inputKey](inputValue, state);
        outputs.concat(newOutputs);
    };

    tick("init", "");
}

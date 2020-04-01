(function () {
  "use strict";

  var app = Elm.Main.init({ node: document.getElementById("main") });

  const nullCache = "There is no cache!";

  async function getEditorInfo() {
    const rawEditorCache = await localforage.getItem("editorCache");
    let editorCache = nullCache;
    if (rawEditorCache !== null) {
      editorCache = base64js.fromByteArray(rawEditorCache);
    }

    const keys = await getCryptoKeys();

    const [myName, myNameErr] = await getMyName(keys);
    if (myNameErr !== "") {
      return [{}, myNameErr];
    }
    const signingKeys = await localforage.getItem("signingKeys");
    let myContacts = [];
    if (signingKeys !== null) {
      const myContactsStr = Object.keys(signingKeys);
      const lenContacts = myContactsStr.length
      for (let i = 0; i < lenContacts; i++) {
        myContacts.push(parseInt(myContactsStr[i]))
      }
    }
    const toSend = {
      myName: myName,
      editorCache: editorCache,
      myContacts: myContacts,
    };
    return [toSend, ""];
  }

  function sendEditorInfo() {
    getEditorInfo().then(function (infoOrErr) {
      const [info, err] = infoOrErr;
      if (err !== "") {
        console.log(err);
        return;
      }
      app.ports.retrievedEditorInfo.send(info);
    });
  }

  app.ports.getEditorInfo.subscribe(function () {
    sendEditorInfo();
  });

  app.ports.cacheEditorInfo.subscribe(function (editorCache) {
    localforage.setItem("editorCache", base64js.toByteArray(editorCache));
  });

  app.ports.cacheEditorInfoImporter.subscribe(function (editorCache) {
    localforage.setItem("editorCache", editorCache);
  });

  const nullReceipts = "There are no receipts!";

  const whitelistSomeone = 1;
  const sendThis = 2;

  function decodeHumanMsg(raw, pos) {
    const rawLength = raw.length;
    if (rawLength < 13) {
      return [{}, "human message is less than 13 bytes long"];
    }
    const from = decodeInt(raw.slice(pos, pos + 4));
    pos += 4;
    const to = decodeInt(raw.slice(pos, pos + 4));
    pos += 4;
    const documentLength = decodeInt(raw.slice(pos, pos + 4));
    pos += 4;
    if (rawLength < 12 + documentLength) {
      return [{}, "raw is shorter than given document length"];
    }
    const document = raw.slice(pos, pos + documentLength);
    pos += 12 + documentLength;
    return [
      { type: sendThis, msg: { to: to, from: from, document: document } },
      "",
    ];
  }

  function decodeInt(eightBytes) {
    let result = 0;
    for (let i = 0; i < 8; i++) {
      result += eightBytes[i] * Math.pow(256, i);
    }
    return result;
  }

  function decodeInt32(fourBytes) {
    let result = 0
    for (let i = 0; i < 4; i++) {
      result += fourBytes[i] * Math.pow(256, i);
    }
    return result
  }

  function parseMessage(raw) {
    let pos = 0;
    const messageType = raw[0];
    pos += 1;
    let message = {};

    switch (messageType) {
      case whitelistSomeone:
        const whitelistee = decodeInt32(raw.slice(pos, pos + 4));
        pos += 4;
        message = { type: whitelistSomeone, msg: whitelistee };
        return [message, ""];

      case sendThis:
        return decodeHumanMsg(raw, pos);
    }

    return [{}, "bad message type: " + messageType];
  }

  function decodeMessages(raw) {
    if (raw === null) {
      return [[], ""];
    }

    let messages = [];
    const rawLength = raw.length;
    for (let i = 0; i < rawLength; i++) {
      const rawMessage = raw[i]
      const [message, parseErr] = parseMessage(rawMessage);
      if (parseErr !== "") {
        return [[], parseErr];
      }
      messages.push(message);
    }
    return [messages, ""];
  }

  async function readMessagesFromCache() {
    const raw = await localforage.getItem("outbox");
    return decodeMessages(raw);
  }

  function createCryptoKeys() {
    const encryptionKeys = nacl.box.keyPair();
    const signingKeys = nacl.sign.keyPair();
    return {
      encryption: {
        secret: encryptionKeys.secretKey,
        public: encryptionKeys.publicKey,
      },
      signing: {
        secret: signingKeys.secretKey,
        public: signingKeys.publicKey,
      },
    };
  }

  async function getCryptoKeys() {
    const keysKey = "cryptokeys";
    let keys = await localforage.getItem(keysKey);
    if (keys === null) {
      keys = createCryptoKeys();
      localforage.setItem(keysKey, keys);
      return keys;
    }
    return keys;
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

  function isDifficult(hash, difficulty) {
    for (let i = 0; i < 32; i++) {
      if (hash[i] < difficulty) {
        return false;
      }
    }
    return true;
  }

  function proofOfWork(powInfo) {
    const unique = powInfo.unique;
    let buffer = new ArrayBuffer(8);
    let bufferView = new Uint8Array(buffer);
    let counter = new Int32Array(buffer);
    while (true) {
      const combined = combine(unique, bufferView);
      const hash = nacl.hash(combined).slice(0, 32);
      if (isDifficult(hash, powInfo.difficulty)) {
        return combined;
      }
      counter[0] = counter[0] + 1;
    }
  }

  // Pow (proof of work) is 16 bytes long.
  function makeMakeMyNameRequest(pow, publicSigningKey) {
    let buffer = new ArrayBuffer(49);
    let bufferView = new Uint8Array(buffer);
    bufferView[0] = 1;
    for (let i = 0; i < 16; i++) {
      bufferView[i + 1] = pow[i];
    }
    for (let i = 0; i < 32; i++) {
      bufferView[i + 17] = publicSigningKey[i];
    }
    return bufferView;
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
      return [
        [],
        "bad response: " +
          response.status +
          ": " +
          String.fromCharCode.apply(null, bodyArray),
      ];
    }

    return [bodyArray, ""];
  }

  async function getPowInfo() {
    const [response, err] = await apiRequest(oneByte(3));
    if (err !== "") {
      return [{}, err];
    }

    if (response.length !== 9) {
      return [
        {},
        "powInfo response is not 9 bytes long, it is " + response.length,
      ];
    }

    const powInfo = {
      difficulty: response[0],
      unique: response.slice(1),
    };
    return [powInfo, ""];
  }

  async function sendMakeMyName(keys) {
    let [powInfo, err] = await getPowInfo();
    if (err !== "") {
      return [{}, err];
    }
    const pow = proofOfWork(powInfo);
    let request = makeMakeMyNameRequest(pow, keys.signing.public);
    let response;
    [response, err] = await apiRequest(request);
    if (err !== "") {
      return [{}, err];
    }
    if (response.length !== 8) {
      return [{}, "response is not 8 bytes long"];
    }
    return [decodeInt(response), ""];
  }

  // Incoming message types:
  const msgInMyName = 0;
  const msgInDontCare = 1;

  async function sendMessage(message, keys, myName) {
    switch (message.type) {
      case whitelistSomeone:
        const err = await sendWhitelistRequest(message.msg, keys, myName);
        return [null, err];
      case sendThis:
        return await sendClientToClient(message.msg, keys, myName);
    }
    return [{}, "unknown message type: " + message.type];
  }

  function constructCtoCMessage(chunk, recipient, keys, authCode, myName) {
    const idToken = makeIdToken(
      8,
      combine(recipient, chunk),
      authCode,
      myName,
      keys.signing.secret
    );
    const request = combine(
      oneByte(8),
      combine(idToken, combine(recipient, chunk))
    );
    return request;
  }

  // A message chunk is like this:
  // + 1 byte: 0x01 for a normal message, 0x00 for a receipt
  //
  // If it is a receipt, then the rest of it is:
  // + 4 bytes: the recipient ID
  // + 96 bytes: signature of message hash (sha512[:32])
  //
  // And if it is a normal message chunk:
  // + 4 bytes: 32-bit int counter, starting at 0
  // + 4 bytes: total number of chunks in message
  // + 32 bytes: hash of complete message
  // + <= 15kB: chunk
  function chopMessageIntoChunks(message) {
    const chunkLength = 15000;
    const hash = nacl.hash(message);
    const numChunks = Math.floor(message.length / chunkLength) + 1;
    const numChunksBytes = encodeInt(numChunks);
    let chunks = [];
    for (let i = 0; i < numChunks; i++) {
      const chunkNum = encodeInt(i);
      const chunkStart = i * chunkLength;
      const chunkEnd = (i + 1) * chunkLength;
      const chunkBase = message.slice(chunkStart, chunkEnd);
      const combined = combine(
        oneByte(1),
        combine(chunkNum, combine(numChunksBytes, combine(hash, chunkBase)))
      );
      chunks.push(combined);
    }
    return chunks;
  }

  async function makeNonce() {
    const counter = await localforage.getItem("noncecounter");
    let buffer = new ArrayBuffer(24);
    let bufferView = new Int32Array(buffer);
    bufferView[0] = counter;
    await localforage.setItem("noncecounter", counter + 1);
    return bufferView;
  }

  async function getEncryptionKey(username, signingKey) {
    const request = combine(oneByte(13), encodeInt(username));
    const [response, responseErr] = apiRequest(request);
    if (responseErr !== "") {
      return [{}, err];
    }

    if (response.length !== 96) {
      return [{}, "signed encryption key is not 96 bytes long"];
    }
    const encryptionKey = nacl.sign.open(response, signingKey);
    if (encryptionKey === null) {
      return [{}, "could not verify encryption key signature"];
    }

    return [encryptionKey, ""];
  }

  async function sendClientToClient(message, keys, myName) {
    const chunks = chopMessageIntoChunks(message.document);
    const chunksLength = chunks.length;
    const signingKey = await getRecipientSigningKey(message.to);
    const encryptionKey = await getEncryptionKey(message.to, signingKey);

    for (let i = 0; i < chunksLength; i++) {
      const [authCode, authErr] = await getAuthCode();
      if (authErr !== "") {
        return [{}, authErr];
      }

      const chunk = chunks[i];
      const nonce = makeNonce();
      const encryptedChunk = nacl.box(
        chunk,
        nonce,
        encryptionKey,
        keys.encryption.secret
      );

      const withNonce = combine(nonce, encryptedChunk);

      const subMsg = constructCtoCMessage(
        withNonce,
        message.to,
        keys,
        authCode,
        myName
      );

      const [response, err] = await apiRequest(subMsg);
      if (err !== "") {
        return [{}, err];
      }
    }
    return [{ type: msgInDontCare }, ""];
  }

  async function getAuthCode() {
    let buffer = new ArrayBuffer(1);
    let request = new Uint8Array(buffer);
    request[0] = 7;
    const [authCode, err] = await apiRequest(request);
    return [authCode, err];
  }

  function oneByte(route) {
    let buffer = new ArrayBuffer(1);
    let view = new Uint8Array(buffer);
    view[0] = route;
    return view;
  }

  function makeIdToken(route, message, authCode, secretSign, myName) {
    const toSign = combine(oneByte(route), combine(message, authCode));
    const hash = nacl.hash(toSign).slice(0, 32);
    const signature = nacl.sign(hash, secretSign);
    return combine(encodeInt(myName), combine(authCode, signature));
  }

  async function updateContacts(whitelistee) {
    const [response, err] = await apiRequest(combine(oneByte(2), encodeInt(whitelistee)));
    if (err !== "") {
      return err;
    }
    let signingKeys = await localforage.getItem("signingKeys");
    if (signingKeys === null) {
        signingKeys = {}
    }
    signingKeys[whitelistee] = response;
    await localforage.setItem("signingKeys", signingKeys);
    sendEditorInfo()
    return "";
  }

  async function sendWhitelistRequest(whitelistee, keys, myName) {
    let [powInfo, err] = await getPowInfo();
    if (err !== "") {
      return err;
    }
    const pow = proofOfWork(powInfo);
    let authCode;
    [authCode, err] = await getAuthCode();
    const idToken = makeIdToken(
      10,
      combine(pow, encodeInt(whitelistee)),
      authCode,
      keys.signing.secret,
      myName
    );
    const request = combine(
      oneByte(10),
      combine(idToken, combine(pow, encodeInt(whitelistee)))
    );
    let response;
    [response, err] = await apiRequest(request);
    if (err !== "") {
      return err;
    }
    const updateContactsErr = await updateContacts(whitelistee);
    return updateContactsErr;
  }

  async function sendMessages(messages, keys, myName) {
    let responses = [];
    const messagesLength = messages.length;
    for (let i = 0; i < messagesLength; i++) {
      const [response, err] = await sendMessage(messages[i], keys, myName);
      if (err !== "") {
        return [[], err];
      }
      if (response !== null) {
        responses.push(response);
      }
    }
    return [responses, ""];
  }

  function getLeftOvers(used, rawDownloads) {
    const numDownloads = rawDownloads.length;
    let leftOvers = [];
    for (let i = 0; i < numDownloads; i++) {
      if (used.has(i)) {
        continue;
      }
      leftOvers.push(rawDownloads[i]);
    }
    return leftOvers;
  }

  function equalBytes(a, b) {
    const alength = a.length;
    const blength = b.length;
    if (alength !== blength) {
      return false;
    }

    for (let i = 0; i < alength; i++) {
      if (a[i] !== b[i]) {
        return false;
      }
    }
    return true;
  }

  async function cacheReceipt(validReceipt) {
    let receipts = await localforage.getItem("receipts");
    if (receipts === null) {
      receipts = [];
    }
    receipts.push(validReceipt);
    await localforage.setItem("receipts", receipts);
  }

  async function lookupSigningKey(author) {
    const keys = await localforage.getItem("signingKeys");
    return keys[author];
  }

  async function validateReceipt(rawChunk) {
    const author = decodeInt(rawChunk.slice(1, 5));
    const key = lookupSigningKey(author);
    if (key === null) {
      return [null, "no key for author"];
    }
    const signed = nacl.sign.open(rawChunk(5), key);
    if (signed === null) {
      return [null, "bad signature"];
    }
    const combined = combine(rawChunk.slice(1, 5), rawChunk(5), signed);
    return [combined, ""];
  }

  async function decodeChunk(rawChunk) {
    const chunkLength = rawChunk.chunk.length;
    if (chunkLength < 41) {
      return [
        {},
        "chunk is only " +
          chunkLength +
          " bytes long, but should be at least 41",
        null,
      ];
    }

    if (rawChunk[0] === 0) {
      const [validReceipt, receiptErr] = validateReceipt(rawChunk);
      if (receiptErr !== "") {
        return [null, null, receiptErr];
      }
      await cacheReceipt(validReceipt);
      return [null, true, ""];
    }

    rawChunk = rawChunk.slice(1);

    return [
      {
        counter: decodeInt(rawChunk.chunk.slice(0, 4)),
        totalChunks: decodeInt(rawChunk.chunk.slice(4, 8)),
        totalHash: rawChunk.chunk.slice(8, 40),
        chunk: rawChunk.chunk.slice(40),
        author: rawChunk.author,
      },
      "",
    ];
  }

  function decodeChunks(rawChunks) {
    let decoded = [];
    const chunksLength = rawChunks.length;
    for (let i = 0; i < chunksLength; i++) {
      const [decoded, isReceipt, err] = decodeChunk(rawChunks[i]);
      if (err !== "") {
        return [[], err];
      }
      if (isReceipt) {
        continue;
      }
      decoded.push(decoded);
    }
    return [decoded, ""];
  }

  function getRelevantChunks(decodedDownloads, totalHash) {
    const decodedLength = decodedDownloads.length;
    let relevant = [];
    let newUsed = Set();
    for (let i = 0; i < decodedLength; i++) {
      const decoded = decodedDownloads[i];
      if (equalBytes(decoded, totalHash)) {
        relevant.push(decoded);
        newUsed.add(i);
      }
    }
    return [relevant, newUsed];
  }

  function sortChunks(chunks) {
    chunks.sort(function (c1, c2) {
      return c1.counter - c2.counter;
    });
    return chunks;
  }

  function encodeInt(theInt) {
    let buffer = new ArrayBuffer(8);
    let result = new Uint8Array(buffer);

    // The reason for only going up to 4, and not to 8 is that apparently when
    // you do bit shifts on numbers in JS, it uses 32-bit ints.  This'll do me
    // till I have 10^9 users!
    for (let i = 0; i < 4; i++) {
      const r = (theInt >> (i * 8)) & 0xff;
      result[i] = r
    }
    return result;
  }

  async function sendReceipt(hash, author, keys, myName) {
    const signature = nacl.sign(hash, keys.signing.secret);
    const combined = combine(oneByte(0), combine(encodeInt(author), signature));
    const theirEncryptionKey = await getEncryptionKey(
      author,
      keys.signing.secret
    );
    const nonce = makeNonce();
    const encryptedChunk = nacl.box(
      combined,
      nonce,
      theirEncryptionKey,
      keys.encryption.secret
    );
    const withNonce = combine(nonce, encryptedChunk);
    const [authCode, authErr] = await getAuthCode();
    if (authErr !== "") {
      return authErr;
    }
    const msg = constructCToCMessage(withNonce, author, keys, authCode, myName);
    const [response, sendErr] = await apiRequest(msg);
    return sendErr;
  }

  async function joinChunks(chunks, used) {
    const chunksLength = chunks.length;
    let assembled = chunks[0].chunk;
    for (let i = 1; i < chunksLength; i++) {
      assembled = combine(assembled, chunks[i].chunk);
    }
    const hash = nacl.hash(assembled).slice(0, 32);
    if (!equalBytes(hash, chunks[0].totalHash)) {
      return [[], "assembled chunk did not match expected hash"];
    }

    const author = chunks[0].author;
    const receiptErr = await sendReceipt(hash, author);
    if (receiptErr !== "") {
      return [null, receiptErr];
    }

    const encodedAuthor = encodeInt(author);
    return [combine(encodedAuthor, assembled), ""];
  }

  function chunksAllThere(sortedChunks) {
    const chunksLength = sortedChunks.length;
    if (chunksLength === 0) {
      return false;
    }
    if (chunksLength !== sortedChunks[0].totalChunks) {
      return false;
    }
    for (let i = 0; i < chunksLength; i++) {
      if (sortedChunks[i].counter !== i) {
        return false;
      }
    }
    return true;
  }

  async function unpackOneDownload(i, decodedDownloads, keys) {
    const start = decodedDownloads(i);
    if (start.counter !== 0) {
      return [[], used, ""];
    }

    if (start.totalChunks === 1) {
      chunkHash = nacl.hash(start.chunk);
      if (!equalBytes(chunkHash, start.totalHash)) {
        return [[], Set(), "single-chunk message with bad hash"];
      }
    }

    const [relevantChunks, used] = getRelevantChunks(
      decodedDownloads,
      start.totalHash
    );

    const relevantChunksLength = relevantChunks.length;
    if (relevantChunksLength === 0) {
      return [[], Set(), "why are there no relevant chunks?"];
    }

    const sortedChunks = sortChunks(relevantChunks);
    if (!chunksAllThere(sortedChunks)) {
      return [[], Set(), ""];
    }
    const [completeMessage, joinErr] = joinChunks(sortedChunks);
    return [completeMessage, used, joinErr];
  }

  async function unpackDownloads(rawDownloads, keys) {
    const numDownloads = rawDownloads.length;
    const [decodedDownloads, decodeErr] = decodeChunks(rawDownloads);
    if (decodeErr !== "") {
      return [null, null, decodeErr]
    }
    let allUnpacked = [];
    let used = new Set();
    for (let i = 0; i < numDownloads; i++) {
      let unpacked, err;
      [unpacked, used, unpackErr] = await unpackOneDownload(
        i,
        decodedDownloads,
        keys,
        used
      );
      if (unpackErr !== "") {
        return [[], [], unpackErr];
      }
      allUnpacked.push(unpacked);
    }
    return [allUnpacked, getLeftOvers(used, rawDownloads), ""];
  }

  async function downloadNewMessages(keys, myName) {
    const messages = [];
    while (true) {
      const [authCode, authErr] = await getAuthCode();
      if (authErr !== "") {
        return [[], authErr];
      }

      const idToken = makeIdToken(9, [], authCode, keys.signing.secret, myName);
      const request = combine(oneByte(9), idToken);
      const [response, apiErr] = await apiRequest(request);
      if (apiErr !== "") {
        return [[], apiErr];
      }

      if (response[0] === 0) {
        return [messages, ""];
      } else {
        messages.push(response.slice(1));
      }
    }
    return messages;
  }

  async function uploadEncryptionKey(keys, myName) {
    const signedKey = nacl.sign(keys.encryption.public, keys.signing.secret);
    const request = combine(oneByte(12), combine(encodeInt(myName), signedKey));
    const [response, err] = await apiRequest(request);
    return err;
  }

  async function getMyName(keys) {
    let myName = await localforage.getItem("myName");
    if (myName === null) {
      let myNameErr;
      [myName, myNameErr] = await sendMakeMyName(keys);
      if (myNameErr !== "") {
        return [null, myNameErr];
      }
      await localforage.setItem("myName", myName);
    }
    return [myName, ""];
  }

  // This function:
  // 1. reads all the messages that were generated by the Generator
  //    and dumped in the cache
  //
  // 2. sends them all and keeps the necessary responses
  //
  // 3. downloads all the new messages on the server
  //
  // 4. combines (2) and (3), serializes them, and dumps them in the
  //    cache
  //
  async function communicateMain() {
    const [messages, readMsgErr] = await readMessagesFromCache();
    if (readMsgErr !== "") {
      return readMsgErr;
    }

    const keys = await getCryptoKeys();

    const [myName, myNameErr] = await getMyName(keys);
    if (myNameErr !== "") {
      return myNameErr;
    }

    const errUpCrypt = await uploadEncryptionKey(keys, myName);
    if (errUpCrypt !== "") {
      return errUpCrypt;
    }

    const [responses, responseErr] = await sendMessages(messages, keys, myName);
    if (responseErr !== "") {
      return responseErr;
    }

    await localforage.removeItem('outbox')

    const [rawDownloads, downloadErr] = await downloadNewMessages(keys, myName);
    if (downloadErr !== "") {
      return downloadErr;
    }

    const [decryptedDownloads, decryptErr] = decryptDownloads(rawDownloads, keys);
    if (decryptErr !== "") {
      return decryptErr;
    }

    const oldLeftovers = await readOldLeftovers();

    const [unpacked, leftOvers, unpackErr] = await unpackDownloads(
      combine(decryptedDownloads, oldLeftovers),
      keys
    );
    if (unpackErr !== "") {
      return unpackErr;
    }

    cacheLeftovers(leftOvers);

    const cacheErr = await cacheMessages(unpacked);
    if (cacheErr !== "") {
      return cacheErr;
    }
    return ""
  }

  async function cacheMessages(newMessages) {
    let rawOldMessages = await localforage.getItem("inbox");
    if (rawOldMessages === null) {
      rawOldMessages = []
    }
    const [decodedMessages, err1] = decodeInbox(rawOldMessages);
    if (err1 !== "") {
      return err1;
    }
    const combined = decodedMessages.concat(newMessages);
    if (newMessages.length === 0) {
      return "";
    }
    const encoded = encodeMessages(combined);
    await localforage.setItem("inbox", encoded);
    return "";
  }

  function decodeInbox(rawMessages) {
    let i = 0;
    let messages = [];
    const rawMessagesLength = rawMessages.length;
    while (i < rawMessagesLength) {
      if (i + 4 >= rawMessagesLength) {
        return [[], "not enough bytes for message length"];
      }

      const messageLength = decodeInt(rawMessages.slice(i, i + 4));
      i += 4;

      if (i + messageLength >= rawMessagesLength) {
        return [[], "not enough bytes for message"];
      }
      const message = rawMessages.slice(i, i + messageLength);
      i += messageLength;
      messages.push(message);
    }
    return [messages, ""];
  }

  function encodeMessage(message) {
    const length = encodeInt(message.length);
    return combine(length, message);
  }

  function encodeMessages(messages) {
    const messagesLength = messages.length;
    let encoded = encodeMessage(messages[0]);
    for (let i = 1; i < messagesLength; i++) {
      encoded.push(encodeMessage(messages[1]));
    }
    return encoded;
  }

  async function cacheLeftovers(leftOvers) {
    await localforage.setItem("leftovers", leftOvers);
  }

  async function readOldLeftovers() {
    let leftovers = await localforage.getItem("leftovers");
    if (leftovers === null) {
      return [];
    }
    return leftovers;
  }

  function decryptDownloads(rawDownloads, keys) {
    const lengthDownloads = rawDownloads.length;
    const messages = [];
    for (let i = 0; i < lengthDownloads; i++) {
      const download = rawDownloads[i];
      if (download.length < 122) {
        return [[], "raw message less than 122 bytes long"];
      }
      const nonce = download.slice(121, 145);
      const encryptedblob = download.slice(145);
      const author = decodeInt(download.slice(1, 9));
      const [theirEncryptionKey, encKeyErr] = getEncryptionKey(
        author,
        keys.signing.secret
      );
      if (encKeyErr !== "") {
        return [[], encKeyErr];
      }
      const chunk = nacl.box.open(
        encryptedBlob,
        nonce,
        theirEncryptionKey,
        keys.encryption.secret
      );
      if (chunk === null) {
        return [[], "could not authenticate chunk"];
      }
      decrypted = {
        author: author,
        chunk: chunk,
      };
      messages.push(decrypted);
    }
    return [messages, ""];
  }

  app.ports.communicate.subscribe(function () {
    // When this has finished, all the incoming messages are saved
    // under 'inbox' in the cache, like this:
    //   + 4 bytes: length of message
    //   + 4 bytes: sender
    //   + message
    communicateMain().then(function (err) {
      if (err !== "") {
        app.ports.communicationError.send(err);
      }
    });
  });

  const nullInbox = "There is no inbox!";

  app.ports.getImporterInfo.subscribe(function () {
    localforage.getItem("editorCache").then(function (rawEditorCache) {
      let editorCache = nullCache;
      if (rawEditorCache !== null) {
        editorCache = base64js.fromByteArray(rawEditorCache);
      }
      localforage.getItem("inbox").then(function (rawInbox) {
        let inbox = nullInbox;
        if (rawInbox !== null) {
          inbox = base64js.fromByteArray(rawInbox);
        }
        app.ports.gotImporterInfo.send({
          editorCache: editorCache,
          inbox: inbox,
        });
      });
    });
  });

  app.ports.sendMessagePort.subscribe(function (rawB64) {
    localforage.getItem("outbox").then(function (outbox) {
      if (outbox === null) {
        outbox = [];
      }
      outbox.push(base64js.toByteArray(rawB64));
      localforage.setItem("outbox", outbox);
    });
  });
})();

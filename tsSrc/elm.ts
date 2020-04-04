(function () {
  "use strict";

  async function localGet(key: string): Promise<any> {
    // @ts-ignore
    const value = await localforage.getItem(key);
    return value;
  }

  async function localSet(key: string, value: any): Promise<void> {
    // @ts-ignore
    await localforage.setItem(key, value);
  }

  async function localRemove(key: string): Promise<void> {
    // @ts-ignore
    await localforage.removeItem(key);
  }

  function fromBytes(bytes: Uint8Array): string {
    // @ts-ignore
    return base64js.fromByteArray(bytes);
  }

  function toBytes(b64: string): Uint8Array {
    // @ts-ignore
    return base64js.toByteArray(b64);
  }

  function signOpen(
    signedMessage: Uint8Array,
    publicKey: Uint8Array
  ): Uint8Array | null {
    // @ts-ignore
    return nacl.sign.open(signedMessage, publicKey);
  }

  function sign(message: Uint8Array, secretKey: Uint8Array): Uint8Array {
    // @ts-ignore
    return nacl.sign(message, secretKey);
  }

  interface SignKeyPair {
    readonly publicKey: Uint8Array;
    readonly secretKey: Uint8Array;
  }

  function signKeyPair(): SignKeyPair {
    // @ts-ignore
    return nacl.sign.keyPair();
  }

  interface Keys {
    readonly signing: SignKeyPair;
    readonly encryption: BoxKeyPair;
  }

  interface BoxKeyPair {
    readonly publicKey: Uint8Array;
    readonly secretKey: Uint8Array;
  }

  function boxKeyPair(): BoxKeyPair {
    // @ts-ignore
    return nacl.box.keyPair();
  }

  function box(
    message: Uint8Array,
    nonce: Uint8Array,
    theirPublicKey: Uint8Array,
    mySecretKey: Uint8Array
  ): Uint8Array {
    // @ts-ignore
    return nacl.box(message, nonce, theirPublicKey, mySecretKey);
  }

  function sha512(message: Uint8Array): Uint8Array {
    // @ts-ignore
    return nacl.hash(message);
  }

  function boxOpen(
    box: Uint8Array,
    nonce: Uint8Array,
    theirPublicKey: Uint8Array,
    mySecretKey: Uint8Array
  ): Uint8Array | null {
    // @ts-ignore
    return nacl.box.open(box, nonce, theirPublicKey, mySecretKey);
  }

  // @ts-ignore
  const app = Elm.Main.init({ node: document.getElementById("main") });

  const nullCache: string = "There is no cache!";

  interface EditorInfo {
    myName: number;
    editorCache: string;
    myContacts: number[];
  }

  const nullEditorInfo: EditorInfo = {
    myName: 0,
    editorCache: "",
    myContacts: [],
  };

  async function getEditorInfo(): Promise<[EditorInfo, string]> {
    const rawEditorCache: Uint8Array | null = await localGet("editorCache");
    let editorCache: string = nullCache;
    if (rawEditorCache !== null) {
      editorCache = fromBytes(rawEditorCache);
    }

    const keys: Keys = await getCryptoKeys();

    const [myName, myNameErr]: [number, string] = await getMyName(keys);
    if (myNameErr !== null) {
      return [nullEditorInfo, myNameErr];
    }
    const signingKeys: SignKeyPair = await localGet("signingKeys");
    let myContacts: number[] = [];
    if (signingKeys !== null) {
      const myContactsStr: string[] = Object.keys(signingKeys);
      const lenContacts: number = myContactsStr.length;
      for (let i = 0; i < lenContacts; i++) {
        myContacts.push(parseInt(myContactsStr[i]));
      }
    }
    const toSend: EditorInfo = {
      myName: myName,
      editorCache: editorCache,
      myContacts: myContacts,
    };
    return [toSend, ""];
  }

  function sendEditorInfo(): void {
    getEditorInfo().then(function (infoOrErr: [EditorInfo, string]) {
      const [info, err]: [EditorInfo, string] = infoOrErr;
      if (err !== null) {
        console.log(err);
        return;
      }
      app.ports.retrievedEditorInfo.send(info);
    });
  }

  app.ports.getEditorInfo.subscribe(function () {
    sendEditorInfo();
  });

  app.ports.cacheEditorInfo.subscribe(function (editorCache: string) {
    localSet("editorCache", toBytes(editorCache));
  });

  app.ports.cacheEditorInfoImporter.subscribe(function (
    editorCache: Uint8Array
  ) {
    localSet("editorCache", editorCache);
  });

  const whitelistSomeone: number = 1;
  const sendThis: number = 2;

  interface MsgOut {
    msg: SendThis | WhitelistSomeone;
    type: "whitelistSomeone" | "sendThis";
  }

  interface WhitelistSomeone {
    id: number;
  }

  interface SendThis {
    to: number;
    from: number;
    document: Uint8Array;
  }

  function decodeHumanMsg(raw: Uint8Array, pos: number): [MsgOut, string] {
    const rawLength: number = raw.length;
    if (rawLength < 13) {
      return [nullMsgOut, "human message is less than 13 bytes long"];
    }
    const from: number = decodeInt(raw.slice(pos, pos + 4));
    pos += 4;
    const to: number = decodeInt(raw.slice(pos, pos + 4));
    pos += 4;
    const documentLength: number = decodeInt(raw.slice(pos, pos + 4));
    pos += 4;
    if (rawLength < 12 + documentLength) {
      return [nullMsgOut, "raw is shorter than given document length"];
    }
    const document: Uint8Array = raw.slice(pos, pos + documentLength);
    pos += 12 + documentLength;
    return [
      { type: "sendThis", msg: { to: to, from: from, document: document } },
      "",
    ];
  }

  function decodeInt(eightBytes: Uint8Array): number {
    let result: number = 0;
    for (let i = 0; i < 8; i++) {
      result += eightBytes[i] * Math.pow(256, i);
    }
    return result;
  }

  function decodeInt32(fourBytes: Uint8Array): number {
    let result: number = 0;
    for (let i = 0; i < 4; i++) {
      result += fourBytes[i] * Math.pow(256, i);
    }
    return result;
  }

  function parseMessage(raw: Uint8Array): [MsgOut, string] {
    let pos: number = 0;
    const messageType: number = raw[0];
    pos += 1;

    switch (messageType) {
      case whitelistSomeone: {
        const whitelistee: number = decodeInt32(raw.slice(pos, pos + 4));
        pos += 4;
        const message: MsgOut = {
          type: "whitelistSomeone",
          msg: { id: whitelistee },
        };
        return [message, ""];
      }

      case sendThis:
        return decodeHumanMsg(raw, pos);
    }

    return [nullMsgOut, "bad message type: " + messageType];
  }

  const nullMsgOut: MsgOut = { msg: { id: 0 }, type: "whitelistSomeone" };

  function decodeMessages(raw: Uint8Array[]): [MsgOut[], string] {
    if (raw === null) {
      return [[], ""];
    }

    let messages = [];
    const rawLength = raw.length;
    for (let i = 0; i < rawLength; i++) {
      const rawMessage = raw[i];
      const [message, parseErr] = parseMessage(rawMessage);
      if (parseErr !== "") {
        return [[], parseErr];
      }
      messages.push(message);
    }
    return [messages, ""];
  }

  async function readMessagesFromCache(): Promise<[MsgOut[], string]> {
    const raw = await localGet("outbox");
    return decodeMessages(raw);
  }

  function createCryptoKeys(): Keys {
    return {
      encryption: boxKeyPair(),
      signing: signKeyPair(),
    };
  }

  async function getCryptoKeys(): Promise<Keys> {
    const keysKey = "cryptokeys";
    let keys = await localGet(keysKey);
    if (keys === null) {
      keys = createCryptoKeys();
      localSet(keysKey, keys);
      return keys;
    }
    return keys;
  }

  function combine(a: Uint8Array, b: Uint8Array): Uint8Array {
    const lena: number = a.length;
    const lenb: number = b.length;
    let buf = new ArrayBuffer(lena + lenb);
    let combined: Uint8Array = new Uint8Array(buf);
    for (let i = 0; i < lena; i++) {
      combined[i] = a[i];
    }
    for (let i = lena; i < lena + lenb; i++) {
      const bval = b[i - lena];
      combined[i] = bval;
    }
    return combined;
  }

  function isDifficult(hash: Uint8Array, difficulty: number): boolean {
    for (let i = 0; i < 32; i++) {
      if (hash[i] < difficulty) {
        return false;
      }
    }
    return true;
  }

  interface PowInfo {
    difficulty: number;
    unique: Uint8Array;
  }

  function proofOfWork(powInfo: PowInfo): Uint8Array {
    const unique = powInfo.unique;
    let buffer = new ArrayBuffer(8);
    let bufferView = new Uint8Array(buffer);
    let counter = new Int32Array(buffer);
    while (true) {
      const combined = combine(unique, bufferView);
      const hash = sha512(combined).slice(0, 32);
      if (isDifficult(hash, powInfo.difficulty)) {
        return combined;
      }
      counter[0] = counter[0] + 1;
    }
  }

  // Pow (proof of work) is 16 bytes long.
  function makeMakeMyNameRequest(
    pow: Uint8Array,
    publicSigningKey: Uint8Array
  ): Uint8Array {
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

  async function apiRequest(
    requestBody: Uint8Array
  ): Promise<[Uint8Array, string]> {
    const response = await fetch("/api", {
      method: "POST",
      headers: { "Content-Type": "application/octet-stream" },
      body: requestBody,
    });

    const body = await response.arrayBuffer();
    const bodyArray = new Uint8Array(body);

    if (!response.ok) {
      return [
        nullUint8Array(),
        "bad response: " +
          response.status +
          ": " +
          String.fromCharCode.apply(null, arrToNums(bodyArray)),
      ];
    }

    return [bodyArray, ""];
  }

  function arrToNums(arr: Uint8Array): number[] {
    let numbers: number[] = [];
    const lenArr = arr.length;
    for (let i = 0; i < lenArr; i++) {
      numbers.push(arr[i]);
    }
    return numbers;
  }

  const nullPowInfo: PowInfo = { difficulty: 0, unique: nullUint8Array() };

  async function getPowInfo(): Promise<[PowInfo, string]> {
    const [response, err] = await apiRequest(oneByte(3));
    if (err !== "") {
      return [nullPowInfo, err];
    }

    if (response.length !== 9) {
      return [
        nullPowInfo,
        "powInfo response is not 9 bytes long, it is " + response.length,
      ];
    }

    const powInfo = {
      difficulty: response[0],
      unique: response.slice(1),
    };
    return [powInfo, ""];
  }

  async function sendMakeMyName(keys: Keys): Promise<[number, string]> {
    let [powInfo, err]: [PowInfo, string] = await getPowInfo();
    if (err !== "") {
      return [0, err];
    }
    const pow: Uint8Array = proofOfWork(powInfo);
    let request: Uint8Array = makeMakeMyNameRequest(
      pow,
      keys.signing.publicKey
    );
    let response: Uint8Array;
    [response, err] = await apiRequest(request);
    if (err !== "") {
      return [0, err];
    }
    if (response.length !== 8) {
      return [0, "response is not 8 bytes long"];
    }
    return [decodeInt(response), ""];
  }

  async function sendMessage(
    message: MsgOut,
    keys: Keys,
    myName: number
  ): Promise<string> {
    switch (message.type) {
      case "whitelistSomeone": {
        const err = await sendWhitelistRequest(
          (message.msg as WhitelistSomeone).id,
          keys,
          myName
        );
        return err;
      }
      case "sendThis":
        return await sendClientToClient(message.msg as SendThis, keys, myName);
    }
  }

  function constructCtoCMessage(
    chunk: Uint8Array,
    recipient: Uint8Array,
    keys: Keys,
    authCode: Uint8Array,
    myName: number
  ): Uint8Array {
    const idToken = makeIdToken(
      8,
      combine(recipient, chunk),
      authCode,
      keys.signing.secretKey,
      myName
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
  function chopMessageIntoChunks(message: Uint8Array): Uint8Array[] {
    const chunkLength = 15000;
    const hash = sha512(message);
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

  async function makeNonce(): Promise<Uint8Array> {
    const counter = await localGet("noncecounter");
    let buffer = new ArrayBuffer(24);
    let uintArr = new Uint8Array(buffer);
    let bufferView = new Int32Array(buffer);
    bufferView[0] = counter;
    await localSet("noncecounter", counter + 1);
    return uintArr;
  }

  async function getEncryptionKey(
    username: number,
    signingKey: Uint8Array
  ): Promise<[Uint8Array, string]> {
    const request = combine(oneByte(13), encodeInt(username));
    const [response, responseErr] = await apiRequest(request);
    if (responseErr !== "") {
      return [nullUint8Array(), responseErr];
    }

    if (response.length !== 96) {
      return [nullUint8Array(), "signed encryption key is not 96 bytes long"];
    }
    const encryptionKey = signOpen(response, signingKey);
    if (encryptionKey === null) {
      return [nullUint8Array(), "could not verify encryption key signature"];
    }

    return [encryptionKey, ""];
  }

  async function getRecipientSigningKey(
    recipient: number
  ): Promise<[Uint8Array, string]> {
    const signingKeys = await localGet("signingKeys");
    if (signingKeys === null) {
      return [nullUint8Array(), "there are no signing keys"];
    }
    const key = signingKeys[recipient];
    if (key === null) {
      return [nullUint8Array(), "no signing key for recipient " + recipient];
    }
    return [key, ""];
  }

  async function sendClientToClient(
    message: SendThis,
    keys: Keys,
    myName: number
  ): Promise<string> {
    const chunks = chopMessageIntoChunks(message.document);
    const chunksLength = chunks.length;
    const [signingKey, signKeyErr] = await getRecipientSigningKey(message.to);
    if (signKeyErr !== "") {
      return signKeyErr;
    }
    const [encryptionKey, encKeyErr] = await getEncryptionKey(
      message.to,
      signingKey
    );

    if (encKeyErr !== "") {
      return encKeyErr;
    }
    for (let i = 0; i < chunksLength; i++) {
      const [authCode, authErr] = await getAuthCode();
      if (authErr !== "") {
        return authErr;
      }

      const chunk = chunks[i];
      const nonce = await makeNonce();
      const encryptedChunk = box(
        chunk,
        nonce,
        encryptionKey,
        keys.encryption.secretKey
      );

      const withNonce = combine(nonce, encryptedChunk);

      const subMsg = constructCtoCMessage(
        withNonce,
        signingKey,
        keys,
        authCode,
        myName
      );

      const response = await apiRequest(subMsg);
      const err = response[1];
      if (err !== "") {
        return err;
      }
    }
    return "";
  }

  async function getAuthCode(): Promise<[Uint8Array, string]> {
    let buffer = new ArrayBuffer(1);
    let request = new Uint8Array(buffer);
    request[0] = 7;
    const [authCode, err] = await apiRequest(request);
    return [authCode, err];
  }

  function oneByte(route: number): Uint8Array {
    let buffer = new ArrayBuffer(1);
    let view = new Uint8Array(buffer);
    view[0] = route;
    return view;
  }

  function makeIdToken(
    route: number,
    message: Uint8Array,
    authCode: Uint8Array,
    secretSign: Uint8Array,
    myName: number
  ): Uint8Array {
    const toSign = combine(oneByte(route), combine(message, authCode));
    const hash = sha512(toSign).slice(0, 32);
    const signature = sign(hash, secretSign);
    return combine(encodeInt(myName), combine(authCode, signature));
  }

  async function updateContacts(whitelistee: number): Promise<string> {
    const [response, err] = await apiRequest(
      combine(oneByte(2), encodeInt(whitelistee))
    );
    if (err !== "") {
      return err;
    }
    let signingKeys = await localGet("signingKeys");
    if (signingKeys === null) {
      signingKeys = {};
    }
    signingKeys[whitelistee] = response;
    await localSet("signingKeys", signingKeys);
    sendEditorInfo();
    return "";
  }

  async function sendWhitelistRequest(
    whitelistee: number,
    keys: Keys,
    myName: number
  ): Promise<string> {
    const [powInfo, powErr] = await getPowInfo();
    if (powErr !== "") {
      return powErr;
    }
    const pow = proofOfWork(powInfo);
    const [authCode, authErr] = await getAuthCode();
    if (authErr !== "") {
      return authErr;
    }
    const idToken = makeIdToken(
      10,
      combine(pow, encodeInt(whitelistee)),
      authCode,
      keys.signing.secretKey,
      myName
    );
    const request = combine(
      oneByte(10),
      combine(idToken, combine(pow, encodeInt(whitelistee)))
    );
    const response = await apiRequest(request);
    const apiErr = response[1];
    if (apiErr !== "") {
      return apiErr;
    }
    const updateContactsErr = await updateContacts(whitelistee);
    return updateContactsErr;
  }

  async function sendMessages(
    messages: MsgOut[],
    keys: Keys,
    myName: number
  ): Promise<string> {
    let responses = [];
    const messagesLength = messages.length;
    for (let i = 0; i < messagesLength; i++) {
      const [response, err] = await sendMessage(messages[i], keys, myName);
      if (err !== "") {
        return err;
      }
      if (response !== null) {
        responses.push(response);
      }
    }
    return "";
  }

  function getLeftOvers(
    used: Set<number>,
    rawDownloads: Decrypted[]
  ): Decrypted[] {
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

  function equalBytes(a: Uint8Array, b: Uint8Array): boolean {
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

  async function cacheReceipt(validReceipt: Uint8Array): Promise<void> {
    let receipts = await localGet("receipts");
    if (receipts === null) {
      receipts = [];
    }
    receipts.push(validReceipt);
    await localSet("receipts", receipts);
  }

  async function lookupSigningKey(author: number): Promise<Uint8Array> {
    const keys = await localGet("signingKeys");
    return keys[author];
  }

  async function validateReceipt(
    rawChunk: Decrypted
  ): Promise<[Uint8Array, string]> {
    const author = decodeInt(rawChunk.chunk.slice(1, 5));
    const key = await lookupSigningKey(author);
    if (key === null) {
      return [nullUint8Array(), "no key for author"];
    }
    const signed = signOpen(rawChunk.chunk.slice(5), key);
    if (signed === null) {
      return [nullUint8Array(), "bad signature"];
    }
    const combined = combine(
      rawChunk.chunk.slice(1, 5),
      combine(rawChunk.chunk.slice(5), signed)
    );
    return [combined, ""];
  }

  interface Chunk {
    readonly counter: number;
    readonly totalChunks: number;
    readonly totalHash: Uint8Array;
    readonly chunk: Uint8Array;
    readonly author: number;
  }

  const nullChunk = {
    counter: 0,
    totalChunks: 0,
    totalHash: nullUint8Array(),
    chunk: nullUint8Array(),
    author: 0,
  };

  async function decodeChunk(
    rawChunk: Decrypted
  ): Promise<[Chunk, boolean, string]> {
    const chunkLength: number = rawChunk.chunk.length;
    if (chunkLength < 41) {
      return [
        nullChunk,
        false,
        "chunk is only " +
          chunkLength +
          " bytes long, but should be at least 41",
      ];
    }

    if (rawChunk.chunk[0] === 0) {
      const [validReceipt, receiptErr] = await validateReceipt(rawChunk);
      if (receiptErr !== "") {
        return [nullChunk, true, receiptErr];
      }
      await cacheReceipt(validReceipt);
      return [nullChunk, true, ""];
    }

    const chunkNot0 = rawChunk.chunk.slice(1);

    return [
      {
        counter: decodeInt(chunkNot0.slice(0, 4)),
        totalChunks: decodeInt(chunkNot0.slice(4, 8)),
        totalHash: chunkNot0.slice(8, 40),
        chunk: chunkNot0.slice(40),
        author: rawChunk.author,
      },
      false,
      "",
    ];
  }

  async function decodeChunks(
    rawChunks: Decrypted[]
  ): Promise<[Chunk[], string]> {
    let allDecoded: Chunk[] = [];
    const chunksLength: number = rawChunks.length;
    for (let i = 0; i < chunksLength; i++) {
      const [decoded, isReceipt, err]: [
        Chunk,
        boolean,
        string
      ] = await decodeChunk(rawChunks[i]);
      if (err !== "") {
        return [[], err];
      }
      if (isReceipt) {
        continue;
      }
      allDecoded.push(decoded);
    }
    return [allDecoded, ""];
  }

  function getRelevantChunks(
    decodedDownloads: Chunk[],
    totalHash: Uint8Array
  ): [Chunk[], Set<number>] {
    const decodedLength = decodedDownloads.length;
    let relevant = [];
    let newUsed: Set<number> = new Set();
    for (let i = 0; i < decodedLength; i++) {
      const decoded: Chunk = decodedDownloads[i];
      if (equalBytes(decoded.totalHash, totalHash)) {
        relevant.push(decoded);
        newUsed.add(i);
      }
    }
    return [relevant, newUsed];
  }

  function sortChunks(chunks: Chunk[]): Chunk[] {
    chunks.sort(function (c1: Chunk, c2: Chunk) {
      return c1.counter - c2.counter;
    });
    return chunks;
  }

  function encodeInt(theInt: number): Uint8Array {
    let buffer = new ArrayBuffer(8);
    let result = new Uint8Array(buffer);

    // The reason for only going up to 4, and not to 8 is that apparently when
    // you do bit shifts on numbers in JS, it uses 32-bit ints.  This'll do me
    // till I have 10^9 users!
    for (let i = 0; i < 4; i++) {
      const r = (theInt >> (i * 8)) & 0xff;
      result[i] = r;
    }
    return result;
  }

  async function sendReceipt(
    hash: Uint8Array,
    author: number,
    keys: Keys,
    myName: number
  ): Promise<string> {
    const signature = sign(hash, keys.signing.secretKey);
    const combined = combine(oneByte(0), combine(encodeInt(author), signature));
    const [theirEncryptionKey, getEncKeyErr] = await getEncryptionKey(
      author,
      keys.signing.secretKey
    );
    if (getEncKeyErr !== "") {
      return getEncKeyErr;
    }
    const nonce = await makeNonce();
    const encryptedChunk = box(
      combined,
      nonce,
      theirEncryptionKey,
      keys.encryption.secretKey
    );
    const withNonce = combine(nonce, encryptedChunk);
    const [authCode, authErr] = await getAuthCode();
    if (authErr !== "") {
      return authErr;
    }
    const [signingKey, signKeyErr] = await getRecipientSigningKey(author);
    if (signKeyErr !== "") {
      return signKeyErr;
    }
    const msg = constructCtoCMessage(
      withNonce,
      signingKey,
      keys,
      authCode,
      myName
    );
    const response = await apiRequest(msg);
    return response[1];
  }

  async function joinChunks(
    chunks: Chunk[],
    keys: Keys,
    myName: number
  ): Promise<[Uint8Array, string]> {
    const chunksLength = chunks.length;
    let assembled = chunks[0].chunk;
    for (let i = 1; i < chunksLength; i++) {
      assembled = combine(assembled, chunks[i].chunk);
    }
    const hash = sha512(assembled).slice(0, 32);
    if (!equalBytes(hash, chunks[0].totalHash)) {
      return [nullUint8Array(), "assembled chunk did not match expected hash"];
    }

    const author = chunks[0].author;
    const receiptErr = await sendReceipt(hash, author, keys, myName);
    if (receiptErr !== "") {
      return [nullUint8Array(), receiptErr];
    }

    const encodedAuthor = encodeInt(author);
    return [combine(encodedAuthor, assembled), ""];
  }

  function nullUint8Array(): Uint8Array {
    let buf = new ArrayBuffer(0);
    const view = new Uint8Array(buf);
    return view;
  }

  function chunksAllThere(sortedChunks: Chunk[]): boolean {
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

  async function unpackOneDownload(
    i: number,
    decodedDownloads: Chunk[],
    keys: Keys,
    oldUsed: Set<number>,
    myName: number
  ): Promise<[Uint8Array, Set<number>, boolean, string]> {
    const start: Chunk = decodedDownloads[i];
    if (start.counter !== 0) {
      return [nullUint8Array(), oldUsed, false, ""];
    }

    if (start.totalChunks === 1) {
      const chunkHash = sha512(start.chunk);
      if (!equalBytes(chunkHash, start.totalHash)) {
        return [
          nullUint8Array(),
          new Set(),
          false,
          "single-chunk message with bad hash",
        ];
      }
    }

    const [relevantChunks, newUsed] = getRelevantChunks(
      decodedDownloads,
      start.totalHash
    );

    const relevantChunksLength = relevantChunks.length;
    if (relevantChunksLength === 0) {
      return [
        nullUint8Array(),
        new Set(),
        false,
        "why are there no relevant chunks?",
      ];
    }

    const sortedChunks = sortChunks(relevantChunks);
    if (!chunksAllThere(sortedChunks)) {
      return [nullUint8Array(), new Set(), false, ""];
    }
    const [completeMessage, joinErr]: [Uint8Array, string] = await joinChunks(
      sortedChunks,
      keys,
      myName
    );
    return [completeMessage, newUsed, true, joinErr];
  }

  async function unpackDownloads(
    rawDownloads: Decrypted[],
    keys: Keys,
    myName: number
  ): Promise<[Uint8Array[], Decrypted[], string]> {
    const numDownloads = rawDownloads.length;
    const [decodedDownloads, decodeErr] = await decodeChunks(rawDownloads);
    if (decodeErr !== "") {
      return [[], [], decodeErr];
    }
    let allUnpacked: Uint8Array[] = [];
    let used: Set<number> = new Set();
    for (let i = 0; i < numDownloads; i++) {
      let unpacked, unpackErr, done;
      [unpacked, used, done, unpackErr] = await unpackOneDownload(
        i,
        decodedDownloads,
        keys,
        used,
        myName
      );
      if (unpackErr !== "") {
        return [[], [], unpackErr];
      }
      if (done) {
        allUnpacked.push(unpacked);
      }
    }
    return [allUnpacked, getLeftOvers(used, rawDownloads), ""];
  }

  async function downloadNewMessages(
    keys: Keys,
    myName: number
  ): Promise<[Uint8Array[], string]> {
    const messages = [];
    while (true) {
      const [authCode, authErr] = await getAuthCode();
      if (authErr !== "") {
        return [[], authErr];
      }

      const idToken = makeIdToken(
        9,
        nullUint8Array(),
        authCode,
        keys.signing.secretKey,
        myName
      );
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
    return [messages, ""];
  }

  async function uploadEncryptionKey(
    keys: Keys,
    myName: number
  ): Promise<string> {
    const signedKey = sign(keys.encryption.publicKey, keys.signing.secretKey);
    const request = combine(oneByte(12), combine(encodeInt(myName), signedKey));
    const response = await apiRequest(request);
    return response[1];
  }

  async function getMyName(keys: Keys): Promise<[number, string]> {
    let myName = await localGet("myName");
    if (myName === null) {
      let myNameErr;
      [myName, myNameErr] = await sendMakeMyName(keys);
      if (myNameErr !== "") {
        return [0, myNameErr];
      }
      await localSet("myName", myName);
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

    const responseErr = await sendMessages(messages, keys, myName);
    if (responseErr !== "") {
      return responseErr;
    }

    await localRemove("outbox");

    const [rawDownloads, downloadErr] = await downloadNewMessages(keys, myName);
    if (downloadErr !== "") {
      return downloadErr;
    }

    const [decryptedDownloads, decryptErr]: [
      Decrypted[],
      string
    ] = await decryptDownloads(rawDownloads, keys);
    if (decryptErr !== "") {
      return decryptErr;
    }

    const oldLeftovers = await readOldLeftovers();

    const [unpacked, leftOvers, unpackErr] = await unpackDownloads(
      decryptedDownloads.concat(oldLeftovers),
      keys,
      myName
    );
    if (unpackErr !== "") {
      return unpackErr;
    }

    cacheLeftovers(leftOvers);

    const cacheErr = await cacheMessages(unpacked);
    if (cacheErr !== "") {
      return cacheErr;
    }
    return "";
  }

  async function cacheMessages(newMessages: Uint8Array[]): Promise<string> {
    let rawOldMessages: Uint8Array = await localGet("inbox");
    let decodedMessages: Uint8Array[];
    if (rawOldMessages !== null) {
      let err1: string;
      [decodedMessages, err1] = decodeInbox(rawOldMessages);
      if (err1 !== "") {
        return err1;
      }
    } else {
      decodedMessages = [];
    }
    const combined: Uint8Array[] = decodedMessages.concat(newMessages);
    if (newMessages.length === 0) {
      return "";
    }
    const encoded = encodeMessages(combined);
    await localSet("inbox", encoded);
    return "";
  }

  function decodeInbox(rawMessages: Uint8Array): [Uint8Array[], string] {
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

  function encodeMessage(message: Uint8Array): Uint8Array {
    const length = encodeInt(message.length);
    return combine(length, message);
  }

  function encodeMessages(messages: Uint8Array[]): Uint8Array[] {
    const messagesLength: number = messages.length;
    let encoded = [encodeMessage(messages[0])];
    for (let i = 1; i < messagesLength; i++) {
      encoded.push(encodeMessage(messages[1]));
    }
    return encoded;
  }

  async function cacheLeftovers(leftOvers: Decrypted[]) {
    await localSet("leftovers", leftOvers);
  }

  async function readOldLeftovers(): Promise<Decrypted[]> {
    let leftovers = await localGet("leftovers");
    if (leftovers === null) {
      return [];
    }
    return leftovers;
  }

  interface Decrypted {
    readonly author: number;
    readonly chunk: Uint8Array;
  }

  async function decryptDownloads(
    rawDownloads: Uint8Array[],
    keys: Keys
  ): Promise<[Decrypted[], string]> {
    const lengthDownloads = rawDownloads.length;
    const messages = [];
    for (let i = 0; i < lengthDownloads; i++) {
      const download = rawDownloads[i];
      if (download.length < 122) {
        return [[], "raw message less than 122 bytes long"];
      }
      const nonce = download.slice(121, 145);
      const encryptedBlob = download.slice(145);
      const author = decodeInt(download.slice(1, 9));
      const [theirEncryptionKey, encKeyErr] = await getEncryptionKey(
        author,
        keys.signing.secretKey
      );
      if (encKeyErr !== "") {
        return [[], encKeyErr];
      }
      const chunk = boxOpen(
        encryptedBlob,
        nonce,
        theirEncryptionKey,
        keys.encryption.secretKey
      );
      if (chunk === null) {
        return [[], "could not authenticate chunk"];
      }
      const decrypted = {
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
    console.log("top of communicate port");
    communicateMain().then(function (err) {
      if (err !== "") {
        app.ports.communicationError.send(err);
      }
    });
  });

  const nullInbox = "There is no inbox!";

  app.ports.getImporterInfo.subscribe(function () {
    localGet("editorCache").then(function (rawEditorCache) {
      let editorCache = nullCache;
      if (rawEditorCache !== null) {
        editorCache = fromBytes(rawEditorCache);
      }
      localGet("inbox").then(function (rawInbox) {
        let inbox = nullInbox;
        if (rawInbox !== null) {
          inbox = fromBytes(rawInbox);
        }
        app.ports.gotImporterInfo.send({
          editorCache: editorCache,
          inbox: inbox,
        });
      });
    });
  });

  app.ports.sendMessagePort.subscribe(function (rawB64: string): void {
    localGet("outbox").then(function (outbox) {
      if (outbox === null) {
        outbox = [];
      }
      outbox.push(toBytes(rawB64));
      localSet("outbox", outbox);
    });
  });
})();

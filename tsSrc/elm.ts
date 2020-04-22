(function () {
  "use strict";

  function parseWat(code: string): Uint8Array {
    // @ts-ignore
    const wasm = wabt.parseWat("", code).toBinary.buffer;
    return wasm;
  }

  async function localGet(key: string): Promise<any> {
    // @ts-ignore
    const value = await localforage.getItem(key);
    return value;
  }

  async function localSet(key: string, value: any): Promise<void> {
    // @ts-ignore
    await localforage.setItem(key, value);
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

  interface EditorInfo {
    myName: number;
    inbox: string[];
    drafts: string[];
    myContacts: number[];
    outbox: string[];
  }

  const nullEditorInfo: EditorInfo = {
    myName: 0,
    inbox: [],
    myContacts: [],
    drafts: [],
    outbox: [],
  };

  async function readMessages(key: string): Promise<string[]> {
    let raw: Uint8Array[] | null = await localGet(key);
    if (raw === null) {
      raw = [];
    }
    let messages: string[] = [];
    const rawLength = raw.length;
    for (let i = 0; i < rawLength; i++) {
      messages.push(fromBytes(raw[i]));
    }
    return messages;
  }

  async function getEditorInfo(): Promise<[EditorInfo, string]> {
    const inbox: string[] = await readMessages("inbox");
    const drafts: string[] = await readMessages("drafts");
    const outbox: string[] = await readMessages("outbox");

    const keys: Keys = await getCryptoKeys();

    const [myName, myNameErr]: [number, string] = await getMyName(keys);
    if (myNameErr !== "") {
      return [nullEditorInfo, myNameErr];
    }

    const errUpCrypt: string = await uploadEncryptionKey(keys, myName);
    if (errUpCrypt !== "") {
      return [nullEditorInfo, errUpCrypt];
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
      inbox: inbox,
      myContacts: myContacts,
      drafts: drafts,
      outbox: outbox,
    };
    return [toSend, ""];
  }

  function sendEditorInfo(): void {
    getEditorInfo().then(function (infoOrErr: [EditorInfo, string]) {
      const [info, err]: [EditorInfo, string] = infoOrErr;
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

  function createCryptoKeys(): Keys {
    return {
      encryption: boxKeyPair(),
      signing: signKeyPair(),
    };
  }

  async function getCryptoKeys(): Promise<Keys> {
    const keysKey: string = "cryptokeys";
    let keys: Keys = await localGet(keysKey);
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
    let buf: ArrayBuffer = new ArrayBuffer(lena + lenb);
    let combined: Uint8Array = new Uint8Array(buf);
    for (let i = 0; i < lena; i++) {
      combined[i] = a[i];
    }
    for (let i = lena; i < lena + lenb; i++) {
      const bval: number = b[i - lena];
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
    const unique: Uint8Array = powInfo.unique;
    let buffer: ArrayBuffer = new ArrayBuffer(8);
    let bufferView: Uint8Array = new Uint8Array(buffer);
    let counter: Int32Array = new Int32Array(buffer);
    while (true) {
      const combined: Uint8Array = combine(unique, bufferView);
      const hash: Uint8Array = sha512(combined).slice(0, 32);
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
    let buffer: ArrayBuffer = new ArrayBuffer(49);
    let bufferView: Uint8Array = new Uint8Array(buffer);
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
    const response: Response = await fetch("/api", {
      method: "POST",
      headers: { "Content-Type": "application/octet-stream" },
      body: requestBody,
    });

    const body: ArrayBuffer = await response.arrayBuffer();
    const bodyArray: Uint8Array = new Uint8Array(body);

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
    const lenArr: number = arr.length;
    for (let i = 0; i < lenArr; i++) {
      numbers.push(arr[i]);
    }
    return numbers;
  }

  const nullPowInfo: PowInfo = { difficulty: 0, unique: nullUint8Array() };

  async function getPowInfo(): Promise<[PowInfo, string]> {
    const [response, responseErr]: [Uint8Array, string] = await apiRequest(
      oneByte(3)
    );
    if (responseErr !== "") {
      return [nullPowInfo, responseErr];
    }

    if (response.length !== 9) {
      return [
        nullPowInfo,
        "powInfo response is not 9 bytes long, it is " + response.length,
      ];
    }

    const powInfo: PowInfo = {
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
    const request: Uint8Array = makeMakeMyNameRequest(
      pow,
      keys.signing.publicKey
    );
    const [response, responseErr]: [Uint8Array, string] = await apiRequest(
      request
    );
    if (responseErr !== "") {
      return [0, err];
    }
    if (response.length !== 8) {
      return [0, "response is not 8 bytes long"];
    }
    return [decodeInt(response), ""];
  }

  // The chunk is the nonce plus the encrypted message.
  // The result is:
  // + 112 bytes: idToken
  // + 8 bytes: recipient
  // + 24 bytes: nonce
  // + <= 15kB: encrypted chunk
  function constructCtoCMessage(
    chunk: Uint8Array,
    recipient: number,
    keys: Keys,
    authCode: Uint8Array,
    myName: number
  ): Uint8Array {
    const recipientEncoded = encodeInt(recipient);
    const idToken: Uint8Array = makeIdToken(
      8,
      combine(recipientEncoded, chunk),
      authCode,
      keys.signing.secretKey,
      myName
    );
    const request: Uint8Array = combine(
      oneByte(8),
      combine(idToken, combine(recipientEncoded, chunk))
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
    const chunkLength: number = 15000;
    const hash: Uint8Array = sha512(message).slice(0, 32);
    const numChunks: number = Math.ceil(message.length / chunkLength);
    const numChunksBytes: Uint8Array = encodeInt32(numChunks);
    let chunks: Uint8Array[] = [];
    for (let i = 0; i < numChunks; i++) {
      const chunkNum: Uint8Array = encodeInt32(i);
      const chunkStart: number = i * chunkLength;
      const chunkEnd: number = (i + 1) * chunkLength;
      const chunkBase: Uint8Array = message.slice(chunkStart, chunkEnd);
      const combined: Uint8Array = combine(
        oneByte(1),
        combine(chunkNum, combine(numChunksBytes, combine(hash, chunkBase)))
      );
      chunks.push(combined);
    }
    return chunks;
  }

  async function makeNonce(): Promise<Uint8Array> {
    const counter: number = await localGet("noncecounter");
    let buffer: ArrayBuffer = new ArrayBuffer(24);
    let uintArr: Uint8Array = new Uint8Array(buffer);
    let bufferView: Int32Array = new Int32Array(buffer);
    bufferView[0] = counter;
    await localSet("noncecounter", counter + 1);
    return uintArr;
  }

  async function getEncryptionKey(
    username: number,
    signingKey: Uint8Array
  ): Promise<[Uint8Array, string]> {
    const request: Uint8Array = combine(oneByte(13), encodeInt(username));
    const [response, responseErr]: [Uint8Array, string] = await apiRequest(
      request
    );
    if (responseErr !== "") {
      return [nullUint8Array(), responseErr];
    }

    if (response.length !== 96) {
      return [nullUint8Array(), "signed encryption key is not 96 bytes long"];
    }
    const encryptionKey: Uint8Array | null = signOpen(response, signingKey);
    if (encryptionKey === null) {
      return [nullUint8Array(), "could not verify encryption key signature"];
    }

    return [encryptionKey, ""];
  }

  interface SigningKeys {
    [key: number]: Uint8Array;
  }

  async function getRecipientSigningKey(
    recipient: number
  ): Promise<[Uint8Array, string]> {
    const signingKeys: SigningKeys = await localGet("signingKeys");
    if (signingKeys === null) {
      return [nullUint8Array(), "there are no signing keys"];
    }
    const key: Uint8Array = signingKeys[recipient];
    if (key === undefined) {
      return [nullUint8Array(), "no signing key for recipient " + recipient];
    }
    return [key, ""];
  }

  async function sendClientToClient(
    message: Uint8Array,
    keys: Keys,
    myName: number
  ): Promise<string> {
    const to: number = decodeInt32(message.slice(0, 4));
    const chunks: Uint8Array[] = chopMessageIntoChunks(message);
    const chunksLength: number = chunks.length;
    const [signingKey, signKeyErr]: [
      Uint8Array,
      string
    ] = await getRecipientSigningKey(to);
    if (signKeyErr !== "") {
      return signKeyErr;
    }
    const [encryptionKey, encKeyErr]: [
      Uint8Array,
      string
    ] = await getEncryptionKey(to, signingKey);

    if (encKeyErr !== "") {
      return encKeyErr;
    }
    for (let i = 0; i < chunksLength; i++) {
      const [authCode, authErr]: [Uint8Array, string] = await getAuthCode();
      if (authErr !== "") {
        return authErr;
      }

      const chunk: Uint8Array = chunks[i];
      const nonce: Uint8Array = await makeNonce();
      const encryptedChunk = box(
        chunk,
        nonce,
        encryptionKey,
        keys.encryption.secretKey
      );

      const withNonce: Uint8Array = combine(nonce, encryptedChunk);

      const subMsg: Uint8Array = constructCtoCMessage(
        withNonce,
        to,
        keys,
        authCode,
        myName
      );

      const response: [Uint8Array, string] = await apiRequest(subMsg);
      const err: string = response[1];
      if (err !== "") {
        return err;
      }
    }
    return "";
  }

  async function getAuthCode(): Promise<[Uint8Array, string]> {
    let buffer: ArrayBuffer = new ArrayBuffer(1);
    let request: Uint8Array = new Uint8Array(buffer);
    request[0] = 7;
    const [authCode, err]: [Uint8Array, string] = await apiRequest(request);
    return [authCode, err];
  }

  function oneByte(route: number): Uint8Array {
    let buffer: ArrayBuffer = new ArrayBuffer(1);
    let view: Uint8Array = new Uint8Array(buffer);
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
    const toSign: Uint8Array = combine(
      oneByte(route),
      combine(message, authCode)
    );
    const hash: Uint8Array = sha512(toSign).slice(0, 32);
    const signature: Uint8Array = sign(hash, secretSign);
    return combine(encodeInt(myName), combine(authCode, signature));
  }

  async function updateContacts(whitelistee: number): Promise<string> {
    const [response, err]: [Uint8Array, string] = await apiRequest(
      combine(oneByte(2), encodeInt(whitelistee))
    );
    if (err !== "") {
      return err;
    }
    let signingKeys: SigningKeys = await localGet("signingKeys");
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
    const pow: Uint8Array = proofOfWork(powInfo);
    const [authCode, authErr]: [Uint8Array, string] = await getAuthCode();
    if (authErr !== "") {
      return authErr;
    }
    const idToken: Uint8Array = makeIdToken(
      10,
      combine(pow, encodeInt(whitelistee)),
      authCode,
      keys.signing.secretKey,
      myName
    );
    const request: Uint8Array = combine(
      oneByte(10),
      combine(idToken, combine(pow, encodeInt(whitelistee)))
    );
    const response: [Uint8Array, string] = await apiRequest(request);
    const apiErr: string = response[1];
    if (apiErr !== "") {
      return apiErr;
    }
    const updateContactsErr: string = await updateContacts(whitelistee);
    return updateContactsErr;
  }

  function getLeftOvers(
    used: Set<number>,
    rawDownloads: Decrypted[]
  ): Decrypted[] {
    const numDownloads: number = rawDownloads.length;
    let leftOvers: Decrypted[] = [];
    for (let i = 0; i < numDownloads; i++) {
      if (used.has(i)) {
        continue;
      }
      leftOvers.push(rawDownloads[i]);
    }
    return leftOvers;
  }

  function equalBytes(a: Uint8Array, b: Uint8Array): boolean {
    const alength: number = a.length;
    const blength: number = b.length;
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
    let receipts: Uint8Array[] | null = await localGet("receipts");
    if (receipts === null) {
      receipts = [];
    }
    receipts.push(validReceipt);
    await localSet("receipts", receipts);
  }

  async function lookupSigningKey(author: number): Promise<Uint8Array | null> {
    const keys: SigningKeys = await localGet("signingKeys");
    return keys[author];
  }

  async function validateReceipt(
    rawChunk: Decrypted
  ): Promise<[Uint8Array, string]> {
    const author: number = decodeInt(rawChunk.chunk.slice(1, 5));
    const key: Uint8Array | null = await lookupSigningKey(author);
    if (key === null) {
      return [nullUint8Array(), "no key for author"];
    }
    const signed: Uint8Array | null = signOpen(rawChunk.chunk.slice(5), key);
    if (signed === null) {
      return [nullUint8Array(), "bad signature"];
    }
    const combined: Uint8Array = combine(
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
    if (chunkLength < 42) {
      return [
        nullChunk,
        false,
        "chunk is only " +
          chunkLength +
          " bytes long, but should be at least 42",
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

    const chunkNot0: Uint8Array = rawChunk.chunk.slice(1);

    return [
      {
        counter: decodeInt32(chunkNot0.slice(0, 4)),
        totalChunks: decodeInt32(chunkNot0.slice(4, 8)),
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
    const decodedLength: number = decodedDownloads.length;
    let relevant: Chunk[] = [];
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

  function encodeInt32(theInt: number): Uint8Array {
    let buffer: ArrayBuffer = new ArrayBuffer(4);
    let result: Uint8Array = new Uint8Array(buffer);
    for (let i = 0; i < 4; i++) {
      result[i] = (theInt >> (i * 8)) & 0xff;
    }
    return result;
  }

  function encodeInt(theInt: number): Uint8Array {
    let buffer: ArrayBuffer = new ArrayBuffer(8);
    let result: Uint8Array = new Uint8Array(buffer);

    // The reason for only going up to 4, and not to 8 is that apparently when
    // you do bit shifts on numbers in JS, it uses 32-bit ints.  This'll do me
    // till I have 10^9 users!
    for (let i = 0; i < 4; i++) {
      const r: number = (theInt >> (i * 8)) & 0xff;
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
    const signature: Uint8Array = sign(hash, keys.signing.secretKey);
    const combined: Uint8Array = combine(
      oneByte(0),
      combine(encodeInt(author), signature)
    );

    const [signingKey, signKeyErr]: [
      Uint8Array,
      string
    ] = await getRecipientSigningKey(author);
    if (signKeyErr !== "") {
      return signKeyErr;
    }

    const [theirEncryptionKey, getEncKeyErr]: [
      Uint8Array,
      string
    ] = await getEncryptionKey(author, signingKey);
    if (getEncKeyErr !== "") {
      return getEncKeyErr;
    }
    const nonce: Uint8Array = await makeNonce();
    const encryptedChunk = box(
      combined,
      nonce,
      theirEncryptionKey,
      keys.encryption.secretKey
    );
    const withNonce: Uint8Array = combine(nonce, encryptedChunk);
    const [authCode, authErr]: [Uint8Array, string] = await getAuthCode();
    if (authErr !== "") {
      return authErr;
    }
    const msg: Uint8Array = constructCtoCMessage(
      withNonce,
      author,
      keys,
      authCode,
      myName
    );
    const response: [Uint8Array, string] = await apiRequest(msg);
    return response[1];
  }

  async function joinChunks(
    chunks: Chunk[],
    keys: Keys,
    myName: number
  ): Promise<[Uint8Array, string]> {
    const chunksLength: number = chunks.length;
    let assembled: Uint8Array = chunks[0].chunk;
    for (let i = 1; i < chunksLength; i++) {
      assembled = combine(assembled, chunks[i].chunk);
    }
    const hash: Uint8Array = sha512(assembled).slice(0, 32);
    if (!equalBytes(hash, chunks[0].totalHash)) {
      return [nullUint8Array(), "assembled chunk did not match expected hash"];
    }

    const author: number = chunks[0].author;
    const receiptErr: string = await sendReceipt(hash, author, keys, myName);
    if (receiptErr !== "") {
      return [nullUint8Array(), receiptErr];
    }

    return [assembled, ""];
  }

  function nullUint8Array(): Uint8Array {
    let buf: ArrayBuffer = new ArrayBuffer(0);
    const view: Uint8Array = new Uint8Array(buf);
    return view;
  }

  function chunksAllThere(sortedChunks: Chunk[]): boolean {
    const chunksLength: number = sortedChunks.length;
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
      const chunkHash: Uint8Array = sha512(start.chunk).slice(0, 32);
      if (!equalBytes(chunkHash, start.totalHash)) {
        return [
          nullUint8Array(),
          new Set(),
          false,
          "single-chunk message with bad hash",
        ];
      }
      return [start.chunk, new Set([i]), true, ""];
    }

    const [relevantChunks, newUsed]: [Chunk[], Set<number>] = getRelevantChunks(
      decodedDownloads,
      start.totalHash
    );

    const relevantChunksLength: number = relevantChunks.length;
    if (relevantChunksLength === 0) {
      return [
        nullUint8Array(),
        new Set(),
        false,
        "why are there no relevant chunks?",
      ];
    }

    const sortedChunks: Chunk[] = sortChunks(relevantChunks);
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
    const numDownloads: number = rawDownloads.length;
    const [decodedDownloads, decodeErr]: [Chunk[], string] = await decodeChunks(
      rawDownloads
    );
    if (decodeErr !== "") {
      return [[], [], decodeErr];
    }
    let allUnpacked: Uint8Array[] = [];
    let used: Set<number> = new Set();
    for (let i = 0; i < numDownloads; i++) {
      if (used.has(i)) {
        continue;
      }
      let unpacked: Uint8Array, unpackErr: string, done: boolean;
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
      const [authCode, authErr]: [Uint8Array, string] = await getAuthCode();
      if (authErr !== "") {
        return [[], authErr];
      }

      const idToken: Uint8Array = makeIdToken(
        9,
        nullUint8Array(),
        authCode,
        keys.signing.secretKey,
        myName
      );
      const request: Uint8Array = combine(oneByte(9), idToken);
      const [response, apiErr]: [Uint8Array, string] = await apiRequest(
        request
      );
      if (apiErr !== "") {
        return [[], apiErr];
      }

      if (response[0] === 0) {
        return [messages, ""];
      } else {
        messages.push(response.slice(2));
      }
    }
    return [messages, ""];
  }

  async function uploadEncryptionKey(
    keys: Keys,
    myName: number
  ): Promise<string> {
    const signedKey: Uint8Array = sign(
      keys.encryption.publicKey,
      keys.signing.secretKey
    );
    const request: Uint8Array = combine(
      oneByte(12),
      combine(encodeInt(myName), signedKey)
    );
    const response: [Uint8Array, string] = await apiRequest(request);
    return response[1];
  }

  async function getMyName(keys: Keys): Promise<[number, string]> {
    let myName: number | null = await localGet("myName");
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
    const keys: Keys = await getCryptoKeys();

    const [myName, myNameErr]: [number, string] = await getMyName(keys);
    if (myNameErr !== "") {
      return myNameErr;
    }

    const [rawDownloads, downloadErr]: [
      Uint8Array[],
      string
    ] = await downloadNewMessages(keys, myName);
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

    const oldLeftovers: Decrypted[] = await readOldLeftovers();

    const [unpacked, leftOvers, unpackErr]: [
      Uint8Array[],
      Decrypted[],
      string
    ] = await unpackDownloads(
      decryptedDownloads.concat(oldLeftovers),
      keys,
      myName
    );
    if (unpackErr !== "") {
      return unpackErr;
    }

    cacheLeftovers(leftOvers);

    const cacheErr: string = await cacheMessages(unpacked);
    if (cacheErr !== "") {
      return cacheErr;
    }
    sendEditorInfo();
    return "";
  }

  async function cacheMessages(newMessages: Uint8Array[]): Promise<string> {
    if (newMessages.length === 0) {
      return "";
    }
    let oldMessages: Uint8Array[] = await localGet("inbox");
    if (oldMessages === null) {
      oldMessages = [];
    }
    const combined: Uint8Array[] = oldMessages.concat(newMessages);
    await localSet("inbox", combined);
    return "";
  }

  async function cacheLeftovers(leftOvers: Decrypted[]) {
    await localSet("leftovers", leftOvers);
  }

  async function readOldLeftovers(): Promise<Decrypted[]> {
    let leftovers: Decrypted[] | null = await localGet("leftovers");
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
    const lengthDownloads: number = rawDownloads.length;
    const messages: Decrypted[] = [];
    // One download is:
    // + 0x08: the upload indicator byte
    // + 112 bytes: identity token
    // + 8 bytes: recipient ID
    // + 24 bytes: nonce
    //
    // The rest is encrypted:
    // + 4 bytes: int chunk counter
    // + 4 bytes: int total chunks in message
    // + 32 bytes: hash of complete message
    // + all the rest: chunk
    for (let i = 0; i < lengthDownloads; i++) {
      const download: Uint8Array = rawDownloads[i];
      if (download.length < 146) {
        return [[], "raw message less than 146 bytes long"];
      }
      const nonce: Uint8Array = download.slice(120, 144);
      const encryptedBlob: Uint8Array = download.slice(144);
      const author: number = decodeInt(download.slice(0, 8));
      const [authorSignKey, authorKeyErr]: [
        Uint8Array,
        string
      ] = await getRecipientSigningKey(author);
      if (authorKeyErr !== "") {
        return [[], authorKeyErr];
      }
      const [theirEncryptionKey, encKeyErr]: [
        Uint8Array,
        string
      ] = await getEncryptionKey(author, authorSignKey);
      if (encKeyErr !== "") {
        return [[], encKeyErr];
      }
      const chunk: Uint8Array | null = boxOpen(
        encryptedBlob,
        nonce,
        theirEncryptionKey,
        keys.encryption.secretKey
      );
      if (chunk === null) {
        return [[], "could not authenticate chunk"];
      }
      const decrypted: Decrypted = {
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
    //   + 4 bytes: sender
    //   + message
    communicateMain().then(function (err) {
      if (err !== "") {
        app.ports.communicationError.send(err);
      }
    });
  });

  app.ports.whitelistPort.subscribe(function (id: number): void {
    handleWhitelistRequest(id).then(function (err: string) {
      if (err !== "") {
        app.ports.badWhitelist.send(err);
      } else {
        app.ports.goodWhitelist.send(id);
      }
    });
  });

  async function handleWhitelistRequest(id: number): Promise<string> {
    const keys: Keys = await getCryptoKeys();
    const [myName, myNameErr]: [number, string] = await getMyName(keys);
    if (myNameErr !== "") {
      return "could not get my name: " + myNameErr;
    }
    const whitelistErr: string = await sendWhitelistRequest(id, keys, myName);
    if (whitelistErr !== "") {
      return "bad whitelist request: " + whitelistErr;
    }

    const signingKey: [Uint8Array, string] = await getRecipientSigningKey(id);
    if (signingKey[1] !== "") {
      return signingKey[1];
    }

    const updateKeyError = await updateContacts(id);
    return updateKeyError;
  }

  async function cacheDraftsHelp(rawB64: string[]): Promise<void> {
    let asBytes: Uint8Array[] = [];
    const numMessages = rawB64.length;
    for (let i = 0; i < numMessages; i++) {
      asBytes.push(toBytes(rawB64[i]));
    }
    await localSet("drafts", asBytes);
  }

  async function cacheOutboxHelp(rawB64: string[]): Promise<void> {
    let asBytes: Uint8Array[] = [];
    const numMessages = rawB64.length;
    for (let i = 0; i < numMessages; i++) {
      asBytes.push(toBytes(rawB64[i]));
    }
    await localSet("outbox", asBytes);
  }

  app.ports.cacheDraftsPort.subscribe(function (rawB64: string[]) {
    cacheDraftsHelp(rawB64);
  });

  app.ports.cacheOutboxPort.subscribe(function (rawB64: string[]) {
    cacheOutboxHelp(rawB64);
  });

  async function sendMessageHelp(rawB64: string): Promise<void> {
    const keys: Keys = await getCryptoKeys();
    const [myName, myNameError]: [number, string] = await getMyName(keys);
    if (myNameError !== "") {
      app.ports.sendMessageError.send(myNameError);
      return;
    }
    const asBytes: Uint8Array = toBytes(rawB64);
    const sendError: string = await sendClientToClient(asBytes, keys, myName);
    if (sendError !== "") {
      app.ports.sendMessageError.send(sendError);
    }
  }

  app.ports.sendMessagePort.subscribe(function (rawB64: string): void {
    sendMessageHelp(rawB64);
  });


  interface ToRun {
    readonly wat: string;
    readonly userInput: string;
  }

  function makeMemory(userInput: string) {
    const stringEncoder = new TextEncoder();
    const encodedUserInput: Uint8Array = stringEncoder.encode(userInput);
    const inputLength = encodedUserInput.length
    const pages = Math.ceil(inputLength / 64000)
    const memory = new WebAssembly.Memory({initial:pages, maximum: 1000})
    const memoryBuffer = new Uint8Array(memory.buffer);
    
    for (let i = 0; i < inputLength; i++) {
      memoryBuffer[i] = encodedUserInput[i]
    }
    return memory
  }

  async function runWat(toRun: ToRun): Promise<Uint8Array> {
    const wasm = parseWat(toRun.wat);
    const memory = makeMemory(toRun.userInput)
    const imports = {env: { memory: memory }};
    const wasmModule = new WebAssembly.Module(wasm);
    const wasmInstance = new WebAssembly.Instance(wasmModule, imports)
    // @ts-ignore
    wasmInstance.exports.main();
    return new Uint8Array(memory.buffer);
  }

  app.ports.runWasmPort.subscribe(function (toRun: ToRun) {
    console.log("app.ports.runWasmPort");
    runWat(toRun).then(function(encodedDocument: Uint8Array) {
       app.ports.wasmDocumentPort.send(fromBytes(encodedDocument))
    })
  });

})();

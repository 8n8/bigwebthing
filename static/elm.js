var app = Elm.Main.init({node: document.getElementById('main')});

app.ports.requestHome.subscribe(function() {
  localforage.getItem('home').then(function(homeBytes) {
    let result = {exists: true, home: ""}
    if (homeBytes === null) {
      result.exists = false;
    } else {
      result.home = base64js.fromByteArray(homeBytes);
    }
    app.ports.retrievedHome.send(result);
  });
});

app.ports.requestHash.subscribe(function(base64hash) {
  localforage.getItem(base64hash).then(function(binaryBlob){
    if (binaryBlob === null) {
      return;
    }
    app.ports.retrievedHash.send(base64js.fromByteArray(binaryBlob));
  });
});

app.ports.cacheHome.subscribe(function(base64str) {
  localforage.setItem('home', base64js.toByteArray(base64str));
});

app.ports.getSecretKeys.subscribe(function() {
  const encryptionKeys = nacl.box.keyPair();
  const signingKeys = nacl.sign.keyPair();
  const secretEncrypt = base64js.fromByteArray(encryptionKeys.secretKey);
  const publicEncrypt = base64js.fromByteArray(encryptionKeys.publicKey);
  const secretSign = base64js.fromByteArray(signingKeys.secretKey);
  const publicSign = base64js.fromByteArray(signingKeys.publicKey);
  const keys = {publicencrypt: publicEncrypt, publicsign: publicSign, secretencrypt: secretEncrypt, secretsign: secretSign};
  app.ports.gotSecretKeys.send(keys);
});

function encodeInt(theInt) {
  let buffer = new ArrayBuffer(8)
  let int
}

function isDifficult(hash, difficulty) {
  const hashView = new Uint8Array(hash)
  for (let i = 0; i < difficulty; i++) {
    if (hashView[i] != 0) {
      return false
    }
  }
  return true
}

function combine(a, b) {
  let buf = new ArrayBuffer(16);
  let combined = new Uint8Array(buf);
  for (let i = 0; i < 8; i++) {
    combined[i] = a[i]
  }  
  const bView = new Uint8Array(b)
  for (let i = 8; i < 16; i++) {
    const bval = bView[i-8]
    combined[i] = bval
  }
  return combined
}

async function doHash(combined) {
  return crypto.subtle.digest('SHA-256', combined)
}

async function proofOfWork(powInfo) {
  const unique = base64js.toByteArray(powInfo.unique)
  let buffer = new ArrayBuffer(8)
  let counter = new Int32Array(buffer)
  while (true) {
    const combined = combine(unique, buffer) 
    const hash = await doHash(combined)
    const hashbytes = new Uint8Array(hash)
    if (isDifficult(hash, powInfo.difficulty)) {
      console.log("done pow")
      return combined
    }
    counter[0] = counter[0] + 1;
  }
}

app.ports.doProofOfWork.subscribe(function(powInfo) {
  proofOfWork(powInfo).then(function(pow) {
      console.log(pow);
      const b64 = base64js.fromByteArray(pow);
      console.log("base64Pow: " + b64)
      app.ports.doneProofOfWork.send(base64js.fromByteArray(pow));
  })
});

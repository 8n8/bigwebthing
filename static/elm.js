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

const powMax = 75

function isDifficult(hash, difficulty) {
  for (let i = 0; i < difficulty; i++) {
    if (hash[i] > powMax) {
      return false
    }
  }
  return true
}

function combine(a, b) {
  const lena = a.length;
  const lenb = b.length;
  let buf = new ArrayBuffer(lena + lenb);
  let combined = new Uint8Array(buf);
  for (let i = 0; i < lena; i++) {
    combined[i] = a[i]
  }  
  for (let i = lena; i < lena + lenb; i++) {
    const bval = b[i-lena]
    combined[i] = bval
  }
  return combined
}

async function doHash(combined) {
  return crypto.subtle.digest('SHA-512', combined);
}

function proofOfWork(powInfo) {
  const unique = base64js.toByteArray(powInfo.unique)
  let buffer = new ArrayBuffer(8)
  let bufferView = new Uint8Array(buffer)
  let counter = new Int32Array(buffer)
  while (true) {
    const combined = combine(unique, bufferView)
    const hash = nacl.hash(combined);
    if (isDifficult(hash, powInfo.difficulty)) {
      return combined
    }
    counter[0] = counter[0] + 1;
  }
}

app.ports.doProofOfWork.subscribe(function(powInfo) {
  const pow = proofOfWork(powInfo)
  const b64 = base64js.fromByteArray(pow);
  app.ports.doneProofOfWork.send(base64js.fromByteArray(pow));
});

function makeIdToken(route, message, authcode, secretsign) {
  const publicsign = nacl.sign.keyPair.fromSecretKey(secretsign).secretKey
  const toSign = combine(combine(route, message), authcode)
  const hash = nacl.hash(toSign).slice(0, 32)
  const signature = nacl.sign(hash, secretsign)
  const encodedIdToken = combine(combine(publicsign, authcode), signature)
  return encodedIdToken
}

app.ports.makeIdToken.subscribe(function(idTokenInfo) {
  const publicsign = new Uint8Array(base64js.toByteArray(idTokenInfo.publicsign));
  const secretsign = new Uint8Array(base64js.toByteArray(idTokenInfo.secretsign));
  const route = new Uint8Array(base64js.toByteArray(idTokenInfo.route));
  const authcode = new Uint8Array(base64js.toByteArray(idTokenInfo.authcode));
  const message = new Uint8Array(base64js.toByteArray(idTokenInfo.message));
  
  const encodedIdToken = makeIdToken(route, message, authcode, publicsign);
  app.ports.newIdToken.send(base64js.fromByteArray(encodedIdToken));
});

function makeRoute(route) {
  array = new ArrayBuffer(1)
  view = new Uint8Array(array)
  view = route
  return view
}

const apiUrl = 'http://localhost:3001/api'

app.ports.sendMsg.subscribe(function(msg) {
  const document = base64js.toByteArray(msg.document)
  const mySecretSign = base64js.toByteArray(msg.mySecretSign)
  const myPublicSign = nacl.sign.keyPair.fromSecretKey(mySecretSign).secretKey
  const mySecretEncrypt = base64js.toByteArray(msg.mySecretEncrypt)
  const toPublicSign = base64js.toByteArray(msg.toPublicSign)
  const nonce = base64js.toByteArray(msg.nonce)

  fetch(apiUrl, {body: makeRoute(7)}).then(function(response) {
    if (!response.ok) {
      console.log("could not get auth code: ", response)
      return
    }
    const authCode = response.arrayBuffer();
    const route = makeRoute(8)
    const idToken = makeIdToken(route, document, authCode, mySecretSign)
    const encoded = combine(route, combine(idToken, combine(toPublicSign, document)))
    fetch(apiUrl, {body: encoded}).then(function(msgResponse) {
      if (!response.ok) {
        console.log("could not send message: ", response)
      }
    })
  })
});

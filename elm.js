var app = Elm.Main.init({node: document.getElementById('main')});

app.ports.requestHome.subscribe(function() {
  localforage.getItem('home').then(function(homeBytes) {
    if (homeBytes === null) {
      return;
    }
    app.ports.retrievedHome.send(base64js.fromByteArray(homeBytes));
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

// app.ports.cacheHash.subscribe(function(base64str) {
//   const bytes = base64js.toByteArray(base64str);
//   const hash = nacl.hash(bytes);
//   localforage.setItem(base64js.fromByteArray(hash.slice(0, 32)), bytes);
// });

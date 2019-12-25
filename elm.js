var app = Elm.Main.init({'node': document.getElementById('main')});

app.ports.request.subscribe(function(key) {
  localforage.getItem(key).then(function(val) {
    app.ports.retrieved.send(JSON.Stringify({key: val}));
  });
});

app.ports.requestHome.subscribe(function() {
  localforage.getItem('home').then(function(homeBytes) {
    app.ports.retrievedHome.send(fromByteArray(homeBytes));
  });
});

app.ports.requestHash.subscribe(function(base64hash) {
  localforage.getItem(base64hash).then(function(binaryBlob){
    app.ports.retrievedHash.send(fromByteArray(binaryBlob));
  });
});

app.ports.cacheHome.subscribe(function(base64str) {
  localforage.setItem('home', toByteArray(base64str));
});

app.ports.cacheHash.subscribe(function(base64str) {
  const bytes = toByteArray(base64str);
  hash = nacl.hash(bytes);
  localforage.setItem(fromByteArray(hash.slice(0, 32)), bytes);
});

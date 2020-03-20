var app = Elm.Main.init({node: document.getElementById('main')});

app.ports.getEditorInfo.subscribe(function() {
  localforage.getItem('editorCache').then(function(rawEditorCache) {
    let editorCache = "There is no cache!";
    if (editorCache !== null) {
      editorCache = base64js.fromByteArray(rawEditorCache);
    }

    localforage.getItem('myName').then(function(rawMyName) {
      let myName = -1
      if (rawMyName !== null) {
         myName = rawMyName
      }
      localforage.getItem('signingKeys').then(function(signingKeys) {
        let myContacts = [];
        if (signingKeys !== null) {
          myContacts = signingKeys.keys();
        }
        const toSend = {myName: myName, editorCache: editorCache, myContacts: myContacts}
        app.ports.retrievedEditorInfo.send(toSend);
      }) 
    })
  });
});

app.ports.cachePort.subscribe(function(editorCache) {
  localforage.setItem('editorCache', base64js.toByteArray(editorCache))
});

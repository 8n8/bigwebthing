var app = Elm.Main.init({'node': document.getElementById('main')});

app.ports.request.subscribe(function(key) {
  localforage.getItem(key).then(function(val) {
    app.ports.retrieved.send(JSON.Stringify({key: val}));
  });
});

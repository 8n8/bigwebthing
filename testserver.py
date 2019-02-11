import bottle

@bottle.get('/')
def getmain():
    return bottle.static_file('index.html', root='.')

bottle.run(host='localhost', port=8000, debug=True)

BigWebThing is a computer system for creating and sharing documents over the internet.

# Encrypted message format

<= 15982 bytes: one of
    117 bytes: KK1
        1 byte: 0
        4 bytes: timestamp
        32 bytes: recipient
        32 bytes: sender
        48 bytes: KK1
    141 bytes: KK2    
        1 byte: 1
        4 bytes: timestamp
        32 bytes: recipient
        32 bytes: sender
        24 bytes: first half of KK1
        48 bytes: KK2
    <=15982 bytes: KK transport
        1 byte: 2
        4 bytes: timestamp
        32 bytes: recipient
        32 bytes: sender
        24 bytes: first half of KK1
        <=15889 bytes: KK transport
            16 bytes: crypto overhead
            <=15873 bytes: encrypted
                15873 bytes: not the final chunk
                    1 byte: 0
                    15872 bytes: the chunk
                <= 15873 bytes: final chunk
                    1 byte: 1
                    <=15872 bytes: the chunk
    69 bytes: add/remove contact
        1 byte: 3/4
        4 bytes: timestamp
        32 bytes: contacter
        32 bytes: contactee
    41 bytes: payment
        1 byte: 5
        4 bytes: timestamp
        4 bytes: amount in pence
        32 bytes: payer
    77 bytes: someone else's transport data
        1 byte: 6
        4 bytes: timestamp
        32 bytes: recipient
        32 bytes: sender
	8 bytes: size
    33 bytes: get public data of
        1 byte: 7
        32 bytes: their ID

# Plain-text message format

One of:
    app
        1 byte: 0
        the app
    data for app
        1 byte: 1
        32 bytes: hash of app
        the data

# Client cache

[]statickeys
    32 bytes: public key
    32 bytes: secret key

[]session
    32 bytes: secret ephemeral key
    32 bytes: their ID
    24 bytes: session ID
    1 byte: 0 for TX, 1 for RX

[]friendly names
    friendly name string
    32 bytes: public key

[]app cache
    32 bytes: hash of the tar archive containing the app
    file name
    file

# Server cache

static keys
    32 bytes: public key
    32 bytes: secret key

A database table for each type of message.

# Client to server

48 bytes: XK1 

64 bytes: XK3

<= 16KB: payload message

# Server to client

64 bytes: XK2

<= 16KB: payload message

# Payload message between server and client

<= 16KB: transport
    2 bytes: size
    16 bytes: crypto overhead
    <= 15982 bytes
        encrypted message

# Websockets messages from backend to frontend

description of layout
    1 byte: 0
    a serialized description of the layout of the app

# Websockets messages from frontend to backend

Events such as button clicks and typing in text boxes.

# App format

Apps are archives containing a bunch of at least one files, one of which must contain a definition of a pure function in a compiled WASM module. This is used like the Elm 'update' function for the app. The other files are movies, images, PDFs etc.

A document is a sequence of parts. The first (required) part is:

    3 bytes: size of WASM
    size bytes: WASM

Subsequent (optional) parts are:

    1 byte: size of name of part
    size bytes: name of part
    5 bytes: size of part
    size bytes: part

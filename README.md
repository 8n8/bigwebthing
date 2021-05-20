BigWebThing is a computer system for creating documents and sharing them over the internet.

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
    165 bytes: KK transport
        1 byte: 2
        4 bytes: timestamp
        32 bytes: recipient
        32 bytes: sender
        24 bytes: first half of KK1
        72 bytes: encrypted
            16 bytes: crypto overhead
            24 bytes: blob ID
            32 bytes: symmetric key for blob
    <= 15982 bytes: blob
        1 byte: 3
        4 bytes: timestamp
        32 bytes: author
        24 bytes: blob ID
        15921 bytes: encrypted
            24 bytes: nonce
            16 bytes: auth tag
            15881 bytes: encrypted
                15881 bytes: not the final chunk
                    1 byte: 0
                    15880 bytes: the chunk
                <= 15881 bytes: final chunk
                    1 byte: 1
                    <=15880 bytes: the chunk
    69 bytes: add/remove contact
        1 byte: 4/5 
        4 bytes: timestamp
        32 bytes: contacter
        32 bytes: contactee
    41 bytes: payment
        1 byte: 6
        4 bytes: timestamp
        4 bytes: amount in pence
        32 bytes: payer
    41 bytes: someone else's blob
        1 byte: 7
        4 bytes: timestamp
        32 bytes: author
        4 bytes: size
    37 bytes: get public data of
        1 byte: 8
        4 bytes: timestamp
        32 bytes: their ID
    25 bytes: get blob
        1 byte: 9
        4 bytes: timestamp
        24 bytes: blob ID
    29 bytes: get blob requests of
        1 byte: 10
        4 bytes: timestamp
        24 bytes: blob ID

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

[]blobkeys
    24 bytes: blob ID
    32 bytes: blob key

[]friendly names
    friendly name string
    32 bytes: public key

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

# API between client backend and frontend

The backend provides URLs for images, movies, and other files as needed, and sends a new UI description whenever it changes.

The frontend sends UI events like clicks to the backend.

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

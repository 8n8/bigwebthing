BigWebThing is a computer system for creating documents and sharing them over the internet.

# Inner message format

<= 15982 bytes: one of
    1 byte: 1 for final message, 0 otherwise
    a sequence of:
        117 bytes: KK1
            1 byte: 0
            4 bytes: timestamp
            32 bytes: recipient
            32 bytes: sender
            48 bytes: KK1
        149 bytes: KK2    
            1 byte: 1
            4 bytes: timestamp
            32 bytes: recipient
            32 bytes: sender
            32 bytes: first part of KK1
            48 bytes: KK2
        181 bytes: KK transport
            1 byte: 2
            4 bytes: timestamp
            32 bytes: recipient
            32 bytes: sender
            32 bytes: first part of KK1
            32 bytes: blob ID
            48 bytes: encrypted
                16 bytes: crypto overhead
                32 bytes: symmetric key for blob
        <= 15981 bytes: blob
            1 byte: 3
            4 bytes: timestamp
            32 bytes: blob ID
            1 byte: 1 for final chunk, 0 otherwise
            4 bytes: blob counter, starting at 0
            2 bytes: size of encrypted, max 15937
            <= 15937 bytes: encrypted
                // the nonce is 24 bytes
                //      1 byte: 1 for final chunk, 0 otherwise
                //      4 bytes: blob counter, starting at 0
                //      19 bytes: zeros
                16 bytes: auth tag
                <= 15921 bytes: chunk
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
        78 bytes: someone else's blob
            1 byte: 7
            4 bytes: timestamp
            32 bytes: author
            32 bytes: blob ID
            1 byte: final chunk
            4 bytes: counter
            4 bytes: size
        69 bytes: get data of
            1 byte: 8
            4 bytes: timestamp
            32 bytes: asker
            32 bytes: their ID
        69 bytes: get blob
            1 byte: 9
            4 bytes: timestamp
	    32 bytes: asker ID
            32 bytes: blob ID
        33 bytes: get blob requests of
            1 byte: 10
            32 bytes: blob ID

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

static keys
    32 bytes: public key
    32 bytes: secret key

[]session
    56 bytes: seed for random number generator
    32 bytes: session ID
    1 byte: 0 for TX, 1 for RX

[]sentblobkeys
    32 bytes: blob ID
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

<=16KB: payload message

# Server to client

64 bytes: XK2

<=16KB: payload message

# Payload message between server and client

<= 16KB: transport
    2 bytes: size
    16 bytes: crypto overhead
    <= 15982 bytes
        encrypted message

# JSON API between client backend and frontend

WASM to compile
    1 byte: 0
    4 bytes: WASM cache ID
    WASM
Input for WASM
    1 byte: 1
    4 bytes: WASM cache ID
    size bytes: input
Output from WASM
    1 byte: 2
    4 bytes: WASM cache ID
    4 bytes: size of input
    size bytes: input
    4 bytes: size of output
    size bytes: output
Bad WASM
    1 byte: 3
    4 bytes: WASM cache ID
New GUI
    1 byte: 4
    4 bytes: box size
    size bytes: UTF8 string
    many of one of
        text
            1 byte: 0
            4 bytes: size
            size bytes: UTF8 string
        image
            1 byte: 1
            2 bytes: image URL size
            size bytes: image URL
            2 bytes: alt size
            size bytes: alt text
        video
            1 byte: 2
            2 bytes: video URL size
            size bytes: video URL
            2 bytes: alt size
            size bytes: alt text
New box
    1 byte: 5
    4 bytes: box size
    size bytes: UTF8 string


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

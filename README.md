BigWebThing is a computer system for creating documents and sharing them over the internet.

# Public message format

These are the messages that flow around on the internet, through the server. The server publishes them all.

<= 15981 bytes: one of
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
        2 bytes: size of encrypted, max 15937
        <= 15941 bytes: encrypted
            // the nonce is random 24 bytes
            // authenticated data is:
            //      1 byte: 1 for final chunk, 0 otherwise
            //      4 bytes: blob counter, starting at 0
            //      UTF-8 "BigWebThing encrypted blob"
            16 bytes: auth tag
            <= 15925 bytes: chunk
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
    33 bytes: get username
        1 byte: 11
        32 bytes: public key
    username
        1 byte: 12
        1 byte: size of username
        size bytes: username
        32 bytes: public key

# Plain-text message format

One of:
    app
        1 byte: 0
        4 bytes: size of WASM
        sequence of zero or more files
            1 byte: size of name of file
            size bytes: UTF-8 name of file
            5 bytes: size of file
            size bytes: file
    data for app
        1 byte: 1
        32 bytes: hash of app
        the data

# Client cache

static keys
    32 bytes: public key
    32 bytes: secret key

[]session
    32 bytes: private ephemeral key
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

A database table for each type of message that is worth keeping.

# Client to server

48 bytes: XX1 

64 bytes: XX3

<=16KB: payload message

# Server to client

148 bytes: XK2 and certificate
    80 bytes: crypto overhead
    4 bytes: expiry timestamp
    64 bytes: signature of
        4 bytes: expiry timestamp
        32 bytes: server public static Noise key
        16 bytes: additional data
            2f 98 99 43 df 8b 74 e2 86 6b 62 19 40 03 4c cc 

<=16KB: payload message

# Payload message between client and server and between server and client

<= 16KB: transport
    2 bytes: size
    16 bytes: crypto overhead
    <= 15982 bytes: encrypted
        1 byte: 1 for final chunk, 0 otherwise
        <= 15981 bytes: sequence of public messages

# BigWebThing

BigWebThing is a computer system for sharing documents over the internet.

There are two programs:

1. runs on the user's computer and stores their encryption keys
2. runs on the server and stores the encrypted documents

When the client program is run, it downloads, assembles and decrypts all the messages that have been sent to it.

## Client cache

statickeys
    32 bytes: public
    32 bytes: secret
[]session
    24 bytes: session ID
    80 bytes: session state
[]send
    32 bytes: recipient
    32 bytes: file hash
    8 bytes: timestamp
[]contact
    publickey

## Client view

files/
    named by session ID

[]metadata
    24 bytes: session ID
    8 bytes: timestamp
    32 bytes: other party ID
    1 byte: from or to

## Client interaction

### Input file

This is a plain-text file containing the client cache.

### Output file

Contains all the client view, except the actual file contents, which are paths.

## Session ID

The first 24 bytes of the the session KK1.

## User ID

The 32-byte Noise public key.

## Transport encoding

92 bytes
    36 bytes: crypto header
    encrypted
        24 bytes: blob ID
        32 bytes: symmetric key of blob

## Server cache

A file containing the secret static keys.

A directory containing blobs named by blob ID.

An SQLITE database containing
    kk1s
        sessionid
        kk1 (second half of KK1, because session ID is first half)
        sender
        recipient
    kk2s
        sessionid
        kk2
        sender
        recipient
    transports
        sessionid
        transport
        sender
        recipient
        uploadtime
    blobuploads
        blobid
        senderid
    payments
        payer
        amount
        timestamp

## Server API

### To server

one of
    48 bytes: XX1
    64 bytes: XX3
    <= 16KB: transport message

### From server

one of
    96 bytes: XX2
    <= 16KB: transport message

## Client / server transport message encoding

<= 16KB
    2 bytes: size
    36 bytes: crypto overhead
    encrypted: one of
        81 bytes: KK1
            1 byte: 0
            32 bytes: sender
            48 bytes: KK1
        105 bytes: KK2
            1 byte: 1
            32 bytes: sender
            24 bytes: session ID
            48 bytes: KK2
        149 bytes: transport
            1 byte: 2
            32 bytes: sender
            24 bytes: session ID
            92 bytes: transport
        <= 15962 bytes: blob
            1 byte: 3
            24 bytes: blob ID
            <= 15937 bytes: blob

## Blob encoding

<= 15937 bytes
    2 bytes: size
    36 bytes: crypto overhead
    <=15899 bytes: encrypted: one of
        not the final chunk in sequence
            1 byte: 0
            24 bytes: ID of next chunk
            15874 bytes: the chunk
        final chunk
            1 byte: 1
            24 bytes: checksum of whole file
            <= 15874 bytes: the chunk

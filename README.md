# BigWebThing

BigWebThing is a computer system for sending files over the internet.

There is a program that runs on the user's computer, and one that runs on the server.

## Client state

static keys
    32 bytes: secret key
    32 bytes: public key

[]sessions
    24 bytes: session ID
    80 bytes: session state

[]sends
    32 bytes: recipient
    32 bytes: file hash
    8 bytes: timestamp

[]contacts
    32 bytes: public key

## Server state

static keys
    32 bytes: secret key
    32 bytes: public key

[]kk1s
    32 bytes: recipient
    32 bytes: sender
    24 bytes: first part of KK1 used as a session reference
    24 bytes: second part of KK1

[]kk2s
    32 bytes: recipient
    32 bytes: sender
    24 bytes: session ID
    48 bytes: KK2

[]transports
    32 bytes: recipient
    32 bytes: sender
    24 bytes: session ID
    92 bytes: transport
    8 bytes: timestamp

[]payments
    4 bytes: amount in pence
    32 bytes: payer
    8 bytes: timestamp

[]blobs
    24 bytes: session ID
    <= 15937 bytes: blob

## Blob encoding

<= 15937 bytes
    36 bytes: crypto overhead
    one of
        not the last chunk
            1 byte: 0
            24 bytes: ID of next chunk
            15876 bytes: the chunk
        the last chunk
            1 byte: 1
            24 bytes: checksum of whole file
            <= 15876 bytes: the chunk

## Transport encoding

92 bytes
    36 bytes: crypto header
    24 bytes: blob ID
    32 bytes: secret key

## Server / client inside crypto

### Client to server

one of
    81 bytes: KK1
        1 byte: 0
        32 bytes: recipient
        48 bytes: KK1
    105 bytes: KK2
        1 byte: 1
        32 bytes: recipient
        24 bytes: session ID
        48 bytes: KK2
    149 bytes: transport
        1 byte: 2
        32 bytes: recipient
        24 bytes: session ID
        92 bytes: transport
    37 bytes: payment
        1 byte: 3
        4 bytes: amount in pence
        32 bytes: payer
    15962 bytes: blob upload
        1 byte: 4
        24 bytes: blob ID
        <= 15937 bytes: blob
    25 bytes: blob request
        1 byte: 5
        24 bytes: blob ID

### Server to client

one of
    81 bytes: KK1
        1 byte: 0
        32 bytes: sender
        48 bytes: KK1
    105 bytes: KK2
        1 byte: 1
        32 bytes: sender
        24 bytes: session ID
        48 bytes: KK2
    157 bytes: transport
        1 byte: 2
        32 bytes: sender
        24 bytes: session ID
        92 bytes: transport
        8 bytes: timestamp
    13 bytes: payment
        1 byte: 3
        4 bytes: amount in pence
        8 bytes: timestamp
    <= 15962 bytes: blob
        1 byte: 4
        24 bytes: blob ID
        <= 15937 bytes: blob

## Server / client encrypted

### Client to server

48 bytes: XX1 

64 bytes: XX3

<= 16KB: transport

### Server to client

96 bytes: XX2

<= 16KB: transport

### Transport

<= 16KB: transport
    2 bytes: size
    36 bytes: crypto overhead
    <= 15962 bytes: encrypted message

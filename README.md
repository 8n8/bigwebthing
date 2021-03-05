BigWebThing is a computer system for sharing documents over the internet.

# Server cache

static keys
    32 bytes: secret key
    32 bytes: public key

[]kk1
    32 bytes: sender ID
    32 bytes: recipient ID
    24 bytes: first half of KK1, used as session ID
    24 bytes: second half of KK1

[]kk2
    32 bytes: sender ID
    32 bytes: recipient ID
    24 bytes: session ID
    48 bytes: KK2

[]kktransport
    32 bytes: sender ID
    32 bytes: recipient ID
    24 bytes: session ID
    72 bytes: KK transport
    8 bytes: transport

[]payment
    4 bytes: payment amount in pence
    8 bytes: timestamp
    32 bytes: payee

# Client cache

static keys
    32 bytes: secret key
    32 bytes: public key

[]session state
    24 bytes: session ID
    pickled session state

[]contact
    32 bytes: public key
    friendly name

# KK transport encoding

72 bytes
    16 bytes: crypto overhead
    24 bytes: blob ID
    32 bytes: encryption key of blob

# Encrypted client / server API

## Client to server

48 bytes: XK1

48 bytes: XK3

<= 16KB encrypted transport
    2 bytes: size
    16 bytes: crypto overhead
    encrypted: one of
        81 bytes: KK1
            1 byte: 0
            32 bytes: recipient
            48 bytes: KK1
        105 bytes: KK2
            1 byte: 1
            32 bytes: recipient
            24 bytes: session ID
            48 bytes: KK2
        129 bytes: KK transport
            1 byte: 2
            32 bytes: recipient
            24 bytes: session ID
            72 bytes: KK transport
        <= 15982 bytes: blob upload
            1 byte: 3
            24 bytes: blob ID
            <=15957 bytes: blob

## Server to client

48 bytes: XK2

<= 16KB encrypted transport
    1 bytes: size
    16 bytes: crypto overhead
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
        129 bytes: KK transport
            1 byte: 2
            32 bytes: sender
            24 bytes: session ID
            72 bytes: KK transport
        <= 15982 bytes: blob
            1 byte: 3
            24 bytes: blob ID
            <=15957 bytes: blob
        payment
            1 byte: 4
            4 bytes: payment amount in pence
            8 bytes: timestamp

## Blob encoding

<=15957 bytes
    40 bytes: crypto overhead
    encrypted: one of
        not the last chunk
            1 byte: 0
            24 bytes: blob ID of next chunk
            15892 bytes: chunk
        the last chunk
            1 byte: 1
            24 bytes: checksum of whole file
            <= 15892 bytes: chunk

## KK transport encoding

72 bytes
    16 bytes: crypto overhead
    24 bytes: blob ID
    32 bytes: symmetric encryption key

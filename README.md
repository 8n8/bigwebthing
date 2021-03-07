BigWebThing is a computer system for creating and sharing documents over the internet.

# Client cache

static keys
    32 bytes: public key
    32 bytes: secret key

[]contact
    32 bytes: public key
    Friendly name

[]session
    32 bytes: seed
    24 bytes: session ID

[]sent
    24 bytes: session ID
    file

# Server cache

static keys
    32 bytes: public key
    32 bytes: secret key

payment auth key
    32 bytes: public key of user allowed to upload payments

[]kk1
    24 bytes: first half of KK1 (session ID)
    24 bytes: second half of KK1
    32 bytes: sender
    32 bytes: recipient

[]kk2
    24 bytes: session ID
    48 bytes: KK2
    32 bytes: sender
    32 bytes: recipient

[]kktransport
    24 bytes: session ID
    72 bytes: KK transport
    32 bytes: sender
    32 bytes: recipient
    8 bytes: upload timestamp

[]payment
    4 bytes: amount in pence
    8 bytes: timestamp
    32 bytes: payer

[]blobupload
    8 bytes: timestamp
    24 bytes: blob ID
    32 bytes: uploader

[]blob
    24 bytes: blob ID
    <= 15909 bytes: the blob

# Client to server

48 bytes: XK1 

<= 16KB XK3
    2 bytes: size
    48 bytes: encrypted static key
    16 bytes: payload crypto overhead
    <= 15934 bytes: encrypted payload
        client to server payload

<= 15952 bytes: transport
    2 bytes: size
    16 bytes: crypto overhead
    <= 15934 bytes
        client to server payload
        
# Client to server payload

15934 bytes
    81 bytes: KK1
        1 byte: 0
        32 bytes: recipient
        48 bytes: KK1
    105 bytes: KK2
        1 byte: 1
        32 bytes: recipient
        48 bytes: KK2
        24 bytes: session ID
    129 bytes: KK transport
        1 byte: 2
        32 bytes: recipient
        72 bytes: KK transport
        24 bytes: session ID
    <= 15934 bytes: blob
        1 byte: 3
        24 bytes: blob ID
        <= 15909 bytes: the blob
    37 bytes: payment
        1 byte: 4
        4 bytes: amount in pence
        32 bytes: payer

# Server to client

64 bytes: XK2

<= 15942 bytes: transport
    2 bytes: size
    16 bytes: crypto overhead
    <= 15934 bytes: one of
        81 bytes: KK1
            1 byte: 0
            32 bytes: sender
            48 bytes: KK1
        105 bytes: KK2
            1 byte: 1
            32 bytes: sender
            48 bytes: KK2
            24 bytes: session ID
        129 bytes: KK transport
            1 byte: 2
            32 bytes: sender
            72 bytes: KK transport
            24 bytes: session ID
        <= 15934 bytes: blob
            1 byte: 3
            24 bytes: blob ID
            <= 15909 bytes: the blob
        13 bytes: payment
            1 byte: 4
            4 bytes: amount in pence
            8 bytes: timestamp
        33 bytes: blob upload
            1 byte: 5
            24 bytes: blob ID
            8 bytes: timestamp

# Blob encoding

<= 15909 bytes
    24 bytes: random nonce
    16 bytes: authentication tag
    encrypted: one of
        15869 bytes: not the final chunk
            1 byte: 0
            24 bytes: ID of next blob in sequence
            15844 bytes: the chunk
        <= 15869 bytes: final chunk
            1 byte: 1
            24 bytes: checksum of whole file
            <= 15844 byts: the chunk

# KK transport encoding

72 bytes
    16 bytes: crypto overhead
    encrypted
        24 bytes: blob ID
        32 bytes: secret key for blob

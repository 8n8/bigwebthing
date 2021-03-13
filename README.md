BigWebThing is a computer system for creating and sharing documents over the internet.

# Client cache

statickeys
    32 bytes: public key
    32 bytes: secret key

[]contact
    32 bytes: public key

[]session
    32 bytes: seed
    32 bytes: their ID
    24 bytes: session ID
    1 byte: 0 for TX, 1 for RX

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
    4 bytes: upload timestamp

[]payment
    4 bytes: amount in pence
    4 bytes: timestamp
    32 bytes: payer

[]blobupload
    4 bytes: timestamp
    24 bytes: blob ID
    32 bytes: uploader

[]blob
    24 bytes: blob ID
    <= 15909 bytes: the blob

# Client to server

48 bytes: XK1 

64 bytes: XK3

<= 16KB: transport
    2 bytes: size
    16 bytes: crypto overhead
    <= 15982 bytes
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
    	<= 15982 bytes: blob
    	    1 byte: 3
    	    24 bytes: blob ID
    	    <= 15957 bytes: the blob
    	37 bytes: payment
    	    1 byte: 4
    	    4 bytes: amount in pence
    	    32 bytes: payer
        25 bytes: request blob
            1 byte: 5
            24 bytes: blob ID

# Server to client

64 bytes: XK2

<= 16KB: transport
    2 bytes: size
    16 bytes: crypto overhead
    <= 15982 bytes: one of
        81 bytes: KK1
            1 byte: 0
            32 bytes: sender
            48 bytes: KK1
        73 bytes: KK2
            1 byte: 1
            48 bytes: KK2
            24 bytes: session ID
        101 bytes: KK transport
            1 byte: 2
            72 bytes: KK transport
            24 bytes: session ID
            4 bytes: timestamp
        <= 15982 bytes: blob
            1 byte: 3
            24 bytes: blob ID
            <= 15957 bytes: the blob
        9 bytes: payment
            1 byte: 4
            4 bytes: amount in pence
            4 bytes: timestamp
        29 bytes: blob upload
            1 byte: 5
            24 bytes: blob ID
            4 bytes: timestamp

# Blob encoding

<= 15957 bytes
    24 bytes: random nonce
    16 bytes: authentication tag
    encrypted: one of
        15917 bytes: not the final chunk
            1 byte: 0
            24 bytes: ID of next blob in sequence
            15892 bytes: the chunk
        <= 15917 bytes: final chunk
            1 byte: 1
            24 bytes: checksum of whole file
            <= 15892 bytes: the chunk

# KK transport encoding

72 bytes
    16 bytes: crypto overhead
    encrypted
        24 bytes: blob ID
        32 bytes: secret key for blob

# Websockets messages from backend to frontend

contact
    1 byte: 0
    32 bytes: public key
    1 byte: friendly name length
    friendly name
93 bytes: file metadata
    1 byte: 1
    32 bytes: sender
    24 bytes: blob ID
    32 bytes: blob secret key
    4 bytes: timestamp
61 bytes: sent metadata
    1 byte: 3
    32 bytes: recipient
    24 bytes: blob ID
    32 bytes: blob secret key
    4 bytes: timestamp
29 bytes: blob upload
    1 byte: 4
    24 bytes: blob ID
    4 bytes: timestamp
9 bytes: payment
    1 byte: 5
    4 bytes: amount in pence
    4 bytes: timestamp
document
    1 byte: 6
    24 bytes: blob ID
    document

# HTTP API for accessing local key-value store

/open/<hex-encoded 16-byte id>
    Response is blob

/save/<hex-encoded 16-byte id>
    Body is blob

# Websockets messages from frontend to backend

add contact
    1 byte: 0
    32 bytes: public key
delete contact
    1 byte: 1
    32 bytes: public key
send document
    1 byte: 2
    32 bytes: recipient
    document
retrieve document
    1 byte: 3
    24 bytes: blob ID
    32 bytes: blob secret key

# Document format

When documents are being exchanged between the frontend and backend, the binaries are replaced with a unique 16-byte ID, and the binaries are put in the key-value store. This is because it is a web frontend, and it is easier to show videos, photos and other binaries using HTTP.

A document is an alternating sequence of elements:

plain text
    1 byte: 0
    4 bytes: size
    UTF8 string
binary
    1 byte: 1
    6 bytes: size
    binary blob

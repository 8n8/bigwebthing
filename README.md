# Overview
 
It is intended to provide a combined and improved solution to the problem of sharing messages and programs conveniently and securely.

User data is kept on their own machine, and never sent out unencrypted. Users' private crypto keys are never shared with other people.

Users can write programs and send them to each other safely and run them safely.

There is a message-passing server for sharing data between people. It stores messages till they are collected.

The cost of running the server is met by users paying for the resources they use, as they use them.

It operates on a strict whitelist-only policy, so you only receive messages from peopleon your whitelist.

# Program structure

There are six different parts to the program:

1. (Haskell) The client backend. This runs on the client's computer and does most of the work, such as caching, crypto, and communicating with the server and the GUI.

2. (Elm) The GUI. This runs in a stripped-down web browser via webview.

3. (Javascript) The GUI plumbing. This handles the connection between the client backend and the Elm GUI, passing data through ports to Elm and websockets to the client backend.

4. (Rust / Webassembly) The user programs. These are programs that users can write and send to each other. They are pure functions written in Rust and compiled to Webassembly, and produces a DOM-like structure from a user input text box and user-uploaded binary blobs.

5. (Go) The client / server crypto. Haskell doesn't have a simple, trustworthy high-level crypto library, so the crypto is done in Go using the nacl library. This runs on the client and server localhost as an HTTP server, and is used by the client backend and by the server.

6. (Haskell) The server. This acts as a route between clients. They upload and download messages, and also use it to store their public keys.

# User ID

A user ID is a public key fingerprint.

The fingerprint is the first few bits of a slow hash of a concatentation of a user's public signing and encryption keys.

The user fingerprint length is calculated as follows:

It must be long enough that it takes too long for an adversary to generate keys that
have the same user ID as someone else. It should be as short as possible so that it
is convenient for people to read and share.

The only thing the adversary can do is keep generating keys and testing if the ID matches one on the system.

Lets say that 2^70 'fast' operations is too much for the attacker. Then if I make them use a slow hash in each operation, that is around 2^12 'fast' operations. So 2^58 'slow' operations is too much for the attacker.

Say there are a maximum 2^40 different users, then that means the user ID should be 58 + 40 = 98 bits. This will fit in 13 bytes.

The encoding uses a 7776 word list from the EFF, very similar to Diceware. So a 2^98 bit fingerprint will need 8 words, like:

fried veal frightful untoasted uplifting carnation breezy hazy

or

rethink doornail refining handiness lend strainer appealing deputy

Which is pretty long, but it's the best I can do.

# Client backend API

The client backend provides a server on port 11833. It provides a websockets server, an HTTP API, and a static file server on /static.

## Websockets API

The websockets API is for sending messages from the client backend to the frontend.

Messages can take the following form:

1. New inbox message summary

    + 0x01
    + subject as a sized string
    + 8 bytes: POSIX time
    + cache key for full message as a sized string

2. New acknowledgement

    + 0x02
    + 13 bytes: recipient ID
    + cache key for full message as a sized string

3. Bad network connection

    + 0x03

4. Good network connection

    + 0x04

5. My ID

    + 0x05
    + 13 bytes: my ID

## HTTP API

1. /cache/get/:cacheKey

    Response:
    + value associated with the key

2. /cache/set/:cacheKey

    Request:
    + value

3. /cache/delete/:cacheKey

4. /sendmessage/:draftId

    Request:
    + 13 bytes: recipient ID

5. /whitelist/add

    Request:
    + 13 bytes: ID of whitelistee

6. /whitelist/remove

    Request:
    + 13 bytes: ID of whitelistee


# Crypto server API

There is an HTTP server on port 59285 that does all the crypto. The API is like this:

1. /encrypt

    Request:
    + 32 bytes: public key of recipient
    + the message to encrypt
    
    Response:
    + encrypted message

2. /decrypt

    Request:
    + 32 bytes: public key of sender
    + the message to decrypt

    Response:
    + decrypted message

    (Status 400 if decryption fails.)

3. /sign

    Request:
    + message to sign

    Response:
    + signed message

4. /checksignature

    Request:
    + public key of sender
    + signed message

    Response:
    + status 200 if good signature or status 400 if bad signature

5. /getmykeys

    Response:
    + 32 bytes: public signing key
    + 32 bytes: public encryption key

6. /userid

    Request:
    + 32 bytes: public signing key
    + 32 bytes: public encryption key

    Response:
    + 13 bytes: user ID

7. /proofofwork

    Request:
    + 1 byte: difficulty
    + 16 bytes: random

    Response:
    + 24 bytes: proof of work


# Server API

The server provides an HTTP API on port 8001, and also a TCP connection on 8002 so that the server can send new messages to the client without them being requested first.

The server will not accept requests greater than 16KB.

## Proof of work

Some APIs are protected by a proof of work problem. To create a proof of work token, the user must download some unique bytes and a difficulty value from the server, and find some more unique bytes that will create an Argon2id hash with all the bytes greater than the difficulty.  So a proof of work token is like this:

+ <16 unique bytes provided by the server>
+ <8 calculated by the client>

The server checks that the first part is indeed something that it recently gave out, then that the hash of the whole meets the current difficulty.

## TCP API

Port 8080

It will accept incoming TCP connections. The client's first message
should be like this:

+ 13 bytes: my ID
+ signed
    + 16 bytes: 6b 87 4c de cc f0 28 b3 7c 4e de ee 15 ca 92 93
    + 16 bytes: auth code

Then the client should just listen on the connection. The server will
post any messages that it receives or has received from other users
down this connection. They will be prefixed with a four-byte
Little-Endian length.

## HTTP API

Port 80

1. /publishkeys

    Request:
    + 24 bytes: proof of work
    + 13 bytes: user ID
    + 32 bytes: public signing key
    + 32 bytes: public encryption key

2. /getkeys

    Request:
    + 13 bytes: the ID to look up

    Response:
    + 32 bytes: their public signing key
    + 32 bytes: their public encryption key


3. /proofofworkinfo

    Response:
    + 1 byte: difficulty, which is the number that each byte in the
              proof of work must be greater than
    + 16 bytes: random

4. /authcode

    Response:
    + 16 bytes: random

5. /message/send

    Request:
    + 13 bytes: my ID
    + signed
        + 16 bytes: 0a cb 78 89 67 cf 64 19 2a dd 32 63 61 2d 10 18
        + 16 bytes: auth code
        + 13 bytes: recipient ID
        + message

6. /message/delete

    Request:
    + 13 bytes: my ID
    + signed
        + 16 bytes: d3 ad fa b3 b4 67 41 bb 51 19 de d5 56 e5 9c 8e
        + 16 bytes: auth code
        + 32 bytes: SHA256 hash of the message to delete

7. /whitelist/add

    Request:
    + 24 bytes: proof of work
    + 13 bytes: my ID
    + signed
        + 16 bytes: df c2 fb 02 ba 19 fd 38 80 fc 93 ca d6 f6 37 33
        + 16 bytes: auth code
        + 13 bytes: ID of person to whitelist

8. /whitelist/remove

    Request:
    + 13 bytes: my ID
    + signed
        + 16 bytes: 32 52 4c a3 77 a2 86 0e 8b ec df e4 23 ae f1 8f
        + 16 bytes: auth code
        + 13 bytes: ID of person to remove from whitelist

# Client to client API

Since the server only accepts messages of 16KB or less, messages are chunked up into chunks 15.5KB or less, allowing space for crypto overhead.

All messages are encrypted.

Inside the encryption, the API is as follows:

0. small message

    + 0x00
    + the message

1. part of a large message

    + 0x01
    + 32 bytes: SHA-256 hash of the whole message
    + 4 bytes: counter (starting from 0) (Little-Endian)
    + the message chunk

2. small blob

    + 0x02
    + the blob

3. part of a large blob

    + 0x03
    + 32 bytes: SHA-256 hash of the whole blob
    + 4 bytes: counter (starting from 0) (Little-Endian)
    + the blob chunk

4. acknowledgement

    + 0x02
    + signed
        + 16 bytes: 77 0a 8b e7 5a 1a 9e 31 c5 97 5b 61 ec 47 16 ef
        + 8 bytes: Unix time received
        + 32 bytes: SHA-256 hash of message received

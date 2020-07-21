# Overview
 
It is intended to provide a combined and improved solution to the problem of sharing messages and programs conveniently and securely.

User data is kept on their own machine. Users' private crypto keys are never shared with other people.

Users can write programs and send them to each other safely and run them safely.

There is a message-passing server for sharing data between people. It stores messages till they are collected.

The cost of running the server is met by users paying for the resources they use.

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

The only thing the adversary can do is keep generating keys for a particular user and testing if the ID matches one on the system.

Lets say that 2^70 'fast' operations is too much for the attacker. Then if I make them use a slow hash in each operation, that is around 2^12 'fast' operations. So 2^58 'slow' operations is too much for the attacker.

Say there are 2^40 different users, then that means the fingerprint should be 2^(58 + 40) = 2^98, say 13 bytes.

The encoding uses a 7776 word list from the EFF, very similar to Diceware. So a 2^98 bit fingerprint will need 8 words, like:

fried veal frightful untoasted uplifting carnation breezy hazy

or

rethink doornail refining handiness lend strainer appealing deputy

Which is pretty long, but I suppose I'll have to live with it.

### Client backend API




### Crypto server API

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

    Request:
    + empty

    Response:
    + 32 bytes: public signing key
    + 32 bytes: public encryption key

### Proof of work

Free APIs are protected by a proof of work problem. To create a proof of work token, the user must download some unique bytes from the server, and find some more unique bytes that will create an Argon2id hash with all the bytes greater than the difficulty.  So a proof of work token is like this:

+ <16 unique bytes provided by the server>
+ <8 calculated by the client>

The server checks that the first part is indeed something that it recently gave out, then that the hash of the whole meets the current difficulty.

### Server API

Messages are sent to the server in HTTP requests. The server will not accept messages greater than 16KB.

The client can also make a websocket connection with the server, which the server will send any new messages through.

Some APIs are only accessible to certain users and must have an identity token as follows:
+ 10 bytes: ID of sender
+ 16 bytes: authentication code downloaded from the server earlier
+ 96 bytes: signature. This is the signed SHA256 hash of the message prepended to the authentication code above. To be clear it is signature(SHA256(route byte + message + authCode)).
So an identity token is 122 bytes long.

These are the types of messages that the server will accept:

1. (Free) Retrieve keys for ID
+ 0x01
+ 10 bytes: the ID to look up
The response is:
+ 32 bytes: their public signing key
+ 32 bytes: their public encryption key
+ 10 bytes: the ID of the owner of the keys

2. (Free) Get proof of work difficulty and key
+ 0x02
The response is:
+ 1 byte: how many of the bytes at the start of the proof of work must be zeros (the difficulty)
+ 16 bytes: unique, i.e. the server must never respond in the same way to this request

3. (Free) Get code for authentication
+ 0x03
The response is a unique 16 bytes, that is, the server must never respond in the same way to this request.

4. (Paid, Signed) Send message
+ 0x04
+ 122 bytes: identity token
+ 10 bytes: recipient id
+ message

5. (Free) Delete message from server
+ 0x05
+ 122 bytes: identity token
+ 32 bytes: SHA256 hash of the message

6. (Free) Whitelist someone
+ 0x06
+ 122 bytes: identity token
+ 24 bytes: proof of work
+ 10 bytes: name of person to whitelist

7. (Free) Remove someone from whitelist
+ 0x07
+ 122 bytes: identity token
+ 10 bytes: name of person to remove from whitelist

### Client API

Since the server only accepts messages of 16KB or less, messages are chunked up into chunks 15.5KB or less, allowing space for crypto overhead.

Each chunk is like this:
+ 1 byte: 0 if it is not chunked (i.e. is small enough to fit in one chunk) or
          1 if it is
+ either
    the whole of a small message

  or
    + 32 bytes: SHA-256 hash of the whole message
    + 4 bytes: counter (first chunk is numbered 0) (Little-Endian)
    + the chunk

Then the chunk gets encrypted as follows:
+ 24-bytes: nonce
+ encrypted bytes calculated with nacl box.Seal

Then it gets sent using the Send message route in the server API.

# Programs

A program can:

1. Display a document.

2. Read user input.

# Spam

Each user has a whitelist of people they will accept messages from. Messages from anyone else are rejected unless they have a valid one-time code. Connections are started by somone sending a one-time code to someone else, by some existing method of communication, such as email. This code is used to authenticate the first message.

# Software components

1. Message-passing server. Messages are accepted if they are to or from subscribers. It deletes messages when they have been read.

2. Javascript client. It has an inbox categorized by program, and a set of programs. The main view is a list of programs and a box to search for them. Clicking on a program launches it. There is a built-in programming language interpreter for running the programs - probably an editor and tooling all built in too.

# Security

Each user has a pair of keys for encryption and signing which are generated from a password known only by the user. This means that data is only accessible in unencrypted form by the user, but has the dowside that if the user loses their password and their local data then they can't recover it.

Key pairs are changed by sending the new keys to everyone on the whitelist, signed by the old signing key.

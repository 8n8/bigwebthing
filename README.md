# Overview

BigWebThing is intended to provide a combined and improved solution to the problem of sharing messages and programs conveniently and securely.

User data is kept on their own machine, and never sent out unencrypted. Users' private crypto keys never leave their own machine.

Users can write programs and send them to each other safely and run them safely.

There is a message-passing server for sharing data between people.

The cost of running the server is met by users paying for the resources they use, as they use them.

It operates on a strict whitelist-only policy, so you only receive messages from people on your whitelist.

# Program structure

These are the different parts of the program:

1. (Haskell) The client backend. This runs on the client's computer and does most of the work, such as caching, crypto, and communicating with the server and the GUI.

2. (Elm) The GUI. This runs in a stripped-down web browser via webview.

3. (Javascript) The GUI plumbing. This handles the connection between the client backend and the Elm GUI, passing data through ports to Elm and websockets to the client backend.

4. (Rust / Webassembly) The user programs. These are programs that users can write and send to each other. They are pure functions written in Rust and compiled to Webassembly, and produce a DOM-like structure from a user input text box and user-uploaded binary blobs.

6. (Haskell) The server. This acts as a route between clients. They upload and download messages, and also use it to store their public keys.

# User ID

A user ID has two components:

1. The username generated by the server. This is a uint64, which is a unique ID in communications with the server. Since most usernames will not use most of the bits, it is displayed to the user as a variable-length integer at the end of their public-key fingerprint. The public-key fingerprint is a fixed length so it is clear which is which.

2. A fingerprint of the static public crypto key. This needs to be long enough so that it is infeasible to mount a man-in-the-middle attack. So if there is an adversary in control of the communication channel, in order to mount a man-in-the-middle, they need to find a pair of keys that matches the fingerprint for each party. If I use a very slow hash to generate the fingerprint, then a uint64 should be sufficient as a fingerprint.

So the whole user ID is:

	very_slow_hash(public_key || username, salt)[:8] || username

The salt is stored alongside the public key in the database on the server.

The encoding uses a custom word list with about 8000 words, very similar to Diceware. So for someone with a 3-byte username, their user ID will be 11 bytes, which will require 7 words, like:

basin glue tree unusable chug crushing hardwired

or

feminist polish fanfare front barber resume palpable

# Client backend API

The client backend provides a server on port 11833. It provides a static file server on /static, a websockets server, and an HTTP API. Websockets are useful for the client backend to push new messages to the front-end when they happen. HTTP is good because I can dump very large file uploads into the request bodies and stream them over to the backend. Elm does not have a native websockets capability, so it has to be done via a port. Everything that goes through an Elm port has to be a string in memory, so binary data has to be chunked and encoded to a string.

## Websockets API

Backend to frontend:

	Bad network connection
		1 byte: 0
	Good network connection
		1 byte: 1
	Send failed
		1 byte: 2
		4 bytes: message ID
	Progress of sending
		1 byte: 3
        4 bytes: message ID
		4 bytes: total bytes in message
		4 bytes: bytes sent
    Message:
        1 byte: 4
        4 bytes: message ID
        sized string: commit hash
        sized string: subject
        sized string: main box
        string: metadata
    Whitelist:
        1 byte: 5
		sequence of sized user IDs
    My ID
        1 byte: 6
        my ID
    Drafts summary
        1 byte: 7
        drafts summary
    Sent summary
        1 byte: 8
        sent summary
    Inbox summary
        1 byte: 9
        inbox summary
    Backend ready
        1 byte: 10
    Merge candidates
        1 byte: 11
        4 bytes: message ID
        list of 4-byte message IDs
    Message history
        1 byte: 12
        4 bytes: message ID
        string: output of git log
    Unique
        1 byte: 13
        4 bytes: unique

Frontend to backend:
    Set message:
        1 byte: 0
        4 bytes: message ID
        sized string: subject
        sized string: main box
        sized string: metadata
	Send message
		1 byte: 1
        4 bytes: message ID
        20 bytes: commit hash
        user ID of recipient
	Add to whitelist
		1 byte: 2
		user ID
	Remove from whitelist
		1 byte: 3
		user ID
	Get message
		1 byte: 4
        4 bytes: message ID
	Get whitelist
		1 byte: 5
	Get my ID
		1 byte: 6
	Get drafts summary
		1 byte: 7
	Get sent summary
		1 byte: 8
	Get inbox summary
		1 byte: 9
    Get merge candidates
        1 byte: 10
        4 bytes: message ID
    Merge
        1 byte: 11
        4 bytes: message ID to merge into
        4 bytes: message ID to merge from
    Get history
        1 byte: 12
        4 bytes: message ID
    Revert
        1 byte: 13
        4 bytes: message ID
        string: commit hash to revert to
    Get commit
        1 byte: 14
        4 bytes: message ID
        string: commit hash to look at
    Get unique
        1 byte: 15

## HTTP API

/setblob
	Request
		binary blob
	Response
		32 bytes: hash of blob
        4 bytes: size of blob
        sized string: file name of blob
        string: mime of blob
/getblob
	Request
		1 byte: 4
        4 bytes: message ID
		32 bytes: hash of blob
	Response
		blob

# Server API

The server provides a TCP server on port 11453.

## Proof of work

Some APIs are protected by a proof of work problem. To create a proof of work token, the user must download some unique bytes and a difficulty value from the server, and find some more unique bytes that will create a 32-byte slow hash with all the bytes greater than the difficulty. The 'slow' hash should not be that slow. It should be slow enough that any implementation details apart from hashing are amortised. So a proof of work token is like this:

+ 16 bytes: unique from the server
+ 8 bytes: calculated by the client

The server checks that the first part is indeed something that it recently gave out, then that the hash of the whole meets the current difficulty.

## Username encoding

A username is a variable-length integer, at least one byte long. It is encoded by prefixing it by a byte containing its length.

## TCP API

It will accept incoming TCP connections. A connection begins unauthenticated, and can change to authenticated. Messages that are only sent/acceptable on an authenticated channel are marked "AUTH".

Each message should be not more than 16KB, and should start with a 2-byte Little-Endian length.

Server to client

	New message from another user (AUTH)
		1 byte: 0
        8 bytes: sender username
        15991 bytes: the message
    New username (AUTH)
        1 byte: 1
        username
    Proof of work info
        1 byte: 2
		1 byte: difficulty
		16 bytes: random
    Price (AUTH)
        1 byte: 3
        4 bytes: price in GBP^(-4)

Client to server

    Get proof of work info
        1 byte: 0
    Sign in
        1 byte: 1
	    16 bytes: secret session key
	    sized username
    New account
		1 byte: 2
		24 bytes: proof of work
		16 bytes: random session key
	Send message (AUTH)
		1 byte: 3
		8 bytes: recipient username
		15991 bytes: message
	Delete message (AUTH)
		1 byte: 4
		32 bytes: message hash
	Get price (AUTH)
		1 byte: 5
    Add contact (AUTH)
        1 byte: 6
        8 bytes: contact username
    Remove contact (AUTH)
        1 byte: 7
        8 bytes: contact username

# Client to client

## API

A message is a tarred Git repository.

Then the message is sliced up into 15KB chunks, each chunk is encrypted, and is sent.

Before encryption, a chunk must be exactly 15KB long. A chunk is encoded like this:

    either:
        1 byte: 0 // There isn't another chunk: this is the last one in the sequence
	    32 bytes: the hash of the whole message before chunking
        2 bytes: length of padding
        the padding
    or
        1 byte: 1: // There are more chunks to come.
	4 bytes: a counter, starting at 0
	4 bytes: the total number of chunks in the whole message
	all the rest of the bytes: the chunk

## Crypto

The end-to-end cryptography is done using Noise, using the XX pattern, i.e. the parties exchange their public keys during the handshake.

For each session I need to check that the sender static public key matches the fingerprint they gave me.

All the chunks in a message are sent using the same session. The session should be thrown thrown away after a while and a fresh one made, as there is not forward secrecy within sessions.

# Client cache

messages/
    A flat folder of Git repositories, named by some unique ID. Each Git repository contains:
        subject.txt
        mainBox.txt
        metadata
        blobs/
            A flat folder of blobs, named by hash.
        program.wasm
database
	sent
		message ID
        commit hash
		time
		to
        status
	received
		from
		message ID
        commit hash
		time
crypto
    A binary file containing all my keys and other crypto information.
log
	A log of error messages, for debugging.

# Server cache

proofOfWorkDifficulty
	A file containing the proof of work difficulty.
price
	A file containing the price.
messages/
	a flat directory of messages, named by hash
database
	uploads
		sender username
		4 bytes: price
		8 bytes: Unix timestamp
		32 bytes: hash of message
	payments (obtained from payments provider API)
		username
		signed payment confirmation from provider
	users
		username
		hashed session key
    contacts
        owner username
        contact username
	tofrom
        counter (to keep messages in order)
		to username
		from username
		hash of message

# Pricing

There is a small fixed charge for each blob upload.

In its database, the server records message uploads for each user, and payments by each user.  When someone uploads a new message, the server queries these tables to calculate the user's balance. If the balance is high enough, the blob is accepted.

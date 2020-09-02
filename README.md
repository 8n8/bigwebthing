# Overview

BigWebThing is intended to provide a combined and improved solution to the problem of sharing messages and programs conveniently and securely.

User data is kept on their own machine, and never sent out unencrypted. Users' private crypto keys never leave their own machine.

Users can write programs and send them to each other safely and run them safely.

There is a message-passing server for sharing data between people.

The cost of running the server is met by users paying for the resources they use, as they use them.

It operates on a strict whitelist-only policy, so you only receive messages from people on your whitelist.

# Program structure

These are the different parts of the program:

1. (Go) The client backend. This runs on the client's computer and does most of the work, such as caching, crypto, and communicating with the server and the GUI.

2. (Elm) The GUI. This runs in a stripped-down web browser via webview.

3. (Javascript) The GUI plumbing. This handles the connection between the client backend and the Elm GUI, passing data through ports to Elm and websockets to the client backend.

4. (Rust / Webassembly) The user programs. These are programs that users can write and send to each other. They are pure functions written in Rust and compiled to Webassembly, and produce a DOM-like structure from a user input text box and user-uploaded binary blobs.

6. (Go) The server. This acts as a route between clients. They upload and download messages, and also use it to store their public keys.

# Calculating fingerprint

fingerprint = slowhash(public_static_key, 8-byte username)[:8]

# Client backend API

The client backend provides a server on port 11833. It provides a static file server on /static, a websockets server, and an HTTP API. Websockets are used for most of the messages. The only purpose of the HTTP is for dumping and retrieving large binaries.

## Websockets API

### Backend to frontend:

	Bad network connection
		1 byte: 0
	Good network connection
		1 byte: 1
    Index
        1 byte: 2
        contents of index file
    Message
        1 byte: 3
        24 bytes: message ID
        message
    Whitelist
        1 byte: 4
		sequence of 16-byte user IDs
    My user ID
        1 byte: 5
        16 bytes: user ID
    Backend ready
        1 byte: 6
    Message chain summary
        1 byte: 7
        20 bytes: chain ID
        sequence of message summaries, where each summary is
            8 bytes: POSIX time
            16 bytes: author ID
            sized string: subject
    Payments
        1 byte: 8
        sequence of payments, where a payment is
            8 bytes: Unix time of payment date
            4 bytes: amount paid in pence
    Price
        1 byte: 9
        4 bytes: price in pence
    Membership status
        1 byte: 10
        either in free period
            1 byte: 0
            8 bytes: start time
        or lapsed due to missing payments
            1 byte: 1
        or paid up
            1 byte: 2

### Frontend to backend:
    New message version
        1 byte: 0
        24 bytes: message ID
        message
    Write index
        1 byte: 1
        index
    Get index
        1 byte: 2
	Add to whitelist
		1 byte: 3
        16 bytes: user ID
	Remove from whitelist
		1 byte: 4
        16 bytes: user ID
	Get message
		1 byte: 5
        24 bytes: message ID
	Get whitelist
		1 byte: 6
	Get my ID
		1 byte: 7
    Get message chain summary
        1 byte: 8
        20 bytes: chain ID
    Get payments
        1 byte: 9
    Get price
        1 byte: 10
    Get membership status
        1 byte: 11

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

## TCP API

It will accept incoming TCP connections. A connection begins unauthenticated, and can change to authenticated. Messages that are only sent/acceptable on an authenticated channel are marked "AUTH".

Each message should be not more than 16KB, and should be prefixed with a 2-byte Little-Endian length.

### Server to client

	New message from another user (AUTH)
		1 byte: 0
        8 bytes: sender username
        <= 15991 bytes: the message
    New username (AUTH)
        1 byte: 1
        8 bytes: username
    Proof of work info
        1 byte: 2
		1 byte: difficulty
		16 bytes: random
    Price
        1 byte: 3
        4 bytes: monthly price in GBP^(-2)
    Public static key
        1 byte: 4
        8 bytes: key owner
        32 bytes: their key

### Client to server

    Get proof of work info
        1 byte: 0
    Sign in
        1 byte: 1
	    16 bytes: secret session key
	    8 bytes: username
    New account
		1 byte: 2
		24 bytes: proof of work
		16 bytes: random session key
        32 bytes: public static encryption key
	Send message (AUTH)
		1 byte: 3
		8 bytes: recipient username
		<= 15991 bytes: message
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
    Get public static key (AUTH)
        1 byte: 8
        8 bytes: their username

# Client to client

## API

The message or blob is sliced up into chunks, each chunk is encrypted, and is sent.

Before encryption, a chunk must be exactly 15942 bytes long. A chunk is encoded like this:

15942 bytes
    1 byte: 0 if a message, 1 if a blob
    15941 bytes
        either the whole message fits in one chunk
            1 byte: 0
            2 bytes: the length of the message
            the message
            padding
        or this is a part of a sequence but not the last item
            1 byte: 1
            4 bytes: chunk counter, starting at 0
            4 bytes: total number of chunks in the sequence
            the chunk
        or this is the final message in a sequence
            1 byte: 2
            32 bytes: the hash of the whole message before chunking
            2 bytes: length of the chunk
            the chunk
            padding

## Crypto

The cryptography is done using Cacophony, a Noise implementation in Haskell. It uses the KK pattern. Each user has a pair of static keys. For each of their contacts they have a public static key, and some handshakes in various stages. Temporary keys deleted after one payload. The server will accept messages that are <= 15991 bytes long:

    either some fresh first handshake messages
        1 byte: 0
        15984 bytes: 333 48-byte messages
            32 bytes: plain-text ephemeral key
            16 bytes: overhead of empty payload
    or some fresh second handshake messages
        1 byte: 1
        the messages, 96 bytes each (max 166 will fit)
            32 bytes: initial ephemeral key
                used as the unique session reference
            16 + 32 bytes: encrypted ephemeral key
            16 bytes: overhead of empty payload
    or a transport message
        1 byte: 2
        32 bytes: initial ephemeral key
            used as the unique session reference
        15958 bytes: Noise transport message
            16 bytes: crypto overhead
            15942 bytes: plaintext

# Encodings

## Message version encoding

A message version is encoded as follows:
    8 bytes: POSIX time
    2 bytes: number of members
    sequence of member IDs
    16 bytes: author ID
    sized string: subject
    sized string: main box
    2 bytes: number of blobs
    sequence of blobs, where one blob is
        32 bytes: hash of blob
        sized string: original file name of blob
    32 bytes: hash of WASM

## User ID encoding

### Internal

16 bytes
    8 bytes: username
    8 bytes: fingerprint

### User facing

The username is Little-Endian, and most of it is zeroes. The zero bytes are stripped off, and the remainder is attached to the end of the fingerprint. So an encoded user ID might be 11 bytes.

The encoding uses a custom word list with about 8000 words, very similar to Diceware. An 11-byte user ID requires 11 words:

basin glue tree unusable chug crushing hardwired

or

feminist polish fanfare front barber resume palpable

## Message ID encoding

24 bytes
    20 bytes: globally unique random message chaining ID
    4 bytes: version counter within the chain

# Client cache

blobs/
    A flat folder of binaries, named by their hash.

messages
    A flat folder of ZPAQ archives, one per message chain, named by chain ID.

index
    A file containing an index entry for each message chain
        sized string: subject of latest message
        16 bytes: author ID
        8 bytes: POSIX time of latest message
        24 bytes: message ID
        
crypto
    A binary file containing all my keys, sessions, and other crypto information.

log
	A log of error messages, for debugging.

# Server cache

proofOfWorkDifficulty
	A file containing the proof of work difficulty.
price
	A file containing the price.
usage limit
    A file containing the maximum number of messages per month.
messages/
	A flat directory of messages, named by counter.
database
	message_counts
		sender username
        number of messages this month
    payments
        username
        timestamp
	users
		username
		hashed session key
        static key
    contacts
        owner username
        contact username
	tofrom
        counter (to keep messages in order)
		to username
		from username

# Pricing

There is a fixed monthly charge, paid in advance, but the first part of a month and full month is free. There is a generous usage allowance, that is intended to be high enough that non-abusive users will never reach it.

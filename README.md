*Note that this project is in rapid progress, with new things added most days, but is not working yet.*

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

6. (Go) The server. This acts as a route between clients. They upload and download messages, and also use it to store their public keys. Users need to inform the server of their whitelists so that it can reject spam for them.

# Calculating fingerprint

fingerprint = slowhash(public_static_key, 8-byte username)[:8]

# Client backend API

The client backend provides a server on port 11833. It provides a static file server on /static, a websockets server, and an HTTP API. Websockets are used for most of the messages. The only purpose of the HTTP is for dumping and retrieving large binaries.

## Websockets API

### Backend to frontend:

A description of the view presented to the user.

### Frontend to backend:

Events triggered by the user, like clicking buttons and typing in the text boxes.

## HTTP API

/setblob
	Request
		binary blob
/getblob
	Request
        4 bytes: message ID
		32 bytes: hash of blob
	Response
		blob
/setcode
    Request:
        binary blob

# Server API

There are three main parts to the server infrastructure. They all provide TCP servers, and all have these routes as well as their individual ones:

    Client to server
        Signed auth code:
            1 byte: 0
            64 bytes: signed auth code

    Server to client
        Auth code to sign
            1 byte: 0
            32 bytes: random
        

1. A billing server for buying billing certificates. This provides a TCP server on port 53745 as follows:

Client to server
    Get billing certificate
        1 byte: 1
    Get inbox URL
        1 byte: 2
    Get blob URL
        1 byte: 3
    Get price
        1 byte: 4
    Get payment history
        1 byte: 5
    Shorten ID
        1 byte: 6
        32 bytes: public signing key
        UTF-8 sized string: URL of inbox server
    Update fingerprint hashing parameters
        1 byte: 7
        32 bytes: public signing key
        new fingerprint hashing parameters
Server to client
    Billing certificate
        1 byte: 1
        64 bytes: billing certificate
    Inbox URL
        1 byte: 2
        UTF-8 sized string: current inbox URL
    Blob URL
        1 byte: 3
        UTF-8 sized string: current blob server URL
    Price
        1 byte: 4
        4 bytes: Little-Endian monthly price in pence
    Payment history
        1 byte: 5
        sequence of payment amounts and times
    Shortened ID
        1 byte: 6
        8 bytes: shortened ID
        fingerprint hashing parameters

2. A bunch of blob storage servers. Blob server URLs are available from the billing server. This provides a TCP server on port 27316 as follows:

    Client to server
        Upload blob
            1 byte: 1
            64 bytes: billing certificate
            <= 15935 bytes: the blob
        Download blob
            1 byte: 2
            32 bytes: hash of blob
    Server to client
        Requested blob
            1 byte: 3
            <= 15935 bytes: the blob
        Acknowledgement
            1 byte: 4
            32 bytes: hash of blob
        No such blob
            1 byte: 5
            32 bytes: hash of blob
        Can't serve you now
            1 byte: 6

3. A bunch of message-passing servers. Each user is assigned to a message-passing server where their inbox is stored. This provides a TCP server on port 55792 as follows:

    Client to server
        Send message
            1 byte: 1
            64 bytes: billing certificate
            32 bytes: recipient public signing key
            32 bytes: message
        Delete inbox
            1 byte: 2

    Server to client
        Message acknowledgement
            1 byte: 1
            32 bytes: hash of whole 'send message' request
        Inbox
            1 byte: 2
            sequence of messages, where each is
                32 bytes: sender public signing key
                32 bytes: message


Users are assigned a server URL by the billing server.

## TCP API

It will accept incoming TCP connections over TLS.

All messages should be prefixed by a 4-byte little-endian length.

### Server to client

	Inbox
		1 byte: 0
        sequence of message stubs, where each is
            32 bytes: sender public signing key
            32 bytes: message hash
    Shortened key
        1 byte: 1
        8 bytes: shortened key
    Price
        1 byte: 2
        4 bytes: monthly price in pence
    Message acknowledgement
        1 byte: 3
        32 bytes: hash of message
    Blob acknowledgement
        1 byte: 4
        32 bytes: hash of blob
    Billing certificate
        1 byte: 5
        72 bytes: certificate
    Auth code
        1 byte: 6
        32 bytes: random
    Blob
        1 byte: 7
        <= 15999 bytes: the blob

### Client to server

Messages must be no more than 16KB, not counting the length prefix.

    Get billing certificate
        1 byte: 0
	Get price
		1 byte: 1
    Get payment history
        1 byte: 2
	Send message
		1 byte: 3
        64 bytes: billing certificate
		32 bytes: recipient public signing key
        32 bytes: message hash
	Delete inbox
		1 byte: 4
    Upload blob
        1 byte: 5
        64 bytes: billing certificate
        <= 15935 bytes: the blob
    Shorten public key
        1 byte: 6
        32 bytes: public key
    ID token
        1 byte: 7
        64 bytes: Ed25519 signature of auth code
    Get blob
        1 byte: 8
        32 bytes: blob hash

# Client to client

## API

The messages can be <= 15935 bytes long. There are several layers to the API, as follows.

### Top layer

One of these:

    15745 bytes: Noise first handshake messages
        1 byte: 0
        15744 bytes: 492 32-byte messages
            (492 = 4 x 123, which is the number of second shakes
             in a chunk)

    15809 bytes: Noise second handshake messages
        1 byte: 1
        64 bytes: signature of Noise static key
        15744 bytes: 123 128-byte messages
            32 bytes: first handshake message
            32 bytes: my plain-text ephemeral key
            32 + 16 bytes: encrypted static key
            16 bytes: overhead of empty payload

    113 bytes: Noise transport message
        1 byte: 2
        32 bytes: first handshake message
        16 bytes: overhead
        64 bytes
            encrypted
                32 bytes: hash of encrypted chunk
                32 bytes: secret key of encrypted chunk

    <= 15935 bytes: Symmetrically encrypted chunk
        1 byte: 3
        12 bytes: random nonce
        16 bytes: auth tag
        <= 15906 bytes
            encrypted
                either
                    1 byte: 0 (this is the last chunk)
                    <= 15905 bytes: the chunk
                or
                    1 byte: 1 (this is not the last chunk)
                    32 bytes: the hash of the next encrypted chunk
                    15873 bytes: the chunk

### Before chunking but after encoding

After encoding the message (or before chunking and sending it), there is another API inside it:

    32 bytes: globally unique message ID
    8 bytes: integrity check
    either a header blob
        1 byte: 0
        the header blob
    or a referenced blob
        1 byte: 1
        the referenced blob

## Crypto

The inbox messages are encrypted using Cacophony, a Noise implementation in Haskell. It uses the XX pattern. Each user has a pair of static keys. For each of their contacts they have some handshakes in various stages. Temporary keys deleted after one payload.

All client-server traffic is encrypted with TLS.

The Noise messages just contain blob hashes and secret keys, and then the actual content is symmetrically encrypted with ChaChaPoly1305.

# Encodings

## Message header encoding for local storage

A message version is encoded as follows:
    8 bytes: POSIX time
    2 bytes: number of members
    sequence of 32-byte member public signing keys
    32 bytes: author signing key
    sized string: main box
    2 bytes: number of blobs
    sequence of blobs, where one blob is
        32 bytes: hash of blob
        sized string: original file name of blob
    32 bytes: hash of WASM

## Message header encoding for transmission

The same as for local storage, except the hashes and secret keys of the encrypted binaries are stored alongside the hashes of plaintext binaries.

## Message edit encoding

The difference between two encoded messages is encoded as follows:

    start (int)
        The position of the first character in the string that is different to the other. Examples are:

        ("", "a") => 0
        ("ab", "ac") => 1

    end (int)
        If the strings are both reversed, this is the position of the first character that is different. Examples are:

        ("", "a") => 0
        ("ab", "ac") => 0
        ("ab", "b") => 1

    insert (blob)
        the piece of the new string that lies between start and end

    integrity (blob)
        an 8-byte BLAKE2b hash of the new string

    the author ID of the edit

This should lead to efficient storage of diffs because most changes are going to be character inserts and deletions.


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

# Client cache

blobs
    A key-value store of binaries.

messages
    a flat folder of message files (sets of edits), named by locally unique message IDs, where each is an encoded sequence of diffs

memCache
    A binary file containing a dump of the in-memory cache.

log.txt
	A log of error messages, for debugging.

# Server cache

blobs
    A flat directory of binaries, named by hash.

memCache
    A binary file containing a dump of the in-memory cache.

inboxes
    A flat directory of inbox files, one per user.

log.txt
    Log messages for debugging.

# Pricing

It is free to download messages and blobs, but only paying users can upload them. Users pay by buying a monthly billing certificate off the server, which they can present when required.

There will probably also be a scheme where paying users can invite people for a free trial period.

There is a generous usage allowance, that is intended to be high enough that non-abusive users will never reach it.

# Embedded programming language (speculative)

This might be put in a later version. The idea is that there will be a programming language embedded in the system for making new built-in programs.

It will be specifically designed to target WASM.

It will have a Lisp-like syntax, except that instead of parentheses, it will use whitespace.

So in Python, a dict would be {"hi": 3, "apple": 4, "onions": 55, "stew": 22}. In Lisp it would be like:

(Map.fromList (("hi" 3) ("apple" 4) ("onions" 55) ("stew" 22)))

And in Truelang it would be like:

====Top of file====
Map.fromList
    "hi" 3
    "apple" 4
    "onions" 55
    "stew" 22

And a file is just a convenience for making a list, so you don't have to indent the whole file to get the opening parenthesis.

And the main feature of the language is

|
| Runtime exceptions are compiler bugs.
|

This is achieved with a type-level lanugage interpreter. So there is a lot of type-level computation. If you type 3 + 2 then the type of the result will be 5, not 'int'.

Other ideas:

+ no type annotations
+ there are no user-defined names, you just use maps
+ module imports include a cryptographic hash of the file so that builds are deterministic
+ performance is nice, but will usually be sacrificed if it makes users lives easier

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

The server runs a TCP server on port 53745.

All messages should be prefixed by a 4-byte little-endian length.

All these messages between client and server are encrypted with TLS.

Client to server
    Signed auth code:
        1 byte: 0
        32 bytes: public key
        64 bytes: signed auth code
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
        // If this is a new signing key, then the server will respond
        // with a new username. If not, it will update the record.
        1 byte: 6
        61 bytes
            32 bytes: public Noise key
            20 bytes: URL of inbox server
            10 bytes: fingerprint hashing options
    Upload blob
        1 byte: 7
        72 bytes: billing certificate
        <= 15935 bytes: the blob
    Download blob
        1 byte: 8
        32 bytes: hash of blob
    Send message
        1 byte: 9
        72 bytes: billing certificate
        32 bytes: recipient public signing key
        32 bytes: message
    Delete message
        1 byte: 10
        32 bytes: sender public signing key
        32 bytes: message
Server to client
    Auth code to sign
        1 byte: 0
        32 bytes: random
    Billing certificate
        1 byte: 1
        72 bytes: billing certificate
    Inbox URL
        1 byte: 2
        20 bytes: current inbox URL
    Blob URL
        1 byte: 3
        20 bytes: current blob server URL
    Price
        1 byte: 4
        4 bytes: Little-Endian monthly price in pence
    Payment history
        1 byte: 5
        sequence of payment amounts and times
    Shortened ID
        1 byte: 6
        8 bytes: shortened ID
    Requested blob
        1 byte: 7
        <= 15935 bytes: the blob
    Acknowledgement
        1 byte: 8
        32 bytes: hash of blob
    No such blob
        1 byte: 9
        32 bytes: hash of blob
    Too busy
        1 byte: 10
    Message acknowledgement
        1 byte: 11
        32 bytes: hash of whole 'send message' request
    Inbox message
        1 byte: 12
        32 bytes: sender public signing key
        32 bytes: message

# Client to client

## API

The messages can be <= 15935 bytes long. There are several layers to the API, as follows.

### Top layer

One of these:

    15841 bytes: Noise first handshake messages
        1 byte: 0
        15840 bytes: 495 32-byte messages
            (495 = 3 x 165, which is the number of second shakes
             in a chunk)

    15841 bytes: Noise second handshake messages
        1 byte: 1
        15840 bytes: 165 96-byte messages
            32 bytes: first handshake message
            32 + 16 bytes: my encrypted ephemeral key
            16 bytes: overhead of empty payload

    133 bytes: Noise transport message
        1 byte: 2
        32 bytes: first handshake message
        16 bytes: overhead
        84 bytes
            encrypted
                32 bytes: hash of encrypted chunk
                32 bytes: secret key of encrypted chunk
                20 bytes: URL of blob server

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
                    20 bytes: URL of next encrypted chunk
                    <= 15853 bytes: the chunk

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

The inbox messages are encrypted using Cacophony, a Noise implementation in Haskell. It uses the KK pattern. Each user has a pair of static keys. For each of their contacts they have some handshakes in various stages. Temporary keys deleted after one payload.

All client-server traffic is encrypted with TLS.

The Noise messages just contain blob hashes and secret keys, and then the actual content is symmetrically encrypted with ChaChaPoly1305.

# Encodings

## Slow hashing options

    Argon2Id:
        1 byte: 0
        4 bytes: iterations
        4 bytes: memory
        1 byte: parallelism

## Billing certificate

    72 bytes
        64 bytes: Ed25519 signature
        8 bytes: POSIX expiry timestamp

## Server URLs

Server URLs are pretty short, and are encoded in 20 bytes:

    1 byte: number of characters in UTF-8 up to 127
    the URL, with one byte per character
    padding up to 20 bytes

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

The same as for local storage, except the hashes, urls and secret keys of the encrypted binaries are stored alongside the hashes of plaintext binaries.

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
    A flat directory containing an inbox file per user.

payments
    A flat directory containing a transactions file per user.

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

So:

+ "(" == newline and increment indent level
+ ")" == two or more newlines and decrement indent level
+ " " == " " or newline

So in Python, a dict would be {"hi": 3, "apple": 4, "onions": 55, "stew": 22}. In Lisp it would be like:

(Map.fromList (("hi" 3) ("apple" 4) ("onions" 55) ("stew" 22)))

And in Truelang it would be like:

====Top of file====
"hi" 3

"apple" 4

"onions" 55

"stew" 22

And a file is just a convenience for making a map, so you don't have to indent the whole file to get the opening parenthesis.

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
+ items in maps can reference their parent map

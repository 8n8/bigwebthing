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
    Signed auth code: 0
        1 byte:
        32 bytes: public key
        64 bytes: signed auth code
    Get prices
        1 byte: 1
    Shorten ID
        // If this is a new signing key, then the server will respond
        // with a new username. If not, it will update the record.
        1 byte: 2
        payment details
        42 bytes
            32 bytes: public Noise key
            10 bytes: fingerprint hashing options
    Upload blob
        1 byte: 3
        payment details
        32 bytes: blob ID
        the blob
    Download blob
        1 byte: 4
        32 bytes: blob ID
    Send message
        1 byte: 5
        payment details
        32 bytes: recipient public static Noise key
        inbox message
    Delete message
        1 byte: 6
        32 bytes: sender public static Noise key
        32 bytes: message
    Look up shortened
        1 byte: 7
        8 bytes: short ID
Server to client
    Auth code to sign
        1 byte: 0
        32 bytes: random
    Prices
        1 byte: 1
        4 bytes: shortening price
        4 bytes: blob upload price
        4 bytes: message upload price
    Shortened ID
        1 byte: 2
        8 bytes: shortened ID
    Requested blob
        1 byte: 3
        32 bytes: blob ID
        the blob
    Acknowledgement
        1 byte: 4
        32 bytes: hash of message
    Inbox message
        1 byte: 5
        inbox message
    New transaction (like an account top-up)
        1 byte: 6
        transaction
    Long ID for short ID
        1 byte: 7
        8 bytes: short ID
        42 bytes
            32 bytes: public Noise key
            10 bytes: fingerprint hashing options

# Client to client

## API

There are several layers to the API, as follows.

## Crypto

The inbox messages are encrypted using Cacophony, a Noise implementation in Haskell. It uses the KK pattern. Each user has a pair of static keys. For each of their contacts they have some handshakes in various stages. Temporary keys deleted after one payload.

All client-server traffic is encrypted with TLS.

The Noise messages just contain blob hashes and secret keys, and then the actual content is symmetrically encrypted with ChaChaPoly1305.

# Encodings

## Inbox message

One of:
    89 bytes: Noise KK first handshake message
        1 byte: 0
        32 bytes: initiator public ephemeral key
        56 bytes: encrypted seed of 999 more first handshake messages
    137 bytes: Noise KK second handshake message
        1 byte: 1
        32 bytes: initiator public ephemeral key
        48 bytes: encrypted responder public ephemeral key
        56 bytes: encrypted seed of 999 more second handshake messages
    89 bytes: Noise KK transport static message
        1 byte: 2
        32 bytes: initiator public ephemeral key
        encrypted:
            16 bytes: MAC
            40 bytes: seed of header

## Slow hashing options

    Argon2Id:
        1 byte: 0
        4 bytes: iterations
        4 bytes: memory
        1 byte: parallelism

## Payment details

    previous transaction
    this transaction
    64 bytes: signature of hash of new accounts

## Transaction

    4 bytes: amount in m£
    8 bytes: POSIX time stamp
    4 bytes: new balance in m£
    either
        Payment to server
            1 byte: 0
            32 bytes: hash of message
    or
        Account top-up
            1 byte: 1
            32 bytes: unique ID
            extra fields like credit card number, payment provider etc
    32 bytes
        hash of previous transaction hash combined with this
        transaction encoded (without the hash of course)

## Message header encoding

A message version is encoded as follows:
    32 bytes: random message ID
    8 bytes: POSIX time
    2 bytes: number of members
    sequence of 32-byte member public static Noise keys
    32 bytes: author public static Noise key
    sized string: main box
    2 bytes: number of blobs
    sequence of blobs, where one blob is
        40 bytes: seed of blob
        sized string: original file name of blob
    40 bytes: seed of wasm

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
    A flat directory of small encrypted blobs, with 32-byte random names.

database
    uploadstatus
        seed
        uploaded
    myephemeralkeys
        public
        secret
    theirfirstshakes
        theirephemeral
        theirstatic
        encrypted
    theirsecondshakes
        theirephemeral
        theirstatic
        encrypted

memCache
    A binary file containing a dump of the in-memory cache.

log.txt
	A log of error messages, for debugging.

accounts
    A file containing the user's blockchain-like accounts data structure.

# Server cache

blobs
    A flat directory of small encrypted blobs, with 32-byte random names.

memCache
    A binary file containing a dump of the in-memory cache.

log.txt
    Log messages for debugging.

database
    accountsignatures
        user
        signature
    messages
        sender
        recipient
        message
    waitingpayments
        uniqueid
        encodedpayment
    shortenings
        user
        short
        shortenable

# Pricing

It is free to download messages and blobs, but there is a small fee for uploading messages and blobs, and for shortening usernames.

There will probably also be a scheme where paying users can invite people for a free trial period.

# Accounting

Clients are responsible for storing their accounts information, and uploading it to the server when required.

The server maintains the signature of the whole accounts for each client. It will only update it when the client uploads the previous transaction and a new one and a new signature.

The server will only accept account top-ups if it has a corresponding transaction stored in its database. Once the client has added it, the server will delete it. The server will not accept negative balances, or balances higher than a constant small upper limit.

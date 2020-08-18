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

# User ID

A user ID has two components:

1. A username generated by the server.  This is a variable-length unsigned integer, which is a unique ID in communications with the server. It will be at least one byte, with the first user having an ID of 0x00, the second 0x01, and so on.

2. A fingerprint of the static public crypto key. This needs to be long engough so that it is infeasible to mount a man in the middle attack. So if there is an adversary in control of the communication channel, in order to mount a man-in-the-middle, they need to find a pair of keys that matches the fingerprint for each party. If I use a very slow hash to generate the fingerprint, then a uint64 should be sufficient as a fingerprint. The fingerprint is generated by hashing the public key concatenated with the username from the server.

So the whole user ID is:

	very_slow_hash(public_key || username)[:8] || username

The encoding uses a custom word list with about 8000 words, very similar to Diceware. So for someone with a 3-byte username, their user ID will be 11 bytes, which will require 7 words, like:

basin glue tree unusable chug crushing hardwired

or

feminist polish fanfare front barber resume palpable

# Client backend API

The client backend provides a server on port 11833. It provides a static file server on /static, a websockets server, and an HTTP API. Websockets are useful for the client backend to push new messages to the front-end when they happen. HTTP is good because I can dump very large file uploads into the request bodies and stream them over to the backend. Elm does not have a native websockets capability, so it has to be done via a port. Everything that goes through an Elm port has to be a string in memory, so binary data has to be chunked and encoded to a string.

## Websockets API

Backend to frontend:

	Inbox altered
		1 byte: 0
	Bad network connection
		1 byte: 1
	Good network connection
		1 byte: 2
	Send failed
		1 byte: 3
		20 bytes: message hash
	Progress of sending
		1 byte: 4
		20 bytes: snapshot hash
		20 bytes: previous hash
		4 bytes: total bytes in message
		4 bytes: bytes sent
    Whole message:
        1 byte: 5
        message:
    Whitelist:
        1 byte: 6
		sequence of sized user IDs
    My ID
        1 byte: 7
        my ID
    Drafts summary
        1 byte: 8
        drafts summary
    Sent summary
        1 byte: 9
        sent summary
    Inbox summary
        1 byte: 10
        inbox summary
    Backend ready
        1 byte: 11

Frontend to backend:

	Set snapshot
		1 byte: 0
		sized bytes: previous snapshot
		string: new snapshot
	Send message
		1 byte: 1
		20 bytes: previous hash
		20 bytes: snapshot hash
        sized user ID
	Add to whitelist
		1 byte: 2
		sized user ID
	Remove from whitelist
		1 byte: 3
		sized user ID
	Get message
		1 byte: 4
		20 bytes: previous hash
		20 bytes: snapshot hash
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

## HTTP API

/setblob
	Request
		binary blob
	Response
		hash of blob
/getBlob
	Request
		1 byte: 4
		20 bytes: hash of blob
	Response
		blob

## Message snapshot format

A snapshot contains the current state of a message. It is a binary blob, encoded as follows:
	+ sized string: subject
	+ sized string: user input box contents
	+ 20 bytes: hash of program
	+ sequence of blobs, where a blob is:
		+ 20 bytes: hash
		+ 4 bytes: size in bytes

# Server API

The server provides an HTTP API on port 8001, and also a TCP connection on 8002 so that the server can send new messages to the client without them being requested first.

## Proof of work

Some APIs are protected by a proof of work problem. To create a proof of work token, the user must download some unique bytes and a difficulty value from the server, and find some more unique bytes that will create a slow hash with all the bytes greater than the difficulty.  So a proof of work token is like this:

+ 16 bytes: unique from the server
+ 8 bytes: calculated by the client

The server checks that the first part is indeed something that it recently gave out, then that the hash of the whole meets the current difficulty.

## TCP API

It will accept incoming TCP connections. The client's first message should be like this:

	sized server ID
	16 bytes: secret session key

Then the client should just listen on the connection. Messages from the server can take these forms:

	Request for an ephemeral key:
		1 byte: 0

	New message from another user
		1 byte: 1
		sized bytes: sender username
		32 bytes: message hash

		(The reason for sending the hash rather than the whole message is to protect the user from DDOS attacks from unwanted senders. If they don't recognise the username they can just request that the message is deleted without downloading it.)

## HTTP API

The maximum request body size is 16KB.

/api
	Create account
	    Request
			1 byte: 0
			24 bytes: proof of work
			16 bytes: random session key
		Response:
			username
	Get proof of work info
		Request
			1 byte: 1
		Response
			1 byte: difficulty
			16 bytes: random
	Send message
		Request
			1 byte: 2
			16 bytes: session key
			sized bytes: my username
			sized bytes: recipient username
			message
	Download message
		Request
			1 byte: 3
			16 bytes: session key
			sized bytes: my username
			32 bytes: message hash
	Delete message
		Request
			1 byte: 4
			16 bytes: session key
			sized bytes: my username
			32 bytes: message hash
	Get price
		Request
			1 byte: 5 
		Response
			4 bytes: price in GBP^-4
	Get ephemeral key for user:
		Request:
			1 byte: 6
			24 bytes: proof of work
			sized bytes: their username
		Response:
			a key or empty if there isn't one
	Upload ephemeral key:
		Request:
			1 byte: 7
			16 bytes: session key
			sized bytes: my username
			32 bytes: the key
			
# Client to client

## API

A message is made up of a chain of diffs, and some blobs. It is encoded as follows:
	+ 4 bytes: number of diffs
	+ sequence of diffs
	+ sequence of blobs, where a blob is some length-encoded bytes

Then the message is sliced up into 15KB chunks, each chunk is encrypted, and is sent.

Before encryption, a chunk must be exactly 15KB long. A chunk is encoded like this:

	sized padding
		2 bytes: length of padding
		the padding: it doesn't matter what it is, so just use zeros
	4 bytes: a counter, starting at 0
	4 bytes: the total number of chunks in the whole message
	32 bytes: the hash of the whole message before chunking
	all the rest of the bytes: the chunk

## Crypto

The end-to-end cryptography is done using Noise, using the KK pattern, i.e. both parties know the other's public static key before the handshake.

The KK handshake is:

-> s
<- s
...
-> e, es, ss
<- e, ee, se

Any message payloads after this handshake are secure.

The server maintains a list of 100 ephemeral public keys for each user, and prompts them for new ones when they get used.

So to send someone a message, I download one of their ephemeral keys from the server, and use Noise to generate an encrypted ephemeral key to send to them.

Then I use Noise again to encrypt a payload. So I send them:

their ephemeral key || my encrypted ephemeral key || my transport message

Each user must keep a record of their valid, unused ephemeral keys, and never accept a message the reuses an ephemeral key.

When I receive a message, I need to:

1. get their static key from my cache
2. check that the ephemeral key they are offering (originally from me) has not been used
3. feed it all through Noise and get the plain-text

# Client cache

blobs/
	A flat folder full of blobs, named by hash.
database
	diffs
		hash
		previous_hash
		start
		end
		insert
		time
		author username
	sent
		hash
		time
		to
	received
		from
		hash
		time
	fingerprints
		username
		fingerprint
	my_ephemeral_keys
		public
		secret
myKeys
	A binary file containing my private static key, session key, and username.
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
	ephemeral_keys
		username
		key
	tofrom
		to username
		from username
		hash of message

# Pricing

There is a small fixed charge for each blob upload.

Blobs last for a few weeks and are then deleted automatically.

In its database, the server records blob uploads for each user, and payments by each user.  When someone uploads a new blob, the server queries these tables to calculate the user's balance. If the balance is high enough, the blob is accepted.

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

A user ID is a public key fingerprint.

The fingerprint is the first few bits of a slow hash of a user's public signing key.

The user fingerprint length is calculated as follows:

It must be long enough that it takes too long for an adversary to generate keys that
have the same user ID as someone else. It should be as short as possible so that it
is convenient for people to read and share.

The only thing the adversary can do is keep generating keys and testing if the ID matches one on the system.

Lets say that 2^70 'fast' operations is too much for the attacker. Then if I make them use a slow hash in each operation, and say that is equivalent to around 2^12 'fast' operations. So 2^58 'slow' operations is too much for the attacker.

Say there are a maximum 2^40 different users, then that means the user ID should be at least 58 + 40 = 98 bits. This will fit in 13 bytes.

The encoding uses a 7776 word list from the EFF, very similar to Diceware. So a 98 bit fingerprint will need at least 8 words, or say 9 for a bit more safety, like:

basin glue tree
unusable chug crushing
hardwired varsity coma

or

feminist polish fanfare
front barber resume
palpable carpool return

So a user ID is 14 bytes long, since 14 bytes will fit in 9 words.

# Client backend API

The client backend provides a server on port 11833. It provides a static file server on /static, a websockets server, and an HTTP API.

## Websockets API

The websockets API is for sending messages from the client backend to the frontend.

Messages can take the following form:

	Summary of inbox
		1 byte: 0
		summary
	Bad network connection
		1 byte: 1
	Good network connection
		1 byte: 2
	Send failed
		1 byte: 3
		20 bytes: message hash
	Progress of sending
		1 byte: 4
		20 bytes: message hash
		4 bytes: number of blobs remaining to send

## HTTP API

/setblob
	Request
		binary blob
	Response
		hash of blob
/api
	Set snapshot
		Request
			1 byte: 0
			sized string: previous snapshot
			string: new snapshot
	Send message
		Request
			1 byte: 1
			20 bytes: diff hash to send
			14 bytes: user ID
	Add to whitelist
		Request
			1 byte: 2
			14 bytes: user ID
	Get blob
		Request
			1 byte: 3
		Response
			blob
	Get snapshot
		Request
			1 byte: 4
			20 bytes: hash of snapshot
		Response
			snapshot
	Get whitelist
		Request
			1 byte: 5
		Response
			sequence of 14-byte user IDs
	Get my ID
		Request
			1 byte: 6
		Response
			14 bytes: my ID
	Get drafts summary
		Request
			1 byte: 7
		Response
			summaries of all drafts
	Get sent summary
		Request
			1 byte: 8
		Response
			summaries of all sent messages
	Get inbox summary
		Request
			1 byte: 9
		Response
			summaries of received messages

# Server API

The server provides an HTTP API on port 8001, and also a TCP connection on 8002 so that the server can send new messages to the client without them being requested first.

## Proof of work

Some APIs are protected by a proof of work problem. To create a proof of work token, the user must download some unique bytes and a difficulty value from the server, and find some more unique bytes that will create a 32-byte Argon2id hash with all the bytes greater than the difficulty.  So a proof of work token is like this:

+ 16 bytes: unique from the server
+ 8 bytes: calculated by the client

The server checks that the first part is indeed something that it recently gave out, then that the hash of the whole meets the current difficulty.

## TCP API

It will accept incoming TCP connections. The client's first message should be like this:

	14 bytes: my ID
	signed
	    16 bytes: 6b 87 4c de cc f0 28 b3 7c 4e de ee 15 ca 92 93
	    16 bytes: auth code

Then the client should just listen on the connection. Messages from the server can take these forms:

	Request for more encryption keys
		1 byte: 0
	New message from another user
		1 byte: 1
		104 bytes: message

## HTTP API

/api
	Create account
	    Request
			1 byte: 0
			24 bytes: proof of work
	    	32 bytes: public signing key
	Upload public encryption key
		Request
			1 byte: 1
			14 bytes: my ID
			32 + 96 bytes: signed public encryption key
		Response
			1 byte: 0 if server has enough keys; 1 if OK
	Get signing key for ID
		Request
			1 byte: 2
			14 bytes: user ID
		Response
			either
				1 byte: 0 (the key doesn't exist)
			or
				1 byte: 1 (the key exists)
				32 bytes: the key
	Get disposable encryption key for ID
		Request
			1 byte: 3
			14 bytes: user ID
			24 bytes: proof of work
		Response
			either
				1 byte: 0 (no keys available)
			or
				1 byte: 1 (key is available)
				32 bytes: public encryption key
	Get proof of work info
		Request
			1 byte: 4
		Response
			1 byte: difficulty
			16 bytes: random
	Get auth code
		Request:
			1 byte: 5
		Response:
			16 bytes: random
	Upload blob
		Request
			1 byte: 6
			14 bytes: my ID
			signed
				16 bytes: 0a cb 78 89 67 cf 64 19 2a dd 32 63 61 2d 10 18
				16 bytes: auth code
				blob
	Send message
		Request
			1 byte: 7
			24 bytes: proof of work
			14 bytes: recipient ID
			104 bytes:
				24 bytes: nonce
				16 bytes: NACL box overhead
				encrypted box containing
					32 bytes: hash of encrypted blob on server
					32 bytes: symmetric key of encrypted blob on server
	Delete message
		Request
			1 byte: 8
			14 bytes: my ID
			signed
				16 bytes: d3 ad fa b3 b4 67 41 bb 51 19 de d5 56 e5 9c 8e
				16 bytes: auth code
				32 bytes: hash of the message to delete
	Download blob
		Request
			1 byte: 9
			32 bytes: hash of blob to download
		Response
			either
				1 byte: 0 (the blob doesn't exist)
			or
				1 byte: 1 (the blob exists)
				blob
	Get price
		Request
			1 byte: 10
		Response
			4 bytes: unsigned Little-Endian int: price in GBP^-4
			
# Client to client API

To send a message:

1. encode it and its blobs and pipe small chunks (~16KB) to a channel
	+ each chunk is prefixed with the hash of the next chunk
2. for each chunk in the channel:
	a. symmetrically encrypt
	b. send to server
3. encode the hash and key of the first chunk
4. encrypt it with their public key and send it to them

Inside the encryption and chunking, the API is as follows:

	Regular inbox message
	    1 byte: 0
	    104 bytes: encoded message
	Acknowledgement
		1 byte: 1
	    signed
	        16 bytes: 77 0a 8b e7 5a 1a 9e 31 c5 97 5b 61 ec 47 16 ef
	        8 bytes: Unix time received
	        32 bytes: hash of message received

# Client cache

blobs/
	A flat folder full of blobs, named by hash.
database
	diffs
		start
		end
		insert
		hash
		previous_hash
		author
	sent
		hash
		time
		to
	received
		from
		hash
		time
	whitelist
		user
	public_sign_keys
		user
		sign
	acknowledgements
		from
		time
		hash
		signature
	my_encryption_keys
		public
		secret
myKeys
	A binary file containing my public and private signing keys.
log
	A log of errors, for debugging.

# Server cache

blobs/
	A flat directory containing files named by their hash.
proofOfWorkDifficulty
	A file containing the proof of work difficulty.
price
	A file containing the price.
database
	uploads
		user_id
		signed confirmation from user:
			4 bytes: price
			8 bytes: Unix timestamp
			16 bytes: de 76 b4 ba 27 13 5d 9a e0 6e ca b7 5e 20 48 e4
			32 bytes: hash of uploaded blob
	payments (obtained from payments provider API)
		user_id
		signed payment confirmation from provider
	users
		user_id
		public_signing_key
	encryption_keys
		user_id
		key

# Pricing

There is a small fixed charge for each blob upload.

Blobs last for a few weeks and are then deleted automatically.

In its database, the server records blob uploads for each user, and payments by each user.  When someone uploads a new blob, the server queries these tables to calculate the user's balance. If the balance is high enough, the blob is accepted.

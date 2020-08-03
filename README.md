# Overview

It is intended to provide a combined and improved solution to the problem of sharing messages and programs conveniently and securely.

User data is kept on their own machine, and never sent out unencrypted. Users' private crypto keys never leave their own machine.

Users can write programs and send them to each other safely and run them safely.

There is a message-passing server for sharing data between people. It stores messages for a fixed period of time and then deletes them.

The cost of running the server is met by users paying for the resources they use, as they use them.

It operates on a strict whitelist-only policy, so you only receive messages from people on your whitelist.

# Program structure

These are the different parts of the program:

1. (Go) The client backend. This runs on the client's computer and does most of the work, such as caching, crypto, and communicating with the server and the GUI.

2. (Elm) The GUI. This runs in a stripped-down web browser via webview.

3. (Javascript) The GUI plumbing. This handles the connection between the client backend and the Elm GUI, passing data through ports to Elm and websockets to the client backend.

4. (Rust / Webassembly) The user programs. These are programs that users can write and send to each other. They are pure functions written in Rust and compiled to Webassembly, and produces a DOM-like structure from a user input text box and user-uploaded binary blobs.

6. (Go) The server. This acts as a route between clients. They upload and download messages, and also use it to store their public keys.

# User ID

A user ID is a public key fingerprint.

The fingerprint is the first few bits of a slow hash of a user's public signing key.

The user fingerprint length is calculated as follows:

It must be long enough that it takes too long for an adversary to generate keys that
have the same user ID as someone else. It should be as short as possible so that it
is convenient for people to read and share.

The only thing the adversary can do is keep generating keys and testing if the ID matches one on the system.

Lets say that 2^70 'fast' operations is too much for the attacker. Then if I make them use a slow hash in each operation, that is around 2^12 'fast' operations. So 2^58 'slow' operations is too much for the attacker.

Say there are a maximum 2^40 different users, then that means the user ID should be 58 + 40 = 98 bits. This will fit in 13 bytes.

The encoding uses a 7776 word list from the EFF, very similar to Diceware. So a 98 bit fingerprint will need 8 words, like:

fried veal frightful untoasted uplifting carnation breezy hazy

or

rethink doornail refining handiness lend strainer appealing deputy

Which is pretty long, but it's the best I can do.

# Client backend API

The client backend provides a server on port 11833. It provides a websockets server, an HTTP API, and a static file server on /static.

## Websockets API

The websockets API is for sending messages from the client backend to the frontend.

Messages can take the following form:

1. Summary of inbox
	+ 0x01
	+ summary

2. Bad network connection
    + 0x02

3. Good network connection
    + 0x03

4. Send failed
    + 0x04
    + 8 bytes: draft ID

5. Progress of sending
	+ 0x05
	+ 8 bytes: draft ID
	+ 4 bytes: number of blobs remaining to send

## HTTP API


/cache
	/set
		/:message_id
			/diff
				+ 4 bytes: where to insert the text
				+ 4 bytes: position of the end of the text
				+ 32 bytes: hash before diff
				+ 32 bytes: hash after diff
				+ string: the text to insert
			/to
				Request:
				+ 13 byte user ID
			/subject
				Request:
				+ UTF-8 subject
			/user_input
				Request:
				+ UTF-8 user input
			/blob
				Request:
				+ binary blob
			/program
				Request:
				+ binary blob of Wasm
			/send
			/whitelist
				Request:
				+ 13 bytes: user ID to add to whitelist
	/get
		/:message_id
			/to
				Response:
				+ 13 byte user IDs, one after another
			/subject
				Response:
				+ UTF-8 subject string
			/user_input
				Response:
				+ UTF-8 subject string
			/blob
				Request:
				+ 32-bytes: SHA-256 hash of blob
				Response:
				+ the body of the blob
			/program
				Response:
				+ binary blob of program
	/delete
		/:message_d
			/to
				Request:
				+ 13 bytes: user ID to remove from message
			/subject
			/userinput
		/blob
			Request:
			+ 32 bytes: SHA-256 hash of blob
		/whitelist
			Request:
			+ 13 bytes: user ID to remove from whitelist
/unique
    Response:
    + unique UTF-8 string
/myid
	Response:
	+ 13 bytes: my ID
/contacts
	Response
	+ sequence of 13-byte user IDs
/summary
	/drafts
		Response:
		+ summaries of all drafts
	/sends
		Response:
		+ summaries of all sent messages
	/inbox
		Response:
		+ summaries of all received messages

# Server API

The server provides an HTTP API on port 8001, and also a TCP connection on 8002 so that the server can send new messages to the client without them being requested first.

## Proof of work

Some APIs are protected by a proof of work problem. To create a proof of work token, the user must download some unique bytes and a difficulty value from the server, and find some more unique bytes that will create a 32-byte Argon2id hash with all the bytes greater than the difficulty.  So a proof of work token is like this:

+ 16 bytes: unique from the server
+ 8 bytes: calculated by the client

The server checks that the first part is indeed something that it recently gave out, then that the hash of the whole meets the current difficulty.

## TCP API

It will accept incoming TCP connections. The client's first message should be like this:

+ 13 bytes: my ID
+ signed
    + 16 bytes: 6b 87 4c de cc f0 28 b3 7c 4e de ee 15 ca 92 93
    + 16 bytes: auth code

Then the client should just listen on the connection. The server will post any messages that it receives or has received from other users down this connection.

## HTTP API

/api

1. Publish keys

    Request:
	+ 0x01
    + 24 bytes: proof of work
    + 32 bytes: public signing key
    + 32 bytes: public encryption key

2. Get keys

    Request:
	+ 0x02
    + 13 bytes: the ID to look up

    Response:
	+ 1 byte: 0x00 if the keys don't exist, 0x01 if they do
    + 32 bytes: their public signing key
    + 32 bytes: their public encryption key


3. Get proof of work info

	Request:
	+ 0x03

    Response:
    + 1 byte: difficulty
    + 16 bytes: random

4. Get auth code

	Request:
	+ 0x04

    Response:
    + 16 bytes: random

5. Upload blob

    Request (max 16KB):
	+ 0x05
    + 13 bytes: my ID
    + signed
        + 16 bytes: 0a cb 78 89 67 cf 64 19 2a dd 32 63 61 2d 10 18
        + 16 bytes: auth code
        + blob

	The server checks the signature, charges the account for the cost of the storage, and stores the nonce and the message under their hash.

6. Send message

	Request:
	+ 0x06
	+ 24 bytes: proof of work
	+ 24 bytes: nonce
	+ encrypted with recipient public key:
		+ 32 bytes: sha256 hash of encrypted blob on server
		+ 32 bytes: symmetric key of encrypted blob on server

	The recipient attempts to decrypt the message for each of its contacts, and accepts it if one of them succeeds.

7. Delete message

    Request:
	+ 0x07
    + 13 bytes: my ID
    + signed
        + 16 bytes: d3 ad fa b3 b4 67 41 bb 51 19 de d5 56 e5 9c 8e
        + 16 bytes: auth code
        + 32 bytes: SHA256 hash of the message to delete

8. Download blob
	
	Request:
	+ 0x08
	+ 32 bytes: sha256 hash of blob to download

	Response:
	+ 1 byte: 0x00 if there is no such blob, 0x01 if there is
	+ the blob if it exists

9. Get price

	Request:
	+ 0x09

	Response:
	+ 4 bytes: unsigned Little-Endian int: price in GBP^-4


# Client to client API

To send a message:

1. encode it and its blobs and pipe small chunks (~16KB) to a channel
	+ each chunk is prefixed with the hash of the previous chunk
2. for each chunk in the channel:
	a. symmetrically encrypt
	b. send to server
3. encode the hash and key of the first chunk of (3)
4. encrypt it with their public key and send it to them

Inside the encryption and chunking, the API is as follows:

1. Regular inbox message

    + 0x01
    + the encoded message

2. Acknowledgement

	+ 0x02
    + signed
        + 16 bytes: 77 0a 8b e7 5a 1a 9e 31 c5 97 5b 61 ec 47 16 ef
        + 8 bytes: Unix time received
        + 32 bytes: SHA-256 hash of message received

# Client cache

blobs/
	A flat folder full of blobs, named by their sha256 hash.
messages/
	A flat directory of sqlite databases, one per message, each with an integer name. Each has one table, called 'diffs':
		+ insertion (text)
		+ start (integer)
		+ end (integer)
		+ hash (blob)
		+ last_hash (blob)
database
	+ sent
		- message
		- hash
		- time
	+ received
		- message
		- user
		- hash
		- time
	+ whitelist
		- user
	+ public_keys
		- user
		- sign
		- encrypt
	+ acknowledgements
		- message
		- from
		- time
		- hash
		- signature
myKeys
	A binary file containing my crypto keys.
iota
	An 8-byte file containing an 64-bit uint, which is used to generate unique IDs.

# Pricing

There is a small, fixed charge for each blob upload.

The server keeps two tables in its accounting database:

1. Blob uploads
	+ user ID
	+ signed confirmation from user, including the price at the time
	+ timestamp
2. Payments (obtained from payments provider API)
	+ userId
	+ signed payment confirmation from provider

When someone uploads a new blob, the server queries these tables to calculate the user's balance. If the balance is high enough, the blob is accepted and recorded.

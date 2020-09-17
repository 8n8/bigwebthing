# Overview

*Note that this project is in rapid progress, but is not working yet.*

BigWebThing is intended to provide a combined and improved solution to the problem of sharing messages and programs conveniently and securely.

User data is kept on their own machine, and never sent out unencrypted. Users' private crypto keys never leave their own machine.

Users can write programs and send them to each other safely and run them safely.

There is a message-passing server for sharing data between people.

The cost of running the server is met by users paying for the resources they use, as they use them.

It operates on a strict whitelist-only policy, so you only receive messages from people on your whitelist.

# Plan

The project is being constructed in stages. It's a big project, but the aim is that each stage adds a single feature, and that it is usable after each stage. The stages are:

1. DOING minimal command-line sharer for small blobs
2. TODO add encryption
3. TODO add support for large blobs
4. TODO add support for directories
5. TODO make message editor
6. TODO add search for large directories of messages
7. TODO make a pretty GUI incorporating all the elements

# Program structure

These are the different parts of the program:

1. (Haskell) The client backend. This runs on the client's computer and communicates with the server.

2. (Go) The server. This acts as a route between clients. Users need to inform the server of their whitelists so that it can reject spam for them.

# Usage

cat /path/to/file | send friend

There is a file of contacts at

# Server API

The server provides a TCP server on port 11453.

## Proof of work

Some API routes are protected by a proof of work problem. To create a proof of work token, the user must download some unique bytes and a difficulty value from the server, and find some more unique bytes that will create a 32-byte slow hash with all the bytes greater than the difficulty. The 'slow' hash should not be that slow. It should be slow enough that any implementation details apart from hashing are amortised. So a proof of work token is like this:

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
        <= 15987 bytes: the message
    New username (AUTH)
        1 byte: 1
        8 bytes: username
    Proof of work info
        1 byte: 2
		1 byte: difficulty
		16 bytes: random
    Price (AUTH)
        1 byte: 3
        4 bytes: monthly price in GBP^(-2)
    Acknowledgement
        1 byte: 4
        4 bytes: acknowledgement code

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
	Send message (AUTH)
		1 byte: 3
        4 bytes: acknowledgement code
		8 bytes: recipient username
		<= 15987 bytes: message
	Delete message (AUTH)
		1 byte: 4
		32 bytes: message hash
	Get price (AUTH)
		1 byte: 5
    Upload contacts (AUTH)
        1 byte: 6
        sequence of 8-byte usernames

# Server cache

blobs
    A key-value store of binaries.

memCache
    A binary file containing a dump of the in-memory cache.

log.txt
    Log messages for debugging.

# Pricing

There is a fixed monthly charge, paid in advance, but the first part of a month and full month is free. There is a generous usage allowance, that is intended to be high enough that non-abusive users will never reach it.

BigWebThing version 4
=====================

# Overview

BigWebThing is a system for constructing and publishing documents.

The main principle is that the server logs everything it can and publishes everything. Any privacy there is comes from the client, such as end-to-end encryption.

# User interface

It's a command-line app. The commands are:

    Update crypto

        $ bwt

    Get usage

        $ bwt help

    Get my ID

        $ bwt myid

    Read messages

        $ bwt read

    Write a message

        $ bwt write <recipient ID> "hi ..."

    Add contact

        $ bwt addcontact <contact ID>

    Make a dummy £1 payment to the server - TESTING ONLY

        $ bwt pay

# Client cache

File containing client's static Noise key pair
SQLITE database
	session secrets
		session ID
		secret
		public Noise key of account

# Server cache

File containing server's static Noise key pair.
SQLITE database
	KK1 messages
		48-byte message
		sender public Noise key
		recipient public Noise key
	KK2 messages
		72-byte message
		sender public Noise key
		recipient public Noise key
	Transport messages
		96-byte message
		sender public Noise key
		recipient public Noise key
	payments
		payer public key
		amount

# Server API

All requests to the server should be made with the Noise IK pattern, using the hard-coded server public static Noise key.

This API is a minimal version that just allows sending and reading messages. It will eventually allow the user to query anything on the server database.

to server
	2 bytes: size
	encrypted
		upload KK2 message
			1 byte: 0
			32 bytes: recipient ID
			72 bytes: message
			1 byte: 0 for final message, 1 for more
		download KK1 new messages to me
			1 byte: 1
		download KK2 messages to me
			1 byte: 2
		download transport messages to me
			1 byte: 3
		download KK1 messages from me
			1 byte: 4
		download KK2 messages from me
			1 byte: 5
		download transport messages from me
			1 byte: 6
from server
	2 bytes: size
	encrypted
		1 byte: not enough money
			1 byte: 0
		1 byte: no more messages
			1 byte: 1
		91 bytes: KK1 message
			1 byte: 2
			48 bytes: message
			32 bytes: their ID
		106 bytes: KK2 message
			1 byte: 3
			72 bytes: message
			32 bytes: their ID
		132 bytes: transport message
			1 byte: 4
			96 bytes: message
			32 bytes: their ID

# KK message encoding

49 bytes: KK1
	1 byte: 0
	48 bytes: KK1 with empty payload
73 bytes: KK2
	1 byte: 1
	24 bytes: start of corresponding KK1, used as a session ID
	48 bytes: KK2 with empty payload
97 bytes: KK transport
	1 byte: 2
	24 bytes: start of corresponding KK1, used as a session ID
	72 bytes: encrypted 56-byte message with 16 byte overhead

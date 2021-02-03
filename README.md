BigWebThing version 4
=====================

# Overview

BigWebThing is a system for constructing and publishing documents.

The main principle is that the server logs everything it can and publishes everything. Any privacy there is comes from measures such as encryption implemented on the client.

# User interface

It's a command-line app. The commands are:

    Update crypto

        $ bwt update

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

# Client cache

File containing client's static Noise key pair, and all the session secrets. A session secret is a random secret byte array, with a session ID. A session ID is the first few bytes of the Noise message that started the session.

# Server cache

File containing server's static Noise key pair.
SQLITE database
	messages uploads
		message ID
		sender public Noise key
		recipient public Noise key
		Noise KK message
		sender IP address
		timestamp of server receiving message
	payments
		payer public key
		amount

# Server API

All requests to the server should be made with the Noise IK pattern, using the hard-coded server public static Noise key.

This API is a minimal version that just allows sending and reading messages. It will eventually allow the user to query anything on the server database.

to server
	upload message
		1 byte:
		32 bytes: recipient ID
		KK message
	download messages to me
		1 byte:
	download messages from me
		1 byte:
from server
	not enough money
		1 byte:
	message to you
		1 byte:
		32 bytes: sender
		KK message
	message from you
		1 byte:
		KK message

# KK message encoding

49 bytes: KK1
	1 byte: 0
	48 bytes: KK1 with empty payload
73 bytes: KK2
	1 byte: 1
	24 bytes: start of corresponding KK1, used as a session ID
	48 bytes: KK2 with empty payload
65 bytes: KK transport
	1 byte: 2
	24 bytes: start of corresponding KK1, used as a session ID
	40 bytes: encrypted 24-byte message with 16 byte overhead

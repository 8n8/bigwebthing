BigWebThing version 2
=====================

# Overview

BigWebThing is for sending messages, via the command line.

There is a message-passing server for sharing data between people.

Access to the server is controlled by a list of public signing keys kept on the server.

# Program structure

These are the different parts of the program:

1. A command-line app that runs on the client.

6. The server. This acts as a route between clients. They upload and download messages.

# User interface

It's a command-line app. The commands are:

    Get usage

        $ bwt help

    Get my ID

        $ bwt myid

    Get messages

        $ bwt get

    Send a new message from STDIN

        $ bwt send <recipient ID>

    Add contact

        $ bwt addcontact <contact ID>

# Server API

The server runs a TCP server on port 53745.

All messages between client and server are encrypted with the Noise XX pattern.

Client to server
	Blob upload
		1 byte: 0
		24 bytes: blob ID
		blob
	Get messages
		1 byte: 1
	Message upload
		1 byte: 2
		message
	Get blob
		1 byte: 3
		24 bytes: blob ID

Server to client
	Message
		1 byte: 0
		message
	Blob
		1 byte: 1
		blob

# Encodings

## Message

One of
	49 bytes: KK1
		1 byte: 0
		48 bytes: crypto overhead
	49 bytes: KK2
		1 byte: 1
		48 bytes: crypto overhead
	41 bytes: KK3
		1 byte: 2
		16 bytes: crypto overhead
		24 bytes: encrypted blob ID

# Client cache

+ A file containing the client Noise static key pair.

+ A database table containing the contact list
	- contact ID

+ A database table containing the ephemeral key pairs
	- other party ID
	- secret key
	- public key

# Server cache

+ a file containing the messages

+ a file containing the access list, with one hex-encoded user public Noise key on each line

+ a file containing the server Noise static key pair

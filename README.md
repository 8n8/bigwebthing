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
	Message to someone
		1 byte: 0
		32 bytes: recipient ID
		<= 15967: the message
    Get message
        1 byte: 1

Server to client
    No messages
        1 byte: 0
	Message from someone
		1 byte: 1
		32 bytes: sender ID
		<= 15967: the message

# Encodings

## Messages between clients

One of:

    100 Noise KK1s
        1 byte: 0
        7200 bytes: 100 Noise KK1 messages
            72 bytes
                48 bytes: Noise KK packet 1
                24 bytes: session ID
    100 Noise KK2s
        1 byte: 1
        7200 bytes: 100 Noise KK2 messages
            72 bytes
                48 bytes: Noise XK2 with empty payload
                24 bytes: session ID
    Noise KK transport
        1 byte: 2
        116 bytes: encrypted payload
        24 bytes: session ID

# Client cache

+ A file containing the client Noise static key pair.

+ A file containing the contact list. Each line is a Base64-encoded public static Noise key.

+ A database table containing the session states
	- session ID
	- other party ID
	- tx or rx
	- Noise HandshakeState / Cipherstate

# Server cache

+ a database table containing inboxes:
    - sender ID
    - recipient ID
	- message

+ a file containing the access list, with one hex-encoded user public Noise key on each line

+ a file containing the server Noise static key pair

BigWebThing version 2
=====================

# Overview

BigWebThing is for sending messages, via the command line.

There is a message-passing server for sharing data between people.

All the data, for the server and the user, is kept in a single append-only binary blob, hosted on the server.

Access to the server is controlled by a list of public signing keys kept on the server.

The program is written in C.

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

    Read messages

        $ bwt read

    Write a new message from STDIN

        $ bwt write <recipient ID>

    Add contact

        $ bwt addcontact <contact ID>

# Server API

The server runs a TCP server on port 53745.

All messages between client and server are encrypted with the Noise XX pattern.

Client to server
    Upload chain link
        1 byte: 0
        chain link
    Get chain
        1 byte: 1

Server to client
    Chain link
        1 byte: 0
        chain link
    End of chain
        1 byte: 1

# Encodings

Message IDs and secret keys are encoded as URL-safe Base64.

## Chain link

one of
    61 bytes: KK packet 1
        1 byte: 0
        12 bytes: session ID
        48 bytes: packet 1
    61 bytes: KK packet 2
        1 byte: 1
        12 bytes: session ID
        48 bytes: packet 2
    149 bytes: transport
        1 byte: 2
        12 bytes: session ID
        36 bytes: nonce and MAC
        100 bytes: encrypted payload

## Chain

    4 bytes: number of items in the chain (Little-Endian)
    sequence of chain links

# Client cache

+ A file containing the client Noise static key pair.

+ A database table containing the ephemeral Noise keys. Each row is:
    - sessionid
    - public
    - secret

+ A file containing the contact list. Each line is a hex-encoded public static Noise key.

# Server cache

+ a file containing the message chain
+ a file containing the access list, with one hex-encoded user public Noise key on each line
+ a file containing the server Noise static key pair

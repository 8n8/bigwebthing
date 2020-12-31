BigWebThing version 2
=====================

# Overview

BigWebThing is for sending messages, via the command line.

There is a message-passing server for sharing data between people.

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
    Augmented file
        1 byte: 0
        24 bytes: the file ID
        <= 16KB: the file
    Ping
        1 byte: 1
        32 bytes: recipient ID
        24 bytes: file ID
    Get file
        1 byte: 2
        8 bytes: file ID
    Get ping
        1 byte: 3

Server to client
    File
        <= 16KB: the file
    No such thing
        1 byte: 0
    Bad file upload
        1 byte: 1
    Ping
        24 bytes: blob ID

# Encodings

Message IDs and secret keys are encoded as URL-safe Base64.

The files on server are a sequence of these:

33 bytes: XX packet 1
    32 bytes: packet
81 bytes: XX packet 2
    80 bytes: packet
49 bytes: XX packet 3
    48 bytes: packet
138 bytes: transport message
    36 bytes: crypto overhead
    101 bytes: encrypted
        1 byte: message length
        100 bytes: message
... more transport messages

# Client cache

+ A file containing the client Noise static key pair.

+ A directory containing the ephemeral Noise keys. Each key pair is kept in a file named after the fileid.

+ A file containing the contact list. Each line is a hex-encoded public static Noise key.

# Server cache

+ a directory containing append-only blobs, named by random 24-byte user-chosen IDs. Any user can read and append to these.

+ a database table containing inboxes:
    - blob ID
    - recipient ID

+ a file containing the access list, with one hex-encoded user public Noise key on each line

+ a file containing the server Noise static key pair

BigWebThing version 2
=====================

# Overview

BigWebThing is for storing messages in the cloud conveniently, via the command line. Messages are symmetrically encrypted. Users can share the keys with each other instead of sharing the files.

There is a message-passing server for sharing data between people.

Access to the server is controlled by a list of public signing keys kept on the server.

# Program structure

These are the different parts of the program:

1. (C) A command-line app that runs on the client.

6. (C) The server. This acts as a route between clients. They upload and download messages.

# User interface

It's a command-line app. The commands are:

    Get usage

        $ bwt help

    Get my ID

        $ bwt myid

    Download a message to STDOUT:

        $ bwt get <message ID>

    Send a message from STDIN:

        $ bwt send

# Server API

The server runs a TCP server on port 53745.

All messages should be prefixed by a 2-byte length.

Client to server
    97 bytes: Signed auth code:
        1 byte: 0
        32 bytes: public signing key
        64 bytes: signed auth code
    Send message
        1 byte: 1
        24 bytes: message ID
        16 bytes: MAC
        12 bytes: nonce
        <= 100 bytes: encrypted message
            a sequence of these UTF-8 characters:
            1!2"3£4$5%6^7&8*9(0)-_+=abcdefghijklmnopqrstuvwxyz
            ABCDEFGHIJKLMNOPQRSTUVWXYZ|\<,>.?/ :;@'#~
    Get message
        1 byte: 2
        24 bytes: message ID

Server to client
    Auth code to sign
        1 byte: 0
        32 bytes: random
    Message
        1 byte: 1
        24 bytes: message ID
        16 bytes: MAC
        12 bytes: nonce
        <= 100 bytes: encrypted inbox message
    No such message
        1 byte: 2
        24 bytes: message ID

# Encodings

Message IDs and secret keys are encoded as URL-safe Base64.

# Client cache

A file called bigwebthingSECRETkey containing the secret signing key.

# Server cache

database
    messages
        message ID
        message

accessList.txt
    Contains one user public signing key per line, encoded in Base64.

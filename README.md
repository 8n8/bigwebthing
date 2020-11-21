*Note that this project is in rapid progress, with new things added most days, but is not working yet.*

# Overview

BigWebThing is for sending 100-byte messages conveniently, via the command line.

User data is kept on their own machine.

There is a message-passing server for sharing data between people.

Access to the server is controlled by a list of public signing keys kept on the server.

# Program structure

These are the different parts of the program:

1. (Haskell) A command-line app that runs on the client.

6. (Haskell) The server. This acts as a route between clients. They upload and download messages.

# User interface

It's a command-line app. The commands are:

    Get usage

        $ bwt help

    Get my user ID

        $ bwt myid

    Download a message to STDOUT:

        $ bwt get

    Send a message from STDIN:

        $ bwt send <recipient ID>

# Server API

The server runs a TCP server on port 53745.

All messages should be prefixed by a 1-byte length.

Client to server
    97 bytes: Signed auth code:
        1 byte:
        32 bytes: public signing key
        64 bytes: signed auth code
    Send message
        1 byte:
        payment details
        32 bytes: recipient public static signing key
        <= 100 bytes: the message
            a sequence of these UTF-8 characters:
            1!2"3£4$5%6^7&8*9(0)-_+=abcdefghijklmnopqrstuvwxyz
            ABCDEFGHIJKLMNOPQRSTUVWXYZ|\<,>.?/ :;@'#~
    Get message
        1 byte:
Server to client
    Auth code to sign
        1 byte:
        32 bytes: random
    Inbox message
        1 byte:
        8 bytes: POSIX timestamp of upload
        <= 100 bytes: inbox message
    No messages
        1 byte:

# Client to client

## API

There are several layers to the API, as follows.

## Crypto

All client-server traffic is encrypted with TLS.

# Encodings

## User ID encoding

The user ID is a Base64-encoded public signing key.

# Client cache

A file called .bigwebthingSECRETkey containing the secret signing key.

# Server cache

memCache
    A binary file containing a dump of the in-memory cache.

log.txt
    Log messages for debugging.

database
    messages
        sender
        recipient
        message

users.txt
    Contains one user public signing key per line, encoded in Base64.

BigWebThing version 3
=====================

# Overview

BigWebThing is for creating encrypted messages and sending them between people.

# User interface

It's a command-line app. It will optionally read two files in the current directory, called 'secret' and 'public'. The 'public' file should be sent to the recipients. The commands are:

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

    Syncronise with the server

        $ bwt sync

# Server API

Client to server
	Upload message
		1 byte: 0
		1 byte: number of messages
		sequence of 49-byte messages
	Download message set
		1 byte: 1
	Download accounts
		1 byte: 2
Server to client
	Message set
		1 byte: 0
		1 byte: number of messages
		sequence of 49-byte messages
	Accounts
		1 byte: 1
		4 bytes: payment amount
		4 bytes: number of spends
		sequence of spends
	Not enough money
		1 byte: 2

# Server cache

database
	+ Messages
		- upload time
		- payment ID
		- message (unique)
	+ Payments
		- payment ID (unique)
		- amount

# Client cache

Secrets file containing:
	+ client Noise static key pair.
	+ set of contact IDs
	+ sending session secrets
		- KK1
		- other party ID
		- secret bytes
	+ receiving session secrets

Public file containing sequence of KKs, where a KK is one of:
	49 bytes: KK1
		1 byte: 0
		48 bytes: KK1 with empty payload
	49 bytes: KK2
		1 byte: 1
		48 bytes: KK2 with empty payload
	49 bytes: KK transport
		1 byte: 2
		32 bytes: message
		16 bytes: crypto overhead

# Client process

1. parse messages in public file, into a sequence of KK messages
2. parse secret file
3. sort the KK messages into sessions, where each session is one of:
	sending
		KK1
		KK1 KK2
		KK1 KK2 Transport
	receiving
		KK1
		KK1 KK2
		KK1 Kk2 Transport

## Read

It picks out all the sessions, both sending and receiving, that have a transport message, deciphers the messages, and prints them out.

## Write

It chooses a sending session that has a complete round trip but no transport, and uses it to encrypt a new message. The new message is then appended to the public file.

## Update

It counts the number of unused sending sessions for each contact, and tops them up to a threshold value.

BigWebThing version 2
=====================

# Overview

BigWebThing is for creating encrypted messages to send between people.

# User interface

It's a command-line app. It will optionally read two files in the current directory, called 'secret' and 'public'. The 'public' file should be sent to the recipients. The commands are:

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

# Client cache

Secrets file containing:
	+ client Noise static key pair.
	+ set of contact IDs
	+ map of session secrets
		key: KK1
		value:
			- other party ID
			- tx or rx
			- secret bytes

Public file containing:
	+ KK1 section, where each is a 48-byte KK1 with empty payload
	+ KK2 section, where each is a 48-byte KK2 with empty payload
	+ KK transport section, where each is 36 bytes
		- 20-byte message
		- 16-byte crypto overhead

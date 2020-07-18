It provides a sufficient but minimal solution to each of these data problems:

1. storing
2. sharing
3. creating
4. viewing
5. searching
6. automatic manipulation
7. spam messages

# Storing

A user's data is stored in a local cache, in IndexedDB in their browser.

# Sharing

There is a message-passing server for sharing data between people. It stores messages till they are collected.

The cost of the server is met by subscriptions. It is free to use the server to communicate with a subscriber, but only subscribers can communicate with non-subscribers.

## Message formats

## User fingerprint

A user fingerprint is the first 100 bits of a slow hash of the user's signing and
encryption public keys.

The user fingerprint length is calculated as follows:

It must be long enough that it takes too long for an adversary to generate keys that
have the same user ID. It should be as short as possible so that it is convenient for
people to read and share.

Suppose there are around 10^11 keys on the system and an adversary is trying to find
keys that have the same fingerprint as one on the system. The only thing they can
do is keep generating keys and testing if the ID matches one on the system.

Say there are 80 bits (~10^24) in a fingerprint, then it will take the adversary
about 10^24 / 10^11 = 10^13 attempts to find a duplicate.

If I use some slow hashing to generate the fingerprint so that it takes on the order
of a second for each try, then 10^13 attempts will take on the order of 100,000 years
of computer time.

This sounds on the margins of safety. Say if there are actually 10^12 keys on the server, and the adversary can actually do 1000 tries a second. Then it will only take 10 years of computer time, which is sort of getting close to attackable.

I could stick another 15 bits (~10^4) on the fingerprint, which would increase the
optimistic attack time to 100000 years. But this is at the cost of bugging my users some more.

In a Diceware-like system with a dictionary of about 10,000 words, a 80-bit user ID
is about 6 words, but with 95 bits it is 7 words.

6 words looks like:

applause goatskin remark boundless everyday surprise

7 words looks like:

extinct womanlike generous dense quilt morality patronage

I choose 6 words (80 bits).

### Crypto API

There is a TCP server that does all the crypto. The API is like this:

1. Encrypt message
+ 0x01
+ 32 bytes: public key of recipient
+ the message to encrypt
Response:
+ 0x01
+ 32 bytes: public key of recipient
+ sized unencrypted message
+ encrypted message

2. Decrypt message
+ 0x02
+ 32 bytes: public key of sender
+ the message to decrypt
Response
+ 0x02
+ either
    + 0x00: decryption failed
    + 32 bytes: public key of sender
    + the message to decrypt
  or
    + 0x01: decryption succeeded
    + 32 bytes: public key of sender
    + sized encrypted message
    + decrypted message

3 Sign message
+ 0x03
+ message to sign
Response:
+ 0x03
+ sized original message
+ signed message

4. Check signature
+ 0x04
+ 32 bytes: public key of sender
+ signature
Response:
+ 0x04
+ either
    + 0x00: bad signature
    + 32 bytes: public key of sender
    + signature
  or
    + 0x01: good signature
    + 32 bytes: public key of sender
    + sized signature
    + unsigned message

5. Get public keys
+ 0x05
Response:
+ 0x05
+ 32 bytes: public signing key
+ 32 bytes: public encryption key

### Proof of work

Free APIs are protected by a proof of work problem. To create a proof of work token, the user must download some unique bytes from the server, and find some more unique bytes that will create an Argon2id hash with all the bytes greater than the difficulty.  So a proof of work token is like this:

+ <16 unique bytes provided by the server>
+ <8 calculated by the client>

The server checks that the first part is indeed something that it recently gave out, then that the hash of the whole meets the current difficulty.

### Server API

Messages are sent to the server in HTTP requests. The server will not accept messages greater than 16KB.

The client can also make a websocket connection with the server, which the server will send any new messages through.

Some APIs are only accessible to certain users and must have an identity token as follows:
+ 10 bytes: ID of sender
+ 16 bytes: authentication code downloaded from the server earlier
+ 96 bytes: signature. This is the signed SHA256 hash of the message prepended to the authentication code above. To be clear it is signature(SHA256(route byte + message + authCode)).
So an identity token is 122 bytes long.

These are the types of messages that the server will accept:

1. (Free) Retrieve keys for ID
+ 0x01
+ 10 bytes: the ID to look up
The response is:
+ 32 bytes: their public signing key
+ 32 bytes: their public encryption key
+ 10 bytes: the ID of the owner of the keys

2. (Free) Get proof of work difficulty and key
+ 0x02
The response is:
+ 1 byte: how many of the bytes at the start of the proof of work must be zeros (the difficulty)
+ 16 bytes: unique, i.e. the server must never respond in the same way to this request

3. (Free) Get code for authentication
+ 0x03
The response is a unique 16 bytes, that is, the server must never respond in the same way to this request.

4. (Paid) Send message
+ 0x04
+ 122 bytes: identity token
+ 10 bytes: recipient id
+ message

5. (Free) Delete message from server
+ 0x05
+ 122 bytes: identity token
+ 32 bytes: SHA256 hash of the message

6. (Free) Whitelist someone
+ 0x06
+ 122 bytes: identity token
+ 24 bytes: proof of work
+ 10 bytes: name of person to whitelist

7. (Free) Remove someone from whitelist
+ 0x07
+ 122 bytes: identity token
+ 10 bytes: name of person to remove from whitelist

### Client API

Since the server only accepts messages of 16KB or less, messages are chunked up into chunks 15.5KB or less, allowing space for crypto overhead.

Each chunk is like this:
+ 1 byte: 0 if it is not chunked (i.e. is small enough to fit in one chunk) or
          1 if it is
+ either
    the whole of a small message

  or
    + 32 bytes: SHA-256 hash of the whole message
    + 4 bytes: counter (first chunk is numbered 0) (Little-Endian)
    + the chunk

Then the chunk gets encrypted as follows:
+ 24-bytes: nonce
+ encrypted bytes calculated with nacl box.Seal

Then it gets sent using the Send message route in the server API.

# Programs

A program can:

1. Display a document.

2. Read user input.

# Spam

Each user has a whitelist of people they will accept messages from. Messages from anyone else are rejected unless they have a valid one-time code. Connections are started by somone sending a one-time code to someone else, by some existing method of communication, such as email. This code is used to authenticate the first message.

# Software components

1. Message-passing server. Messages are accepted if they are to or from subscribers. It deletes messages when they have been read.

2. Javascript client. It has an inbox categorized by program, and a set of programs. The main view is a list of programs and a box to search for them. Clicking on a program launches it. There is a built-in programming language interpreter for running the programs - probably an editor and tooling all built in too.

# Security

Each user has a pair of keys for encryption and signing which are generated from a password known only by the user. This means that data is only accessible in unencrypted form by the user, but has the dowside that if the user loses their password and their local data then they can't recover it.

Key pairs are changed by sending the new keys to everyone on the whitelist, signed by the old signing key.

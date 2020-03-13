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

### Proof of work

Free APIs are protected by a proof of work problem. To create a proof of work token, the user must download some unique bytes from the server, and find some more unique bytes that will create an sha512 hash with a number of the first bytes as zeros.  So a proof of work token is like this:

+ <8 unique bytes provided by the server>
+ <8 calculated by the client>

The server checks that the first part is indeed something that it recently gave out, then that the hash of the whole meets the current difficulty, that is, that the required initial number of bytes are zeros.

### Server API

Messages are sent to the server in HTTP requests. The server will not accept messages greater than 16KB.

Some APIs are only accessible to certain users and must have an identity token as follows:
+ 32 bytes: public signing key of sender>
+ 8 bytes: authentication code downloaded from the server earlier>
+ 96 bytes: signature. This is the signed SHA256 hash of the message prepended to the authentication code above. To be clear it is signature(sha256hash(route + message + authCode)).>
So an identity token is 136 bytes long.

These are the types of messages that the server will accept:

1. (Free) Make a friendly name for a public signing key:
+ 0x01
+ 16 bytes: proof of work
+ 32 bytes: public signing key
Response is an 8 byte name.

2. (Free) Retrieve key for name
+ 0x02
+ 8 bytes: name - the name to look up
The response is the 32-byte public key attached to the name.

3. (Free) Get proof of work difficulty and key
+ 0x03
The response is:
+ 1 byte: how many of the bytes at the start of the proof of work must be zeros (the difficulty)
+ 8 bytes: unique, i.e. the server must never respond in the same way to this request

4. (Admin) Add a member
+ 0x04
+ 136 bytes: identity token for user 'admin'
+ 8 bytes: name of member to add

5. (Admin) Remove a member
+ 0x05
+ 136 bytes: identity token for admin user
+ 8 bytes: name of member to remove

6. (Free) Change the key attached to a friendly name
+ 0x06
+ 136 bytes: identity token (using the old key)
+ 32 bytes: new key

7. (Free) Get code for authentication
+ 0x07
The response is a unique 8 bytes, that is, the server must never respond in the same way to this request.

8. (Paid) Send message
+ 0x08
+ 136 bytes: identity token
+ 32 bytes: recipient public key
+ message

9. (Paid) Retrieve message
+ 0x09
+ 136 bytes: identity token
The response is:
+ 0x01 if there are messages or 0x00 if there aren't
+ the message - as in (8) above, if there is one

10. (Free) Whitelist someone
+ 0x0A
+ 136 bytes: identity token
+ 16 bytes: proof of work
+ 8 bytes: name of person to whitelist

11. (Free) Remove someone from whitelist
+ 0x0B
+ 136 bytes: identity token
+ 8 bytes: name of person to remove from whitelist

### Client API

Messages are encrypted and decrypted on the client, so that the server can't read them.

The encrypted blob must be no more than 16KB long. Before encryption and encoding it is one of:

1. a new public encryption key
2. a new public signing key
3. a chunk of a program
4. a chunk of a document

A chunk of a program or document contains:

1. the cryptographic hash of the whole
2. the offset: 0 for the first chunk, 1 for the second, and so on
3. whether the chunk is the final one
4. the chunk body

A document contains:

1. a body and
2. the name of the program that can open it.

A program contains:

1. the code
2. its name
3. a description
4. a version number

The version number is an integer, starting at 0. All versions must be able to read data created by previous versions.

# Programs

A program can:

1. Display a document.

3. Read and delete messages sent to it from other people.

3. Read user input. Text documents are displayed as editible text areas. A program can subscribe to be notified of any changes to the text area. A program can also prompt users for a file upload from the local file system.

5. Send messages to other people.

# Spam

Each user has a whitelist of people they will accept messages from. Messages from anyone else are rejected unless they have a valid one-time code. Connections are started by somone sending a one-time code to someone else, by some existing method of communication, such as email. This code is used to authenticate the first message.

# Software components

1. Message-passing server. Messages are accepted if they are to or from subscribers. It deletes messages when they have been read.

2. Javascript client. It has an inbox categorized by program, and a set of programs. The main view is a list of programs and a box to search for them. Clicking on a program launches it. There is a built-in programming language interpreter for running the programs - probably an editor and tooling all built in too.

# Security

Each user has a pair of keys for encryption and signing which are generated from a password known only by the user. This means that data is only accessible in unencrypted form by the user, but has the dowside that if the user loses their password and their local data then they can't recover it.

Key pairs are changed by sending the new keys to everyone on the whitelist, signed by the old signing key.

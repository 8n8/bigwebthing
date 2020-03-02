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

Free APIs are protected by a proof of work problem. To create a proof of work token, the user must download some unique bytes from the server, and find some more unique bytes that will create an sha256 hash with a number of the first bytes as zeros.  So a proof of work token is like this:

+ <16 unique bytes provided by the server>
+ <16 calculated by the client>

The server checks that the first part is indeed something that it recently gave out, then that the hash of the whole meets the current difficulty, that is, that the required initial number of bytes are zeros.

### Server API

Messages are sent to the server in HTTP requests. The server will not accept messages greater than 16KB.

Some APIs are only accessible to certain users and must have an identity token as follows:
+ <32 public signing key of sender>
+ <16 authentication code downloaded from the server earlier>
+ <96 signature. This is the signed SHA256 hash of the message prepended to the authentication code above. To be clear it is signature(sha256hash(message + authCode)).>
So an identity token is 144 bytes long.

These are the types of messages that the server will accept:

1. (Free) Make a friendly name for a public signing key:
+ <1 must be 0x01>
+ <32 proof of work>
+ <32 public signing key>
+ <name, a Utf-8 string of non-confusing characters, no more than 40>

2. (Free) Retrieve key for name
+ <1 must be 0x02>
+ <name - the name to look up>
The response is the 32-byte public key attached to the name.

3. (Free) Get proof of work difficulty and key
+ <1 must be 0x03>
The response is:
+ <1 how many of the bytes at the start of the proof of work must be zeros (the difficulty)>
+ <16 unique, i.e. the server must never respond in the same way to this request>

4. (Admin) Add a member
+ <1 must be 0x04>
+ <144 identity token for user 'admin'>
+ <name of member to add>

5. (Admin) Add a member
+ <1 must be 0x05>
+ <144 identity token for user 'admin'>
+ <name of member to add>

6. (Free) Change the key attached to a friendly name
+ <1 must be 0x06>
+ <144 identity token (using the old key)>
+ <32 new key>

7. (Free) Get code for authentication
+ <1 must be 0x07>
The response is a unique 16 bytes, that is, the server must never respond in the same way to this request.

8. (Paid) Send message
+ <1 must be 0x08>
+ <144 identity token>
+ <32 recipient public key>
+ <2 length of message>
+ <message>

9. (Paid) Retrieve message
+ <1 must be 0x09>
+ <144 identity token>
The response is:
+ <1 0x01 if there are messages or 0x00 if there aren't>
+ the message - as in (8) above

### Client API

The encrypted blob must be no more than 16KB long. Before encryption and encoding it is one of:

1. a new public encryption key
2. a request to be whitelisted, using a one-time code
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

The body of a document is either a binary blob or some text with links to other documents in it. A link is the cryptographic hash of the document linked to.

A program contains:

1. the code
2. its name
3. a description
4. a version number

The version number is an integer, starting at 0. All versions must be able to read data created by previous versions.

# Programs

A program can:

1. Display a document.

2. Access a local database.

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

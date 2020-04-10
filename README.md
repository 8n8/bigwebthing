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
+ 8 bytes: ID of sender
+ 8 bytes: authentication code downloaded from the server earlier
+ 96 bytes: signature. This is the signed SHA512[:32] hash of the message prepended to the authentication code above. To be clear it is signature(sha512hash[:32](route + message + authCode)).
So an identity token is 112 bytes long.

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
+ 112 bytes: identity token for user 'admin'
+ 8 bytes: name of member to add

5. (Admin) Remove a member
+ 0x05
+ 112 bytes: identity token for admin user
+ 8 bytes: name of member to remove

6. (Free) Change the key attached to a friendly name
+ 0x06
+ 112 bytes: identity token (using the old key)
+ 32 bytes: new key

7. (Free) Get code for authentication
+ 0x07
The response is a unique 8 bytes, that is, the server must never respond in the same way to this request.

8. (Paid) Send message
+ 0x08
+ 112 bytes: identity token
+ 8 bytes: recipient id
+ message

9. (Paid) Retrieve message
+ 0x09
+ 112 bytes: identity token
The response is:
+ 0x01 if there are messages or 0x00 if there aren't
+ the message - as in (8) above, if there is one

10. (Free) Whitelist someone
+ 0x0A
+ 112 bytes: identity token
+ 16 bytes: proof of work
+ 8 bytes: name of person to whitelist

11. (Free) Remove someone from whitelist
+ 0x0B
+ 112 bytes: identity token
+ 8 bytes: name of person to remove from whitelist

12. (Free) Upload encryption key
+ 0x0C
+ 8 bytes: uploader name
+ 96 bytes: signed public encryption key

13. (Free) Download public encryption key
+ 0x0D
+ 8 bytes: name of owner of key
The response is the 96-byte signed public encryption key.

### Client API

This is what happens to a message as it is sent and received:

1. It starts as a Utils.MsgOut in Elm. MsgOut is defined as follows:

   type MsgOut
       = MakeMyName
       | WhitelistSomeone Int
       | SendThis HumanMsg

   (The first two are not mentioned further in this list. This is
    about SendThis.)
   
   type alias HumanMsgHelp = 
       { to : Int
       , from : Int
       , code : String
       , version : Version
       }
   
   type alias Version =
       { description : String
       , userInput : String
       , author : Int
       }

2. MsgOut 'SendThis' is encoded to bytes as follows:
    + 0x02 indicator byte
    + 4 bytes: recipient ID as little-endian integer
    + the program code string
    + the program description string
    + the program user input string
    + 4 bytes: version author ID as little-endian integer

3. On the JS side, the message from (2) is parsed as follows:
    + the recipient is extracted from bytes 1 to 5
    + the document is extracted from bytes 5 to end

4. The document is chopped up into chunks. A chunk is like this:
    + 0x01 (or 0x00 for a receipt)
    + 4 bytes: chunk counter integer
    + 4 bytes: total chunks in message
    + 32 bytes: sha512[:32] of the whole document: to be clear, the version author ID, the program code and the program description
    + <=15kB: the rest of the chunk

5. Each chunk is encrypted and prefixed with a 24-byte nonce.

6. The encrypted chunk is bundled up as follows:
    + 0x08
    + 112 bytes: ID token
    + 8 bytes: the recipient ID
    + the encrypted nonce and chunk

7. The bundle is uploaded to the server, and downloaded by the other user. The server prepends it with a 1 or 0 byte depending on whether  there are any messages or not.

8. The first two bytes are sliced off, because they are indicators and not needed any more.

9. The chunk is decrypted

10. The decrypted chunk is parsed.

11. Assembled chunks are put in the inbox as follows:
    + 4 bytes: sender ID
    + a 'SendThis' message, except for the indicator byte and recipient ID

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

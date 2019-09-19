It provides a sufficient but minimal solution to each of the problems caused by data:

1. storing
2. sharing
3. creating
4. viewing
5. searching
6. automatic manipulation
7. spam

# Storing

A user's data is a set of immutable messages. Messages can be added to the set but not removed. Messages are stored:

1. on a cloud server
2. in a local cache (indexedDB)

If the two sets of messages are different then the local one is changed to match the cloud one.

The cost of the server is met by customer subscriptions. It is free for anyone to use the server to communicate with a subscriber, but only subscribers can communicate with non-subscribers.

## Data format

A message is a document and a program. In Haskell syntax, a document is:

```
data Document
    = TextD [TextWithLinks]
    | BinaryD ByteString

data TextWithLinks
    -- The text is a friendly display name for the link,
    -- and the ByteString is the 256-bit crytpographic
    -- hash of the content linked to.
    = LinkT Text ByteString
    | UnicodeT Text
```

The actions that a program can do are:

1. Display a document.

2. Add documents to its document set.

3. Read user input. Text documents are displayed as editible text areas. A program can subscribe to be notified of any changes to the text area. A program can also prompt users for a file upload from the local file system.

4. Call other programs. A program is a function that takes a set of documents as its input, and produces another set of documents as its output. A program can call any other program by its hash and use it - a bit like Unix pipes, but can't access its document set.

5. Create other programs.

6. Send messages to other people. A document must be marked with the hash of the program it is being sent to, as well as the key of the person who is receiving it. A program can be sent to a specific program on another person's computer, or just to the person.

# Sharing

By sending messages from one user to another.

# Spam

Each user has a whitelist of people they will accept messages from. Messages from anyone else are rejected unless they have a valid one-time code. To start communicating with a new user, I send them a one-time code by an existing channel, such as email, which they use the first time they message me. If I get a message from someone not on the whitelist who has a valid one-time code then I add them to my whitelist.

# Software components

1. Message-passing server. Messages are accepted if they are to or from subscribers. The server also keeps a copy of all messages.

2. Javascript client. It has an inbox categorized by program, and a set of programs. The main view is a list of programs and a box to search for them. Clicking on a program launches it. There is a built-in programming language interpreter for running the programs - probably an editor and tooling all built in too.

3. Public key server. A dead-simple public database containing usernames, public signing keys and public encryption keys. Anyone can upload a username + public signing key + public encryption key, and it will be accepted as long as the username is unique and is signed by the corresponding private signing key. There will probably be a fairly hefty proof of work required to upload, to discourage DDOS attacks. A user can change their keys whenever they want, but the new key must be signed by the old one. Encryption keys will get changed really often, to prevent compromise of a key leading to loads of old messages getting decrypted.

# Security

Each user has a pair of keys which are generated from a password only known by the user. This means that data is only accessible by the user, and is encrypted on the server.

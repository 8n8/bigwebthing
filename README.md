BigWebThing Spec

It provides a sufficient but minimal solution to each of the set of problems caused by data:

1. storing
2. sharing
3. creating
4. viewing
5. searching
6. automatic manipulation
7. spam


# Storing

Data is stored:

1. in a local cache (indexedDB)
2. on a backup server

The cost of the server is met by customer subscriptions. It is free for anyone to use the server to communicate with a subscriber, but only subscribers can communicate with non-subscribers.

## Data format

All a person's data is represented as a set of messages. A message can be either a document or a program. In Haskell syntax, a document is:

```
data Document
    = TextD [TextWithLinks]
    | BinaryD ByteString

data TxtWithLinks
    = LinkT ByteString -- A 256-bit cryptographic hash of another document.
    | UnicodeT Text
```

The IO actions that a program can do are:

1. Display a document.

2. Read user input. Text documents are displayed as editible text areas. Programs can subscribe to the document, so that on any change its contents are fed to them. Program can also prompt users for a file upload from the local file system.

3. Call other programs. A program is a function that takes a set of documents as its input, and produces another set of documents as its output. A program can call any other program by its hash and use it - a bit like Unix pipes, but can't access its document set.

4. Create other programs.

5. Send messages to other people. A message must be marked with the hash of the program it is being sent to, as well as the key of the person who is receiving it.

5. Make HTTP requests.

# Sharing

By sending messages from one user to another.

# Spam

Each user has a whitelist of people they will accept messages from. Messages from anyone else are rejected unless they have a valid one-time code. To add a new user to the system, I send them an email or something with a one-time code in it, which they use the first time they message me. If I get a message from someone not on the whitelist who has a valid one-time code then I add them to my whitelist.

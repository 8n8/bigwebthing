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

All a person's data is represented as a set of messages.  A message has two parts: a document and a computer program. In Haskell syntax, a document has the form:

```
data Document
    = TextD [TextWithLinks]
    | BinaryD ByteString

data TxtWithLinks
    = LinkT ByteString -- A 256-bit cryptographic hash.
    | UnicodeT Text
```

The IO actions that a program can do are:

1. Display a document.

2. Read user input. Text documents are displayed as editible text areas. Programs can subscribe to the document, so that on any change its contents are fed to them.

3. Call other programs. A program is a function that takes its document as input, and produces another document as its output. A program can call any other program by its hash and use it - a bit like Unix pipes, but can't access its document.

4. Create other programs.

5. Make HTTP requests.

# Sharing

By sending messages from one user to another.

# Spam

Options for dealing with spam messages:

1.

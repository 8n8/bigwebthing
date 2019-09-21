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

In Haskell syntax, a message is:
```
data Message = Message
    { author :: Text
    , recipient :: Text
    -- If the recipient does not have you on their whitelist, this
    -- field is set to code they gave you when they contacted you.
    , youWantMeCode :: Maybe Text
    , encryptedBody :: ByteString
    , nonce :: ByteString
    }
```

This is what is seen by the server, when the message has been encrypted. The 'encryptedBody' field must be exactly 16KB long. The encrypted field is an encrypted document. In Haskell syntax, a document is:
```
data Document = Document
    { body :: DocumentBody
    , programName :: Text -- The name of the program that can use this document.
    }

type DocumentBody
    = TextD [TextWithLinks]
    | BinaryD ByteString

data TextWithLinks
    -- The text is a friendly display name for the link,
    -- and the ByteString is the 256-bit crytpographic
    -- hash of the content linked to.
    = LinkT Text ByteString
    | UnicodeT Text
```

## Programs

The actions that a program can do are:

1. Display a document.

2. Add documents to its document set.

3. Read user input. Text documents are displayed as editible text areas. A program can subscribe to be notified of any changes to the text area. A program can also prompt users for a file upload from the local file system.

5. Send messages to other people. A document must be marked with the name of the program it is being sent to, as well as the name of the person who is receiving it, but programs are just sent to other people.

# Sharing

By sending messages from one user to another.

# Spam

Each user has a whitelist of people they will accept messages from. Messages from anyone else are rejected unless they have a valid one-time code. To start communicating with a new user, I send them a one-time code by an existing channel, such as email, which they use the first time they message me. If I get a message from someone not on the whitelist who has a valid one-time code then I add them to my whitelist.

# Software components

1. Message-passing server. Messages are accepted if they are to or from subscribers. The server also keeps a copy of all messages.

2. Public key server. A public database containing usernames, public signing keys and public encryption keys. Anyone can upload a username + public signing key + public encryption key, and it will be accepted as long as the username is unique and is signed by the corresponding private signing key. A user can change their keys whenever they want, but the new key must be signed by the old one. Encryption keys should be changed often for forward secrecy.

3. Program name server. Another simple public lookup database that links program names to programs.

4. Javascript client. It has an inbox categorized by program, and a set of programs. The main view is a list of programs and a box to search for them. Clicking on a program launches it. There is a built-in programming language interpreter for running the programs - probably an editor and tooling all built in too.

# Security

Each user has a pair of keys which are generated from a password only known by the user. This means that data is only accessible by the user, and is encrypted on the server.

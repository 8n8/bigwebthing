It provides a sufficient but minimal solution to each of these problems caused by data:

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

There are four different message types:

1. Document: a piece of user data, like a personal message.
2. Program: a computer program for processing and sending documents.
3. New public encryption key
4. New public signing key

Messages are encrypted and decrypted on users' machines, and can't be read by the server. To the server, all messages look like this (Haskell syntax):

```
data Message = Message
    { author :: PublicSigningKey
    , recipient :: PublicSigningKey
    , newKeyOrBody :: NewKeyOrBody
    }

data NewKeyOrBody
    = SigningKeyN SigningKey Signature
    | EncryptedBodyN EncryptedBody Nonce

newtype PublicSigningKey = SigningKey ByteString
newtype Signature = Signature ByteString
newtype EncryptedBody = EncryptedBody ByteString
newtype Nonce = Nonce ByteString
```

The 'EncryptedBody' field is an encrypted chunk or new encryption key or whitelist request, and must be no more than 16KB long. In Haskell syntax, it is:

```
data ChunkOrKey
    = EncryptionKeyC ByteString -- The new public key.
    | WhitelistMe Text -- A code sent by someone who invited me.
    | ProgramChunkC Chunk -- Part of a program.
    | DocumentChunkC Chunk

data Chunk = Chunk
    { hashOfWhole :: Hash -- Cryptographic hash of whole document / program.
    , offset :: Int -- 0 for the first chunk, 1 for the second, etc.
    , finalChunk :: Bool -- True if this is the last chunk of the document / program.
    , chunk :: ByteString
    }

newtype Hash = Hash ByteString
```

A document is:

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

A program is:

```
data Program = Program
    { code :: Text
    , name :: Text
    , description :: Text
    -- Increments with each new version. There are no major / minor versions, since
    -- all versions are expected to be able to read data produced by previous versions.
    -- However it is OK for earlier versions not to be able to read data produces by
    -- later versions.
    , version :: Int
    }
```

## Programs

The actions that a program can do are:

1. Display a document.

2. Add documents to its document set.

3. Read user input. Text documents are displayed as editible text areas. A program can subscribe to be notified of any changes to the text area. A program can also prompt users for a file upload from the local file system.

5. Send messages to other people.

# Spam

Each user has a whitelist of people they will accept messages from. Messages from anyone else are rejected unless they have a valid one-time code. To start communicating with a new user, I send them a one-time code by an existing channel, such as email, which they use the first time they message me. If I get a message from someone not on the whitelist who has a valid one-time code then I add them to my whitelist.

# Software components

1. Message-passing server. Messages are accepted if they are to or from subscribers. The server also keeps a copy of all messages.

2. Javascript client. It has an inbox categorized by program, and a set of programs. The main view is a list of programs and a box to search for them. Clicking on a program launches it. There is a built-in programming language interpreter for running the programs - probably an editor and tooling all built in too.

# Security

Each user has a pair of keys which are generated from a password only known by the user. This means that data is only accessible in unencrypted form by the user, but has the dowside that if the user loses their password then their data is lost for ever.

# BigWebThing client state machine

This is a description of the BigWebThing client as a state machine. It describes what the client does in response to each input.

## Startup

When the program starts, it reads the command-line arguments.

## Command-line arguments

When the program receives the command-line arguments it sets the "mode" field of the state.

If the mode is "update crypto", the output is a command to read the contents of the static keys file.

## New static keys

When the program receives newly generated static Noise keys, it sets the 'static keys' field of the state, writes the keys to the static keys file, and requests a server connection.

## Static keys file

When the program receives the result of reading the static keys file, it:

1. stores the keys in the state and connects to the server if the file is OK, or
2. requests that new static keys be created

## New server connection

If the connection attempt fails, then the program shows an error message to the user and ends.

If the connection attempt succeeds, then the "status" field of the state is set to "making server sessions secret", the "conn" field is set to the new connection, and a request is made for a new session secret.

## Read result

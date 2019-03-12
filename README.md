BigWebThing will be a tool for sharing and running programmable documents.

The project is in the early stages and there is nothing much working yet.

The current plan is that the project will be in three parts:

1. A simple message-passing server.

2. A local webserver on the user's machine that will serve up web apps to their browser. This will provide authenticated message-passing back to the app author. It will also provide controlled, permanent local storage.

3. A web app served up by the local webserver that will allow the user to find and share other web apps.

Item (1) is mostly in place now, though untested. The project started in Haskell but switched to Go mainly because good trustworthy crypto is hard to come by in Haskell. 

# Installation of the message-passing server in Ubuntu Linux - terminal commands

1. Clone this repository: ```git clone https://github.com/8n8/bigwebthing```
2. ```cd bigwebthing```
3. Install Go 1.12 using (these)[https://golang.org/doc/install#install] instructions, but set the GOPATH to /path/to/bigwebthing/go.
4. ```go install server```

# Running the program

5. ```cd go/bin```
6. ```./server```

This will (hopefully - it's not been tested yet) set up a websockets server on http://localhost:4000. Clients can then connect to this and ping messages back and forth to each other. It doesn't store the messages, so will fail if both sender and receiver are not connected at the same time.

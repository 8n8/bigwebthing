BigWebThing will be a programming language, interpreter and tooling for creating and sharing programmable documents.

The project is in the early stages, the current task being to write an initial specification.

# Installation (Ubuntu Linux 18.04)

First clone this repository and ```cd``` into it.

## Specification

To make a nice pdf document of the specification, do this:

1. Install Latex with ```sudo apt install texlive-full```.
2. Install [Trux](https://github.com/8n8/trux) (a compile-to-Latex language).
3. Make the pdf document with ```trux spec.tx```.

## Program

It is still in the early stages and does not work yet. To build it:

1. Install the Haskell Tool stack.
2. Build with ```stack build```.

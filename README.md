BigWebThing will be a programming language, interpreter and tooling for creating and sharing programmable documents.

The project is in the early stages.  Here is the current TODO list:

1. CURRENT Write as full a specification as possible.
2. TODO Research implementation options and write plan for the implementation.
3. TODO Update the specification if necessary.
3. TODO Implement.

# Installation (Ubuntu Linux 18.04)

At this stage, the project is only a work-in-progress specification. To make a nice pdf document of the specification, do this:

1. Install Latex with ```sudo apt install texlive-full```.
1. Install [Trux](https://github.com/8n8/trux) (a compile-to-Latex language).
2. Clone this repository and ```cd``` into it.
3. Make the pdf document with ```trux spec.tx```.

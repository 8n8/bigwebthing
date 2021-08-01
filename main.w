\documentclass{article}
\usepackage{listings}
\usepackage[hidelinks]{hyperref}
\usepackage{microtype}

\begin{document}
\section{Building}
BigWebThing uses the Nix package manager to handle build dependencies.  The nice thing about Nix is that it provides nix-shell, which gives a completely isolated and reproducible build environment. The installation instructions are at \url{https://nixos.org/download.html}.

The \texttt{nix-shell} command uses the contents of the file \texttt{shell.nix} to set up the development environment, as follows:
\begin{itemize}
\item Tie to a particlar hash of the Nix packages. Otherwise you just get the most recent set of packages, which might not be the one you tried out, so future builds might break. Bear in mind that I'm pretty sure this means that all the dependencies are byte-for-byte identical. This is good because the build is reproducible, but bad because security updates don't come through. I think there are tools to help with this, but I prefer to just manually update the hash now and then, and check the build still works.
\item The Elm build tools. (Elm is a compile-to-Javascript language that is used to generate the user interface code.)
\item \texttt{uglify-js} is used to minify the generated Elm code.
\item Go compiler and tools.
\item \texttt{golangci-lint} combines a lot of Go linters.
\item I can't remember what \texttt{pkg-config} does, but it is necessary.
\item \texttt{gtk3-x11} and \texttt{webkitgtk} are for Webview, which is used to display the web-based graphical user interface. I chose it because the web is in my view the best and most modern way to do graphical UI, and because I'm used to it.
\end{itemize}

@o generated/shell.nix @{@%
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/a165aeceda9f9741d15bc2488425daeb06c0707e.tar.gz") {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.elmPackages.elm
    pkgs.elmPackages.elm-test
    pkgs.elmPackages.elm-format
    pkgs.nodePackages.uglify-js
    pkgs.go
    pkgs.golangci-lint
    pkgs.pkg-config
    pkgs.gtk3-x11
    pkgs.webkitgtk
  ];
}
@|@}

The Bash script that builds the client does these steps:
\begin{enumerate}
\item test
\item format
\item build
\end{enumerate}
The first line is to tell the script to fail if any command fails.
@o generated/client/go/build.sh @{@%
go test &&
golangci-lint run &&
go install
@|@}
Run the script with the command \texttt{bash go/client/build.sh}.
\section{Resuming Noise sessions}
The state in BigWebThing is made up of encrypted / public messages, and the users' secrets. The plain-text data is generated from this when needed.

The messages that are sent between users are encrypted using the Noise Protocol. Several messages have to be exchanged before the session is secure enough to send a useful payload.

BigWebThing uses this implementation of Noise: \url{https://pkg.go.dev/github.com/flynn/noise}. I think the intended use is that both parties are online throughout the session, but in BigWebThing the sessions could last a long time, and people will probably go offline during them and come back. The problem is the contents of the handshake state are not accessible in this implementation, so how to resume sessions after a program restart?

The solutions I thought of are:

\begin{enumerate}
\item Fork the repository and capitalise all the fields in HandshakeState. Then I can access the fields for serialization to disk. This makes me uneasy because I am not using the library as the author intended, and they know more than me, as do the people who reviewed it.

\item Perhaps the unexported fields can be accessed with reflection? Same disadvantage as the previous item.

\item The library allows the user to specify the random number generator it uses, so I could use ChaCha and keep the seed, and then replay the sessions. I know ChaCha is a good CPRNG, so this should be OK, and it is part of the official API.

\item Use Haskell's Cacophony Noise library. This is deterministic so would allow session replay. But cross-compilation is tricky or impossible with Haskell.

\item Use libsignal-protocol-c. I think this would be hard to do right. For example, they don't provide default implementations of the cryptographic primitives, just some examples with OpenSSL in the tests. And the documentation is not complete. And I would either have to switch to C, or make bindings.
\end{enumerate}

Option 3 seems like it has the least difficulties.

A test for a CSPRNG in Go with the same API as crypto/rand is as follows. It initializes a random number generator using a secret seed, and then generates two 10-byte random arrays. If they are random, it is very unlikely that these are equal, so it fails if they are equal.

@d csprngTest @{@%
func TestCsprng(t *testing.T) {
	var secret [64]byte
	csprng, err := makeNewCsprng(secret[:])
	if err != nil {
		t.Errorf("couldn't make new CSPRNG: %s", err)
	}

	var r1 [10]byte
	n, err := csprng.Read(r1[:])
	if err != nil || n != 10 {
		t.Errorf("couldn't read from CSPRING")
	}

	var r2 [10]byte
	n, err = cspring.Read(r2[:])
	if err != nil || n != 10 {
		t.Errorf("couldn't read from CSPRING (2)")
	}

	if r1 == r2 {
		t.Errorf("CSPRNG generating identical values")
	}
}
@|@}

@o go/client/client_test.go @{@%
package main

import (
	"testing"
)

@<csprngTest@>
@|@}
\end{document}


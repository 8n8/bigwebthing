
{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/a165aeceda9f9741d15bc2488425daeb06c0707e.tar.gz") {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.nuweb
    pkgs.texlive.combined.scheme-small
    pkgs.go
    pkgs.golangci-lint
  ];
}

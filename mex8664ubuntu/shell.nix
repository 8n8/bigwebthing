{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/348bc5de8bca09c624f5c4975f538684da4713d2.tar.gz") {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.go
    pkgs.golangci-lint
  ];
}

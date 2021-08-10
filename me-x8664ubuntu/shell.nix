{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/348bc5de8bca09c624f5c4975f538684da4713d2.tar.gz") {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.python39
    pkgs.python39Packages.flake8
    pkgs.python39Packages.pytest
    pkgs.python39Packages.noiseprotocol
    pkgs.python39Packages.black
  ];
}

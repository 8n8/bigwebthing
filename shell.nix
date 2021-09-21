{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/74d017edb6717ad76d38edc02ad3210d4ad66b96.tar.gz") {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.gmp
    pkgs.haskellPackages.stack
    pkgs.python39
    pkgs.python39Packages.flake8
    pkgs.python39Packages.cryptography
    pkgs.python39Packages.pytest
    pkgs.python39Packages.black
  ];
}

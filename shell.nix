{ nixpkgs ? import <nixpkgs> {} }:
(nixpkgs.haskellPackages.callPackage (import ./default.nix) {}).env


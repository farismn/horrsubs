{ nixpkgs ? import <nixpkgs> {} }:

nixpkgs.haskellPackages.callCabal2nix "rafflesia" ../.. {}
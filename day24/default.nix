{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.developPackage {
  root = ./.;
  source-overrides = { common = ../common; };
}


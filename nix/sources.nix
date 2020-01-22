# nix-build -E \
#  'with import <nixpkgs> { overlays = [(import ./.)];}; pkgs.bs-platform'
{ pkgs ? import <nixpkgs> {}, ocamlVersion ? "4_09" }:

let
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "7415c4f";
    sha256 = "1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
  };

  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/5e4c96c3efd20048fc04f306956e3e0c5c04f544.tar.gz;
    sha256 = "1jaa9i2b5a4x9ryjh7ckvsgikqv6aqbb2lnn6xxdh3nqjk4vhx4m";
  };

  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

  ocamlPackages = pkgs.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
    (pkgs.callPackage "${overlays}/ocaml" { });

in
  { inherit pkgs ocamlPackages gitignoreSource; }

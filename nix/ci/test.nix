{ ocamlVersion }:

let
  pkgs = import ../sources.nix { inherit ocamlVersion; };
in
  import ./.. {
    inherit pkgs ocamlVersion;
    doCheck = true;
  }

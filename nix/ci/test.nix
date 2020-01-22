{ ocamlVersion }:

let
  sources = import ../sources.nix { inherit ocamlVersion; };
in
  import ./.. {
    inherit sources ocamlVersion;
    doCheck = true;
  }

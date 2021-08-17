{ ocamlVersion ? "4_12" }:

let
  overlays = builtins.fetchTarball
    https://github.com/anmonteiro/nix-overlays/archive/5e425acf2.tar.gz;

in

import "${overlays}/sources.nix" {
  overlays = [
    (import overlays)
    (self: super: {
      ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
        (super.callPackage "${overlays}/ocaml" { });
    })
  ];
}

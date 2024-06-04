{ pkgs, stdenv, lib }:

let
  httpunPkgs = pkgs.recurseIntoAttrs (import ./nix { inherit pkgs; doCheck = false; });
  httpunDrvs = lib.filterAttrs (_: value: lib.isDerivation value) httpunPkgs;

in
with pkgs;

(mkShell {
  OCAMLRUNPARAM = "b";
  inputsFrom = lib.attrValues httpunDrvs;
  buildInputs = with ocamlPackages; [ merlin utop ];
}).overrideAttrs (o: {
  propagatedBuildInputs = lib.filter
    (drv:
      !(lib.hasAttr "pname" drv) ||
      drv.pname == null ||
      !(lib.any (name: name == drv.pname) (lib.attrNames httpunDrvs)))
    o.propagatedBuildInputs;
})

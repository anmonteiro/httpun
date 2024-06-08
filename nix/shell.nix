{ pkgs
, packages
, stdenv
, lib
, release-mode ? false
}:

let
  httpunDrvs = lib.filterAttrs (_: value: lib.isDerivation value) packages;

in
with pkgs;

(mkShell {
  OCAMLRUNPARAM = "b";
  nativeBuildInputs =
    lib.optionals release-mode [
      cacert
      curl
      ocamlPackages.dune-release
      git
    ];
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

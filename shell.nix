let
  pkgs = import ./nix/sources.nix { };
  inherit (pkgs) stdenv lib;
  httpafPkgs = pkgs.recurseIntoAttrs (import ./nix { inherit pkgs; doCheck = false; });
  httpafDrvs = lib.filterAttrs (_: value: lib.isDerivation value) httpafPkgs;

in
with pkgs;

(mkShell {
  OCAMLRUNPARAM = "b";
  inputsFrom = lib.attrValues httpafDrvs;
  buildInputs = with ocamlPackages; [ merlin utop ];
}).overrideAttrs (o: {
  propagatedBuildInputs = lib.filter
    (drv:
      !(lib.hasAttr "pname" drv) ||
      drv.pname == null ||
      !(lib.any (name: name == drv.pname) (lib.attrNames httpafDrvs)))
    o.propagatedBuildInputs;
})

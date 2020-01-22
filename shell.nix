let
  sources = import ./nix/sources.nix {};
  inherit (sources) pkgs ocamlPackages;
  inherit (pkgs) stdenv lib;
  httpafPkgs = pkgs.recurseIntoAttrs (import ./nix { inherit sources; doCheck = false; });
  httpafDrvs = lib.filterAttrs (_: value: lib.isDerivation value) httpafPkgs;

in
  with pkgs;

  (mkShell {
    inputsFrom = lib.attrValues httpafDrvs;
    buildInputs = with ocamlPackages; [ merlin ];
  }).overrideAttrs (o : {
    propagatedBuildInputs = lib.filter
      (drv: drv.pname == null || !(lib.any (name: name == drv.pname) (lib.attrNames httpafDrvs)))
      o.propagatedBuildInputs;
  })


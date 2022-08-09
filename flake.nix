{
  description = "HTTP/AF Nix Flake";

  inputs.nix-filter.url = "github:numtide/nix-filter";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:anmonteiro/nix-overlays?rev=5d3c6e7c37bf9487a88cc5466dd632282cea928c";
  # inputs.nixpkgs.url = "/home/anmonteiro/projects/nix-overlays";

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}";
      in
      rec {
        packages = pkgs.callPackage ./nix { nix-filter = nix-filter.lib; };
        defaultPackage = packages.httpaf;
        devShell = pkgs.callPackage ./shell.nix { };
      });
}

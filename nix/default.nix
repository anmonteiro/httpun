{ pkgs ? import ./sources.nix { inherit ocamlVersion; }
, ocamlVersion ? "4_10"
, doCheck ? true }:

let
  inherit (pkgs) lib stdenv ocamlPackages;

in

  with ocamlPackages;

  let
    buildHttpaf = args: buildDunePackage ({
      version = "0.6.5-dev";
      doCheck = doCheck;
      src = lib.gitignoreSource ./..;
    } // args);

# TODO: httpaf-async, httpaf-mirage
  in rec {
    httpaf = buildHttpaf {
      pname = "httpaf";
      buildInputs = [ alcotest hex yojson ];
      propagatedBuildInputs = [
        angstrom
        faraday
      ];
    };

  # These two don't have tests
  httpaf-lwt = buildHttpaf {
    pname = "httpaf-lwt";
    doCheck = false;
    propagatedBuildInputs = [ gluten-lwt httpaf lwt4 ];
  };

  httpaf-lwt-unix = buildHttpaf {
    pname = "httpaf-lwt-unix";
    doCheck = false;
    propagatedBuildInputs = [
      gluten-lwt-unix
      httpaf-lwt
      faraday-lwt-unix
      lwt_ssl
    ];
  };
}

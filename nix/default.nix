{ pkgs ? import ./sources.nix { inherit ocamlVersion; }
, ocamlVersion ? "4_11"
, doCheck ? true
}:

let
  inherit (pkgs) lib stdenv ocamlPackages;

in

with ocamlPackages;

let
  genSrc = { dirs, files }: lib.filterGitSource {
    src = ./..;
    inherit dirs;
    files = files ++ [ "dune-project" ];
  };
  buildHttpaf = args: buildDunePackage ({
    version = "0.6.5-dev";
    doCheck = doCheck;
  } // args);

in
rec {

  httpaf = buildHttpaf {
    pname = "httpaf";
    src = genSrc {
      dirs = [ "lib" "lib_test" ];
      files = [ "httpaf.opam" ];
    };
    buildInputs = [ alcotest hex yojson ];
    propagatedBuildInputs = [
      angstrom
      faraday
    ];
  };

  # These two don't have tests
  httpaf-lwt = buildHttpaf {
    pname = "httpaf-lwt";
    src = genSrc {
      dirs = [ "lwt" ];
      files = [ "httpaf-lwt.opam" ];
    };
    doCheck = false;
    propagatedBuildInputs = [ gluten-lwt httpaf lwt ];
  };

  httpaf-lwt-unix = buildHttpaf {
    pname = "httpaf-lwt-unix";
    src = genSrc {
      dirs = [ "lwt-unix" ];
      files = [ "httpaf-lwt-unix.opam" ];
    };
    doCheck = false;
    propagatedBuildInputs = [
      gluten-lwt-unix
      httpaf-lwt
      faraday-lwt-unix
      lwt_ssl
    ];
  };

  httpaf-async = buildHttpaf {
    pname = "httpaf-async";
    src = genSrc {
      dirs = [ "async" ];
      files = [ "httpaf-async.opam" ];
    };
    doCheck = false;
    propagatedBuildInputs = [
      httpaf
      async
      gluten-async
      faraday-async
      async_ssl
    ];
  };

  httpaf-mirage = buildHttpaf {
    pname = "httpaf-mirage";
    src = genSrc {
      dirs = [ "mirage" ];
      files = [ "httpaf-mirage.opam" ];
    };
    doCheck = false;
    propagatedBuildInputs = [
      faraday-lwt
      conduit-mirage
      httpaf-lwt
      gluten-mirage
    ];
  };
}

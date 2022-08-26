{ nix-filter, lib, stdenv, ocamlPackages, doCheck ? true }:

with ocamlPackages;

let
  genSrc = { dirs, files }:
    with nix-filter; filter {
      root = ./..;
      include = [ "dune-project" ] ++ files ++ (builtins.map inDirectory dirs);
    };
  buildHttpaf = args: buildDunePackage ({
    version = "0.6.5-dev";
    doCheck = doCheck;
  } // args);

  httpafPkgs = rec {
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
  };

in

with httpafPkgs;

httpafPkgs // (if lib.versionOlder "5.0" ocaml.version then {
  httpaf-eio = buildHttpaf {
    pname = "httpaf-eio";
    src = genSrc {
      dirs = [ "eio" ];
      files = [ "httpaf-eio.opam" ];
    };

    propagatedBuildInputs = [
      httpaf
      gluten-eio
      eio
    ];
  };

} else { })

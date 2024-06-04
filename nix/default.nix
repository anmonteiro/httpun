{ nix-filter, lib, stdenv, ocamlPackages, doCheck ? true }:

with ocamlPackages;

let
  genSrc = { dirs, files }:
    with nix-filter; filter {
      root = ./..;
      include = [ "dune-project" ] ++ files ++ (builtins.map inDirectory dirs);
    };
  buildHttpun = args: buildDunePackage ({
    version = "0.6.5-dev";
    doCheck = doCheck;
  } // args);

  httpunPkgs = rec {
    httpun = buildHttpun {
      pname = "httpun";
      src = genSrc {
        dirs = [ "lib" "lib_test" ];
        files = [ "httpun.opam" ];
      };
      buildInputs = [ alcotest hex yojson ];
      propagatedBuildInputs = [
        angstrom
        faraday
      ];
    };

    # These two don't have tests
    httpun-lwt = buildHttpun {
      pname = "httpun-lwt";
      src = genSrc {
        dirs = [ "lwt" ];
        files = [ "httpun-lwt.opam" ];
      };
      doCheck = false;
      propagatedBuildInputs = [ gluten-lwt httpun lwt ];
    };

    httpun-lwt-unix = buildHttpun {
      pname = "httpun-lwt-unix";
      src = genSrc {
        dirs = [ "lwt-unix" ];
        files = [ "httpun-lwt-unix.opam" ];
      };
      doCheck = false;
      propagatedBuildInputs = [
        gluten-lwt-unix
        httpun-lwt
        faraday-lwt-unix
        lwt_ssl
      ];
    };

    httpun-async = buildHttpun {
      pname = "httpun-async";
      src = genSrc {
        dirs = [ "async" ];
        files = [ "httpun-async.opam" ];
      };
      doCheck = false;
      propagatedBuildInputs = [
        httpun
        async
        gluten-async
        faraday-async
        async_ssl
      ];
    };

    httpun-mirage = buildHttpun {
      pname = "httpun-mirage";
      src = genSrc {
        dirs = [ "mirage" ];
        files = [ "httpun-mirage.opam" ];
      };
      doCheck = false;
      propagatedBuildInputs = [
        faraday-lwt
        conduit-mirage
        httpun-lwt
        gluten-mirage
      ];
    };
  };

in

with httpunPkgs;

httpunPkgs // (if lib.versionOlder "5.0" ocaml.version then {
  httpun-eio = buildHttpun {
    pname = "httpun-eio";
    src = genSrc {
      dirs = [ "eio" ];
      files = [ "httpun-eio.opam" ];
    };

    propagatedBuildInputs = [
      httpun
      gluten-eio
      eio
    ];
  };

} else { })

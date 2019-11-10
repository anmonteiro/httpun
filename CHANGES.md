Unreleased
--------------

- httpaf-lwt-unix: replace the `dune` file (previously written in OCaml) with a
  `(select)` form to avoid depending on `ocamlfind`
  ([#18](https://github.com/anmonteiro/httpaf/pull/18))
- httpaf-mirage: depend on `mirage-conduit` instead of `conduit-mirage`,
  effectively placing a lower bound of OCaml 4.07 on httpaf-mirage
  ([#16](https://github.com/anmonteiro/httpaf/pull/16))
- httpaf-lwt, httpaf-lwt-unix, httpaf-mirage: deduplicate interface code via a
  common `Httpaf_lwt_intf` interface
  ([#13](https://github.com/anmonteiro/httpaf/pull/13))
- httpaf, httpaf-lwt, httpaf-lwt-unix, httpaf-async, httpaf-mirage: add support
  for persistent connections and pipelining in the client implementations
  ([#5](https://github.com/anmonteiro/httpaf/pull/5))
- httpaf, httpaf-lwt, httpaf-lwt-unix, httpaf-async, httpaf-mirage: add support
  for switching protocols, e.g. to a WebSocket connection, via a new function
  `Reqd.respond_with_upgrade`
  ([#8](https://github.com/anmonteiro/httpaf/pull/8))
- httpaf-lwt-unix: add support for HTTPS in the Lwt runtime, either via OpenSSL
  or `ocaml-tls` ([#2](https://github.com/anmonteiro/httpaf/pull/2))
- httpaf, httpaf-lwt, httpaf-lwt-unix, httpaf-mirage: Add a Mirage adapter
  ([#3](https://github.com/anmonteiro/httpaf/pull/3))
- Add an `esy.json` file ([#6](https://github.com/anmonteiro/httpaf/pull/6))
- httpaf: Catch parsing errors and hand them to `error_handler` in
  `Server_connection` ([#4](https://github.com/anmonteiro/httpaf/pull/4))
- httpaf-mirage: Adapt to Mirage 3.7 interfaces. `httpaf_mirage` now requires
  `conduit-mirage` >= 2.0.2 and `mirage-flow` >= 2.0.0
  ([#24](https://github.com/anmonteiro/httpaf/pull/24))
- httpaf: Shutdown the writer after closing a non chunk-encoded request body on
  the client ([#23](https://github.com/anmonteiro/httpaf/pull/23))

httpaf (upstream) 0.6.0
--------------

- Initial fork point
  [025f5bc](https://github.com/anmonteiro/httpaf/commit/025f5bc94a48a7aa1c2dee4fd6e5667c0268d2a7)

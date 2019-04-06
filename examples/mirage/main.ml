(* Generated by mirage configure -t unix (2019-03-11 12:42:42-00:00). *)

open Lwt.Infix
let return = Lwt.return
let run =
OS.Main.run

let _ = Printexc.record_backtrace true

module Conduit_mirage1 = Conduit_mirage.With_tcp(Tcpip_stack_socket)

module Unikernel1 = Unikernel.Make(Console_unix)(Pclock)
  (Httpaf_mirage.Server_with_conduit)

module Mirage_logs1 = Mirage_logs.Make(Pclock)

let tcpv4_socket11 = lazy (
  Tcpv4_socket.connect (Key_gen.socket ())
  )

let udpv4_socket11 = lazy (
  Udpv4_socket.connect (Key_gen.socket ())
  )

let stackv4_socket1 = lazy (
  let __udpv4_socket11 = Lazy.force udpv4_socket11 in
  let __tcpv4_socket11 = Lazy.force tcpv4_socket11 in
  __udpv4_socket11 >>= fun _udpv4_socket11 ->
  __tcpv4_socket11 >>= fun _tcpv4_socket11 ->
  Tcpip_stack_socket.connect (Key_gen.ips ()) _udpv4_socket11 _tcpv4_socket11
  )

let nocrypto1 = lazy (
  Nocrypto_entropy_lwt.initialize ()
  )

let tcp_conduit_connector1 = lazy (
  let __stackv4_socket1 = Lazy.force stackv4_socket1 in
  __stackv4_socket1 >>= fun _stackv4_socket1 ->
  Lwt.return (Conduit_mirage1.connect _stackv4_socket1)

  )

let conduit11 = lazy (
  let __nocrypto1 = Lazy.force nocrypto1 in
  let __tcp_conduit_connector1 = Lazy.force tcp_conduit_connector1 in
  __nocrypto1 >>= fun _nocrypto1 ->
  __tcp_conduit_connector1 >>= fun _tcp_conduit_connector1 ->
  Lwt.return Conduit_mirage.empty >>= _tcp_conduit_connector1 >>=
fun t -> Lwt.return t
  )

let argv_unix1 = lazy (
  Bootvar.argv ()
  )

let console_unix_01 = lazy (
  Console_unix.connect "0"
  )

let pclock1 = lazy (
  Pclock.connect ()
  )

let httpaf1 = lazy (
  let __conduit11 = Lazy.force conduit11 in
  __conduit11 >>= fun _conduit11 ->
  Httpaf_mirage.Server_with_conduit.connect _conduit11
  )

let key1 = lazy (
  let __argv_unix1 = Lazy.force argv_unix1 in
  __argv_unix1 >>= fun _argv_unix1 ->
  return (Functoria_runtime.with_argv (List.map fst Key_gen.runtime_keys) "httpaf_unikernel" _argv_unix1)
  )

let noop1 = lazy (
  return ()
  )

let f11 = lazy (
  let __console_unix_01 = Lazy.force console_unix_01 in
  let __pclock1 = Lazy.force pclock1 in
  let __httpaf1 = Lazy.force httpaf1 in
  __console_unix_01 >>= fun _console_unix_01 ->
  __pclock1 >>= fun _pclock1 ->
  __httpaf1 >>= fun _httpaf1 ->
  Unikernel1.start _console_unix_01 _pclock1 _httpaf1
  )

let mirage_logs1 = lazy (
  let __pclock1 = Lazy.force pclock1 in
  __pclock1 >>= fun _pclock1 ->
  let ring_size = None in
  let reporter = Mirage_logs1.create ?ring_size _pclock1 in
  Mirage_runtime.set_level ~default:Logs.Info (Key_gen.logs ());
  Mirage_logs1.set_reporter reporter;
  Lwt.return reporter
  )

let mirage1 = lazy (
  let __noop1 = Lazy.force noop1 in
  let __noop1 = Lazy.force noop1 in
  let __key1 = Lazy.force key1 in
  let __mirage_logs1 = Lazy.force mirage_logs1 in
  let __f11 = Lazy.force f11 in
  __noop1 >>= fun _noop1 ->
  __noop1 >>= fun _noop1 ->
  __key1 >>= fun _key1 ->
  __mirage_logs1 >>= fun _mirage_logs1 ->
  __f11 >>= fun _f11 ->
  Lwt.return_unit
  )

let () =
  let t =
  Lazy.force noop1 >>= fun _ ->
    Lazy.force noop1 >>= fun _ ->
    Lazy.force key1 >>= fun _ ->
    Lazy.force mirage_logs1 >>= fun _ ->
    Lazy.force mirage1
  in run t

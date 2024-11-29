(* curl -v -p -x http://localhost:8080 http://example.com *)
open Base
module Arg = Stdlib.Arg
open Httpun_eio
open Httpun

let error_handler (_ : Eio.Net.Sockaddr.stream) =
  Httpun_examples.Server.error_handler

let request_handler
      env
      ~sw
      ~u
      flow
      (_ : Eio.Net.Sockaddr.stream)
      { Gluten.reqd; _ }
  =
  match Reqd.request reqd with
  | { Request.meth = `CONNECT; headers; _ } ->
    Stdlib.Format.eprintf "x: %a@." Request.pp_hum (Reqd.request reqd);
    let host, port =
      let host_and_port = Headers.get_exn headers "host" in
      let[@ocaml.warning "-8"] [ host; port ] =
        String.split_on_chars ~on:[ ':' ] host_and_port
      in
      host, port
    in
    let () =
      (* todo: try/with *)
      let p, u' = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Eio.Net.with_tcp_connect
          (Eio.Stdenv.net env)
          ~host
          ~service:port
          (fun upstream ->
             Eio.Promise.resolve u' ();
             Stdlib.Format.eprintf
               "connected to upstream %s (port %s)@."
               host
               port;
             Eio.Fiber.both
               (fun () -> Eio.Flow.copy flow upstream)
               (fun () -> Eio.Flow.copy upstream flow);
             Eio.Promise.resolve_ok u ()));
      Eio.Promise.await p
    in
    Reqd.respond_with_string reqd (Response.create `OK) ""
  | _ ->
    let headers = Headers.of_list [ "connection", "close" ] in
    Reqd.respond_with_string
      reqd
      (Response.create ~headers `Method_not_allowed)
      ""

let log_connection_error ex =
  Eio.traceln "Uncaught exception handling client: %a" Fmt.exn ex

let main port =
  Eio_main.run (fun env ->
    let listen_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
    let network = Eio.Stdenv.net env in
    let handler ~u =
     fun ~sw client_addr socket ->
      let request_handler = request_handler env ~sw ~u socket in
      Server.create_connection_handler
        ~request_handler
        ~error_handler
        ~sw
        client_addr
        socket
    in
    Eio.Switch.run (fun sw ->
      let socket =
        Eio.Net.listen
          ~reuse_addr:true
          ~reuse_port:false
          ~backlog:5
          ~sw
          network
          listen_address
      in
      Stdio.printf "Listening on port %i and echoing POST requests.\n" port;
      Stdio.printf "To send a POST request, try one of the following\n\n";
      Stdio.printf
        "  echo \"Testing echo POST\" | dune exec examples/async/async_post.exe\n";
      Stdio.printf
        "  echo \"Testing echo POST\" | dune exec examples/lwt/lwt_post.exe\n";
      Stdio.printf
        "  echo \"Testing echo POST\" | curl -XPOST --data @- \
         http://localhost:%d\n\n\
         %!"
        port;
      let domain_mgr = Eio.Stdenv.domain_mgr env in
      let p, _ = Eio.Promise.create () in
      for _i = 1 to Stdlib.Domain.recommended_domain_count () do
        Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Domain_manager.run domain_mgr (fun () ->
            Eio.Switch.run (fun sw ->
              while true do
                Eio.Net.accept_fork
                  socket
                  ~sw
                  ~on_error:log_connection_error
                  (fun client_sock client_addr ->
                     let p, u = Eio.Promise.create () in
                     handler ~sw ~u client_addr client_sock;
                     Eio.Promise.await_exn p)
              done;
              `Stop_daemon)))
      done;
      Eio.Promise.await p))

let () =
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore
    "Echoes POST requests. Runs forever.";
  main !port

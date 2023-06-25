module Arg = Stdlib.Arg

open Httpaf

module Client = Httpaf_eio.Client

let () =
  Ssl.init ~thread_safe:true ()

let handler ~on_eof response response_body =
  match response with
  | { Response.status = `OK; _ } as response ->
    Format.fprintf Format.std_formatter "%a\n%!" Response.pp_hum response;
    let rec on_read _bs ~off:_ ~len:_ =
      (* Bigstringaf.substring ~off ~len bs |> print_string; *)
      Body.Reader.schedule_read response_body ~on_read ~on_eof
    in
    Body.Reader.schedule_read response_body ~on_read ~on_eof;
  | response ->
    Format.fprintf Format.err_formatter "%a\n%!" Response.pp_hum response;
    Stdlib.exit 124
;;

let main port host =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let addrs =
        let addrs =
          Eio_unix.run_in_systhread (fun () ->
              Unix.getaddrinfo
                host
                (string_of_int port)
                [ Unix.(AI_FAMILY PF_INET) ])
        in
        List.filter_map
          (fun (addr : Unix.addr_info) ->
            match addr.ai_addr with
            | Unix.ADDR_UNIX _ -> None
            | ADDR_INET (addr, port) -> Some (addr, port))
          addrs
      in
      let addr =
        let inet, port = List.hd addrs in
        `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port)
      in
      let socket = Eio.Net.connect ~sw (Eio.Stdenv.net env) addr in
      let ctx = Ssl.create_context (Ssl.SSLv23 [@ocaml.warning "-3"]) Ssl.Client_context in
      Ssl.disable_protocols ctx [ (Ssl.SSLv23 [@ocaml.warning "-3"]) ];
      Ssl.honor_cipher_order ctx;
      Ssl.set_context_alpn_protos ctx [ "h2" ];
      let ssl_ctx = Eio_ssl.Context.create ~ctx socket in
      let ssl_sock = Eio_ssl.Context.ssl_socket ssl_ctx in
      Ssl.set_client_SNI_hostname ssl_sock host;
      Ssl.set_hostflags ssl_sock [ No_partial_wildcards ];
      Ssl.set_host ssl_sock host;
      let ssl_sock = Eio_ssl.connect ssl_ctx in

      let headers = Headers.of_list [ "host", host ] in
      let connection = Client.create_connection ~sw (ssl_sock :> Eio.Flow.two_way) in
      let response_handler =
        handler ~on_eof:(fun () ->
          Stdlib.Format.eprintf "eof@.";
          Client.shutdown connection |> Eio.Promise.await)
      in
      let request_body =
        Client.request
          connection
          ~flush_headers_immediately:true
          ~error_handler:Httpaf_examples.Client.error_handler
          ~response_handler
          (Request.create ~headers `GET "/")
      in
      Body.Writer.close request_body));
;;

let () =
  let host = ref None in
  let port = ref 443 in
  Arg.parse
    ["-p", Set_int port, " Port number (80 by default)"]
    (fun host_argument -> host := Some host_argument)
    "lwt_get.exe [-p N] HOST";
  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in
  main !port host
;;

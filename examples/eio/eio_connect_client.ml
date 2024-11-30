module Arg = Stdlib.Arg
open Httpun

let http_handler ~on_eof response response_body =
  match response with
  | { Response.status = `OK; _ } as response ->
    Format.eprintf "response: %a@." Response.pp_hum response;
    let rec on_read bs ~off ~len =
      Bigstringaf.substring ~off ~len bs |> print_string;
      flush stdout;
      Body.Reader.schedule_read response_body ~on_read ~on_eof
    in
    Body.Reader.schedule_read response_body ~on_read ~on_eof
  | response ->
    Format.fprintf Format.err_formatter "%a\n%!" Response.pp_hum response;
    Stdlib.exit 124

let proxy_handler _env ~sw ~headers flow ~on_eof response _response_body =
  Format.eprintf "CONNECT response: %a@." Response.pp_hum response;
  match response with
  | { Response.status = `OK; _ } as response ->
    (* This means we can now communicate via any protocol on the socket since
       the server approved the tunnel.

       We'll be boring and use HTTP/1.1 again. *)
    let connection = Httpun_eio.Client.create_connection ~sw flow in
    let exit_cond = Eio.Condition.create () in
    Eio.Fiber.fork ~sw (fun () ->
      let response_handler =
        http_handler ~on_eof:(fun () ->
          Stdlib.Format.eprintf "http eof@.";
          Eio.Condition.broadcast exit_cond;
          on_eof ())
      in
      let request_body =
        Httpun_eio.Client.request
          ~flush_headers_immediately:true
          ~error_handler:Httpun_examples.Client.error_handler
          ~response_handler
          connection
          (Request.create ~headers `GET "/")
      in
      Body.Writer.close request_body);
    Eio.Condition.await_no_mutex exit_cond;
    Httpun_eio.Client.shutdown connection |> Eio.Promise.await
  | _response -> Stdlib.exit 124

let main port proxy_host =
  let real_host = "example.com:80" in
  Eio_main.run (fun _env ->
    Eio.Switch.run (fun sw ->
      let fd = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
      let addrs =
        Eio_unix.run_in_systhread (fun () ->
          Unix.getaddrinfo
            proxy_host
            (Int.to_string port)
            [ Unix.(AI_FAMILY PF_INET) ])
      in
      Eio_unix.run_in_systhread (fun () ->
        Unix.connect fd (List.hd addrs).ai_addr);
      let socket = Eio_unix.Net.import_socket_stream ~sw ~close_unix:true fd in
      let headers = Headers.of_list [ "host", real_host ] in
      let connection = Httpun_eio.Client.create_connection ~sw socket in

      let exit_cond = Eio.Condition.create () in
      Eio.Fiber.fork ~sw (fun () ->
        let response_handler =
         fun response response_body ->
          Eio.Fiber.fork ~sw @@ fun () ->
          proxy_handler
            _env
            ~sw
            socket
            ~headers
            ~on_eof:(fun () ->
              Stdlib.Format.eprintf "(connect) eof@.";
              Eio.Condition.broadcast exit_cond)
            response
            response_body
        in
        let request_body =
          Httpun_eio.Client.request
            ~flush_headers_immediately:true
            ~error_handler:Httpun_examples.Client.error_handler
            ~response_handler
            connection
            (Request.create ~headers `CONNECT real_host)
        in
        Body.Writer.close request_body;
        Eio.Condition.await_no_mutex exit_cond;

        Httpun_eio.Client.shutdown connection |> Eio.Promise.await)))

let () =
  let host = ref None in
  let port = ref 80 in
  Arg.parse
    [ "-p", Set_int port, " Port number (80 by default)" ]
    (fun host_argument -> host := Some host_argument)
    "lwt_get.exe [-p N] HOST";
  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in
  main !port host

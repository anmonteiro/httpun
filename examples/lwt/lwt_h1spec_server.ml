open Base
open Lwt.Infix
module Arg = Stdlib.Arg
module Buffer = Stdlib.Buffer
open Httpun
open Httpun_lwt_unix

let error_handler (_ : Unix.sockaddr) = Httpun_examples.Server.error_handler

let request_handler (_ : Unix.sockaddr) { Gluten.reqd; _ } =
  let request = Reqd.request reqd in
  let request_body = Reqd.request_body reqd in
  let body = Buffer.create 128 in
  let rec on_read buffer ~off ~len =
    Buffer.add_string body (Bigstringaf.substring ~off ~len buffer);
    Body.Reader.schedule_read request_body ~on_eof ~on_read
  and on_eof () =
    let response_headers =
      Headers.of_list [ "content-length", Int.to_string (Buffer.length body) ]
    in
    let response_headers =
      match Headers.get request.headers "content-type" with
      | None -> response_headers
      | Some value -> Headers.add response_headers "content-type" value
    in
    Reqd.respond_with_string
      reqd
      (Response.create ~headers:response_headers `OK)
      (Buffer.contents body)
  in
  Body.Reader.schedule_read request_body ~on_eof ~on_read

let main port =
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, port)) in
  Lwt.async (fun () ->
    Lwt_io.establish_server_with_client_socket
      listen_address
      (Server.create_connection_handler ~request_handler ~error_handler)
    >|= fun _server ->
    Stdio.printf "Listening on port %i for h1spec.\n%!" port);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever

let () =
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore
    "Echoes request bodies for h1spec. Runs forever.";
  main !port

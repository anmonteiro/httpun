open! Core
open Async

open Httpaf
open Httpaf_async

let main port host () =
  let where_to_connect = Tcp.Where_to_connect.of_host_and_port { host; port } in
  Tcp.connect_sock where_to_connect
  >>= fun socket ->
    let finished = Ivar.create () in
    let response_handler = Httpaf_examples.Client.print ~on_eof:(Ivar.fill finished) in
    let headers = Headers.of_list [ "host", host ] in
    Client.create_connection socket >>= fun connection ->
    let request_body =
      Client.request
        connection
        ~error_handler:Httpaf_examples.Client.error_handler
        ~response_handler
        (Request.create ~headers `GET "/")
    in
    Body.Writer.close request_body;
    Ivar.read finished
;;

let () =
  Command.async
    ~summary:"Start a hello world Async client"
    Command.Param.(
      map (both
          (flag "-p" (optional_with_default 80 int)
            ~doc:"int destination port")
          (anon ("host" %: string)))
        ~f:(fun (port, host) ->
              (fun () -> main port host ())))
  |> Command_unix.run

open! Core
open Async

open Httpaf
open Httpaf_async

let error_handler _ = assert false

let main port host () =
  let where_to_connect = Tcp.Where_to_connect.of_host_and_port { host; port } in
  Tcp.connect_sock where_to_connect
  >>= fun socket ->
    let finished = Ivar.create () in
    let response_handler = Httpaf_examples.Client.print ~on_eof:(Ivar.fill finished) in
    let request_headers =
      Request.create ~headers:(Headers.of_list [ "host", host ]) `GET "/"
    in
    Client.create_connection socket >>= fun connection ->
    let request_body =
      Client.request
        connection
        ~response_handler
        ~error_handler
        request_headers
    in
    let finished' = Ivar.create () in
    let response_handler' =
      Httpaf_examples.Client.print ~on_eof:(Ivar.fill finished')
    in
    let request_body' =
      Client.request
        connection
        ~response_handler:response_handler'
        ~error_handler
        request_headers
    in
    Body.close_writer request_body';
    Body.close_writer request_body;
    Async.Deferred.all_unit [Ivar.read finished; Ivar.read finished'] >>| fun () ->
      Client.shutdown connection
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
  |> Command.run

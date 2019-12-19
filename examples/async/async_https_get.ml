open Core
open Async

open Httpaf
open Httpaf_async

let response_handler finished response response_body =
  match response with
  | { Response.status = `OK; _ } ->
    let rec on_read bs ~off ~len =
      Bigstring.to_string ~pos:off ~len bs |> print_endline;
      Body.schedule_read response_body ~on_read ~on_eof
    and on_eof () = Ivar.fill finished () in
    Body.schedule_read response_body ~on_read ~on_eof;
  | response ->
    Format.fprintf Format.std_formatter "%a\n%!" Response.pp_hum response;
    Core.exit 1
;;

let error_handler _ = assert false

let main port host () =
  let where_to_connect = Tcp.Where_to_connect.of_host_and_port { host; port } in
  let finished = Ivar.create () in
  Tcp.connect_sock where_to_connect
  >>= fun socket ->
    Client.SSL.create_connection_with_default socket >>= fun conn ->
    let headers = Headers.of_list [ "host", host ] in
    let request_body =
      Client.SSL.request
        ~error_handler
        ~response_handler:(response_handler finished)
        conn
        (Request.create ~headers `GET "/")
    in
    Body.close_writer request_body;
    Ivar.read finished
;;

let () =
  Command.async_spec
    ~summary:"Start a hello world Async server"
    Command.Spec.(empty +>
      flag "-p" (optional_with_default 443 int)
        ~doc:"int destination port"
      +>
      flag "-h" (required string)
        ~doc:"string destination host"
    ) main
  |> Command.run

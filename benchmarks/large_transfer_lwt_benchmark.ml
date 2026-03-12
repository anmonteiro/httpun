open Base
open Httpun
open Httpun_lwt_unix
open Lwt.Infix

module Arg = Stdlib.Arg
module Printf = Stdlib.Printf

type measurement =
  { name : string
  ; bytes : int64
  ; seconds : float
  }

type writer_mode =
  | Copy
  | Schedule
  | Queue_schedule

let default_path =
  let home =
    match Sys.getenv "HOME" with
    | Some home -> home
    | None -> "."
  in
  Stdlib.Filename.concat home "Downloads/x.zip"

let await_flush writer =
  let finished, notify_finished = Lwt.wait () in
  Body.Writer.flush writer (function
    | `Written -> Lwt.wakeup_later notify_finished ()
    | `Closed ->
      Lwt.wakeup_later_exn
        notify_finished
        (Failure "connection closed while flushing body writer"));
  finished

let with_open_file path flags perm f =
  Lwt_unix.openfile path flags perm >>= fun fd ->
  Lwt.finalize (fun () -> f fd) (fun () -> Lwt_unix.close fd)

let make_config ~read_buffer_size ~body_buffer_size =
  { Httpun.Config.default with
    read_buffer_size
  ; request_body_buffer_size = body_buffer_size
  ; response_body_buffer_size = body_buffer_size
  }

let read_file_size path =
  let stats = Unix.LargeFile.stat path in
  stats.Unix.LargeFile.st_size

let writer_mode_to_string = function
  | Copy -> "copy"
  | Schedule -> "schedule"
  | Queue_schedule -> "queue-schedule"

let writer_mode_of_string = function
  | "copy" -> Copy
  | "schedule" -> Schedule
  | "queue-schedule" -> Queue_schedule
  | mode ->
    failwith
      (Printf.sprintf
         "unknown writer mode %S (expected copy|schedule|queue-schedule)"
         mode)

let write_chunk writer_mode writer buffer ~len =
  match writer_mode with
  | Copy -> Body.Writer.write_bigstring writer buffer ~off:0 ~len
  | Schedule -> Body.Writer.schedule_bigstring writer buffer ~off:0 ~len
  | Queue_schedule -> Body.Writer.schedule_bigstring writer buffer ~off:0 ~len

type slot =
  { buffer : Lwt_bytes.t
  ; mutable ready : unit Lwt.t
  }

let rec stream_fd_to_writer
          fd
          writer
          ~writer_mode
          ~buffer
          ~chunk_size
          ~written
  =
  Lwt_bytes.read fd buffer 0 chunk_size >>= function
  | 0 ->
    Body.Writer.close writer;
    Lwt.return written
  | len ->
    write_chunk writer_mode writer buffer ~len;
    await_flush writer >>= fun () ->
    stream_fd_to_writer
      fd
      writer
      ~writer_mode
      ~buffer
      ~chunk_size
      ~written:Int64.(written + of_int len)

let stream_fd_to_writer_queued
      fd
      writer
      ~buffer_count
      ~chunk_size
      ~written
  =
  let slots =
    Array.init buffer_count ~f:(fun _ ->
      { buffer = Lwt_bytes.create chunk_size; ready = Lwt.return_unit })
  in
  let rec loop slot_index written =
    let slot = Array.unsafe_get slots slot_index in
    slot.ready >>= fun () ->
    Lwt_bytes.read fd slot.buffer 0 chunk_size >>= function
    | 0 ->
      Lwt_list.iter_s (fun slot -> slot.ready) (Array.to_list slots) >>= fun () ->
      Body.Writer.close writer;
      Lwt.return written
    | len ->
      Body.Writer.schedule_bigstring writer slot.buffer ~off:0 ~len;
      let ready, notify_ready = Lwt.wait () in
      Body.Writer.flush writer (function
        | `Written -> Lwt.wakeup_later notify_ready ()
        | `Closed ->
          Lwt.wakeup_later_exn
            notify_ready
            (Failure "connection closed while flushing body writer"));
      slot.ready <- ready;
      loop
        (Int.rem (slot_index + 1) buffer_count)
        Int64.(written + of_int len)
  in
  loop 0 written

let rec consume_request_body reader bytes_seen on_finished =
  Body.Reader.schedule_read
    reader
    ~on_eof:(fun () -> on_finished !bytes_seen)
    ~on_read:(fun _buffer ~off:_ ~len ->
      bytes_seen := Int64.(!bytes_seen + of_int len);
      consume_request_body reader bytes_seen on_finished)

let error_handler _client_addr ?request:_ error start_response =
  let response_body = start_response Headers.empty in
  let message =
    match error with
    | `Bad_request -> "bad request"
    | `Bad_gateway -> "bad gateway"
    | `Internal_server_error -> "internal server error"
    | `Exn exn -> Exn.to_string exn
  in
  Body.Writer.write_string response_body message;
  Body.Writer.close response_body

let create_request_handler
      ~path
      ~chunk_size
      ~file_size
      ~writer_mode
      ~buffer_count
      _client_addr
      { Gluten.reqd; _ }
  =
  match Reqd.request reqd with
  | { Request.meth = `GET; target = "/download"; _ } ->
    let headers =
      Headers.of_list
        [ "content-length", Int64.to_string file_size
        ; "content-type", "application/octet-stream"
        ; "connection", "close"
        ]
    in
    let response_body =
      Reqd.respond_with_streaming reqd (Response.create ~headers `OK)
    in
    let buffer = Lwt_bytes.create chunk_size in
    Lwt.async (fun () ->
      with_open_file path [ Unix.O_RDONLY ] 0 (fun fd ->
        (match writer_mode with
        | Queue_schedule ->
          stream_fd_to_writer_queued
            fd
            response_body
            ~buffer_count
            ~chunk_size
            ~written:0L
        | Copy | Schedule ->
          stream_fd_to_writer
            fd
            response_body
            ~writer_mode
            ~buffer
            ~chunk_size
            ~written:0L)
        >|= ignore))
  | { Request.meth = `POST; target = "/upload"; _ } ->
    let request_body = Reqd.request_body reqd in
    let bytes_seen = ref 0L in
    consume_request_body request_body bytes_seen (fun received ->
      let headers =
        Headers.of_list
          [ "content-length", "0"
          ; "connection", "close"
          ; "x-bytes-received", Int64.to_string received
          ]
      in
      Reqd.respond_with_string reqd (Response.create ~headers `OK) "")
  | _ ->
    let headers =
      Headers.of_list [ "content-length", "0"; "connection", "close" ]
    in
    Reqd.respond_with_string reqd (Response.create ~headers `Not_found) ""

let start_server ~path ~chunk_size ~file_size ~writer_mode ~buffer_count ~config =
  let listen_socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.setsockopt listen_socket Unix.SO_REUSEADDR true;
  let listen_addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 0) in
  Lwt_unix.bind listen_socket listen_addr >>= fun () ->
  Lwt_unix.listen listen_socket 128;
  let request_handler =
    create_request_handler
      ~path
      ~chunk_size
      ~file_size
      ~writer_mode
      ~buffer_count
  in
  let connection_handler =
    Server.create_connection_handler ~config ~request_handler ~error_handler
  in
  let rec accept_loop () =
    Lwt.catch
      (fun () ->
        Lwt_unix.accept listen_socket >>= fun (client_socket, client_addr) ->
        Lwt.async (fun () -> connection_handler client_addr client_socket);
        accept_loop ())
      (function
        | Unix.Unix_error ((Unix.EBADF | Unix.EINVAL), _, _) -> Lwt.return_unit
        | exn -> Lwt.fail exn)
  in
  Lwt.async accept_loop;
  let port =
    match Lwt_unix.getsockname listen_socket with
    | Unix.ADDR_INET (_, port) -> port
    | Unix.ADDR_UNIX _ -> failwith "expected an INET listener"
  in
  let stop () = Lwt_unix.close listen_socket in
  Lwt.return (port, stop)

let with_connection ~config port f =
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sockaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Lwt_unix.connect socket sockaddr >>= fun () ->
  Lwt.finalize
    (fun () ->
      Client.create_connection ~config socket >>= fun connection -> f connection)
    (fun () ->
      Lwt.catch
        (fun () -> Lwt_unix.close socket)
        (fun _ -> Lwt.return_unit))

let run_download ~config ~port ~host ~expected_bytes =
  let started_at = Unix.gettimeofday () in
  let finished, notify_finished = Lwt.wait () in
  let error_handler error =
    let message =
      match error with
      | `Malformed_response err -> "malformed response: " ^ err
      | `Invalid_response_body_length _ -> "invalid response body length"
      | `Exn exn -> Exn.to_string exn
    in
    Lwt.wakeup_later_exn notify_finished (Failure message)
  in
  let response_handler response response_body =
    match response with
    | { Response.status = `OK; _ } ->
      let bytes_received = ref 0L in
      let rec on_read _buffer ~off:_ ~len =
        bytes_received := Int64.(!bytes_received + of_int len);
        Body.Reader.schedule_read response_body ~on_eof ~on_read
      and on_eof () =
        let bytes_received = !bytes_received in
        if Int64.(bytes_received <> expected_bytes)
        then
          Lwt.wakeup_later_exn
            notify_finished
            (Failure
               (Printf.sprintf
                  "downloaded %Ld bytes, expected %Ld"
                  bytes_received
                  expected_bytes))
        else Lwt.wakeup_later notify_finished bytes_received
      in
      Body.Reader.schedule_read response_body ~on_eof ~on_read
    | response ->
      Lwt.wakeup_later_exn
        notify_finished
        (Failure
           (Printf.sprintf
              "unexpected download response status: %d"
              (Status.to_code response.status)))
  in
  with_connection ~config port (fun connection ->
    let headers =
      Headers.of_list [ "host", host; "connection", "close" ]
    in
    let request_body =
      Client.request
        connection
        ~error_handler
        ~response_handler
        (Request.create ~headers `GET "/download")
    in
    Body.Writer.close request_body;
    finished)
  >|= fun bytes ->
  { name = "download"
  ; bytes
  ; seconds = Unix.gettimeofday () -. started_at
  }

let run_upload
      ~config
      ~path
      ~port
      ~host
      ~expected_bytes
      ~chunk_size
      ~writer_mode
      ~buffer_count
  =
  let started_at = Unix.gettimeofday () in
  let finished, notify_finished = Lwt.wait () in
  let error_handler error =
    let message =
      match error with
      | `Malformed_response err -> "malformed response: " ^ err
      | `Invalid_response_body_length _ -> "invalid response body length"
      | `Exn exn -> Exn.to_string exn
    in
    Lwt.wakeup_later_exn notify_finished (Failure message)
  in
  let response_handler response response_body =
    match response with
    | { Response.status = `OK; headers; _ } ->
      let received =
        match Headers.get headers "x-bytes-received" with
        | None -> 0L
        | Some received -> Int64.of_string received
      in
      let on_eof () =
        if Int64.(received <> expected_bytes)
        then
          Lwt.wakeup_later_exn
            notify_finished
            (Failure
               (Printf.sprintf
                  "server received %Ld bytes, expected %Ld"
                  received
                  expected_bytes))
        else Lwt.wakeup_later notify_finished received
      in
      Body.Reader.schedule_read response_body ~on_eof ~on_read:(fun _ ~off:_ ~len:_ -> ())
    | response ->
      Lwt.wakeup_later_exn
        notify_finished
        (Failure
           (Printf.sprintf
              "unexpected upload response status: %d"
              (Status.to_code response.status)))
  in
  with_connection ~config port (fun connection ->
    let headers =
      Headers.of_list
        [ "host", host
        ; "connection", "close"
        ; "content-length", Int64.to_string expected_bytes
        ; "content-type", "application/octet-stream"
        ]
    in
    let request_body =
      Client.request
        connection
        ~error_handler
        ~response_handler
        (Request.create ~headers `POST "/upload")
    in
    let buffer = Lwt_bytes.create chunk_size in
    let send_request_body =
      with_open_file path [ Unix.O_RDONLY ] 0 (fun fd ->
        match writer_mode with
        | Queue_schedule ->
          stream_fd_to_writer_queued
            fd
            request_body
            ~buffer_count
            ~chunk_size
            ~written:0L
        | Copy | Schedule ->
          stream_fd_to_writer
            fd
            request_body
            ~writer_mode
            ~buffer
            ~chunk_size
            ~written:0L)
    in
    Lwt.both send_request_body finished >|= fun (written, received) ->
    if Int64.(written <> expected_bytes)
    then
      failwith
        (Printf.sprintf
           "uploaded %Ld bytes from disk, expected %Ld"
           written
           expected_bytes);
    received)
  >|= fun bytes ->
  { name = "upload"
  ; bytes
  ; seconds = Unix.gettimeofday () -. started_at
  }

let format_throughput bytes seconds =
  let mib_per_second =
    Int64.to_float bytes /. (1024. *. 1024.) /. seconds
  in
  Printf.sprintf "%.2f MiB/s" mib_per_second

let print_measurement measurement =
  Printf.printf
    "%s: %Ld bytes in %.3fs (%s)\n%!"
    measurement.name
    measurement.bytes
    measurement.seconds
    (format_throughput measurement.bytes measurement.seconds)

let run path chunk_size writer_mode buffer_count =
  let file_size = read_file_size path in
  let config =
    make_config ~read_buffer_size:chunk_size ~body_buffer_size:chunk_size
  in
  Printf.printf
    "Benchmark file: %s\nsize: %Ld bytes\nchunk size: %d bytes\nwriter mode: \
     %s\nbuffer count: %d\n\
     %!"
    path
    file_size
    chunk_size
    (writer_mode_to_string writer_mode)
    buffer_count;
  start_server ~path ~chunk_size ~file_size ~writer_mode ~buffer_count ~config
  >>= fun (port, stop_server) ->
  let host = "127.0.0.1" in
  Lwt.finalize
    (fun () ->
      run_upload
        ~config
        ~path
        ~port
        ~host
        ~expected_bytes:file_size
        ~chunk_size
        ~writer_mode
        ~buffer_count
      >>= fun upload ->
      run_download ~config ~port ~host ~expected_bytes:file_size
      >>= fun download ->
      print_measurement upload;
      print_measurement download;
      Lwt.return_unit)
    stop_server

let () =
  let path = ref default_path in
  let chunk_size = ref (1024 * 1024) in
  let writer_mode = ref Copy in
  let buffer_count = ref 4 in
  Arg.parse
    [ "-f", Arg.Set_string path, " Path to the file to transfer"
    ; "-c", Arg.Set_int chunk_size, " Chunk size in bytes (default: 1048576)"
    ; ( "-m"
      , Arg.String (fun mode -> writer_mode := writer_mode_of_string mode)
      , " Writer mode: copy, schedule, or queue-schedule (default: copy)" )
    ; ( "-b"
      , Arg.Set_int buffer_count
      , " Number of rotating buffers for queue-schedule mode (default: 4)" )
    ]
    ignore
    "large_transfer_lwt_benchmark.exe";
  if not (Stdlib.Sys.file_exists !path)
  then failwith (Printf.sprintf "file does not exist: %s" !path);
  if !chunk_size <= 0 then failwith "chunk size must be positive";
  if !buffer_count <= 0 then failwith "buffer count must be positive";
  Lwt_main.run (run !path !chunk_size !writer_mode !buffer_count)

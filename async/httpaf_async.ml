open Core
open Async


let read fd buffer =
  let badfd fd = failwithf "read got back fd: %s" (Fd.to_string fd) () in
  let rec finish fd buffer result =
    let open Unix.Error in
    match result with
    | `Already_closed | `Ok 0 -> return `Eof
    | `Ok n                   -> return (`Ok n)
    | `Error (Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
      begin Fd.ready_to fd `Read
      >>= function
        | `Bad_fd -> badfd fd
        | `Closed -> return `Eof
        | `Ready  -> go fd buffer
      end
    | `Error (Unix.Unix_error (EBADF, _, _)) ->
      badfd fd
    | `Error exn ->
      Deferred.don't_wait_for (Fd.close fd);
      raise exn
  and go fd buffer  =
    if Fd.supports_nonblock fd then
      finish fd buffer
        (Fd.syscall fd ~nonblocking:true
          (fun file_descr ->
            Bigstring_buffer.put buffer ~f:(fun bigstring ~off ~len ->
              Unix.Syscall_result.Int.ok_or_unix_error_exn ~syscall_name:"read"
                (Bigstring.read_assume_fd_is_nonblocking file_descr bigstring ~pos:off ~len))))
    else
      Fd.syscall_in_thread fd ~name:"read"
        (fun file_descr ->
          Bigstring_buffer.put buffer ~f:(fun bigstring ~off ~len ->
            Bigstring.read file_descr bigstring ~pos:off ~len))
      >>= fun result -> finish fd buffer result
  in
  go fd buffer

open Httpaf

let close_read socket =
  let fd = Socket.fd socket in
  if not (Fd.is_closed fd)
  then Socket.shutdown socket `Receive;
  Deferred.return ()

let close_write socket =
  let fd = Socket.fd socket in
  if not (Fd.is_closed fd)
  then Socket.shutdown socket `Send;
  Deferred.return ()

module Server = struct
  let start_read_write_loops
    ?(readf=read)
    ?(writev=Faraday_async.writev_of_fd)
    ?(close_read=close_read)
    ?(close_write=close_write)
    ~config
    ~socket
    connection =
    let fd     = Socket.fd socket in
    let writev = writev fd in
      let read_complete = Ivar.create () in
      let buffer = Bigstring_buffer.create config.Config.read_buffer_size in
      let rec reader_thread () =
        match Server_connection.next_read_operation connection with
        | `Read ->
          (* Log.Global.printf "read(%d)%!" (Fd.to_int_exn fd); *)
          read fd buffer
          >>> begin function
            | `Eof  ->
              Bigstring_buffer.get buffer ~f:(fun bigstring ~off ~len ->
                Server_connection.read_eof connection bigstring ~off ~len)
              |> ignore;
              reader_thread ()
            | `Ok _ ->
              Bigstring_buffer.get buffer ~f:(fun bigstring ~off ~len ->
                Server_connection.read connection bigstring ~off ~len)
              |> ignore;
              reader_thread ()
          end
        | `Yield  ->
          (* Log.Global.printf "read_yield(%d)%!" (Fd.to_int_exn fd); *)
          Server_connection.yield_reader connection reader_thread
        | `Upgrade ->
          Ivar.fill read_complete ();
        | `Close ->
          (* Log.Global.printf "read_close(%d)%!" (Fd.to_int_exn fd); *)
          Ivar.fill read_complete ();
          if not (Fd.is_closed fd)
          then Socket.shutdown socket `Receive
      in
      let write_complete = Ivar.create () in
      let rec writer_thread () =
        match Server_connection.next_write_operation connection with
        | `Write iovecs ->
          (* Log.Global.printf "write(%d)%!" (Fd.to_int_exn fd); *)
          writev iovecs >>> fun result ->
            Server_connection.report_write_result connection result;
            writer_thread ()
        | `Upgrade (iovecs, upgrade_handler) ->
          writev iovecs >>> fun result ->
            Server_connection.report_write_result connection result;
            upgrade_handler socket >>> Ivar.fill write_complete
        | `Yield ->
          (* Log.Global.printf "write_yield(%d)%!" (Fd.to_int_exn fd); *)
          Server_connection.yield_writer connection writer_thread;
        | `Close _ ->
          (* Log.Global.printf "write_close(%d)%!" (Fd.to_int_exn fd); *)
          Ivar.fill write_complete ();
          if not (Fd.is_closed fd)
          then Socket.shutdown socket `Send
      in
      let conn_monitor = Monitor.create () in
      Scheduler.within ~monitor:conn_monitor reader_thread;
      Scheduler.within ~monitor:conn_monitor writer_thread;
      Monitor.detach_and_iter_errors conn_monitor ~f:(fun exn ->
        Server_connection.report_exn connection exn);
      (* The Tcp module will close the file descriptor once this becomes determined. *)
      Deferred.all_unit
        [ Ivar.read read_complete
        ; Ivar.read write_complete ]

  let create_connection_handler ?(config=Config.default) ~request_handler ~error_handler =
    fun client_addr socket ->
      let request_handler = request_handler client_addr in
      let error_handler   = error_handler client_addr in
      let conn = Server_connection.create ~config ~error_handler request_handler in
      start_read_write_loops ~config ~socket conn

  module SSL = struct
    let create_connection_handler
      ?server
      ?certfile
      ?keyfile
      ?(config=Config.default)
      ~request_handler
      ~error_handler =
      fun client_addr socket ->
        let request_handler = request_handler client_addr in
        let error_handler   = error_handler client_addr in
        let conn = Server_connection.create ~config ~error_handler request_handler in
        Ssl_io.make_server ?server ?certfile ?keyfile socket >>= fun ssl ->
        let ssl_reader = Ssl_io.reader ssl in
        let ssl_writer = Ssl_io.writer ssl in
        let readf = Ssl_io.readf ssl_reader in
        let writev = Ssl_io.writev ssl_writer in
        let close_read = Ssl_io.close_read ssl_reader in
        let close_write = Ssl_io.close_write ssl_writer in
        start_read_write_loops
          ~config
          ~readf
          ~writev
          ~socket
          ~close_read
          ~close_write
          conn
  end
end

module Client = struct
  type t = Client_connection.t

  let start_read_write_loops
    ?(readf=read)
    ?(writev=Faraday_async.writev_of_fd)
    ?(close_read=close_read)
    ~config
    ~socket
    connection =
    let fd     = Socket.fd socket in
    let writev = writev fd in
    let read_complete = Ivar.create () in
    let buffer = Bigstring_buffer.create config.Config.read_buffer_size in
    let rec reader_thread () =
      match Client_connection.next_read_operation connection with
      | `Read ->
        (* Log.Global.printf "read(%d)%!" (Fd.to_int_exn fd); *)
        readf fd buffer
          >>> begin function
            | `Eof  ->
               Bigstring_buffer.get buffer ~f:(fun bigstring ~off ~len ->
                Client_connection.read_eof connection bigstring ~off ~len)
              |> ignore;
              reader_thread ()
            | `Ok _ ->
              Bigstring_buffer.get buffer ~f:(fun bigstring ~off ~len ->
                Client_connection.read connection bigstring ~off ~len)
              |> ignore;
              reader_thread ()
          end
      | `Yield ->
        Client_connection.yield_writer connection reader_thread;
      | `Close ->
        (* Log.Global.printf "read_close(%d)%!" (Fd.to_int_exn fd); *)
        Deferred.don't_wait_for (close_read socket >>| Ivar.fill read_complete)
    in
    let write_complete = Ivar.create () in
    let rec writer_thread () =
      match Client_connection.next_write_operation connection with
      | `Write iovecs ->
        (* Log.Global.printf "write(%d)%!" (Fd.to_int_exn fd); *)
        writev iovecs >>> fun result ->
          Client_connection.report_write_result connection result;
          writer_thread ()
      | `Yield ->
        (* Log.Global.printf "write_yield(%d)%!" (Fd.to_int_exn fd); *)
        Client_connection.yield_writer connection writer_thread;
      | `Close _ ->
        (* Log.Global.printf "write_close(%d)%!" (Fd.to_int_exn fd); *)
        Ivar.fill write_complete ();
    in
    let conn_monitor = Monitor.create () in
    Scheduler.within ~monitor:conn_monitor reader_thread;
    Scheduler.within ~monitor:conn_monitor writer_thread;
    Monitor.detach_and_iter_errors conn_monitor ~f:(fun exn ->
      Client_connection.report_exn connection exn);
    don't_wait_for (
      Deferred.all_unit
        [ Ivar.read read_complete
        ; Ivar.read write_complete ]
      >>| fun () ->
        if not (Fd.is_closed fd)
        then don't_wait_for (Fd.close fd));
    connection

  let create_connection ?(config=Config.default) socket =
    let conn   = Client_connection.create ~config in
      start_read_write_loops ~config ~socket conn

  let request = Client_connection.request

  let shutdown = Client_connection.shutdown

  let is_closed = Client_connection.is_closed

  module SSL = struct
    let create_connection ?client ?(config=Config.default) socket =
      Ssl_io.make_client ?client socket >>| begin fun ssl ->
      let ssl_reader = Ssl_io.reader ssl in
      let ssl_writer = Ssl_io.writer ssl in
      let readf = Ssl_io.readf ssl_reader in
      let writev = Ssl_io.writev ssl_writer in

    let conn   = Client_connection.create ~config in
      start_read_write_loops
        ~config
        ~readf
        ~writev
        ~socket
        conn
      end

  let request = Client_connection.request

  let shutdown = Client_connection.shutdown

  let is_closed = Client_connection.is_closed

  end
end

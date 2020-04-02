(*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.
    Copyright (c) 2018 Anton Bachin
    Copyright (c) 2019 António Nuno Monteiro

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Lwt.Infix

(* Based on the Buffer module in httpaf_async.ml. *)
module Buffer : sig
  type t

  val create : int -> t

  val get : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int
  val put
    :  t
    -> f:(Bigstringaf.t -> off:int -> len:int -> [ `Eof | `Ok of int ] Lwt.t)
    -> [ `Eof | `Ok of int ] Lwt.t
end = struct
  type t =
    { buffer      : Bigstringaf.t
    ; mutable off : int
    ; mutable len : int }

  let create size =
    let buffer = Bigstringaf.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0
    then begin
      t.off <- 0;
      t.len <- 0;
    end else if t.off > 0
    then begin
      Bigstringaf.blit t.buffer ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len;
      t.off <- 0;
    end

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0
    then t.off <- 0;
    n

  let put t ~f =
    compress t;
    f t.buffer ~off:(t.off + t.len) ~len:(Bigstringaf.length t.buffer - t.len)
    >|= function
      | `Eof -> `Eof
      | `Ok n as ret ->
        t.len <- t.len + n;
        ret
end

module Config = Httpaf.Config
include Httpaf_lwt_intf

module Server (Io: IO) = struct
  type socket = Io.socket

  let report_exn connection socket exn =
    (* This needs to handle two cases. The case where the socket is
     * still open and we can gracefully respond with an error, and the
     * case where the client has already left. The second case is more
     * common when communicating over HTTPS, given that the remote peer
     * can close the connection without requiring an acknowledgement:
     *
     * From RFC5246§7.2.1:
     *   Unless some other fatal alert has been transmitted, each party
     *   is required to send a close_notify alert before closing the
     *   write side of the connection.  The other party MUST respond
     *   with a close_notify alert of its own and close down the
     *   connection immediately, discarding any pending writes. It is
     *   not required for the initiator of the close to wait for the
     *   responding close_notify alert before closing the read side of
     *   the connection. *)
    (match Io.state socket with
    | `Error | `Closed ->
      Httpaf.Server_connection.shutdown connection
    | `Open ->
      Httpaf.Server_connection.report_exn connection exn);
    Lwt.return_unit

  let create_connection_handler ?(config=Config.default) ~request_handler ~error_handler =
    fun client_addr socket ->
      let module Server_connection = Httpaf.Server_connection in
      let connection =
        Server_connection.create
          ~config
          ~error_handler:(error_handler client_addr)
          (request_handler client_addr)
      in

      let read_buffer = Buffer.create config.read_buffer_size in
      let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

      let rec read_loop () =
        let rec read_loop_step () =
          match Server_connection.next_read_operation connection with
          | `Read ->
            Buffer.put ~f:(Io.read socket) read_buffer >>= begin function
            | `Eof ->
              Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
                Server_connection.read_eof connection bigstring ~off ~len)
              |> ignore;
              read_loop_step ()
            | `Ok _ ->
              Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
                Server_connection.read connection bigstring ~off ~len)
              |> ignore;
              read_loop_step ()
            end

          | `Upgrade ->
            Lwt.wakeup_later notify_read_loop_exited ();
            Lwt.return_unit

          | `Yield ->
            Server_connection.yield_reader connection read_loop;
            Lwt.return_unit

          | `Close ->
            Lwt.wakeup_later notify_read_loop_exited ();
            Io.shutdown_receive socket;
            Lwt.return_unit
        in

        Lwt.async (fun () ->
            Lwt.catch read_loop_step (report_exn connection socket))
      in


      let writev = Io.writev socket in
      let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

      let rec write_loop () =
        let rec write_loop_step () =
          match Server_connection.next_write_operation connection with
          | `Write io_vectors ->
            writev io_vectors >>= fun result ->
            Server_connection.report_write_result connection result;
            write_loop_step ()

          | `Upgrade (io_vectors, upgrade_handler) ->
            writev io_vectors >>= fun result ->
            Server_connection.report_write_result connection result;
            upgrade_handler socket >|= fun () ->
            Lwt.wakeup_later notify_write_loop_exited ()

          | `Yield ->
            Server_connection.yield_writer connection write_loop;
            Lwt.return_unit

          | `Close _ ->
            Lwt.wakeup_later notify_write_loop_exited ();
            Io.shutdown_send socket;
            Lwt.return_unit
        in

        Lwt.async (fun () ->
            Lwt.catch write_loop_step (report_exn connection socket))
      in


      read_loop ();
      write_loop ();
      Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->

      Io.close socket
end


module Client (Io: IO) = struct
  module Client_connection = Httpaf.Client_connection

  type socket = Io.socket
  type t =
    { connection : Client_connection.t
    ; socket : socket
    }

  let create_connection ?(config=Config.default) socket =
    let connection =
      Client_connection.create ~config in


    let read_buffer = Buffer.create config.read_buffer_size in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let rec read_loop () =
      let rec read_loop_step () =
        match Client_connection.next_read_operation connection with
        | `Read ->
          Buffer.put ~f:(Io.read socket) read_buffer >>= begin function
          | `Eof ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read_eof connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          | `Ok _ ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          end

        | `Yield ->
          Client_connection.yield_reader connection read_loop;
          Lwt.return_unit

        | `Close ->
          Lwt.wakeup_later notify_read_loop_exited ();
          Io.shutdown_receive socket;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          read_loop_step
          (fun exn ->
            Client_connection.report_exn connection exn;
            Lwt.return_unit))
    in


    let writev = Io.writev socket in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec write_loop () =
      let rec write_loop_step () =
        match Client_connection.next_write_operation connection with
        | `Write io_vectors ->
          writev io_vectors >>= fun result ->
          Client_connection.report_write_result connection result;
          write_loop_step ()

        | `Yield ->
          Client_connection.yield_writer connection write_loop;
          Lwt.return_unit

        | `Close _ ->
          Lwt.wakeup_later notify_write_loop_exited ();
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          write_loop_step
          (fun exn ->
            Client_connection.report_exn connection exn;
            Lwt.return_unit))
    in


    read_loop ();
    write_loop ();

    Lwt.async (fun () ->
      Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->
      Io.close socket);

    Lwt.return { connection; socket }

  let request t = Client_connection.request t.connection

  let shutdown t =
    Client_connection.shutdown t.connection;
    Io.close t.socket

  let is_closed t = Client_connection.is_closed t.connection
end

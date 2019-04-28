(*----------------------------------------------------------------------------
    Copyright (c) 2017-2019 Inhabited Type LLC.
    Copyright (c) 2019 Antonio Nuno Monteiro.

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

module Reader = Parse.Reader
module Writer = Serialize.Writer

module Oneshot = struct
  type error =
    [ `Malformed_response of string | `Invalid_response_body_length of Response.t | `Exn of exn ]

  type response_handler = Response.t -> [`read] Body.t  -> unit
  type error_handler = error -> unit

  type t =
    { config : Config.t
    ; reader : Reader.response
    ; writer : Writer.t
    ; request_queue : Respd.t Queue.t
      (* invariant: If [request_queue] is not empty, then the head of the queue
         has already written the request headers to the wire. *)
    ; wakeup_writer  : (unit -> unit) list ref
    ; wakeup_reader  : (unit -> unit) list ref
    }

  let is_closed t =
    Reader.is_closed t.reader && Writer.is_closed t.writer

  let is_waiting t =
    not (is_closed t) && Queue.is_empty t.request_queue

  let is_active t =
    not (Queue.is_empty t.request_queue)

  let current_respd_exn t =
    Queue.peek t.request_queue

  let on_wakeup_reader t k =
    if is_closed t
    then failwith "on_wakeup_reader on closed conn"
    else t.wakeup_reader := k::!(t.wakeup_reader)

  let on_wakeup_writer t k =
    if is_closed t
    then failwith "on_wakeup_writer on closed conn"
    else t.wakeup_writer := k::!(t.wakeup_writer)

  let _wakeup_writer callbacks =
    let fs = !callbacks in
    callbacks := [];
    List.iter (fun f -> f ()) fs

  let wakeup_writer t =
    _wakeup_writer t.wakeup_writer

  let wakeup_reader t =
    let fs = !(t.wakeup_reader) in
    t.wakeup_reader := [];
    List.iter (fun f -> f ()) fs

  let[@ocaml.warning "-16"] create ?(config=Config.default) =
    let request_queue = Queue.create () in
    { config
    ; reader = Reader.response request_queue
    ; writer = Writer.create ()
    ; request_queue
    ; wakeup_writer = ref []
    ; wakeup_reader = ref []
    }

  let request t request ~error_handler ~response_handler =
    let request_body =
      Body.create (Bigstringaf.create t.config.request_body_buffer_size)
    in
    let respd =
      Respd.create error_handler request request_body t.writer response_handler in
    let handle_now = Queue.is_empty t.request_queue in
    Queue.push respd t.request_queue;
    if handle_now then
      Respd.write_request respd;
    (* Not handling the request now means it may be pipelined.
     * `advance_request_queue_if_necessary` will take care of it. *)
    _wakeup_writer t.wakeup_writer;
    request_body
  ;;

  let flush_request_body t =
    if is_active t then begin
      let respd = current_respd_exn t in
      Respd.flush_request_body respd
    end
  ;;

  let set_error_and_handle_without_shutdown t error =
    if is_active t then begin
      let respd = current_respd_exn t in
      Respd.report_error respd error
    end
    (* TODO: not active?! *)
  ;;

  let unexpected_eof t =
    set_error_and_handle_without_shutdown t (`Malformed_response "unexpected eof");
  ;;

  let shutdown_reader t =
    Reader.force_close t.reader;
    if is_active t
    then Respd.close_response_body (current_respd_exn t)
    else wakeup_reader t

  let shutdown_writer t =
    flush_request_body t;
    Writer.close t.writer;
    if is_active t then begin
      let respd = current_respd_exn t in
      Body.close_writer respd.request_body;
    end
  ;;

  let shutdown t =
    shutdown_reader t;
    shutdown_writer t;
  ;;

  (* TODO: Need to check in the RFC if reporting an error, e.g. in a malformed
   * response causes the whole connection to shutdown. *)
  let set_error_and_handle t error =
    shutdown t;
    set_error_and_handle_without_shutdown t error;
  ;;

  let report_exn t exn =
    set_error_and_handle t (`Exn exn)
  ;;

  (* TODO: review this function *)
  let advance_request_queue_if_necessary t =
    if is_active t then begin
      let respd = current_respd_exn t in
      if Respd.persistent_connection respd then begin
        if Respd.is_complete respd then begin
          ignore (Queue.take t.request_queue);
          if not (Queue.is_empty t.request_queue) then begin
            (* write request to the wire *)
            let respd = current_respd_exn t in
            Respd.write_request respd;
          end;
          (* TODO: wake up writer?! *)
          wakeup_reader t;
        end else if not (Respd.requires_output respd) then begin
          (* From RFC7230§6.3.2:
           *   A client that supports persistent connections MAY "pipeline" its
           *   requests (i.e., send multiple requests without waiting for each
           *   response). *)
          ignore @@ (Queue.fold (fun first respd ->
              if not first then begin
                if respd.Respd.state = Uninitialized
                then Respd.write_request respd
                (* else raise Local *)
              end;
              false)
            true
            t.request_queue)
        end
      end else begin
        ignore (Queue.take t.request_queue);
        Queue.iter Respd.close_response_body t.request_queue;
        Queue.clear t.request_queue;
        Queue.push respd t.request_queue;
        wakeup_writer t;
        if Respd.is_complete respd
        then shutdown t
        else if not (Respd.requires_output respd)
        then shutdown_writer t
        (* else if not (Respd.requires_input respd)
        then shutdown_reader t *)
      end
    end else if Reader.is_closed t.reader
    then shutdown t

  let next_read_operation t =
    advance_request_queue_if_necessary t;
    match Reader.next t.reader with
    | `Error (`Parse(marks, message)) ->
      let message = String.concat "" [ String.concat ">" marks; ": "; message] in
      set_error_and_handle t (`Malformed_response message);
      `Close
    | `Error (`Invalid_response_body_length _ as error) ->
      set_error_and_handle t error;
      `Close
    | (`Read | `Close) as operation -> operation
  ;;

  let read_with_more t bs ~off ~len more =
    let consumed = Reader.read_with_more t.reader bs ~off ~len more in
    if is_active t then
      Respd.flush_response_body (current_respd_exn t);
    consumed
  ;;

  let read t bs ~off ~len =
    read_with_more t bs ~off ~len Incomplete

  let read_eof t bs ~off ~len =
    let bytes_read = read_with_more t bs ~off ~len Complete in
    if is_active t then begin
      let respd = current_respd_exn t in
      match respd.state with
      | Uninitialized -> assert false
      | Received_response _ | Closed -> ()
      | Awaiting_response ->
        (* TODO: review this. It makes sense to tear down the connection if an
         * unexpected EOF is received. *)
        shutdown t;
        unexpected_eof t
    end;
    bytes_read
  ;;

  let next_write_operation t =
    advance_request_queue_if_necessary t;
    flush_request_body t;
    Writer.next t.writer
  ;;

  let yield_writer t k =
    if is_active t then begin
      let respd = current_respd_exn t in
      if Respd.requires_output respd then
        Respd.on_more_output_available respd k
      else if Respd.persistent_connection respd then
        on_wakeup_writer t k
      else begin
        (*  TODO: call shutdown? *)
        Writer.close t.writer;
        k ()
      end
    end else
      on_wakeup_writer t k

  let report_write_result t result =
    Writer.report_result t.writer result

  let is_closed t = Reader.is_closed t.reader && Writer.is_closed t.writer
end

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
  ; mutable wakeup_writer  : Optional_thunk.t
  ; mutable wakeup_reader  : Optional_thunk.t
  }

let is_closed t =
  Reader.is_closed t.reader && Writer.is_closed t.writer

let is_waiting t =
  not (is_closed t) && Queue.is_empty t.request_queue

let is_active t =
  not (Queue.is_empty t.request_queue)

let current_respd_exn t =
  Queue.peek t.request_queue

let yield_reader t k =
  if is_closed t
  then failwith "on_wakeup_reader on closed conn"
  else if Optional_thunk.is_some t.wakeup_reader
  then failwith "yield_reader: only one callback can be registered at a time"
  else if is_active t then
    let respd = current_respd_exn t in
    begin match Respd.input_state respd with
    | Wait ->
      (* `Wait` means that the response body isn't closed yet (there may be
       * more incoming bytes) but the response handler hasn't scheduled a read
       * either. *)
      Respd.on_more_input_available respd k
    | Ready | Complete ->
      (* `Complete` may happen when connection has been upgraded. *)
      t.wakeup_reader <- Optional_thunk.some k
    end
  else t.wakeup_reader <- Optional_thunk.some k

let wakeup_reader t =
  let f = t.wakeup_reader in
  t.wakeup_reader <- Optional_thunk.none;
  Optional_thunk.call_if_some f

let transfer_reader_callback t respd =
  if Optional_thunk.is_some t.wakeup_reader
  then (
    let f = t.wakeup_reader in
    t.wakeup_reader <- Optional_thunk.none;
    Respd.on_more_input_available respd (Optional_thunk.unchecked_value f))

let yield_writer t k =
  if is_closed t
  then failwith "yield_writer on closed conn"
  else if Optional_thunk.is_some t.wakeup_writer
  then failwith "yield_writer: only one callback can be registered at a time"
  else if is_active t then
    let respd = current_respd_exn t in
    match Respd.output_state respd with
    | Consume -> Respd.on_more_output_available respd k
    | Wait | Complete ->
      (* This can feel counter-intuitive. We don't immediately execute this
       * callback or we'd trigger an infinite loop in the case where we're done
       * sending the request body and are waiting for the response to arrive,
       * in which case the "output state" for the descriptor is "Complete" and
       * the state machine issues a `Yield` operation. *)
      t.wakeup_writer <- Optional_thunk.some k
  else
    t.wakeup_writer <- Optional_thunk.some k

let wakeup_writer t =
  let f = t.wakeup_writer in
  t.wakeup_writer <- Optional_thunk.none;
  Optional_thunk.call_if_some f

let[@ocaml.warning "-16"] create ?(config=Config.default) =
  let request_queue = Queue.create () in
  { config
  ; reader = Reader.response request_queue
  ; writer = Writer.create ()
  ; request_queue
  ; wakeup_writer = Optional_thunk.none
  ; wakeup_reader = Optional_thunk.none
  }

let request t request ~error_handler ~response_handler =
  let request_body =
    Body.create (Bigstringaf.create t.config.request_body_buffer_size)
  in
  if not (Request.body_length request = `Chunked) then
    Body.set_non_chunked request_body;
  let respd =
    Respd.create error_handler request request_body t.writer response_handler in
  let handle_now = Queue.is_empty t.request_queue in
  Queue.push respd t.request_queue;
  if handle_now then
    Respd.write_request respd;
  (* Not handling the request now means it may be pipelined.
   * `advance_request_queue_if_necessary` will take care of it, but we still
   * wanna wake up the writer so that the function gets called. *)
  wakeup_writer t;
  request_body
;;

let shutdown_reader t =
  Reader.force_close t.reader;
  if is_active t
  then Respd.close_response_body (current_respd_exn t)
  else wakeup_reader t

let shutdown_writer t =
  Writer.close t.writer;
  if is_active t then begin
    let respd = current_respd_exn t in
    Body.close_writer respd.request_body;
  end
;;

let shutdown t =
  Queue.iter Respd.close_response_body t.request_queue;
  Queue.clear t.request_queue;
  shutdown_reader t;
  shutdown_writer t;
  wakeup_reader t;
  wakeup_writer t;
;;

(* TODO: Need to check in the RFC if reporting an error, e.g. in a malformed
 * response causes the whole connection to shutdown. *)
let set_error_and_handle t error =
  Reader.force_close t.reader;
  Queue.iter (fun respd ->
   match Respd.input_state respd with
   | Wait | Ready ->
     Respd.report_error respd error
   | Complete ->
     match Reader.next t.reader with
     | `Error _ | `Read ->
       Respd.report_error respd error
     | _ ->
       (* Don't bother reporting errors to responses that have already
        * completed. *)
       ())
  t.request_queue;
;;

let unexpected_eof t =
  set_error_and_handle t (`Malformed_response "unexpected eof");
  shutdown t;
;;

let report_exn t exn =
  set_error_and_handle t (`Exn exn)
;;

exception Local

let maybe_pipeline_queued_requests t =
  (* Don't bother trying to pipeline if there aren't multiple requests in the
   * queue. *)
  if Queue.length t.request_queue > 1 then
    try
      let _ = Queue.fold (fun prev respd ->
        begin match prev with
        | None -> ()
        | Some prev ->
          match respd.Respd.state, Respd.output_state prev with
          | Uninitialized, Complete ->
            Respd.write_request respd
          | _ ->
            (* bail early. If we can't pipeline this request, we can't write
             * next ones either. *)
            raise Local
        end;
        Some respd)
        None
        t.request_queue
      in ()
    with
    | _ -> ()

let advance_request_queue t =
  ignore (Queue.take t.request_queue);
  if not (Queue.is_empty t.request_queue) then begin
    (* write request to the wire *)
    let respd = current_respd_exn t in
    match respd.state with
    | Uninitialized ->
      (* Only write request if it hasn't been written to the wire yet (e.g. via
       * pipelining). *)
      Respd.write_request respd;
      wakeup_writer t
    | _ -> ()
  end

let rec _next_read_operation t =
  if not (is_active t) then (
    if Reader.is_closed t.reader
    then shutdown t;
    Reader.next t.reader
  ) else (
    let respd = current_respd_exn t in
    match Respd.input_state respd with
    | Wait ->
      transfer_reader_callback t respd;
      `Yield
    | Ready  -> Reader.next t.reader
    | Complete -> _final_read_operation_for t respd
  )

and _final_read_operation_for t respd =
  let next =
    if not (Respd.persistent_connection respd) then (
      shutdown_reader t;
      Reader.next t.reader;
    ) else (
      match Respd.output_state respd with
      | Wait | Consume -> `Yield
      | Complete       ->
         match Reader.next t.reader with
         | `Error _ | `Read as operation->
           (* Keep reading when in a "partial" state (`Read).
            * Don't advance the request queue if in an error state. *)
           operation
         | _ ->
           advance_request_queue t;
           _next_read_operation t;
    )
  in
  wakeup_writer t;
  next
;;

let next_read_operation t =
  match _next_read_operation t with
  | `Error (`Parse(marks, message)) ->
    let message = String.concat "" [ String.concat ">" marks; ": "; message] in
    set_error_and_handle t (`Malformed_response message);
    `Close
  | `Error (`Invalid_response_body_length _ as error) ->
    set_error_and_handle t error;
    `Close
  | `Start -> `Read
  | (`Read | `Yield | `Close) as operation -> operation
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
  if is_active t
  then unexpected_eof t;
  bytes_read
;;

let rec _next_write_operation t =
  if not (is_active t) then (
    if Reader.is_closed t.reader
    then shutdown t;
    Writer.next t.writer
  ) else (
    let respd = current_respd_exn t in
    match Respd.output_state respd with
    | Wait -> `Yield
    | Consume ->
      Respd.flush_request_body respd;
      Writer.next t.writer
    | Complete -> _final_write_operation_for t respd
  )

and _final_write_operation_for t respd =
  let next =
    if not (Respd.persistent_connection respd) then (
      shutdown_writer t;
      Writer.next t.writer;
    ) else (
      (* From RFC7230§6.3.2:
       *   A client that supports persistent connections MAY "pipeline" its
       *   requests (i.e., send multiple requests without waiting for each
       *   response). *)
      maybe_pipeline_queued_requests t;
      match Respd.input_state respd with
      | Wait | Ready ->
        Writer.next t.writer;
      | Complete ->
         match Reader.next t.reader with
         | `Error _ | `Read  -> Writer.next t.writer
         | _ ->
           advance_request_queue t;
           _next_write_operation t
    )
  in
  wakeup_reader t;
  next
;;

let next_write_operation t = _next_write_operation t

let report_write_result t result =
  Writer.report_result t.writer result

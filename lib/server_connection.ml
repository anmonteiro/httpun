(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.

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


type ('fd, 'io) request_handler = ('fd, 'io) Reqd.t -> unit

type error =
  [ `Bad_gateway | `Bad_request | `Internal_server_error | `Exn of exn]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> [`write] Body.t) -> unit

type ('fd, 'io) error_code =
  | No_error
  | Error of
    { request: Request.t option
    ; mutable response_state: ('fd, 'io) Response_state.t
    }

type ('fd, 'io) t =
  { reader                 : Reader.request
  ; writer                 : Writer.t
  ; response_body_buffer   : Bigstringaf.t
  ; request_handler        : ('fd, 'io) request_handler
  ; error_handler          : error_handler
  ; request_queue          : ('fd, 'io) Reqd.t Queue.t
    (* invariant: If [request_queue] is not empty, then the head of the queue
       has already had [request_handler] called on it. *)
  ; mutable wakeup_writer  : Optional_thunk.t
  ; mutable wakeup_reader  : Optional_thunk.t
    (* Represents an unrecoverable error that will cause the connection to
     * shutdown. Holds on to the response body created by the error handler
     * that might be streaming to the client. *)
  ; mutable error_code : ('fd, 'io) error_code
  }

let is_closed t =
  Reader.is_closed t.reader && Writer.is_closed t.writer

let is_waiting t =
  not (is_closed t) && Queue.is_empty t.request_queue

let is_active t =
  not (Queue.is_empty t.request_queue)

let current_reqd_exn t =
  Queue.peek t.request_queue

let yield_reader t k =
  if is_closed t
  then failwith "yield_reader on closed conn"
  else if Optional_thunk.is_some t.wakeup_reader
  then failwith "yield_reader: only one callback can be registered at a time"
  else t.wakeup_reader <- Optional_thunk.some k
;;

let wakeup_reader t =
  let f = t.wakeup_reader in
  t.wakeup_reader <- Optional_thunk.none;
  Optional_thunk.call_if_some f
;;

let yield_writer t k =
  if is_closed t
  then failwith "yield_writer on closed conn"
  else if Optional_thunk.is_some t.wakeup_writer
  then failwith "on_wakeup_writer: only one callback can be registered at a time"
  else match t.error_code with
  | No_error ->
    if is_active t then
      let reqd = current_reqd_exn t in
      begin match Reqd.output_state reqd with
      | Wait -> t.wakeup_writer <- Optional_thunk.some k
      | Consume -> Reqd.on_more_output_available reqd k
      | Complete -> k ()
      end
    else
      t.wakeup_writer <- Optional_thunk.some k
  | Error { response_state; _ } ->
    Response_state.on_more_output_available response_state k
;;

let wakeup_writer t =
  let f = t.wakeup_writer in
  t.wakeup_writer <- Optional_thunk.none;
  Optional_thunk.call_if_some f
;;

let transfer_writer_callback t reqd =
  if Optional_thunk.is_some t.wakeup_writer
  then (
    let f = t.wakeup_writer in
    t.wakeup_writer <- Optional_thunk.none;
    Reqd.on_more_output_available reqd (Optional_thunk.unchecked_value f))
;;

let default_error_handler ?request:_ error handle =
  let message =
    match error with
    | `Exn exn -> Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error -> Status.to_string error
  in
  let body = handle Headers.empty in
  Body.write_string body message;
  Body.close_writer body
;;

let create ?(config=Config.default) ?(error_handler=default_error_handler) request_handler =
  let
    { Config
    . response_buffer_size
    ; response_body_buffer_size
    ; _ } = config
  in
  let writer = Writer.create ~buffer_size:response_buffer_size () in
  let request_queue = Queue.create () in
  let response_body_buffer = Bigstringaf.create response_body_buffer_size in
  let handler request request_body =
    let reqd =
      Reqd.create error_handler request request_body writer response_body_buffer
    in
    Queue.push reqd request_queue;
  in
  { reader          = Reader.request handler
  ; writer
  ; response_body_buffer
  ; request_handler = request_handler
  ; error_handler   = error_handler
  ; request_queue
  ; wakeup_writer   = Optional_thunk.none
  ; wakeup_reader   = Optional_thunk.none
  ; error_code = No_error
  }

let shutdown_reader t =
  Reader.force_close t.reader;
  if is_active t
  then Reqd.close_request_body (current_reqd_exn t)
  else wakeup_reader t

let shutdown_writer t =
  Writer.close t.writer;
  if is_active t
  then Reqd.close_request_body (current_reqd_exn t)
  else wakeup_writer t

let error_code t =
  if is_active t
  then Reqd.error_code (current_reqd_exn t)
  else None

let shutdown t =
  Queue.clear t.request_queue;
  shutdown_reader t;
  shutdown_writer t;
  wakeup_reader t;
  wakeup_writer t

let set_error_and_handle ?request t error =
  if is_active t then begin
    assert (request = None);
    let reqd = current_reqd_exn t in
    Reqd.report_error reqd error
  end else begin
    let status =
      match (error :> [error | Status.standard]) with
      | `Exn _                     -> `Internal_server_error
      | #Status.standard as status -> status
    in
    shutdown_reader t;
    let writer = t.writer in
    match t.error_code with
    | No_error ->
      (* The (shared) response body buffer can be used in this case because in
       * this conditional branch we're not sending a response
       * (is_active t == false), and are therefore not making use of that
       * buffer. *)
      let response_body = Body.create t.response_body_buffer in
      t.error_code <- Error { request; response_state = Waiting (ref Optional_thunk.none) };
      t.error_handler ?request error (fun headers ->
          let response = Response.create ~headers status in
          Writer.write_response writer response;
          t.error_code <- Error { request; response_state = Streaming(response, response_body) };
          wakeup_writer t;
          response_body)
    | Error _ ->
      (* When reading, this should be impossible: even if we try to read more,
       * the parser does not ingest it, and even if someone attempts to feed
       * more bytes to the parser when we already told them to [`Close], that's
       * really their own fault.
       *
       * We do, however, need to handle this case if any other exception is
       * reported (we're already handling an error and e.g. the writing channel
       * is closed). Just shut down the connection in that case.
       *)
      Writer.close_and_drain t.writer;
      shutdown t
  end

let report_exn t exn =
  set_error_and_handle t (`Exn exn)

let advance_request_queue t =
  ignore (Queue.take t.request_queue);
  if not (Queue.is_empty t.request_queue)
  then t.request_handler (Queue.peek t.request_queue);
;;

let rec _next_read_operation t =
  if not (is_active t) then (
    if Reader.is_failed t.reader then
      (* Don't tear down the whole connection if we saw an unrecoverable
       * parsing error, as we might be in the process of streaming back the
       * error response body to the client. *)
      shutdown_reader t
    else if Reader.is_closed t.reader
    then shutdown t;
    Reader.next t.reader
  ) else (
    let reqd = current_reqd_exn t in
    match Reqd.input_state reqd with
    | Provide  -> Reader.next t.reader
    | Complete -> _final_read_operation_for t reqd
  )

and _final_read_operation_for t reqd =
  let next =
    if not (Reqd.persistent_connection reqd) then (
      shutdown_reader t;
      Reader.next t.reader;
    ) else (
      match Reqd.output_state reqd with
      | Wait | Consume -> `Yield
      | Complete       ->
        advance_request_queue t;
        _next_read_operation t;
    )
  in
  wakeup_writer t;
  next
;;

let next_read_operation t =
  match _next_read_operation t with
  | `Error (`Parse _)             -> set_error_and_handle          t `Bad_request; `Close
  | `Error (`Bad_request request) -> set_error_and_handle ~request t `Bad_request; `Close
  | (`Read | `Yield | `Close) as operation ->
    if is_active t then begin
      let reqd = current_reqd_exn t in
      match Reqd.upgrade_handler reqd with
      | Some _ -> `Upgrade
      | None -> operation
    end else
      operation

let read_with_more t bs ~off ~len more =
  let call_handler = Queue.is_empty t.request_queue in
  let consumed = Reader.read_with_more t.reader bs ~off ~len more in
  if is_active t
  then (
    let reqd = current_reqd_exn t in
    if call_handler
    then (
      transfer_writer_callback t reqd;
      t.request_handler reqd
    );
    Reqd.flush_request_body reqd;
  );
  consumed
;;

let read t bs ~off ~len =
  read_with_more t bs ~off ~len Incomplete

let read_eof t bs ~off ~len =
  read_with_more t bs ~off ~len Complete

let flush_response_error_body t ?request response_state =
  let request_method = match request with
    | Some { Request.meth; _ } -> meth
    | None ->
      (* XXX(anmonteiro): Error responses may not have a request method if they
       * are the result of e.g. an EOF error. Assuming that the request method
       * is `GET` smells a little because it's exposing implementation details,
       * though the only case where it'd matter would be potentially assuming
       * the _successful_ response to a CONNECT request and sending one of the
       * forbidden headers according to RFC7231§4.3.6:
       *
       *   A server MUST NOT send any Transfer-Encoding or Content-Length
       *   header fields in a 2xx (Successful) response to CONNECT.
       *
       * If we're running this code, however, we're not responding with a
       * successful status code, which makes us compliant to the above. *)
      `GET
  in
  Response_state.flush_response_body response_state ~request_method t.writer

let rec _next_write_operation t =
  if not (is_active t) then (
    match t.error_code with
    | No_error ->
      if Reader.is_closed t.reader
      then shutdown t;
      Writer.next t.writer
    | Error { request; response_state } ->
      match Response_state.output_state response_state with
      | Wait -> `Yield
      | Consume ->
        flush_response_error_body t ?request response_state;
        Writer.next t.writer
      | Complete ->
        shutdown_writer t;
        Writer.next t.writer
  ) else (
    let reqd = current_reqd_exn t in
    match Reqd.output_state reqd with
    | Wait -> `Yield
    | Consume ->
      Reqd.flush_response_body reqd;
      begin match Writer.next t.writer with
      | `Write iovecs as write_op ->
        begin match Reqd.upgrade_handler reqd with
        | Some upgrade ->
          `Upgrade (iovecs, upgrade)
        | None -> write_op
        end
      | operation -> operation
      end
    | Complete -> _final_write_operation_for t reqd
  )

and _final_write_operation_for t reqd =
  let next =
    if not (Reqd.persistent_connection reqd) then (
      shutdown_writer t;
      Writer.next t.writer;
    ) else (
      match Reqd.input_state reqd with
      | Provide  -> assert false
      | Complete ->
        advance_request_queue t;
        _next_write_operation t;
    )
  in
  wakeup_reader t;
  next
;;

let next_write_operation t = _next_write_operation t

let report_write_result t result =
  Writer.report_result t.writer result

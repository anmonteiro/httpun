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


type request_handler = Reqd.t -> unit

type error =
  [ `Bad_gateway | `Bad_request | `Internal_server_error | `Exn of exn]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> Body.Writer.t) -> unit

type error_code =
  | No_error
  | Error of
    { request: Request.t option
    ; mutable response_state: Response_state.t
    }

type t =
  { reader                 : Reader.request
  ; writer                 : Writer.t
  ; response_body_buffer   : Bigstringaf.t
  ; request_handler        : request_handler
  ; error_handler          : error_handler
  ; request_queue          : Reqd.t Queue.t
    (* invariant: If [request_queue] is not empty, then the head of the queue
       has already had [request_handler] called on it. *)
  ; mutable error_code : error_code
    (* Represents an unrecoverable error that will cause the connection to
     * shutdown. Holds on to the response body created by the error handler
     * that might be streaming to the client. *)
  }

let is_closed t =
  Reader.is_closed t.reader && Writer.is_closed t.writer

let is_active t =
  not (Queue.is_empty t.request_queue)

let current_reqd_exn t =
  Queue.peek t.request_queue

let yield_reader t k =
  Reader.on_wakeup t.reader k

let wakeup_reader t =
  if is_active t then begin
    let reqd = current_reqd_exn t in
    (* Before going through another read loop, give the body a chance to flush
       its buffered bytes to the application. This fixes a pathological case
       where the body could buffer too much without a chance of executing
       scheduled reads. *)
    Reqd.flush_request_body reqd;
  end;
  Reader.wakeup t.reader

let yield_writer t k =
 Writer.on_wakeup t.writer k
;;

let wakeup_writer t = Writer.wakeup t.writer

let default_error_handler ?request:_ error handle =
  let message =
    match error with
    | `Exn exn -> Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error -> Status.to_string error
  in
  let body = handle Headers.empty in
  Body.Writer.write_string body message;
  Body.Writer.close body
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
  let rec reader = lazy (Reader.request ~wakeup:(fun () -> wakeup_reader (Lazy.force t)) handler)
  and handler request request_body =
    let reqd =
      Reqd.create error_handler request request_body (Lazy.force reader) writer response_body_buffer
    in
    Queue.push reqd request_queue;
  and t = lazy
    { reader = Lazy.force reader
    ; writer
    ; response_body_buffer
    ; request_handler = request_handler
    ; error_handler   = error_handler
    ; request_queue
    ; error_code = No_error
    }
  in
  Lazy.force t

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
  Queue.iter Reqd.close_request_body t.request_queue;
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
      t.error_code <- Error { request; response_state = Waiting };
      t.error_handler ?request error (fun headers ->
          let response = Response.create ~headers status in
          let encoding =
            (* If we haven't parsed the request method, just use GET as a standard
               placeholder. The method is only used for edge cases, like HEAD or
               CONNECT. *)
            let request_method =
              match request with
              | None -> `GET
              | Some (request: Request.t) -> request.meth
            in
            match Response.body_length ~request_method response with
            | `Fixed _ | `Close_delimited | `Chunked as encoding -> encoding
            | `Error (`Bad_gateway | `Internal_server_error) ->
              failwith "httpaf.Server_connection.error_handler: invalid response body length"
          in
          let response_body =
            (* The (shared) response body buffer can be used in this case
             * because in this conditional branch we're not sending a response
             * (is_active t == false), and are therefore not making use of that
             * buffer. *)
            Body.Writer.create
              t.response_body_buffer
              ~encoding ~when_ready_to_write:(Optional_thunk.some (fun () ->
                wakeup_writer t))
          in
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
    let next = Reader.next t.reader in
    begin match next with
    | `Error _ ->
      (* Don't tear down the whole connection if we saw an unrecoverable
       * parsing error, as we might be in the process of streaming back the
       * error response body to the client. *)
      shutdown_reader t
    | `Close ->
      (match t.error_code with
      | No_error -> shutdown t
      | Error _ -> ())
    | _ -> ()
    end;
    next
  ) else (
    let reqd = current_reqd_exn t in
    match Reqd.input_state reqd with
    | Wait ->
      begin match Reqd.output_state reqd with
      | Complete ->
        (* this branch happens if the writer has completed sending the response
           and there are still bytes remaining to be read in the request body.
         *)
        Reader.next t.reader
      | Waiting | Ready ->
        (* `Wait` signals that we should add backpressure to the read channel,
         * meaning the reader should tell the runtime to yield.
         *
         * The exception here is if there has been an error in the parser; in
         * that case, we need to return that exception and signal the runtime to
         * close. *)
        begin match Reader.next t.reader with
        | `Error _ as operation -> operation
        | _ -> `Yield
        end
      end
    | Ready -> Reader.next t.reader
    | Complete -> _final_read_operation_for t reqd
  )

and _final_read_operation_for t reqd =
  if Reader.is_closed t.reader || not (Reqd.persistent_connection reqd) then (
    shutdown_reader t;
    Reader.next t.reader;
  ) else
    match Reqd.output_state reqd with
    | Waiting | Ready -> `Yield
    | Complete       ->
      (* The "final read" operation for a request descriptor that is
       * `Complete` from both input and output perspectives needs to account
       * for the fact that the reader may not have finished reading the
       * request body.
       * It's important that we don't advance the request queue in this case
       * for persistent connections, or we'd break the invariant that a
       * non-empty `request_queue` has had the request handler called on its
       * head element. *)
       match Reader.next t.reader with
       | `Error _ as op ->
         (* Keep reading when in a "partial" state (`Read).
          * Don't advance the request queue if in an error state. *)
         op
       | `Read as op ->
         (* we just don't advance the request queue in the case of a parser
           error. *)
         advance_request_queue t;
         op
       | _ ->
         advance_request_queue t;
         _next_read_operation t

let next_read_operation t =
  match _next_read_operation t with
  | `Error (`Parse _)             -> set_error_and_handle          t `Bad_request; `Close
  | `Error (`Bad_request request) -> set_error_and_handle ~request t `Bad_request; `Close
  | `Start | `Read -> `Read
  | (`Yield | `Close) as operation -> operation

let read_with_more t bs ~off ~len more =
  let call_handler = Queue.is_empty t.request_queue in
  let consumed = Reader.read_with_more t.reader bs ~off ~len more in
  if is_active t
  then (
    let reqd = current_reqd_exn t in
    if call_handler
    then t.request_handler reqd;
    Reqd.flush_request_body reqd;
  );
  consumed
;;

let read t bs ~off ~len =
  read_with_more t bs ~off ~len Incomplete

let read_eof t bs ~off ~len =
  read_with_more t bs ~off ~len Complete

let flush_response_error_body t response_state =
  Response_state.flush_response_body response_state t.writer

let rec _next_write_operation t =
  if not (is_active t) then (
    match t.error_code with
    | No_error ->
      if Reader.is_closed t.reader
      then shutdown t;
      Writer.next t.writer
    | Error { response_state; _ } ->
      match Response_state.output_state response_state with
      | Waiting -> `Yield
      | Ready ->
        flush_response_error_body t response_state;
        Writer.next t.writer
      | Complete ->
        shutdown_writer t;
        Writer.next t.writer
  ) else (
    let reqd = current_reqd_exn t in
    match Reqd.output_state reqd with
    | Waiting -> Writer.next t.writer
    | Ready ->
      Reqd.flush_response_body reqd;
      Writer.next t.writer
    | Complete -> _final_write_operation_for t reqd
  )

and _final_write_operation_for t reqd =
  if not (Reqd.persistent_connection reqd) then (
    shutdown_writer t;
    wakeup_reader t;
    Writer.next t.writer;
  ) else (
    match Reqd.input_state reqd with
    | Wait ->
      wakeup_reader t;
      Writer.next t.writer
    | Ready ->
      (* we can't close the request body here, otherwise the reader loop is
         going to think that its "input state" is complete, and remove the
         request descriptor from the request queue, when in fact it needs to
         read the remainder of the request body. It needs to hang around
         because there could be a sudden EOF while discarding the request body,
         which we need to handle. *)
      wakeup_reader t;
      Writer.next t.writer
    | Complete ->
       match Reader.next t.reader with
       | `Error _ | `Read  ->
         Writer.next t.writer
       | _ ->
         advance_request_queue t;
         wakeup_reader t;
         _next_write_operation t
  )
;;

let next_write_operation t = _next_write_operation t

let report_write_result t result =
  Writer.report_result t.writer result

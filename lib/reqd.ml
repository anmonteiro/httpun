(*----------------------------------------------------------------------------
  Copyright (c) 2017 Inhabited Type LLC.

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

  3. Neither the name of the author nor the names of his contributors may be
  used to endorse or promote products derived from this software without
  specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
  EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

type error =
  [ `Bad_request
  | `Bad_gateway
  | `Internal_server_error
  | `Exn of exn
  ]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> Body.Writer.t) -> unit

module Reader = Parse.Reader
module Writer = Serialize.Writer

(* XXX(seliopou): The current design assumes that a new [Reqd.t] will be
 * allocated for each new request/response on a connection. This is wasteful,
 * as it creates garbage on persistent connections. A better approach would be
 * to allocate a single [Reqd.t] per connection and reuse it across
 * request/responses. This would allow a single [Faraday.t] to be allocated for
 * the body and reused. The [response_state] type could then be inlined into
 * the [Reqd.t] record, with dummy values occuping the fields for [response].
 * Something like this:
 *
 * {[
 *   type 'handle t =
 *     { mutable request        : Request.t
 *     ; mutable request_body   : Response.Body.Reader.t
 *     ; mutable response       : Response.t (* Starts off as a dummy value,
 *                                            * using [(==)] to identify it when
 *                                            * necessary *)
 *     ; mutable response_body  : Response.Body.Writer.t
 *     ; mutable persistent     : bool
 *     ; mutable response_state : [ `Waiting | `Started | `Streaming ]
 *     }
 *  ]}
 *
 * *)
type t =
  { request : Request.t
  ; request_body : Body.Reader.t
  ; reader : Reader.request
  ; writer : Writer.t
  ; response_body_buffer : Bigstringaf.t
  ; error_handler : error_handler
  ; mutable persistent : bool
  ; mutable response_state : Response_state.t
  ; mutable error_code : [ `Ok | error ]
  ; proxy : bool
  }

let create
      ~error_handler
      ~reader
      ~writer
      ~response_body_buffer
      ~proxy
      request
      request_body
  =
  { request
  ; request_body
  ; reader
  ; writer
  ; response_body_buffer
  ; error_handler
  ; persistent = Request.persistent_connection request
  ; response_state = Waiting
  ; error_code = `Ok
  ; proxy
  }

let request { request; _ } = request
let request_body { request_body; _ } = request_body

let response { response_state; _ } =
  match response_state with
  | Waiting -> None
  | Streaming (response, _) | Fixed response | Upgrade response -> Some response

let response_exn { response_state; _ } =
  match response_state with
  | Waiting -> failwith "httpun.Reqd.response_exn: response has not started"
  | Streaming (response, _) | Fixed response | Upgrade response -> response

let respond_with_string t response str =
  if t.error_code <> `Ok
  then
    failwith
      "httpun.Reqd.respond_with_string: invalid state, currently handling error";
  match t.response_state with
  | Waiting ->
    (* XXX(seliopou): check response body length *)
    Writer.write_response t.writer response;
    Writer.write_string t.writer str;
    if t.persistent then t.persistent <- Response.persistent_connection response;
    t.response_state <- Fixed response;
    Writer.wakeup t.writer
  | Streaming _ | Upgrade _ ->
    failwith "httpun.Reqd.respond_with_string: response already started"
  | Fixed _ ->
    failwith "httpun.Reqd.respond_with_string: response already complete"

let respond_with_bigstring t response (bstr : Bigstringaf.t) =
  if t.error_code <> `Ok
  then
    failwith
      "httpun.Reqd.respond_with_bigstring: invalid state, currently handling \
       error";
  match t.response_state with
  | Waiting ->
    (* XXX(seliopou): check response body length *)
    Writer.write_response t.writer response;
    Writer.schedule_bigstring t.writer bstr;
    if t.persistent then t.persistent <- Response.persistent_connection response;
    t.response_state <- Fixed response;
    Writer.wakeup t.writer
  | Streaming _ | Upgrade _ ->
    failwith "httpun.Reqd.respond_with_bigstring: response already started"
  | Fixed _ ->
    failwith "httpun.Reqd.respond_with_bigstring: response already complete"

let unsafe_respond_with_streaming ~flush_headers_immediately t response =
  match t.response_state with
  | Waiting ->
    let encoding =
      match
        Response.body_length
          ~proxy:t.proxy
          ~request_method:t.request.meth
          response
      with
      | (`Fixed _ | `Close_delimited | `Chunked) as encoding -> encoding
      | `Error (`Bad_gateway | `Internal_server_error) ->
        failwith
          "httpun.Reqd.respond_with_streaming: invalid response body length"
    in
    let response_body =
      Body.Writer.create t.response_body_buffer ~encoding ~writer:t.writer
    in
    Writer.write_response t.writer response;
    if t.persistent then t.persistent <- Response.persistent_connection response;
    t.response_state <- Streaming (response, response_body);
    if flush_headers_immediately then Writer.wakeup t.writer;
    response_body
  | Streaming _ | Upgrade _ ->
    failwith "httpun.Reqd.respond_with_streaming: response already started"
  | Fixed _ ->
    failwith "httpun.Reqd.respond_with_streaming: response already complete"

let respond_with_streaming ?(flush_headers_immediately = false) t response =
  if t.error_code <> `Ok
  then
    failwith
      "httpun.Reqd.respond_with_streaming: invalid state, currently handling \
       error";
  unsafe_respond_with_streaming ~flush_headers_immediately t response

let unsafe_respond_with_upgrade t headers upgrade_handler =
  match t.response_state with
  | Waiting ->
    let response = Response.create ~headers `Switching_protocols in
    Writer.write_response t.writer response;
    if t.persistent then t.persistent <- Response.persistent_connection response;
    t.response_state <- Upgrade response;
    Writer.flush t.writer (fun _reason ->
      (* TODO(anmonteiro): probably need to check `Closed here? *)
      upgrade_handler ());
    Body.Reader.close t.request_body;
    Writer.wakeup t.writer
  | Streaming _ | Upgrade _ ->
    failwith "httpun.Reqd.unsafe_respond_with_upgrade: response already started"
  | Fixed _ ->
    failwith
      "httpun.Reqd.unsafe_respond_with_upgrade: response already complete"

let respond_with_upgrade t response upgrade_handler =
  if t.error_code <> `Ok
  then
    failwith
      "httpun.Reqd.respond_with_streaming: invalid state, currently handling \
       error";
  unsafe_respond_with_upgrade t response upgrade_handler

let report_error t error =
  t.persistent <- false;
  match t.response_state, t.error_code with
  | Waiting, `Ok ->
    t.error_code <- (error :> [ `Ok | error ]);
    let status =
      match (error :> [ error | Status.standard ]) with
      | `Exn _ -> `Internal_server_error
      | #Status.standard as status -> status
    in
    t.error_handler ~request:t.request error (fun headers ->
      let response_body =
        unsafe_respond_with_streaming
          t
          ~flush_headers_immediately:true
          (Response.create ~headers status)
      in
      (* NOTE(anmonteiro): When reporting an error that calls the error handler,
         we can only deliver an EOF to the request body once the error response
         has started. Otherwise, the request body `on_eof` handler could
         erroneously send a successful response instead of letting us handle the
         error. *)
      Body.Reader.close t.request_body;
      response_body)
  | other ->
    Body.Reader.close t.request_body;
    (match other with
    | Waiting, `Exn _ ->
      (* XXX(seliopou): Decide what to do in this unlikely case. There is an *
         outstanding call to the [error_handler], but an intervening exception *
         has been reported as well. *)
      failwith "httpun.Reqd.report_exn: NYI"
    | Streaming (_response, response_body), `Ok ->
      Body.Writer.force_close response_body;
      Reader.wakeup t.reader
    | Streaming (_response, response_body), `Exn _ ->
      Body.Writer.close response_body;
      Writer.close_and_drain t.writer;
      Reader.wakeup t.reader
    | (Fixed _ | Streaming _ | Upgrade _ | Waiting), _ ->
      (* XXX(seliopou): Once additional logging support is added, log the error
         * in case it is not spurious. *)
      ())

let report_exn t exn = report_error t (`Exn exn)

let try_with t f : (unit, exn) result =
  try
    f ();
    Ok ()
  with
  | exn ->
    report_exn t exn;
    Error exn

(* Private API, not exposed to the user through httpun.mli *)

let close_request_body { request_body; _ } = Body.Reader.close request_body

let error_code t =
  match t.error_code with #error as error -> Some error | `Ok -> None

let persistent_connection t = t.persistent

let input_state t : Io_state.t =
  match t.response_state, t.request.meth with
  | Upgrade _, _ | _, `CONNECT -> Wait
  | _ ->
    if Body.Reader.is_closed t.request_body
    then Complete
    else if Body.Reader.is_read_scheduled t.request_body
    then Ready
    else Wait

let output_state { request; response_state; writer; _ } =
  Response_state.output_state
    response_state
    ~request_method:request.meth
    ~writer

let flush_request_body t =
  if Body.Reader.has_pending_output t.request_body
  then
    try Body.Reader.execute_read t.request_body with exn -> report_exn t exn

let flush_response_body t = Response_state.flush_response_body t.response_state

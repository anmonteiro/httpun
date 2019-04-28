module Writer = Serialize.Writer

type state =
  | Uninitialized
  | Awaiting_response
  | Received_response of Response.t * [`read] Body.t
  | Closed

type t =
  { request          : Request.t
  ; request_body     : [ `write ] Body.t
  ; response_handler : (Response.t -> [`read] Body.t -> unit)
  ; writer : Writer.t
  ; mutable state    : state
  ; mutable persistent      : bool
  }

let create request request_body response_handler writer =
  let rec handler response body =
    let t = Lazy.force t in
    if t.persistent then
      t.persistent <- Response.persistent_connection response;
    t.state <- Received_response(response, body);
    response_handler response body
  and t =
    lazy
    { request
    ; request_body
    ; response_handler = handler
    ; writer
    ; state = Uninitialized
    ; persistent = Request.persistent_connection request
    }
  in
  Lazy.force t

let request { request; _ } = request

let request_body { request_body; _ } = request_body

let write_request t =
  Writer.write_request t.writer t.request;
  t.state <- Awaiting_response

let on_more_output_available { request_body; _ } f =
  Body.when_ready_to_write request_body f

let persistent_connection t =
  t.persistent

let close_response_body t =
  match t.state with
  | Uninitialized
  | Awaiting_response
  | Closed -> ()
  | Received_response (_, response_body) ->
    Body.close_reader response_body

let requires_input t =
  match t.state with
  | Uninitialized -> true
  | Awaiting_response -> true
  | Received_response (_, response_body) ->
    not (Body.is_closed response_body)
    || Body.has_pending_output response_body
  | Closed -> false

let requires_output { request_body; state; _ } =
  state = Uninitialized || not (Body.is_closed request_body)

let is_complete t =
  not (requires_input t || requires_output t)

let flush_request_body { request; request_body; writer; _ } =
  if Body.has_pending_output request_body
  then
    let encoding =
      match Request.body_length request with
      | `Fixed _ | `Chunked as encoding -> encoding
      | `Error _ -> assert false (* XXX(seliopou): This needs to be handled properly *)
    in
    Body.transfer_to_writer_with_encoding request_body ~encoding writer

let flush_response_body t =
  match t.state with
  | Uninitialized | Awaiting_response | Closed -> ()
  | Received_response(_, response_body) ->
    try Body.execute_read response_body
    (* TODO: report_exn *)
    with _exn -> ()
    (* report_exn t exn *)

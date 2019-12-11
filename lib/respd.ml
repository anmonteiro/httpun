module Writer = Serialize.Writer

type error =
  [ `Malformed_response of string
  | `Invalid_response_body_length of Response.t
  | `Exn of exn ]

type state =
  | Uninitialized
  | Awaiting_response
  | Received_response of Response.t * [`read] Body.t
  | Upgraded of Response.t
  | Closed

type t =
  { request          : Request.t
  ; request_body     : [ `write ] Body.t
  ; response_handler : (Response.t -> [`read] Body.t -> unit)
  ; error_handler    : (error -> unit)
  ; mutable error_code : [ `Ok | error ]
  ; writer : Writer.t
  ; mutable state    : state
  ; mutable persistent      : bool
  }

let create error_handler request request_body writer response_handler =
  let rec handler response body =
    let t = Lazy.force t in
    if t.persistent then
      t.persistent <- Response.persistent_connection response;
    let next_state = match response.status with
      | `Switching_protocols ->
        Upgraded response
      | _ ->
        Received_response (response, body)
    in
    t.state <- next_state;
    response_handler response body
  and t =
    lazy
    { request
    ; request_body
    ; response_handler = handler
    ; error_handler
    ; error_code = `Ok
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

let report_error t error =
  (* t.persistent <- false; *)
  (* TODO: drain queue? *)
  match t.state, t.error_code with
  | (Uninitialized | Awaiting_response | Received_response _ | Upgraded _), `Ok ->
    t.state <- Closed;
    t.error_code <- (error :> [`Ok | error]);
    t.error_handler error
  | Uninitialized, `Exn _ ->
    (* TODO(anmonteiro): Not entirely sure this is possible in the client. *)
    failwith "httpaf.Reqd.report_exn: NYI"
  | (Uninitialized | Awaiting_response | Received_response _ | Closed | Upgraded _), _ ->
    (* XXX(seliopou): Once additional logging support is added, log the error
     * in case it is not spurious. *)
    ()

let persistent_connection t =
  t.persistent

let close_response_body t =
  match t.state with
  | Uninitialized
  | Awaiting_response
  | Closed -> ()
  | Received_response (_, response_body) ->
    Body.close_reader response_body
  | Upgraded _ -> t.state <- Closed

let requires_input t =
  match t.state with
  | Uninitialized -> true
  | Awaiting_response -> true
  | Upgraded _ -> false
  | Received_response (_, response_body) ->
    not (Body.is_closed response_body)
  | Closed -> false

let requires_output { request_body; state; _ } =
  match state with
  | Upgraded _ ->
    (* XXX(anmonteiro): Connections that have been upgraded "require output"
     * forever, but outside the HTTP layer, meaning they're permanently
     * "yielding". For now they need to be explicitly shutdown in order to
     * transition the response descriptor to the `Closed` state. *)
    true
  | state ->
    state = Uninitialized ||
    not (Body.is_closed request_body) ||
    Body.has_pending_output request_body

let is_complete t =
  not (requires_input t || requires_output t)

let flush_request_body { request; request_body; writer; _ } =
  if Body.has_pending_output request_body then begin
    let encoding =
      match Request.body_length request with
      | `Fixed _ | `Chunked as encoding -> encoding
      | `Error _ -> assert false (* XXX(seliopou): This needs to be handled properly *)
    in
    Body.transfer_to_writer_with_encoding request_body ~encoding writer
  end

let flush_response_body t =
  match t.state with
  | Uninitialized | Awaiting_response | Closed | Upgraded _ -> ()
  | Received_response(_, response_body) ->
    try Body.execute_read response_body
    (* TODO: report_exn *)
    with _exn ->
      Format.eprintf "EXN@."
    (* report_exn t exn *)

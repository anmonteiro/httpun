type ('handle, 'io) t =
  | Waiting   of Optional_thunk.t ref
  | Complete  of Response.t
  | Streaming of Response.t * [`write] Body.t
  | Upgrade of Response.t * ('handle -> 'io)

let upgrade_handler t =
  match t with
  | Upgrade (_, upgrade_handler) ->
    Some upgrade_handler
  | _ -> None

let on_more_output_available t f =
  match t with
  | Waiting when_done_waiting ->
    if Optional_thunk.is_some !when_done_waiting
    then failwith "httpaf.Reqd.on_more_output_available: only one callback can be registered at a time";
    when_done_waiting := Optional_thunk.some f
  | Streaming(_, response_body) ->
    Body.when_ready_to_write response_body f
  | Complete _ ->
    failwith "httpaf.Reqd.on_more_output_available: response already complete"
  | Upgrade _ ->
    (* XXX(anmonteiro): Connections that have been upgraded "require output"
     * forever, but outside the HTTP layer, meaning they're permanently
     * "yielding". We don't register the wakeup callback because it's not going
     * to get called. *)
    ()

let output_state t : Output_state.t =
  match t with
  | Complete _ -> Complete
  | Waiting _ -> Wait
  | Streaming(_, response_body) ->
    if Body.requires_output response_body
    then Consume
    else Complete
  | Upgrade _ -> Consume

let flush_response_body t ~request_method writer =
  match t with
  | Streaming (response, response_body) ->
    let encoding =
      match Response.body_length ~request_method response with
      | `Fixed _ | `Close_delimited | `Chunked as encoding -> encoding
      | `Error _ -> assert false (* XXX(seliopou): This needs to be handled properly *)
    in
    Body.transfer_to_writer_with_encoding response_body ~encoding writer
  | _ -> ()

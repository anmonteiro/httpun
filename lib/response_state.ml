type t =
  | Waiting
  | Complete  of Response.t
  | Streaming of Response.t * [`write] Body.t
  | Upgrade of Response.t * (unit -> unit)

let output_state t : Output_state.t =
  match t with
  | Complete _ -> Complete
  | Waiting -> Wait
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

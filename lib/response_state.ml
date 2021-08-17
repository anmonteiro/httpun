type t =
  | Waiting
  | Fixed  of Response.t
  | Streaming of Response.t * Body.Writer.t
  | Upgrade of Response.t * (unit -> unit)

let output_state t : Output_state.t =
  match t with
  | Fixed _ -> Complete
  | Waiting -> Waiting
  | Streaming(_, response_body) ->
    if Body.Writer.requires_output response_body
    then Ready
    else Complete
  | Upgrade _ -> Ready

let flush_response_body t writer =
  match t with
  | Streaming (_, response_body) ->
    Body.Writer.transfer_to_writer response_body writer
  | _ -> ()

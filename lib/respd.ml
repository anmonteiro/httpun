module Writer = Serialize.Writer

type error =
  [ `Malformed_response of string
  | `Invalid_response_body_length of Response.t
  | `Exn of exn
  ]

module Request_state = struct
  type t =
    | Uninitialized
    | Awaiting_response
    | Received_response of Response.t * Body.Reader.t
    | Upgraded of Response.t
    | Closed
end

type t =
  { request : Request.t
  ; request_body : Body.Writer.t
  ; response_handler : Response.t -> Body.Reader.t -> unit
  ; error_handler : error -> unit
  ; mutable error_code : [ `Ok | error ]
  ; writer : Writer.t
  ; mutable state : Request_state.t
  ; mutable persistent : bool
  }

let create ~error_handler ~writer ~response_handler request request_body =
  let handler t =
   fun response body ->
    let t = Lazy.force t in
    if t.persistent then t.persistent <- Response.persistent_connection response;
    let next_request_state =
      match request.Request.meth, response.status with
      (* From RFC9110§6.4.1: * 2xx (Successful) responses to a CONNECT request
         method (Section * 9.3.6) switch the connection to tunnel mode instead
         of having * content. *)
      | `CONNECT, #Status.successful | _, `Switching_protocols ->
        Request_state.Upgraded response
      | _ -> Received_response (response, body)
    in
    t.state <- next_request_state;
    response_handler response body
  in
  let rec t =
    lazy
      { request
      ; request_body
      ; response_handler = handler t
      ; error_handler
      ; error_code = `Ok
      ; writer
      ; state = Uninitialized
      ; persistent = Request.persistent_connection request
      }
  in
  Lazy.force t

let request { request; _ } = request

let write_request t =
  Writer.write_request t.writer t.request;
  t.state <- Awaiting_response

let report_error t error =
  t.persistent <- false;
  Body.Writer.force_close t.request_body;
  match t.state, t.error_code with
  | (Uninitialized | Awaiting_response | Upgraded _), `Ok ->
    t.state <- Closed;
    t.error_code <- (error :> [ `Ok | error ]);
    t.error_handler error
  | Uninitialized, `Exn _ ->
    (* TODO(anmonteiro): Not entirely sure this is possible in the client. *)
    assert false
  | Received_response (_, response_body), `Ok ->
    t.error_code <- (error :> [ `Ok | error ]);
    t.error_handler error;
    Body.Reader.close response_body
  | ( ( Uninitialized | Awaiting_response | Received_response _ | Closed
      | Upgraded _ )
    , _ ) ->
    (* XXX(seliopou): Once additional logging support is added, log the error *
       in case it is not spurious. *)
    ()

let persistent_connection t = t.persistent
let close_request_body t = Body.Writer.close t.request_body

let close_response_body t =
  match t.state with
  | Uninitialized | Awaiting_response | Closed -> ()
  | Received_response (_, response_body) -> Body.Reader.close response_body
  | Upgraded _ -> t.state <- Closed

let input_state t : Io_state.t =
  match t.state with
  | Uninitialized | Awaiting_response -> Ready
  | Received_response (_, response_body) ->
    if Body.Reader.is_closed response_body
    then Complete
    else if Body.Reader.is_read_scheduled response_body
    then Ready
    else Wait
  | Upgraded _ -> Wait
  | Closed -> Complete

let output_state { request_body; state; writer; _ } : Io_state.t =
  match state with
  | Upgraded _ ->
    (* XXX(anmonteiro): Connections that have been upgraded "require output" *
       forever, but outside the HTTP layer, meaning they're permanently *
       "yielding". For now they need to be explicitly shutdown in order to *
       transition the response descriptor to the `Closed` state. *)
    Wait
  | state ->
    if Writer.is_closed writer
    then Complete
    else if state = Uninitialized || Body.Writer.requires_output request_body
    then Ready
    else Complete

let flush_request_body { request_body; _ } =
  if Body.Writer.has_pending_output request_body
  then Body.Writer.transfer_to_writer request_body

let flush_response_body t =
  match t.state with
  | Uninitialized | Awaiting_response | Closed | Upgraded _ -> ()
  | Received_response (_, response_body) ->
    if Body.Reader.has_pending_output response_body
    then (
      try Body.Reader.execute_read response_body with
      | exn -> report_error t (`Exn exn))

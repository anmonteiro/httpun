(*----------------------------------------------------------------------------
    Copyright (c) 2016 Inhabited Type LLC.

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


include Angstrom

module P = struct
  let is_space =
    function | ' ' | '\t' -> true | _ -> false

  let is_cr =
    function | '\r' -> true | _ -> false

  let is_space_or_colon =
    function | ' ' | '\t' | ':' -> true | _ -> false

  let is_hex =
    function | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

  let is_digit =
    function '0' .. '9' -> true | _ -> false

  let is_separator =
    function
      | ')' | '(' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"'
      | '/' | '[' | ']' | '?' | '=' | '{' | '}' | ' ' | '\t' -> true
      | _ -> false

  let is_token =
    (* The commented-out ' ' and '\t' are not necessary because of the range at
     * the top of the match. *)
    function
      | '\000' .. '\031' | '\127'
      | ')' | '(' | '<' | '>' | '@' | ',' | ';' | ':' | '\\' | '"'
      | '/' | '[' | ']' | '?' | '=' | '{' | '}' (* | ' ' | '\t' *) -> false
      | _ -> true
end

let unit = return ()
let token = take_while1 P.is_token
let spaces = skip_while P.is_space

let digit =
  satisfy P.is_digit
  >>| function
    | '0' -> 0 | '1' -> 1 | '2' -> 2 | '3' -> 3 | '4' -> 4 | '5' -> 5
    | '6' -> 6 | '7' -> 7 | '8' -> 8 | '9' -> 9 | _ -> assert false

let eol = string "\r\n" <?> "eol"
let hex str =
  try return (Int64.of_string ("0x" ^ str)) with _ -> fail "hex"
let skip_line = take_till P.is_cr *> eol

let version =
  string "HTTP/" *>
  lift2 (fun major minor -> { Version.major; minor })
    (digit <* char '.')
    digit

let header =
  (* From RFC7230§3.2.4:

       "No whitespace is allowed between the header field-name and colon.  In
       the past, differences in the handling of such whitespace have led to
       security vulnerabilities in request routing and response handling.  A
       server MUST reject any received request message that contains whitespace
       between a header field-name and colon with a response code of 400 (Bad
       Request).  A proxy MUST remove any such whitespace from a response
       message before forwarding the message downstream."

    This can be detected by checking the message and marks in a parse failure,
    which should look like this when serialized "... > header > :". *)
  lift2 (fun key value -> (key, value))
    (take_till P.is_space_or_colon <* char ':' <* spaces)
    (take_till P.is_cr <* eol >>| String.trim)
  <* commit
  <?> "header"

let headers =
  let cons x xs = x :: xs in
  fix (fun headers ->
    let _emp = return [] in
    let _rec = lift2 cons header headers in
    peek_char_fail
    >>= function
      | '\r' -> _emp
      | _    -> _rec)
  >>| Headers.of_list

let request =
  let meth = take_till P.is_space >>| Method.of_string in
  lift4 (fun meth target version headers ->
    Request.create ~version ~headers meth target)
    (meth                 <* char ' ')
    (take_till P.is_space <* char ' ')
    (version              <* eol <* commit)
    (headers              <* eol)

let response =
  let status =
    take_while P.is_digit
    >>= fun str ->
      if String.length str = 0
      then fail "status-code empty"
      else (
        if String.length str > 3
        then fail (Printf.sprintf "status-code too long: %S" str)
        else return (Status.of_string str))
  in
  lift4 (fun version status reason headers ->
    Response.create ~reason ~version ~headers status)
    (version              <* char ' ')
    (status               <* char ' ')
    (take_till P.is_cr    <* eol <* commit)
    (headers              <* eol)

let finish body =
  Body.Reader.close body;
  commit

let schedule_size body n =
  let faraday = Body.Reader.unsafe_faraday body in
  (* XXX(seliopou): performance regression due to switching to a single output
   * format in Farady. Once a specialized operation is exposed to avoid the
   * intemediate copy, this should be back to the original performance. *)
  begin if Faraday.is_closed faraday
  then advance n
  else take_bigstring n >>| fun s -> Faraday.schedule_bigstring faraday s
  end *> commit

let body ~encoding body =
  let rec fixed n ~unexpected =
    if n = 0L
    then unit
    else
      at_end_of_input
      >>= function
        | true -> commit *> fail unexpected
        | false ->
          available >>= fun m ->
          let m' = Int64.(min (of_int m) n) in
          let n' = Int64.sub n m' in
          schedule_size body (Int64.to_int m') >>= fun () -> fixed n' ~unexpected
  in
  match encoding with
  | `Fixed n ->
    fixed n ~unexpected:"expected more from fixed body"
    >>= fun () -> finish body
  | `Chunked ->
    (* XXX(seliopou): The [eol] in this parser should really parse a collection
     * of "chunk extensions", as defined in RFC7230§4.1. These do not show up
     * in the wild very frequently, and the httpaf API has no way of exposing
     * them to the suer, so for now the parser does not attempt to recognize
     * them. This means that any chunked messages that contain chunk extensions
     * will fail to parse. *)
    fix (fun p ->
      let _hex =
        (take_while1 P.is_hex >>= fun size -> hex size)
        (* swallows chunk-ext, if present, and CRLF *)
        <* (eol *> commit)
      in
      _hex >>= fun size ->
      if size = 0L
      then eol *> finish body
      else fixed size ~unexpected:"expected more from body chunk" *> eol *> p)
  | `Close_delimited ->
    fix (fun p ->
      let _rec = (available >>= fun n -> schedule_size body n) *> p in
      at_end_of_input
      >>= function
        | true  -> finish body
        | false -> _rec)

module Reader = struct
  module AU = Angstrom.Unbuffered

  type request_error = [
    | `Bad_request of Request.t
    | `Parse of string list * string ]

  type response_error = [
    | `Invalid_response_body_length of Response.t
    | `Parse of string list * string ]

  type 'error parse_state =
    | Done
    | Fail    of 'error
    | Partial of (Bigstringaf.t -> off:int -> len:int -> AU.more -> (unit, 'error) result AU.state)

  type 'error t =
    { parser              : (unit, 'error) result Angstrom.t
    ; mutable parse_state : 'error parse_state
      (* The state of the parse for the current request *)
    ; mutable closed      : bool
      (* Whether the input source has left the building, indicating that no
       * further input will be received. *)
    ; mutable wakeup      : Optional_thunk.t
    }

  type request  = request_error t
  type response = response_error t

  let create parser =
    { parser
    ; parse_state = Done
    ; closed      = false
    ; wakeup      = Optional_thunk.none
    }

  let ok = return (Ok ())

  let is_closed t =
    t.closed

  let on_wakeup t k =
    if is_closed t
    then failwith "on_wakeup on closed reader"
    else if Optional_thunk.is_some t.wakeup
    then failwith "on_wakeup: only one callback can be registered at a time"
    else t.wakeup <- Optional_thunk.some k

  let wakeup t =
    let f = t.wakeup in
    t.wakeup <- Optional_thunk.none;
    Optional_thunk.call_if_some f

  let request ~wakeup handler =
    let parser handler =
      request <* commit >>= fun request ->
        match Request.body_length request with
      | `Error `Bad_request -> return (Error (`Bad_request request))
      | `Fixed 0L  ->
        handler request (Body.Reader.create_empty ());
        ok
      | `Fixed _ | `Chunked as encoding ->
        let request_body =
          Body.Reader.create
            Bigstringaf.empty
            ~when_ready_to_read:(Optional_thunk.some wakeup)
        in
        handler request request_body;
        body ~encoding request_body *> ok
    in
    create (parser handler)

  let response request_queue =
    let parser t request_queue =
      response <* commit >>= fun response ->
      assert (not (Queue.is_empty request_queue));
      let exception Local of Respd.t in
      let respd = match
        (Queue.iter (fun respd ->
          if respd.Respd.state = Awaiting_response then
            raise (Local respd)) request_queue)
        with
        | exception Local respd -> respd
        | _ -> assert false
      in
      let request = Respd.request respd in
      let proxy = false in
      match Response.body_length ~request_method:request.meth response with
      | `Error `Bad_gateway           -> assert (not proxy); assert false
      | `Error `Internal_server_error -> return (Error (`Invalid_response_body_length response))
      | `Fixed 0L ->
        respd.response_handler response (Body.Reader.create_empty ());
        ok
      | `Fixed _ | `Chunked | `Close_delimited as encoding ->
        (* We do not trust the length provided in the [`Fixed] case, as the
           client could DOS easily. *)
        let response_body =
          Body.Reader.create Bigstringaf.empty ~when_ready_to_read:(Optional_thunk.some (fun () ->
            wakeup (Lazy.force t)))
        in
        respd.response_handler response response_body;
        body ~encoding response_body *> ok
    in
    let rec t = lazy (create (parser t request_queue)) in
    Lazy.force t
  ;;


  let transition t state =
    match state with
    | AU.Done(consumed, Ok ()) ->
      t.parse_state <- Done;
      consumed
    | AU.Done(consumed, Error error) ->
      t.parse_state <- Fail error;
      consumed
    | AU.Fail(consumed, marks, msg) ->
      t.parse_state <- Fail (`Parse(marks, msg));
      consumed
    | AU.Partial { committed; continue } ->
      t.parse_state <- Partial continue;
      committed
  and start t state =
      match state with
      | AU.Done _         -> failwith "httpaf.Parse.unable to start parser"
      | AU.Fail(0, marks, msg) ->
        t.parse_state <- Fail (`Parse(marks, msg))
      | AU.Partial { committed = 0; continue } ->
        t.parse_state <- Partial continue
      | _ -> assert false
  ;;

  let rec _read_with_more t bs ~off ~len more =
    let initial = match t.parse_state with Done -> true | _ -> false in
    let consumed =
      match t.parse_state with
      | Fail _ -> 0
      (* Don't feed empty input when we're at a request boundary *)
      | Done when len = 0 -> 0
      | Done   ->
        start t (AU.parse t.parser);
        _read_with_more  t bs ~off ~len more;
      | Partial continue ->
        transition t (continue bs more ~off ~len)
    in
    (* Special case where the parser just started and was fed a zero-length
     * bigstring. Avoid putting them parser in an error state in this scenario.
     * If we were already in a `Partial` state, return the error. *)
    if initial && len = 0 then t.parse_state <- Done;
    match t.parse_state with
    | Done when consumed < len ->
      let off = off + consumed
      and len = len - consumed in
      consumed + _read_with_more t bs ~off ~len more
    | _ -> consumed
  ;;

  let read_with_more t bs ~off ~len more =
    let consumed = _read_with_more t bs ~off ~len more in
    (match more with
     | Complete ->
       t.closed <- true
     | Incomplete -> ());
    consumed

  let force_close t =
    t.closed <- true;
  ;;

  let next t =
    match t.parse_state with
    | Fail failure -> `Error failure
    | _ when t.closed -> `Close
    | Done      -> `Start
    | Partial _ -> `Read
  ;;
end

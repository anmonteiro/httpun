open Httpaf
open Helpers
open Client_connection

module Response = struct
  include Response

  let pp = pp_hum
  let equal x y = x = y
end

let feed_string t str =
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  read t input ~off:0 ~len

let read_string t str =
  let c = feed_string t str in
  Alcotest.(check int) "read consumes all input" (String.length str) c;
;;

let read_response t r =
  let response_string = response_to_string r in
  read_string t response_string
;;

let reader_ready t =
  Alcotest.check read_operation "Reader is ready"
    `Read (next_read_operation t :> [`Close | `Read | `Yield]);
;;

let reader_yielded t =
  Alcotest.check read_operation "Reader is in a yield state"
    `Yield (next_read_operation t :> [`Close | `Read | `Yield]);
;;

let write_string ?(msg="output written") t str =
  let len = String.length str in
  Alcotest.(check (option string)) msg
    (Some str)
    (next_write_operation t |> Write_operation.to_write_as_string);
  report_write_result t (`Ok len);
;;

let write_request ?(msg="request written") t r =
  let request_string = request_to_string r in
  write_string ~msg t request_string
;;

let writer_yielded t =
  Alcotest.check write_operation "Writer is in a yield state"
    `Yield (next_write_operation t);
;;

let writer_closed t =
  Alcotest.check write_operation "Writer is closed"
    (`Close 0) (next_write_operation t);
;;

let reader_closed t =
  Alcotest.check read_operation "Reader is closed"
    `Close (next_read_operation t :> [`Close | `Read | `Yield])

let connection_is_shutdown t =
  reader_closed t;
  writer_closed t;
;;

let default_response_handler expected_response response body =
  Alcotest.check (module Response) "expected response" expected_response response;
  let on_read _ ~off:_ ~len:_ = () in
  let on_eof () = () in
  Body.schedule_read body ~on_read ~on_eof;
;;

let no_error_handler _ = assert false

let test_commit_parse_after_every_header () =
  let request' = Request.create `GET "/" in
  let response =
    Response.create
      ~headers:(Headers.of_list
        [ "Links", "/path/to/some/website"
        ; "Links", "/path/to/some/website"
        ; "connection", "close"
        ])
      `OK
  in

  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request t request';

  let response_line = "HTTP/1.1 200 OK\r\n" in
  let single_header = "Links: /path/to/some/website\r\n" in
  let r =
    (* Each header is 30 bytes *)
    response_line ^ single_header ^ single_header ^ "connection: close\r\n\r\n"
  in
  let bs = Bigstringaf.of_string r ~off:0 ~len:(String.length r) in
  let c = read t bs ~off:0 ~len:(String.length response_line + 15) in
  Alcotest.(check int) "only reads the response line" (String.length response_line) c;

  let c' =
    read t bs ~off:c ~len:(String.length single_header)
  in
  Alcotest.(check int) "parser can read a single header and commit" (String.length single_header) c';

  let c'' = read_eof t bs ~off:(c + c') ~len:(String.length r - (c + c')) in
  Alcotest.(check int) "read_eof with the rest of the input is accepted" (String.length r - (c + c')) c'';

  connection_is_shutdown t;
;;

let test_get () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in

  (* Single GET *)
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request t request';
  read_response t response;

  (* Single GET, request closes the connection. *)
  let request_close =
    Request.create
      ~headers:(Headers.of_list ["connection", "close"])
      `GET "/"
  in
  let t = create ?config:None in
  let body =
    request
      t
      request_close
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request t request_close;
  writer_closed t;
  read_response t response;

  (* Single GET, response closes connection *)
  let response =
    Response.create `OK ~headers:(Headers.of_list [ "connection", "close" ])
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request t request';
  read_response t response;
  writer_closed t;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;

  (* Single GET, streaming body *)
  let response =
    Response.create `OK ~headers:(Headers.of_list [ "transfer-encoding", "chunked" ])
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request t request';
  read_response t response;
  read_string t "d\r\nHello, world!\r\n0\r\n\r\n";
;;

let test_get_last_close () =
  (* Multiple GET requests, the last one closes the connection *)
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list ["content-length", "0"]) `OK
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request t request';
  read_response t response;

  let request'' =
    Request.create ~headers:(Headers.of_list ["connection", "close"]) `GET "/"
  in
  let body' =
    request
      t
      request''
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body';
  write_request t request'';
  read_response t response;

  writer_closed t;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
;;

let test_response_eof () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in (* not actually writen to the channel *)

  let error_message = ref None in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:(function
        | `Malformed_response msg -> error_message := Some msg
        | _ -> assert false)
  in
  Body.close_writer body;
  write_request t request';
  reader_ready t;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
  Alcotest.(check (option string)) "unexpected eof"
    (Some "unexpected eof")
    !error_message
;;

let test_persistent_connection_requests () =
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request t request';
  read_response t response;
  writer_yielded t;
  reader_ready t;
  let body' =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body';
  write_request t request';
  read_response t response;
;;

let test_persistent_connection_requests_pipelining () =
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request t request';
  (* send the 2nd request without reading the response *)
  let response' =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `Not_found
  in
  let body' =
    request
      t
      request'
      ~response_handler:(fun response body ->
        (default_response_handler response' response body))
      ~error_handler:no_error_handler
  in
  Body.close_writer body';
  write_request t request';
  read_response t response;
  read_response t response';
;;

let test_persistent_connection_requests_pipelining_send_body () =
  let request' =
    Request.create ~headers:(Headers.of_list [ "content-length", "8" ]) `GET "/"
  in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `OK
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  (* send the 2nd request without reading the response *)
  let request'' = Request.create `GET "/" in
  let response' =
    Response.create ~headers:(Headers.of_list [ "content-length", "0" ]) `Not_found
  in
  let body' =
    request
      t
      request''
      ~response_handler:(fun response body ->
        (default_response_handler response' response body))
      ~error_handler:no_error_handler
  in
  Body.close_writer body';
  Body.write_string body "a string";
  Body.close_writer body;
  write_string ~msg:"writes the body for the first request" t "a string";
  write_request t request'';
  read_response t response;
  read_response t response';
;;

let test_persistent_connection_requests_body () =
  let request' = Request.create `GET "/" in
  let request'' = Request.create `GET "/second" in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request t request';
  let response' = Response.create `OK in
  read_response t response;
  read_string t "ten chars.";
  let body' =
    request
      t
      request''
      ~response_handler:(default_response_handler response')
      ~error_handler:no_error_handler
  in
  Body.close_writer body';
  write_request t request'';
  read_response t response';
;;

let test_response_header_order () =
  let request' = Request.create `GET "/" in
  let headers =
    [ "a", "1"
    ; "b", "2"
    ; "c", "3"
    ]
  in
  let response = Response.create `OK ~headers:(Headers.of_list headers) in
  let received = ref None in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(fun response _ -> received := Some response)
      ~error_handler:no_error_handler
  in
  Body.close_writer body;
  write_request t request';
  writer_yielded t;
  read_response t response;
  match !received with
  | None -> assert false
  | Some received ->
    Alcotest.(check (list (pair string string))) "headers are equal"
      headers (Headers.to_list received.headers);
;;

let test_report_exn () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in (* not actually writen to the channel *)

  let error_message = ref None in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:(function
        | `Exn (Failure msg) -> error_message := Some msg
        | _ -> assert false)
  in
  Body.close_writer body;
  write_request  t request';
  writer_yielded  t;
  reader_ready t;
  report_exn t (Failure "something went wrong");
  connection_is_shutdown t;
  Alcotest.(check (option string)) "something went wrong"
    (Some "something went wrong")
    !error_message
;;

let test_input_shrunk () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK in (* not actually writen to the channel *)

  let error_message = ref None in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:(function
        | `Exn (Failure msg) -> error_message := Some msg
        | _ -> assert false)
  in
  Body.close_writer body;
  write_request  t request';
  writer_yielded  t;
  reader_ready t;
  let c = feed_string  t "HTTP/1.1 200 OK\r\nDate" in
  Alcotest.(check int) "read the status line" c 17;
  report_exn t (Failure "something went wrong");
  connection_is_shutdown t;
  Alcotest.(check (option string)) "something went wrong"
    (Some "something went wrong")
    !error_message
;;

let test_partial_input () =
  let request' = Request.create `GET "/" in
  let response_handler response response_body =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      [ "Connection", "close" ]
      (Headers.to_rev_list response.Response.headers);
    Body.close_reader response_body
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.close_writer body;
  reader_ready t;
  let len = feed_string t "HTTP/1.1 200 OK\r\nC" in
  Alcotest.(check int) "partial read" 17 len;
  read_string t "Connection: close\r\n\r\n";
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  shutdown t;
  reader_closed t;
  writer_closed t;
;;

let test_empty_fixed_body () =
  let request' = Request.create
    ~headers:(Headers.of_list ["Connection", "close"])
    `GET "/"
  in
  let response_handler response response_body =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      [ "Connection", "close" ]
      (Headers.to_rev_list response.Response.headers);
    Body.close_reader response_body
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.close_writer body;
  reader_ready t;
  read_response t (Response.create ~headers:(Headers.of_list ["Connection", "close"]) `OK);
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  reader_closed t;
  writer_closed t;
;;

let test_fixed_body () =
  let request' = Request.create
    ~headers:(Headers.of_list ["Connection", "close"; "Content-Length", "3"])
    `GET "/"
  in
  let response_handler response response_body =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      [ "Connection", "close" ]
      (Headers.to_rev_list response.Response.headers);
    Body.close_reader response_body
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.write_string body "foo";
  write_string t "foo";
  writer_yielded t;
  Body.close_writer body;
  reader_ready t;
  read_response t (Response.create ~headers:(Headers.of_list ["Connection", "close"]) `OK);
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  reader_closed t;
  writer_closed t;
;;

let test_fixed_body_persistent_connection () =
  let request' = Request.create
    ~headers:(Headers.of_list ["Content-Length", "0"])
    `GET "/"
  in
  let response_handler response response_body =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      []
      (Headers.to_rev_list response.Response.headers);
    Body.close_reader response_body
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.close_writer body;
  reader_ready t;
  read_response t (Response.create ~headers:(Headers.of_list []) `OK);
  reader_ready t;
  writer_yielded t;
;;

let test_client_upgrade () =
  let request' = Request.create
    ~headers:(Headers.of_list ["Content-Length", "0"])
    `GET "/"
  in
  let t = create ?config:None in
  let response = Response.create `Switching_protocols in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.close_writer body;
  reader_ready t;
  read_response t response;
  reader_yielded t;
  writer_yielded t;
  shutdown t;
  reader_closed t;
  writer_closed t;
;;

let tests =
  [ "commit parse after every header line", `Quick, test_commit_parse_after_every_header
  ; "GET"         , `Quick, test_get
  ; "Response EOF", `Quick, test_response_eof
  ; "Response header order preserved", `Quick, test_response_header_order
  ; "report_exn"  , `Quick, test_report_exn
  ; "input_shrunk", `Quick, test_input_shrunk
  ; "multiple GET, last request closes connection", `Quick, test_get_last_close
  ; "Persistent connection, multiple GETs", `Quick, test_persistent_connection_requests
  ; "Persistent connection, request pipelining", `Quick, test_persistent_connection_requests_pipelining
  ; "Persistent connection, first request includes body", `Quick, test_persistent_connection_requests_pipelining_send_body
  ; "Persistent connections, read response body", `Quick, test_persistent_connection_requests_body
  ; "Partial input", `Quick, test_partial_input
  ; "Empty fixed body shuts down writer", `Quick, test_empty_fixed_body
  ; "Fixed body shuts down writer if connection is not persistent", `Quick, test_fixed_body
  ; "Fixed body doesn't shut down the writer if connection is persistent",`Quick, test_fixed_body_persistent_connection
  ; "Client support for upgrading a connection", `Quick, test_client_upgrade
  ]

(*
 * TODO:
 * - test client connection error handling
 *
 *)

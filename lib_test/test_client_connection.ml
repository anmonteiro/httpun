open Httpaf
open Helpers
open Client_connection

let response_error_pp_hum fmt = function
  | `Malformed_response str ->
    Format.fprintf fmt "Malformed_response: %s" str
  | `Invalid_response_body_length resp ->
    Format.fprintf fmt "Invalid_response_body_length: %s" (response_to_string resp)
  | `Exn exn ->
    Format.fprintf fmt "Exn (%s)" (Printexc.to_string exn)
;;

module Response = struct
  include Response

  let pp = pp_hum
  let equal x y = x = y
end

module Alcotest = struct
  include Alcotest

  let response_error = of_pp response_error_pp_hum
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

let reader_ready ?(msg="Reader is ready") t =
  Alcotest.check read_operation msg
    `Read (next_read_operation t :> [`Close | `Read | `Yield]);
;;

let reader_yielded ?(msg="Reader is in a yield state") t =
  Alcotest.check read_operation msg
    `Yield (next_read_operation t);
;;

let reader_closed t =
  Alcotest.check read_operation "Reader is closed"
    `Close (next_read_operation t :> [`Close | `Read | `Yield]);
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

let connection_is_shutdown t =
  Alcotest.(check bool) "connection is shutdown" true (is_closed t)
;;

let default_response_handler expected_response response body =
  Alcotest.check (module Response) "expected response" expected_response response;
  let on_read _ ~off:_ ~len:_ = () in
  let on_eof () = () in
  Body.Reader.schedule_read body ~on_read ~on_eof;
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
  Body.Writer.close body;
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
  Body.Writer.close body;
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
  Body.Writer.close body;
  write_request t request_close;
  writer_closed t;
  read_response t response;

  (* Single GET, response closes connection *)
  let response = Response.create `OK ~headers:Headers.connection_close in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  writer_closed t;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;

  (* Single GET, streaming body *)
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  read_string t "d\r\nHello, world!\r\n0\r\n\r\n";
;;

let test_head () =
  let error_handler_called = ref false in
  let request' = Request.create `HEAD "/" in
  let response = Response.create
      ~headers:(Headers.of_list ["content-length", "5"])
      `OK
  in

  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
  Alcotest.(check bool) "error handler not called" false !error_handler_called;
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
  Body.Writer.close body;
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
  Body.Writer.close body';
  write_request t request'';
  read_response t response;

  writer_closed t;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
;;

let test_send_streaming_body () =
  let request' = Request.create `GET "/" ~headers:Headers.encoding_chunked in
  let response = Response.create `OK ~headers:Headers.encoding_chunked in
  let t = create ?config:None in
  let body =
    request t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  write_request  t request';
  read_response  t response;
  Body.Writer.write_string body "hello";
  write_string t "5\r\nhello\r\n";
  Body.Writer.write_string body "world";
  Body.Writer.close body;
  write_string t "5\r\nworld\r\n";
  write_string t "0\r\n\r\n";
  writer_yielded t;
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
  Body.Writer.close body;
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
  Body.Writer.close body;
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
  Body.Writer.close body';
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
  Body.Writer.close body;
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
  Body.Writer.close body';
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
  Body.Writer.close body';
  Body.Writer.write_string body "a string";
  Body.Writer.close body;
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
  Body.Writer.close body;
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
  Body.Writer.close body';
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
  Body.Writer.close body;
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
  Body.Writer.close body;
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
  Body.Writer.close body;
  write_request t request';
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
    Body.Reader.close response_body
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
  Body.Writer.close body;
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
    Body.Reader.close response_body
  in
  let t = create ?config:None in
  let (_body: Body.Writer.t) =
    request
      t
      request'
      ~response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_closed t;
  reader_ready t;
  read_response t (Response.create ~headers:(Headers.of_list ["Connection", "close"]) `OK);
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  reader_closed t;
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
    Body.Reader.close response_body
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
  Body.Writer.write_string body "foo";
  write_string t "foo";
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t (Response.create ~headers:(Headers.of_list ["Connection", "close"]) `OK);
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  reader_closed t;
  writer_closed t;
;;

let test_fixed_body_persistent_connection () =
  let writer_woken_up = ref false in
  let request' = Request.create
    ~headers:(Headers.of_list ["Content-Length", "2"])
    `GET "/"
  in
  let response_handler response response_body =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      []
      (Headers.to_rev_list response.Response.headers);
    Body.Reader.close response_body
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
  yield_writer t (fun () -> writer_woken_up := true);
  Body.Writer.write_string body "hi";
  Body.Writer.close body;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_string t "hi";
  writer_yielded t;
  writer_woken_up := false;
  yield_writer t (fun () -> writer_woken_up := true);
  Alcotest.(check bool) "Writer doesn't wake up immediately" false !writer_woken_up;
  reader_ready t;
  read_response t (Response.create ~headers:(Headers.of_list []) `OK);
  reader_ready t;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_yielded t;
;;

let test_empty_fixed_body_persistent_connection () =
  let request' = Request.create
    ~headers:(Headers.of_list ["Content-Length", "0"])
    `GET "/"
  in
  let response_handler response response_body =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      []
      (Headers.to_rev_list response.Response.headers);
    Body.Reader.close response_body
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:response_handler
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t (Response.create ~headers:(Headers.of_list []) `OK);
  reader_ready t;
  writer_yielded t;
;;

let test_client_upgrade () =
  let writer_woken_up = ref false in
  let reader_woken_up = ref false in
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
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  reader_yielded t;
  yield_reader t (fun () -> reader_woken_up := true);
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  Alcotest.(check bool) "Writer hasn't woken up yet" false !writer_woken_up;
  shutdown t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  connection_is_shutdown t;
;;

let backpressure_response_handler continue_reading expected_response response body =
  Alcotest.check (module Response) "expected response" expected_response response;
  let rec on_read _buffer ~off:_ ~len:_ =
    continue_reading := (fun () ->
      Body.Reader.schedule_read body ~on_eof ~on_read);
  and on_eof () = print_endline "got eof" in
  Body.Reader.schedule_read body ~on_eof ~on_read
;;

let test_handling_backpressure_when_read_not_scheduled () =
  let writer_woken_up = ref false in
  let reader_woken_up = ref false in
  let continue_reading = ref (fun () -> ()) in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list ["content-length", "10"]) `OK
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(backpressure_response_handler continue_reading response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  yield_writer t (fun () -> writer_woken_up := true);
  read_string t "five.";
  reader_yielded t;
  yield_reader t (fun () -> reader_woken_up := true);
  !continue_reading ();
  reader_ready ~msg:"Reader wants to read if there's a read scheduled in the body" t;
  Alcotest.(check bool) "Reader wakes up if scheduling read" true !reader_woken_up;
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
;;

let test_handling_backpressure_when_read_not_scheduled_early_yield () =
  let writer_woken_up = ref false in
  let reader_woken_up = ref false in
  let continue_reading = ref (fun () -> ()) in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list ["content-length", "10"]) `OK
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(backpressure_response_handler continue_reading response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  yield_reader t (fun () -> reader_woken_up := true);
  yield_writer t (fun () -> writer_woken_up := true);
  read_string t "five.";
  reader_yielded t;
  !continue_reading ();
  reader_ready ~msg:"Reader wants to read if there's a read scheduled in the body" t;
  Alcotest.(check bool) "Reader wakes up if scheduling read" true !reader_woken_up;
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
;;

let test_eof_with_another_pipelined_request () =
  let reader_woken_up = ref false in
  let continue_reading = ref (fun () -> ()) in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list ["content-length", "10"]) `OK
  in
  let t = create ?config:None in
  let response_handler continue_reading expected_response response body =
    Alcotest.check (module Response) "expected response" expected_response response;
    let on_read _buffer ~off:_ ~len:_ =
      continue_reading := (fun () ->
        Body.Reader.close body);
    and on_eof () = print_endline "got eof" in
    Body.Reader.schedule_read body ~on_eof ~on_read
  in
  let body =
    request
      t
      request'
      ~response_handler:(response_handler continue_reading response)
      ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  yield_reader t (fun () -> reader_woken_up := true);
  yield_writer t ignore;
  read_string t "five.";
  reader_yielded t;
  !continue_reading ();
  reader_ready ~msg:"Reader wants to read if there's a read scheduled in the body" t;
  Alcotest.(check bool) "Reader wakes up if scheduling read" true !reader_woken_up;
  writer_yielded t;

  (* Pipeline the 2nd request. *)
  let expected_response = Response.create `OK in
  let error_handler_called = ref false in
  let body' =
    request
      t
      request'
      ~response_handler:(fun response _ ->
        Alcotest.check (module Response) "expected response" expected_response response)
      ~error_handler:(fun _ -> error_handler_called := true;)
  in
  Body.Writer.close body';
  write_request t request';
  writer_yielded t;

  let str = "rest." in
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  let just_read = read_eof t input ~off:0 ~len in
  Alcotest.(check int) "Rest of the response read completely, server EOF" len just_read;
  reader_closed t;

  Alcotest.(check bool) "Error handler called on the 2nd request" true !error_handler_called;
  connection_is_shutdown t;
;;

(* If we haven't finished receiving the full response, the error handler
 * should be called. *)
let test_eof_handler_response_body_not_closed () =
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list ["content-length", "10"]) `OK
  in
  let t = create ?config:None in
  let response_handler expected_response response body =
    Alcotest.check (module Response) "expected response" expected_response response;
    let on_read _buffer ~off:_ ~len:_ = () in
    let on_eof () = print_endline "got eof" in
    Body.Reader.schedule_read body ~on_eof ~on_read
  in
  let error_handler_called = ref false in
  let body =
    request
      t
      request'
      ~response_handler:(response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true;)
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  yield_writer t ignore;

  reader_ready ~msg:"Reader wants to read if there's a read scheduled in the body" t;

  let str = "rest." in
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  let just_read = read_eof t input ~off:0 ~len in
  Alcotest.(check int) "server EOF after delivering the first part of the response" len just_read;
  reader_closed t;

  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  connection_is_shutdown t;
;;

let test_eof_handler_closed_response_body () =
  let continue_reading = ref (fun () -> ()) in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list ["content-length", "10"]) `OK
  in
  let t = create ?config:None in
  let response_handler continue_reading expected_response response body =
    Alcotest.check (module Response) "expected response" expected_response response;
    let on_read _buffer ~off:_ ~len:_ =
      continue_reading := (fun () ->
        Body.Reader.close body);
    and on_eof () = print_endline "got eof" in
    Body.Reader.schedule_read body ~on_eof ~on_read
  in
  let error_handler_called = ref false in
  let body =
    request
      t
      request'
      ~response_handler:(response_handler continue_reading response)
      ~error_handler:(fun _ -> error_handler_called := true;)
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  yield_writer t ignore;

  !continue_reading ();
  reader_ready ~msg:"Reader wants to read if there's a read scheduled in the body" t;

  let str = "rest." in
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  let just_read = read_eof t input ~off:0 ~len in
  Alcotest.(check int) "Rest of the response read completely, server EOF" len just_read;
  reader_closed t;

  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  connection_is_shutdown t;
;;

let test_exception_closes_reader () =
  let reader_woken_up = ref false in
  let request' = Request.create `GET "/" in
  let error_handler_called = ref false in
  let response =
    Response.create
      ~headers:(Headers.of_list [ "connection", "close"; "content-length", "10" ])
      `OK
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true;)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  writer_closed t;
  read_string t "hello";
  reader_yielded t ~msg:"Reader yields if no read scheduled";
  yield_reader t (fun () -> reader_woken_up := true);
  report_exn t (Failure "something went wrong");
  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  Alcotest.(check bool) "Reader wakes up if scheduling read" true !reader_woken_up;
  connection_is_shutdown t;
;;

let test_exception_closes_reader_persistent_connection () =
  let reader_woken_up = ref false in
  let writer_woken_up = ref false in
  let request' = Request.create `GET "/" in
  let error_handler_called = ref false in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true;)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  read_string t "hello";
  reader_yielded t ~msg:"Reader yields if no read scheduled";
  yield_reader t (fun () -> reader_woken_up := true);
  report_exn t (Failure "something went wrong");
  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  Alcotest.(check bool) "Reader wakes up if scheduling read" true !reader_woken_up;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  connection_is_shutdown t;
;;

let test_exception_reading_response_body () =
  let request' = Request.create `GET "/" in
  let error_handler_called = ref false in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create ?config:None in
  let response_handler expected_response response body =
    Alcotest.check (module Response) "expected response" expected_response response;
    let on_read _ ~off:_ ~len:_ = failwith "something went wrong" in
    let on_eof () = () in
    Body.Reader.schedule_read body ~on_read ~on_eof;
  in
  let body =
    request
      t
      request'
      ~response_handler:(response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true;)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  read_string t "hello";
  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  writer_closed t;
  reader_closed t;
  connection_is_shutdown t;
;;

let test_exception_reading_response_body_last_chunk () =
  let writer_woken_up = ref false in
  let request' = Request.create `GET "/" in
  let error_handler_called = ref false in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create ?config:None in
  let response_handler expected_response response body =
    Alcotest.check (module Response) "expected response" expected_response response;
    let on_eof () = () in
    let on_read _ ~off:_ ~len:_ =
      Body.Reader.schedule_read
        body
        ~on_read:(fun _ ~off:_ ~len:_ ->
          report_exn t (Failure "something went wrong"))
        ~on_eof
    in
    Body.Reader.schedule_read body ~on_read ~on_eof;
  in
  let body =
    request
      t
      request'
      ~response_handler:(response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true;)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  read_string t "hello";
  reader_ready t;
  read_string t "hello";
  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_closed t;
  reader_closed t;
  connection_is_shutdown t;
;;

let test_async_exception_reading_response_body () =
  let reader_woken_up = ref false in
  let writer_woken_up = ref false in
  let continue_reading = ref (fun () -> ()) in
  let request' = Request.create `GET "/" in
  let error_handler_called = ref false in
  let response =
    Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
  in
  let t = create ?config:None in
  let response_handler expected_response response body =
    Alcotest.check (module Response) "expected response" expected_response response;
    let on_read _ ~off:_ ~len:_ =
      continue_reading := (fun () -> report_exn t (Failure "something went wrong")) in
    let on_eof () = () in
    Body.Reader.schedule_read body ~on_read ~on_eof;
  in
  let body =
    request
      t
      request'
      ~response_handler:(response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true;)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  read_string t "hello";
  reader_yielded t;
  yield_reader t (fun () -> reader_woken_up := true);
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  !continue_reading ();
  Alcotest.(check bool) "Error handler called" true !error_handler_called;
  Alcotest.(check bool) "Reader wakes up if scheduling read" true !reader_woken_up;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_closed t;
  reader_closed t;
  connection_is_shutdown t;
;;

let test_failed_response_parse () =
  let writer_woken_up = ref false in
  let request' = Request.create `GET "/" in

  let test response bytes_read expected_error =
    let error = ref None in
    let t = create ?config:None in
    let body =
      request
        t
        request'
        ~response_handler:(fun _ _ -> assert false)
        ~error_handler:(fun e -> error := Some e)
    in
    Body.Writer.close body;
    write_request t request';
    writer_yielded t;
    yield_writer t (fun () -> writer_woken_up := true);
    reader_ready t;
    let len = feed_string t response in
    Alcotest.(check int) "bytes read" len bytes_read;
    reader_closed t;
    Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
    connection_is_shutdown t;
    Alcotest.(check (option response_error)) "Response error"
      (Some expected_error) !error;
  in

  test "HTTP/1.1 200\r\n\r\n" 12 (`Malformed_response ": char ' '");

  let response = Response.create `OK ~headers:(Headers.encoding_fixed (-1)) in
  test (response_to_string response) 39 (`Invalid_response_body_length response);
;;

let test_shutdown_hangs_response_body_read () =
  let got_eof = ref false in
  let request' = Request.create `GET "/" in
  let response =
    Response.create ~headers:(Headers.of_list ["content-length", "10"]) `OK
  in
  let response_handler expected_response response response_body =
    Alcotest.check (module Response) "expected response" expected_response response;
    let rec on_read _buffer ~off:_ ~len:_ =
      Body.Reader.schedule_read response_body ~on_eof ~on_read;
    and on_eof () = got_eof := true in
  Body.Reader.schedule_read response_body ~on_eof ~on_read
  in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(response_handler response)
      ~error_handler:no_error_handler
  in
  reader_ready t;
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  read_response t response;
  read_string t "five.";
  reader_ready ~msg:"Reader wants to read if there's a read scheduled in the body" t;
  shutdown t;
  Alcotest.(check bool) "EOF delivered to the request body if the connection shuts down" true !got_eof;
  connection_is_shutdown t;
;;

let test_response_arrives_before_body_uploaded () =
  let reader_woken_up = ref false in
  let eof_delivered = ref false in
  let continue_response = ref (fun () -> ()) in
  let request' =
   Request.create `GET "/" ~headers:(Headers.of_list ["content-length", "10"])
  in
  let response =
    Response.create ~headers:(Headers.of_list ["content-length", "10"]) `OK
  in
  let t = create ?config:None in
  let response_handler continue_response expected_response response body =
    Alcotest.check (module Response) "expected response" expected_response response;
    let on_read _buffer ~off:_ ~len:_ = ()
    and on_eof () = eof_delivered := true;
    in
    Body.Reader.schedule_read body ~on_eof ~on_read;
    continue_response := (fun () ->
     Body.Reader.schedule_read body ~on_eof ~on_read;
     continue_response := (fun () ->
       Body.Reader.schedule_read body ~on_eof ~on_read))
  in
  let error_handler_called = ref false in
  let second_error_handler_called = ref false in
  let body =
    request
      t
      request'
      ~response_handler:(response_handler continue_response response)
      ~error_handler:(fun _ -> error_handler_called := true;)
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.write_string body "hello";
  write_string t "hello";
  writer_yielded t;
  (* Never close the request body. *)
  reader_ready t;
  read_response t response;
  yield_writer t ignore;

  reader_ready ~msg:"Reader wants to read if there's a read scheduled in the body" t;
  let str = "hello" in
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  let just_read = read t input ~off:0 ~len in
  Alcotest.(check int) "reads the first chunk" len just_read;
  reader_yielded t;
  yield_reader t (fun () -> reader_woken_up := true);

  (* Sneak in a 2nd request. *)
  let expected_response = Response.create `OK in
  let body' =
    request
      t
      request'
      ~response_handler:(fun response _ ->
        Alcotest.check (module Response) "expected response" expected_response response)
      ~error_handler:(fun _ -> second_error_handler_called := true;)
  in
  Body.Writer.close body';
  writer_yielded t;

  !continue_response ();
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
  reader_ready t;
  reader_woken_up := false;

  let str = "hello" in
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  let just_read = read_eof t input ~off:0 ~len in
  Alcotest.(check int) "server EOF after delivering the entire response" len just_read;
  reader_closed t;

  !continue_response ();
  Alcotest.(check bool) "EOF delivered to the response body" true !eof_delivered;

  Alcotest.(check bool) "First error handler not called" false !error_handler_called;
  Alcotest.(check bool) "2nd Error handler called" true !second_error_handler_called;
  connection_is_shutdown t;
;;

let test_race_condition_writer_issues_yield_after_reader_eof () =
  let reader_woken_up = ref false in
  let request' =
   Request.create ~headers:(Headers.of_list [ "content-length", "5" ]) `GET "/"
  in
  let response =
    Response.create ~headers:(Headers.of_list ["content-length", "10"]) `OK
  in
  let t = create ?config:None in
  let response_handler expected_response response _body =
    Alcotest.check (module Response) "expected response" expected_response response;
  in
  let body =
    request
      t
      request'
      ~response_handler:(response_handler response)
      ~error_handler:(fun _ -> ())
  in
  write_request t request';
  Body.Writer.write_string body "hello";
  Body.Writer.flush body ignore;
  write_string t "hello";
  reader_ready t;
  read_response t response;
  read_string t (String.make 5 'a');
  Body.Writer.close body;
  reader_yielded t;
  yield_reader t (fun () ->
    reader_woken_up := true;
    ignore @@ read_eof t Bigstringaf.empty ~off:0 ~len:0;
    reader_closed t);
  writer_closed t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
  connection_is_shutdown t;
;;

let test_multiple_responses_in_single_read () =
  let res_handled = ref 0 in
  let request' =
   Request.create ~headers:(Headers.of_list [ "content-length", "0" ]) `GET "/"
  in
  let response =
    Response.create ~headers:(Headers.of_list ["content-length", "0"]) `OK
  in
  let t = create ?config:None in
  let response_handler _response _body =
    res_handled := !res_handled + 1;
  in
  let body =
    request t request' ~response_handler ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;
  let body =
    request t request' ~response_handler ~error_handler:no_error_handler
  in
  write_request t request';
  writer_yielded t;
  Body.Writer.close body;
  reader_ready t;

  let responses =
    response_to_string response ^
    response_to_string response
  in
  read_string t responses;
  reader_ready t;
  Alcotest.(check int) "fired handler of both requests" 2 !res_handled
;;

let test_chunked_error () =
  let error_handler_called = ref false in
  let request' =
   Request.create ~headers:(Headers.of_list [ "transfer-encoding", "chunked" ]) `GET "/"
  in
  let t = create ?config:None in
  let response_handler _response _body =
    assert false
  in
  let body =
    request
      t
      request'
      ~response_handler
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  write_request t request';
  Body.Writer.write_string body "hello";
  write_string t "5\r\nhello\r\n";
  report_exn t (Failure "something went wrong");
  Alcotest.(check bool) "First error handler called" true !error_handler_called;
  Alcotest.(check bool) "request body is closed" true (Body.Writer.is_closed body);
  writer_closed t;
  connection_is_shutdown t;

  error_handler_called := false;
  let t = create ?config:None in
  let response =
    Response.create ~headers:(Headers.of_list ["content-length", "10"]) `OK
  in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  write_request t request';
  Body.Writer.write_string body "hello";
  Body.Writer.flush body ignore;
  write_string t "5\r\nhello\r\n";
  read_response t response;
  report_exn t (Failure "something went wrong");
  Alcotest.(check bool) "First error handler called" true !error_handler_called;
  Alcotest.(check bool) "request body is closed" true (Body.Writer.is_closed body);
  writer_closed t;
  connection_is_shutdown t;
;;

let test_304_not_modified () =
  let error_handler_called = ref false in
  let request' = Request.create `GET "/" in
  let response = Response.create
      ~headers:(Headers.of_list ["content-length", "5"])
      `Not_modified
  in

  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:(fun _ -> error_handler_called := true)
  in
  Body.Writer.close body;
  write_request t request';
  read_response t response;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
  Alcotest.(check bool) "error handler not called" false !error_handler_called;
;;

let test_empty_content_length_body_closed () =
  let response = Response.create `OK in
  let request' = Request.create `GET "/" in
  let t = create ?config:None in
  let body =
    request
      t
      request'
      ~response_handler:(default_response_handler response)
      ~error_handler:no_error_handler
  in
  Alcotest.(check bool) "request body length is 0"
    true
    ((Request.body_length request') = (`Fixed 0L));
  Alcotest.(check bool) "Request body starts out closed" true (Body.Writer.is_closed body);
  write_request t request';
  read_response t response;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
;;

let test_schedule_read_with_data_available () =
  let request' = Request.create `GET "/" in
  let response = Response.create `OK ~headers:(Headers.encoding_fixed 6) in
  let t = create ?config:None in

  let body = ref None in
  let response_handler response' body' =
    body := Some body';
    Alcotest.check (module Response) "expected response" response response';
  in
  let req_body =
    request t request' ~response_handler ~error_handler:no_error_handler
  in
  Body.Writer.close req_body;
  write_request t request';
  writer_yielded t;
  read_response t response;

  let body = Option.get !body in
  let schedule_read expected =
    let did_read = ref false in
    Body.Reader.schedule_read body
      ~on_read:(fun buf ~off ~len ->
        let actual = Bigstringaf.substring buf ~off ~len in
        did_read := true;
        Alcotest.(check string) "Body" expected actual)
      ~on_eof:(fun () -> assert false);
    Alcotest.(check bool) "on_read called" true !did_read;
  in

  (* We get some data on the connection, but not the full response yet. *)
  read_string t "Hello";
  reader_yielded t;

  (* Schedule a read when there is already data available. on_read should be called
     straight away, as who knows how long it'll be before more data arrives. *)
  schedule_read "Hello";
  read_string t "!";
  schedule_read "!";
  let did_eof = ref false in
  Body.Reader.schedule_read body
    ~on_read:(fun _ ~off:_ ~len:_ -> Alcotest.fail "Expected eof")
    ~on_eof:(fun () -> did_eof := true);
  Alcotest.(check bool) "on_eof called" true !did_eof;
;;

let tests =
  [ "commit parse after every header line", `Quick, test_commit_parse_after_every_header
  ; "GET"         , `Quick, test_get
  ; "HEAD"        , `Quick, test_head
  ; "send streaming body", `Quick, test_send_streaming_body
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
  ; "Empty fixed body in a persistent connection doesn't shut down the writer", `Quick, test_empty_fixed_body_persistent_connection
  ; "Fixed body shuts down writer if connection is not persistent", `Quick, test_fixed_body
  ; "Fixed body doesn't shut down the writer if connection is persistent",`Quick, test_fixed_body_persistent_connection
  ; "Client support for upgrading a connection", `Quick, test_client_upgrade
  ; "test yield when read isn't scheduled", `Quick, test_handling_backpressure_when_read_not_scheduled
  ; "test yield when read isn't scheduled, reader yields early", `Quick, test_handling_backpressure_when_read_not_scheduled_early_yield
  ; "EOF while a request is in the pipeline", `Quick, test_eof_with_another_pipelined_request
  ; "EOF after handler response body not closed", `Quick, test_eof_handler_response_body_not_closed
  ; "EOF after handler closed response body", `Quick, test_eof_handler_closed_response_body
  ; "Exception closes the reader (on a non-persistent connection)", `Quick, test_exception_closes_reader
  ; "Exception closes the reader (on a persistent connection)", `Quick, test_exception_closes_reader_persistent_connection
  ; "Exception while reading the response body", `Quick, test_exception_reading_response_body
  ; "Exception while reading the response body", `Quick, test_exception_reading_response_body_last_chunk
  ; "Aynchronous exception while reading the response body", `Quick, test_async_exception_reading_response_body
  ; "failed response parse", `Quick, test_failed_response_parse
  ; "shutdown delivers eof to response bodies", `Quick, test_shutdown_hangs_response_body_read
  ; "full response arrives before uploading the entire request body, 2nd request in the pipeline", `Quick, test_response_arrives_before_body_uploaded
  ; "reader EOF race condition causes state machine to issue writer yield", `Quick, test_race_condition_writer_issues_yield_after_reader_eof
  ; "multiple responses in single read", `Quick, test_multiple_responses_in_single_read
  ; "test chunked error", `Quick, test_chunked_error
  ; "304 Not Modified with Content-Length", `Quick, test_304_not_modified
  ; "body for a request with no payload starts out closed", `Quick, test_empty_content_length_body_closed
  ; "schedule read with data available", `Quick, test_schedule_read_with_data_available
  ]

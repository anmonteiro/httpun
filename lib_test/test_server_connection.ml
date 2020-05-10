open Httpaf
open Helpers
open Server_connection

let feed_string t str =
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  read t input ~off:0 ~len

let read_string t str =
  let c = feed_string t str in
  Alcotest.(check int) "read consumes all input" (String.length str) c;
;;

let read_request t r =
  let request_string = request_to_string r in
  read_string t request_string
;;

let reader_ready ?(msg="Reader is ready") t =
  Alcotest.check read_operation msg
    `Read (next_read_operation t);
;;

let reader_yielded t =
  Alcotest.check read_operation "Reader is in a yield state"
    `Yield (next_read_operation t);
;;

let reader_closed ?(msg="Reader is closed") t =
  Alcotest.check read_operation msg
    `Close (next_read_operation t);
;;

let reader_errored = reader_closed ~msg:"Error shuts down the reader"

let write_string ?(msg="output written") t str =
  let len = String.length str in
  Alcotest.(check (option string)) msg
    (Some str)
    (next_write_operation t |> Write_operation.to_write_as_string);
  report_write_result t (`Ok len);
;;

let write_response ?(msg="response written") ?body t r =
  let response_string = response_to_string ?body r in
  write_string ~msg t response_string
;;

let write_eof t =
  report_write_result t `Closed;
;;

let writer_yielded t =
  Alcotest.check write_operation "Writer is in a yield state"
    `Yield (next_write_operation t);
;;

let writer_closed ?(unread = 0) t =
  Alcotest.check write_operation "Writer is closed"
    (`Close unread) (next_write_operation t);
;;

let connection_is_shutdown t =
  reader_closed t;
  writer_closed  t;
;;

let request_handler_with_body body reqd =
  Body.close_reader (Reqd.request_body reqd);
  Reqd.respond_with_string reqd (Response.create `OK) body
;;

let default_request_handler reqd =
  request_handler_with_body "" reqd
;;

let echo_handler response reqd =
  let request_body  = Reqd.request_body reqd in
  let response_body = Reqd.respond_with_streaming reqd response in
  let rec on_read buffer ~off ~len =
    Body.write_string response_body (Bigstringaf.substring ~off ~len buffer);
    Body.flush response_body (fun () ->
      Body.schedule_read request_body ~on_eof ~on_read)
    and on_eof () = print_endline "got eof"; Body.close_writer response_body in
  Body.schedule_read request_body ~on_eof ~on_read;
;;

let streaming_handler ?(flush=false) response writes reqd =
  let writes = ref writes in
  let request_body = Reqd.request_body reqd in
  Body.close_reader request_body;
  let body = Reqd.respond_with_streaming ~flush_headers_immediately:flush reqd response in
  let rec write () =
    match !writes with
    | [] -> Body.close_writer body
    | w :: ws ->
      Body.write_string body w;
      writes := ws;
      Body.flush body write
  in
  write ();
;;

let synchronous_raise reqd =
  Reqd.report_exn reqd (Failure "caught this exception")
;;

let error_handler ?request:_ _error start_response =
  let resp_body = start_response Headers.empty in
  Body.write_string resp_body "got an error";
  Body.close_writer resp_body
;;

let test_initial_reader_state () =
  let t = create default_request_handler in
  reader_ready ~msg:"A new reader wants input" t
;;

let test_reader_is_closed_after_eof () =
  let t = create default_request_handler in
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;

  let t = create default_request_handler in
  let c = read t Bigstringaf.empty ~off:0 ~len:0 in
  Alcotest.(check int) "read with no input returns 0" 0 c;
  let c = read_eof t Bigstringaf.empty ~off:0 ~len:0; in
  Alcotest.(check int) "read_eof with no input returns 0" 0 c;
  connection_is_shutdown t;
;;

let test_commit_parse_after_every_header () =
  let t = create default_request_handler in
  let request_line = "GET / HTTP/1.1\r\n" in
  let single_header = "Links: /path/to/some/website\r\n" in
  let r =
    (* Each header is 30 bytes *)
    request_line ^ single_header ^ single_header ^ "connection: close\r\n\r\n"
  in
  let bs = Bigstringaf.of_string r ~off:0 ~len:(String.length r) in
  let c = read t bs ~off:0 ~len:30 in
  Alcotest.(check int) "only reads the request line" (String.length request_line) c;

  let c' =
    read t bs ~off:c ~len:(String.length single_header)
  in
  Alcotest.(check int) "parser can read a single header and commit" (String.length single_header) c';

  let c'' = read_eof t bs ~off:(c + c') ~len:(String.length r - (c + c')) in
  Alcotest.(check int) "read_eof with the rest of the input is accepted" (String.length r - (c + c')) c'';
  write_response t (Response.create `OK);
  connection_is_shutdown t;
;;

let test_single_get () =
  (* Single GET *)
  let t = create default_request_handler in
  read_request   t (Request.create `GET "/");
  write_response t (Response.create `OK);

  (* Single GET, close the connection *)
  let t = create default_request_handler in
  read_request   t (Request.create `GET "/" ~headers:(Headers.of_list ["connection", "close"]));
  write_response t (Response.create `OK);
  connection_is_shutdown t;

  (* Single GET, with reponse body *)
  let response_body = "This is a test" in
  let t = create (request_handler_with_body response_body) in
  read_request   t (Request.create `GET "/" ~headers:(Headers.of_list ["connection", "close"]));
  write_response t
    ~body:response_body
    (Response.create `OK);
  connection_is_shutdown t;
;;

let test_asynchronous_response () =
  let response_body = "hello, world!" in
  let response_body_length = String.length response_body in
  let response =
    Response.create
      `OK
      ~headers:(Headers.of_list [("content-length", string_of_int response_body_length)])
  in
  let continue = ref (fun () -> ()) in
  let t = create (fun reqd ->
    continue := fun () ->
      Body.close_reader (Reqd.request_body reqd);
      let data = Bigstringaf.of_string ~off:0 ~len:response_body_length response_body in
      let size = Bigstringaf.length data in
      let response =
        Response.create
          `OK
          ~headers:(Headers.of_list [("content-length", string_of_int size)])
      in
      let response_body =
        Reqd.respond_with_streaming reqd response in
      Body.write_bigstring response_body data;
      Body.close_writer response_body)
   in
  read_request   t (Request.create `GET "/");
  reader_yielded t;
  writer_yielded t;
  !continue ();
  write_response t ~body:response_body response;
  read_request   t (Request.create `GET "/");
  reader_yielded t;
  writer_yielded t;
  !continue ();
  write_response t ~body:response_body response
;;

let test_echo_post () =
  let request = Request.create `GET "/" ~headers:(Headers.of_list ["transfer-encoding", "chunked"]) in

  (* Echo a single chunk *)
  let response =
    Response.create `OK ~headers:(Headers.of_list ["transfer-encoding", "chunked"])
  in
  let t = create (echo_handler response) in
  read_request t request;
  read_string  t "e\r\nThis is a test";
  write_response t
    ~body:"e\r\nThis is a test\r\n"
    response;
  read_string  t "\r\n0\r\n";
  write_string t "0\r\n\r\n";
  writer_yielded t;

  (* Echo two chunks *)
  let response =
    Response.create `OK ~headers:(Headers.of_list ["transfer-encoding", "chunked"])
  in
  let t = create (echo_handler response) in
  read_request t request;
  read_string  t "e\r\nThis is a test";
  write_response t
    ~body:"e\r\nThis is a test\r\n"
    response;
  read_string  t "\r\n21\r\n... that involves multiple chunks";
  write_string t "21\r\n... that involves multiple chunks\r\n";
  read_string  t "\r\n0\r\n";
  write_string t "0\r\n\r\n";
  writer_yielded t;

  (* Echo and close *)
  let response =
    Response.create `OK ~headers:(Headers.of_list ["connection", "close"])
  in
  let t = create (echo_handler response) in
  read_request t request;
  read_string  t "e\r\nThis is a test";
  write_response t
    ~body:"This is a test"
    response;
  read_string  t "\r\n21\r\n... that involves multiple chunks";
  read_string  t "\r\n0\r\n";
  write_string t "... that involves multiple chunks";
  connection_is_shutdown t;
;;

let test_streaming_response () =
  let request  = Request.create `GET "/" in
  let response = Response.create `OK in

  let t = create (streaming_handler response ["Hello "; "world!"]) in
  read_request   t request;
  write_response t
    ~body:"Hello "
    response;
  write_string   t "world!";
  writer_yielded t;
;;

let test_asynchronous_streaming_response () =
  let request  = Request.create `GET "/" ~headers:(Headers.of_list ["connection", "close"]) in
  let response = Response.create `OK in

  let body = ref None in
  let t = create (fun reqd ->
    body := Some (Reqd.respond_with_streaming reqd response))
  in

  let writer_woken_up = ref false in
  writer_yielded t;
  yield_writer t (fun () ->
    writer_woken_up := true;
    writer_yielded t);
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;

  read_request t request;
  let body =
    match !body with
    | None -> failwith "no body found"
    | Some body -> body
  in
  (* XXX(dpatti): This is an observation of a current behavior where the writer
     is awoken only to find that it was asked to yield again. It is cleaned up
     in another branch where we move the continuation off of the reqd/body. *)
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  let writer_woken_up = ref false in
  yield_writer t (fun () ->
    writer_woken_up := true;
    write_response t ~body:"Hello " response);

  Body.write_string body "Hello ";
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  Body.flush body ignore;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;

  let writer_woken_up = ref false in
  writer_yielded t;
  yield_writer t (fun () ->
    writer_woken_up := true;
    write_string t "world!";
    writer_closed t);
  Body.write_string body "world!";
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  Body.close_writer body;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up
;;

let test_asynchronous_streaming_response_with_immediate_flush () =
  let request  = Request.create `GET "/" ~headers:(Headers.of_list ["connection", "close"]) in
  let response = Response.create `OK in

  let body = ref None in
  let t = create (fun reqd ->
    body := Some (Reqd.respond_with_streaming reqd response ~flush_headers_immediately:true))
  in
  let writer_woken_up = ref false in
  writer_yielded t;
  yield_writer t (fun () ->
    writer_woken_up := true;
    write_response t response);
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;

  read_request t request;
  let body =
    match !body with
    | None -> failwith "no body found"
    | Some body -> body
  in
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;

  let writer_woken_up = ref false in
  writer_yielded t;
  yield_writer t (fun () ->
    writer_woken_up := true;
    writer_closed t);
  Body.close_writer body;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up
;;

let test_empty_fixed_streaming_response () =
  let request  = Request.create `GET "/" in
  let response =
    Response.create `OK
      ~headers:(Headers.of_list ["Content-length", "0"])
  in

  let t = create (streaming_handler response []) in
  read_request   t request;
  write_response t response;
  writer_yielded t;
;;

let test_empty_chunked_streaming_response () =
  let request  = Request.create `GET "/" in
  let response =
    Response.create `OK
      ~headers:(Headers.of_list ["Transfer-encoding", "chunked"])
  in

  let t = create (streaming_handler response []) in
  read_request   t request;
  write_response t response
    ~body:"0\r\n\r\n";
  writer_yielded t;
;;

let test_multiple_get () =
  let t = create default_request_handler in
  read_request   t (Request.create `GET "/");
  write_response t (Response.create `OK);
  read_request   t (Request.create `GET "/");
  write_response t (Response.create `OK);
;;

let test_asynchronous_streaming_response_flush_immediately () =
  let writer_woken_up = ref false in
  let continue_response = ref (fun () -> ()) in
  let request  = Request.create `GET "/" in
  let response = Response.create `OK in
  let request_handler reqd =
    let body = Reqd.respond_with_streaming ~flush_headers_immediately:true reqd response in
    continue_response := (fun () ->
      Body.write_string body "hello";
      Body.close_writer body)
  in
  let t = create request_handler in
  read_request   t request;
  write_response t response;
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  !continue_response ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_string   t "hello";
  writer_yielded t;
;;

(* Writer doesn't get a chance to yield before incoming request. *)
let test_asynchronous_streaming_response_writer_doesnt_yield () =
  let writer_woken_up = ref false in
  let continue_response = ref (fun () -> ()) in
  let request  = Request.create `GET "/" in
  let response = Response.create `OK in
  let request_handler reqd =
    continue_response := (fun () ->
      let body = Reqd.respond_with_streaming reqd response in
      Body.write_string body "hello";
      Body.close_writer body)
  in
  let t = create request_handler in
  read_request   t request;
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  !continue_response ();
  write_response t ~body:"hello" response;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_yielded t;
;;

let test_synchronous_error () =
  let writer_woken_up = ref false in
  let t = create ~error_handler synchronous_raise in
  yield_writer t (fun () -> writer_woken_up := true);
  read_request t (Request.create `GET "/");
  reader_errored t;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t
    ~msg:"Error response written"
    (Response.create `Internal_server_error)
    ~body:"got an error"
;;

let test_synchronous_error_asynchronous_handling () =
  let writer_woken_up = ref false in
  let continue = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue := (fun () ->
      error_handler ?request error start_response)
  in
  let t = create ~error_handler synchronous_raise in
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  read_request t (Request.create `GET "/");
  reader_errored t;
  writer_yielded t;
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  !continue ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t
    ~msg:"Error response written"
    (Response.create `Internal_server_error)
    ~body:"got an error"
;;

let test_asynchronous_error () =
  let continue = ref (fun () -> ()) in
  let asynchronous_raise reqd =
    continue := (fun () -> synchronous_raise reqd)
  in
  let writer_woken_up = ref false in
  let t = create ~error_handler asynchronous_raise in
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  read_request t (Request.create `GET "/");
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
(* ;; ?? *)
  writer_yielded t;
  reader_yielded t;
  !continue ();
  reader_errored t;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t
    ~msg:"Error response written"
    (Response.create `Internal_server_error)
    ~body:"got an error"
;;

let test_asynchronous_error_asynchronous_handling () =
  let continue_request = ref (fun () -> ()) in
  let asynchronous_raise reqd =
    continue_request := (fun () -> synchronous_raise reqd)
  in
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error := (fun () ->
      error_handler ?request error start_response)
  in
  let writer_woken_up = ref false in
  let t = create ~error_handler asynchronous_raise in
  writer_yielded t;
  yield_writer   t (fun () -> writer_woken_up := true);
  read_request   t (Request.create `GET "/");
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  writer_yielded t;
  reader_yielded t;
  !continue_request ();
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  writer_yielded t;
  !continue_error ();
  reader_errored t;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t
    ~msg:"Error response written"
    (Response.create `Internal_server_error)
    ~body:"got an error"
;;

let test_asynchronous_error_asynchronous_response_body () =
  let continue_request = ref (fun () -> ()) in
  let asynchronous_raise reqd =
    continue_request := (fun () -> synchronous_raise reqd)
  in
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request:_ _error start_response =
    continue_error := (fun () ->
      let resp_body = start_response Headers.empty in
      continue_error := (fun () ->
        Body.write_string resp_body "got an error";
        Body.close_writer resp_body))
  in
  let writer_woken_up = ref false in
  let t = create ~error_handler asynchronous_raise in
  writer_yielded t;
  yield_writer   t (fun () -> writer_woken_up := true);
  read_request   t (Request.create `GET "/");
  writer_yielded t;
  reader_yielded t;
  !continue_request ();
  writer_yielded t;
  !continue_error ();
  reader_errored t;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_woken_up := false;
  write_response t
    ~msg:"Error response written"
    (Response.create `Internal_server_error);
  yield_writer t (fun () -> writer_woken_up := true);
  !continue_error ();
  write_string t "got an error";
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
;;

let test_chunked_encoding () =
  let request_handler reqd =
    let response =
      Response.create `OK
        ~headers:(Headers.of_list [ "Transfer-encoding", "chunked" ])
    in
    let resp_body = Reqd.respond_with_streaming reqd response in
    Body.write_string resp_body "First chunk";
    Body.flush resp_body (fun () ->
      Body.write_string resp_body "Second chunk";
      Body.close_writer resp_body);
  in
  let t = create ~error_handler request_handler in
  writer_yielded t;
  read_request t (Request.create `GET "/");
  write_response t
    ~msg:"First chunk written"
    ~body:"b\r\nFirst chunk\r\n"
    (Response.create `OK ~headers:(Headers.of_list ["Transfer-encoding", "chunked"]));
  write_string t
    ~msg:"Second chunk"
    "c\r\nSecond chunk\r\n";
  write_string t
    ~msg:"Final chunk written"
    "0\r\n\r\n";
  reader_ready ~msg:"Keep-alive" t;
;;

let test_blocked_write_on_chunked_encoding () =
  let request_handler reqd =
    let response =
      Response.create `OK
        ~headers:(Headers.of_list [ "Transfer-encoding", "chunked" ])
    in
    let resp_body = Reqd.respond_with_streaming reqd response in
    Body.write_string resp_body "gets partially written";
    (* Response body never gets closed but for the purposes of the test, that's
     * OK. *)
  in
  let t = create ~error_handler request_handler in
  writer_yielded t;
  read_request t (Request.create `GET "/");
  let first_write = "HTTP/1.1 200 OK\r\nTransfer-encoding: chunked\r\n\r\n16\r\ngets partially written\r\n" in
  Alcotest.(check (option string)) "first write"
    (Some first_write)
    (next_write_operation t |> Write_operation.to_write_as_string);
  report_write_result t (`Ok 16);
  Alcotest.(check (option string)) "second write"
    (Some (String.sub first_write 16 (String.length first_write - 16)))
    (next_write_operation t |> Write_operation.to_write_as_string);
;;

let test_respond_with_upgrade () =
  let upgraded = ref false in
  let upgrade_handler reqd =
    Reqd.respond_with_upgrade reqd Headers.empty (fun () ->
     upgraded := true)
  in
  let t = create ~error_handler upgrade_handler in
  read_request t (Request.create `GET "/");
  match next_write_operation t with
  | `Write iovecs ->
    let response_str = response_to_string (Response.create `Switching_protocols) in
    Alcotest.(check string) "Switching protocols response"
      (Write_operation.iovecs_to_string iovecs)
      response_str;
    let len = String.length response_str in
    report_write_result t (`Ok len);
    Alcotest.(check bool) "Callback was called" true !upgraded;
    reader_ready t;
  | _ -> Alcotest.fail "Expected Upgrade operation"

let test_unexpected_eof () =
  let t = create default_request_handler in
  read_request   t (Request.create `GET "/");
  write_eof      t;
  writer_closed  t ~unread:19;
;;

let test_input_shrunk () =
  let continue_response = ref (fun () -> ()) in
  let error_handler ?request:_ _ = assert false in
  let request_handler reqd =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      [ "Host"           , "example.com"
      ; "Connection"     , "close"
      ; "Accept"         , "application/json, text/plain, */*"
      ; "Accept-Language", "en-US,en;q=0.5" ]
      (Headers.to_list (Reqd.request reqd).headers);
    Body.close_reader (Reqd.request_body reqd);
    continue_response := (fun () ->
      Reqd.respond_with_string reqd (Response.create `OK) "");
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  let writer_woken_up = ref false in
  writer_yielded t;
  yield_writer t (fun () ->
    writer_woken_up := true;
    write_response t (Response.create `OK);
  );
  let len = feed_string t "GET /v1/b HTTP/1.1\r\nH" in
  Alcotest.(check int) "partial read" 20 len;
  read_string t "Host: example.com\r\n\
Connection: close\r\n\
Accept: application/json, text/plain, */*\r\n\
Accept-Language: en-US,en;q=0.5\r\n\r\n";
  writer_yielded t;
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;
  reader_closed t;
  !continue_response ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_closed t;
;;

let malformed_request_string =
  "GET / HTTP/1.1\r\nconnection: close\r\nX-Other-Header : shouldnt_have_space_before_colon\r\n\r\n"

let eof_request_string =
  "GET / HTTP/1.1\r\nconnection: close\r\nX-Other-Header: EOF_after_this"

let basic_handler body reqd =
  let request_body = Reqd.request_body reqd in
  Body.close_reader request_body;
  Reqd.respond_with_string reqd (Response.create `OK) body;
;;

let test_malformed conn =
  let writer_woken_up = ref false in
  writer_yielded conn;
  yield_writer conn (fun () -> writer_woken_up := true);
  let len = String.length malformed_request_string in
  let input = Bigstringaf.of_string malformed_request_string ~off:0 ~len in
  let c = read conn input ~off:0 ~len in
  Alcotest.(check bool) "read doesn't consume all input"
    true (c < String.length malformed_request_string);
  reader_errored conn;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up

let test_malformed_request () =
  let t = create ~error_handler (basic_handler "") in
  test_malformed t;
  Alcotest.(check (option string)) "Error response written"
    (Some "HTTP/1.1 400 Bad Request\r\n\r\ngot an error")
    (next_write_operation t |> Write_operation.to_write_as_string)
;;

let test_malformed_request_async () =
  let continue = ref (fun () -> ()) in
  let error_handler ?request:_ _error start_response =
    let resp_body = start_response Headers.empty in
    continue := (fun () ->
      Body.write_string resp_body "got an error";
      Body.close_writer resp_body)
  in
  let t = create ~error_handler (basic_handler "") in
  test_malformed t;
  !continue ();
  Alcotest.(check (option string)) "Error response written"
    (Some "HTTP/1.1 400 Bad Request\r\n\r\ngot an error")
    (next_write_operation t |> Write_operation.to_write_as_string)
;;

let test_malformed_request_async_multiple_errors () =
  let continue = ref (fun () -> ()) in
  let error_handler ?request:_ _error start_response =
    let resp_body = start_response Headers.empty in
    continue := (fun () ->
      Body.write_string resp_body "got an error";
      Body.close_writer resp_body)
  in
  let t = create ~error_handler (basic_handler "") in
  test_malformed t;
  !continue ();
  let len = String.length malformed_request_string in
  let input = Bigstringaf.of_string malformed_request_string ~off:0 ~len in
  let c = read t input ~off:0 ~len in
  Alcotest.(check int) "read doesn't consume more input" 0 c;
  Alcotest.(check (option string)) "Error response written"
    (Some "HTTP/1.1 400 Bad Request\r\n\r\ngot an error")
    (next_write_operation t |> Write_operation.to_write_as_string)
;;

let read_string_eof t str =
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  read_eof t input ~off:0 ~len;
;;

let test_malformed_request_eof () =
  let t = create ~error_handler (basic_handler "") in
  test_malformed t;
  Alcotest.(check (option string)) "Error response written"
    (Some "HTTP/1.1 400 Bad Request\r\n\r\ngot an error")
    (next_write_operation t |> Write_operation.to_write_as_string)
;;

let streaming_error_handler continue_error ?request:_ _error start_response =
  let resp_body = start_response Headers.empty in
  continue_error := (fun () ->
    Body.write_string resp_body "got an error\n";
    Body.flush resp_body (fun () ->
      continue_error := (fun () ->
          Body.write_string resp_body "more output";
          Body.close_writer resp_body)))
;;

let test_malformed_request_streaming_error_response () =
  let writer_woken_up = ref false in
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error := (fun () ->
      streaming_error_handler continue_error ?request error start_response)
  in
  let t = create ~error_handler (basic_handler "") in
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  let c = read_string_eof t eof_request_string in
  Alcotest.(check int) "read consumes all input"
    (String.length eof_request_string) c;
  reader_errored t;
  !continue_error ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_woken_up := false;
  write_response t
    (Response.create `Bad_request ~headers:Headers.empty);
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  !continue_error ();
  write_string t ~msg:"First part of the response body written" "got an error\n";
  Alcotest.(check bool) "Writer woken up once more input is available"
    true !writer_woken_up;
  !continue_error ();
  write_string t ~msg:"Rest of the error response written" "more output";
  writer_closed t;
  Alcotest.(check bool) "Connection is shutdown" true (is_closed t);
;;

let chunked_error_handler continue_error ?request:_ _error start_response =
  let resp_body =
    start_response (Headers.of_list ["transfer-encoding", "chunked"])
  in
  Body.write_string resp_body "chunk 1\n";
  Body.flush resp_body (fun () ->
    continue_error := (fun () ->
      Body.write_string resp_body "chunk 2\n";
      Body.flush resp_body (fun () ->
        continue_error := (fun () ->
          Body.write_string resp_body "chunk 3\n";
          Body.close_writer resp_body))))
;;

let test_malformed_request_chunked_error_response () =
  let writer_woken_up = ref false in
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error := (fun () ->
      chunked_error_handler continue_error ?request error start_response)
  in
  let t = create ~error_handler (basic_handler "") in
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  let c = read_string_eof t eof_request_string in
  Alcotest.(check int) "read consumes all input"
    (String.length eof_request_string) c;
  reader_errored t;
  Alcotest.(check bool) "Writer hasn't woken up yet" false !writer_woken_up;
  !continue_error ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_woken_up := false;
  write_response t
    ~msg:"First chunk written"
    ~body:"8\r\nchunk 1\n\r\n"
    (Response.create `Bad_request
      ~headers:(Headers.of_list ["transfer-encoding", "chunked"]));
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  !continue_error ();
  write_string t
    ~msg:"Second chunk"
    "8\r\nchunk 2\n\r\n";
  !continue_error ();
  write_string t
    ~msg:"Second chunk"
    "8\r\nchunk 3\n\r\n";
  write_string t
    ~msg:"Final chunk written"
    "0\r\n\r\n";
  Alcotest.(check bool) "Writer woken up once more input is available"
    true !writer_woken_up;
  writer_closed t;
  Alcotest.(check bool) "Connection is shutdown" true (is_closed t);
;;

(* This may happen when writing an asynchronous error response on a broken
 * pipe. *)
let test_malformed_request_double_report_exn () =
  let writer_woken_up = ref false in
  let continue_error = ref (fun () -> ()) in
  let error_handler ?request error start_response =
    continue_error := (fun () ->
      streaming_error_handler continue_error ?request error start_response)
  in
  let t = create ~error_handler (basic_handler "") in
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  let c = read_string_eof t eof_request_string in
  Alcotest.(check int) "read consumes all input"
    (String.length eof_request_string) c;
  reader_errored t;
  Alcotest.(check bool) "Writer hasn't woken up yet" false !writer_woken_up;
  !continue_error ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_woken_up := false;
  write_eof t;
  report_exn t (Failure "broken pipe");
  writer_closed t ~unread:28;
  Alcotest.(check bool) "Connection is shutdown" true (is_closed t);
;;

let test_immediate_flush_empty_body () =
  let reader_woken_up = ref false in
  let response = Response.create `OK in
  let request_handler reqd =
    let resp_body = Reqd.respond_with_streaming
      ~flush_headers_immediately:true reqd response
    in
    Body.close_writer resp_body;
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  yield_writer t (fun () -> write_response t ~body:"" response);
  read_request t (Request.create `GET "/");
  yield_reader t (fun () -> reader_woken_up := true);
  writer_yielded t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
;;

let test_empty_body_no_immediate_flush () =
  let reader_woken_up = ref false in
  let response = Response.create `OK in
  let request_handler reqd =
    let resp_body = Reqd.respond_with_streaming
      ~flush_headers_immediately:false reqd response
    in
    Body.close_writer resp_body;
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request t (Request.create `GET "/");
  yield_reader t (fun () -> reader_woken_up := true);
  write_response t ~body:"" response;
  writer_yielded t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
;;

let test_yield_before_starting_a_response () =
  let reader_woken_up = ref false in
  let response = Response.create `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    Body.close_reader (Reqd.request_body reqd);
    continue_response := (fun () ->
      let resp_body = Reqd.respond_with_streaming reqd response in
      Body.close_writer resp_body)
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request t (Request.create `GET "/");
  yield_reader t (fun () -> reader_woken_up := true);
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  yield_writer t ignore;
  !continue_response ();
  write_response t ~body:"" response;
  writer_yielded t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
;;

let test_respond_before_reading_entire_body () =
  let reader_woken_up = ref false in
  let response = Response.create `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    (* Important that we never close the request body for this test. *)
    continue_response := (fun () ->
      let resp_body = Reqd.respond_with_streaming reqd response in
      Body.close_writer resp_body)
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request t (Request.create `GET "/" ~headers:(Headers.of_list ["content-length", "2"]));
  yield_reader t (fun () -> reader_woken_up := true);
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  yield_writer t ignore;
  !continue_response ();
  write_response t ~body:"" response;
  writer_yielded t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
;;

let backpressure_request_handler continue_reading reqd =
  let request_body  = Reqd.request_body reqd in
  let rec on_read _buffer ~off:_ ~len:_ =
    continue_reading := (fun () ->
      Body.schedule_read request_body ~on_eof ~on_read);
  and on_eof () = print_endline ("got eof" ^ (string_of_bool (Body.is_closed request_body))) in
  Body.schedule_read request_body ~on_eof ~on_read

let test_handling_backpressure_when_read_not_scheduled () =
  let reader_woken_up = ref false in
  let continue_reading = ref (fun () -> ()) in
  let t = create ~error_handler (backpressure_request_handler continue_reading) in
  reader_ready t;
  writer_yielded t;
  let request =
    Request.create
      `GET
      ~headers:(Headers.of_list ["content-length", "10"])
      "/"
  in
  read_request t request;
  yield_writer t ignore;
  read_string t "five.";
  reader_yielded t;
  yield_reader t (fun () -> reader_woken_up := true);
  !continue_reading ();
  Alcotest.(check bool) "Reader wakes up if scheduling read" true !reader_woken_up;
  reader_ready ~msg:"Reader wants to read if there's a read scheduled in the body" t;
  writer_yielded t;
;;

let test_handling_backpressure_when_read_not_scheduled_early_yield () =
  let reader_woken_up = ref false in
  let continue_reading = ref (fun () -> ()) in
  let t = create ~error_handler (backpressure_request_handler continue_reading) in
  reader_ready t;
  writer_yielded t;
  let request =
    Request.create
      `GET
      ~headers:(Headers.of_list ["content-length", "10"])
      "/"
  in
  read_request t request;
  yield_reader t (fun () -> reader_woken_up := true);
  yield_writer t ignore;
  read_string t "five.";
  reader_yielded t;
  !continue_reading ();
  Alcotest.(check bool) "Reader wakes up if scheduling read" true !reader_woken_up;
  reader_ready ~msg:"Reader wants to read if there's a read scheduled in the body" t;
  writer_yielded t;
;;

let test_input_shrunk_chunked () =
  let continue_response = ref (fun () -> ()) in
  let error_handler ?request:_ _ = assert false in
  let request_handler reqd =
    Alcotest.(check (list (pair string string)))
      "got expected headers"
      [ "Host"           , "example.com"
      ; "Transfer-Encoding", "chunked" ]
      (Headers.to_list (Reqd.request reqd).headers);
    Body.close_reader (Reqd.request_body reqd);
    continue_response := (fun () ->
      Reqd.respond_with_string reqd (Response.create `OK) "");
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  yield_writer t (fun () -> write_response t (Response.create `OK));
  let len = feed_string t "GET /v1/b HTTP/1.1\r\nH" in
  Alcotest.(check int) "partial read" 20 len;
  read_string t "Host: example.com\r\nTransfer-Encoding: chunked\r\n\r\n";

  let str = "5\r\ninput\r\n" in
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  let just_read = read t input ~off:0 ~len in
  Alcotest.(check int) "partial read" (len - 2) just_read;

  let just_read = read_eof t input ~off:(len - 2) ~len:2 in
  Alcotest.(check int) "eof partial read, doesn't get terminating chunk" 2 just_read;

  writer_yielded t;
  (* TODO: test error handling. *)
  (* reader_closed t;
  !continue_response ();
  writer_closed t; *)
;;

let test_respond_before_reading_entire_body_chunked_eof () =
  let reader_woken_up = ref false in
  let writer_woken_up = ref false in
  let response = Response.create `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    (* Important that we never close the request body for this test. *)
    continue_response := (fun () ->
      let resp_body = Reqd.respond_with_streaming reqd response in
      Body.close_writer resp_body)
  in
  let error_handler ?request:_ _error _start_response =
    Alcotest.fail "Expected error_handler not to be called because the response was already sent"
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request t (Request.create `GET "/" ~headers:(Headers.of_list ["transfer-encoding", "chunked"]));
  yield_reader t (fun () -> reader_woken_up := true);
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  yield_writer t (fun () -> writer_woken_up := true);
  !continue_response ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t ~body:"" response;
  writer_yielded t;
  Alcotest.(check bool) "Reader woken up" true !reader_woken_up;
  writer_woken_up := false;
  reader_ready t;
  (* Yield writer before feeding eof.
   *
   * Note: writer here is done. It yields before we feed more to the reader
   * to allow for it to complete. *)

  let str = "5\r\ninput\r\n" in
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  let just_read = read_eof t input ~off:0 ~len in
  Alcotest.(check int) "malformed chunked encoding read completely" len just_read;

  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  reader_errored t;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_closed t
;;

let test_finish_response_after_read_eof () =
  let reader_woken_up = ref false in
  let writer_woken_up = ref false in
  let response = Response.create `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    (* Important that we never close the request body for this test. *)
      let resp_body = Reqd.respond_with_streaming reqd response in
    continue_response := (fun () ->
      Body.close_writer resp_body)
  in
  let error_handler ?request:_ _error _start_response =
    Alcotest.fail "Expected error_handler not to be called because the response was already sent"
  in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request t (Request.create `GET "/" ~headers:(Headers.of_list ["transfer-encoding", "chunked"]));
  yield_reader t (fun () -> reader_woken_up := true);
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  yield_writer t (fun () -> writer_woken_up := true);

  let str = "5\r\ninput\r\n" in
  let len = String.length str in
  let input = Bigstringaf.of_string str ~off:0 ~len in
  let just_read = read_eof t input ~off:0 ~len in
  Alcotest.(check int) "malformed chunked encoding read completely" len just_read;

  reader_errored t;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t ~body:"" response;
  !continue_response ();
  writer_closed t;
  reader_closed t;
;;

let test_respond_before_reading_entire_body_no_error () =
  let reader_woken_up = ref false in
  let writer_woken_up = ref false in
  let response = Response.create `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    (* Important that we never close the request body for this test. *)
    continue_response := (fun () ->
      let resp_body = Reqd.respond_with_streaming reqd response in
      Body.close_writer resp_body)
  in
  let error_handler ?request:_ _error _start_response = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request t (Request.create `GET "/" ~headers:(Headers.of_list ["content-length", "10"]));
  yield_reader t (fun () -> reader_woken_up := true);
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  yield_writer t (fun () -> writer_woken_up := true);

  read_string t "data.";
  !continue_response ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t ~body:"" response;
  writer_yielded t;
  reader_ready t;
  (* Yield writer before feeding eof.
   *
   * Note: writer here is done. It yields before we feed more to the reader
   * to allow for it to complete. *)
  writer_yielded t;
  read_string t "final";

  (* Ready for the next request *)
  reader_ready t;
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_yielded t
;;

let test_streaming_response_before_reading_entire_body_no_error () =
  let reader_woken_up = ref false in
  let writer_woken_up = ref false in
  let response = Response.create `OK in
  let continue_response = ref (fun () -> ()) in
  let request_handler reqd =
    (* Important that we never close the request body for this test. *)
    continue_response := (fun () ->
      let resp_body = Reqd.respond_with_streaming reqd response in
      continue_response := (fun () ->
        Body.write_string resp_body "hello";
        Body.flush resp_body (fun () ->
          continue_response := (fun () ->
            Body.close_writer resp_body))))
  in
  let error_handler ?request:_ _error _start_response = assert false in
  let t = create ~error_handler request_handler in
  reader_ready t;
  writer_yielded t;
  read_request t (Request.create `GET "/" ~headers:(Headers.of_list ["content-length", "10"]));
  yield_reader t (fun () -> reader_woken_up := true);
  Alcotest.(check bool) "Reader hasn't woken up yet" false !reader_woken_up;
  yield_writer t (fun () -> writer_woken_up := true);

  read_string t "data.";
  !continue_response ();
  Alcotest.(check bool) "Writer not woken up" false !writer_woken_up;

  !continue_response ();
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  write_response t ~body:"hello" response;

  writer_woken_up := false;
  writer_yielded t;
  yield_writer t (fun () -> writer_woken_up := true);
  !continue_response ();
  (* Important that the writer wakes up after closing it, so that it gets a
   * chance to close the request body, and thus advance the remaining request
   * body bytes to prepare the parser for the next request. *)
  Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
  writer_yielded t;

  reader_ready t;
  read_string t "final";

  (* Ready for the next request *)
  reader_ready t;
;;

let tests =
  [ "initial reader state"  , `Quick, test_initial_reader_state
  ; "shutdown reader closed", `Quick, test_reader_is_closed_after_eof
  ; "commit parse after every header line", `Quick, test_commit_parse_after_every_header
  ; "single GET"            , `Quick, test_single_get
  ; "multiple GETs"         , `Quick, test_multiple_get
  ; "asynchronous response" , `Quick, test_asynchronous_response
  ; "asynchronous response, asynchronous body", `Quick, test_asynchronous_streaming_response_flush_immediately
  ; "asynchronous response, asynchronous body, writer doesn't yield", `Quick, test_asynchronous_streaming_response_writer_doesnt_yield
  ; "echo POST"             , `Quick, test_echo_post
  ; "streaming response"    , `Quick, test_streaming_response
  ; "asynchronous streaming response", `Quick, test_asynchronous_streaming_response
  ; "asynchronous streaming response, immediate flush", `Quick, test_asynchronous_streaming_response_with_immediate_flush
  ; "empty fixed streaming response", `Quick, test_empty_fixed_streaming_response
  ; "empty chunked streaming response", `Quick, test_empty_chunked_streaming_response
  ; "synchronous error, synchronous handling", `Quick, test_synchronous_error
  ; "synchronous error, asynchronous handling", `Quick, test_synchronous_error_asynchronous_handling
  ; "asynchronous error, synchronous handling", `Quick, test_asynchronous_error
  ; "asynchronous error, asynchronous handling", `Quick, test_asynchronous_error_asynchronous_handling
  ; "asynchronous error, asynchronous handling + asynchronous body", `Quick, test_asynchronous_error_asynchronous_response_body
  ; "chunked encoding", `Quick, test_chunked_encoding
  ; "blocked write on chunked encoding", `Quick, test_blocked_write_on_chunked_encoding
  ; "respond with upgrade", `Quick, test_respond_with_upgrade
  ; "writer unexpected eof", `Quick, test_unexpected_eof
  ; "input shrunk", `Quick, test_input_shrunk
  ; "malformed request", `Quick, test_malformed_request
  ; "malformed request (async)", `Quick, test_malformed_request_async
  ; "multiple malformed requests?", `Quick, test_malformed_request_async_multiple_errors
  ; "malformed request, chunked error response", `Quick, test_malformed_request_chunked_error_response
  ; "malformed request, double report_exn", `Quick, test_malformed_request_double_report_exn
  ; "malformed request (EOF)", `Quick, test_malformed_request_eof
  ; "malformed request, streaming response", `Quick, test_malformed_request_streaming_error_response
  ; "`flush_headers_immediately` with empty body", `Quick, test_immediate_flush_empty_body
  ; "empty body with no immediate flush", `Quick, test_empty_body_no_immediate_flush
  ; "yield before starting a response", `Quick, test_yield_before_starting_a_response
  ; "respond before body has been read", `Quick, test_respond_before_reading_entire_body
  ; "test yield when read isn't scheduled", `Quick, test_handling_backpressure_when_read_not_scheduled
  ; "test yield when read isn't scheduled, reader yields early", `Quick, test_handling_backpressure_when_read_not_scheduled_early_yield
  ; "test partial input chunked body", `Quick, test_input_shrunk_chunked
  ; "respond before reading request body, then request body EOFs", `Quick, test_respond_before_reading_entire_body_chunked_eof
  ; "request body EOFs before closing response body, request body not closed", `Quick, test_finish_response_after_read_eof
  ; "respond before reading entire request body", `Quick, test_respond_before_reading_entire_body_no_error
  ; "respond before reading entire request body, streaming response", `Quick, test_streaming_response_before_reading_entire_body_no_error
  ]

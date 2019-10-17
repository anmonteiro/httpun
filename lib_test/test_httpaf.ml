open Httpaf

module Version = struct
  include Version

  let v1_0 = { major = 1; minor = 0 }
  let v1_1 = { major = 1; minor = 1 }

  let test_compare () =
    Alcotest.(check int) "compare v1_1 v1_0" (compare v1_1 v1_0) 1;
    Alcotest.(check int) "compare v1_1 v1_1" (compare v1_1 v1_1) 0;
    Alcotest.(check int) "compare v1_0 v1_0" (compare v1_0 v1_0) 0;
    Alcotest.(check int) "compare v1_0 v1_1" (compare v1_0 v1_1) (-1);
  ;;

  let test_to_string () =
    Alcotest.(check string) "to_string v1_1" (to_string v1_1) "HTTP/1.1";
    Alcotest.(check string) "to_string v1_0" (to_string v1_0) "HTTP/1.0";
  ;;

  let tests =
    [ "compare"  , `Quick, test_compare
    ; "to_string", `Quick, test_to_string
    ]
end

module Method = struct
  include Method

  let test_is_safe () =
    Alcotest.(check bool) "GET is safe"     (is_safe `GET )    true;
    Alcotest.(check bool) "HEAD is safe"    (is_safe `HEAD)    true;
    Alcotest.(check bool) "POST is safe"    (is_safe `POST)    false;
    Alcotest.(check bool) "PUT is safe"     (is_safe `PUT )    false;
    Alcotest.(check bool) "DELETE is safe"  (is_safe `DELETE ) false;
    Alcotest.(check bool) "CONNECT is safe" (is_safe `CONNECT) false;
    Alcotest.(check bool) "OPTIONS is safe" (is_safe `OPTIONS) true;
    Alcotest.(check bool) "TRACE is safe"   (is_safe `TRACE  ) true;
  ;;

  let test_is_cacheable () =
    Alcotest.(check bool) "GET is cacheable"     (is_cacheable `GET )    true;
    Alcotest.(check bool) "HEAD is cacheable"    (is_cacheable `HEAD)    true;
    Alcotest.(check bool) "POST is cacheable"    (is_cacheable `POST)    true;
    Alcotest.(check bool) "PUT is cacheable"     (is_cacheable `PUT )    false;
    Alcotest.(check bool) "DELETE is cacheable"  (is_cacheable `DELETE ) false;
    Alcotest.(check bool) "CONNECT is cacheable" (is_cacheable `CONNECT) false;
    Alcotest.(check bool) "OPTIONS is cacheable" (is_cacheable `OPTIONS) false;
    Alcotest.(check bool) "TRACE is cacheable"   (is_cacheable `TRACE  ) false;
  ;;

  let test_is_idempotent () =
    Alcotest.(check bool) "GET is idempotent"     (is_idempotent `GET )    true;
    Alcotest.(check bool) "HEAD is idempotent"    (is_idempotent `HEAD)    true;
    Alcotest.(check bool) "POST is idempotent"    (is_idempotent `POST)    false;
    Alcotest.(check bool) "PUT is idempotent"     (is_idempotent `PUT )    true;
    Alcotest.(check bool) "DELETE is idempotent"  (is_idempotent `DELETE ) true;
    Alcotest.(check bool) "CONNECT is idempotent" (is_idempotent `CONNECT) false;
    Alcotest.(check bool) "OPTIONS is idempotent" (is_idempotent `OPTIONS) true;
    Alcotest.(check bool) "TRACE is idempotent"   (is_idempotent `TRACE  ) true;
  ;;

  let tests =
    [ "is_safe"      , `Quick, test_is_safe
    ; "is_cacheable" , `Quick, test_is_cacheable
    ; "is_idempotent", `Quick, test_is_idempotent
    ]
end

module IOVec = struct
  include IOVec

  (* The length of the buffer is ignored by iovec operations *)
  let buffer = Bigstringaf.empty

  let test_lengthv () =
    Alcotest.(check int) "lengthv [] = 0"                 (lengthv []) 0;
    Alcotest.(check int) "lengthv [iovec] = length iovec"
      (lengthv [{ buffer; off = 0; len = 0 }]) (length {buffer; off = 0; len = 0 });
    Alcotest.(check int) "lengthv [iovec] = length iovec"
      (lengthv [{ buffer; off = 0; len = 10 }]) (length {buffer; off = 0; len = 10 });
  ;;

  let test_shiftv_raises () =
    Alcotest.check_raises
      "IOVec.shiftv: -1 is a negative number"
      (Failure "IOVec.shiftv: -1 is a negative number")
      (fun () -> ignore (shiftv [] (-1)));
    let test f =
      Alcotest.check_raises
        "shiftv iovecs n raises when n > lengthv iovecs"
        (Failure "shiftv: n > lengthv iovecs")
      (fun () -> ignore (f ()))
    in
    test (fun () -> shiftv [] 1);
    test (fun () -> shiftv [{ buffer; off = 0; len = 1 }] 2);
    test (fun () -> shiftv [{ buffer; off = 0; len = 1 }; { buffer; off = 0; len = 1 }] 3);
  ;;

  let test_shiftv () =
    Alcotest.(check (of_pp pp_hum |> list)) "shiftv [] 0 = []" (shiftv [] 0) [];
    Alcotest.(check (of_pp pp_hum |> list)) "shiftv [{... len ... }] len = []"
      (shiftv [{ buffer; off = 0; len = 1 }] 1) [];
    Alcotest.(check (of_pp pp_hum |> list)) "shiftv [iovec] n when length iovec < n"
      (shiftv [{ buffer; off = 0; len = 4 }] 2) [{ buffer; off = 2; len = 2 }];
  ;;

  let tests =
    [ "lengthv"       , `Quick, test_lengthv
    ; "shiftv"        , `Quick, test_shiftv
    ; "shiftv raises ", `Quick, test_shiftv_raises ]
end

module Headers = struct
  include Headers

  let check msg ~expect actual =
    Alcotest.(check (list (pair string string))) msg expect (Headers.to_list actual)
  ;;

  let test_replace () =
    check "replace trailing element"
      ~expect:["c", "d"; "a", "d"]
      (Headers.replace
        (Headers.of_list ["c", "d"; "a", "b"])
        "a"
        "d");

    check "remove multiple trailing elements"
      ~expect:["c", "d"; "a", "d"]
      (Headers.replace
        (Headers.of_list [ "c", "d"; "a", "b"; "a", "c"])
        "a"
        "d");
  ;;

  let test_remove () =
    check "remove leading element"
      ~expect:["c", "d"]
      (Headers.remove
        (Headers.of_list ["a", "b"; "c", "d"])
        "a");
    check "remove trailing element"
      ~expect:["c", "d"]
      (Headers.remove
        (Headers.of_list ["c", "d"; "a", "b"])
        "a");
  ;;

  let tests =
    [ "remove" , `Quick, test_remove
    ; "replace", `Quick, test_replace ]
end

let maybe_serialize_body f body =
  match body with
  | None -> ()
  | Some body -> Faraday.write_string f body

let request_to_string ?body r =
  let f = Faraday.create 0x1000 in
  Httpaf_private.Serialize.write_request f r;
  maybe_serialize_body f body;
  Faraday.serialize_to_string f

let response_to_string ?body r =
  let f = Faraday.create 0x1000 in
  Httpaf_private.Serialize.write_response f r;
  maybe_serialize_body f body;
  Faraday.serialize_to_string f

module Read_operation = struct
  type t = [ `Read | `Yield | `Close ]

  let pp_hum fmt t =
    let str =
      match t with
      | `Read -> "Read"
      | `Yield -> "Yield"
      | `Close -> "Close"
    in
    Format.pp_print_string fmt str
  ;;
end

module Write_operation = struct
  type 'fd t = [
    | `Write of Bigstringaf.t IOVec.t list
    | `Upgrade of Bigstringaf.t IOVec.t list * ('fd -> unit)
    | `Yield
    | `Close of int ]

  let iovecs_to_string iovecs =
    let len = IOVec.lengthv iovecs in
    let bytes = Bytes.create len in
    let dst_off = ref 0 in
    List.iter (fun { IOVec.buffer; off = src_off; len } ->
      Bigstringaf.unsafe_blit_to_bytes buffer ~src_off bytes ~dst_off:!dst_off ~len;
      dst_off := !dst_off + len)
    iovecs;
    Bytes.unsafe_to_string bytes
  ;;

  let pp_hum fmt t =
    match t with
    | `Write iovecs -> Format.fprintf fmt "Write %S" (iovecs_to_string iovecs)
    | `Upgrade (iovecs, _) -> Format.fprintf fmt "Upgrade %S" (iovecs_to_string iovecs)
    | `Yield -> Format.pp_print_string fmt "Yield"
    | `Close len -> Format.fprintf fmt "Close %i" len
  ;;

  let to_write_as_string t =
    match t with
    | `Write iovecs | `Upgrade (iovecs, _) -> Some (iovecs_to_string iovecs)
    | `Close _ | `Yield -> None
  ;;
end

let write_operation = Alcotest.of_pp Write_operation.pp_hum
let read_operation = Alcotest.of_pp Read_operation.pp_hum

module Server_connection = struct
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

  let reader_ready t =
    Alcotest.check read_operation "Reader is ready"
      `Read (next_read_operation t);
  ;;

  let reader_yielded t =
    Alcotest.check read_operation "Reader is in a yield state"
      `Yield (next_read_operation t);
  ;;

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
    Alcotest.check read_operation "Reader is closed"
      `Close (next_read_operation t);
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
    Alcotest.check read_operation "A new reader wants input"
      `Read (next_read_operation t);
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

  let test_synchronous_error () =
    let writer_woken_up = ref false in
    let t = create ~error_handler synchronous_raise in
    yield_writer t (fun () -> writer_woken_up := true);
    read_request t (Request.create `GET "/");
    Alcotest.check read_operation "Error shuts down the reader"
      `Close (next_read_operation t);
    Alcotest.(check bool) "Writer woken up"
      true !writer_woken_up;
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
    Alcotest.check read_operation "Error shuts down the reader"
      `Close (next_read_operation t);
    writer_yielded t;
    !continue ();
    Alcotest.(check bool) "Writer woken up"
      true !writer_woken_up;
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
    writer_yielded t;
    reader_yielded t;
    !continue ();
    Alcotest.check read_operation "Error shuts down the reader"
      `Close (next_read_operation t);
    Alcotest.(check bool) "Writer woken up"
      true !writer_woken_up;
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
    writer_yielded t;
    reader_yielded t;
    !continue_request ();
    writer_yielded t;
    !continue_error ();
    Alcotest.check read_operation "Error shuts down the reader"
      `Close (next_read_operation t);
    Alcotest.(check bool) "Writer woken up"
      true !writer_woken_up;
    write_response t
      ~msg:"Error response written"
      (Response.create `Internal_server_error)
      ~body:"got an error"
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
    Alcotest.check read_operation "Keep-alive"
      `Read (next_read_operation t);
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
    Alcotest.check write_operation "Writer is in a yield state"
      `Yield (next_write_operation conn);
    yield_writer conn (fun () -> writer_woken_up := true);
    let len = String.length malformed_request_string in
    let input = Bigstringaf.of_string malformed_request_string ~off:0 ~len in
    let c = read conn input ~off:0 ~len in
    Alcotest.(check bool) "read doesn't consume all input"
      true (c < String.length malformed_request_string);
    Alcotest.check read_operation "Error shuts down the reader"
      `Close (next_read_operation conn);
    Alcotest.(check bool) "Writer woken up"
      true !writer_woken_up

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
    Body.write_string resp_body "got an error\n";
    continue_error := (fun () ->
      Body.write_string resp_body "more output";
      Body.close_writer resp_body)
  ;;

  let test_malformed_request_streaming_error_response () =
    let eof_request_string =
      "GET / HTTP/1.1\r\nconnection: close\r\nX-Other-Header: EOF_after_this"
    in
    let writer_woken_up = ref false in
    let continue_error = ref (fun () -> ()) in
    let error_handler ?request error start_response =
      continue_error := (fun () ->
        streaming_error_handler continue_error ?request error start_response)
    in
    let t = create ~error_handler (basic_handler "") in
    Alcotest.check write_operation "Writer is in a yield state"
      `Yield (next_write_operation t);
    yield_writer t (fun () -> writer_woken_up := true);
    let c = read_string_eof t eof_request_string in
    Alcotest.(check int) "read consumes all input"
      (String.length eof_request_string) c;
    Alcotest.check read_operation "Error shuts down the reader"
      `Close (next_read_operation t);
    !continue_error ();
    Alcotest.(check bool) "Writer woken up" true !writer_woken_up;
    writer_woken_up := false;
    write_string t ~msg:"Error response and first output written"
      "HTTP/1.1 400 Bad Request\r\n\r\ngot an error\n";
    Alcotest.check write_operation "Writer is in a yield state"
      `Yield (next_write_operation t);
    yield_writer t (fun () -> writer_woken_up := true);
    !continue_error ();
    Alcotest.(check bool) "Writer woken up once more input is available"
      true !writer_woken_up;
    write_string t ~msg:"Rest of the error response written" "more output";
    Alcotest.(check bool) "Connection is shutdown" true (is_closed t);
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
    | `Upgrade (iovecs, fn) ->
      let response_str = response_to_string (Response.create `Switching_protocols) in
      Alcotest.(check string) "Switching protocols response"
        (Write_operation.iovecs_to_string iovecs)
        response_str;
      fn ();
      let len = String.length response_str in
      report_write_result t (`Ok len);
      Alcotest.(check bool) "Callback was called" true !upgraded
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
        (Headers.to_rev_list (Reqd.request reqd).headers);
      Body.close_reader (Reqd.request_body reqd);
      continue_response := (fun () ->
        Reqd.respond_with_string reqd (Response.create `OK) "");
    in
    let t = create ~error_handler request_handler in
    reader_ready t;
    writer_yielded t;
    yield_writer t (fun () ->
      write_response t (Response.create `OK);
    );
    let len = feed_string t "GET /v1/b HTTP/1.1\r\nH" in
    Alcotest.(check int) "partial read" 20 len;
    read_string t "Host: example.com\r\n\
Connection: close\r\n\
Accept: application/json, text/plain, */*\r\n\
Accept-Language: en-US,en;q=0.5\r\n\r\n";
    writer_yielded t;
    Alcotest.check read_operation "reader closed"
      `Close (next_read_operation t);
    !continue_response ();
    writer_closed t;
	;;

  let tests =
    [ "initial reader state"  , `Quick, test_initial_reader_state
    ; "shutdown reader closed", `Quick, test_reader_is_closed_after_eof
    ; "single GET"            , `Quick, test_single_get
    ; "multiple GETs"         , `Quick, test_multiple_get
    ; "asynchronous response" , `Quick, test_asynchronous_response
    ; "echo POST"             , `Quick, test_echo_post
    ; "streaming response"    , `Quick, test_streaming_response
    ; "empty fixed streaming response", `Quick, test_empty_fixed_streaming_response
    ; "empty chunked streaming response", `Quick, test_empty_chunked_streaming_response
    ; "synchronous error, synchronous handling", `Quick, test_synchronous_error
    ; "synchronous error, asynchronous handling", `Quick, test_synchronous_error_asynchronous_handling
    ; "asynchronous error, synchronous handling", `Quick, test_asynchronous_error
    ; "asynchronous error, asynchronous handling", `Quick, test_asynchronous_error_asynchronous_handling
    ; "chunked encoding", `Quick, test_chunked_encoding
    ; "blocked write on chunked encoding", `Quick, test_blocked_write_on_chunked_encoding
    ; "malformed request", `Quick, test_malformed_request
    ; "malformed request (async)", `Quick, test_malformed_request_async
    ; "multiple malformed requests?", `Quick, test_malformed_request_async_multiple_errors
    ; "malformed request (EOF)", `Quick, test_malformed_request_eof
    ; "respond with upgrade", `Quick, test_respond_with_upgrade
    ; "writer unexpected eof", `Quick, test_unexpected_eof
    ; "input shrunk", `Quick, test_input_shrunk
    ]
end

module Client_connection = struct
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
      `Yield (next_write_operation t :> unit Write_operation.t);
  ;;

  let writer_closed t =
    Alcotest.check write_operation "Writer is closed"
      (`Close 0) (next_write_operation t :> unit Write_operation.t);
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

  let test_report_exn () =
    let request' = Request.create `GET "/" in
    let response = Response.create `OK in (* not actually written to the channel *)

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
    reader_ready t;
    report_exn t (Failure "something went wrong");
    connection_is_shutdown t;
    Alcotest.(check (option string)) "something went wrong"
      (Some "something went wrong")
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
        ~response_handler:response_handler
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
        ~response_handler:response_handler
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
        ~response_handler:response_handler
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

  let tests =
    [ "GET"         , `Quick, test_get
    ; "Response EOF", `Quick, test_response_eof
    ; "report_exn"  , `Quick, test_report_exn
    ; "multiple GET, last request closes connection", `Quick, test_get_last_close
    ; "Response EOF", `Quick, test_response_eof
    ; "Persistent connection, multiple GETs", `Quick, test_persistent_connection_requests
    ; "Persistent connection, request pipelining", `Quick, test_persistent_connection_requests_pipelining
    ; "Persistent connection, first request includes body", `Quick, test_persistent_connection_requests_pipelining_send_body
    ; "Persistent connections, read response body", `Quick, test_persistent_connection_requests_body
    ; "Empty fixed body shuts down writer", `Quick, test_empty_fixed_body
    ; "Fixed body shuts down writer", `Quick, test_fixed_body ]

end

let () =
  Alcotest.run "httpaf unit tests"
    [ "version"          , Version.tests
    ; "method"           , Method.tests
    ; "iovec"            , IOVec.tests
    ; "headers"          , Headers.tests
    ; "client connection", Client_connection.tests
    ; "server connection", Server_connection.tests
    ]

(*
 * TODO:
 * - test client connection error handling
 *
 *)

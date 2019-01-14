(*----------------------------------------------------------------------------
    Copyright (c) 2018 Inhabited Type LLC.
    Copyright (c) 2018 Anton Bachin
    Copyright (c) 2019 António Nuno Monteiro

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

open Lwt.Infix

module Io
  : Httpaf_lwt.IO with
    type socket = Lwt_unix.file_descr
    and type addr = Unix.sockaddr = struct
  type socket = Lwt_unix.file_descr
  type addr = Unix.sockaddr

  let read socket bigstring ~off ~len =
    Lwt.catch
      (fun () -> Lwt_bytes.read socket bigstring off len)
      (function
      | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
        Lwt.fail exn
      | exn ->
        Lwt.async (fun () ->
          Lwt_unix.close socket);
        Lwt.fail exn)

     >>= fun bytes_read ->
    if bytes_read = 0 then
      Lwt.return `Eof
    else
      Lwt.return (`Ok bytes_read)

  let writev socket = Faraday_lwt_unix.writev_of_fd socket

   let shutdown socket command =
    try Lwt_unix.shutdown socket command
    with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

  let shutdown_send socket =
    if not (Lwt_unix.state socket = Lwt_unix.Closed) then
      shutdown socket Unix.SHUTDOWN_SEND

  let shutdown_receive socket =
    if not (Lwt_unix.state socket = Lwt_unix.Closed) then
      shutdown socket Unix.SHUTDOWN_RECEIVE

  let close socket =
    if Lwt_unix.state socket <> Lwt_unix.Closed then
      Lwt.catch
        (fun () -> Lwt_unix.close socket)
        (fun _exn -> Lwt.return_unit)
    else
      Lwt.return_unit
end

module Config = Httpaf.Config

module Server = struct
  include Httpaf_lwt.Server (Io)

  module TLS = struct
    include Httpaf_lwt.Server (Tls_io.Io)

    let create_connection_handler
      ?server
      ?certfile
      ?keyfile
      ?(config=Config.default)
      ~request_handler
      ~error_handler =
      let make_tls_server = Tls_io.make_server ?server ?certfile ?keyfile in
      fun client_addr socket ->
        make_tls_server socket >>= fun tls_server ->
        create_connection_handler
          ~config
          ~request_handler
          ~error_handler
          client_addr
          (socket, tls_server)
  end

  module SSL = struct
    include Httpaf_lwt.Server (Ssl_io.Io)

    let create_connection_handler
      ?server
      ?certfile
      ?keyfile
      ?(config=Config.default)
      ~request_handler
      ~error_handler =
      let make_ssl_server = Ssl_io.make_server ?server ?certfile ?keyfile in
      fun client_addr socket ->
        make_ssl_server socket >>= fun ssl_server ->
        create_connection_handler
          ~config
          ~request_handler
          ~error_handler
          client_addr
          ssl_server
  end
end

module Client = struct
  include Httpaf_lwt.Client (Io)

  module TLS = struct
    include Httpaf_lwt.Client (Tls_io.Io)

    let request ?client ?(config=Config.default) socket request_headers ~error_handler ~response_handler =
      Tls_io.make_client ?client socket >|= fun tls_client ->
      request ~config (socket, tls_client) request_headers ~error_handler ~response_handler
  end

  module SSL = struct
    include Httpaf_lwt.Client (Ssl_io.Io)

    let request ?client ?(config=Config.default) socket request_headers ~error_handler ~response_handler =
      Ssl_io.make_client ?client socket >|= fun ssl_client ->
      request ~config ssl_client request_headers ~error_handler ~response_handler
  end
end

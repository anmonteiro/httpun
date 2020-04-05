(*----------------------------------------------------------------------------
 *  Copyright (c) 2019 António Nuno Monteiro
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

open Lwt.Infix

type descriptor = Lwt_ssl.socket

module Io :
  Httpaf_lwt.IO with type socket = descriptor and type addr = Unix.sockaddr =
struct
  type socket = Lwt_ssl.socket
  type addr = Unix.sockaddr

  let read ssl bigstring ~off ~len =
    Lwt.catch
      (fun () ->
        Lwt_ssl.read_bytes ssl bigstring off len)
      (function
        | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
          Lwt.fail exn
        | exn ->
          Lwt.async (fun () ->
              Lwt_ssl.ssl_shutdown ssl >>= fun () ->
              Lwt_ssl.close ssl);
          Lwt.fail exn)
    >>= fun bytes_read ->
    if bytes_read = 0 then
      Lwt.return `Eof
    else
      Lwt.return (`Ok bytes_read)

  let writev ssl iovecs =
    Lwt.catch
      (fun () ->
        Lwt_list.fold_left_s
          (fun acc { Faraday.buffer; off; len } ->
            Lwt_ssl.write_bytes ssl buffer off len >|= fun written ->
            acc + written)
          0
          iovecs
        >|= fun n ->
        `Ok n)
      (function
        | Unix.Unix_error (Unix.EBADF, "check_descriptor", _) ->
          Lwt.return `Closed
        | exn ->
          Lwt.fail exn)

  (* From RFC8446§6.1:
   *   The client and the server must share knowledge that the connection is
   *   ending in order to avoid a truncation attack.
   *
   * Note: In the SSL / TLS runtimes we can't just shutdown one part of the
   * full-duplex connection, as both sides must know that the underlying TLS
   * conection is closing. *)
  let shutdown_send _ssl = ()

  let shutdown_receive _ssl = ()

  let close ssl =
    Lwt_ssl.ssl_shutdown ssl >>= fun () ->
      Lwt.wrap2 Lwt_ssl.shutdown ssl Unix.SHUTDOWN_ALL >>= fun () ->
      Lwt_ssl.close ssl

  let state ssl =
    match Lwt_unix.state (Lwt_ssl.get_fd ssl) with
    | Aborted _ ->
      `Error
    | Closed ->
      `Closed
    | Opened ->
      `Open
end

let make_default_client socket =
  let client_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
  Ssl.disable_protocols client_ctx [Ssl.SSLv23];
  Ssl.honor_cipher_order client_ctx;
  Ssl.set_context_alpn_protos client_ctx [ "http/1.1" ];
  Lwt_ssl.ssl_connect socket client_ctx

let make_server ~certfile ~keyfile socket =
  let server_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Server_context in
  Ssl.disable_protocols server_ctx [Ssl.SSLv23];
  Ssl.use_certificate server_ctx certfile keyfile;
  Lwt_ssl.ssl_accept socket server_ctx

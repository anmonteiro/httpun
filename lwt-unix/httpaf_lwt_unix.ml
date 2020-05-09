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

module Server = struct
  include Httpaf_lwt.Server (Gluten_lwt_unix.Server)

  module TLS = struct
    include Httpaf_lwt.Server (Gluten_lwt_unix.Server.TLS)

    let create_connection_handler_with_default
      ~certfile
      ~keyfile
      ?config
      ~request_handler
      ~error_handler =
      let make_tls_server =
        Gluten_lwt_unix.Server.TLS.create_default ~certfile ~keyfile
      in
      fun client_addr socket ->
        make_tls_server client_addr ~alpn_protocols:["http/1.1"] socket
        >>= fun tls_server ->
          create_connection_handler
            ?config
            ~request_handler
            ~error_handler
            client_addr
            tls_server
  end

  module SSL = struct
    include Httpaf_lwt.Server (Gluten_lwt_unix.Server.SSL)

    let create_connection_handler_with_default
      ~certfile
      ~keyfile
      ?config
      ~request_handler
      ~error_handler =
      let make_ssl_server =
        Gluten_lwt_unix.Server.SSL.create_default ~certfile ~keyfile
      in
      fun client_addr socket ->
        make_ssl_server ~alpn_protocols:["http/1.1"] client_addr socket
        >>= fun ssl_server ->
          create_connection_handler
            ?config
            ~request_handler
            ~error_handler
            client_addr
            ssl_server
  end
end

module Client = struct
  include Httpaf_lwt.Client (Gluten_lwt_unix.Client)

  module TLS = struct
    include Httpaf_lwt.Client (Gluten_lwt_unix.Client.TLS)

    let create_connection_with_default ?config socket =
      Gluten_lwt_unix.Client.TLS.create_default
        ~alpn_protocols:["http/1.1"]
        socket
      >>= fun tls_client ->
        create_connection ?config tls_client
  end

  module SSL = struct
    include Httpaf_lwt.Client (Gluten_lwt_unix.Client.SSL)

    let create_connection_with_default ?config socket =
      Gluten_lwt_unix.Client.SSL.create_default
        ~alpn_protocols:["http/1.1"]
        socket
      >>= fun ssl_client ->
        create_connection ?config ssl_client
  end
end

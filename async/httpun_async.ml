(*----------------------------------------------------------------------------
  Copyright (c) 2018 Inhabited Type LLC. Copyright (c) 2019 António Nuno
  Monteiro

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

  3. Neither the name of the author nor the names of his contributors may be
  used to endorse or promote products derived from this software without
  specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR
  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
  EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Async

module Server = struct
  type 'a socket = ([ `Active ], ([< Socket.Address.t ] as 'a)) Socket.t

  let create_connection_handler
        ?(config = Httpun.Config.default)
        ~request_handler
        ~error_handler
    =
   fun client_addr socket ->
    let create_connection =
      Httpun.Server_connection.create
        ~config
        ~error_handler:(error_handler client_addr)
    in
    Gluten_async.Server.create_upgradable_connection_handler
      ~read_buffer_size:config.read_buffer_size
      ~protocol:(module Httpun.Server_connection)
      ~create_protocol:create_connection
      ~request_handler
      client_addr
      socket

  module SSL = struct
    let create_connection_handler
          ?(config = Httpun.Config.default)
          ~request_handler
          ~error_handler
      =
     fun client_addr socket ->
      let create_connection =
        Httpun.Server_connection.create
          ~config
          ~error_handler:(error_handler client_addr)
      in
      Gluten_async.Server.SSL.create_upgradable_connection_handler
        ~read_buffer_size:config.read_buffer_size
        ~protocol:(module Httpun.Server_connection)
        ~create_protocol:create_connection
        ~request_handler
        client_addr
        socket

    let create_connection_handler_with_default
          ~certfile
          ~keyfile
          ?config
          ~request_handler
          ~error_handler
      =
      let make_ssl_server =
        Gluten_async.Server.SSL.create_default
          ~alpn_protocols:[ "http/1.1" ]
          ~certfile
          ~keyfile
      in
      fun client_addr socket ->
        make_ssl_server client_addr socket >>= fun ssl_server ->
        create_connection_handler
          ?config
          ~request_handler
          ~error_handler
          client_addr
          ssl_server
  end
end

module Client = struct
  module Client_runtime = Gluten_async.Client

  type 'a socket = 'a Client_runtime.socket
  type 'a runtime = 'a Client_runtime.t

  type 'a t =
    { connection : Httpun.Client_connection.t
    ; runtime : 'a runtime
    }

  let create_connection ?(config = Httpun.Config.default) socket =
    let connection = Httpun.Client_connection.create ~config () in
    Client_runtime.create
      ~read_buffer_size:config.read_buffer_size
      ~protocol:(module Httpun.Client_connection)
      connection
      socket
    >>| fun runtime -> { runtime; connection }

  let request t = Httpun.Client_connection.request t.connection
  let shutdown t = Client_runtime.shutdown t.runtime
  let is_closed t = Client_runtime.is_closed t.runtime
  let upgrade t protocol = Client_runtime.upgrade t.runtime protocol

  module SSL = struct
    module Client_runtime = Gluten_async.Client.SSL

    type 'a socket = 'a Client_runtime.socket
    type 'a runtime = 'a Client_runtime.t

    type 'a t =
      { connection : Httpun.Client_connection.t
      ; runtime : 'a runtime
      }

    let create_connection ?(config = Httpun.Config.default) socket =
      let connection = Httpun.Client_connection.create ~config () in
      Client_runtime.create
        ~read_buffer_size:config.read_buffer_size
        ~protocol:(module Httpun.Client_connection)
        connection
        socket
      >>| fun runtime -> { runtime; connection }

    let request t = Httpun.Client_connection.request t.connection
    let shutdown t = Client_runtime.shutdown t.runtime
    let is_closed t = Client_runtime.is_closed t.runtime
    let upgrade t protocol = Client_runtime.upgrade t.runtime protocol

    let create_connection_with_default ?config socket =
      Client_runtime.create_default ~alpn_protocols:[ "http/1.1" ] socket
      >>= fun ssl_client -> create_connection ?config ssl_client
  end
end

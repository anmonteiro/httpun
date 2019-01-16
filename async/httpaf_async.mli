open Async

open Httpaf

module Server : sig
  val create_connection_handler
    :  ?config         : Config.t
    -> request_handler : ('a -> (([`Active], 'a) Socket.t, unit Deferred.t) Server_connection.request_handler)
    -> error_handler   : ('a -> Server_connection.error_handler)
    -> ([< Socket.Address.t] as 'a)
    -> ([`Active], 'a) Socket.t
    -> unit Deferred.t

  module SSL : sig
    val create_connection_handler
      :  ?server         : Ssl_io.server
      -> ?certfile       : string
      -> ?keyfile        : string
      -> ?config         : Config.t
      -> request_handler : ('a -> (Ssl_io.server, unit Deferred.t) Server_connection.request_handler)
      -> error_handler   : ('a -> Server_connection.error_handler)
      -> ([< Socket.Address.t] as 'a)
      -> ([`Active], 'a) Socket.t
      -> unit Deferred.t
  end

end

module Client : sig
  type t

  val create_connection
    : ?config:Config.t
    -> ([`Active], [< Socket.Address.t]) Socket.t
    -> t

  val request
    :  t
    -> Request.t
    -> error_handler    : Client_connection.error_handler
    -> response_handler : Client_connection.response_handler
    -> [`write] Body.t

  val shutdown : t -> unit

  val is_closed : t -> bool

  module SSL : sig
  val create_connection
    :  ?client          : Ssl_io.client
    -> ?config:Config.t
    -> ([`Active], [< Socket.Address.t]) Socket.t
    -> t

  val request
    :  t
    -> Request.t
    -> error_handler    : Client_connection.error_handler
    -> response_handler : Client_connection.response_handler
    -> [`write] Body.t

  val shutdown : t -> unit

  val is_closed : t -> bool

  end
end

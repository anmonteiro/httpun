module type Server = sig
  type socket

  val create_connection_handler
    :  ?config         : Httpaf.Config.t
    -> request_handler : (Eio.Net.Sockaddr.stream  -> Httpaf.Reqd.t Gluten.reqd -> unit)
    -> error_handler   : (Eio.Net.Sockaddr.stream -> Httpaf.Server_connection.error_handler)
    -> Eio.Net.Sockaddr.stream
    -> socket
    -> unit
end

module type Client = sig
  type socket
  type runtime

  type t =
    { connection: Httpaf.Client_connection.t
    ; runtime: runtime
    }

  val create_connection
    :  ?config:Httpaf.Config.t -> sw:Eio.Switch.t -> socket -> t

  val request
    :  t
    -> ?flush_headers_immediately:bool
    -> Httpaf.Request.t
    -> error_handler    : Httpaf.Client_connection.error_handler
    -> response_handler : Httpaf.Client_connection.response_handler
    -> Httpaf.Body.Writer.t

  val shutdown: t -> unit

  val is_closed : t -> bool

  val upgrade : t -> Gluten.impl -> unit
end

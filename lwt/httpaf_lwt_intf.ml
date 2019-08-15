open Httpaf

module type IO = sig
  type socket
  type addr

  (** The region [[off, off + len)] is where read bytes can be written to *)
  val read
    :  socket
    -> Bigstringaf.t
    -> off:int
    -> len:int
    -> [ `Eof | `Ok of int ] Lwt.t

  val writev
    : socket
    -> Faraday.bigstring Faraday.iovec list
    -> [ `Closed | `Ok of int ] Lwt.t

  val shutdown_send : socket -> unit

  val shutdown_receive : socket -> unit

  val close : socket -> unit Lwt.t
end

module type Server = sig
  type socket

  type addr

  val create_connection_handler
    :  ?config         : Config.t
    -> request_handler : (addr -> socket Server_connection.request_handler)
    -> error_handler   : (addr -> Server_connection.error_handler)
    -> addr
    -> socket
    -> unit Lwt.t
end

module type Client = sig
  type t

  type socket

  val create_connection
    : ?config          : Config.t
    -> socket
    -> t Lwt.t

  val request
    :  t
    -> Request.t
    -> error_handler    : Client_connection.error_handler
    -> response_handler : Client_connection.response_handler
    -> [`write] Body.t

  val shutdown: t -> unit

  val is_closed : t -> bool
end

(*----------------------------------------------------------------------------
  Copyright (c) 2017 Inhabited Type LLC. Copyright (c) 2019 Antonio Nuno
  Monteiro.

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

(** httpun is a high-performance, memory-efficient, and scalable HTTP/1.x
    library for OCaml. It implements the HTTP 1.1 specification with respect to
    parsing, serialization, and pipelining. *)

(** {2 Basic HTTP Types} *)

module IOVec : module type of Httpun_types.IOVec
module Version : module type of Httpun_types.Version
module Method : module type of Httpun_types.Method
module Status : module type of Httpun_types.Status
module Headers : module type of Httpun_types.Headers

(** {2 Message Body} *)

module Body : sig
  module Reader : sig
    type t

    val schedule_read :
       t
      -> on_eof:(unit -> unit)
      -> on_read:(Bigstringaf.t -> off:int -> len:int -> unit)
      -> unit
    (** [schedule_read t ~on_eof ~on_read] will setup [on_read] and [on_eof] as
        callbacks for when bytes are available in [t] for the application to
        consume, or when the input channel has been closed and no further bytes
        will be received by the application.

        Once either of these callbacks have been called, they become inactive.
        The application is responsible for scheduling subsequent reads, either
        within the [on_read] callback or by some other mechanism. *)

    val close : t -> unit
    (** [close t] closes [t], indicating that any subsequent input received
        should be discarded. *)

    val is_closed : t -> bool
    (** [is_closed t] is [true] if {!close} has been called on [t] and [false]
        otherwise. A closed [t] may still have bytes available for reading. *)
  end

  module Writer : sig
    type t

    val write_char : t -> char -> unit
    (** [write_char w char] copies [char] into an internal buffer. If possible,
        this write will be combined with previous and/or subsequent writes
        before transmission. *)

    val write_string : t -> ?off:int -> ?len:int -> string -> unit
    (** [write_string w ?off ?len str] copies [str] into an internal buffer. If
        possible, this write will be combined with previous and/or subsequent
        writes before transmission. *)

    val write_bigstring : t -> ?off:int -> ?len:int -> Bigstringaf.t -> unit
    (** [write_bigstring w ?off ?len bs] copies [bs] into an internal buffer. If
        possible, this write will be combined with previous and/or subsequent
        writes before transmission. *)

    val schedule_bigstring : t -> ?off:int -> ?len:int -> Bigstringaf.t -> unit
    (** [schedule_bigstring w ?off ?len bs] schedules [bs] to be transmitted at
        the next opportunity without performing a copy. [bs] should not be
        modified until a subsequent call to {!flush} has successfully completed.
    *)

    val flush : t -> ([ `Written | `Closed ] -> unit) -> unit
    (** [flush t f] makes all bytes in [t] available for writing to the awaiting
        output channel. Once those bytes have reached that output channel,
        [f `Written] will be called. If instead, the output channel is closed
        before all of those bytes are successfully written, [f `Closed] will be
        called.

        The type of the output channel is runtime-dependent, as are guarantees
        about whether those packets have been queued for delivery or have
        actually been received by the intended recipient. *)

    val close : t -> unit
    (** [close t] closes [t], causing subsequent write calls to raise. If [t] is
        writable, this will cause any pending output to become available to the
        output channel. *)

    val is_closed : t -> bool
    (** [is_closed t] is [true] if {!close} has been called on [t], or if the
        attached output channel is closed (e.g. because
        [report_write_result `Closed] has been called). A closed [t] may still
        have pending output. *)
  end
end

(** {2 Message Types} *)

(** Request

    A client-initiated HTTP message. *)
module Request : sig
  type t =
    { meth : Method.t
    ; target : string
    ; version : Version.t
    ; headers : Headers.t
    }

  val create :
     ?version:Version.t (** default is HTTP 1.1 *)
    -> ?headers:Headers.t (** default is {!Headers.empty} *)
    -> Method.t
    -> string
    -> t

  module Body_length : sig
    type t =
      [ `Fixed of Int64.t
      | `Chunked
      | `Error of [ `Bad_request ]
      ]

    val pp_hum : Format.formatter -> t -> unit
  end

  val body_length : t -> Body_length.t
  (** [body_length t] is the length of the message body accompanying [t]. It is
      an error to generate a request with a close-delimited message body.

      See {{:https://tools.ietf.org/html/rfc7230#section-3.3.3} RFC7230§3.3.3}
      for more details. *)

  val persistent_connection : ?proxy:bool -> t -> bool
  (** [persistent_connection ?proxy t] indicates whether the connection for [t]
      can be reused for multiple requests and responses. If the calling code
      is acting as a proxy, it should pass [~proxy:true].

      See {{:https://tools.ietf.org/html/rfc7230#section-6.3} RFC7230§6.3 for
      more details. *)

  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

(** Response

    A server-generated message to a {Request}. *)
module Response : sig
  type t =
    { version : Version.t
    ; status : Status.t
    ; reason : string
    ; headers : Headers.t
    }

  val create :
     ?reason:string
       (** default is determined by {!Status.default_reason_phrase} *)
    -> ?version:Version.t (** default is HTTP 1.1 *)
    -> ?headers:Headers.t (** default is {!Headers.empty} *)
    -> Status.t
    -> t
  (** [create ?reason ?version ?headers status] creates an HTTP response with
      the given parameters. For typical use cases, it's sufficient to provide
      values for [headers] and [status]. *)

  module Body_length : sig
    type t =
      [ `Fixed of Int64.t
      | `Chunked
      | `Close_delimited
      | `Error of [ `Bad_gateway | `Internal_server_error ]
      ]

    val pp_hum : Format.formatter -> t -> unit
  end

  val body_length :
     ?proxy:bool
    -> request_method:Method.standard
    -> t
    -> Body_length.t
  (** [body_length ?proxy ~request_method t] is the length of the message body
      accompanying [t] assuming it is a response to a request whose method was
      [request_method]. If the calling code is acting as a proxy, it should pass
      [~proxy:true]. This optional parameter only affects error reporting.

      See {{:https://tools.ietf.org/html/rfc7230#section-3.3.3} RFC7230§3.3.3}
      for more details. *)

  val persistent_connection : ?proxy:bool -> t -> bool
  (** [persistent_connection ?proxy t] indicates whether the connection for [t]
      can be reused for multiple requests and responses. If the calling code
      is acting as a proxy, it should pass [~proxy:true].

      See {{:https://tools.ietf.org/html/rfc7230#section-6.3} RFC7230§6.3 for
      more details. *)

  val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
end

(** {2 Request Descriptor} *)
module Reqd : sig
  type t

  type error =
    [ `Bad_request
    | `Bad_gateway
    | `Internal_server_error
    | `Exn of exn
    ]

  val request : t -> Request.t
  val request_body : t -> Body.Reader.t
  val response : t -> Response.t option
  val response_exn : t -> Response.t

  (** Responding

      The following functions will initiate a response for the corresponding
      request in [t]. Depending on the state of the current connection, and the
      header values of the response, this may cause the connection to close or
      to persist for reuse by the client.

      See {{:https://tools.ietf.org/html/rfc7230#section-6.3} RFC7230§6.3} for
      more details. *)

  val respond_with_string : t -> Response.t -> string -> unit
  val respond_with_bigstring : t -> Response.t -> Bigstringaf.t -> unit

  val respond_with_streaming :
     ?flush_headers_immediately:bool
    -> t
    -> Response.t
    -> Body.Writer.t

  val respond_with_upgrade : t -> Headers.t -> (unit -> unit) -> unit

  (** {3 Exception Handling} *)

  val error_code : t -> error option
  val report_exn : t -> exn -> unit
  val try_with : t -> (unit -> unit) -> (unit, exn) result
end

(** {2 Buffer Size Configuration} *)
module Config : sig
  type t =
    { read_buffer_size : int  (** Default is [4096] *)
    ; request_body_buffer_size : int  (** Default is [4096] *)
    ; response_buffer_size : int  (** Default is [1024] *)
    ; response_body_buffer_size : int  (** Default is [4096] *)
    ; proxy : bool
    }

  val default : t
  (** [default] is a configuration record with all parameters set to their
      default values. *)
end

(** {2 Server Connection} *)

module Server_connection : sig
  type t
  type error = Reqd.error
  type request_handler = Reqd.t -> unit

  type error_handler =
    ?request:Request.t -> error -> (Headers.t -> Body.Writer.t) -> unit

  val create :
     ?config:Config.t
    -> ?error_handler:error_handler
    -> request_handler
    -> t
  (** [create ?config ?error_handler ~request_handler] creates a connection
      handler that will service individual requests with [request_handler]. *)

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  (** [next_read_operation t] returns a value describing the next operation that
      the caller should conduct on behalf of the connection. *)

  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the connection.
      {!read} should be called after {!next_read_operation} returns a [`Read]
      value and additional input is available for the connection to consume. *)

  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read_eof t bigstring ~off ~len] reads bytes of input from the provided
      range of [bigstring] and returns the number of bytes consumed by the
      connection. {!read_eof} should be called after {!next_read_operation}
      returns a [`Read] and an EOF has been received from the communication
      channel. The connection will attempt to consume any buffered input and
      then shutdown the HTTP parser for the connection. *)

  val yield_reader : t -> (unit -> unit) -> unit
  (** [yield_reader t continue] registers with the connection to call
      [continue] when reading should resume. {!yield_reader} should be called
      after {next_read_operation} returns a [`Yield] value. *)

  val next_write_operation :
     t
    -> [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int ]
  (** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. *)

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  (** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {report_write_result} should be called after a
      call to {next_write_operation} that returns a [`Write buffer] value.

        {ul
        {- [`Ok n] indicates that the caller successfully wrote [n] bytes of
        output from the buffer that the caller was provided by
        {next_write_operation}. }
        {- [`Closed] indicates that the output destination will no longer
        accept bytes from the write processor. }} *)

  val yield_writer : t -> (unit -> unit) -> unit
  (** [yield_writer t continue] registers with the connection to call
      [continue] when writing should resume. {!yield_writer} should be called
      after {next_write_operation} returns a [`Yield] value. *)

  val report_exn : t -> exn -> unit
  (** [report_exn t exn] reports that an error [exn] has been caught and that it
      has been attributed to [t]. Calling this function will switch [t] into an
      error state. Depending on the state [t] is transitioning from, it may call
      its error handler before terminating the connection. *)

  val is_closed : t -> bool
  (** [is_closed t] is [true] if both the read and write processors have been
      shutdown. When this is the case {!next_read_operation} will return
      [`Close _] and {!next_write_operation} will return [`Write _] until all
      buffered output has been flushed. *)

  val error_code : t -> error option
  (** [error_code t] returns the [error_code] that caused the connection to
      close, if one exists. *)

  (**/**)

  val shutdown : t -> unit

  (**/**)
end

(** {2 Client Connection} *)

module Client_connection : sig
  type t

  type error =
    [ `Malformed_response of string
    | `Invalid_response_body_length of Response.t
    | `Exn of exn
    ]

  type response_handler = Response.t -> Body.Reader.t -> unit
  type error_handler = error -> unit

  val create : ?config:Config.t -> unit -> t

  val request :
     t
    -> ?flush_headers_immediately:bool
    -> Request.t
    -> error_handler:error_handler
    -> response_handler:response_handler
    -> Body.Writer.t

  val next_read_operation : t -> [ `Read | `Yield | `Close ]
  (** [next_read_operation t] returns a value describing the next operation that
      the caller should conduct on behalf of the connection. *)

  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the connection.
      {!read} should be called after {!next_read_operation} returns a [`Read]
      value and additional input is available for the connection to consume. *)

  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read_eof t bigstring ~off ~len] reads bytes of input from the provided
      range of [bigstring] and returns the number of bytes consumed by the
      connection. {!read_eof} should be called after {!next_read_operation}
      returns a [`Read] and an EOF has been received from the communication
      channel. The connection will attempt to consume any buffered input and
      then shutdown the HTTP parser for the connection. *)

  val next_write_operation :
     t
    -> [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int ]
  (** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. *)

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  (** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {report_write_result} should be called after a
      call to {next_write_operation} that returns a [`Write buffer] value.

        {ul
        {- [`Ok n] indicates that the caller successfully wrote [n] bytes of
        output from the buffer that the caller was provided by
        {next_write_operation}. }
        {- [`Closed] indicates that the output destination will no longer
        accept bytes from the write processor. }} *)

  val yield_reader : t -> (unit -> unit) -> unit

  val yield_writer : t -> (unit -> unit) -> unit
  (** [yield_writer t continue] registers with the connection to call
      [continue] when writing should resume. {!yield_writer} should be called
      after {next_write_operation} returns a [`Yield] value. *)

  val report_exn : t -> exn -> unit
  (** [report_exn t exn] reports that an error [exn] has been caught and that it
      has been attributed to [t]. Calling this function will switch [t] into an
      error state. Depending on the state [t] is transitioning from, it may call
      its error handler before terminating the connection. *)

  val is_closed : t -> bool
  (** [is_closed t] is [true] if both the read and write processors have been
      shutdown. When this is the case {!next_read_operation} will return
      [`Close _] and {!next_write_operation} will return [`Write _] until all
      buffered output has been flushed, at which point it will also return
      `Close. *)

  val shutdown : t -> unit
  (** [shutdown connection] closes the underlying input and output channels of
      the connection, rendering it unusable for any further communication. *)
end

(**/**)

module Httpun_private : sig
  module Parse : sig
    val request : Request.t Angstrom.t
    val response : Response.t Angstrom.t
  end

  module Serialize : sig
    val write_request : Faraday.t -> Request.t -> unit
    val write_response : Faraday.t -> Response.t -> unit
  end
end

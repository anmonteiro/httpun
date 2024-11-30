(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.

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

(** Header Fields

    Each header field consists of a case-insensitive {b field name} and a {b
    field value}. The order in which header fields {i with differing field
    names} are received is not significant. However, it is good practice to
    send header fields that contain control data first so that implementations
    can decide when not to handle a message as early as possible.

    A sender MUST NOT generate multiple header fields with the same field name
    in a message unless either the entire field value for that header field is
    defined as a comma-separated list or the header field is a well-known
    exception, e.g., [Set-Cookie].

    A recipient MAY combine multiple header fields with the same field name
    into one "field-name: field-value" pair, without changing the semantics of
    the message, by appending each subsequent field value to the combined field
    value in order, separated by a comma. {i The order in which header fields
    with the same field name are received is therefore significant to the
    interpretation of the combined field value}; a proxy MUST NOT change the
    order of these field values when forwarding a message.

    {i Note.} Unless otherwise specified, all operations preserve header field
    order and all reference to equality on names is assumed to be
    case-insensitive.

    See {{:https://tools.ietf.org/html/rfc7230#section-3.2} RFC7230§3.2} for
    more details. *)

type t

type name = string
type value = string

(** Case-insensitive equality for testing header names or values *)
val ci_equal : string -> string -> bool

val empty : t
val singleton : (name * value) -> t

val of_list     : (name * value) list -> t
val of_rev_list : (name * value) list -> t
val to_list     : t -> (name * value) list
val to_rev_list : t -> (name * value) list

val add               : t -> name -> value -> t
val add_unless_exists : t -> name -> value -> t
val add_list          : t -> (name * value) list -> t
val add_multi         : t -> (name * value list) list -> t

val remove  : t -> name -> t
val replace : t -> name -> value -> t

val mem       : t -> name -> bool
val get       : t -> name -> value option
val get_exn   : t -> name -> value
val get_multi : t -> name -> value list

val iter : f:(name -> value -> unit) -> t -> unit
val fold : f:(name -> value -> 'a -> 'a) -> init:'a -> t -> 'a

val to_string : t -> string
val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

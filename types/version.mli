(*----------------------------------------------------------------------------
  Copyright (c) 2017 Inhabited Type LLC.

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

(** Protocol Version

    HTTP uses a "<major>.<minor>" numbering scheme to indicate versions of the
    protocol. The protocol version as a whole indicates the sender's conformance
    with the set of requirements laid out in that version's corresponding
    specification of HTTP.

    See {{:https://tools.ietf.org/html/rfc7230#section-2.6} RFC7230§2.6} for
    more details. *)

type t =
  { major : int  (** The major protocol number. *)
  ; minor : int  (** The minor protocol number. *)
  }

val v1_0 : t
val v1_1 : t
val compare : t -> t -> int
val to_string : t -> string
val of_string : string -> t
val pp_hum : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

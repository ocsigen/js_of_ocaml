(*
Copyright (c) 2010 Martin Jambon
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. The name of the author may not be used to endorse or promote products
   derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** Output buffer *)

type t = {
  mutable o_s : string;
    (** Buffer string *)

  mutable o_max_len : int;
    (** Same as [String.length s] *)

  mutable o_len : int;
    (** Length of data already written = current position *)

  o_init_len : int;
    (** Initial length of the buffer *)

  o_make_room : t -> int -> unit;
  (**
    [make_room buf n] must provide space for at least the requested
    number of bytes [n], typically by reallocating a larger buffer
    string or by flushing the data to a channel.
    This function is only called when there is not enough space for [n]
    bytes.
  *)
}

val really_extend : t -> int -> unit
  (**
    Default make_room function: reallocate a larger buffer string.
  *)

val flush_to_channel : out_channel -> t -> int -> unit
  (**
    Alternate make_room function: write to an out_channel.
  *)

val create : ?make_room:(t -> int -> unit) -> int -> t
  (**
    Create a buffer.  The default [make_room] function is [really_extend].
  *)

val contents : t -> string
  (**
    Returns the data currently in the buffer.
  *)

val create_channel_writer : ?len:int -> out_channel -> t
val flush_channel_writer : t -> unit
  (**
    Pair of convenience functions for creating a buffer that
    flushes data to an out_channel when it is full.
  *)

val extend : t -> int -> unit
  (**
    Guarantee that the buffer string has enough room for n additional bytes.
  *)

val alloc : t -> int -> int
  (**
    [alloc buf n] makes room for [n] bytes and returns the position
    of the first byte in the buffer string [buf.s].
    It behaves as if [n] arbitrary bytes were added and it is
    the user's responsibility to set them to some meaningful values
    by accessing [buf.s] directly.
  *)

val add_string : t -> string -> unit
  (** Add a string to the buffer. *)

val add_substring : t -> string -> int -> int -> unit
  (** [add_substring dst src srcpos len] copies [len] bytes from
     string [src] to buffer [dst] starting from position [srcpos]. *)

val add_char : t -> char -> unit
  (** Add a byte to the buffer. *)

val add_char2 : t -> char -> char -> unit
  (** Add two bytes to the buffer. *)

val add_char4 : t -> char -> char -> char -> char -> unit
  (** Add four bytes to the buffer. *)

val unsafe_add_char : t -> char -> unit
  (** Add a byte to the buffer without checking that there is enough
     room for it. *)

val clear : t -> unit
  (** Remove any data present in the buffer. *)

val reset : t -> unit
  (** Remove any data present in the buffer. *)

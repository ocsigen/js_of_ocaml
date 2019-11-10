(*
Copyright (c) 2010 Martin Jambon
Copyright (c) 2010 Grégoire Henry
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

type lexbuf

val init_lexer : ?buf:Buffer.t -> Lexing.lexbuf -> lexbuf
(** Create a fresh lexbuf record. *)

val tag_error : typename:string -> lexbuf -> 'a

val read_int : lexbuf -> int

val read_bounded_int : ?min:int -> max:int -> lexbuf -> int

val read_tag_1 : int -> lexbuf -> int

val read_tag_2 : int -> int -> lexbuf -> int

val read_int32 : lexbuf -> int32

val read_int64 : lexbuf -> int64

val read_number : lexbuf -> float

val read_string : lexbuf -> string

val read_case : lexbuf -> [ `Cst of int | `NCst of int ]

val read_vcase : lexbuf -> [ `Cst of int | `NCst of int ]

val read_comma : lexbuf -> unit

val read_lbracket : lexbuf -> unit

val read_rbracket : lexbuf -> unit

val read_comma_or_rbracket : lexbuf -> [ `Comma | `RBracket ]

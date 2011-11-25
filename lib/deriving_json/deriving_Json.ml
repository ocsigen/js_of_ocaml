(* Js_of_ocaml
 * http://www.ocsigen.org
 * Copyright Grégoire Henry 2010.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** Json **)

type 'a t = {
    write: Buffer.t -> 'a -> unit;
    read: Deriving_Json_lexer.lexbuf -> 'a
  }

let to_string t v =
  let buf = Buffer.create 50 in
  t.write buf v;
  Buffer.contents buf

let to_channel t oc v =
  let buf = Buffer.create 50 in
  t.write buf v;
  Buffer.output_buffer oc buf

let from_string t s =
  t.read (Deriving_Json_lexer.init_lexer (Lexing.from_string s))

let from_channel t ic =
  t.read (Deriving_Json_lexer.init_lexer (Lexing.from_channel ic))

(** Deriver **)

module type Json_min = sig
  type a
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
end

module type Json_min' = sig
  type a
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
  val match_variant: [`Cst of int | `NCst of int] -> bool
  val read_variant: Deriving_Json_lexer.lexbuf -> [`Cst of int | `NCst of int] -> a
end

module type Json_min'' = sig
  type a
  val t : a t
end

module type Json = sig
  type a
  val t: a t
  val write: Buffer.t -> a -> unit
  val read: Deriving_Json_lexer.lexbuf -> a
  val to_string: a -> string
  (* val to_channel: out_channel -> a -> unit *)
  val from_string: string -> a
  (* val from_channel: in_channel -> a *)
  val match_variant: [`Cst of int | `NCst of int] -> bool
  val read_variant: Deriving_Json_lexer.lexbuf -> [`Cst of int | `NCst of int] -> a
end

module Defaults(J : Json_min) : Json with type a = J.a = struct
  include J
  let t = { write; read }
  let to_string v = to_string t v
  (* let to_channel oc v = to_channel t oc v *)
  let from_string s = from_string t s
  (* let from_channel ic = from_channel t ic *)
  let match_variant hash = assert false
  let read_variant buf hash = assert false
end

module Defaults'(J : Json_min') : Json with type a = J.a = struct
  include J
  let t = { write; read }
  let to_string v = to_string t v
  (* let to_channel oc v = to_channel t oc v *)
  let from_string s = from_string t s
  (* let from_channel ic = from_channel t ic *)
end

module Defaults''(J : Json_min'') : Json with type a = J.a = struct
  include J
  let read = t.read
  let write = t.write
  let to_string v = to_string t v
  (* let to_channel oc v = to_channel t oc v *)
  let from_string s = from_string t s
  (* let from_channel ic = from_channel t ic *)
  let match_variant hash = assert false
  let read_variant buf hash = assert false
end

(** Predefs *)

module Json_undef (T : sig type a end) = Defaults(struct
    type a = T.a
    let write buf _ = failwith "Unimplemented"
    let read buf = failwith "Unimplemented"
  end)

module Json_char = Defaults(struct
    type a = char
    let write buffer c =
      Buffer.add_string buffer (string_of_int (int_of_char c))
    let read buf = char_of_int (Deriving_Json_lexer.read_bounded_int ~max:255 buf)
  end)

module Json_bool =  Defaults(struct
  type a = bool
  let write buffer b =
    Buffer.add_char buffer (if b then '1' else '0')
  let read buf = 1 = Deriving_Json_lexer.read_bounded_int ~max:1 buf
end)
module Json_unit = Defaults(struct
  type a = unit
  let write buffer () = Buffer.add_char buffer '0'
  let read buf = ignore(Deriving_Json_lexer.read_bounded_int ~max:0buf)
end)
module Json_int =  Defaults(struct
    type a = int
    let write buffer i = Format.bprintf buffer "%d" i
    let read buf = Deriving_Json_lexer.read_int buf
end)
module Json_int32 =  Defaults(struct
    type a = int32
    let write buffer i = Format.bprintf buffer "%ld" i
    let read buf = Deriving_Json_lexer.read_int32 buf
end)
module Json_int64 =  Defaults(struct
  type a = int64
  let mask24 = Int64.of_int 0xffffff
  let mask16 = Int64.of_int 0xffff
  let write buffer i =
    Printf.bprintf buffer "[255,%Ld,%Ld,%Ld]"
      (Int64.logand i mask24)
      (Int64.logand (Int64.shift_right i 24) mask24)
      (Int64.logand (Int64.shift_right i 48) mask16)
  let read buf =
    Deriving_Json_lexer.read_lbracket buf;
    ignore(Deriving_Json_lexer.read_bounded_int ~min:255 ~max:255 buf);
    Deriving_Json_lexer.read_comma buf;
    let h1 = Deriving_Json_lexer.read_int64 buf in
    Deriving_Json_lexer.read_comma buf;
    let h2 = Int64.shift_left (Deriving_Json_lexer.read_int64 buf) 24 in
    Deriving_Json_lexer.read_comma buf;
    let h3 = Int64.shift_left (Deriving_Json_lexer.read_int64 buf) 48 in
    Deriving_Json_lexer.read_rbracket buf;
    Int64.logor h3 (Int64.logor h2 h1)
end)

module Json_nativeint = Json_undef(struct type a = nativeint end)
(* module Json_num = Json_undef(struct type a = Num.num end) *)
module Json_float = Defaults(struct
    type a = float
    let write buffer f = Printf.bprintf buffer "%e" f
    let read buf = Deriving_Json_lexer.read_number buf
end)
module Json_string = Defaults(struct
  (* Given that JSON must be valid UTF-8 and that OCaml string are
     just a sequence of byte we need to "embed" byte string in an
     UTF-8 sequence. Each byte af an OCaml string is considered as
     Unicode code point (< 256) and then encoded in UTF-8. Hence,
     bytes greater than 127 are "wrapped" in two bytes.  *)
  type a = string
  let write buffer s =
    Buffer.add_char buffer '\"';
    for i = 0 to String.length s - 1 do
      match s.[i] with
	| '\"' -> Buffer.add_string buffer "\\\""
	| '\\' -> Buffer.add_string buffer "\\\\"
	| '\b' -> Buffer.add_string buffer "\\b"
	| '\x0C' -> Buffer.add_string buffer "\\f"
	| '\n' -> Buffer.add_string buffer "\\n"
	| '\r' -> Buffer.add_string buffer "\\r"
	| '\t' -> Buffer.add_string buffer "\\t"
	| c when c <= '\x1F' -> (* Other control characters are escaped. *)
	  Printf.bprintf buffer "\\u%04X" (int_of_char c)
      | c when c < '\x80' ->
	  Buffer.add_char buffer s.[i]
      | c (* >= '\x80' *) -> (* Bytes greater than 127 are embeded in a UTF-8 sequence. *)
	  Buffer.add_char buffer (Char.chr (0xC2 lor (Char.code s.[i] lsr 6)));
	  Buffer.add_char buffer (Char.chr (0x80 lor (Char.code s.[i] land 0x3F)))
    done;
    Buffer.add_char buffer '\"'
  let read buf = Deriving_Json_lexer.read_string buf
end)

module Json_list(A : Json) = Defaults(struct
      type a = A.a list
      let rec write buffer xs =
	match xs with
	| [] -> Buffer.add_char buffer '0'
	| x :: xs ->
	    Printf.bprintf buffer "[0,%a,%a]"
	      A.write x
	      write xs
      let rec read buf =
	match Deriving_Json_lexer.read_case buf with
	| `Cst 0 -> []
	| `NCst 0 ->
	    Deriving_Json_lexer.read_comma buf;
	    let x = A.read buf in
	    Deriving_Json_lexer.read_comma buf;
	    let xs = read buf in
	    Deriving_Json_lexer.read_rbracket buf;
	    x :: xs
	| _ -> failwith "Json_list.read: unexpected constructor."
    end)

module Json_ref(A : Json) = Defaults(struct
      type a = A.a ref
      let rec write buffer r =
	Printf.bprintf buffer "[0,%a]"
	  A.write !r
      let read buf =
	match Deriving_Json_lexer.read_case buf with
	| `NCst 0 ->
	    Deriving_Json_lexer.read_comma buf;
	    let x = A.read buf in
	    Deriving_Json_lexer.read_rbracket buf;
	    ref x
	| _ -> failwith "Json_ref.read: unexpected constructor."
    end)

module Json_option(A : Json) = Defaults(struct
      type a = A.a option
      let rec write buffer o =
	match o with
	| None -> Buffer.add_char buffer '0'
	| Some x ->
	    Printf.bprintf buffer "[0,%a]"
	      A.write x
      let read buf =
	match Deriving_Json_lexer.read_case buf with
	| `Cst 0 -> None
	| `NCst 0 ->
	    Deriving_Json_lexer.read_comma buf;
	    let x = A.read buf in
	    Deriving_Json_lexer.read_rbracket buf;
	    Some x
	| _ -> failwith "Json_option.read: unexpected constructor."
    end)

module Json_array(A : Json) = Defaults(struct
      type a = A.a array
      let write buffer a =
	Buffer.add_string buffer "[0";
	for i = 0 to Array.length a - 1 do
	  Buffer.add_char buffer ',';
	  A.write buffer a.(i);
	done;
	Buffer.add_char buffer ']'
      let rec read_list acc buf =
	match Deriving_Json_lexer.read_comma_or_rbracket buf with
	| `RBracket -> acc
	| `Comma ->
	    let x = A.read buf in
	    read_list (x :: acc) buf
      let read buf =
	match Deriving_Json_lexer.read_case buf with
	| `NCst 0 -> Array.of_list (List.rev (read_list [] buf))
	| _ -> failwith "Json_array.read: unexpected constructor."
    end)

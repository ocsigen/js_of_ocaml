(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
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

open! Deriving_Json_import
module Lexer = Deriving_Json_lexer

type 'a t =
  { write : Buffer.t -> 'a -> unit
  ; read : Lexer.lexbuf -> 'a
  }

let make write read = { write; read }

let read t = t.read

let write t = t.write

let convert t f1 f2 =
  { write = (fun buf a -> t.write buf (f2 a)); read = (fun buf -> f1 (t.read buf)) }

let to_string t v =
  let buf = Buffer.create 50 in
  t.write buf v;
  Buffer.contents buf

(*let to_channel t oc v =
  let buf = Buffer.create 50 in
  t.write buf v;
  Buffer.output_buffer oc buf
*)

let from_string t s = t.read (Lexer.init_lexer (Lexing.from_string s))

(*
let from_channel t ic =
  t.read (Lexer.init_lexer (Lexing.from_channel ic))
*)

(** Deriver **)

module type Json_min = sig
  type a

  val write : Buffer.t -> a -> unit

  val read : Lexer.lexbuf -> a
end

module type Json_min' = sig
  type a

  val write : Buffer.t -> a -> unit

  val read : Lexer.lexbuf -> a

  val match_variant : [ `Cst of int | `NCst of int ] -> bool

  val read_variant : Lexer.lexbuf -> [ `Cst of int | `NCst of int ] -> a
end

module type Json_min'' = sig
  type a

  val t : a t
end

module type Json_converter = sig
  type a

  type b

  val t : a t

  val from_ : a -> b

  val to_ : b -> a
end

module type Json = sig
  type a

  val t : a t

  val write : Buffer.t -> a -> unit

  val read : Lexer.lexbuf -> a

  val to_string : a -> string

  (* val to_channel: out_channel -> a -> unit *)
  val from_string : string -> a

  (* val from_channel: in_channel -> a *)
  val match_variant : [ `Cst of int | `NCst of int ] -> bool

  val read_variant : Lexer.lexbuf -> [ `Cst of int | `NCst of int ] -> a
end

module Defaults (J : Json_min) : Json with type a = J.a = struct
  include J

  let t = { write; read }

  let to_string v = to_string t v

  (* let to_channel oc v = to_channel t oc v *)
  let from_string s = from_string t s

  (* let from_channel ic = from_channel t ic *)
  let match_variant _hash = assert false

  let read_variant _buf _hash = assert false
end

module Defaults' (J : Json_min') : Json with type a = J.a = struct
  include J

  let t = { write; read }

  let to_string v = to_string t v

  (* let to_channel oc v = to_channel t oc v *)
  let from_string s = from_string t s

  (* let from_channel ic = from_channel t ic *)
end

module Defaults'' (J : Json_min'') : Json with type a = J.a = struct
  include J

  let read = t.read

  let write = t.write

  let to_string v = to_string t v

  (* let to_channel oc v = to_channel t oc v *)
  let from_string s = from_string t s

  (* let from_channel ic = from_channel t ic *)
  let match_variant _hash = assert false

  let read_variant _buf _hash = assert false
end

module Convert (J : Json_converter) : Json with type a = J.b = struct
  module Tmp : Json with type a = J.b = Defaults'' (struct
    type a = J.b

    let t = convert J.t J.from_ J.to_
  end)

  include Tmp
end

(** Predefs *)

module Json_undef (T : sig
  type a
end) =
Defaults (struct
  type a = T.a

  let write _buf _ = failwith "Unimplemented"

  let read _buf = failwith "Unimplemented"
end)

module Json_char = Defaults (struct
  type a = char

  let write buffer c = Buffer.add_string buffer (string_of_int (int_of_char c))

  let read buf = char_of_int (Lexer.read_bounded_int ~max:255 buf)
end)

module Json_bool = Defaults (struct
  type a = bool

  let write buffer b = Buffer.add_char buffer (if b then '1' else '0')

  let read buf = 1 = Lexer.read_tag_2 0 1 buf
end)

module Json_unit = Defaults (struct
  type a = unit

  let write buffer () = Buffer.add_char buffer '0'

  let read buf = ignore (Lexer.read_tag_1 0 buf)
end)

module Json_int = Defaults (struct
  type a = int

  let write buffer i = Printf.bprintf buffer "%d" i

  let read buf = Lexer.read_int buf
end)

module Json_int32 = Defaults (struct
  type a = int32

  let write buffer i = Printf.bprintf buffer "%ld" i

  let read buf = Lexer.read_int32 buf
end)

module Json_int64 = Defaults (struct
  type a = int64

  let mask24 = Int64.of_int 0xffffff

  let mask16 = Int64.of_int 0xffff

  let write buffer i =
    Printf.bprintf
      buffer
      "[255,%Ld,%Ld,%Ld]"
      (Int64.logand i mask24)
      (Int64.logand (Int64.shift_right i 24) mask24)
      (Int64.logand (Int64.shift_right i 48) mask16)

  let read buf =
    Lexer.read_lbracket buf;
    ignore (Lexer.read_tag_1 255 buf);
    Lexer.read_comma buf;
    let h1 = Lexer.read_int64 buf in
    Lexer.read_comma buf;
    let h2 = Int64.shift_left (Lexer.read_int64 buf) 24 in
    Lexer.read_comma buf;
    let h3 = Int64.shift_left (Lexer.read_int64 buf) 48 in
    Lexer.read_rbracket buf;
    Int64.logor h3 (Int64.logor h2 h1)
end)

module Json_nativeint = Json_undef (struct
  type a = nativeint
end)

(* module Json_num = Json_undef(struct type a = Num.num end) *)
module Json_float = Defaults (struct
  type a = float

  let write buffer f =
    (* "%.15g" can be (much) shorter; "%.17g" is round-trippable *)
    let s = Printf.sprintf "%.15g" f in
    if Poly.(float_of_string s = f)
    then Buffer.add_string buffer s
    else Printf.bprintf buffer "%.17g" f

  let read buf = Lexer.read_number buf
end)

module Json_string = Defaults (struct
  (* Given that JSON must be valid UTF-8 and that OCaml string are
     just a sequence of byte we need to "embed" byte string in an
     UTF-8 sequence. Each byte af an OCaml string is considered as
     Unicode code point (< 256) and then encoded in UTF-8. Hence,
     bytes greater than 127 are "wrapped" in two bytes. *)
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
      | c when Poly.(c <= '\x1F') ->
          (* Other control characters are escaped. *)
          Printf.bprintf buffer "\\u%04X" (int_of_char c)
      | c when Poly.(c < '\x80') -> Buffer.add_char buffer s.[i]
      | _c (* >= '\x80' *) ->
          (* Bytes greater than 127 are embedded in a UTF-8 sequence. *)
          Buffer.add_char buffer (Char.chr (0xC2 lor (Char.code s.[i] lsr 6)));
          Buffer.add_char buffer (Char.chr (0x80 lor (Char.code s.[i] land 0x3F)))
    done;
    Buffer.add_char buffer '\"'

  let read buf = Lexer.read_string buf
end)

let read_list f buf =
  let rec aux l c =
    match Lexer.read_case buf with
    | `Cst 0 ->
        for _i = c downto 1 do
          Lexer.read_rbracket buf
        done;
        List.rev l
    | `NCst 0 ->
        Lexer.read_comma buf;
        let x = f buf in
        Lexer.read_comma buf;
        aux (x :: l) (succ c)
    | _ -> Lexer.tag_error ~typename:"list" buf
  in
  aux [] 0

let write_list f buffer xs =
  let rec aux l c =
    match l with
    | [] ->
        Buffer.add_char buffer '0';
        for _i = c downto 1 do
          Buffer.add_char buffer ']'
        done
    | x :: xs ->
        Printf.bprintf buffer "[0,%a," f x;
        aux xs (succ c)
  in
  aux xs 0

module Json_list (A : Json) = Defaults (struct
  type a = A.a list

  let read = read_list A.read

  let write = write_list A.write
end)

let read_ref f buf =
  match Lexer.read_case buf with
  | `NCst 0 ->
      Lexer.read_comma buf;
      let x = f buf in
      Lexer.read_rbracket buf;
      ref x
  | _ -> Lexer.tag_error ~typename:"ref" buf

let write_ref f buffer r = Printf.bprintf buffer "[0,%a]" f !r

module Json_ref (A : Json) = Defaults (struct
  type a = A.a ref

  let write = write_ref A.write

  let read = read_ref A.read
end)

let read_option f buf =
  match Lexer.read_case buf with
  | `Cst 0 -> None
  | `NCst 0 ->
      Lexer.read_comma buf;
      let x = f buf in
      Lexer.read_rbracket buf;
      Some x
  | _ -> Lexer.tag_error ~typename:"option" buf

let write_option f buffer o =
  match o with
  | None -> Buffer.add_char buffer '0'
  | Some x -> Printf.bprintf buffer "[0,%a]" f x

module Json_option (A : Json) = Defaults (struct
  type a = A.a option

  let read = read_option A.read

  let write = write_option A.write
end)

let read_array f buf =
  let rec read_list acc buf =
    match Lexer.read_comma_or_rbracket buf with
    | `RBracket -> acc
    | `Comma ->
        let x = f buf in
        read_list (x :: acc) buf
  in
  match Lexer.read_case buf with
  (* We allow the tag 254 in case of float array *)
  | `NCst 0 | `NCst 254 -> Array.of_list (List.rev (read_list [] buf))
  | _ -> Lexer.tag_error ~typename:"array" buf

let write_array f buffer a =
  Buffer.add_string buffer "[0";
  for i = 0 to Array.length a - 1 do
    Buffer.add_char buffer ',';
    f buffer a.(i)
  done;
  Buffer.add_char buffer ']'

module Json_array (A : Json) = Defaults (struct
  type a = A.a array

  let read = read_array A.read

  let write = write_array A.write
end)

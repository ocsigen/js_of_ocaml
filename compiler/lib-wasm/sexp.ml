(* Wasm_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

open Stdlib

type t =
  | Atom of string
  | List of t list

let reserved_char c =
  match c with
  | '\x00' .. ' ' | '(' | ')' | '#' | ';' | '"' | '\x7f' .. '\xff' -> true
  | _ -> false

let need_escaping s =
  let len = String.length s in
  len = 0
  ||
  let res = ref false in
  for i = 0 to len - 1 do
    res := !res || reserved_char s.[i]
  done;
  !res

let should_quote c =
  match c with
  | '\x00' .. '\x1F' | '"' | '\\' | '\x7f' .. '\xff' -> true
  | _ -> false

let escape_to_buffer buf s =
  let start = ref 0 in
  let len = String.length s in
  Buffer.add_char buf '"';
  for i = 0 to len - 1 do
    let c = s.[i] in
    if should_quote c
    then (
      if !start < i then Buffer.add_substring buf s !start (i - !start);
      Buffer.add_char buf '\\';
      let c = Char.code c in
      Buffer.add_uint8 buf ((c / 100) + 48);
      Buffer.add_uint8 buf ((c / 10 mod 10) + 48);
      Buffer.add_uint8 buf ((c mod 10) + 48);
      start := i + 1)
  done;
  if !start < len then Buffer.add_substring buf s !start (len - !start);
  Buffer.add_char buf '"'

let rec add_to_buffer buf v =
  match v with
  | Atom s -> if need_escaping s then escape_to_buffer buf s else Buffer.add_string buf s
  | List l ->
      Buffer.add_char buf '(';
      List.iteri
        ~f:(fun i v' ->
          if i > 0 then Buffer.add_char buf ' ';
          add_to_buffer buf v')
        l;
      Buffer.add_char buf ')'

let to_string v =
  let b = Buffer.create 128 in
  add_to_buffer b v;
  Buffer.contents b

let parse_error () = failwith "parse error"

let rec parse buf s pos : t * int =
  match s.[pos] with
  | '(' -> parse_list buf s [] (pos + 1)
  | '\"' ->
      Buffer.clear buf;
      parse_quoted_atom buf s (pos + 1) (pos + 1)
  | _ -> parse_atom buf s pos pos

and parse_list buf s acc pos =
  match s.[pos] with
  | ' ' -> parse_list buf s acc (pos + 1)
  | ')' -> List (List.rev acc), pos + 1
  | _ ->
      let v, pos' = parse buf s pos in
      parse_list buf s (v :: acc) pos'

and parse_atom buf s pos0 pos =
  if reserved_char s.[pos]
  then (
    if pos0 = pos then parse_error ();
    Atom (String.sub s ~pos:pos0 ~len:(pos - pos0)), pos)
  else parse_atom buf s pos0 (pos + 1)

and parse_quoted_atom buf s pos0 pos =
  match s.[pos] with
  | '\"' ->
      if pos0 < pos then Buffer.add_substring buf s pos0 (pos - pos0);
      Atom (Buffer.contents buf), pos + 1
  | '\\' ->
      if pos0 < pos then Buffer.add_substring buf s pos0 (pos - pos0);
      Buffer.add_uint8
        buf
        (((Char.code s.[pos + 1] - 48) * 100)
        + ((Char.code s.[pos + 2] - 48) * 10)
        + Char.code s.[pos + 3]
        - 48);
      parse_quoted_atom buf s (pos + 4) (pos + 4)
  | _ -> parse_quoted_atom buf s pos0 (pos + 1)

let from_string s =
  let v, pos = parse (Buffer.create 16) s 0 in
  if pos < String.length s then parse_error ();
  v

module Util = struct
  let single f v =
    match v with
    | [ v ] -> f v
    | _ -> invalid_arg "Sexp.Util.single"

  let string v =
    match v with
    | Atom s -> s
    | _ -> invalid_arg "Sexp.Util.string"

  let assoc v =
    let invalid_arg () = invalid_arg "Sexp.Util.assoc" in
    match v with
    | List l ->
        List.map
          ~f:(fun p ->
            match p with
            | List (Atom k :: v) -> k, v
            | _ -> invalid_arg ())
          l
    | Atom _ -> invalid_arg ()

  let member nm v =
    match v with
    | Atom _ -> invalid_arg "Sexp.Util.member"
    | List l ->
        List.find_map
          ~f:(fun p ->
            match p with
            | List (Atom nm' :: v) when String.equal nm nm' -> Some v
            | _ -> None)
          l

  let bool v =
    match v with
    | Atom "true" -> true
    | Atom "false" -> false
    | _ -> invalid_arg "Sexp.Util.bool"

  let mandatory f v =
    match v with
    | Some v -> f v
    | None -> invalid_arg "Sexp.Util.mandatory"
end

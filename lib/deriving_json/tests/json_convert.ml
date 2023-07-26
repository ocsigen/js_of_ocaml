(* Js_of_ocaml example
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
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

(* Json conversion *)

open Js_of_ocaml

let str =
  let b = Buffer.create 256 in
  for i = 0 to 255 do
    Buffer.add_char b (Char.chr i)
  done;
  Buffer.contents b

[@@@ocaml.warning "-39"]

type t = int list * float option * string [@@deriving json]

let wasm =
  match Sys.backend_type with
  | Other "wasm_of_ocaml" -> true
  | _ -> false

let test t v =
  if wasm || v = Json.unsafe_input (Json.output v) then () else print_endline "Not equal";
  if wasm || v = Deriving_Json.from_string t (Js.to_string (Json.output v))
  then ()
  else print_endline "Not equal";
  if wasm || v = Json.unsafe_input (Js.string (Deriving_Json.to_string t v))
  then ()
  else print_endline "Not equal";
  if v = Deriving_Json.from_string t (Deriving_Json.to_string t v)
  then ()
  else print_endline "Not equal"

let%expect_test _ =
  test json ([ 1; 2; 3 ], Some 1., str);
  [%expect {||}]

type intseq =
  | Z
  | S of int * intseq
[@@deriving json]

let%expect_test _ =
  test intseq_json (S (1, S (2, S (3, Z))));
  [%expect {||}]

type 'a seq =
  | ZZ
  | SS of 'a * 'a seq
[@@deriving json]

type w = int seq [@@deriving json]

let%expect_test _ =
  test w_json (SS (1, SS (2, SS (3, ZZ))));
  [%expect {||}]

type i64 = int64 [@@deriving json]

let%expect_test _ =
  test i64_json 100000000000000000L;
  [%expect {||}]

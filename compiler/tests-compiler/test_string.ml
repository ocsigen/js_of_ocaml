(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Ty Overby
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

open Util

let%expect_test _ =
  let program =
    compile_and_parse
      ~debug:false
      ~flags:[ "--enable"; "use-js-string" ]
      {|
external string_length : string -> int = "%string_length"
external bytes_create : int -> bytes = "caml_create_bytes"
external string_blit : string -> int -> bytes -> int -> int -> unit
                     = "caml_blit_string" [@@noalloc]
external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"

let ( ^ ) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = bytes_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  bytes_unsafe_to_string s

let here () =
  let a = "a" in
  let b = "b" in
  a ^ a ^ b ^ b

let (_ : string) = here ()
    |}
  in
  print_fun_decl program None;
  [%expect
    {|
    function _b_(_c_){return cst_a + (cst_a + (cst_b + cst_b))} |}]

let%expect_test _ =
  let program =
    compile_and_parse
      ~debug:false
      ~flags:[ "--disable"; "use-js-string" ]
      {|
external string_length : string -> int = "%string_length"
external bytes_create : int -> bytes = "caml_create_bytes"
external string_blit : string -> int -> bytes -> int -> int -> unit
                     = "caml_blit_string" [@@noalloc]

external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"

let ( ^ ) s1 s2 =
  let l1 = string_length s1 and l2 = string_length s2 in
  let s = bytes_create (l1 + l2) in
  string_blit s1 0 s 0 l1;
  string_blit s2 0 s l1 l2;
  bytes_unsafe_to_string s

let here () =
  let a = "a" in
  let b = "b" in
  a ^ a ^ b ^ b

let (_ : string) = here ()
    |}
  in
  print_fun_decl program None;
  [%expect
    {|
    function _b_(_c_)
     {return caml_string_concat
              (cst_a,caml_string_concat(cst_a,caml_string_concat(cst_b,cst_b)))} |}]

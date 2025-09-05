(* Js_of_ocaml
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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

open! Stdlib
open StdLabels
open Bigarray

module Complex = struct
  type t = Complex.t =
    { re : float
    ; im : float
    }

  let to_string { re; im } = Printf.sprintf "%g+%gi" re im

  let of_string s =
    match String.split_on_char ~sep:'+' s with
    | [ x ] ->
        if x.[String.length x - 2] = 'i'
        then
          { re = 0.
          ; im = Float.of_string (String.sub x ~pos:0 ~len:(String.length x - 1))
          }
        else { re = Float.of_string x; im = 0. }
    | [ x; y ] ->
        assert (y.[String.length y - 2] = 'i');
        { re = Float.of_string x
        ; im = Float.of_string (String.sub y ~pos:0 ~len:(String.length y - 1))
        }
    | _ -> assert false
end

module Char = struct
  let to_string = Printf.sprintf "'%C'"

  let code = Char.code

  let chr = Char.chr
end

let from_list kind vals =
  let a = Array1.create kind c_layout (List.length vals) in
  let rec set i = function
    | [] -> ()
    | hd :: tl ->
        a.{i} <- hd;
        set (i + 1) tl
  in
  set 0 vals;
  a

let%expect_test "compare elt" =
  let test kind to_string a b =
    let op c =
      if c = 0 then "=" else if c < 0 then "<" else if c > 0 then ">" else "??"
    in
    let c = compare a b in
    let c' = compare (from_list kind [ a ]) (from_list kind [ b ]) in
    if c' = c
    then
      Printf.printf
        "%s %s %s: Bigarray compare the same\n"
        (to_string a)
        (op c)
        (to_string b)
    else
      Printf.printf
        "%s %s %s vs %s %s %s: Bigarray compare differently\n"
        (to_string a)
        (op c)
        (to_string b)
        (to_string a)
        (op c')
        (to_string b)
  in
  test int64 Int64.to_string 0x0000000110000000L 0x0000001001000000L;
  [%expect {| 4563402752 < 68736253952: Bigarray compare the same |}];
  test int64 Int64.to_string 8796093022210L 17592186044417L;
  [%expect {| 8796093022210 < 17592186044417: Bigarray compare the same |}];
  test int64 Int64.to_string 0xffffffffL 0x0fffffffL;
  [%expect {| 4294967295 > 268435455: Bigarray compare the same |}];
  test int32 Int32.to_string 0x00011000l 0x00100100l;
  [%expect {| 69632 < 1048832: Bigarray compare the same |}];
  test int32 Int32.to_string 0xffff0000l 0x0000ffffl;
  [%expect {| -65536 < 65535: Bigarray compare the same |}];
  test complex32 Complex.to_string { re = 1.0; im = 0.0 } { re = 0.0; im = 1.0 };
  [%expect {| 1+0i > 0+1i: Bigarray compare the same |}];
  test complex64 Complex.to_string { re = 1.0; im = 0.0 } { re = 0.0; im = 1.0 };
  [%expect {| 1+0i > 0+1i: Bigarray compare the same |}];
  test float32 Float.to_string 1.0 0.0;
  [%expect {| 1. > 0.: Bigarray compare the same |}];
  test float32 Float.to_string nan nan;
  [%expect {| nan = nan: Bigarray compare the same |}];
  test float64 Float.to_string 0.0 1.0;
  [%expect {| 0. < 1.: Bigarray compare the same |}];
  test float64 Float.to_string nan nan;
  [%expect {| nan = nan: Bigarray compare the same |}];
  test int8_signed Int.to_string (-1) 1;
  [%expect {| -1 < 1: Bigarray compare the same |}];
  test int8_unsigned Int.to_string (-1) 1;
  [%expect {| -1 < 1 vs -1 > 1: Bigarray compare differently |}];
  test int8_unsigned Int.to_string 2 3;
  [%expect {| 2 < 3: Bigarray compare the same |}];
  test int16_signed Int.to_string (-1) 1;
  [%expect {| -1 < 1: Bigarray compare the same |}];
  test int16_unsigned Int.to_string (-1) 1;
  [%expect {| -1 < 1 vs -1 > 1: Bigarray compare differently |}];
  test int16_unsigned Int.to_string 2 3;
  [%expect {| 2 < 3: Bigarray compare the same |}];
  test int Int.to_string (-65536) 65535;
  [%expect {| -65536 < 65535: Bigarray compare the same |}];
  test int Int.to_string 0x00011000 0x00100100;
  [%expect {| 69632 < 1048832: Bigarray compare the same |}];
  test nativeint Nativeint.to_string (-65536n) 65535n;
  [%expect {| -65536 < 65535: Bigarray compare the same |}];
  test nativeint Nativeint.to_string 0x00011000n 0x00100100n;
  [%expect {| 69632 < 1048832: Bigarray compare the same |}];
  test char Char.to_string '\000' '\001';
  [%expect {| ''\000'' < ''\001'': Bigarray compare the same |}];
  test char Char.to_string '\255' '\000';
  [%expect {| ''\255'' > ''\000'': Bigarray compare the same |}]

let%expect_test "compare" =
  let test (type a b) (a : a) (b : b) =
    let c = compare (Obj.magic a) (Obj.magic b) in
    let s = if c = 0 then "Eq" else if c < 0 then "<" else ">" in
    Printf.printf "%s\n" s
  in
  (* different kind *)
  test (Array1.create int8_signed c_layout 1) (Array1.create char c_layout 1);
  [%expect {| > |}];
  test (Array1.create char c_layout 1) (Array1.create int8_signed c_layout 1);
  [%expect {| < |}];
  test (Array1.create int64 c_layout 1) (Array1.create char c_layout 1);
  [%expect {| > |}];
  test (Array1.create char c_layout 1) (Array1.create int64 c_layout 1);
  [%expect {| < |}];
  (* different layout *)
  test (Array1.create char c_layout 1) (Array1.create char fortran_layout 1);
  [%expect {| > |}];
  test (Array1.create char fortran_layout 1) (Array1.create char c_layout 1);
  [%expect {| < |}];
  (* different size *)
  test (Array1.create char c_layout 1) (Array1.create char c_layout 2);
  [%expect {| < |}];
  test (Array1.create char c_layout 2) (Array1.create char c_layout 1);
  [%expect {| > |}];
  test (Array2.create char c_layout 1 2) (Array1.create char c_layout 2);
  [%expect {| < |}];
  test (Array2.create char c_layout 1 2) (Array2.create char c_layout 2 1);
  [%expect {| < |}];
  test (Array2.create char c_layout 2 1) (Array2.create char c_layout 1 2);
  [%expect {| > |}];
  ()

let%expect_test "change_layout, reshape" =
  let f = Array2.create int fortran_layout 2 3 in
  let c = Array2.create int c_layout 3 2 in
  f.{1, 1} <- 1;
  f.{2, 1} <- 2;
  f.{1, 2} <- 3;
  f.{2, 2} <- 4;
  f.{1, 3} <- 5;
  f.{2, 3} <- 6;
  c.{0, 0} <- 1;
  c.{0, 1} <- 2;
  c.{1, 0} <- 3;
  c.{1, 1} <- 4;
  c.{2, 0} <- 5;
  c.{2, 1} <- 6;
  let c2 = Array2.change_layout f c_layout in
  assert (compare c c2 = 0);
  let c' = genarray_of_array2 c in
  let c2' = genarray_of_array2 c2 in
  assert (compare (reshape_1 c' 6) (reshape_1 c2' 6) = 0)

external blit_ba_to_ba :
  (_, _, _) Array1.t -> int -> (_, _, _) Array1.t -> int -> int -> unit
  = "caml_bigstring_blit_ba_to_ba"

external blit_ba_to_bytes : (_, _, _) Array1.t -> int -> Bytes.t -> int -> int -> unit
  = "caml_bigstring_blit_ba_to_bytes"

external blit_bytes_to_ba : Bytes.t -> int -> (_, _, _) Array1.t -> int -> int -> unit
  = "caml_bigstring_blit_bytes_to_ba"

external blit_string_to_ba : string -> int -> (_, _, _) Array1.t -> int -> int -> unit
  = "caml_bigstring_blit_string_to_ba"

let print_ba a =
  for i = 0 to Array1.dim a - 1 do
    Printf.printf "\\%03d" (Char.code a.{i})
  done;
  Printf.printf "\n"

let print_bytes a =
  for i = 0 to Bytes.length a - 1 do
    Printf.printf "\\%03d" (Char.code (Bytes.get a i))
  done;
  Printf.printf "\n"

let%expect_test "blit ba-ba" =
  let a = Array1.create char c_layout 10 in
  Array1.fill a '\000';
  let a' = Array1.sub a 2 6 in
  let b = Array1.create char c_layout 10 in
  Array1.fill b '\001';
  let b' = Array1.sub b 4 6 in
  for i = 0 to 10 - 1 do
    a.{i} <- Char.chr i
  done;
  blit_ba_to_ba a' 1 b' 2 3;
  print_ba a;
  print_ba b;
  [%expect
    {|
    \000\001\002\003\004\005\006\007\008\009
    \001\001\001\001\001\001\003\004\005\001 |}]

let%expect_test "blit ba-bytes" =
  let a = Array1.create char c_layout 10 in
  let a' = Array1.sub a 2 6 in
  let b = Bytes.create 10 in
  Bytes.fill b ~pos:0 ~len:10 '\000';
  for i = 0 to 10 - 1 do
    a.{i} <- Char.chr i
  done;
  blit_ba_to_bytes a' 1 b 6 3;
  print_ba a;
  print_bytes b;
  [%expect
    {|
    \000\001\002\003\004\005\006\007\008\009
    \000\000\000\000\000\000\003\004\005\000 |}]

let%expect_test "blit bytes-ba" =
  let a = Bytes.create 10 in
  Bytes.fill a ~pos:0 ~len:10 '\000';
  let b = Array1.create char c_layout 10 in
  Array1.fill b '\255';
  let b' = Array1.sub b 4 6 in
  for i = 0 to 10 - 1 do
    Bytes.set a i (Char.chr i)
  done;
  blit_bytes_to_ba a 3 b' 2 3;
  print_bytes a;
  print_ba b;
  [%expect
    {|
    \000\001\002\003\004\005\006\007\008\009
    \255\255\255\255\255\255\003\004\005\255 |}]

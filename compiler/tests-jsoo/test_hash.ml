(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Jérôme Vouillon
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

(* A forced lazy value is hashed as the value it points to. *)
let%expect_test "forced lazy value" =
  let l = lazy (Sys.opaque_identity (1, 2)) in
  ignore (Lazy.force l);
  Printf.printf "%b\n" (Hashtbl.hash l = Hashtbl.hash (1, 2));
  [%expect {| true |}]

(* With a negative count, nothing is hashed. *)
let%expect_test "negative count" =
  Printf.printf
    "%b\n"
    (Hashtbl.hash_param (-1) 100 (1, 2) = Hashtbl.hash_param (-1) 100 "abc");
  [%expect {| true |}]

(* Objects count against the number of meaningful values (blocks do not):
   with a count of 1, the hash stops after [o1]. *)
let%expect_test "objects are counted" =
  let o1 = object end and o2 = object end and o3 = object end in
  Printf.printf
    "%b\n"
    (Hashtbl.hash_param 1 100 [ o1; o2 ] = Hashtbl.hash_param 1 100 [ o1; o3 ]);
  [%expect {| true |}]

(* String hashes are pinned (and checked against native OCaml): every length
   modulo 4, bytes above 0x7f, and strings nested in other values. *)
let%expect_test "string hashes" =
  List.iter
    (fun s -> Printf.printf "%S -> %d\n" s (Hashtbl.hash s))
    [ ""
    ; "a"
    ; "ab"
    ; "abc"
    ; "abcd"
    ; "abcde"
    ; "abcdef"
    ; "abcdefg"
    ; "abcdefgh"
    ; "abcdefghi"
    ; "\128"
    ; "\255\254"
    ; "h\195\169llo"
    ; "\000\001\002\003\252\253\254\255\127"
    ];
  Printf.printf "all bytes -> %d\n" (Hashtbl.hash (String.init 256 Char.chr));
  Printf.printf "%d\n" (Hashtbl.hash [ "abc"; "\255"; "" ]);
  Printf.printf "%d\n" (Hashtbl.hash ("key", 1, Some "value"));
  Printf.printf "%d\n" (Hashtbl.hash_param 2 10 [ "a"; "b"; "c"; "d" ]);
  [%expect {|
           "" -> 0
           "a" -> 721651713
           "ab" -> 856662637
           "abc" -> 767105082
           "abcd" -> 65890154
           "abcde" -> 335633756
           "abcdef" -> 926323203
           "abcdefg" -> 577950338
           "abcdefgh" -> 165530820
           "abcdefghi" -> 640011927
           "\128" -> 873615215
           "\255\254" -> 927824754
           "h\195\169llo" -> 179461141
           "\000\001\002\003\252\253\254\255\127" -> 925112789
           all bytes -> 604638806
           177509747
           699725957
           78740037
           |}]

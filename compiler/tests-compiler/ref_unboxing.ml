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

open Util

(* Reference unboxing eliminates the variable [y] bound by [!r] and
   substitutes the current contents of [r] for it. Here [y] is then
   stored into another unboxed reference [r'] in a nested closure, so
   the variable read from [r'] gets substituted by [y]. Since [f] is
   inlined at two call sites, it is duplicated and the nested closure
   ends up with a smaller address than its parent, so it is processed
   first: the substitution must be followed transitively, otherwise
   the generated code refers to the eliminated variable [y]. *)
let%expect_test "substitution chains across nested closures" =
  compile_and_run
    {|
    let f n =
      let g m =
        let r = ref 0 in
        r := m + 1;
        let y = !r in
        let g2 () =
          let r' = ref 0 in
          r' := y;
          let z = !r' in
          z + n
        in
        g2
      in
      g

    let () =
      let h1 = f 1 in
      let h2 = f 10 in
      Printf.printf "%d %d\n" (h1 5 ()) (h2 7 ())
    |};
  [%expect {| 7 18 |}]

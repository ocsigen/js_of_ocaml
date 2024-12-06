(* Js_of_ocaml compiler
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

open Code

val f : to_lift:Var.Set.t -> program -> program * Var.t Var.Map.t
(** Lambda-lift all functions of the program that are in [to_lift]. All
    functions are lifted to toplevel. Functions that may be
    mutually recursive are lifted together. Also yields a map from the original
    function names to the names of their lambda-lifted counterparts. E.g.
    consider:

        let y = -3 in
        (* ... *)
        let rec fib n =
                match n with
                | 0 | 1 -> 1
                | _ -> fib (n-1) + fib (n-2) + y
        in
        fib 42

    After lambda-lifting of [fib], it will look like:

      let y = -3 in
      (* ... *)
      let fib' y =
              let rec fib_l n =
                      match n with
                      | 0 | 1 -> 1
                      | _ -> fib_l (n-1) + fib_l (n-2) + y
              in
              fib_l
      in
      let fib = fib' y in
      fib 42

      [fib_l] is the lifted version of [fib], [fib'] is the lifting closure.
      *)

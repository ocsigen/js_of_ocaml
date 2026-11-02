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

open Js_of_ocaml_compiler
open Code

(* Two unreachable blocks whose parameters only receive each other *)
let%expect_test "phisimpl: unreachable cycle" =
  let c = Var.fresh () in
  let x = Var.fresh () in
  let y = Var.fresh () in
  let blocks =
    Addr.Map.empty
    |> Addr.Map.add
         0
         { params = []
         ; body = [ Let (c, Constant (Int Targetint.zero)) ]
         ; branch = Return c
         }
    |> Addr.Map.add 1 { params = [ x ]; body = []; branch = Branch (2, [ x ]) }
    |> Addr.Map.add 2 { params = [ y ]; body = []; branch = Branch (1, [ y ]) }
  in
  let p = { start = 0; blocks; free_pc = 3 } in
  let p = Phisimpl.f p in
  Code.Print.program Format.std_formatter (fun _ _ -> "") p;
  Format.pp_print_flush Format.std_formatter ();
  [%expect
    {|
           Entry point: 0

           ==== 0 () ====
             v1 = CONST{0}
             return v1

           ==== 1 (v2) ====
             branch 2 (v3)

           ==== 2 (v3) ====
             branch 1 (v3)
           |}]

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

(* The use counts returned by [Deadcode.f] must account for the
   branches redirected past empty blocks: [z] is used once in block 3,
   but twice once the branches from blocks 1 and 2 are redirected to
   block 4. *)
let%expect_test "deadcode: use counts after skipping empty blocks" =
  let c = Var.fresh () in
  let z = Var.fresh () in
  let a = Var.fresh () in
  let b = Var.fresh () in
  let p = Var.fresh () in
  let q = Var.fresh () in
  let r = Var.fresh () in
  let s = Var.fresh () in
  let cst x _ = Let (x, Constant (Int Targetint.zero)) in
  let blocks =
    Addr.Map.empty
    |> Addr.Map.add
         0
         { params = []
         ; body = [ cst c 1; cst z 2; cst a 3; cst b 4 ]
         ; branch = Cond (c, (1, []), (2, []))
         }
    |> Addr.Map.add 1 { params = []; body = []; branch = Branch (3, [ a ]) }
    |> Addr.Map.add 2 { params = []; body = []; branch = Branch (3, [ b ]) }
    |> Addr.Map.add 3 { params = [ p ]; body = []; branch = Branch (4, [ p; z ]) }
    |> Addr.Map.add
         4
         { params = [ q; r ]
         ; body =
             [ Let (s, Block (0, [| q; r |], NotArray, Maybe_mutable, Block_desc.none)) ]
         ; branch = Return s
         }
  in
  let p = { start = 0; blocks; free_pc = 5 } in
  let p, live = Deadcode.f (Pure_fun.f p) p in
  Code.Print.program Format.std_formatter (fun _ _ -> "") p;
  Format.printf "uses of z: %d@." live.(Var.idx z);
  [%expect
    {|
           Entry point: 0

           ==== 0 () ====
             v1 = CONST{0}
             v2 = CONST{0}
             v3 = CONST{0}
             v4 = CONST{0}
             if v1 then 4 (v3, v2) else 4 (v4, v2)

           ==== 4 (v6, v7) ====
             v8 = {tag=0; 0 = v6; 1 = v7}
             return v8

           uses of z: 2
           |}]

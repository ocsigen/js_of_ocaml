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

type def =
  | Expr of expr
  | Var of Var.t

type state =
  { blocks : block Addr.Map.t
  ; var_life : bool Var.Tbl.t
  ; defs : def list array
  ; pure_funs : Var.Set.t
  }

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain = struct
  type t = bool

  let equal = Bool.equal

  let bot = false
end

module Solver = G.Solver (Domain)

let pure_expr pure_funs e = Pure_fun.pure_expr pure_funs e && Config.Flag.deadcode ()

let live_instr st i =
  match i with
  | Let (x, e) ->
      (* variable has been marked as live or the expression is impure *)
      Var.Tbl.get st.var_life x || not (pure_expr st.pure_funs e)
  | Assign (x, _) ->
      (* variable has been marked as live *)
      Var.Tbl.get st.var_life x
  | Set_field _ | Offset_ref _ | Array_set _ ->
      (* these are impure so always live *)
      true

let propagate deps (defs : def array) pure_funs st x =
  match defs.(Var.idx x) with
  | Expr e -> _
  | Var v -> Var.Tbl.get st.var_life x

let solver vars deps defs pure_funs =
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  Solver.f () g (propagate deps defs pure_funs)

let run (p : program) =
  let blocks = p.blocks in
  let vars, deps, defs = program_deps p in
  let pure_funs = Pure_fun.f p in
  let var_life = solver vars deps defs pure_funs in
  let st = { blocks; var_life; defs; pure_funs } in
  let remove_dead (block : block) =
    { params = List.filter (fun x -> Var.Tbl.get st.var_life x) block.params
    ; body = (* remove dead instructions *)
             _
    ; branch = (* Remove dead code from the last instruction *)
               _
    }
  in
  let blocks = Addr.Map.map remove_dead p.blocks in
  { p with blocks }

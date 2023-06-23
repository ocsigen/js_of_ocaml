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

(* type state =
   { blocks : block Addr.Map.t
   ; live : bool Var.Tbl.t
   ; defs : def list array
   ; pure_funs : Var.Set.t
   } *)

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain = struct
  type t = bool

  let equal = Bool.equal

  let bot = false
end

module Solver = G.Solver (Domain)

let pure_expr pure_funs e = Pure_fun.pure_expr pure_funs e && Config.Flag.deadcode ()

(* Returns an array of definitions of each variable *)
let definitions nv (p : program) =
  let defs = Array.make nv [] in
  let add_def x d = defs.(Var.idx x) <- d :: defs.(Var.idx x) in
  Addr.Map.iter
    (fun _ block ->
      List.iter
        (fun (i, _) ->
          match i with
          | Let (x, e) -> add_def x (Expr e)
          | Assign (x, y) -> add_def x (Var y)
          | _ -> ())
        block.body)
    p.blocks;
  defs

(* Returns the adjacency list for the variable dependency graph. *)
let dependencies nv defs =
  let deps = Array.make nv Var.Set.empty in
  let add_dep i x = deps.(i) <- Var.Set.add x deps.(i) in
  Array.iteri
    (fun i ds ->
      List.iter
        (fun d ->
          match d with
          | Expr e -> (
              match e with
              | Apply { f; args; _ } ->
                  add_dep i f;
                  List.iter (add_dep i) args
              | Block (_, params, _) -> Array.iter (add_dep i) params
              | Field (z, _) -> add_dep i z
              | Constant _ -> ()
              | Closure (params, _) -> List.iter (add_dep i) params
              | Prim (_, args) ->
                  List.iter
                    (fun arg ->
                      match arg with
                      | Pv v -> add_dep i v
                      | Pc _ -> ())
                    args)
          | Var y -> add_dep i y)
        ds)
    defs;
  deps

(* Returns a boolean array representing whether each variable appears in an effectful definition. *)
let effectful nv defs pure_funs =
  let effs = Array.make nv false in
  Array.iteri (* For each set of definitions *)
    (fun i ds ->
      List.iter (* For each definition *)
        (fun d -> (* See if definition is effectful *)
          match d with
          | Expr e -> if not (pure_expr pure_funs e) then effs.(i) <- true
          | Var y -> effs.(i) <- effs.(Var.idx y))
        ds)
    defs;
  effs

(* Returns the set of variables given the adjacency list of variable depencies. *)
let variables deps =
  let vars = Var.ISet.empty () in
  Array.iter (fun s -> Var.Set.iter (fun v -> Var.ISet.add vars v) s) deps;
  vars

(* A variable x is live if either
   (1) it appears in an effectful expression; or
   (2) there exists a live variable y that depends on x. *)
let propagate deps effectful live x =
  let idx = Var.idx x in
  Format.eprintf "%b\n" effectful.(idx);
  effectful.(idx) || Var.Set.exists (fun y -> Var.Tbl.get live y) deps.(idx)

let solver vars deps effectful =
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  Solver.f () g (propagate deps effectful)

(* let live_instr st i =
   match i with
   | Let (x, e) ->
       (* variable has been marked as live or the expression is impure *)
       Var.Tbl.get st.live x || not (pure_expr st.pure_funs e)
   | Assign (x, _) ->
       (* variable has been marked as live *)
       Var.Tbl.get st.live x
   | Set_field _ | Offset_ref _ | Array_set _ ->
       (* these are impure so always live *)
       true *)

let run (p : program) =
  let nv = Var.count () in
  let blocks = p.blocks in
  let defs = definitions nv p in
  let deps = dependencies nv defs in
  (* Print out dependency info *)
  Format.eprintf "Dependencies:\n";
  Array.iteri
    (fun i ds ->
      Format.eprintf "%d: { " i;
      Var.Set.iter (fun d -> Format.eprintf "%a " Var.print d) ds;
      Format.eprintf "}\n")
    deps;
  let pure_funs = Pure_fun.f p in
  let effs = effectful nv defs pure_funs in
  (* Print out effectfulness info *)
  Format.eprintf "Effectful:\n";
  Array.iteri (fun i b -> Format.eprintf "%d: %b\n" i b) effs;
  let vars = variables deps in
  let live = solver vars deps effs in
  (* Print out liveness info *)
  Format.eprintf "Liveness:\n";
  Var.Tbl.iter (fun v b -> Format.eprintf "%a: %b\n" Var.print v b) live;
  { p with blocks }, Array.make nv 0

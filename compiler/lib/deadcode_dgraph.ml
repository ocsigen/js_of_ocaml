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
let definitions nv prog =
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
    prog.blocks;
  defs

(* Returns the adjacency list for the variable dependency graph. *)
let dependencies nv defs =
  let deps = Array.make nv Var.Set.empty in
  let add_dep i x = deps.(Var.idx x) <- Var.Set.add (Var.of_idx i) deps.(Var.idx x) in
  Array.iteri (* For each set of definitions *)
    (fun i ds ->
      List.iter (* For each definition *)
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

(* Return the set of variables used in a given expression *)
let expr_vars (e : expr) =
  let vars = Var.ISet.empty () in
  (match e with
  | Apply { f; args; _ } ->
      Var.ISet.add vars f;
      List.iter (Var.ISet.add vars) args
  | Block (_, params, _) -> Array.iter (Var.ISet.add vars) params
  | Field (z, _) -> Var.ISet.add vars z
  | Constant _ -> ()
  | Closure (params, _) -> List.iter (Var.ISet.add vars) params
  | Prim (_, args) ->
      List.iter
        (fun v ->
          match v with
          | Pv v -> Var.ISet.add vars v
          | Pc _ -> ())
        args);
  vars

(* Returns a boolean array representing whether each variable appears in an effectful instruction. *)
let effectful nv prog pure_funs =
  let effs = Array.make nv false in
  let effectful_instruction i =
    match i with
    | Let (_, e) ->
        if not (pure_expr pure_funs e)
        then
          let vars = expr_vars e in
          Var.ISet.iter (fun v -> effs.(Var.idx v) <- true) vars
    | Assign (x, _) -> effs.(Var.idx x) <- true (* TODO: correct? *)
    | _ -> ()
  in
  let rec effectful_block block =
    List.iter (fun (i, _) -> effectful_instruction i) block.body;
    match fst block.branch with
    | Stop -> ()
    | Return x | Raise (x, _) -> effs.(Var.idx x) <- true
    | Branch cont | Poptrap cont ->
        effectful_continuation prog cont;
        effectful_continuation prog cont
    | Cond (_, cont1, cont2) | Pushtrap (cont1, _, cont2, _) ->
        effectful_continuation prog cont1;
        effectful_continuation prog cont2
    | Switch (_, a1, a2) ->
        Array.iter (fun cont -> effectful_continuation prog cont) a1;
        Array.iter (fun cont -> effectful_continuation prog cont) a2
  and effectful_continuation prog ((pc, _) : cont) =
    let block = Addr.Map.find pc prog.blocks in
    effectful_block block
  in
  Addr.Map.iter (fun _ block -> effectful_block block) prog.blocks;
  effs

(* Returns the set of variables given the adjacency list of variable dependencies. *)
let variables deps =
  let vars = Var.ISet.empty () in
  Array.iteri (fun i _ -> Var.ISet.add vars (Var.of_idx i)) deps;
  vars

(* A variable x is live if either
   (1) it appears in an effectful expression; or
   (2) there exists a live variable y that depends on x. *)
let propagate deps effectful live x =
  let idx = Var.idx x in
  effectful.(idx) || Var.Set.exists (fun y -> Var.Tbl.get live y) deps.(idx)

let solver vars deps effectful =
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  Solver.f () (G.invert () g) (propagate deps effectful)

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
  let effs = effectful nv p pure_funs in
  (* Print out effectfulness info *)
  Format.eprintf "Effectful:\n";
  Array.iteri (fun i b -> Format.eprintf "%d: %b\n" i b) effs;
  let vars = variables deps in
  let live = solver vars deps effs in
  (* Print out liveness info *)
  Format.eprintf "Liveness:\n";
  Var.Tbl.iter (fun v b -> Format.eprintf "%a: %b\n" Var.print v b) live;
  { p with blocks }, Array.make nv 0

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
open Stdlib

type def =
  | Expr of expr
  | Var of Var.t

type live =
  | Live of IntSet.t
  | Top
  | Dead

let live_to_string = function
  | Live fields ->
      "live { " ^ IntSet.fold (fun i s -> s ^ Format.sprintf "%d " i) fields "" ^ "}"
  | Top -> "top"
  | Dead -> "dead"

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain = struct
  type t = live

  let equal = Poly.( = )

  let bot = Dead
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
        ~f:(fun (i, _) ->
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
    ~f:(fun i ds ->
      List.iter (* For each definition *)
        ~f:(fun d ->
          match d with
          (* Add dependencies from definition *)
          | Expr e -> (
              match e with
              | Apply { f; args; _ } ->
                  add_dep i f;
                  List.iter ~f:(add_dep i) args
              | Block (_, params, _) -> Array.iter ~f:(add_dep i) params
              | Field (z, _) -> add_dep i z
              | Constant _ -> ()
              | Closure (params, _) -> List.iter ~f:(add_dep i) params
              | Prim (_, args) ->
                  List.iter
                    ~f:(fun arg ->
                      match arg with
                      | Pv v -> add_dep i v
                      | Pc _ -> ())
                    args)
          | Var y -> add_dep i y)
        ds)
    defs;
  deps

(* Return the set of variables used in a given expression *)
let expr_vars e =
  let vars = Var.ISet.empty () in
  (match e with
  | Apply { f; args; _ } ->
      Var.ISet.add vars f;
      List.iter ~f:(Var.ISet.add vars) args
  | Block (_, params, _) -> Array.iter ~f:(Var.ISet.add vars) params
  | Field (z, _) -> Var.ISet.add vars z
  | Constant _ -> ()
  | Closure (params, _) -> List.iter ~f:(Var.ISet.add vars) params
  | Prim (_, args) ->
      List.iter
        ~f:(fun v ->
          match v with
          | Pv v -> Var.ISet.add vars v
          | Pc _ -> ())
        args);
  vars

(* Returns a boolean array representing whether each variable either
   (1) appears in an effectful instruction; or
   (2) is returned or raised by a function. *)
let liveness nv prog pure_funs =
  let live_vars = Array.make nv Dead in
  let add_top v =
    let idx = Var.idx v in
    live_vars.(idx) <- Top
  in
  let add_live v i =
    let idx = Var.idx v in
    match live_vars.(idx) with
    | Live fields -> live_vars.(idx) <- Live (IntSet.add i fields)
    | _ -> live_vars.(idx) <- Live (IntSet.singleton i)
  in
  let live_instruction i =
    match i with
    | Let (_, e) ->
        if not (pure_expr pure_funs e)
        then
          let vars = expr_vars e in
          Var.ISet.iter add_top vars
    | Assign (_, _) -> ()
    (* TODO: what to do with these? *)
    | Set_field (x, i, y) ->
        add_live x i;
        add_top y
    | Array_set (x, y, z) ->
        add_top x;
        add_top y;
        add_top z
    | Offset_ref (x, i) -> add_live x i
  in
  let rec live_block block =
    List.iter ~f:(fun (i, _) -> live_instruction i) block.body;
    match fst block.branch with
    (* Base Cases *)
    | Stop -> ()
    | Return x | Raise (x, _) -> add_top x
    (* Recursive cases *)
    | Branch cont | Poptrap cont ->
        live_continuation prog cont;
        live_continuation prog cont
    | Cond (_, cont1, cont2) | Pushtrap (cont1, _, cont2, _) ->
        live_continuation prog cont1;
        live_continuation prog cont2
    | Switch (_, a1, a2) ->
        Array.iter ~f:(fun cont -> live_continuation prog cont) a1;
        Array.iter ~f:(fun cont -> live_continuation prog cont) a2
  and live_continuation prog ((pc, _) : cont) =
    let block = Addr.Map.find pc prog.blocks in
    live_block block
  in
  Addr.Map.iter (fun _ block -> live_block block) prog.blocks;
  live_vars

(* Returns the set of variables given the adjacency list of variable dependencies. *)
let variables deps =
  let vars = Var.ISet.empty () in
  Array.iteri ~f:(fun i _ -> Var.ISet.add vars (Var.of_idx i)) deps;
  vars

(* A variable x is live if either
    (1) it appears in an effectful expression;
    (2) it is returned or raised by a function; or
    (3) there exists a live variable y that depends on x.
   The first two conditions are determined by a traversal of the program and given by `live_vars`.
   The third is determined here by propagating liveness to a variable's dependencies. *)

(* Look at each dependency y of x.
   - If x is Live fields, then x becomes Live (union_deps fields)
   - If x is Top, then if union_deps is not empty (some y is Live fields) then x is Live (union_deps empty)
   - If x is Dead, then if union_deps is empty and all dependencies are dead, x is dead. If a dep
    is Top then x is Top. *)
let propagate deps defs live_vars live_table x =
  let idx = Var.idx x in
  let union_deps fields =
    Var.Set.fold
      (fun y acc ->
        match Var.Tbl.get live_table y with
        | Live y_fields -> IntSet.union acc y_fields
        | Top | Dead ->
            List.fold_left
              ~f:(fun acc def ->
                match def with
                | Expr (Field (_, i)) -> IntSet.add i acc
                | _ -> acc)
              ~init:acc
              defs.(Var.idx y))
      deps.(idx)
      fields
  in
  let is_top x =
    match Var.Tbl.get live_table x with
    | Top -> true
    | _ -> false
  in
  match live_vars.(idx) with
  | Live fields -> Live (union_deps fields)
  | Top ->
      let fields = union_deps IntSet.empty in
      if IntSet.is_empty fields then Top else Live fields
  | Dead ->
      let fields = union_deps IntSet.empty in
      if IntSet.is_empty fields
      then if Var.Set.exists is_top deps.(idx) then Top else Dead
      else Live fields

let solver vars deps defs live_vars =
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  Solver.f () (G.invert () g) (propagate deps defs live_vars)

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

let run p =
  let nv = Var.count () in
  let blocks = p.blocks in
  let defs = definitions nv p in
  let deps = dependencies nv defs in
  (* Print out dependency info *)
  Format.eprintf "Dependencies:\n";
  Array.iteri
    ~f:(fun i ds ->
      Format.eprintf "%d: { " i;
      Var.Set.iter (fun d -> Format.eprintf "%a " Var.print d) ds;
      Format.eprintf "}\n")
    deps;
  let pure_funs = Pure_fun.f p in
  let live_vars = liveness nv p pure_funs in
  (* Print out liveness info *)
  Format.eprintf "Liveness:\n";
  Array.iteri ~f:(fun i l -> Format.eprintf "%d: %s\n" i (live_to_string l)) live_vars;
  let vars = variables deps in
  let live_table = solver vars deps defs live_vars in
  (* After dependency propagation *)
  Format.eprintf "Liveness with dependencies:\n";
  Var.Tbl.iter
    (fun v l -> Format.eprintf "%a: %s\n" Var.print v (live_to_string l))
    live_table;
  { p with blocks }, Array.make nv 0

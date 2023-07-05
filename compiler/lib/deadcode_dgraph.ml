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
  | Top
  | Live of IntSet.t
  | Dead

let live_to_string = function
  | Live fields ->
      "live { " ^ IntSet.fold (fun i s -> s ^ Format.sprintf "%d " i) fields "" ^ "}"
  | Top -> "top"
  | Dead -> "dead"

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)

module Domain = struct
  type t = live

  let equal l1 l2 =
    match l1, l2 with
    | Top, Top | Dead, Dead -> true
    | Live l1, Live l2 -> IntSet.equal l1 l2
    | _ -> false

  let bot = Dead

  let join l1 l2 =
    match l1, l2 with
    | _, Top | Top, _ -> Top
    | Live f1, Live f2 -> Live (IntSet.union f1 f2)
    | Dead, Live f | Live f, Dead -> Live f
    | Dead, Dead -> Dead
end

module Solver = G.Solver (Domain)

let pure_expr pure_funs e = Pure_fun.pure_expr pure_funs e && Config.Flag.deadcode ()

(* Returns an array of definitions of each variable *)
(* TODO: Change this to return def array instead of def list array *)
let definitions nv prog =
  let defs = Array.make nv [] in
  let add_def x d = defs.(Var.idx x) <- d :: defs.(Var.idx x) in
  let add_arg_def params args =
    try List.iter2 ~f:(fun x y -> add_def x (Var y)) params args
    with Invalid_argument _ -> ()
  in
  let add_cont_defs (pc, args) =
    match try Some (Addr.Map.find pc prog.blocks) with Not_found -> None with
    | Some block -> add_arg_def block.params args
    | None -> () (* Dead continuation *)
  in
  Addr.Map.iter
    (fun _ block ->
      (* Add definitions from block body *)
      List.iter
        ~f:(fun (i, _) ->
          match i with
          | Let (x, e) -> add_def x (Expr e)
          | Assign (x, y) -> add_def x (Var y)
          | _ -> ())
        block.body;
      (* Add definitions for block parameters *)
      match fst block.branch with
      | Return _ | Raise _ | Stop -> ()
      | Branch cont -> add_cont_defs cont
      | Cond (_, cont1, cont2) ->
          add_cont_defs cont1;
          add_cont_defs cont2
      | Switch (_, a1, a2) ->
          Array.iter ~f:add_cont_defs a1;
          Array.iter ~f:add_cont_defs a2
      | Pushtrap (cont, _, cont_h, _) ->
          add_cont_defs cont;
          add_cont_defs cont_h
      | Poptrap cont -> add_cont_defs cont)
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

let propagate deps defs live_vars live_table x =
  let idx = Var.idx x in
  let contribution y =
    List.fold_left
      ~f:(fun acc def ->
        match def with
        | Expr (Field (_, i)) -> Domain.join acc (Live (IntSet.singleton i))
        | Var _ -> Domain.join acc (Var.Tbl.get live_table y)
        | _ -> Top)
      ~init:Dead
      defs.(Var.idx y)
  in
  Var.Set.fold
    (fun y live -> Domain.join (contribution y) live)
    deps.(idx)
    live_vars.(idx)

let solver vars deps defs live_vars =
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  Solver.f () (G.invert () g) (propagate deps defs live_vars)

let run p =
  let nv = Var.count () in
  let blocks = p.blocks in
  let defs = definitions nv p in
  (* Print out definitions *)
  Format.eprintf "Definitions:\n";
  Array.iteri
    ~f:(fun i defs ->
      Format.eprintf "v%d: { " i;
      List.iter
        ~f:(function
          | Expr e -> Format.eprintf "%a " Print.expr e
          | Var y -> Format.eprintf "%a " Var.print y)
        defs;
      Format.eprintf "}\n")
    defs;
  let deps = dependencies nv defs in
  (* Print out dependency info *)
  Format.eprintf "Dependencies:\n";
  Array.iteri
    ~f:(fun i ds ->
      Format.eprintf "v%d: { " i;
      Var.Set.iter (fun d -> Format.eprintf "%a " Var.print d) ds;
      Format.eprintf "}\n")
    deps;
  let pure_funs = Pure_fun.f p in
  let live_vars = liveness nv p pure_funs in
  (* Print out liveness info *)
  Format.eprintf "Liveness:\n";
  Array.iteri ~f:(fun i l -> Format.eprintf "v%d: %s\n" i (live_to_string l)) live_vars;
  let vars = variables deps in
  let live_table = solver vars deps defs live_vars in
  (* After dependency propagation *)
  Format.eprintf "Liveness with dependencies:\n";
  Var.Tbl.iter
    (fun v l -> Format.eprintf "%a: %s\n" Var.print v (live_to_string l))
    live_table;
  { p with blocks }, Array.make nv 0

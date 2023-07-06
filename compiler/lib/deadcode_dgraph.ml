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
  | Param

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

let definitions nv prog =
  let defs = Array.make nv Param in
  let set_def x d = defs.(Var.idx x) <- d in
  Addr.Map.iter
    (fun _ block ->
      (* Add defs from block body *)
      List.iter
        ~f:(fun (i, _) ->
          match i with
          | Let (x, e) -> set_def x (Expr e)
          | Assign (x, _) -> set_def x Param
          | _ -> ())
        block.body)
    prog.blocks;
  defs

let usages nv prog =
  let uses = Array.make nv Var.Set.empty in
  let add_use x y = uses.(Var.idx y) <- Var.Set.add x uses.(Var.idx y) in
  let add_arg_dep params args =
    try List.iter2 ~f:(fun x y -> add_use x y) params args with Invalid_argument _ -> ()
  in
  let add_cont_deps (pc, args) =
    match try Some (Addr.Map.find pc prog.blocks) with Not_found -> None with
    | Some block -> add_arg_dep block.params args
    | None -> () (* Dead continuation *)
  in
  let add_expr_uses x e =
    match e with
    | Apply { f; args; _ } ->
        add_use x f;
        List.iter ~f:(add_use x) args
    | Block (_, params, _) -> Array.iter ~f:(add_use x) params
    | Field (z, _) -> add_use x z
    | Constant _ -> ()
    | Closure (_, cont) -> add_cont_deps cont
    | Prim (_, args) ->
        List.iter
          ~f:(fun arg ->
            match arg with
            | Pv v -> add_use x v
            | Pc _ -> ())
          args
  in
  Addr.Map.iter
    (fun _ block ->
      (* Add deps from block body *)
      List.iter
        ~f:(fun (i, _) ->
          match i with
          | Let (x, e) -> add_expr_uses x e
          | Assign (x, y) -> add_use x y
          (* TODO: These? *)
          | Set_field (_, _, _) | Offset_ref (_, _) | Array_set (_, _, _) -> ())
        block.body;
      (* Add deps from block branch *)
      match fst block.branch with
      | Return _ | Raise _ | Stop -> ()
      | Branch cont -> add_cont_deps cont
      | Cond (_, cont1, cont2) ->
          add_cont_deps cont1;
          add_cont_deps cont2
      | Switch (_, a1, a2) ->
          Array.iter ~f:add_cont_deps a1;
          Array.iter ~f:add_cont_deps a2
      | Pushtrap (cont, _, cont_h, _) ->
          add_cont_deps cont;
          add_cont_deps cont_h
      | Poptrap cont -> add_cont_deps cont)
    prog.blocks;
  uses

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
    | Let (x, e) ->
        if not (pure_expr pure_funs e)
        then (
          let vars = expr_vars e in
          Var.ISet.iter add_top vars;
          add_top x)
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

let propagate uses defs live_vars live_table x =
  let idx = Var.idx x in
  let contribution y =
    match Var.Tbl.get live_table y with
    | Dead -> Dead (* If x is used in y, and y is dead, then x is dead. *)
    | _ -> (
        (* Otherwise we may be able to refine y's liveness. *)
        match defs.(Var.idx y) with
        | Expr (Field (_, i)) -> Live (IntSet.singleton i)
        | Param -> Var.Tbl.get live_table y
        | _ -> Top)
  in
  Var.Set.fold
    (fun y live -> Domain.join (contribution y) live)
    uses.(idx)
    live_vars.(idx)

let solver vars uses defs live_vars =
  let g =
    { G.domain = vars; G.iter_children = (fun f x -> Var.Set.iter f uses.(Var.idx x)) }
  in
  Solver.f () (G.invert () g) (propagate uses defs live_vars)

let run p =
  let nv = Var.count () in
  let blocks = p.blocks in
  let defs = definitions nv p in
  (* Print out definitions *)
  Format.eprintf "Definitions:\n";
  Array.iteri
    ~f:(fun i def ->
      Format.eprintf "v%d: " i;
      (match def with
      | Expr e -> Format.eprintf "%a " Print.expr e
      | Param -> Format.eprintf "param");
      Format.eprintf "\n")
    defs;
  (* Print out usage info *)
  let uses = usages nv p in
  Format.eprintf "Usages:\n";
  Array.iteri
    ~f:(fun i ds ->
      Format.eprintf "v%d: { " i;
      Var.Set.iter (fun d -> Format.eprintf "%a " Var.print d) ds;
      Format.eprintf "}\n")
    uses;
  let pure_funs = Pure_fun.f p in
  let live_vars = liveness nv p pure_funs in
  (* Print out liveness info *)
  Format.eprintf "Liveness:\n";
  Array.iteri ~f:(fun i l -> Format.eprintf "v%d: %s\n" i (live_to_string l)) live_vars;
  let vars = variables uses in
  let live_table = solver vars uses defs live_vars in
  (* After dependency propagation *)
  Format.eprintf "Liveness with dependencies:\n";
  Var.Tbl.iter
    (fun v l -> Format.eprintf "%a: %s\n" Var.print v (live_to_string l))
    live_table;
  { p with blocks }, Array.make nv 0

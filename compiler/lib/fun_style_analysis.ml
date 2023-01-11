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

(*
Closure must be in CPS if it contains any calling point in CPS

Closure might be in CPS if
  it is called from CPS calling point => two-way dep
  or it is used in unknown context  ==> possibly mutable

Closure in CPS if contains any calling point in CPS
                      => one-way dep
   or if called from CPS calling point => two-way dep
   or used in unknown context  ==> possibly mutable

Calling point must be in CPS if function unknown
                        ==> known_origins = false
    or function in CPS
                       ==> dep

When a function escape but is also used directly, maybe we should wrap
it so that we do not have to convert it to CPS:
- Three levels: CPS, undecided, direct
- For each undecided call site, duplicate the corresponding functions
*)
open! Stdlib

let times = Debug.find "times"

open Code

module Domain = struct
  type t = bool

  let equal = Bool.equal

  let bot = false
end

let add_var s x = s := Var.Set.add x !s

(* x depends on y *)
let add_dep deps x y =
  if not (Var.Map.mem x !deps) then deps := Var.Map.add x Var.Set.empty !deps;
  deps :=
    (*Var.Set.add x (try Var.Map.find y !deps with Not_found -> Var.Set.empty))
          !deps*)
    Var.Map.update
      y
      (fun s -> Some (Var.Set.add x (Option.value ~default:Var.Set.empty s)))
      !deps

let rec list_iter ~f = function
  | [] -> ()
  | [ a ] -> f true a
  | a :: l ->
      f false a;
      list_iter ~f l

let block_deps ~info ~vars ~tail_deps ~deps ~blocks ~fun_name pc =
  let block = Addr.Map.find pc blocks in
  list_iter block.body ~f:(fun is_last i ->
      match i with
      | Let (x, Apply { f; _ }) -> (
          let tail_call =
            is_last
            &&
            match block.branch with
            | Return x' -> Var.equal x x'
            | _ -> false
          in
          add_var vars x;
          (match fun_name with
          | None -> ()
          | Some fun_name ->
              add_var vars fun_name;
              add_dep deps fun_name x);
          match Var.Tbl.get info.Global_flow.info_approximation f with
          | Top -> ()
          | Values { known; others } ->
              Var.Set.iter
                (fun g ->
                  add_var vars g;
                  (if tail_call && not others
                  then
                    match fun_name with
                    | None -> ()
                    | Some fun_name -> add_dep tail_deps fun_name g);
                  add_dep deps x g;
                  add_dep deps g x)
                known)
      | Let (x, Prim (Extern ("%perform" | "%reperform" | "%resume"), _)) -> (
          add_var vars x;
          match fun_name with
          | None -> ()
          | Some fun_name ->
              add_var vars fun_name;
              add_dep deps fun_name x)
      | Let (x, (Closure _ | Prim (Extern "%closure", _))) -> add_var vars x
      | _ -> ())

module G' = Dgraph.Make (Var) (Var.Set) (Var.Map)
module Solver = G'.Solver (Domain)

let fold_children g f x acc = g.G'.fold_children (fun y acc -> f y acc) x acc

let cps_needed ~info ~in_loop ~rev_deps st x =
  Var.Set.mem x in_loop
  ||
  let idx = Var.idx x in
  fold_children rev_deps (fun y acc -> acc || Var.Map.find y st) x false
  ||
  match info.Global_flow.info_defs.(idx) with
  | Expr (Apply { f; _ }) -> (
      match Var.Tbl.get info.Global_flow.info_approximation f with
      | Top | Values { others = true; _ } -> true
      | Values { known; others = false } ->
          Var.Set.fold
            (fun g acc ->
              acc
              ||
              match info.Global_flow.info_defs.(Var.idx g) with
              | Expr (Closure _ | Prim (Extern "%closure", _)) -> false
              | _ -> true)
            known
            false)
  | Expr (Closure _) | Expr (Prim (Extern "%closure", _)) ->
      info.Global_flow.info_may_escape.(idx)
  | Expr (Prim (Extern ("%perform" | "%reperform" | "%resume"), _)) -> true
  | _ -> false

module SCC = Strongly_connected_components.Make (struct
  type t = Var.t

  module Set = Var.Set
  module Map = Var.Map
end)

let annot st xi =
  match (xi : Print.xinstr) with
  | Instr (Let (x, _)) when Var.Set.mem x st -> "*"
  | _ -> " "

let f p info =
  let t = Timer.make () in
  let t1 = Timer.make () in
  let vars = ref Var.Set.empty in
  let deps = ref Var.Map.empty in
  let tail_deps = ref Var.Map.empty in
  fold_closures
    p
    (fun fun_name _ (pc, _) _ ->
      traverse
        { fold = Code.fold_children }
        (fun pc () ->
          block_deps ~info ~vars ~tail_deps ~deps ~blocks:p.blocks ~fun_name pc)
        pc
        p.blocks
        ())
    ();
  let scc = SCC.component_graph !tail_deps in
  let in_loop =
    Array.fold_left
      ~f:(fun s (c, _) ->
        match c with
        | SCC.No_loop _ -> s
        | Has_loop l ->
            (*
            Format.eprintf "LOOP ";
            List.iter ~f:(fun x -> Format.eprintf " %a" Var.print x) l;
            Format.eprintf "@.";
            *)
            List.fold_left ~f:(fun s x -> Var.Set.add x s) l ~init:s)
      ~init:Var.Set.empty
      scc
  in
  let g =
    { G'.domain = !vars
    ; fold_children =
        (fun f x r ->
          Var.Set.fold f (try Var.Map.find x !deps with Not_found -> Var.Set.empty) r)
    }
  in
  let rev_deps = G'.invert g in
  if times () then Format.eprintf "    fun analysis (initialize): %a@." Timer.print t1;
  let t2 = Timer.make () in
  let res = Solver.f g (cps_needed ~info ~in_loop ~rev_deps) in
  if times () then Format.eprintf "    fun analysis (solve): %a@." Timer.print t2;
  if times () then Format.eprintf "  fun analysis: %a@." Timer.print t;
  Code.Print.program
    (fun _ xi ->
      match (xi : Print.xinstr) with
      | Instr (Let (x, _)) ->
          if try Var.Map.find x res with Not_found -> false then "*" else " "
      | _ -> " ")
    p;
  (* *)
  Var.Map.fold (fun x v s -> if v then Var.Set.add x s else s) res Var.Set.empty

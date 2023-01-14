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

(* We compute which functions and which call points needs to be in CPS. *)

open! Stdlib

let times = Debug.find "times"

open Code

let add_var = Var.ISet.add

(* x depends on y *)
let add_dep deps x y =
  let idx = Var.idx y in
  deps.(idx) <- Var.Set.add x deps.(idx)

let add_tail_dep deps x y =
  if not (Var.Map.mem x !deps) then deps := Var.Map.add x Var.Set.empty !deps;
  deps :=
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
          add_var vars x;
          (match fun_name with
          | None -> ()
          | Some g ->
              add_var vars g;
              (* If a call point is in CPS, then the englobing
                 function should be in CPS *)
              add_dep deps g x);
          match Var.Tbl.get info.Global_flow.info_approximation f with
          | Top -> ()
          | Values { known; others } ->
              let known_tail_call =
                (not others)
                && is_last
                &&
                match block.branch with
                | Return x' -> Var.equal x x'
                | _ -> false
              in
              IntMap.iter
                (fun _ v ->
                  Var.Set.iter
                    (fun g ->
                      add_var vars g;
                      (if known_tail_call
                      then
                        match fun_name with
                        | None -> ()
                        | Some f -> add_tail_dep tail_deps f g);
                      (* If a called function is in CPS, then the call
                         point is in CPS *)
                      add_dep deps x g
                      (*
                  (* Conversally, if a call point is in CPS then all
                     called functions must be in CPS *)
                  add_dep deps g x
*))
                    v)
                known)
      | Let (x, Prim (Extern ("%perform" | "%reperform" | "%resume"), _)) -> (
          add_var vars x;
          match fun_name with
          | None -> ()
          | Some f ->
              add_var vars f;
              (* If a function contains effect primitives, it must be
                 in CPS *)
              add_dep deps f x)
      | Let (x, Closure _) -> add_var vars x
      | _ -> ())

let program_deps ~info ~vars ~tail_deps ~deps p =
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
    ()

module Domain = struct
  type t = bool

  let equal = Bool.equal

  let bot = false
end

module G' = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)
module Solver = G'.Solver (Domain)

let fold_children g f x acc =
  let acc = ref acc in
  g.G'.iter_children (fun y -> acc := f y !acc) x;
  !acc

let cps_needed ~info ~in_loop ~rev_deps st x =
  (*
  rev_deps.G'.iter_children
    (fun y ->
      if Var.Tbl.get st y then Format.eprintf "ZZZ %a ==> %a@." Var.print y Var.print x)
    x;
*)
  (* Mutually recursive functions are turned into CPS for tail
     optimization *)
  Var.Set.mem x in_loop
  ||
  let idx = Var.idx x in
  fold_children rev_deps (fun y acc -> acc || Var.Tbl.get st y) x false
  ||
  match info.Global_flow.info_defs.(idx) with
  | Expr (Apply { f; args; _ }) -> (
      (* If we don't know all possible functions at a call point, it
         must be in CPS *)
      match Var.Tbl.get info.Global_flow.info_approximation f with
      | Top -> true
      | Values { others; known } ->
          if (not others) && not (Global_flow.exact_call info f (List.length args))
          then (
            Format.eprintf "AAA %a (%d):" Var.print x (List.length args);
            IntMap.iter
              (fun i v ->
                Var.Set.iter
                  (fun g ->
                    match info.info_defs.(Var.idx g) with
                    | Expr (Closure (params, _)) ->
                        Format.eprintf " %a@%d(%d)" Var.print g i (List.length params)
                    | Expr (Block _) -> ()
                    | Expr _ | Phi _ -> assert false)
                  v)
              known;
            Format.eprintf "@.");
          (* ZZZ under applied? *)
          others
          (*|| not (Global_flow.exact_call info f (List.length args))*))
  | Expr (Closure _) ->
      (* If a function escapes, it must be in CPS *)
      false
      (*      info.Global_flow.info_may_escape.(idx)*)
  | Expr (Prim (Extern ("%perform" | "%reperform" | "%resume"), _)) ->
      (* Effects primitives are in CPS *)
      true
  | _ -> false

module SCC = Strongly_connected_components.Make (struct
  type t = Var.t

  module Set = Var.Set
  module Map = Var.Map
end)

let find_loops tail_deps =
  let scc = SCC.component_graph !tail_deps in
  Array.fold_left
    ~f:(fun s (c, _) ->
      match c with
      | SCC.No_loop _ -> s
      | Has_loop l -> List.fold_left ~f:(fun s x -> Var.Set.add x s) l ~init:s)
    ~init:Var.Set.empty
    scc

let annot st xi =
  match (xi : Print.xinstr) with
  | Instr (Let (x, _)) when Var.Set.mem x st -> "*"
  | _ -> " "

let f p info =
  let t = Timer.make () in
  let t1 = Timer.make () in
  let nv = Var.count () in
  let vars = Var.ISet.empty () in
  let deps = Array.make nv Var.Set.empty in
  let tail_deps = ref Var.Map.empty in
  program_deps ~info ~vars ~tail_deps ~deps p;
  if times () then Format.eprintf "      fun analysis (initialize): %a@." Timer.print t1;
  let t2 = Timer.make () in
  let in_loop = find_loops tail_deps in
  if times () then Format.eprintf "      fun analysis (tail calls): %a@." Timer.print t2;
  let t3 = Timer.make () in
  let g =
    { G'.domain = vars; iter_children = (fun f x -> Var.Set.iter f deps.(Var.idx x)) }
  in
  let rev_deps = G'.invert () g in
  let res = Solver.f () g (cps_needed ~info ~in_loop ~rev_deps) in
  if times () then Format.eprintf "      fun analysis (solve): %a@." Timer.print t3;
  let s = ref Var.Set.empty in
  Var.Tbl.iter (fun x v -> if v then s := Var.Set.add x !s) res;
  if times () then Format.eprintf "    fun analysis: %a@." Timer.print t;
  !s

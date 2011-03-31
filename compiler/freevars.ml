(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

let (>>) x f = f x
let list_fold f b l s = List.fold_left (fun s x -> f b x s) s l
let array_fold f b a s = Array.fold_right (fun x s ->  f b x s) a s

let add_var b x s = if VarSet.mem x b then s else VarSet.add x s

let closure_free_vars = ref (fun pc params (pc', _) -> assert false)

let cont_free_vars b (_, l) s = list_fold add_var b l s

let expr_free_vars pc b x e s =
  match e with
    Const _ | Constant _ ->
      s
  | Apply (x, l, _) ->
      s >> add_var b x >> list_fold add_var b l
  | Block (_, a) ->
      Array.fold_right (fun x s -> add_var b x s) a s
  | Field (x, _) ->
      add_var b x s
  | Closure (params, cont) ->
      let s' = !closure_free_vars pc params cont in
(*
Format.eprintf "@[<2>Free vars of function %a:@ %a@]@."
Var.print x
print_var_list (VarSet.elements s');
*)
      VarSet.union s (VarSet.diff s' b)
  | Prim (_, l) ->
      list_fold
        (fun b x s -> match x with Pv x -> add_var b x s | Pc _ -> s) b l s

let instr_free_vars pc b i s =
  match i with
    Let (x, e) ->
      expr_free_vars pc b x e s
  | Set_field (x, _, y) ->
      s >> add_var b x >> add_var b y
  | Offset_ref (x, _) ->
      add_var b x s
  | Array_set (x, y, z) ->
      s >> add_var b x >> add_var b y >> add_var b z

let last_free_var b l s =
  match l with
    Return x
  | Raise x ->
      add_var b x s
  | Stop ->
      s
  | Branch cont | Poptrap cont ->
      cont_free_vars b cont s
  | Cond (_, x, cont1, cont2) ->
      s >> add_var b x >> cont_free_vars b cont1 >> cont_free_vars b cont2
  | Switch (x, a1, a2) ->
      s
      >> add_var b x
      >> array_fold cont_free_vars b a1
      >> array_fold cont_free_vars b a2
  | Pushtrap (cont1, _, cont2, _) ->
      s
      >> cont_free_vars b cont1
      >> cont_free_vars b cont2

let block_free_vars pc b block s =
  s
  >> list_fold (instr_free_vars pc) b block.body
  >> last_free_var b block.branch

let instr_bound_vars b i s =
  match i with
    Let (x, _) ->
      add_var b x s
  | Set_field _ | Offset_ref _ | Array_set _ ->
      s

let last_bound_vars b l s =
  match l with
    Return _ | Raise _ | Stop | Branch _
  | Cond _ | Switch _ | Poptrap _ ->
      s
  | Pushtrap (_, x, _, _) ->
      add_var b x s

let block_bound_vars block s =
  let b = VarSet.empty in
  s
  >> list_fold add_var b block.params
  >> list_fold instr_bound_vars b block.body
  >> last_bound_vars b block.branch

(****)

module G = Dgraph.Make (struct type t = int end) (AddrSet) (AddrMap)

let forward_graph (_, blocks, _) =
  let d =
    AddrMap.fold (fun x _ s -> AddrSet.add x s)
      blocks AddrSet.empty in
  { G.domain = d;
    G.fold_children = fun f x a -> fold_children blocks x f a }

module D = struct
  type t = VarSet.t * VarSet.t
  let equal (_, x) (_, y) = VarSet.equal x y
  let bot = (VarSet.empty, VarSet.empty)
end

let propagate g bound_vars s x =
  let a =
    g.G.fold_children
      (fun y a -> VarSet.union (snd (AddrMap.find y s)) a) x VarSet.empty
  in
  (a, VarSet.union (AddrMap.find x bound_vars) a)

module Solver = G.Solver (D)

let solver ((_, blocks, _) as p) =
  let g = forward_graph p in
  let bound_vars =
    AddrMap.map (fun b -> block_bound_vars b VarSet.empty) blocks in
  AddrMap.map fst (Solver.f (G.invert g) (propagate g bound_vars))

(****)

let rec traverse pc visited blocks bound_vars free_vars =
  if not (AddrSet.mem pc visited) then begin
    let visited = AddrSet.add pc visited in
    let block = AddrMap.find pc blocks in
    let bound_vars = block_bound_vars block bound_vars in
    let free_vars = block_free_vars pc bound_vars block free_vars in
    let (visited, free_vars) =
      fold_children blocks pc
        (fun pc (visited, free_vars) ->
           let (visited, free_vars) =
             traverse pc visited blocks bound_vars free_vars in
           (visited, free_vars))
        (visited, free_vars)
    in
    (visited, free_vars)
  end else
    (visited, free_vars)

let f ((pc, blocks, free_pc) as p) =
  let ctx = ref AddrMap.empty in
  let cont_bound_vars = solver p in
  closure_free_vars :=
    (fun pc params (pc', _) ->
       let bound_vars = list_fold add_var VarSet.empty params VarSet.empty in
       let (_, free_vars) =
         traverse pc' AddrSet.empty blocks bound_vars VarSet.empty in
       let cvars = VarSet.inter (AddrMap.find pc cont_bound_vars) free_vars in
(*
if not (VarSet.is_empty cvars) then begin
Format.eprintf "@[<2>Should protect at %d:@ %a@]@."
pc print_var_list (VarSet.elements cvars);
(*
Format.eprintf "@[<2>%a@]@."
print_var_list (VarSet.elements (AddrMap.find pc cont_bound_vars));
Format.eprintf "@[<2>%a@]@."
print_var_list (VarSet.elements free_vars);
*)
end;
*)
       ctx := AddrMap.add pc' cvars !ctx;
       free_vars);
  let free_vars = !closure_free_vars pc [] (pc, []) in
(*
Format.eprintf "@[<2>Global free variables:@ %a@]@."
print_var_list (VarSet.elements free_vars);
*)
  ignore free_vars;
  !ctx

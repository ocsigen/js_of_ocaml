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

open! Stdlib
open Code

let subst_cont m s (pc, arg) = Addr.Map.find pc m, List.map arg ~f:(fun x -> s x)

let expr s e =
  match e with
  | Constant _ -> e
  | Apply { f; args; exact } ->
      Apply { f = s f; args = List.map args ~f:(fun x -> s x); exact }
  | Block (n, a, k, mut) -> Block (n, Array.map a ~f:(fun x -> s x), k, mut)
  | Field (x, n, field_type) -> Field (s x, n, field_type)
  | Closure _ -> failwith "Inlining/Duplicating closure is currenly not supported"
  | Special x -> Special x
  | Prim (p, l) ->
      Prim
        ( p
        , List.map l ~f:(function
            | Pv x -> Pv (s x)
            | Pc _ as x -> x) )

let instr s i =
  match i with
  | Let (x, e) -> Let (s x, expr s e)
  | Assign (x, y) -> Assign (s x, s y)
  | Set_field (x, n, typ, y) -> Set_field (s x, n, typ, s y)
  | Offset_ref (x, n) -> Offset_ref (s x, n)
  | Array_set (x, y, z) -> Array_set (s x, s y, s z)
  | Event _ -> i

let instrs s l = List.map l ~f:(fun i -> instr s i)

let last m s l =
  match l with
  | Stop -> l
  | Branch cont -> Branch (subst_cont m s cont)
  | Pushtrap (cont1, x, cont2) ->
      Pushtrap (subst_cont m s cont1, s x, subst_cont m s cont2)
  | Return x -> Return (s x)
  | Raise (x, k) -> Raise (s x, k)
  | Cond (x, cont1, cont2) -> Cond (s x, subst_cont m s cont1, subst_cont m s cont2)
  | Switch (x, a1) -> Switch (s x, Array.map a1 ~f:(fun cont -> subst_cont m s cont))
  | Poptrap cont -> Poptrap (subst_cont m s cont)

let block m s block =
  { params = List.map ~f:s block.params
  ; body = instrs s block.body
  ; branch = last m s block.branch
  }

let closure p ~bound_vars ~f ~params ~cont:(pc, args) =
  let s =
    Subst.from_map
      (Var.Set.fold (fun x s -> Var.Map.add x (Var.fork x) s) bound_vars Var.Map.empty)
  in
  let free_pc, m =
    Code.traverse
      { fold = Code.fold_children }
      (fun pc (pc', m) -> pc' + 1, Addr.Map.add pc pc' m)
      pc
      p.blocks
      (p.free_pc, Addr.Map.empty)
  in
  let blocks =
    Code.traverse
      { fold = Code.fold_children }
      (fun pc blocks ->
        let b = Addr.Map.find pc blocks in
        let b = block m s b in
        Addr.Map.add (Addr.Map.find pc m) b blocks)
      pc
      p.blocks
      p.blocks
  in
  let p = { p with blocks; free_pc } in
  p, s f, List.map ~f:s params, (Addr.Map.find pc m, List.map ~f:s args)

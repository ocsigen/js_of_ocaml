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

open! Stdlib
open Code

let subst_cont s (pc, arg) = pc, List.map arg ~f:s

module Excluding_Binders = struct
  let expr s e =
    match e with
    | Constant _ -> e
    | Apply { f; args; exact } ->
        Apply { f = s f; args = List.map args ~f:(fun x -> s x); exact }
    | Block (n, a, k, mut) -> Block (n, Array.map a ~f:(fun x -> s x), k, mut)
    | Field (x, n, typ) -> Field (s x, n, typ)
    | Closure (l, pc, loc) -> Closure (l, subst_cont s pc, loc)
    | Special _ -> e
    | Prim (p, l) ->
        Prim
          ( p
          , List.map l ~f:(fun x ->
                match x with
                | Pv x -> Pv (s x)
                | Pc _ -> x) )

  let instr s i =
    match i with
    | Let (x, e) -> Let (x, expr s e)
    | Assign (x, y) -> Assign (x, s y) (* x is handled like a parameter *)
    | Set_field (x, n, typ, y) -> Set_field (s x, n, typ, s y)
    | Offset_ref (x, n) -> Offset_ref (s x, n)
    | Array_set (x, y, z) -> Array_set (s x, s y, s z)
    | Event _ -> i

  let instrs s l = List.map l ~f:(fun i -> instr s i)

  let last s l =
    match l with
    | Stop -> l
    | Branch cont -> Branch (subst_cont s cont)
    | Pushtrap (cont1, x, cont2) -> Pushtrap (subst_cont s cont1, x, subst_cont s cont2)
    | Return x -> Return (s x)
    | Raise (x, k) -> Raise (s x, k)
    | Cond (x, cont1, cont2) -> Cond (s x, subst_cont s cont1, subst_cont s cont2)
    | Switch (x, a1) -> Switch (s x, Array.map a1 ~f:(fun cont -> subst_cont s cont))
    | Poptrap cont -> Poptrap (subst_cont s cont)

  let block s block =
    { params = block.params; body = instrs s block.body; branch = last s block.branch }

  let program s p =
    let blocks = Addr.Map.map (fun b -> block s b) p.blocks in
    { p with blocks }

  let rec cont' s pc blocks visited =
    if Addr.Set.mem pc visited
    then blocks, visited
    else
      let visited = Addr.Set.add pc visited in
      let b = Addr.Map.find pc blocks in
      let b = block s b in
      let blocks = Addr.Map.add pc b blocks in
      let blocks, visited =
        List.fold_left b.body ~init:(blocks, visited) ~f:(fun (blocks, visited) instr ->
            match instr with
            | Let (_, Closure (_, (pc, _), _)) -> cont' s pc blocks visited
            | _ -> blocks, visited)
      in
      Code.fold_children
        blocks
        pc
        (fun pc (blocks, visited) -> cont' s pc blocks visited)
        (blocks, visited)

  let cont s addr p =
    let blocks, _ = cont' s addr p.blocks Addr.Set.empty in
    { p with blocks }
end

(****)

let from_array s x = if Var.idx x < Array.length s then s.(Var.idx x) else x

(****)

let rec build_mapping params args =
  match params, args with
  | x :: params, y :: args -> Var.Map.add x y (build_mapping params args)
  | [], [] -> Var.Map.empty
  | _ -> assert false

let from_map m x = try Var.Map.find x m with Not_found -> x

(****)

module Including_Binders = struct
  let expr s e =
    match e with
    | Constant _ -> e
    | Apply { f; args; exact } -> Apply { f = s f; args = List.map args ~f:s; exact }
    | Block (n, a, k, mut) -> Block (n, Array.map a ~f:s, k, mut)
    | Field (x, n, typ) -> Field (s x, n, typ)
    | Closure (l, pc, loc) -> Closure (List.map l ~f:s, subst_cont s pc, loc)
    | Special _ -> e
    | Prim (p, l) ->
        Prim
          ( p
          , List.map l ~f:(fun x ->
                match x with
                | Pv x -> Pv (s x)
                | Pc _ -> x) )

  let instr s i =
    match i with
    | Let (x, e) -> Let (s x, expr s e)
    | Assign (x, y) -> Assign (s x, s y)
    | Set_field (x, n, typ, y) -> Set_field (s x, n, typ, s y)
    | Offset_ref (x, n) -> Offset_ref (s x, n)
    | Array_set (x, y, z) -> Array_set (s x, s y, s z)
    | Event _ -> i

  let instrs s l = List.map l ~f:(fun i -> instr s i)

  let last s l =
    match l with
    | Stop -> l
    | Branch cont -> Branch (subst_cont s cont)
    | Pushtrap (cont1, x, cont2) -> Pushtrap (subst_cont s cont1, s x, subst_cont s cont2)
    | Return x -> Return (s x)
    | Raise (x, k) -> Raise (s x, k)
    | Cond (x, cont1, cont2) -> Cond (s x, subst_cont s cont1, subst_cont s cont2)
    | Switch (x, conts) -> Switch (s x, Array.map conts ~f:(fun cont -> subst_cont s cont))
    | Poptrap cont -> Poptrap (subst_cont s cont)

  let block s block =
    { params = List.map block.params ~f:s
    ; body = instrs s block.body
    ; branch = last s block.branch
    }

  module And_Continuations = struct
    let subst_cont m s (pc, arg) = Addr.Map.find pc m, List.map arg ~f:s

    let expr m s e =
      match e with
      | Constant _ -> e
      | Apply { f; args; exact } -> Apply { f = s f; args = List.map args ~f:s; exact }
      | Block (n, a, k, mut) -> Block (n, Array.map a ~f:s, k, mut)
      | Field (x, n, typ) -> Field (s x, n, typ)
      | Closure (l, pc, loc) -> Closure (List.map l ~f:s, subst_cont m s pc, loc)
      | Special _ -> e
      | Prim (p, l) ->
          Prim
            ( p
            , List.map l ~f:(fun x ->
                  match x with
                  | Pv x -> Pv (s x)
                  | Pc _ -> x) )

    let instr m s i =
      match i with
      | Let (x, e) -> Let (s x, expr m s e)
      | Assign (x, y) -> Assign (s x, s y)
      | Set_field (x, n, typ, y) -> Set_field (s x, n, typ, s y)
      | Offset_ref (x, n) -> Offset_ref (s x, n)
      | Array_set (x, y, z) -> Array_set (s x, s y, s z)
      | Event _ -> i

    let instrs m s l = List.map l ~f:(fun i -> instr m s i)

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
      ; body = instrs m s block.body
      ; branch = last m s block.branch
      }
  end
end

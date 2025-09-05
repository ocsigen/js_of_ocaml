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

let times = Debug.find "times"

open Code

(****)

let iter_cont_free_vars f (_, l) = List.iter ~f l

let iter_expr_free_vars f e =
  match e with
  | Constant _ -> ()
  | Apply { f = x; args; _ } ->
      f x;
      List.iter ~f args
  | Block (_, a, _, _) -> Array.iter ~f a
  | Field (x, _, _) -> f x
  | Closure _ -> ()
  | Special _ -> ()
  | Prim (_, l) ->
      List.iter l ~f:(fun x ->
          match x with
          | Pv x -> f x
          | Pc _ -> ())

let iter_instr_free_vars f i =
  match i with
  | Let (_, e) -> iter_expr_free_vars f e
  | Set_field (x, _, _, y) ->
      f x;
      f y
  | Offset_ref (x, _) -> f x
  | Array_set (x, y, z) ->
      f x;
      f y;
      f z
  | Assign (_, y) -> f y
  | Event _ -> ()

let iter_last_free_var f l =
  match l with
  | Return x | Raise (x, _) -> f x
  | Stop -> ()
  | Branch cont | Poptrap cont -> iter_cont_free_vars f cont
  | Cond (x, cont1, cont2) ->
      f x;
      iter_cont_free_vars f cont1;
      iter_cont_free_vars f cont2
  | Switch (x, a1) ->
      f x;
      Array.iter a1 ~f:(fun c -> iter_cont_free_vars f c)
  | Pushtrap (cont1, _, cont2) ->
      iter_cont_free_vars f cont1;
      iter_cont_free_vars f cont2

let iter_block_free_vars f block =
  List.iter block.body ~f:(fun i -> iter_instr_free_vars f i);
  iter_last_free_var f block.branch

let iter_instr_bound_vars f i =
  match i with
  | Let (x, _) -> f x
  | Event _ | Set_field _ | Offset_ref _ | Array_set _ | Assign _ -> ()

let iter_last_bound_vars f l =
  match l with
  | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ -> ()
  | Pushtrap (_, x, _) -> f x

let iter_block_bound_vars f block =
  List.iter ~f block.params;
  List.iter block.body ~f:(fun i -> iter_instr_bound_vars f i);
  iter_last_bound_vars f block.branch

(****)

type st =
  { index : int
  ; mutable lowlink : int
  ; mutable in_stack : bool
  ; mutable revisited : bool
  }

let find_loops p in_loop pc =
  let in_loop = ref in_loop in
  let index = ref 0 in
  let state = ref Addr.Map.empty in
  let stack = Stack.create () in
  let rec traverse pc =
    let st = { index = !index; lowlink = !index; in_stack = true; revisited = false } in
    state := Addr.Map.add pc st !state;
    incr index;
    Stack.push pc stack;
    Code.fold_children
      p.blocks
      pc
      (fun pc' () ->
        try
          let st' = Addr.Map.find pc' !state in
          if st'.in_stack
          then (
            st'.revisited <- true;
            st.lowlink <- min st.lowlink st'.index)
        with Not_found ->
          traverse pc';
          let st' = Addr.Map.find pc' !state in
          st.lowlink <- min st.lowlink st'.lowlink)
      ();
    if st.index = st.lowlink
    then (
      let l = ref [] in
      while
        let pc' = Stack.pop stack in
        l := pc' :: !l;
        (Addr.Map.find pc' !state).in_stack <- false;
        pc' <> pc
      do
        ()
      done;
      (* If we revisit the top element of the stack, then we have a loop.
         This work even for loops of size 1 *)
      if st.revisited
      then List.iter !l ~f:(fun pc' -> in_loop := Addr.Map.add pc' pc !in_loop))
  in
  traverse pc;
  !in_loop

let find_loops_in_closure p pc = find_loops p Addr.Map.empty pc

let find_all_loops p =
  Code.fold_closures
    p
    (fun _ _ (pc, _) (in_loop : _ Addr.Map.t) -> find_loops p in_loop pc)
    Addr.Map.empty

let mark_variables in_loop p =
  let vars = Var.Tbl.make () (-1) in
  let visited = BitSet.create' p.free_pc in
  let rec traverse pc =
    if not (BitSet.mem visited pc)
    then (
      BitSet.set visited pc;
      let block = Addr.Map.find pc p.blocks in
      (try
         let pc' = Addr.Map.find pc in_loop in
         iter_block_bound_vars (fun x -> Var.Tbl.set vars x pc') block
       with Not_found -> ());
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (_, Closure (_, (pc', _))) -> traverse pc'
          | _ -> ());
      Code.fold_children p.blocks pc (fun pc' () -> traverse pc') ())
  in
  traverse p.start;
  vars

let free_variables vars in_loop p =
  let all_freevars = ref Addr.Map.empty in
  let freevars = ref Addr.Map.empty in
  let visited = BitSet.create' p.free_pc in
  let rec traverse pc =
    if not (BitSet.mem visited pc)
    then (
      BitSet.set visited pc;
      let block = Addr.Map.find pc p.blocks in
      iter_block_free_vars
        (fun x ->
          let pc' = Var.Tbl.get vars x in
          if pc' <> -1
          then
            let fv =
              try Addr.Map.find pc' !all_freevars with Not_found -> Var.Set.empty
            in
            let s = Var.Set.add x fv in
            all_freevars := Addr.Map.add pc' s !all_freevars)
        block;
      (try
         let pc'' = Addr.Map.find pc in_loop in
         all_freevars := Addr.Map.remove pc'' !all_freevars
       with Not_found -> ());
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (_, Closure (_, (pc', _))) -> (
              traverse pc';
              try
                let pc'' = Addr.Map.find pc in_loop in
                let fv =
                  try Addr.Map.find pc'' !all_freevars with Not_found -> Var.Set.empty
                in
                freevars := Addr.Map.add pc' fv !freevars;
                all_freevars := Addr.Map.remove pc'' !all_freevars
              with Not_found -> freevars := Addr.Map.add pc' Var.Set.empty !freevars)
          | _ -> ());
      Code.fold_children p.blocks pc (fun pc' () -> traverse pc') ())
  in
  traverse p.start;
  !freevars

let f p =
  Code.invariant p;
  let t = Timer.make () in
  let bound = Code.Var.ISet.empty () in
  let visited = BitSet.create' p.free_pc in
  let free_vars =
    Code.fold_closures_innermost_first
      p
      (fun _name_opt params (pc, args) acc ->
        let free = ref Var.Set.empty in
        let using x =
          if Code.Var.ISet.mem bound x then () else free := Var.Set.add x !free
        in
        let rec traverse pc =
          if not (BitSet.mem visited pc)
          then (
            BitSet.set visited pc;
            let block = Addr.Map.find pc p.blocks in
            iter_block_bound_vars (fun x -> Code.Var.ISet.add bound x) block;
            iter_block_free_vars using block;
            List.iter block.body ~f:(function
              | Let (_, Closure (_, (pc_clo, _))) ->
                  Code.Var.Set.iter using (Code.Addr.Map.find pc_clo acc)
              | _ -> ());
            Code.fold_children p.blocks pc (fun pc' () -> traverse pc') ())
        in
        List.iter params ~f:(fun x -> Code.Var.ISet.add bound x);
        List.iter args ~f:using;
        traverse pc;
        Code.Addr.Map.add pc !free acc)
      Code.Addr.Map.empty
  in
  if times () then Format.eprintf "  free vars 2: %a@." Timer.print t;
  free_vars

let f_mutable p =
  Code.invariant p;
  let t = Timer.make () in
  let in_loop = find_all_loops p in
  let vars = mark_variables in_loop p in
  let free_vars = free_variables vars in_loop p in
  if times () then Format.eprintf "  free vars 1: %a@." Timer.print t;
  free_vars

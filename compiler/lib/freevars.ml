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
open Stdlib

let times = Debug.find "times"

open Code

(****)

let iter_cont_free_vars f (_, l) = List.iter ~f l

let iter_expr_free_vars f e =
  match e with
  | Const _ | Constant _ -> ()
  | Apply (x, l, _) -> f x; List.iter ~f l
  | Block (_, a) -> Array.iter ~f a
  | Field (x, _) -> f x
  | Closure _ -> ()
  | Prim (_, l) -> List.iter l ~f:(fun x -> match x with Pv x -> f x | Pc _ -> ())

let iter_instr_free_vars f i =
  match i with
  | Let (_, e) -> iter_expr_free_vars f e
  | Set_field (x, _, y) -> f x; f y
  | Offset_ref (x, _) -> f x
  | Array_set (x, y, z) -> f x; f y; f z

let iter_last_free_var f l =
  match l with
  | Return x | Raise (x, _) -> f x
  | Stop -> ()
  | Branch cont | Poptrap (cont, _) -> iter_cont_free_vars f cont
  | Cond (_, x, cont1, cont2) ->
      f x; iter_cont_free_vars f cont1; iter_cont_free_vars f cont2
  | Switch (x, a1, a2) ->
      f x;
      Array.iter a1 ~f:(fun c -> iter_cont_free_vars f c);
      Array.iter a2 ~f:(fun c -> iter_cont_free_vars f c)
  | Pushtrap (cont1, _, cont2, _) ->
      iter_cont_free_vars f cont1; iter_cont_free_vars f cont2

let iter_block_free_vars f block =
  List.iter block.body ~f:(fun i -> iter_instr_free_vars f i);
  iter_last_free_var f block.branch

let iter_instr_bound_vars f i =
  match i with Let (x, _) -> f x | Set_field _ | Offset_ref _ | Array_set _ -> ()

let iter_last_bound_vars f l =
  match l with
  | Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ -> ()
  | Pushtrap (_, x, _, _) -> f x

let iter_block_bound_vars f block =
  List.iter ~f block.params;
  List.iter block.body ~f:(fun i -> iter_instr_bound_vars f i);
  iter_last_bound_vars f block.branch

(****)

type st =
  { index : int
  ; mutable lowlink : int
  ; mutable in_stack : bool }

let find_loops ((_, blocks, _) as prog) =
  let in_loop = ref Addr.Map.empty in
  let index = ref 0 in
  let state = ref Addr.Map.empty in
  let stack = Stack.create () in
  let rec traverse pc =
    let st = {index = !index; lowlink = !index; in_stack = true} in
    state := Addr.Map.add pc st !state;
    incr index;
    Stack.push pc stack;
    Code.fold_children
      blocks
      pc
      (fun pc' () ->
        try
          let st' = Addr.Map.find pc' !state in
          if st'.in_stack then st.lowlink <- min st.lowlink st'.index
        with Not_found ->
          traverse pc';
          let st' = Addr.Map.find pc' !state in
          st.lowlink <- min st.lowlink st'.lowlink )
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
      if List.length !l > 1
      then List.iter !l ~f:(fun pc' -> in_loop := Addr.Map.add pc' pc !in_loop) )
  in
  Code.fold_closures prog (fun _ _ (pc, _) () -> traverse pc) ();
  !in_loop

let mark_variables in_loop (pc, blocks, free_pc) =
  let vars = Var.Tbl.make () (-1) in
  let visited = Array.make free_pc false in
  let rec traverse pc =
    if not visited.(pc)
    then (
      visited.(pc) <- true;
      let block = Addr.Map.find pc blocks in
      ( try
          let pc' = Addr.Map.find pc in_loop in
          iter_block_bound_vars
            (fun x ->
              (*
Format.eprintf "!%a: %d@." Var.print x pc';
*)
              Var.Tbl.set vars x pc' )
            block
        with Not_found -> () );
      List.iter block.body ~f:(fun i ->
          match i with Let (_, Closure (_, (pc', _))) -> traverse pc' | _ -> () );
      Code.fold_children blocks pc (fun pc' () -> traverse pc') () )
  in
  traverse pc; vars

let free_variables vars in_loop (pc, blocks, free_pc) =
  let all_freevars = ref Addr.Map.empty in
  let freevars = ref Addr.Map.empty in
  let visited = Array.make free_pc false in
  let rec traverse pc =
    if not visited.(pc)
    then (
      visited.(pc) <- true;
      let block = Addr.Map.find pc blocks in
      iter_block_free_vars
        (fun x ->
          let pc' = Var.Tbl.get vars x in
          (*
Format.eprintf "%a: %d@." Var.print x pc';
*)
          if pc' <> -1
          then
            let fv =
              try Addr.Map.find pc' !all_freevars with Not_found -> Var.Set.empty
            in
            let s = Var.Set.add x fv in
            all_freevars := Addr.Map.add pc' s !all_freevars )
        block;
      ( try
          let pc'' = Addr.Map.find pc in_loop in
          all_freevars := Addr.Map.remove pc'' !all_freevars
        with Not_found -> () );
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
              with Not_found -> freevars := Addr.Map.add pc' Var.Set.empty !freevars )
          | _ -> () );
      Code.fold_children blocks pc (fun pc' () -> traverse pc') () )
  in
  traverse pc;
  (*
Addr.Map.iter
(fun pc fv -> if Var.Set.cardinal fv > 0 then
Format.eprintf ">> %d: %d@." pc (Var.Set.cardinal fv))
!freevars;
*)
  !freevars

let f p =
  Code.invariant p;
  let t = Timer.make () in
  let in_loop = find_loops p in
  let vars = mark_variables in_loop p in
  let free_vars = free_variables vars in_loop p in
  if times () then Format.eprintf "  free vars: %a@." Timer.print t;
  free_vars

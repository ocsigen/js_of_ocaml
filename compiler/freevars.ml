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


let times = Option.Debug.find "times"

open Code

(****)

let iter_cont_free_vars f (_, l) = List.iter f l

let iter_expr_free_vars f e =
  match e with
    Const _ | Constant _ ->
      ()
  | Apply (x, l, _) ->
      f x; List.iter f l
  | Block (_, a) ->
      Array.iter f a
  | Field (x, _) ->
      f x
  | Closure _ ->
      ()
  | Prim (_, l) ->
      List.iter (fun x -> match x with Pv x -> f x | Pc _ -> ()) l

let iter_instr_free_vars f i =
  match i with
    Let (_, e)          -> iter_expr_free_vars f e
  | Set_field (x, _, y) -> f x; f y
  | Offset_ref (x, _)   -> f x
  | Array_set (x, y, z) -> f x; f y; f z

let iter_last_free_var f l =
  match l with
    Return x
  | Raise x ->
      f x
  | Stop ->
      ()
  | Branch cont | Poptrap (cont,_) ->
      iter_cont_free_vars f cont
  | Cond (_, x, cont1, cont2) ->
      f x; iter_cont_free_vars f cont1; iter_cont_free_vars f cont2
  | Switch (x, a1, a2) ->
      f x;
      Array.iter (fun c -> iter_cont_free_vars f c) a1;
      Array.iter (fun c -> iter_cont_free_vars f c) a2
  | Pushtrap (cont1, _, cont2, _) ->
      iter_cont_free_vars f cont1; iter_cont_free_vars f cont2

let iter_block_free_vars f block =
  List.iter (fun i -> iter_instr_free_vars f i) block.body;
  iter_last_free_var f block.branch

let iter_instr_bound_vars f i =
  match i with
    Let (x, _) ->
      f x
  | Set_field _ | Offset_ref _ | Array_set _ ->
      ()

let iter_last_bound_vars f l =
  match l with
    Return _ | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Poptrap _ ->
      ()
  | Pushtrap (_, x, _, _) ->
      f x

let iter_block_bound_vars f block =
  List.iter f block.params;
  List.iter (fun i -> iter_instr_bound_vars f i) block.body;
  iter_last_bound_vars f block.branch

(****)

type st = { index : int; mutable lowlink : int; mutable in_stack : bool }

let find_loops ((_, blocks, _) as prog) =
  let in_loop = ref AddrMap.empty in
  let index = ref 0 in
  let state = ref AddrMap.empty in
  let stack = Stack.create () in
  let rec traverse pc =
    let st = {index = !index; lowlink = !index; in_stack = true } in
    state := AddrMap.add pc st !state;
    incr index;
    Stack.push pc stack;
    Code.fold_children blocks pc
      (fun pc' () ->
         try
           let st' = AddrMap.find pc' !state in
           if st'.in_stack then st.lowlink <- min st.lowlink st'.index
         with Not_found ->
           traverse pc';
           let st' = AddrMap.find pc' !state in
           st.lowlink <- min st.lowlink st'.lowlink)
      ();
    if st.index = st.lowlink then begin
      let l = ref [] in
      while
        let pc' = Stack.pop stack in
        l := pc' :: !l;
        (AddrMap.find pc' !state).in_stack <- false;
        pc' <> pc
      do () done;
      if List.length !l > 1 then
        List.iter (fun pc' -> in_loop := AddrMap.add pc' pc !in_loop) !l
    end
  in
  Code.fold_closures prog (fun _ _ (pc, _) () -> traverse pc) ();
  !in_loop

let mark_variables in_loop (pc, blocks, free_pc) =
  let vars = VarTbl.make () (-1) in
  let visited = Array.make free_pc false in
  let rec traverse pc =
    if not visited.(pc) then begin
      visited.(pc) <- true;
      let block = AddrMap.find pc blocks in
      begin try
        let pc' = AddrMap.find pc in_loop in
        iter_block_bound_vars (fun x ->
(*
Format.eprintf "!%a: %d@." Var.print x pc';
*)
 VarTbl.set vars x pc') block
      with Not_found -> () end;
      List.iter
        (fun i ->
           match i with
             Let (_, Closure (_, (pc', _))) -> traverse pc'
           | _                              -> ())
        block.body;
      Code.fold_children blocks pc (fun pc' () -> traverse pc') ()
    end
  in
  traverse pc;
  vars

let free_variables vars in_loop (pc, blocks, free_pc) =
  let all_freevars = ref AddrMap.empty in
  let freevars = ref AddrMap.empty in
  let visited = Array.make free_pc false in
  let rec traverse pc =
    if not visited.(pc) then begin
      visited.(pc) <- true;
      let block = AddrMap.find pc blocks in
      iter_block_free_vars
        (fun x ->
           let pc' = VarTbl.get vars x in
(*
Format.eprintf "%a: %d@." Var.print x pc';
*)
           if pc' <> -1 then begin
             let fv =
               try AddrMap.find pc' !all_freevars with Not_found -> VarSet.empty in
             let s = VarSet.add x fv in
             all_freevars := AddrMap.add pc' s !all_freevars
           end)
        block;
      begin try
        let pc'' = AddrMap.find pc in_loop in
        all_freevars := AddrMap.remove pc'' !all_freevars
      with Not_found -> () end;
      List.iter
        (fun i ->
           match i with
             Let (_, Closure (_, (pc', _))) ->
               traverse pc';
               begin try
                 let pc'' = AddrMap.find pc in_loop in
                 let fv =
                   try AddrMap.find pc'' !all_freevars with Not_found -> VarSet.empty in
                 freevars := AddrMap.add pc' fv !freevars;
                 all_freevars := AddrMap.remove pc'' !all_freevars
               with Not_found ->
                 freevars := AddrMap.add pc' VarSet.empty !freevars;
               end
           | _ -> ())
        block.body;
      Code.fold_children blocks pc (fun pc' () -> traverse pc') ()
    end
  in
   traverse pc;
(*
AddrMap.iter
(fun pc fv -> if VarSet.cardinal fv > 0 then
Format.eprintf ">> %d: %d@." pc (VarSet.cardinal fv))
!freevars;
*)
  !freevars

let f p =
  Code.invariant p;
  let t = Util.Timer.make () in
  let in_loop = find_loops p in
  let vars = mark_variables in_loop p in
  let free_vars = free_variables vars in_loop p in
  if times () then Format.eprintf "  free vars: %a@." Util.Timer.print t;
  free_vars

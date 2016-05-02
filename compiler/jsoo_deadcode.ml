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

let debug = Option.Debug.find "deadcode"
let times = Option.Debug.find "times"

open Code

type def = Expr of expr | Var of Var.t

type t =
  { blocks : block AddrMap.t;
    live : int array;
    defs : def list array;
    mutable reachable_blocks : AddrSet.t;
    pure_funs : VarSet.t }

(****)

let pure_expr pure_funs e =
  Pure_fun.pure_expr pure_funs e && Option.Optim.deadcode ()

(****)

let rec mark_var st x =
  let x = Var.idx x in
  st.live.(x) <- st.live.(x) + 1;
  if st.live.(x) = 1 then
    List.iter (fun e -> mark_def st e) st.defs.(x)

and mark_def st d =
  match d with
    Var x  -> mark_var st x
  | Expr e -> if pure_expr st.pure_funs e then mark_expr st e

and mark_expr st e =
  match e with
    Const _ | Constant _ ->
      ()
  | Apply (f, l, _) ->
      mark_var st f; List.iter (fun x -> mark_var st x) l
  | Block (_, a) ->
      Array.iter (fun x -> mark_var st x) a
  | Field (x, _) ->
      mark_var st x
  | Closure (_, (pc, _)) ->
      mark_reachable st pc
  | Prim (_, l) ->
      List.iter (fun x -> match x with Pv x -> mark_var st x | _ -> ()) l

and mark_cont_reachable st (pc, _param) = mark_reachable st pc

and mark_reachable st pc =
  if not (AddrSet.mem pc st.reachable_blocks) then begin
    st.reachable_blocks <- AddrSet.add pc st.reachable_blocks;
    let block = AddrMap.find pc st.blocks in
    List.iter
      (fun i ->
         match i with
           Let (_, e) ->
             if not (pure_expr st.pure_funs e) then mark_expr st e
         | Set_field (x, _, y) ->
             mark_var st x; mark_var st y
         | Array_set (x, y, z) ->
             mark_var st x; mark_var st y; mark_var st z
         | Offset_ref (x, _) ->
             mark_var st x)
      block.body;
    match block.branch with
      Return x | Raise x ->
        mark_var st x
    | Stop ->
        ()
    | Branch cont | Poptrap (cont,_) ->
        mark_cont_reachable st cont
    | Cond (_, x, cont1, cont2) ->
        mark_var st x;
        mark_cont_reachable st cont1; mark_cont_reachable st cont2
    | Switch (x, a1, a2) ->
        mark_var st x;
        Array.iter (fun cont -> mark_cont_reachable st cont) a1;
        Array.iter (fun cont -> mark_cont_reachable st cont) a2
    | Pushtrap (cont1, _, cont2, _) ->
        mark_cont_reachable st cont1; mark_cont_reachable st cont2
  end

(****)

let live_instr st i =
  match i with
    Let (x, e) ->
      st.live.(Var.idx x) > 0 || not (pure_expr st.pure_funs e)
  | Set_field _ | Offset_ref _ | Array_set _ ->
      true

let rec filter_args st pl al =
  match pl, al with
    x :: pl, y :: al ->
      if st.live.(Var.idx x) > 0 then
        y :: filter_args st pl al
      else
        filter_args st pl al
  | [], _ ->
      []
  | _ ->
      assert false

let filter_cont blocks st (pc, args) =
  let params = (AddrMap.find pc blocks).params in
  (pc, filter_args st params args)

let filter_closure blocks st i =
  match i with
    Let (x, Closure (l, cont)) ->
      Let (x, Closure (l, filter_cont blocks st cont))
  | _ ->
      i

let filter_live_last blocks st l =
  match l with
    Return _ | Raise _ | Stop ->
      l
  | Branch cont ->
      Branch (filter_cont blocks st cont)
  | Cond (c, x, cont1, cont2) ->
      Cond (c, x, filter_cont blocks st cont1, filter_cont blocks st cont2)
  | Switch (x, a1, a2) ->
      Switch (x,
              Array.map (fun cont -> filter_cont blocks st cont) a1,
              Array.map (fun cont -> filter_cont blocks st cont) a2)
  | Pushtrap (cont1, x, cont2, pcs) ->
      Pushtrap (filter_cont blocks st cont1,
                x, filter_cont blocks st cont2,
                AddrSet.inter pcs st.reachable_blocks)
  | Poptrap (cont,addr) ->
      Poptrap (filter_cont blocks st cont, addr)

(****)

let ref_count st i =
  match i with
    Let (x, _) -> st.live.(Var.idx x)
  | _          -> 0

let annot st pc xi =
  if not (AddrSet.mem pc st.reachable_blocks) then "x" else
  match xi with
    Last _ ->
      " "
  | Instr i ->
      let c = ref_count st i in
      if c > 0 then Format.sprintf "%d" c else
      if live_instr st i then " " else "x"

(****)

let add_def defs x i =
  let idx = Var.idx x in
  defs.(idx) <- i :: defs.(idx)

let rec add_arg_dep defs params args =
  match params, args with
    x :: params, y :: args ->
      add_def defs x (Var y);
      add_arg_dep defs params args
  | _ ->
      ()

let add_cont_dep blocks defs (pc, args) =
  match try Some (AddrMap.find pc blocks) with Not_found -> None with
    Some block -> add_arg_dep defs block.params args
  | None       -> () (* Dead continuation *)

let f ((pc, blocks, free_pc) as program) =
  let t = Util.Timer.make () in
  let nv = Var.count () in
  let defs = Array.make nv [] in
  let live = Array.make nv 0 in
  let pure_funs = Pure_fun.f program in
  AddrMap.iter
    (fun _ block ->
       List.iter
         (fun i ->
            match i with
              Let (x, e) ->
              add_def defs x (Expr e)
            | Set_field (_, _, _) | Array_set (_, _, _) | Offset_ref (_, _) ->
              ())
         block.body;
       Util.opt_iter
         (fun (_, cont) -> add_cont_dep blocks defs cont) block.handler;
       match block.branch with
         Return _ | Raise _ | Stop ->
         ()
       | Branch cont ->
         add_cont_dep blocks defs cont
       | Cond (_, _, cont1, cont2) ->
         add_cont_dep blocks defs cont1;
         add_cont_dep blocks defs cont2
       | Switch (_, a1, a2) ->
         Array.iter (fun cont -> add_cont_dep blocks defs cont) a1;
         Array.iter (fun cont -> add_cont_dep blocks defs cont) a2
       | Pushtrap (cont, _, _, _) ->
         add_cont_dep blocks defs cont
       | Poptrap (cont,_) ->
         add_cont_dep blocks defs cont)
    blocks;
  let st =
    { live = live; defs = defs; blocks = blocks;
      reachable_blocks = AddrSet.empty; pure_funs = pure_funs }
  in
  mark_reachable st pc;

  if debug () then
    print_program (fun pc xi -> annot st pc xi) (pc, blocks, free_pc);

  let all_blocks = blocks in
  let blocks =
    AddrMap.fold
      (fun pc block blocks ->
         if not (AddrSet.mem pc st.reachable_blocks) then blocks else
         AddrMap.add pc
           { params =
               List.filter (fun x -> st.live.(Var.idx x) > 0) block.params;
             handler =
               Util.opt_map
                 (fun (x, cont) -> (x, filter_cont all_blocks st cont))
                 block.handler;
             body =
               List.map (fun i -> filter_closure all_blocks st i)
                 (List.filter (fun i -> live_instr st i) block.body);
             branch =
               filter_live_last all_blocks st block.branch }
           blocks)
      blocks AddrMap.empty
  in
  if times () then Format.eprintf "  dead code elim.: %a@." Util.Timer.print t;
  (pc, blocks, free_pc), st.live

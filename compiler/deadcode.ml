(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

let debug = Util.debug "deadcode"

open Code

type t =
  { blocks : block AddrMap.t;
    live : int array;
    deps : instr list array;
    mutable live_block : AddrSet.t;
    pure_funs : VarSet.t }

(****)

let req_expr pure_funs e =
  match e with
    Const _  | Block _ | Field _ | Closure _ | Constant _ | Variable _ ->
      false
  | Apply (f, l, n) ->
      begin match n with
        Some n -> let m = List.length l in
                  m > n || (m = n && not (VarSet.mem f pure_funs))
      | None   -> true
      end
  | Prim (p, l) ->
      match p with
        Extern f -> not (Primitive.is_pure f)
      | _        -> false

let req_instr pure_funs i =
  match i with
    Let (_, e) ->
      req_expr pure_funs e
  | Set_field _ | Offset_ref _ | Array_set _ ->
      false

(****)

let rec mark_var st x =
  let x = Var.idx x in
  st.live.(x) <- st.live.(x) + 1;
  if st.live.(x) = 1 then
    List.iter
      (fun i -> if not (req_instr st.pure_funs i) then mark_instr st i)
      st.deps.(x)

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
      mark_req st pc
  | Prim (_, l) ->
      List.iter (fun x -> match x with Pv x -> mark_var st x | _ -> ()) l
  | Variable x ->
      mark_var st x

and mark_instr st i =
  match i with
    Let (_, e) ->
      mark_expr st e
  | Set_field (_, _, x) ->
      mark_var st x
  | Array_set (_, x, y) ->
      mark_var st x; mark_var st y
  | Offset_ref _ ->
      assert false

and mark_cont st (pc, param) = mark_req st pc

and mark_req st pc =
  if not (AddrSet.mem pc st.live_block) then begin
    st.live_block <- AddrSet.add pc st.live_block;
    let block = AddrMap.find pc st.blocks in
    Util.opt_iter (fun (_, cont) -> mark_cont st cont) block.handler;
    List.iter
      (fun i ->
         match i with
           Let (_, e) ->
             if req_expr st.pure_funs e then mark_expr st e
         | Set_field (x, _, _) | Offset_ref (x, _)
         | Array_set (x, _, _) ->
             mark_var st x)
      block.body;
    match block.branch with
      Return x | Raise x ->
        mark_var st x
    | Stop ->
        ()
    | Branch cont ->
        mark_cont st cont
    | Cond (_, x, cont1, cont2) ->
        mark_var st x; mark_cont st cont1; mark_cont st cont2
    | Switch (x, a1, a2) ->
        mark_var st x;
        Array.iter (fun cont -> mark_cont st cont) a1;
        Array.iter (fun cont -> mark_cont st cont) a2
    | Pushtrap (cont1, _, cont2, _) ->
        mark_cont st cont1; mark_cont st cont2
    | Poptrap cont ->
        mark_cont st cont
  end

(****)

let ref_count st i =
  match i with
    Let (x, _) -> st.live.(Var.idx x)
  | _          -> 0

let fully_live_instr st i =
  match i with
    Let (x, _)
  | Set_field (x, _, _) | Offset_ref (x, _) | Array_set (x, _, _) ->
      st.live.(Var.idx x) > 0

let live_instr st i = req_instr st.pure_funs i || fully_live_instr st i

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

let filter_cont blocks st ((pc, args) as cont) =
  let params =
    if Code.is_dummy_cont cont then [] else (AddrMap.find pc blocks).params in
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
  | Pushtrap (cont1, x, cont2, pc) ->
      Pushtrap (filter_cont blocks st cont1,
                x, filter_cont blocks st cont2,
                pc)
  | Poptrap cont ->
      Poptrap (filter_cont blocks st cont)

let annot st pc xi =
  if not (AddrSet.mem pc st.live_block) then "x" else
  match xi with
    Last _ ->
      " "
  | Instr i ->
      if fully_live_instr st i then begin
        let c = ref_count st i in
        if c = 0 then " " else Format.sprintf "%d" c
      end else
        if req_instr st.pure_funs i then "*" else "x"

let add_dep deps x i =
  let idx = Var.idx x in
  deps.(idx) <- i :: deps.(idx)

let rec add_arg_dep deps params args =
  match params, args with
    x :: params, y :: args ->
      add_dep deps x (Let (x, Variable y));
      add_arg_dep deps params args
  | _ ->
      ()

let add_cont_dep blocks deps (pc, args) =
  match try Some (AddrMap.find pc blocks) with Not_found -> None with
    Some block -> add_arg_dep deps block.params args
  | None       -> () (* Dead continuation *)

let f ((pc, blocks, free_pc) as program) =
  let nv = Var.count () in
  let deps = Array.make nv [] in
  let live = Array.make nv 0 in
  AddrMap.iter
    (fun _ block ->
       List.iter
         (fun i ->
            match i with
              Let (x, _) | Set_field (x, _, _) | Array_set (x, _, _) ->
                add_dep deps x i
            | Offset_ref _  ->
                ())
         block.body;
       Util.opt_iter
         (fun (_, cont) -> add_cont_dep blocks deps cont) block.handler;
       match block.branch with
         Return _ | Raise _ | Stop ->
           ()
       | Branch cont ->
           add_cont_dep blocks deps cont
       | Cond (_, _, cont1, cont2) ->
           add_cont_dep blocks deps cont1;
           add_cont_dep blocks deps cont2
       | Switch (_, a1, a2) ->
           Array.iter (fun cont -> add_cont_dep blocks deps cont) a1;
           Array.iter (fun cont -> add_cont_dep blocks deps cont) a2
       | Pushtrap (cont, _, _, _) ->
           add_cont_dep blocks deps cont
       | Poptrap cont ->
           add_cont_dep blocks deps cont)
    blocks;
  let st =
    { live = live; deps = deps; blocks = blocks;
      live_block = AddrSet.empty; pure_funs = Pure_fun.f program }
  in
  mark_req st pc;

  if debug () then
    print_program (fun pc xi -> annot st pc xi) (pc, blocks, free_pc);

  let all_blocks = blocks in
  let blocks =
    AddrMap.fold
      (fun pc block blocks ->
         if not (AddrSet.mem pc st.live_block) then blocks else
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
  (pc, blocks, free_pc), st.live

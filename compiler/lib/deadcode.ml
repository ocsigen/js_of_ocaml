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

let debug = Debug.find "deadcode"

let times = Debug.find "times"

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

open Code

type def =
  | Expr of expr
  | Var of Var.t
  | Field_update of Var.t

let add_def defs x i =
  let idx = Var.idx x in
  defs.(idx) <- i :: defs.(idx)

type variable_uses = int array

type t =
  { blocks : block Addr.Map.t
  ; live : variable_uses
  ; defs : def list array
  ; reachable_blocks : BitSet.t
  ; pure_funs : Pure_fun.t
  ; mutable deleted_instrs : int
  ; mutable deleted_blocks : int
  ; mutable deleted_params : int
  ; mutable block_shortcut : int
  }

(****)

let pure_expr pure_funs e = Pure_fun.pure_expr pure_funs e && Config.Flag.deadcode ()

(****)

let rec mark_var st x =
  let x = Var.idx x in
  st.live.(x) <- st.live.(x) + 1;
  if st.live.(x) = 1 then List.iter st.defs.(x) ~f:(fun e -> mark_def st x e)

and mark_def st x d =
  match d with
  | Var y -> mark_var st y
  | Field_update y ->
      (* A [Set_field (x, _, y)] becomes live *)
      st.live.(x) <- st.live.(x) + 1;
      mark_var st y
  | Expr e -> if pure_expr st.pure_funs e then mark_expr st e

and mark_expr st e =
  match e with
  | Constant _ -> ()
  | Apply { f; args; _ } ->
      mark_var st f;
      List.iter args ~f:(fun x -> mark_var st x)
  | Block (_, a, _, _) -> Array.iter a ~f:(fun x -> mark_var st x)
  | Field (x, _, _) -> mark_var st x
  | Closure (_, (pc, _), _) -> mark_reachable st pc
  | Special _ -> ()
  | Prim (_, l) ->
      List.iter l ~f:(fun x ->
          match x with
          | Pv x -> mark_var st x
          | _ -> ())

and mark_cont_reachable st (pc, _param) = mark_reachable st pc

and mark_reachable st pc =
  if not (BitSet.mem st.reachable_blocks pc)
  then (
    BitSet.set st.reachable_blocks pc;
    let block = Addr.Map.find pc st.blocks in
    List.iter block.body ~f:(fun i ->
        match i with
        | Let (_, Prim (Extern "caml_update_dummy", [ Pv x; Pv y ])) ->
            if st.live.(Var.idx x) = 0
            then
              (* We will keep this instruction only if x is live *)
              add_def st.defs x (Field_update y)
            else (
              mark_var st x;
              mark_var st y)
        | Let (_, e) -> if not (pure_expr st.pure_funs e) then mark_expr st e
        | Event _ | Assign _ -> ()
        | Set_field (x, _, _, y) -> (
            match st.defs.(Var.idx x) with
            | [ Expr (Block _) ] when st.live.(Var.idx x) = 0 ->
                (* We will keep this instruction only if x is live *)
                add_def st.defs x (Field_update y)
            | _ ->
                mark_var st x;
                mark_var st y)
        | Array_set (x, y, z) ->
            mark_var st x;
            mark_var st y;
            mark_var st z
        | Offset_ref (x, _) -> mark_var st x);
    match block.branch with
    | Return x | Raise (x, _) -> mark_var st x
    | Stop -> ()
    | Branch cont | Poptrap cont -> mark_cont_reachable st cont
    | Cond (x, cont1, cont2) ->
        mark_var st x;
        mark_cont_reachable st cont1;
        mark_cont_reachable st cont2
    | Switch (x, a1) ->
        mark_var st x;
        Array.iter a1 ~f:(fun cont -> mark_cont_reachable st cont)
    | Pushtrap (cont1, _, cont2) ->
        mark_cont_reachable st cont1;
        mark_cont_reachable st cont2)

(****)

let live_instr st i =
  match i with
  | Let (_, Prim (Extern "caml_update_dummy", [ Pv x; Pv _ ])) -> st.live.(Var.idx x) > 0
  | Let (x, e) -> st.live.(Var.idx x) > 0 || not (pure_expr st.pure_funs e)
  | Assign (x, _) | Set_field (x, _, _, _) -> st.live.(Var.idx x) > 0
  | Event _ | Offset_ref _ | Array_set _ -> true

let rec filter_args st pl al =
  match pl, al with
  | x :: pl, y :: al ->
      if st.live.(Var.idx x) > 0
      then y :: filter_args st pl al
      else (
        st.deleted_params <- st.deleted_params + 1;
        filter_args st pl al)
  | [], [] -> []
  | _ -> assert false

let filter_cont blocks st (pc, args) =
  let params = (Addr.Map.find pc blocks).params in
  pc, filter_args st params args

let filter_closure blocks st i =
  match i with
  | Let (x, Closure (l, cont, gloc)) ->
      Let (x, Closure (l, filter_cont blocks st cont, gloc))
  | _ -> i

let filter_live_last blocks st l =
  match l with
  | Return _ | Raise _ | Stop -> l
  | Branch cont -> Branch (filter_cont blocks st cont)
  | Cond (x, cont1, cont2) ->
      Cond (x, filter_cont blocks st cont1, filter_cont blocks st cont2)
  | Switch (x, a1) -> Switch (x, Array.map a1 ~f:(fun cont -> filter_cont blocks st cont))
  | Pushtrap (cont1, x, cont2) ->
      Pushtrap (filter_cont blocks st cont1, x, filter_cont blocks st cont2)
  | Poptrap cont -> Poptrap (filter_cont blocks st cont)

(****)

let ref_count st i =
  match i with
  | Let (x, _) -> st.live.(Var.idx x)
  | _ -> 0

let annot st pc xi =
  if not (BitSet.mem st.reachable_blocks pc)
  then "x"
  else
    match (xi : Code.Print.xinstr) with
    | Last _ -> " "
    | Instr i ->
        let c = ref_count st i in
        if c > 0 then Format.sprintf "%d" c else if live_instr st i then " " else "x"

(****)

let remove_unused_blocks' p =
  let count = ref 0 in
  let used = Code.used_blocks p in
  let blocks =
    Addr.Map.filter
      (fun pc _ ->
        let b = BitSet.mem used pc in
        if not b then incr count;
        b)
      p.blocks
  in
  { p with blocks }, !count

let remove_unused_blocks p =
  let previous_p = p in
  let t = Timer.make () in
  let p, count = remove_unused_blocks' p in
  if times () then Format.eprintf "  dead block: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - dead block: deleted %d@." count;
  if debug_stats () then Code.check_updates ~name:"dead block" previous_p p ~updates:count;
  p

(****)

let rec add_arg_dep defs params args =
  match params, args with
  | x :: params, y :: args ->
      add_def defs x (Var y);
      add_arg_dep defs params args
  | [], [] -> ()
  | _ -> assert false

let add_cont_dep blocks defs (pc, args) =
  let block = Addr.Map.find pc blocks in
  add_arg_dep defs block.params args

let empty_body b =
  match b with
  | [] | [ Event _ ] -> true
  | _ -> false

let merge_blocks p =
  let previous_p = p in
  let t = Timer.make () in
  let preds = Array.make p.free_pc 0 in
  let assigned = ref Var.Set.empty in
  let merged = ref 0 in
  let subst =
    let nv = Var.count () in
    Array.init nv ~f:(fun i -> Var.of_idx i)
  in
  let () =
    let mark_cont (pc', _) = preds.(pc') <- preds.(pc') + 1 in
    Addr.Map.iter
      (fun _ { body; branch; _ } ->
        List.iter body ~f:(function
          | Let (_, Closure (_, cont, _)) -> mark_cont cont
          | Assign (x, _) -> assigned := Var.Set.add x !assigned
          | _ -> ());
        match branch with
        | Branch cont -> mark_cont cont
        | Cond (_, cont1, cont2) ->
            mark_cont cont1;
            mark_cont cont2
        | Switch (_, a1) -> Array.iter ~f:mark_cont a1
        | Pushtrap (cont1, _, cont2) ->
            mark_cont cont1;
            mark_cont cont2
        | Poptrap cont -> mark_cont cont
        | Return _ | Raise _ | Stop -> ())
      p.blocks
  in
  let p =
    let visited = BitSet.create' p.free_pc in
    let rec process_branch pc blocks =
      let block = Addr.Map.find pc blocks in
      match block.branch with
      | Branch (pc_, args) when preds.(pc_) = 1 ->
          let to_inline = Addr.Map.find pc_ blocks in
          if List.exists to_inline.params ~f:(fun x -> Var.Set.mem x !assigned)
          then block, blocks
          else (
            incr merged;
            let to_inline, blocks = process_branch pc_ blocks in
            List.iter2 args to_inline.params ~f:(fun arg param ->
                Code.Var.propagate_name param arg;
                subst.(Code.Var.idx param) <- arg);
            let block =
              { params = block.params
              ; branch = to_inline.branch
              ; body =
                  (let[@tail_mod_cons] rec aux = function
                     | [ (Event _ as ev) ] -> (
                         match to_inline.body with
                         | Event _ :: _ -> to_inline.body
                         | _ -> ev :: to_inline.body)
                     | [] -> to_inline.body
                     | x :: rest -> x :: aux rest
                   in
                   aux block.body)
              }
            in
            let blocks = Addr.Map.remove pc_ blocks in
            let blocks = Addr.Map.add pc block blocks in
            block, blocks)
      | _ -> block, blocks
    in
    let rec traverse pc blocks =
      if BitSet.mem visited pc
      then blocks
      else
        let () = BitSet.set visited pc in
        let _block, blocks = process_branch pc blocks in
        Code.fold_children blocks pc traverse blocks
    in
    let blocks =
      Code.fold_closures p (fun _ _ (pc, _) _ blocks -> traverse pc blocks) p.blocks
    in
    { p with blocks }
  in
  let p =
    if !merged = 0
    then p
    else
      let rec rename x =
        let y = subst.(Code.Var.idx x) in
        if Code.Var.equal x y then y else rename y
      in
      Subst.Excluding_Binders.program rename p
  in
  if times () then Format.eprintf "  merge block: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - merge block: merged %d@." !merged;
  if debug_stats ()
  then Code.check_updates ~name:"merge block" previous_p p ~updates:!merged;
  p

let remove_empty_blocks st (p : Code.program) : Code.program =
  let shortcuts = Addr.Hashtbl.create 16 in
  let rec resolve_rec visited ((pc, args) as cont) =
    if Addr.Set.mem pc visited
    then cont
    else
      match Addr.Hashtbl.find_opt shortcuts pc with
      | Some (params, cont) ->
          let pc', args' = resolve_rec (Addr.Set.add pc visited) cont in
          let s = Subst.from_map (Subst.build_mapping params args) in
          pc', List.map ~f:s args'
      | None -> cont
  in
  let resolve cont =
    let cont' = resolve_rec Addr.Set.empty cont in
    if not (Code.cont_equal cont cont') then st.block_shortcut <- st.block_shortcut + 1;
    cont'
  in
  Addr.Map.iter
    (fun pc block ->
      match block with
      | { params; body; branch = Branch cont; _ } when empty_body body ->
          let args =
            List.fold_left
              ~f:(fun args x -> Var.Set.add x args)
              ~init:Var.Set.empty
              (snd cont)
          in
          (* We can skip an empty block if its parameters are only
             used as argument to the continuation *)
          if
            List.for_all
              ~f:(fun x -> st.live.(Var.idx x) = 1 && Var.Set.mem x args)
              params
          then Addr.Hashtbl.add shortcuts pc (params, cont)
      | _ -> ())
    p.blocks;
  let blocks =
    Addr.Map.map
      (fun block ->
        { block with
          branch =
            (let branch = block.branch in
             match branch with
             | Branch cont -> Branch (resolve cont)
             | Cond (x, cont1, cont2) ->
                 let cont1' = resolve cont1 in
                 let cont2' = resolve cont2 in
                 if Code.cont_equal cont1' cont2'
                 then Branch cont1'
                 else Cond (x, cont1', cont2')
             | Switch (x, a1) -> Switch (x, Array.map ~f:resolve a1)
             | Pushtrap (cont1, x, cont2) -> Pushtrap (resolve cont1, x, resolve cont2)
             | Poptrap cont -> Poptrap (resolve cont)
             | Return _ | Raise _ | Stop -> branch)
        })
      p.blocks
  in
  { p with blocks }

let f ({ blocks; _ } as p : Code.program) =
  let previous_p = p in
  Code.invariant p;
  let t = Timer.make () in
  let nv = Var.count () in
  let defs = Array.make nv [] in
  let live = Array.make nv 0 in
  let pure_funs = Pure_fun.f p in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, e) -> add_def defs x (Expr e)
          | Assign (x, y) -> add_def defs x (Var y)
          | Event _ | Set_field (_, _, _, _) | Array_set (_, _, _) | Offset_ref (_, _) ->
              ());
      match block.branch with
      | Return _ | Raise _ | Stop -> ()
      | Branch cont -> add_cont_dep blocks defs cont
      | Cond (_, cont1, cont2) ->
          add_cont_dep blocks defs cont1;
          add_cont_dep blocks defs cont2
      | Switch (_, a1) -> Array.iter a1 ~f:(fun cont -> add_cont_dep blocks defs cont)
      | Pushtrap (cont, _, cont_h) ->
          add_cont_dep blocks defs cont_h;
          add_cont_dep blocks defs cont
      | Poptrap cont -> add_cont_dep blocks defs cont)
    blocks;
  let st =
    { live
    ; defs
    ; blocks
    ; reachable_blocks = BitSet.create' p.free_pc
    ; pure_funs
    ; deleted_instrs = 0
    ; deleted_blocks = 0
    ; deleted_params = 0
    ; block_shortcut = 0
    }
  in
  mark_reachable st p.start;
  if debug () then Print.program Format.err_formatter (fun pc xi -> annot st pc xi) p;
  let p =
    let all_blocks = blocks in
    let blocks =
      Addr.Map.filter_map
        (fun pc block ->
          if not (BitSet.mem st.reachable_blocks pc)
          then (
            st.deleted_blocks <- st.deleted_blocks + 1;
            None)
          else
            Some
              { params = List.filter block.params ~f:(fun x -> st.live.(Var.idx x) > 0)
              ; body =
                  List.fold_left block.body ~init:[] ~f:(fun acc i ->
                      match i, acc with
                      | Event _, Event _ :: prev ->
                          (* Avoid consecutive events (keep just the last one) *)
                          i :: prev
                      | _ ->
                          if live_instr st i
                          then filter_closure all_blocks st i :: acc
                          else (
                            st.deleted_instrs <- st.deleted_instrs + 1;
                            acc))
                  |> List.rev
              ; branch = filter_live_last all_blocks st block.branch
              })
        blocks
    in
    { p with blocks }
  in
  let p = remove_empty_blocks st p in
  if times () then Format.eprintf "  dead code elim.: %a@." Timer.print t;
  if stats ()
  then
    Format.eprintf
      "Stats - dead code: deleted %d instructions, %d blocks, %d parameters, %d \
       branches@."
      st.deleted_instrs
      st.deleted_blocks
      st.deleted_params
      st.block_shortcut;
  if debug_stats ()
  then
    Code.check_updates
      ~name:"deadcode"
      previous_p
      p
      ~updates:
        (st.deleted_instrs + st.deleted_blocks + st.deleted_params + st.block_shortcut);
  let p = remove_unused_blocks p in
  if stats ()
  then (
    let live = ref 0 in
    Array.iter st.live ~f:(function
      | 0 -> ()
      | _ -> incr live);
    let total = Var.count () in
    let ratio = float !live /. float total *. 100. in
    Format.eprintf "Stats - live variables: %d/%d = %.1f%%@." !live total ratio);
  Code.invariant p;
  p, st.live

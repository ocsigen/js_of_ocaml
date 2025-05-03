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
  ; mutable reachable_blocks : Addr.Set.t
  ; pure_funs : Var.Set.t
  ; mutable deleted_instrs : int
  ; mutable deleted_blocks : int
  ; mutable deleted_params : int
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
  if not (Addr.Set.mem pc st.reachable_blocks)
  then (
    st.reachable_blocks <- Addr.Set.add pc st.reachable_blocks;
    let block = Addr.Map.find pc st.blocks in
    List.iter block.body ~f:(fun i ->
        match i with
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
  if not (Addr.Set.mem pc st.reachable_blocks)
  then "x"
  else
    match (xi : Code.Print.xinstr) with
    | Last _ -> " "
    | Instr i ->
        let c = ref_count st i in
        if c > 0 then Format.sprintf "%d" c else if live_instr st i then " " else "x"

(****)

let eval_int_cond body branch x i =
  match body, branch with
  | [ Let (y, Prim (Eq, [ Pc (Int j); Pv x' ])) ], Cond (y', cont, cont')
    when Var.equal x x' && Var.equal y y' ->
      Some (if Targetint.equal i j then cont else cont')
  | [ Let (y, Prim (Lt, [ Pc (Int j); Pv x' ])) ], Cond (y', cont, cont')
    when Var.equal x x' && Var.equal y y' ->
      Some (if Targetint.( < ) i j then cont else cont')
  | [ Let (y, Prim (IsInt, [ Pv x' ])) ], Cond (y', cont, _)
    when Var.equal x x' && Var.equal y y' -> Some cont
  | [], Switch (x', a)
    when Var.equal x x' && Targetint.unsigned_lt i (Targetint.of_int_exn (Array.length a))
    -> Some a.(Targetint.to_int_exn i)
  | _ -> None

let eval_tag_cond body branch x i =
  match body, branch with
  | Let (y, Prim (Extern "%direct_obj_tag", [ Pv x' ])) :: rem, _ when Var.equal x x' ->
      eval_int_cond rem branch y i
  | [ Let (y, Prim (IsInt, [ Pv x' ])) ], Cond (y', _, cont)
    when Var.equal x x' && Var.equal y y' -> Some cont
  | _ -> None

let check_cont st blocks pc (pc', args) kind =
  let block = Addr.Map.find pc' blocks in
  List.iter2
    ~f:(fun arg param ->
      if
        match st.defs.(Var.idx arg) with
        | [ Expr (Constant (Int i)) ] ->
            true || Option.is_none (eval_int_cond block.body block.branch param i)
        | [ Expr (Block (i, _, _, _)) ] ->
            true
            || Option.is_none
                 (eval_tag_cond block.body block.branch param (Targetint.of_int_exn i))
        | _ -> false
      then
        if
          match block.body, block.branch with
          | Let (_, Prim (Extern "%direct_obj_tag", [ Pv x ])) :: _, _
            when Var.equal x param ->
              Format.eprintf "ZZZZ %d %s tag@." pc kind;
              true
          | Let (_, Prim (Eq, [ Pc _; Pv x ])) :: _, _ when Var.equal x param ->
              Format.eprintf "ZZZZ %d %s eq@." pc kind;
              true
          | Let (_, Prim (Lt, [ Pc _; Pv x ])) :: _, _ when Var.equal x param ->
              Format.eprintf "ZZZZ %d %s lt@." pc kind;
              true
          | Let (_, Prim (IsInt, [ Pv x ])) :: _, _ when Var.equal x param ->
              Format.eprintf "ZZZZ %d %s int@." pc kind;
              true
          | _, Switch (x, _) when Var.equal x param ->
              Format.eprintf "ZZZZ %d %s switch@." pc kind;
              true
          | _ -> false
        then (
          (try
             let g = Structure.build_graph blocks pc in
             let t = Structure.dominator_tree g in
             Format.eprintf
               "BBB %b %d@."
               (Structure.is_loop_header g pc')
               (Addr.Set.cardinal (Structure.get_edges t pc'))
           with Assert_failure _ -> ());
          Format.eprintf "AAA %d" (List.length args);
          List.iter
            ~f:(fun x ->
              Format.eprintf
                " %d%s"
                st.live.(Var.idx x)
                (if Var.equal x param then "*" else ""))
            block.params;
          Format.eprintf "@."))
    args
    block.params

let check_branch st blocks pc branch =
  match branch with
  | Return _ | Raise _ | Stop -> ()
  | Branch cont -> check_cont st blocks pc cont "branch"
  | Cond (_, cont, cont') ->
      check_cont st blocks pc cont "cond";
      check_cont st blocks pc cont' "cond"
  | Switch (_, a) -> Array.iter ~f:(fun cont -> check_cont st blocks pc cont "switch") a
  | Pushtrap (cont, _, _) -> check_cont st blocks pc cont "pushtrap"
  | Poptrap cont -> check_cont st blocks pc cont "poptrap"

let check_branches st (p : program) =
  Addr.Map.iter (fun pc block -> check_branch st p.blocks pc block.branch) p.blocks

(****)

let rec add_arg_dep defs params args =
  match params, args with
  | x :: params, y :: args ->
      add_def defs x (Var y);
      add_arg_dep defs params args
  | [], [] -> ()
  | _ -> assert false

let add_cont_dep blocks defs (pc, args) =
  match try Some (Addr.Map.find pc blocks) with Not_found -> None with
  | Some block -> add_arg_dep defs block.params args
  | None -> () (* Dead continuation *)

let empty_body b =
  match b with
  | [] | [ Event _ ] -> true
  | _ -> false

let remove_empty_blocks ~live_vars (p : Code.program) : Code.program =
  let shortcuts = Hashtbl.create 16 in
  let rec resolve_rec visited ((pc, args) as cont) =
    if Addr.Set.mem pc visited
    then cont
    else
      match Hashtbl.find_opt shortcuts pc with
      | Some (params, cont) ->
          let pc', args' = resolve_rec (Addr.Set.add pc visited) cont in
          let s = Subst.from_map (Subst.build_mapping params args) in
          pc', List.map ~f:s args'
      | None -> cont
  in
  let resolve cont = resolve_rec Addr.Set.empty cont in
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
              ~f:(fun x -> live_vars.(Var.idx x) = 1 && Var.Set.mem x args)
              params
          then Hashtbl.add shortcuts pc (params, cont)
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
             | Cond (x, cont1, cont2) -> Cond (x, resolve cont1, resolve cont2)
             | Switch (x, a1) -> Switch (x, Array.map ~f:resolve a1)
             | Pushtrap (cont1, x, cont2) -> Pushtrap (resolve cont1, x, resolve cont2)
             | Poptrap cont -> Poptrap (resolve cont)
             | Return _ | Raise _ | Stop -> branch)
        })
      p.blocks
  in
  { p with blocks }

let f ({ blocks; _ } as p : Code.program) =
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
    ; reachable_blocks = Addr.Set.empty
    ; pure_funs
    ; deleted_instrs = 0
    ; deleted_blocks = 0
    ; deleted_params = 0
    }
  in
  mark_reachable st p.start;
  if debug () then Print.program Format.err_formatter (fun pc xi -> annot st pc xi) p;
  let all_blocks = blocks in
  let blocks =
    Addr.Map.filter_map
      (fun pc block ->
        if not (Addr.Set.mem pc st.reachable_blocks)
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
  if times () then Format.eprintf "  dead code elim.: %a@." Timer.print t;
  check_branches st { p with blocks };
  if stats ()
  then
    Format.eprintf
      "Stats - dead code: deleted %d instructions, %d blocks, %d parameters@."
      st.deleted_instrs
      st.deleted_blocks
      st.deleted_params;
  { p with blocks }, st.live

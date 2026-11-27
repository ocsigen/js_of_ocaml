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

(* Path-sensitive propagation of the value of the variable tested by a
   conditional: [x] is zero in the second branch of [Cond (x, _, _)], one in
   the first one when [x] is known to be a boolean, and [i] in the [i]th
   branch of [Switch (x, _)].

   Along such an edge, a fresh variable bound to the constant is substituted
   for [x] in the arguments of the edge, and in the block it leads to when
   that block is reached through this edge only. A test of [x] at the end of
   this block is folded.

   The value tested by a conditional is either an integer or a block
   corresponding to a variant, so the second branch is indeed taken exactly
   when it is zero. A [Cond] tests JavaScript truthiness, though: an ill-typed
   value coming from an external primitive or [Obj.magic], such as [false],
   [null], [""] or [NaN], also takes the second branch, and is then replaced
   by zero. The compiler does not preserve the truthiness of such values
   elsewhere either: [Eval] folds a [Cond] on [""] or on [nan] as true.

   We only substitute the constant when this rewrites the last use of the
   value of [x]: the code generator does not have to materialize a variable
   which is just tested by conditionals, so this is where the transformation
   pays off. Rewriting the other uses costs more than it saves, since the
   constant then has to be bound to a variable of its own. The typical
   pattern is [if not c then ... else <value of c>], compiled to

     var a = x === y ? 1 : 0; if(! a) return a;

   which becomes [if(x !== y) return 0;]. *)

open! Stdlib
open Code

let times = Debug.find "times"

let stats = Debug.find "stats"

let incr_addr tbl pc =
  Addr.Hashtbl.replace
    tbl
    pc
    (match Addr.Hashtbl.find_opt tbl pc with
    | None -> 1
    | Some n -> n + 1)

let add_var_count tbl x n =
  Var.Hashtbl.replace
    tbl
    x
    (n
    +
    match Var.Hashtbl.find_opt tbl x with
    | None -> 0
    | Some n -> n)

let incr_var tbl x = add_var_count tbl x 1

let var_count tbl x =
  match Var.Hashtbl.find_opt tbl x with
  | None -> 0
  | Some n -> n

(* Iterate over the occurrences of the variables whose value is used, that is,
   over all the free variables of a block but the variable tested by a [Cond]
   or a [Switch]. *)
let iter_value_uses f block =
  let cont (_, args) = List.iter ~f args in
  List.iter
    ~f:(fun i ->
      match i with
      | Let (_, Closure (_, c, _)) -> cont c
      | _ -> Freevars.iter_instr_free_vars f i)
    block.body;
  match block.branch with
  | Cond (_, c1, c2) ->
      cont c1;
      cont c2
  | Switch (_, conts) -> Array.iter ~f:cont conts
  | (Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _) as branch ->
      Freevars.iter_last_free_var f branch

let subst_cont s ((pc, args) as cont) =
  let args' = List.map_sharing args ~f:s in
  if phys_equal args' args then cont else pc, args'

(* Like [Subst.Excluding_Binders.block], but does not substitute the variable
   tested by a [Cond] or a [Switch] (matching [iter_value_uses]). *)
let subst_block_value_uses s block =
  let body = Subst.Excluding_Binders.instrs s block.body in
  let branch =
    match block.branch with
    | Cond (x, c1, c2) ->
        let c1' = subst_cont s c1 in
        let c2' = subst_cont s c2 in
        if phys_equal c1' c1 && phys_equal c2' c2 then block.branch else Cond (x, c1', c2')
    | Switch (x, conts) ->
        let conts' = Array.map_sharing conts ~f:(fun c -> subst_cont s c) in
        if phys_equal conts' conts then block.branch else Switch (x, conts')
    | Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _ ->
        Subst.Excluding_Binders.last s block.branch
  in
  if phys_equal body block.body && phys_equal branch block.branch
  then block
  else { block with body; branch }

(* [Assign] needs no special care: it amounts to passing an argument to an
   exception handler (the parser uses it to avoid adding explicit parameters
   for all the expressions which may raise), so the value of an assigned
   variable does not change within a block. *)
type info =
  { boolean : Var.Set.t
        (* Variables whose value is zero or one. Their value is then known in
           both branches of a conditional, not just in the one where they are
           zero. *)
  ; predecessors : int Addr.Hashtbl.t
  ; entries : Addr.Set.t (* Function entry points, entered by a call *)
  }

let collect_info blocks start =
  let predecessors = Addr.Hashtbl.create 128 in
  let boolean = ref Var.Set.empty in
  let entries = ref (Addr.Set.singleton start) in
  Addr.Map.iter
    (fun pc block ->
      Code.fold_children blocks pc (fun pc' () -> incr_addr predecessors pc') ();
      List.iter
        ~f:(fun i ->
          match i with
          | Let (x, Prim ((Not | IsInt _ | Eq _ | Neq _ | Lt | Le | Ult), _)) ->
              boolean := Var.Set.add x !boolean
          | Let (_, Closure (_, (pc', _), _)) -> entries := Addr.Set.add pc' !entries
          | Let _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
        block.body)
    blocks;
  { boolean = !boolean; predecessors; entries = !entries }

(* Whether the block is reached through a single edge, so that what we know
   about the branch leading to it holds at its beginning *)
let reached_once info pc =
  (not (Addr.Set.mem pc info.entries))
  &&
  match Addr.Hashtbl.find_opt info.predecessors pc with
  | Some 1 -> true
  | None | Some _ -> false

(* The edges along which the value of the tested variable is known, as
   [(i, x, v, cont)]: along the [i]th continuation of the branch, [x] has
   value [v]. *)
let known_edges info block =
  match block.branch with
  | Cond (x, cont1, cont2) ->
      let l = [ 1, x, Targetint.zero, cont2 ] in
      if Var.Set.mem x info.boolean then (0, x, Targetint.one, cont1) :: l else l
  | Switch (x, conts) ->
      Array.to_list
        (Array.mapi conts ~f:(fun i cont -> i, x, Targetint.of_int_exn i, cont))
  | Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _ -> []

let cont_of_branch branch i =
  match branch with
  | Cond (_, cont1, cont2) -> if i = 0 then cont1 else cont2
  | Switch (_, conts) -> conts.(i)
  | Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _ -> assert false

let set_cont branch i cont =
  match branch with
  | Cond (x, cont1, cont2) ->
      if i = 0 then Cond (x, cont, cont2) else Cond (x, cont1, cont)
  | Switch (x, conts) ->
      let conts = Array.copy conts in
      conts.(i) <- cont;
      Switch (x, conts)
  | Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _ -> assert false

(* Fold a test of [x], whose value is [v] *)
let fold_branch x v branch =
  match branch with
  | Cond (y, c1, c2) when Var.equal x y ->
      Some (Branch (if Targetint.is_zero v then c2 else c1))
  | Switch (y, conts) when Var.equal x y ->
      let i = Targetint.to_int_exn v in
      if i >= 0 && i < Array.length conts then Some (Branch conts.(i)) else None
  | Cond _ | Switch _ | Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _ ->
      None

(* What we plan to do along an edge where [x] has value [v]: bind a fresh
   variable to [v] and substitute it for [x] in the arguments of the edge
   (when [edge] is set) and in the block the edge leads to ([target]); fold a
   redundant test of [x] at the end of [target]. [rewrites] is the number of
   occurrences of [x] to be substituted. The fresh variable is only created
   when a use is actually substituted. The binding goes at the end of the
   block the edge starts from when the arguments are rewritten — it is then
   in scope in the target block, which this block dominates — and at the
   beginning of the target block otherwise, so that it stays in the
   branch. *)
type plan =
  { x : Var.t
  ; v : Targetint.t
  ; edge : (Addr.t * int) option
  ; target : Addr.t option
  ; rewrites : int
  }

let count_in_args x args =
  List.fold_left ~f:(fun n y -> if Var.equal x y then n + 1 else n) ~init:0 args

let count_in_block x block =
  let n = ref 0 in
  iter_value_uses (fun y -> if Var.equal x y then incr n) block;
  !n

let make_plans info blocks =
  (* Collect candidate edges and record, for each single-predecessor target
     block, the variable whose value is known there. *)
  let target_var = Addr.Hashtbl.create 128 in
  let raw_edges =
    Addr.Map.fold
      (fun pc block acc ->
        List.fold_left
          ~f:(fun acc (i, x, v, (pc', args)) ->
            let target =
              if reached_once info pc'
              then (
                Addr.Hashtbl.replace target_var pc' x;
                Some pc')
              else None
            in
            (pc, i, x, v, args, target) :: acc)
          ~init:acc
          (known_edges info block))
      blocks
      []
  in
  List.fold_left
    ~f:(fun plans (pc, i, x, v, args, target) ->
      (* When [pc] is itself a target block for [x], the incoming plan for
         [pc] already covers the uses of [x] in [pc]'s outgoing branch
         arguments (and folds its branch); do not count or rewrite them
         again here. *)
      let pc_is_target_for_x =
        match Addr.Hashtbl.find_opt target_var pc with
        | Some y -> Var.equal x y
        | None -> false
      in
      let in_args = if pc_is_target_for_x then 0 else count_in_args x args in
      let edge = if in_args > 0 then Some (pc, i) else None in
      let target, in_target =
        match target with
        | None -> None, 0
        | Some pc' ->
            let block' = Addr.Map.find pc' blocks in
            let in_target = count_in_block x block' in
            let can_fold = Option.is_some (fold_branch x v block'.branch) in
            if in_target > 0 || can_fold then Some pc', in_target else None, 0
      in
      if Option.is_none edge && Option.is_none target
      then plans
      else { x; v; edge; target; rewrites = in_args + in_target } :: plans)
    ~init:[]
    raw_edges

type stats =
  { mutable rewritten_edges : int
  ; mutable rewritten_blocks : int
  ; mutable folded_tests : int
  }

let apply_plans blocks plans =
  let st = { rewritten_edges = 0; rewritten_blocks = 0; folded_tests = 0 } in
  (* How many times the value of a candidate variable is used, and how many
     of these uses we would rewrite. *)
  let rewritable = Var.Hashtbl.create 128 in
  List.iter plans ~f:(fun { x; _ } -> Var.Hashtbl.replace rewritable x ());
  let uses = Var.Hashtbl.create 128 in
  Addr.Map.iter
    (fun _ block ->
      iter_value_uses
        (fun x -> if Var.Hashtbl.mem rewritable x then incr_var uses x)
        block)
    blocks;
  let rewritten = Var.Hashtbl.create 128 in
  List.iter plans ~f:(fun { x; rewrites; _ } -> add_var_count rewritten x rewrites);
  let blocks =
    List.fold_left
      ~f:(fun blocks { x; v; edge; target; rewrites = _ } ->
        let rewrite_values = var_count rewritten x = var_count uses x in
        let z = ref None in
        let get_z () =
          match !z with
          | Some z -> z
          | None ->
              let z' = Var.fork x in
              z := Some z';
              z'
        in
        let blocks =
          match edge with
          | Some (pc, i) when rewrite_values ->
              st.rewritten_edges <- st.rewritten_edges + 1;
              let z = get_z () in
              let block = Addr.Map.find pc blocks in
              let pc', args = cont_of_branch block.branch i in
              let args = List.map args ~f:(fun y -> if Var.equal x y then z else y) in
              Addr.Map.add
                pc
                { block with
                  body = block.body @ [ Let (z, Constant (Int v)) ]
                ; branch = set_cont block.branch i (pc', args)
                }
                blocks
          | None | Some _ -> blocks
        in
        match target with
        | None -> blocks
        | Some pc ->
            let block = Addr.Map.find pc blocks in
            let block =
              match fold_branch x v block.branch with
              | Some branch ->
                  st.folded_tests <- st.folded_tests + 1;
                  { block with branch }
              | None -> block
            in
            let used = ref false in
            let block =
              if rewrite_values
              then
                subst_block_value_uses
                  (fun y ->
                    if Var.equal x y
                    then (
                      used := true;
                      get_z ())
                    else y)
                  block
              else block
            in
            if !used then st.rewritten_blocks <- st.rewritten_blocks + 1;
            if !used && Option.is_none edge
            then
              let z = get_z () in
              let body =
                (* After a leading [Event], which other passes expect to
                   find at the very beginning of a block *)
                match block.body with
                | (Event _ as event) :: rem -> event :: Let (z, Constant (Int v)) :: rem
                | rem -> Let (z, Constant (Int v)) :: rem
              in
              Addr.Map.add pc { block with body } blocks
            else Addr.Map.add pc block blocks)
      ~init:blocks
      plans
  in
  blocks, st

let f p =
  let t = Timer.make () in
  let info = collect_info p.blocks p.start in
  let plans = make_plans info p.blocks in
  let p, st =
    if List.is_empty plans
    then p, { rewritten_edges = 0; rewritten_blocks = 0; folded_tests = 0 }
    else
      let blocks, st = apply_plans p.blocks plans in
      let p = { p with blocks } in
      let p = if st.folded_tests > 0 then Deadcode.remove_unused_blocks p else p in
      Code.invariant p;
      p, st
  in
  if times () then Format.eprintf "  condition propagation: %a@." Timer.print t;
  if stats ()
  then
    Format.eprintf
      "Stats - condition propagation: %d edges and %d blocks rewritten, %d tests folded@."
      st.rewritten_edges
      st.rewritten_blocks
      st.folded_tests;
  p

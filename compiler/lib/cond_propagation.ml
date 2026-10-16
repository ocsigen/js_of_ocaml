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
   that block is reached through this edge only.

   The value tested by a conditional is either an integer or a block
   corresponding to a variant, so the second branch is indeed taken exactly
   when it is zero.

   We only do so when this rewrites the last use of the value of [x]: the
   code generator does not have to materialize a variable which is just
   tested by conditionals, so this is where the transformation pays off.
   Rewriting the other uses costs more than it saves, since the constant then
   has to be bound to a variable of its own. The typical pattern is [if not c
   then ... else <value of c>], compiled to

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
   or a [Switch]. Note that [Freevars] ignores the continuation arguments of a
   [Closure], which [Subst] does rewrite; these are always empty when this
   pass runs ([Lambda_lifting] and [Effects] come later), but we would
   under-count uses if this were to change. *)
let iter_value_uses f block =
  let cont (_, args) = List.iter ~f args in
  List.iter ~f:(fun i -> Freevars.iter_instr_free_vars f i) block.body;
  match block.branch with
  | Cond (_, c1, c2) ->
      cont c1;
      cont c2
  | Switch (_, conts) -> Array.iter ~f:cont conts
  | (Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _) as branch ->
      Freevars.iter_last_free_var f branch

type info =
  { assigned : Var.Set.t
        (* An [Assign] makes the value of a variable depend on the path
           followed, so nothing can be propagated about assigned variables. *)
  ; boolean : Var.Set.t
        (* Variables whose value is zero or one. Their value is then known in
           both branches of a conditional, not just in the one where they are
           zero. *)
  ; predecessors : int Addr.Hashtbl.t
  ; entries : Addr.Set.t (* Function entry points, entered by a call *)
  }

let collect_info blocks start =
  let predecessors = Addr.Hashtbl.create 128 in
  let assigned = ref Var.Set.empty in
  let boolean = ref Var.Set.empty in
  let entries = ref (Addr.Set.singleton start) in
  Addr.Map.iter
    (fun pc block ->
      Code.fold_children blocks pc (fun pc' () -> incr_addr predecessors pc') ();
      List.iter
        ~f:(fun i ->
          match i with
          | Assign (x, _) -> assigned := Var.Set.add x !assigned
          | Let (x, Prim ((Not | IsInt _ | Eq _ | Neq _ | Lt | Le | Ult), _)) ->
              boolean := Var.Set.add x !boolean
          | Let (_, Closure (_, (pc', _), _)) -> entries := Addr.Set.add pc' !entries
          | Let _ | Set_field _ | Offset_ref _ | Array_set _ | Event _ -> ())
        block.body)
    blocks;
  { assigned = !assigned; boolean = !boolean; predecessors; entries = !entries }

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
  let known x = not (Var.Set.mem x info.assigned) in
  match block.branch with
  | Cond (x, cont1, cont2) when known x ->
      let l = [ 1, x, Targetint.zero, cont2 ] in
      if Var.Set.mem x info.boolean then (0, x, Targetint.one, cont1) :: l else l
  | Switch (x, conts) when known x ->
      Array.to_list
        (Array.mapi conts ~f:(fun i cont -> i, x, Targetint.of_int_exn i, cont))
  | Cond _ | Switch _ | Return _ | Raise _ | Stop | Branch _ | Pushtrap _ | Poptrap _ ->
      []

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

(* What we plan to do along an edge: bind [z] to the value [v] of [x], and
   substitute it for [x] in the arguments of the edge ([in_args] occurrences)
   and in the block the edge leads to ([target]). The binding goes at the end
   of the block the edge starts from when the arguments are rewritten — it is
   then in scope in the target block, which this block dominates — and at the
   beginning of the target block otherwise, so that it stays in the branch. *)
type plan =
  { x : Var.t
  ; v : Targetint.t
  ; z : Var.t
  ; edge : (Addr.t * int) option
  ; in_args : int
  ; target : Addr.t option
  }

let count_in_args x args =
  List.fold_left ~f:(fun n y -> if Var.equal x y then n + 1 else n) ~init:0 args

let f p =
  let t = Timer.make () in
  let info = collect_info p.blocks p.start in
  let plans =
    Addr.Map.fold
      (fun pc block plans ->
        List.fold_left
          ~f:(fun plans (i, x, v, (pc', args)) ->
            let in_args = count_in_args x args in
            let target = if reached_once info pc' then Some pc' else None in
            if in_args = 0 && Option.is_none target
            then plans
            else
              { x
              ; v
              ; z = Var.fork x
              ; edge = (if in_args > 0 then Some (pc, i) else None)
              ; in_args
              ; target
              }
              :: plans)
          ~init:plans
          (known_edges info block))
      p.blocks
      []
  in
  if List.is_empty plans
  then p
  else
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
      p.blocks;
    let rewritten = Var.Hashtbl.create 128 in
    List.iter plans ~f:(fun { x; in_args; target; _ } ->
        add_var_count rewritten x in_args;
        Option.iter target ~f:(fun pc ->
            iter_value_uses
              (fun y -> if Var.equal x y then incr_var rewritten x)
              (Addr.Map.find pc p.blocks)));
    let n = ref 0 in
    let blocks =
      List.fold_left
        ~f:(fun blocks { x; v; z; edge; target; in_args = _ } ->
          if var_count rewritten x <> var_count uses x
          then blocks
          else
            let blocks =
              match edge with
              | None -> blocks
              | Some (pc, i) ->
                  incr n;
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
            in
            match target with
            | None -> blocks
            | Some pc ->
                let block = Addr.Map.find pc blocks in
                let used = ref false in
                let block =
                  Subst.Excluding_Binders.block
                    (fun y ->
                      if Var.equal x y
                      then (
                        used := true;
                        z)
                      else y)
                    block
                in
                if not !used
                then blocks
                else (
                  incr n;
                  let body =
                    if Option.is_some edge
                    then block.body
                    else
                      (* After a leading [Event], which other passes expect to
                         find at the very beginning of a block *)
                      match block.body with
                      | (Event _ as event) :: rem ->
                          event :: Let (z, Constant (Int v)) :: rem
                      | rem -> Let (z, Constant (Int v)) :: rem
                  in
                  Addr.Map.add pc { block with body } blocks))
        ~init:p.blocks
        plans
    in
    if times () then Format.eprintf "  condition propagation: %a@." Timer.print t;
    if stats () then Format.eprintf "Stats - condition propagation: %d@." !n;
    { p with blocks }

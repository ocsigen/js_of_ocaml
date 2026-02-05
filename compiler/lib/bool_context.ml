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

open! Stdlib
open Code

let times = Debug.find "times"

(* Backward dataflow analysis to determine which variables are only
   used in "boolean contexts" — places where a JS boolean (true/false)
   behaves identically to an OCaml int boolean (1/0).

   Boolean contexts:
   - Cond(v, _, _): JS truthiness is identical for true/1 and false/0
   - Prim(Not, [Pv v]): compiles to 1-v; 1-true=0, 1-false=1
   - Prim(Extern "caml_js_from_bool", [Pv v]): compiles to !!v
   - Flowing into a variable (via block params or Assign) that is itself
     bool-context-only

   Everything else is a non-boolean context. We propagate "not bool-context-only"
   taint backward through phi-nodes (block params) and Assign edges. *)

type t = BitSet.t (* not_bool_only: variables that are NOT bool-context-only *)

let is_bool_context_only (t : t) x = not (BitSet.mem t (Var.idx x))

let mark_var not_bool_only worklist v =
  let idx = Var.idx v in
  if not (BitSet.mem not_bool_only idx)
  then (
    BitSet.set not_bool_only idx;
    Queue.push v worklist)

let mark_prim_arg not_bool_only worklist = function
  | Pv v -> mark_var not_bool_only worklist v
  | Pc _ -> ()

let f (p : program) =
  let t = Timer.make () in
  let nv = Var.count () in
  (* backward_edges.(idx of x) = list of vars y such that y flows into x
     (via block params or Assign). When x is tainted, y must be too. *)
  let backward_edges = Array.make nv [] in
  let not_bool_only = BitSet.create' nv in
  let worklist = Queue.create () in
  (* Build backward edges from continuation args to block params *)
  let add_cont_edges (pc, args) =
    let block = Addr.Map.find pc p.blocks in
    List.iter2 block.params args ~f:(fun param arg ->
        (* param <- arg: if param is tainted, arg must be too *)
        backward_edges.(Var.idx param) <- arg :: backward_edges.(Var.idx param))
  in
  (* Process a block: build edges and classify uses *)
  let process_block _pc block =
    (* Classify instruction uses *)
    List.iter block.body ~f:(fun instr ->
        match instr with
        | Let (_, Prim (Not, [ Pv _ ])) ->
            (* Bool context: 1 - v works for JS booleans *)
            ()
        | Let (_, Prim (Extern name, [ Pv _ ])) when String.equal name "caml_js_from_bool"
          ->
            (* Bool context: !!v works for JS booleans *)
            ()
        | Let (_, Prim (_, args)) ->
            (* All other prims: non-bool context *)
            List.iter args ~f:(mark_prim_arg not_bool_only worklist)
        | Let (_, Apply { f = fv; args; _ }) ->
            mark_var not_bool_only worklist fv;
            List.iter args ~f:(mark_var not_bool_only worklist)
        | Let (_, Block (_, a, _, _)) -> Array.iter a ~f:(mark_var not_bool_only worklist)
        | Let (_, Field (x, _, _)) -> mark_var not_bool_only worklist x
        | Let (_, Closure (_, cont, _)) -> add_cont_edges cont
        | Let (_, (Constant _ | Special _)) -> ()
        | Assign (x, y) ->
            (* x <- y: backward edge from x to y *)
            backward_edges.(Var.idx x) <- y :: backward_edges.(Var.idx x)
        | Set_field (x, _, _, y) ->
            mark_var not_bool_only worklist x;
            mark_var not_bool_only worklist y
        | Offset_ref (x, _) -> mark_var not_bool_only worklist x
        | Array_set (x, y, z) ->
            mark_var not_bool_only worklist x;
            mark_var not_bool_only worklist y;
            mark_var not_bool_only worklist z
        | Event _ -> ());
    (* Classify terminator uses *)
    match block.branch with
    | Return v | Raise (v, _) -> mark_var not_bool_only worklist v
    | Stop -> ()
    | Branch cont -> add_cont_edges cont
    | Cond (_, cont1, cont2) ->
        (* v is used as condition — bool context, don't mark *)
        add_cont_edges cont1;
        add_cont_edges cont2
    | Switch (v, conts) ->
        mark_var not_bool_only worklist v;
        Array.iter conts ~f:add_cont_edges
    | Pushtrap (cont1, _, cont2) ->
        add_cont_edges cont1;
        add_cont_edges cont2
    | Poptrap cont -> add_cont_edges cont
  in
  (* Process all blocks *)
  Addr.Map.iter process_block p.blocks;
  (* Propagate taint backward *)
  while not (Queue.is_empty worklist) do
    let v = Queue.pop worklist in
    List.iter
      backward_edges.(Var.idx v)
      ~f:(fun pred -> mark_var not_bool_only worklist pred)
  done;
  if times () then Format.eprintf "  bool-analysis: %a@." Timer.print t;
  not_bool_only

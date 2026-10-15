(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2026 Hugo Heuzard
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

(* Jump threading. Consider a block that only tests one of its
   parameters:

     join (p, q):  if p then goto t(q) else goto f

   An edge [goto join (c, x)] where [c] is a known integer constant is
   redirected to [t(x)] (if [c] is not zero) or [f] (otherwise). This is
   frequent after inlining a function returning a boolean which is then
   tested. Loop headers, entry points of closures and of the program are
   left alone, so that no loop gets a second entry. *)

open! Stdlib
open Code

let debug = Debug.find "jump-threading"

(* Loop headers: targets of back edges in a depth-first traversal *)
let loop_headers p roots =
  let headers = Addr.Hashtbl.create 16 in
  let state = Addr.Hashtbl.create 16 in
  let rec visit pc =
    match Addr.Hashtbl.find_opt state pc with
    | Some `Active -> Addr.Hashtbl.replace headers pc ()
    | Some `Done -> ()
    | None ->
        Addr.Hashtbl.replace state pc `Active;
        Code.fold_children p.blocks pc (fun pc' () -> visit pc') ();
        Addr.Hashtbl.replace state pc `Done
  in
  List.iter roots ~f:visit;
  headers

let f p =
  let t = Timer.make () in
  let constants = Var.Hashtbl.create 16 in
  let roots = ref [ p.start ] in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(function
        | Let (x, Constant (Int c)) -> Var.Hashtbl.replace constants x c
        | Let (_, Closure (_, (pc, _), _)) -> roots := pc :: !roots
        | _ -> ()))
    p.blocks;
  let headers = loop_headers p !roots in
  let entries = Addr.Hashtbl.create 16 in
  List.iter !roots ~f:(fun pc -> Addr.Hashtbl.replace entries pc ());
  (* Blocks that only test one of their parameters *)
  let tests = Addr.Hashtbl.create 16 in
  Addr.Map.iter
    (fun pc block ->
      match block.branch with
      | Cond (x, cont1, cont2)
        when List.for_all block.body ~f:(function
               | Event _ -> true
               | _ -> false)
             && (not (Addr.Hashtbl.mem headers pc))
             && not (Addr.Hashtbl.mem entries pc) -> (
          match List.find_index ~f:(fun y -> Var.equal x y) block.params with
          | Some i -> Addr.Hashtbl.replace tests pc (i, block.params, cont1, cont2)
          | None -> ())
      | _ -> ())
    p.blocks;
  (* The parameters of a test block may be used in the blocks it
     dominates, not only in its branch: a threaded edge would bypass
     their definition. Only blocks whose parameters are not used
     elsewhere are considered. *)
  let params = Var.Hashtbl.create 16 in
  Addr.Hashtbl.iter
    (fun _ (_, ps, _, _) -> List.iter ps ~f:(fun x -> Var.Hashtbl.replace params x 0))
    tests;
  Addr.Map.iter
    (fun _ block ->
      Freevars.iter_block_free_vars
        (fun x ->
          match Var.Hashtbl.find_opt params x with
          | Some n -> Var.Hashtbl.replace params x (n + 1)
          | None -> ())
        block)
    p.blocks;
  let local_only pc =
    let block = Addr.Map.find pc p.blocks in
    let local = Var.Hashtbl.create 4 in
    Freevars.iter_last_free_var
      (fun x ->
        Var.Hashtbl.replace
          local
          x
          (1 + Option.value ~default:0 (Var.Hashtbl.find_opt local x)))
      block.branch;
    List.for_all block.params ~f:(fun x ->
        Option.value ~default:0 (Var.Hashtbl.find_opt local x) = Var.Hashtbl.find params x)
  in
  Addr.Hashtbl.filter_map_inplace
    (fun pc v -> if local_only pc then Some v else None)
    tests;
  let count = ref 0 in
  let rec thread depth ((pc, args) as cont) =
    if depth > 4
    then cont
    else
      match Addr.Hashtbl.find_opt tests pc with
      | None -> cont
      | Some (i, params, cont1, cont2) -> (
          match Var.Hashtbl.find_opt constants (List.nth args i) with
          | None -> cont
          | Some c ->
              incr count;
              let pc', args' = if Targetint.is_zero c then cont2 else cont1 in
              let s = Subst.from_map (Subst.build_mapping params args) in
              thread (depth + 1) (pc', List.map args' ~f:s))
  in
  let blocks =
    if Addr.Hashtbl.length tests = 0
    then p.blocks
    else
      Addr.Map.map
        (fun block ->
          let branch =
            match block.branch with
            | Branch cont -> Branch (thread 0 cont)
            | Poptrap cont -> Poptrap (thread 0 cont)
            | Cond (x, cont1, cont2) ->
                let cont1 = thread 0 cont1 and cont2 = thread 0 cont2 in
                if cont_equal cont1 cont2 then Branch cont1 else Cond (x, cont1, cont2)
            | Switch (x, a) -> Switch (x, Array.map a ~f:(thread 0))
            | (Return _ | Raise _ | Stop | Pushtrap _) as b -> b
          in
          { block with branch })
        p.blocks
  in
  if debug () then Format.eprintf "  jump threading: %d edges, %a@." !count Timer.print t;
  let p = Deadcode.remove_unused_blocks { p with blocks } in
  Code.invariant p;
  p

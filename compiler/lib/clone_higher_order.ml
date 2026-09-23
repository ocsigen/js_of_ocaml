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

(* Clone higher-order functions for each combination of known
   function arguments they are called with.

   A function [f] one of whose parameters [p] is called in its body is
   duplicated for each set of closures passed for such parameters at
   its call sites, and these call sites are redirected to the
   corresponding copy. The copies are identical to [f] (in
   particular, [p] remains a parameter): the point is that the flow
   analysis then finds that a single function can be called through
   [p] in each copy, so that these calls become direct calls to a
   known function. We only do that for functions passed as argument
   that may raise, as this is then possible to use a cheaper way to
   propagate exceptions.
*)

open! Stdlib
open Code

let debug = Debug.find "clone"

let times = Debug.find "times"

(* Maximal size of a cloned function, in number of instructions *)
let max_size = 300

(* Maximal number of copies of a function *)
let max_clones = 4

let rec iter_closure_blocks blocks pc ~f =
  Code.traverse
    { fold = fold_children }
    (fun pc () ->
      let block = Addr.Map.find pc blocks in
      f block;
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (_, Closure (_, (pc', _), _)) -> iter_closure_blocks blocks pc' ~f
          | _ -> ()))
    pc
    blocks
    ()

let closure_size blocks pc =
  let n = ref 0 in
  iter_closure_blocks blocks pc ~f:(fun block -> n := !n + 1 + List.length block.body);
  !n

(* Indices of the parameters used as a function in the closure body *)
let called_parameters blocks params pc =
  let called = ref Var.Set.empty in
  iter_closure_blocks blocks pc ~f:(fun block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (_, Apply { f; _ }) -> called := Var.Set.add f !called
          | _ -> ()));
  List.mapi params ~f:(fun i x -> i, x)
  |> List.filter_map ~f:(fun (i, x) -> if Var.Set.mem x !called then Some i else None)

(* Whether a function may raise an exception by itself, that is,
   outside of an exception handler *)
let may_raise blocks pc =
  Code.traverse
    { fold = Code.fold_children_skip_try_body }
    (fun pc b ->
      b
      ||
      match (Addr.Map.find pc blocks).branch with
      | Raise _ -> true
      | _ -> false)
    pc
    blocks
    false

let key_equal = List.equal ~eq:(Option.equal Var.equal)

let find_key key l =
  List.find_map l ~f:(fun (k, v) -> if key_equal k key then Some v else None)

type candidate =
  { params : Var.t list
  ; cont : Code.cont
  ; info : Optimization_hint.closure_hint option * Parse_info.t option
  ; positions : int list
  ; size : int
  }

let f p =
  let t = Timer.make () in
  let closures = Var.Hashtbl.create 1024 in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, Closure (params, cont, info)) ->
              Var.Hashtbl.replace closures x (params, cont, info)
          | _ -> ()))
    p.blocks;
  let candidates = Var.Hashtbl.create 16 in
  Var.Hashtbl.iter
    (fun f (params, ((pc, _) as cont), info) ->
      match called_parameters p.blocks params pc with
      | [] -> ()
      | positions ->
          let size = closure_size p.blocks pc in
          if size <= max_size
          then Var.Hashtbl.replace candidates f { params; cont; info; positions; size })
    closures;
  (* The closures known to be passed to the called parameters, if any.
     We only clone when one of them may raise: this is where the
     benefit is the largest, since a direct call makes it possible to
     signal exceptions by returning null in Wasm. Cloning for all known
     functions has a much larger cost in code size and compilation time
     for no significant further speed-up. *)
  let call_key { params; positions; _ } args =
    if List.compare_lengths args params <> 0
    then None
    else
      let key =
        List.map positions ~f:(fun i ->
            let x = List.nth args i in
            if Var.Hashtbl.mem closures x then Some x else None)
      in
      if
        List.exists key ~f:(function
          | Some g ->
              let _, (pc, _), _ = Var.Hashtbl.find closures g in
              may_raise p.blocks pc
          | None -> false)
      then Some key
      else None
  in
  (* Count the uses of each candidate, and the call sites with a key *)
  let uses = Var.Hashtbl.create 16 in
  let keys = Var.Hashtbl.create 16 in
  let bump tbl x =
    Var.Hashtbl.replace tbl x (1 + try Var.Hashtbl.find tbl x with Not_found -> 0)
  in
  Addr.Map.iter
    (fun _ block ->
      Freevars.iter_block_free_vars
        (fun x -> if Var.Hashtbl.mem candidates x then bump uses x)
        block;
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (_, Apply { f; args; _ }) -> (
              match Var.Hashtbl.find_opt candidates f with
              | None -> ()
              | Some c -> (
                  match call_key c args with
                  | None -> ()
                  | Some key ->
                      let l = try Var.Hashtbl.find keys f with Not_found -> [] in
                      let l =
                        match find_key key l with
                        | Some n ->
                            (key, n + 1)
                            :: List.filter l ~f:(fun (k, _) -> not (key_equal k key))
                        | None -> (key, 1) :: l
                      in
                      Var.Hashtbl.replace keys f l))
          | _ -> ()))
    p.blocks;
  (* Uses of a function within its own body do not count: when all
     external uses are calls with the same key, the flow analysis is
     already precise. *)
  let own_uses f { cont = pc, _; _ } =
    let n = ref 0 in
    iter_closure_blocks p.blocks pc ~f:(fun block ->
        Freevars.iter_block_free_vars (fun x -> if Var.equal x f then n := !n + 1) block);
    !n
  in
  let live_vars = Array.make (Var.count ()) 0 in
  let p = ref p in
  let clones = Var.Hashtbl.create 16 in
  let redirect = Var.Hashtbl.create 16 in
  let count = ref 0 in
  Var.Hashtbl.iter
    (fun f l ->
      let c = Var.Hashtbl.find candidates f in
      let keyed_calls = List.fold_left l ~init:0 ~f:(fun n (_, n') -> n + n') in
      let other_uses = Var.Hashtbl.find uses f - own_uses f c - keyed_calls in
      if List.length l >= 2 || other_uses > 0
      then (
        let l =
          List.sort l ~cmp:(fun (_, n) (_, n') -> compare n' n)
          |> List.filteri ~f:(fun i _ -> i < max_clones)
        in
        List.iter l ~f:(fun (key, _) ->
            let p', f', params', cont' =
              Duplicate.closure !p ~f ~params:c.params ~cont:c.cont live_vars
            in
            p := p';
            incr count;
            Var.Hashtbl.add clones f (Let (f', Closure (params', cont', c.info)));
            Var.Hashtbl.add redirect f (key, f'));
        if debug ()
        then
          Format.eprintf
            "clone %a (size %d): %d copies (%d other uses)@."
            Var.print
            f
            c.size
            (List.length l)
            other_uses))
    keys;
  let p = !p in
  let p =
    if Var.Hashtbl.length clones = 0
    then p
    else
      let blocks =
        Addr.Map.map
          (fun block ->
            let body =
              List.concat_map block.body ~f:(fun i ->
                  match i with
                  | Let (x, Apply ({ f; args; _ } as call))
                    when Var.Hashtbl.mem redirect f -> (
                      let c = Var.Hashtbl.find candidates f in
                      match call_key c args with
                      | None -> [ i ]
                      | Some key -> (
                          match find_key key (Var.Hashtbl.find_all redirect f) with
                          | Some f' -> [ Let (x, Apply { call with f = f' }) ]
                          | None -> [ i ]))
                  | Let (f, Closure _) when Var.Hashtbl.mem clones f ->
                      i :: List.rev (Var.Hashtbl.find_all clones f)
                  | _ -> [ i ])
            in
            { block with body })
          p.blocks
      in
      { p with blocks }
  in
  if times () then Format.eprintf "  clone higher-order: %a@." Timer.print t;
  if debug () then Format.eprintf "clone higher-order: %d copies@." !count;
  Code.invariant p;
  p

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

(* Turn local functions into blocks of their enclosing function.

   This is the same optimization as OCaml's [Simplif.simplify_local_functions]
   (ocaml/ocaml#2143), which [ocamlc] only performs without [-g]. A
   function [f] is contified when:
   - it is only used as the callee of full applications;
   - it is not defined in toplevel code;
   - all these applications occur directly in the function defining [f];
   - they all have the same continuation: they are all tail calls, or
     they all jump to the same block with the same arguments, the
     result of the call excepted. A function applied only once is
     always contified.
   The function body then becomes part of the enclosing function: the
   calls are turned into jumps to its entry block, and its [Return]
   instructions are turned into jumps to the continuation. This saves
   the closure allocation and the calls, and makes the body visible to
   intraprocedural optimizations.

   Contifying a function can make the functions it calls contifiable:
   a call located in a contified function [g] is considered to be
   located in the function where [g] is moved, and a tail call in [g]
   gets the continuation of [g]. *)

open! Stdlib
open Code

let times = Debug.find "times"

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

type continuation =
  | Tail
  | Jump of
      { trap : Addr.t option
            (* For a [Poptrap], the [Pushtrap] block of the exception
               handler being removed *)
      ; pc : Addr.t
      ; args : Var.t option list (* [None] stands for the result of the call *)
      }
  | Unique (* Any other continuation; only possible for a single call *)
  | Return_of of Var.t
(* The continuation of the single call to this contified function,
   which had a [Unique] continuation *)

let continuation_equal c c' =
  match c, c' with
  | Tail, Tail -> true
  | Jump { trap; pc; args }, Jump { trap = trap'; pc = pc'; args = args' } ->
      Option.equal ( = ) trap trap'
      && pc = pc'
      && List.equal ~eq:(Option.equal Var.equal) args args'
  | Return_of f, Return_of f' -> Var.equal f f'
  | (Tail | Jump _ | Unique | Return_of _), _ -> false

type call =
  { caller : Var.t option
  ; block : Addr.t
  ; continuation : continuation
  }

type closure =
  { name : Var.t
  ; params : Var.t list
  ; cont : cont
  ; definer : Var.t option
  ; def_block : Addr.t
  ; calls : call list
  ; returns : Addr.t list (* Blocks ending with a [Return] *)
  }

let call_continuation ~uses ~traps x rem branch =
  match rem with
  | [] | [ Event _ ] -> (
      let jump trap (pc, args) =
        if
          (* The result may also be used directly in the blocks
             dominated by the call *)
          List.fold_left args ~init:0 ~f:(fun n y -> if Var.equal x y then n + 1 else n)
          = Var.Tbl.get uses x
        then
          Jump
            { trap
            ; pc
            ; args = List.map ~f:(fun y -> if Var.equal x y then None else Some y) args
            }
        else Unique
      in
      match branch with
      | Return y when Var.equal x y -> Tail
      | Branch cont -> jump None cont
      | Poptrap cont -> jump (Some (List.hd traps)) cont
      | Return _ | Raise _ | Stop | Cond _ | Switch _ | Pushtrap _ -> Unique)
  | _ -> Unique

let rec split_at_call ~f body =
  match body with
  | [] -> None
  | Let (x, Apply { f = f'; args; _ }) :: rem when Var.equal f f' ->
      Some ([], x, args, rem)
  | i :: rem -> (
      match split_at_call ~f rem with
      | Some (before, x, args, rem) -> Some (i :: before, x, args, rem)
      | None -> None)

(* Find the functions which are only used as the callee of full
   applications *)
let analyze p =
  (* Uses in reachable code: the unreachable blocks are removed after
     the rewrite *)
  let uses = Var.Tbl.make () 0 in
  let calls = Var.Hashtbl.create 128 in
  let returns = Var.Hashtbl.create 128 in
  let closures = ref [] in
  let visited = BitSet.create' p.free_pc in
  let on_stack = BitSet.create' p.free_pc in
  let loop_headers = BitSet.create' p.free_pc in
  (* [traps] is the stack of the [Pushtrap] blocks of the enclosing
     exception handlers: calls ending with a [Poptrap] to the same
     block only have the same continuation if they remove the same
     handler. *)
  let rec visit caller traps pc =
    if BitSet.mem visited pc
    then (if BitSet.mem on_stack pc then BitSet.set loop_headers pc)
    else (
      BitSet.set visited pc;
      BitSet.set on_stack pc;
      let block = Addr.Map.find pc p.blocks in
      Freevars.iter_block_free_vars
        (fun x -> Var.Tbl.set uses x (Var.Tbl.get uses x + 1))
        block;
      let rec scan l =
        match l with
        | [] -> ()
        | Let (name, Closure (params, cont, (hint, _))) :: rem ->
            visit (Some name) [] (fst cont);
            (match hint with
            | Some
                { Optimization_hint.inline = Always_inline | Hint_inline | Unroll _; _ }
              ->
                (* As in OCaml, leave functions with an inlining attribute to
                   the inliner *)
                ()
            | Some { inline = Never_inline | Default_inline; _ } | None ->
                closures := (name, params, cont, caller, pc) :: !closures);
            scan rem
        | Let (x, Apply { f; args; _ }) :: rem ->
            Var.Hashtbl.replace
              calls
              f
              (( List.length args
               , caller
               , pc
               , (* All uses must have been counted *)
                 lazy (call_continuation ~uses ~traps x rem block.branch) )
              :: (try Var.Hashtbl.find calls f with Not_found -> []));
            scan rem
        | _ :: rem -> scan rem
      in
      scan block.body;
      (match block.branch with
      | Return _ -> Option.iter caller ~f:(fun f -> Var.Hashtbl.add returns f pc)
      | Raise _ | Stop -> ()
      | Branch (pc', _) -> visit caller traps pc'
      | Poptrap (pc', _) -> visit caller (List.tl traps) pc'
      | Pushtrap ((pc', _), _, (pc_h, _)) ->
          visit caller (pc :: traps) pc';
          visit caller traps pc_h
      | Cond (_, (pc1, _), (pc2, _)) ->
          visit caller traps pc1;
          visit caller traps pc2
      | Switch (_, a) -> Array.iter a ~f:(fun (pc', _) -> visit caller traps pc'));
      BitSet.unset on_stack pc)
  in
  visit None [] p.start;
  ( loop_headers
  , List.filter_map !closures ~f:(fun (name, params, cont, definer, def_block) ->
        match definer, Var.Hashtbl.find_opt calls name with
        | None, _ ->
            (* Toplevel code runs once and is less likely to get optimized
             (V8 does not perform on-stack replacement for Wasm code), so
             we do not move code there, even without loops: further
             inlining could then move loops there. *)
            None
        | _, None -> None
        | Some _, Some l ->
            if
              List.compare_length_with l ~len:(Var.Tbl.get uses name) = 0
              && List.for_all l ~f:(fun (arity, _, _, _) ->
                  List.compare_length_with params ~len:arity = 0)
            then
              Some
                { name
                ; params
                ; cont
                ; definer
                ; def_block
                ; calls =
                    List.map l ~f:(fun (_, caller, block, continuation) ->
                        { caller; block; continuation = Lazy.force continuation })
                ; returns = Var.Hashtbl.find_all returns name
                }
            else None) )

let decide (loop_headers, closures) =
  let closure_info = Var.Hashtbl.create 128 in
  List.iter closures ~f:(fun c -> Var.Hashtbl.replace closure_info c.name c);
  (* Contified functions, with the continuation of their body *)
  let contified = Var.Hashtbl.create 128 in
  let is_contified f = Var.Hashtbl.mem contified f in
  (* Where a call ends up, given the functions contified so far *)
  let rec resolve caller continuation =
    match caller with
    | Some g when is_contified g ->
        resolve
          (Var.Hashtbl.find closure_info g).definer
          (match continuation with
          | Tail -> Var.Hashtbl.find contified g
          | Jump _ | Unique | Return_of _ -> continuation)
    | Some _ | None -> caller, continuation
  in
  let try_contify c =
    let calls =
      List.map c.calls ~f:(fun { caller; continuation; _ } -> resolve caller continuation)
    in
    if List.for_all calls ~f:(fun (caller, _) -> Option.equal Var.equal caller c.definer)
    then
      let continuation =
        match calls with
        | [ (_, continuation) ] -> Some continuation
        | (_, continuation) :: rem ->
            if
              (match continuation with
                | Jump { pc; _ } ->
                    (* The function body would become a second entry
                       point of the loop if some calls are inside the
                       loop and some outside *)
                    not (BitSet.mem loop_headers pc)
                | Tail | Unique | Return_of _ -> true)
              && List.for_all rem ~f:(fun (_, continuation') ->
                  continuation_equal continuation continuation')
            then Some continuation
            else None
        | [] -> assert false
      in
      match continuation with
      | Some continuation ->
          Var.Hashtbl.replace
            contified
            c.name
            (match continuation with
            | Unique -> Return_of c.name
            | Tail | Jump _ | Return_of _ -> continuation);
          true
      | None -> false
    else false
  in
  let rec loop closures =
    let changed = ref false in
    let closures =
      List.filter closures ~f:(fun c ->
          if try_contify c
          then (
            changed := true;
            false)
          else true)
    in
    if !changed then loop closures
  in
  loop closures;
  List.filter_map closures ~f:(fun c ->
      Option.map (Var.Hashtbl.find_opt contified c.name) ~f:(fun continuation ->
          c, continuation))

let rewrite p contified =
  let free_pc = ref p.free_pc in
  let fresh_pc () =
    let pc = !free_pc in
    incr free_pc;
    pc
  in
  let info = Var.Hashtbl.create 16 in
  (* Where the end of a block was moved when splitting it at a call *)
  let moved = Addr.Hashtbl.create 16 in
  List.iter contified ~f:(fun (c, continuation) ->
      Var.Hashtbl.replace info c.name (fresh_pc (), continuation, ref None));
  (* Remove the closure definitions and turn the calls into jumps *)
  let rec rewrite_block pc { params; body; branch } blocks =
    let body =
      List.fold_left body ~init:[] ~f:(fun acc i ->
          match i, acc with
          | Event _, Event _ :: prev ->
              (* Avoid consecutive events (keep just the last one) *)
              i :: prev
          | Let (f, Closure _), _ when Var.Hashtbl.mem info f -> acc
          | _ -> i :: acc)
      |> List.rev
    in
    let call =
      List.find_map body ~f:(function
        | Let (_, Apply { f; _ }) when Var.Hashtbl.mem info f -> Some f
        | _ -> None)
    in
    match call with
    | None -> Addr.Map.add pc { params; body; branch } blocks
    | Some f -> (
        let entry, continuation, return_pc = Var.Hashtbl.find info f in
        match split_at_call ~f body with
        | None -> assert false
        | Some (before, x, args, rem) -> (
            let blocks =
              Addr.Map.add
                pc
                { params; body = before; branch = Branch (entry, args) }
                blocks
            in
            match continuation with
            | Return_of f' when Var.equal f f' ->
                let pc' = fresh_pc () in
                return_pc := Some pc';
                Addr.Hashtbl.replace moved pc pc';
                rewrite_block pc' { params = [ x ]; body = rem; branch } blocks
            | Tail | Jump _ | Return_of _ ->
                (* Only a debug event remains *)
                blocks
            | Unique -> assert false))
  in
  let blocks =
    List.fold_left contified ~init:p.blocks ~f:(fun blocks (c, _) ->
        let blocks =
          List.fold_left
            (c.def_block :: List.map c.calls ~f:(fun { block; _ } -> block))
            ~init:blocks
            ~f:(fun blocks pc -> rewrite_block pc (Addr.Map.find pc blocks) blocks)
        in
        let entry, _, _ = Var.Hashtbl.find info c.name in
        Addr.Map.add entry { params = c.params; body = []; branch = Branch c.cont } blocks)
  in
  (* Replace the [Return] instructions of the contified functions by
     jumps to their continuation. Tail calls to contified functions are
     now jumps: the continuation of these functions has been resolved
     to the one of their caller. *)
  let rec location pc =
    match Addr.Hashtbl.find_opt moved pc with
    | Some pc' -> location pc'
    | None -> pc
  in
  let blocks =
    List.fold_left contified ~init:blocks ~f:(fun blocks (c, continuation) ->
        let rewrite_return =
          match continuation with
          | Tail -> None
          | Jump { trap; pc; args } ->
              Some
                (fun v ->
                  let cont =
                    ( pc
                    , List.map args ~f:(function
                        | None -> v
                        | Some y -> y) )
                  in
                  match trap with
                  | Some _ -> Poptrap cont
                  | None -> Branch cont)
          | Return_of f ->
              let _, _, return_pc = Var.Hashtbl.find info f in
              let pc =
                match !return_pc with
                | Some pc -> pc
                | None -> assert false
              in
              Some (fun v -> Branch (pc, [ v ]))
          | Unique -> assert false
        in
        match rewrite_return with
        | None -> blocks
        | Some rewrite_return ->
            List.fold_left c.returns ~init:blocks ~f:(fun blocks pc ->
                let pc = location pc in
                let block = Addr.Map.find pc blocks in
                match block.branch with
                | Return v ->
                    Addr.Map.add pc { block with branch = rewrite_return v } blocks
                | _ -> blocks))
  in
  { p with blocks; free_pc = !free_pc }

let f p =
  let previous_p = p in
  let t = Timer.make () in
  let contified = decide (analyze p) in
  let count = List.length contified in
  let p =
    if count = 0
    then p
    else
      (* The continuation is unreachable if the function never returns *)
      Deadcode.remove_unused_blocks (rewrite p contified)
  in
  if times () then Format.eprintf "  contification: %a@." Timer.print t;
  if stats () then Format.eprintf "Stats - contification: %d functions@." count;
  if debug_stats () then Code.check_updates ~name:"contify" previous_p p ~updates:count;
  p

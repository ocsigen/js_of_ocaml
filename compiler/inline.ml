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

open Code

let optimizable blocks pc acc =
  Code.traverse Code.fold_children (fun pc acc ->
    if not acc
    then acc
    else
      let b = AddrMap.find pc blocks in
      match b with
      | {handler = Some _}
      | {branch = Pushtrap _ }
      | {branch = Poptrap _ } -> false
      | _ ->
        List.for_all (function
          | Let (_, Prim (Extern "caml_js_eval_string",_)) -> false
          | Let (_, Prim (Extern "debugger",_)) -> false
          | Let
              (x,
               Prim(Extern
                      ("caml_js_var"
                      |"caml_js_expr"
                      |"caml_pure_js_expr"),_)) ->
            (* TODO: we should smarter here and look the generated js *)
            (* let's consider it this opmiziable *)
            true
          | _ -> true
        ) b.body )  pc blocks true

let get_closures (_, blocks, _) =
  AddrMap.fold
    (fun _ block closures ->
       List.fold_left
         (fun closures i ->
            match i with
              Let (x, Closure (l, cont)) ->
              (* we can compute this once during the pass
                 as the property won't change with inlining *)
              let f_optimizable = optimizable blocks (fst cont) true in
              VarMap.add x (l, cont, f_optimizable) closures
            | _ ->
                closures)
         closures block.body)
    blocks VarMap.empty

(****)

let rewrite_block (pc', handler) pc blocks =
  let block = AddrMap.find pc blocks in
  assert (block.handler = None);
  let block = { block with handler = handler } in
  let block =
    match block.branch, pc' with
      Return y, Some pc' -> { block with branch = Branch (pc', [y]) }
    | _                  -> block
  in
  AddrMap.add pc block blocks

let (>>) x f = f x

(* Skip try body *)
let fold_children blocks pc f accu =
  let block = AddrMap.find pc blocks in
  match block.branch with
    Return _ | Raise _ | Stop ->
      accu
  | Branch (pc', _) | Poptrap (pc', _) ->
      f pc' accu
  | Pushtrap (_, _, (pc1, _), pc2) ->
      f pc1 (if pc2 >= 0 then f pc2 accu else accu)
  | Cond (_, _, (pc1, _), (pc2, _)) ->
      accu >> f pc1 >> f pc2
  | Switch (_, a1, a2) ->
      accu >> Array.fold_right (fun (pc, _) accu -> f pc accu) a1
           >> Array.fold_right (fun (pc, _) accu -> f pc accu) a2

let rewrite_closure blocks cont_pc clos_pc handler =
  Code.traverse fold_children (rewrite_block (cont_pc, handler)) clos_pc blocks blocks

(****)

(*
get new location
put continuation at new location
update closure body to return to this location
make current block continuation jump to closure body
*)

let inline closures live_vars outer_optimizable pc (blocks,free_pc)=
  let block = AddrMap.find pc blocks in
  let (body, (branch, blocks, free_pc)) =
    List.fold_right
      (fun i (rem, state) ->
         match i with
           Let (x, Apply (f, args, true))
               when live_vars.(Var.idx f) = 1
                 && VarMap.mem f closures ->
             let (params, (clos_pc, clos_args),f_optimizable) = VarMap.find f closures in
             (* inlining the code of an optimizable function could make
                this code unoptimized. (wrt to Jit compilers)
                At the moment, V8 doesn't optimize function containing try..catch.
                We disable inlining if the inner and outer funcitons don't have
                the same "contain_try_catch" property *)
             if outer_optimizable = f_optimizable
             then
               let (branch, blocks, free_pc) = state in
               let (blocks, cont_pc) =
                 match rem, branch with
                   [], Return y when Var.compare x y = 0 ->
                   (* We do not need a continuation block for tail calls *)
                   (blocks, None)
                 | _ ->
                   (AddrMap.add free_pc
                      { params = [x]; handler = block.handler;
                        body = rem; branch = branch } blocks,
                    Some free_pc)
               in
               let blocks =
                 rewrite_closure blocks cont_pc clos_pc block.handler in
               (* We do not really need this intermediate block.  It
                  just avoid the need to find which function parameters
                  are used in the function body. *)
               let blocks =
                 AddrMap.add (free_pc + 1)
                   { params = params; handler = block.handler;
                     body = []; branch = Branch (clos_pc, clos_args) } blocks
               in
               ([], (Branch (free_pc + 1, args), blocks, free_pc + 2))
             else begin
               (* Format.eprintf "Do not inline because inner:%b outer:%b@." f_has_handler outer_has_handler; *)
               (i :: rem, state)
             end
         | _ ->
             (i :: rem, state))
      block.body ([], (block.branch, blocks, free_pc))
  in
  (AddrMap.add pc {block with body = body; branch = branch} blocks, free_pc)

(****)

let times = Option.Debug.find "times"
let f ((pc, blocks, free_pc) as p) live_vars =
  let t = Util.Timer.make () in
  let closures = get_closures p in
  let (blocks, free_pc) =
    Code.fold_closures p (fun name _ (pc,_) (blocks,free_pc) ->
      let outer_optimizable = match name with
        | None -> optimizable blocks pc true
        | Some x -> let _,_,b = VarMap.find x closures in
          b in
      Code.traverse Code.fold_children (inline closures live_vars outer_optimizable) pc blocks (blocks,free_pc)
    ) (blocks, free_pc)
  in
  if times () then Format.eprintf "  inlining: %a@." Util.Timer.print t;
  (pc, blocks, free_pc)

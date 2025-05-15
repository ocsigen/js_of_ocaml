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

(*
The code transformation performed to deal with effect handlers produce
deeply nested functions, which is not supported by JavaScript engines.
So, we lift some functions to a lower level to limit the nesting
depth. To lift a function f, we basically wrapped it in a function f'
taking as parameter all free variables of f and returning f. Then we
can move f' to a lower level and replace the definition of f by an
application of f' to the actual value of the free variables. For
instance, this piece of code:

   function g () {
     var x = e
     function f (y) {
       return (x + y)
     }
     ...
   }

is turned into:

   function f'(x) {
     return function f (y) {
       return (x + y)
     }
   }
   function g () {
     var x = e
     var f = f'(x)
   }

Lambda-lifing has a quadratic space complexity, so we try not to lift
too many functions: we lift functions only starting at a given depth
threshold and when they themselves contains nested functions. We also
only lift functions that are isolated, so as not having to deal with
mutually recursive functions.

This implementation is doing the job: the nesting depth remains low
enough for the JavaScript engines and few functions are lifted.
However, on some large code, we can generate functions with thousands
of parameters. We might be able to improve on that by not lifting
functions too much, so that most of their free variables remain
directly accessible. A complimentary approach would be that when we
lift two functions which are initially within one another, we could
put them into nested wrapper functions. Then, the inner wrapper would
only need to deal with free variables from the inner function which
are not bound in the outer function.
*)

open! Stdlib
open Code

let debug = Debug.find "lifting"

let rec compute_depth program pc =
  Code.preorder_traverse
    { fold = Code.fold_children }
    (fun pc d ->
      let block = Code.Addr.Map.find pc program.blocks in
      List.fold_left block.body ~init:d ~f:(fun d i ->
          match i with
          | Let (_, Closure (_, (pc', _), _)) ->
              let d' = compute_depth program pc' in
              max d (d' + 1)
          | _ -> d))
    pc
    program.blocks
    0

let collect_free_vars program var_depth depth pc =
  let vars = ref Var.Set.empty in
  let baseline = Config.Param.lambda_lifting_baseline () in
  let rec traverse pc =
    Code.preorder_traverse
      { fold = Code.fold_children }
      (fun pc () ->
        let block = Code.Addr.Map.find pc program.blocks in
        Freevars.iter_block_free_vars
          (fun x ->
            let idx = Var.idx x in
            if idx < Array.length var_depth
            then (
              let d = var_depth.(idx) in
              assert (d >= 0);
              if d > baseline && d < depth then vars := Var.Set.add x !vars))
          block;
        List.iter block.body ~f:(fun i ->
            match i with
            | Let (_, Closure (_, (pc', _), _)) -> traverse pc'
            | _ -> ()))
      pc
      program.blocks
      ()
  in
  traverse pc;
  !vars

let mark_bound_variables var_depth block depth =
  Freevars.iter_block_bound_vars (fun x -> var_depth.(Var.idx x) <- depth) block;
  List.iter block.body ~f:(fun i ->
      match i with
      | Let (_, Closure (params, _, _)) ->
          List.iter params ~f:(fun x -> var_depth.(Var.idx x) <- depth + 1)
      | _ -> ())

let rec traverse var_depth (program, functions) pc depth limit =
  let baseline = Config.Param.lambda_lifting_baseline () in
  Code.preorder_traverse
    { fold = Code.fold_children }
    (fun pc (program, functions) ->
      let block = Code.Addr.Map.find pc program.blocks in
      mark_bound_variables var_depth block depth;
      if depth = baseline
      then (
        assert (List.is_empty functions);
        let program, body =
          List.fold_right block.body ~init:(program, []) ~f:(fun i (program, rem) ->
              match i with
              | Let (_, Closure (_, (pc', _), _)) as i ->
                  let program, functions =
                    traverse var_depth (program, []) pc' (depth + 1) limit
                  in
                  program, List.rev_append functions (i :: rem)
              | i -> program, i :: rem)
        in
        { program with blocks = Addr.Map.add pc { block with body } program.blocks }, [])
      else if depth < limit
      then
        List.fold_left block.body ~init:(program, functions) ~f:(fun st i ->
            match i with
            | Let (_, Closure (_, (pc', _), _)) ->
                traverse var_depth st pc' (depth + 1) limit
            | _ -> st)
      else
        (* We lift isolated closures (so that we do not have to deal
           with mutual recursion) which are deep enough and contain
           deeply nested closures. *)
        let does_not_start_with_closure l =
          match l with
          | Let (_, Closure _) :: _ -> false
          | _ -> true
        in
        let rec rewrite_body first st l =
          match l with
          | (Let (f, (Closure (_, (pc', _), _) as cl)) as i) :: rem
            when first && does_not_start_with_closure rem ->
              let threshold = Config.Param.lambda_lifting_threshold () in
              let program, functions =
                traverse var_depth st pc' (depth + 1) (depth + threshold)
              in
              if compute_depth program pc' + 1 >= threshold
              then (
                let free_vars = collect_free_vars program var_depth (depth + 1) pc' in
                let s =
                  Var.Set.fold
                    (fun x m -> Var.Map.add x (Var.fork x) m)
                    free_vars
                    Var.Map.empty
                in
                let program =
                  Subst.Excluding_Binders.cont (Subst.from_map s) pc' program
                in
                let f' = try Var.Map.find f s with Not_found -> Var.fork f in
                let s = Var.Map.bindings (Var.Map.remove f s) in
                let f'' = Var.fork f in
                if debug ()
                then
                  Format.eprintf
                    "LIFT %a (depth:%d free_vars:%d inner_depth:%d)@."
                    Code.Var.print
                    f''
                    depth
                    (Var.Set.cardinal free_vars)
                    (compute_depth program pc');
                let pc'' = program.free_pc in
                let bl = { params = []; body = [ Let (f', cl) ]; branch = Return f' } in
                let program =
                  { program with
                    free_pc = pc'' + 1
                  ; blocks = Addr.Map.add pc'' bl program.blocks
                  }
                in
                let functions =
                  Let (f'', Closure (List.map s ~f:snd, (pc'', []), None)) :: functions
                in
                let rem', st = rewrite_body false (program, functions) rem in
                ( Let (f, Apply { f = f''; args = List.map ~f:fst s; exact = true })
                  :: rem'
                , st ))
              else
                let rem', st = rewrite_body false (program, functions) rem in
                i :: rem', st
          | (Let (_, Closure (_, (pc', _), _)) as i) :: rem ->
              let st = traverse var_depth st pc' (depth + 1) limit in
              let rem', st = rewrite_body false st rem in
              i :: rem', st
          | i :: rem ->
              let rem', st = rewrite_body (does_not_start_with_closure l) st rem in
              i :: rem', st
          | [] -> [], st
        in
        let body, (program, functions) =
          rewrite_body true (program, functions) block.body
        in
        ( { program with blocks = Addr.Map.add pc { block with body } program.blocks }
        , functions ))
    pc
    program.blocks
    (program, functions)

let f program =
  let t = Timer.make () in
  let nv = Var.count () in
  let var_depth = Array.make nv (-1) in
  let program, functions =
    let threshold = Config.Param.lambda_lifting_threshold () in
    let baseline = Config.Param.lambda_lifting_baseline () in
    traverse var_depth (program, []) program.start 0 (baseline + threshold)
  in
  assert (List.is_empty functions);
  if Debug.find "times" () then Format.eprintf "  lambda lifting: %a@." Timer.print t;
  program

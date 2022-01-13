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
open Code

type prop =
  { size : int
  ; try_catch : bool
  ; optimizable : bool
  }

let optimizable blocks pc _ =
  Code.traverse
    { fold = Code.fold_children }
    (fun pc { size; try_catch; optimizable } ->
      let b = Addr.Map.find pc blocks in
      let this_size =
        match b with
        | { branch; body; _ } -> (
            List.length body
            +
            match branch with
            | Cond _ -> 2
            | Switch (_, a1, a2) -> Array.length a1 + Array.length a2
            | _ -> 0)
      in
      let try_catch =
        try_catch
        ||
        match b with
        | { handler = Some _; _ } | { branch = Pushtrap _; _ } | { branch = Poptrap _; _ }
          -> true
        | _ -> false
      in
      let optimizable =
        optimizable
        || List.for_all b.body ~f:(function
               | Let (_, Prim (Extern "caml_js_eval_string", _)) -> false
               | Let (_, Prim (Extern "debugger", _)) -> false
               | Let
                   ( _
                   , Prim
                       (Extern ("caml_js_var" | "caml_js_expr" | "caml_pure_js_expr"), _)
                   ) ->
                   (* TODO: we should be smarter here and look the generated js *)
                   (* let's consider it this opmiziable *)
                   true
               | _ -> true)
      in
      { try_catch; optimizable; size = size + this_size })
    pc
    blocks
    { try_catch = false; optimizable = true; size = 0 }

let rec follow_branch_rec seen blocks = function
  | (pc, []) as k -> (
      let seen = Addr.Set.add pc seen in
      try
        match Addr.Map.find pc blocks with
        | { body = []; branch = Branch (pc, []); _ } when not (Addr.Set.mem pc seen) ->
            follow_branch_rec seen blocks (pc, [])
        | _ -> k
      with Not_found -> k)
  | k -> k

let follow_branch = follow_branch_rec Addr.Set.empty

let get_closures { blocks; _ } =
  Addr.Map.fold
    (fun _ block closures ->
      List.fold_left block.body ~init:closures ~f:(fun closures i ->
          match i with
          | Let (x, Closure (l, cont)) ->
              let cont = follow_branch blocks cont in
              (* we can compute this once during the pass
                 as the property won't change with inlining *)
              let f_optimizable = optimizable blocks (fst cont) true in
              Var.Map.add x (l, cont, f_optimizable) closures
          | _ -> closures))
    blocks
    Var.Map.empty

(****)

let rewrite_block (pc', handler) pc blocks =
  let block = Addr.Map.find pc blocks in
  assert (Option.is_none block.handler);
  let block = { block with handler } in
  let block =
    match block.branch, pc' with
    | Return y, Some pc' -> { block with branch = Branch (pc', [ y ]) }
    | _ -> block
  in
  Addr.Map.add pc block blocks

(* Skip try body *)
let fold_children blocks pc f accu =
  let block = Addr.Map.find pc blocks in
  match block.branch with
  | Return _ | Raise _ | Stop -> accu
  | Branch (pc', _) | Poptrap ((pc', _), _) -> f pc' accu
  | Pushtrap (_, _, (pc1, _), pcs) -> f pc1 (Addr.Set.fold f pcs accu)
  | Cond (_, (pc1, _), (pc2, _)) ->
      let accu = f pc1 accu in
      let accu = f pc2 accu in
      accu
  | Switch (_, a1, a2) ->
      let accu = Array.fold_right a1 ~init:accu ~f:(fun (pc, _) accu -> f pc accu) in
      let accu = Array.fold_right a2 ~init:accu ~f:(fun (pc, _) accu -> f pc accu) in
      accu

let rewrite_closure blocks cont_pc clos_pc handler =
  Code.traverse
    { fold = fold_children }
    (rewrite_block (cont_pc, handler))
    clos_pc
    blocks
    blocks

(****)

let rec find_mapping mapping x =
  match mapping with
  | [] -> x
  | ([], []) :: rest -> find_mapping rest x
  | (a :: _, b :: _) :: rest when Code.Var.compare a x = 0 -> find_mapping rest b
  | (_ :: ax, _ :: bx) :: rest -> find_mapping ((ax, bx) :: rest) x
  | ([], _ | _, []) :: _ -> assert false

let simple blocks cont mapping =
  let map_var mapping x =
    let x' = find_mapping mapping x in
    if Var.equal x x' then raise Not_found else x'
  in
  let map_prim_arg mapping = function
    | Pc c -> Pc c
    | Pv x -> Pv (map_var mapping x)
  in
  let rec follow seen (pc, args) (instr : [ `Empty | `Ok of 'a ]) mapping =
    if Addr.Set.mem pc seen
    then `Fail
    else
      let b = Addr.Map.find pc blocks in
      let mapping = (b.params, args) :: mapping in
      let instr : [ `Empty | `Ok of 'a | `Fail ] =
        match b.body, instr with
        | [], _ -> (instr :> [ `Empty | `Ok of 'a | `Fail ])
        | [ Let (y, exp) ], `Empty -> `Ok (y, exp)
        | _, _ -> `Fail
      in
      match instr, b.branch with
      | `Fail, _ -> `Fail
      | `Empty, Return ret -> `Alias (map_var mapping ret)
      | `Ok (x, exp), Return ret when Code.Var.compare x (find_mapping mapping ret) = 0
        -> (
          match exp with
          | Constant (Float _ | Int64 _ | Int _ | NativeString _) -> `Exp exp
          | Apply (f, args, true) ->
              `Exp (Apply (map_var mapping f, List.map args ~f:(map_var mapping), true))
          | Prim (prim, args) ->
              `Exp (Prim (prim, List.map args ~f:(map_prim_arg mapping)))
          | Block (tag, args, aon) ->
              `Exp (Block (tag, Array.map args ~f:(map_var mapping), aon))
          | Field (x, i) -> `Exp (Field (map_var mapping x, i))
          | Closure _ -> `Fail
          | Constant _ -> `Fail
          | Apply _ -> `Fail)
      | ((`Empty | `Ok _) as instr), Branch cont ->
          follow (Addr.Set.add pc seen) cont instr mapping
      | (`Empty | `Ok _), _ -> `Fail
  in
  try follow Addr.Set.empty cont `Empty mapping with Not_found -> `Fail

let rec args_equal xs ys =
  match xs, ys with
  | [], [] -> true
  | x :: xs, Pv y :: ys -> Code.Var.compare x y = 0 && args_equal xs ys
  | _ -> false

let inline live_vars closures pc (outer, blocks, free_pc) =
  let block = Addr.Map.find pc blocks in
  let body, (outer, branch, blocks, free_pc) =
    List.fold_right
      block.body
      ~init:([], (outer, block.branch, blocks, free_pc))
      ~f:(fun i (rem, state) ->
        match i with
        | Let (x, Apply (f, args, true)) when Var.Map.mem f closures -> (
            let outer, branch, blocks, free_pc = state in
            let ( params
                , clos_cont
                , { size = f_size; optimizable = f_optimizable; try_catch = f_try_catch }
                ) =
              Var.Map.find f closures
            in
            match simple blocks clos_cont [ params, args ] with
            | `Alias arg -> (
                match rem, branch with
                | [], Return y when Var.compare x y = 0 ->
                    [], (outer, Return arg, blocks, free_pc)
                | _ ->
                    let blocks =
                      Addr.Map.add
                        free_pc
                        { params = [ x ]; handler = block.handler; body = rem; branch }
                        blocks
                    in
                    [], (outer, Branch (free_pc, [ arg ]), blocks, free_pc + 1))
            | `Exp exp -> Let (x, exp) :: rem, state
            | `Fail ->
                if live_vars.(Var.idx f) = 1
                   && Bool.equal outer.optimizable f_optimizable
                   && Bool.equal outer.try_catch f_try_catch
                   && f_size < Config.Param.inlining_limit ()
                   (* Inlining the code of an optimizable function could
                      make this code unoptimized. (wrt to Jit compilers)

                      At the moment, V8 doesn't optimize function
                      containing try..catch.  We disable inlining if the
                      inner and outer functions don't have the same
                      "try_catch" property *)
                then
                  let blocks, cont_pc =
                    match rem, branch with
                    | [], Return y when Var.compare x y = 0 ->
                        (* We do not need a continuation block for tail calls *)
                        blocks, None
                    | _ ->
                        ( Addr.Map.add
                            free_pc
                            { params = [ x ]
                            ; handler = block.handler
                            ; body = rem
                            ; branch
                            }
                            blocks
                        , Some free_pc )
                  in
                  let blocks =
                    rewrite_closure blocks cont_pc (fst clos_cont) block.handler
                  in
                  (* We do not really need this intermediate block.
                     It just avoids the need to find which function
                     parameters are used in the function body. *)
                  let blocks =
                    Addr.Map.add
                      (free_pc + 1)
                      { params
                      ; handler = block.handler
                      ; body = []
                      ; branch = Branch clos_cont
                      }
                      blocks
                  in
                  let outer = { outer with size = outer.size + f_size } in
                  [], (outer, Branch (free_pc + 1, args), blocks, free_pc + 2)
                else i :: rem, state)
        | Let (x, Closure (l, (pc, []))) -> (
            let block = Addr.Map.find pc blocks in
            match block with
            | { body = [ Let (y, Prim (Extern prim, args)) ]
              ; branch = Return y'
              ; handler = None
              ; params = []
              } ->
                let len = List.length l in
                if Code.Var.compare y y' = 0
                   && Primitive.has_arity prim len
                   && args_equal l args
                then
                  ( Let (x, Prim (Extern "%closure", [ Pc (NativeString prim) ])) :: rem
                  , state )
                else i :: rem, state
            | _ -> i :: rem, state)
        | _ -> i :: rem, state)
  in
  outer, Addr.Map.add pc { block with body; branch } blocks, free_pc

(****)

let times = Debug.find "times"

let f p live_vars =
  Code.invariant p;
  let t = Timer.make () in
  let closures = get_closures p in
  let _closures, blocks, free_pc =
    Code.fold_closures
      p
      (fun name _ (pc, _) (closures, blocks, free_pc) ->
        let traverse outer =
          Code.traverse
            { fold = Code.fold_children }
            (inline live_vars closures)
            pc
            blocks
            (outer, blocks, free_pc)
        in
        match name with
        | None ->
            let _, blocks, free_pc = traverse (optimizable blocks pc true) in
            closures, blocks, free_pc
        | Some x ->
            let l, c, outer = Var.Map.find x closures in
            let outer, blocks, free_pc = traverse outer in
            let closures = Var.Map.add x (l, c, outer) closures in
            closures, blocks, free_pc)
      (closures, p.blocks, p.free_pc)
  in
  if times () then Format.eprintf "  inlining: %a@." Timer.print t;
  let p = { p with blocks; free_pc } in
  Code.invariant p;
  p

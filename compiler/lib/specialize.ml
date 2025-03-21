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
open Flow

let function_arity info x =
  let rec arity info x acc =
    get_approx
      info
      (fun x ->
        match Flow.Info.def info x with
        | Some (Closure (l, _)) -> Some (List.length l)
        | Some (Special (Alias_prim prim)) -> (
            try Some (Primitive.arity prim) with Not_found -> None)
        | Some (Apply { f; args; _ }) -> (
            if List.mem f ~set:acc
            then None
            else
              match arity info f (f :: acc) with
              | Some n ->
                  let diff = n - List.length args in
                  if diff > 0 then Some diff else None
              | None -> None)
        | _ -> None)
      None
      (fun u v ->
        match u, v with
        | Some n, Some m when n = m -> u
        | _ -> None)
      x
  in
  arity info x []

let add_event loc instrs =
  match loc with
  | Some loc -> Event loc :: instrs
  | None -> instrs

let specialize_instr function_arity ((acc, free_pc, extra), loc) i =
  match i with
  | Let (x, Apply { f; args; exact = false }) when Config.Flag.optcall () -> (
      let n' = List.length args in
      match function_arity f with
      | None -> i :: acc, free_pc, extra
      | Some n when n = n' ->
          Let (x, Apply { f; args; exact = true }) :: acc, free_pc, extra
      | Some n when n < n' ->
          let v = Code.Var.fresh () in
          let args, rest = List.take n args in
          ( (* Reversed *)
            Let (x, Apply { f = v; args = rest; exact = false })
            :: add_event loc (Let (v, Apply { f; args; exact = true }) :: acc)
          , free_pc
          , extra )
      | Some n when n > n' ->
          let missing = Array.init (n - n') ~f:(fun _ -> Code.Var.fresh ()) in
          let missing = Array.to_list missing in
          let block =
            let params' = Array.init (n - n') ~f:(fun _ -> Code.Var.fresh ()) in
            let params' = Array.to_list params' in
            let return' = Code.Var.fresh () in
            { params = params'
            ; body =
                add_event
                  loc
                  [ Let (return', Apply { f; args = args @ params'; exact = true }) ]
            ; branch = Return return'
            }
          in
          ( Let (x, Closure (missing, (free_pc, missing))) :: acc
          , free_pc + 1
          , (free_pc, block) :: extra )
      | _ -> i :: acc, free_pc, extra)
  | _ -> i :: acc, free_pc, extra

let specialize_instrs ~function_arity p =
  let blocks, free_pc =
    Addr.Map.fold
      (fun pc block (blocks, free_pc) ->
        let (body, free_pc, extra), _ =
          List.fold_left
            block.body
            ~init:(([], free_pc, []), None)
            ~f:(fun acc i ->
              match i with
              | Event loc ->
                  let (body, free_pc, extra), _ = acc in
                  (i :: body, free_pc, extra), Some loc
              | _ -> specialize_instr function_arity acc i, None)
        in
        let blocks =
          List.fold_left extra ~init:blocks ~f:(fun blocks (pc, b) ->
              Addr.Map.add pc b blocks)
        in
        Addr.Map.add pc { block with Code.body = List.rev body } blocks, free_pc)
      p.blocks
      (Addr.Map.empty, p.free_pc)
  in
  { p with blocks; free_pc }

let f = specialize_instrs

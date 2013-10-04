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
open Flow

let function_cardinality info x =
  get_approx info
    (fun x ->
       match info.info_defs.(Var.idx x) with
         Expr (Closure (l, _)) -> Some (List.length l)
       | _                     -> None)
    None
    (fun u v -> match u, v with Some n, Some m when n = m -> u | _ -> None)
    x

let specialize_instr info i =
  match i with
    | Let (x, Apply (f, l, _)) when Option.Optim.optcall () ->
      Let (x, Apply (f, l, function_cardinality info f))
    | _ ->
      i

let specialize_instrs info (pc, blocks, free_pc) =
  let blocks =
    AddrMap.map
      (fun block ->
         { block with Code.body =
             List.map (fun i -> specialize_instr info i) block.body })
      blocks
  in
  (pc, blocks, free_pc)

let f p info =
  let p = specialize_instrs info p in
  p

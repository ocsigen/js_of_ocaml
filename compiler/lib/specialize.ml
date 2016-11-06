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

let rec function_cardinality info x acc =
  get_approx info
    (fun x ->
       match info.info_defs.(Var.idx x) with
       | Expr (Closure (l, _)) ->
           Some (List.length l)
       | Expr (Prim (Extern "%closure", [Pc (IString prim)])) ->
         (try Some (Jsoo_primitive.arity prim) with Not_found -> None)
       | Expr (Apply (f, l, _)) ->
         if List.mem f acc
         then None
         else begin match function_cardinality info f (f::acc) with
             Some n ->
             let diff = n - List.length l in
             if diff > 0 then Some diff else None
           | None ->
             None
         end
       | _ ->
         None)
    None
    (fun u v -> match u, v with Some n, Some m when n = m -> u | _ -> None)
    x

let specialize_instr info (acc,free_pc,extra) i =
  match i with
    | Let (x, Apply (f, l, _)) when Option.Optim.optcall () -> begin
      let n' = List.length l in
      match function_cardinality info f [] with
        | None -> i::acc,free_pc,extra
        | Some n when n = n' -> Let (x, Apply (f, l, true))::acc,free_pc,extra
        | Some n when n < n' ->
          let v = Code.Var.fresh () in
          let args,rest = Util.take n l in
          (Let(v, Apply(f,args,true)))
          ::(Let(x,Apply(v,rest,false)))
          ::acc,free_pc,extra
        | Some n when n > n' ->
          let missing = Array.init (n - n') (fun _ -> Code.Var.fresh ()) in
          let missing = Array.to_list missing in
          let block =
            let params' = Array.init (n - n') (fun _ -> Code.Var.fresh ()) in
            let params' = Array.to_list params' in
            let return' = Code.Var.fresh () in
            { params=params';
              body = [Let(return',Apply(f,l@params',true))];
              branch = Return return';
              handler = None;
            } in
          Let(x, Closure(missing,(free_pc,missing)))::acc,(free_pc + 1),(free_pc,block)::extra
        | _ -> i::acc, free_pc,extra
    end
    | _ ->
      i::acc,free_pc,extra

let specialize_instrs info (pc, blocks, free_pc) =
  let blocks,free_pc =
    AddrMap.fold
      (fun pc block (blocks,free_pc) ->
        let body,free_pc,extra =
          List.fold_right (fun i acc -> specialize_instr info acc i) block.body ([],free_pc,[]) in
        let blocks = List.fold_left (fun blocks (pc,b) -> AddrMap.add pc b blocks) blocks extra in
        (AddrMap.add pc { block with Code.body = body } blocks),free_pc)
      blocks (AddrMap.empty,free_pc)
  in
  (pc, blocks, free_pc)

let f info p = specialize_instrs info p

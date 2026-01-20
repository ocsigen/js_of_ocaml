(* Wasm_of_ocaml compiler
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

let debug = Debug.find "call-graph"

let times = Debug.find "times"

let get_approx info x =
  (* Specialization can add some variables *)
  if Var.idx x < Var.Tbl.length info.Global_flow.info_approximation
  then Var.Tbl.get info.Global_flow.info_approximation x
  else Top

let block_deps ~info ~non_escaping ~ambiguous ~blocks pc =
  let block = Addr.Map.find pc blocks in
  List.iter block.body ~f:(fun i ->
      match i with
      | Let (_, Apply { f; exact; _ }) -> (
          match get_approx info f with
          | Top -> ()
          | Values { known; others } ->
              if (not exact) || others || Var.Set.cardinal known > 1
              then Var.Set.iter (fun x -> Var.Hashtbl.replace ambiguous x ()) known;
              if debug ()
              then
                Format.eprintf "CALL others:%b known:%d@." others (Var.Set.cardinal known)
          )
      | Let (x, Closure _) -> (
          match get_approx info x with
          | Top -> ()
          | Values { known; others } ->
              if Var.Set.cardinal known = 1 && (not others) && Var.Set.mem x known
              then (
                let may_escape = Var.ISet.mem info.Global_flow.info_may_escape x in
                if debug () then Format.eprintf "CLOSURE may-escape:%b@." may_escape;
                if not may_escape then Var.Hashtbl.replace non_escaping x ()))
      | Let (_, (Prim _ | Block _ | Constant _ | Field _ | Special _))
      | Event _ | Assign _ | Set_field _ | Offset_ref _ | Array_set _ -> ())

type t = { unambiguous_non_escaping : unit Var.Hashtbl.t }

let direct_calls_only info f =
  Config.Flag.optcall () && Var.Hashtbl.mem info.unambiguous_non_escaping f

let f p info =
  let t = Timer.make () in
  let non_escaping = Var.Hashtbl.create 128 in
  let ambiguous = Var.Hashtbl.create 128 in
  fold_closures
    p
    (fun _ _ (pc, _) _ () ->
      traverse
        { fold = Code.fold_children }
        (fun pc () -> block_deps ~info ~non_escaping ~ambiguous ~blocks:p.blocks pc)
        pc
        p.blocks
        ())
    ();
  if debug ()
  then Format.eprintf "SUMMARY non-escaping:%d" (Var.Hashtbl.length non_escaping);
  Var.Hashtbl.iter (fun x () -> Var.Hashtbl.remove non_escaping x) ambiguous;
  if debug ()
  then Format.eprintf " unambiguous-non-escaping:%d@." (Var.Hashtbl.length non_escaping);
  if times () then Format.eprintf "  call graph analysis: %a@." Timer.print t;
  { unambiguous_non_escaping = non_escaping }

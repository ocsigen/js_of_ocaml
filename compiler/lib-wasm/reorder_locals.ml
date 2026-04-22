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
module W = Wasm_ast
module Var = Code.Var

(* --------------------------------------------------------------------- *)
(*  Use counting                                                         *)
(* --------------------------------------------------------------------- *)

let bump counts x =
  match Var.Hashtbl.find_opt counts x with
  | Some n -> Var.Hashtbl.replace counts x (n + 1)
  | None -> ()
(* Parameters are absent from [counts], so their reads go uncounted. *)

let rec count_expr counts (e : W.expression) =
  match e with
  | Const _ | GlobalGet _ | RefFunc _ | RefNull _ | Pop _ -> ()
  | LocalGet x -> bump counts x
  | LocalTee (x, e') ->
      bump counts x;
      count_expr counts e'
  | UnOp (_, e')
  | I32WrapI64 e'
  | I64ExtendI32 (_, e')
  | F32DemoteF64 e'
  | F64PromoteF32 e'
  | RefI31 e'
  | I31Get (_, e')
  | ArrayLen e'
  | StructGet (_, _, _, e')
  | RefCast (_, e')
  | RefTest (_, e')
  | Br_on_cast (_, _, _, e')
  | Br_on_cast_fail (_, _, _, e')
  | Br_on_null (_, e')
  | ExternConvertAny e'
  | AnyConvertExtern e' -> count_expr counts e'
  | BinOp (_, e1, e2)
  | ArrayNew (_, e1, e2)
  | ArrayNewData (_, _, e1, e2)
  | ArrayGet (_, _, e1, e2)
  | RefEq (e1, e2) ->
      count_expr counts e1;
      count_expr counts e2
  | Call (_, l) | ArrayNewFixed (_, l) | StructNew (_, l) ->
      List.iter l ~f:(count_expr counts)
  | Call_ref (_, f, l) ->
      count_expr counts f;
      List.iter l ~f:(count_expr counts)
  | IfExpr (_, c, t, el) ->
      count_expr counts c;
      count_expr counts t;
      count_expr counts el
  | BlockExpr (_, l) -> count_instrs counts l
  | Seq (l, e') ->
      count_instrs counts l;
      count_expr counts e'
  | Try (_, body, _) -> count_instrs counts body

and count_instr counts (i : W.instruction) =
  match i with
  | Nop | Event _ | Br (_, None) | Return None | Rethrow _ | Unreachable -> ()
  | Drop e
  | Push e
  | GlobalSet (_, e)
  | Br (_, Some e)
  | Br_if (_, e)
  | Br_table (e, _, _)
  | Throw (_, e)
  | Return (Some e) -> count_expr counts e
  | LocalSet (x, e) ->
      bump counts x;
      count_expr counts e
  | StructSet (_, _, e1, e2) ->
      count_expr counts e1;
      count_expr counts e2
  | ArraySet (_, e1, e2, e3) ->
      count_expr counts e1;
      count_expr counts e2;
      count_expr counts e3
  | CallInstr (_, l) | Return_call (_, l) -> List.iter l ~f:(count_expr counts)
  | Return_call_ref (_, f, l) ->
      count_expr counts f;
      List.iter l ~f:(count_expr counts)
  | Loop (_, l) | Block (_, l) -> count_instrs counts l
  | If (_, c, t, el) ->
      count_expr counts c;
      count_instrs counts t;
      count_instrs counts el

and count_instrs counts l = List.iter l ~f:(count_instr counts)

(* --------------------------------------------------------------------- *)
(*  Reordering                                                           *)
(* --------------------------------------------------------------------- *)

let is_ref_type (t : W.value_type) =
  match t with
  | Ref _ -> true
  | I32 | I64 | F32 | F64 -> false

(* Split the input list into consecutive same-type runs, preserving
   the first-occurrence order of the types.
   Returns a list of (type, locals-in-that-run). *)
let group_by_type locals =
  match locals with
  | [] -> []
  | (_, t0) :: _ ->
      let rec loop acc curr_t curr_rev = function
        | [] -> List.rev ((curr_t, List.rev curr_rev) :: acc)
        | ((_, t) as loc) :: rest ->
            if Poly.equal t curr_t
            then loop acc curr_t (loc :: curr_rev) rest
            else loop ((curr_t, List.rev curr_rev) :: acc) t [ loc ] rest
      in
      loop [] t0 [] locals

(* Stable sort [l] by descending key [f x]. *)
let sort_desc ~key l = List.stable_sort l ~cmp:(fun a b -> compare (key b) (key a))

let f ~locals body =
  let counts = Var.Hashtbl.create (List.length locals) in
  List.iter locals ~f:(fun (x, _) -> Var.Hashtbl.add counts x 0);
  count_instrs counts body;
  let count_of x = Var.Hashtbl.find counts x in
  (* Split into reference-typed and numeric-typed locals, preserving the
     original relative order within each group so that [group_by_type]
     sees the same runs the input would have produced. *)
  let refs, nums = List.partition locals ~f:(fun (_, t) -> is_ref_type t) in
  let reorder_group group =
    let runs = group_by_type group in
    let runs =
      List.map runs ~f:(fun (t, members) ->
          let sorted = sort_desc members ~key:(fun (x, _) -> count_of x) in
          let total = List.fold_left sorted ~init:0 ~f:(fun s (x, _) -> s + count_of x) in
          t, sorted, total)
    in
    let runs = sort_desc runs ~key:(fun (_, _, total) -> total) in
    List.concat_map runs ~f:(fun (_, sorted, _) -> sorted)
  in
  (* Numeric-typed locals are typically much rarer in OCaml code than
     reference-typed ones, so placing them first keeps the high-use
     reference locals in a contiguous run starting right after — none
     of them pushed across the 128-index boundary by intervening
     numerics. *)
  reorder_group nums @ reorder_group refs

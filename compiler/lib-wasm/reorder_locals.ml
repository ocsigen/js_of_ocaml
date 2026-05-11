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

(*
   Reorder a function's non-parameter locals. The resulting layout is

     [ numeric block | reference block ]

   Each block is a sequence of same-type runs (i32, i64, f32, f64 in
   the numeric block; the various ref types in the reference block).
   Within a block, runs are ordered by descending total use count, and
   within each run the individual locals are ordered by descending use
   count. Parameters are not part of this list and keep their
   signature-fixed indices.

   Why low indices matter. [local.get]/[local.set]/[local.tee] encode
   their local index as LEB128, so indices 0..127 take one byte and
   128..16383 take two. Placing high-use locals first shaves a byte
   off each of their accesses in the emitted Wasm.

   Why numerics come before references. V8's Liftoff baseline expects
   two contiguous regions: numerics first (zero-initialised via a fast
   block-zeroing path) and references after (null-initialised by a
   single follow-up pass). OCaml code is reference-heavy, so the
   leading numeric block tends to be small and the hot ref locals
   still get low indices.

   Why same-type runs. The binary format encodes consecutive locals of
   the same type as a single [(count, type)] pair, and V8 Liftoff
   initialises each type-run as one block, so contiguity makes the
   function section smaller and warm-up faster.

   Why no body rewrite is needed. [local.get]/[local.set]/[local.tee]
   reference locals by [Var.t], not by a baked-in numeric index;
   [wasm_output] derives each numeric index from the variable's position
   in [param_names @ locals] at emit time. So permuting the [locals]
   list is the whole transformation — the body is read only to count
   uses and returned untouched. (Parameters live in [param_names], which
   we never touch, so their indices are fixed.)
*)

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

(* Bucket the locals by type — one bucket holding every local of a given
   type — preserving the first-occurrence order of the types. Returns a
   list of (type, locals-of-that-type).
   Collecting all same-type locals into a single bucket is what lets the
   final layout emit one [(count, type)] run per type. *)
let group_by_type locals =
  (* A polymorphic hash table keyed by type, each bucket accumulating its
     locals in reverse order. [order] records the types in reverse
     first-occurrence order. *)
  let tbl = Poly.Hashtbl.create 16 in
  let order = ref [] in
  List.iter locals ~f:(fun ((_, t) as local) ->
      match Poly.Hashtbl.find_opt tbl t with
      | Some bucket -> bucket := local :: !bucket
      | None ->
          order := t :: !order;
          Poly.Hashtbl.add tbl t (ref [ local ]));
  List.rev_map !order ~f:(fun t -> t, List.rev !(Poly.Hashtbl.find tbl t))

(* Stable sort [l] by descending key [f x]. *)
let sort_desc ~key l = List.stable_sort l ~cmp:(fun a b -> compare (key b) (key a))

let f ~locals body =
  let counts = Var.Hashtbl.create (List.length locals) in
  List.iter locals ~f:(fun (x, _) -> Var.Hashtbl.add counts x 0);
  count_instrs counts body;
  let count_of x = Var.Hashtbl.find counts x in
  (* Split into reference-typed and numeric-typed locals. [List.partition]
     preserves relative order, so [group_by_type] sees the types in their
     original first-occurrence order. *)
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

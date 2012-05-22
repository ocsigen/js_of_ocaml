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

(*XXX
Patterns:
  => loops should avoid absorbing the whole continuation...
     (detect when the continuation does not loop anymore and close
      the loop at this point)
  => should have special code for switches that include the preceding
     if statement when possible
  => if e1 then {if e2 then P else Q} else {if e3 then P else Q}
  => if e then return e1; return e2
  => if e then var x = e1; else var x = e2;
  => while (true) {.... if (e) continue; break; }

- CLEAN UP!!!
*)

let compact = ref true
let debug = Util.debug "gen"
let times = Util.debug "times"
let disable_compact_expr = Util.disabled "compactexpr"

let set_pretty () = compact := false

(****)

open Code

module J = Javascript

(****)

let rec list_group_rec f l b m n =
  match l with
    [] ->
      List.rev ((b, List.rev m) :: n)
  | a :: r ->
      let fa = f a in
      if fa = b then
        list_group_rec f r b (a :: m) n
      else
        list_group_rec f r fa [a] ((b, List.rev m) :: n)

let list_group f l =
  match l with
    []     -> []
  | a :: r -> list_group_rec f r (f a) [a] []

(****)

module Ctx = struct
  type t =
    { var_stream : Var.stream;
      mutable blocks : block AddrMap.t;
      live : int array;
      mutated_vars : VarSet.t AddrMap.t }

  let fresh_var ctx =
    let (x, stream) = Var.next ctx.var_stream in
    (x, {ctx with var_stream = stream})

  let initial b l v =
    { var_stream = Var.make_stream (); blocks = b; live = l; mutated_vars = v }

  let used_once ctx x = ctx.live.(Var.idx x) <= 1
end

let add_names = Hashtbl.create 101

let var x = J.EVar (Var.to_string x)
let int n = J.ENum (float n)
let one = int 1
let zero = int 0
let addr pc =
  if not !compact then
    Format.sprintf "f%d" pc
  else begin
    try
      Hashtbl.find add_names pc
    with Not_found ->
      let x = Var.to_string (Var.fresh ()) in
      Hashtbl.replace add_names pc x;
      x
  end
let bool e = J.ECond (e, one, zero)
let boolnot e = J.ECond (e, zero, one)
let val_float f = f (*J.EArr [Some (J.ENum 253.); Some f]*)
let float_val e = e (*J.EAccess (e, one)*)

(****)

let float_const f = val_float (J.ENum f)

let rec constant x =
  match x with
    String s ->
      Primitive.mark_used "MlString";
      J.ENew (J.EVar ("MlString"), Some [J.EStr (s, `Bytes)])
  | Float f ->
      float_const f
  | Float_array a ->
      J.EArr (Some (int Obj.double_array_tag) ::
              Array.to_list (Array.map (fun f -> Some (float_const f)) a))
  | Int32 i ->
      J.ENum (Int32.to_float i)
  | Nativeint i ->
      J.ENum (Nativeint.to_float i)
  | Int64 i ->
      J.EArr [Some (int 255);
              Some (int (Int64.to_int i land 0xffffff));
              Some (int (Int64.to_int (Int64.shift_right i 24) land 0xffffff));
              Some (int (Int64.to_int (Int64.shift_right i 48) land 0xffff))]
  | Tuple (tag, a) ->
      J.EArr (Some (int tag) ::
              Array.to_list (Array.map (fun x -> Some (constant x)) a))
  | Int i ->
      int i

(****)

(*
Some variables are constant:   x = 1
Some may change after effectful operations : x = y[z]

There can be at most one effectful operations in the queue at once

let (e, expr_queue) = ... in
flush_queue expr_queue e
*)

let const_p = 0
let mutable_p = 1
let mutator_p = 2
let flush_p = 3
let or_p p q = max p q
let is_mutable p = p >= mutable_p
let is_mutator p = p >= mutator_p
let kind k =
  match k with
    `Pure -> const_p | `Mutable -> mutable_p | `Mutator -> mutator_p

let access_queue queue x =
  try
    let res = List.assoc x queue in
    (res, List.remove_assoc x queue)
  with Not_found ->
    ((const_p, var x), queue)

let should_flush cond prop = cond <> const_p && cond + prop >= flush_p

let flush_queue expr_queue prop l =
  let (instrs, expr_queue) =
    if prop >= flush_p then (expr_queue, []) else
    List.partition (fun (y, (p, _)) -> should_flush prop p) expr_queue
  in
  let instrs =
    List.map (fun (x, (_, ce)) ->
                J.Variable_statement
                  [Var.to_string x, Some ce]) instrs
  in
  (List.rev_append instrs l, expr_queue)

let flush_all expr_queue l = fst (flush_queue expr_queue flush_p l)

let enqueue expr_queue prop x ce =
  let (instrs, expr_queue) =
    if disable_compact_expr () then
      flush_queue expr_queue flush_p []
    else if is_mutable prop then
      flush_queue expr_queue prop []
    else
      [], expr_queue
  in
  (instrs, (x, (prop, ce)) :: expr_queue)

(****)

type state =
  { all_succs : (int, AddrSet.t) Hashtbl.t;
    succs : (int, int list) Hashtbl.t;
    backs : (int, AddrSet.t) Hashtbl.t;
    preds : (int, int) Hashtbl.t;
    mutable loops : AddrSet.t;
    mutable loop_stack : (addr * (int * bool ref)) list;
    mutable visited_blocks : AddrSet.t;
    mutable interm_idx : int;
    ctx : Ctx.t; mutable blocks : Code.block AddrMap.t }

let get_preds st pc = try Hashtbl.find st.preds pc with Not_found -> 0
let incr_preds st pc = Hashtbl.replace st.preds pc (get_preds st pc + 1)
let decr_preds st pc = Hashtbl.replace st.preds pc (get_preds st pc - 1)
let protect_preds st pc =
  Hashtbl.replace st.preds pc (get_preds st pc + 1000000)
let unprotect_preds st pc =
  Hashtbl.replace st.preds pc (get_preds st pc - 1000000)

let (>>) x f = f x

(* This as to be kept in sync with the way we build conditionals
   and switches! *)
let fold_children blocks pc f accu =
  let block = AddrMap.find pc blocks in
  match block.branch with
    Return _ | Raise _ | Stop ->
      accu
  | Branch (pc', _) | Poptrap (pc', _) ->
      f pc' accu
  | Cond (_, _, (pc1, _), (pc2, _)) | Pushtrap ((pc1, _), _, (pc2, _), _) ->
      accu >> f pc1 >> f pc2
  | Switch (_, a1, a2) ->
      let normalize a =
        a >> Array.to_list
          >> List.sort compare
          >> list_group (fun x -> x)
          >> List.map fst
          >> Array.of_list
      in
      accu >> Array.fold_right (fun (pc, _) accu -> f pc accu) (normalize a1)
           >> Array.fold_right (fun (pc, _) accu -> f pc accu) (normalize a2)

let rec build_graph st pc anc =
  if not (AddrSet.mem pc st.visited_blocks) then begin
    st.visited_blocks <- AddrSet.add pc st.visited_blocks;
    let anc = AddrSet.add pc anc in
    let s = Code.fold_children st.blocks pc AddrSet.add AddrSet.empty in
    Hashtbl.add st.all_succs pc s;
    let backs = AddrSet.inter s anc in
    Hashtbl.add st.backs pc backs;

    let s = fold_children st.blocks pc (fun x l -> x :: l) [] in
    let succs = List.filter (fun pc -> not (AddrSet.mem pc anc)) s in
    Hashtbl.add st.succs pc succs;
    AddrSet.iter (fun pc' -> st.loops <- AddrSet.add pc' st.loops) backs;
    List.iter (fun pc' -> build_graph st pc' anc) succs;
    List.iter (fun pc' -> incr_preds st pc') succs
  end

let rec dominance_frontier_rec st pc visited grey =
  let n = get_preds st pc in
  let v = try AddrMap.find pc visited with Not_found -> 0 in
(*
Format.eprintf "%d %d %d@." pc n v;
*)
  if v < n then begin
    let v = v + 1 in
    let visited = AddrMap.add pc v visited in
    if v = n then begin
      let grey = AddrSet.remove pc grey in
      let s = Hashtbl.find st.succs pc in
      List.fold_right
        (fun pc' (visited, grey) ->
           dominance_frontier_rec st pc' visited grey)
        s (visited, grey)
    end else begin
      (visited, if v = 1 then AddrSet.add pc grey else grey)
    end
  end else
    (visited, grey)

let dominance_frontier st pc =
  snd (dominance_frontier_rec st pc AddrMap.empty AddrSet.empty)

(* Block of code that never continues (either returns, throws an exception
   or loops back) *)
let never_continue st (pc, _) frontier interm succs =
  (* If not found in successors, this is a backward edge *)
  let d = try List.assoc pc succs with Not_found -> AddrSet.empty in
(*
Format.eprintf "pc: %d@." pc;
List.iter (fun (pc, _) ->Format.eprintf "pc: %d@." pc) succs;
Format.eprintf "never_continue@.";
Format.eprintf "  %d /" pc;
AddrSet.iter (fun i -> Format.eprintf " %d" i) frontier;
Format.eprintf " /";
AddrMap.iter (fun i _ -> Format.eprintf " %d" i) interm;
Format.eprintf " /";
AddrSet.iter (fun i -> Format.eprintf " %d" i) d;
let res =
*)
  not (AddrSet.mem pc frontier || AddrMap.mem pc interm)
    &&
  AddrSet.is_empty d
(*
in
Format.eprintf " ==> %b@." res;
res
*)

let rec resolve_node interm pc =
  try
    resolve_node interm (fst (AddrMap.find pc interm))
  with Not_found ->
    pc

let resolve_nodes interm s =
  AddrSet.fold (fun pc s' -> AddrSet.add (resolve_node interm pc) s')
    s AddrSet.empty

(****)

let rec visit visited prev s m x l =
  if not (VarSet.mem x visited) then begin
    let visited = VarSet.add x visited in
    let y = VarMap.find x m in
    if Code.Var.compare x y = 0 then
      (visited, None, l)
    else if VarSet.mem y prev then begin
      let t = Code.Var.fresh () in
      (visited, Some (y, t), (x, t) :: l)
    end else if VarSet.mem y s then begin
      let (visited, aliases, l) = visit visited (VarSet.add x prev) s m y l in
      match aliases with
        Some (a, b) when Code.Var.compare a x = 0 ->
          (visited, None, (b, a) :: (x, y) :: l)
      | _ ->
          (visited, aliases, (x, y) :: l)
    end else
      (visited, None, (x, y) :: l)
  end else
    (visited, None, l)

let visit_all params args =
  let m = Subst.build_mapping params args in
  let s = List.fold_left (fun s x -> VarSet.add x s) VarSet.empty params in
  let (_, l) =
    VarSet.fold
      (fun x (visited, l) ->
         let (visited, _, l) = visit visited VarSet.empty s m x l in
         (visited, l))
      s (VarSet.empty, [])
  in
  l

let parallel_renaming ctx params args continuation queue =
  let l = List.rev (visit_all params args) in
  List.fold_left
    (fun continuation (y, x) ->
       fun queue ->
       let ((px, cx), queue) = access_queue queue x in
       let (st, queue) =
(*
         let idx = Var.idx y in
         let len = Array.length ctx.Ctx.live in
         match if idx >= len then 2 else ctx.Ctx.live.(Var.idx y) with
           0 -> assert false
         | 1 -> enqueue queue px y cx
         | _ -> *)
         flush_queue queue px [J.Variable_statement [Var.to_string y, Some cx]]
       in
       st @ continuation queue)
    continuation l queue

(****)

let apply_funs = ref Util.IntMap.empty

let get_apply_fun n =
  try
    Util.IntMap.find n !apply_funs
  with Not_found ->
    Primitive.mark_used "caml_call_gen";
    let x = Var.fresh () in
    apply_funs := Util.IntMap.add n x !apply_funs;
    x

let generate_apply_funs cont =
  let funs = !apply_funs in
  apply_funs := Util.IntMap.empty;
  Util.IntMap.fold
    (fun n x cont ->
       let f = Var.to_string (Var.fresh ()) in
       let params =
         Array.to_list (Array.init n (fun _ -> Var.to_string (Var.fresh ())))
       in
       let f' = J.EVar f in
       let params' = List.map (fun x -> J.EVar x) params in
       J.Function_declaration
         (Var.to_string x, f :: params,
          [J.Statement
             (J.Return_statement
                (Some
                   (J.ECond (J.EBin (J.EqEq, J.EDot (f', "length"),
                                     J.ENum (float n)),
                             J.ECall (f', params'),
                             J.ECall (J.EVar "caml_call_gen",
                                      [f'; J.EArr (List.map (fun x -> Some x) params')])))))]) ::
       cont)
    funs cont

(****)

let to_int cx = J.EBin(J.Bor, cx, J.ENum 0.) (* 32 bit ints *)

let _ =
  List.iter (fun (nm, nm') -> Primitive.alias nm nm')
    ["%int_mul", "caml_mul";
     "%int_div", "caml_div";
     "%int_mod", "caml_mod";
     "caml_int32_neg", "%int_neg";
     "caml_int32_add", "%int_add";
     "caml_int32_sub", "%int_sub";
     "caml_int32_mul", "%int_mul";
     "caml_int32_div", "%int_div";
     "caml_int32_mod", "%int_mod";
     "caml_int32_and", "%int_and";
     "caml_int32_or", "%int_or";
     "caml_int32_xor", "%int_xor";
     "caml_int32_shift_left", "%int_lsl";
     "caml_int32_shift_right", "%int_asr";
     "caml_int32_shift_right_unsigned", "%int_lsr";
     "caml_int32_of_int", "%identity";
     "caml_int32_to_int", "%identity";
     "caml_int32_of_float", "caml_int_of_float";
     "caml_int32_to_float", "%identity";
     "caml_int32_format", "caml_format_int";
     "caml_int32_of_string", "caml_int_of_string";
     "caml_int32_compare", "caml_int_compare";
     "caml_nativeint_neg", "%int_neg";
     "caml_nativeint_add", "%int_add";
     "caml_nativeint_sub", "%int_sub";
     "caml_nativeint_mul", "%int_mul";
     "caml_nativeint_div", "%int_div";
     "caml_nativeint_mod", "%int_mod";
     "caml_nativeint_and", "%int_and";
     "caml_nativeint_or", "%int_or";
     "caml_nativeint_xor", "%int_xor";
     "caml_nativeint_shift_left", "%int_lsl";
     "caml_nativeint_shift_right", "%int_asr";
     "caml_nativeint_shift_right_unsigned", "%int_lsr";
     "caml_nativeint_of_int", "%identity";
     "caml_nativeint_to_int", "%identity";
     "caml_nativeint_of_float", "caml_int_of_float";
     "caml_nativeint_to_float", "%identity";
     "caml_nativeint_of_int32", "%identity";
     "caml_nativeint_to_int32", "%identity";
     "caml_nativeint_format", "caml_format_int";
     "caml_nativeint_of_string", "caml_int_of_string";
     "caml_nativeint_compare", "caml_int_compare";
     "caml_int64_of_int", "caml_int64_of_int32";
     "caml_int64_to_int", "caml_int64_to_int32";
     "caml_int64_of_nativeint", "caml_int64_of_int32";
     "caml_int64_to_nativeint", "caml_int64_to_int32";
     "caml_float_of_int", "%identity";
     "caml_array_get_float", "caml_array_get";
     "caml_array_get_addr", "caml_array_get";
     "caml_array_set_float", "caml_array_set";
     "caml_array_set_addr", "caml_array_set";
     "caml_array_unsafe_get_float", "caml_array_unsafe_get";
     "caml_array_unsafe_set_float", "caml_array_unsafe_set";
     "caml_alloc_dummy_float", "caml_alloc_dummy";
     "caml_make_array", "%identity";
     "caml_ensure_stack_capacity", "%identity";
     "caml_js_from_float", "%identity";
     "caml_js_to_float", "%identity"]

let internal_primitives = Hashtbl.create 31

let internal_prim name =
  try Hashtbl.find internal_primitives name with Not_found -> None

let register_prim name k f =
  Primitive.register name k;
  Hashtbl.add internal_primitives name (Some f)

let register_un_prim name k f =
  register_prim name k
    (fun l queue ->
       match l with
         [Pv x] ->
           let ((px, cx), queue) = access_queue queue x in
           (f cx, or_p (kind k) px, queue)
       | _ ->
           assert false)

let register_bin_prim name k f =
  register_prim name k
    (fun l queue ->
       match l with
         [Pv x; Pv y] ->
           let ((px, cx), queue) = access_queue queue x in
           let ((py, cy), queue) = access_queue queue y in
           (f cx cy, or_p (kind k) (or_p px py), queue)
       | _ ->
           assert false)

let register_tern_prim name f =
  register_prim name `Mutator
    (fun l queue ->
       match l with
         [Pv x; Pv y; Pv z] ->
           let ((px, cx), queue) = access_queue queue x in
           let ((py, cy), queue) = access_queue queue y in
           let ((pz, cz), queue) = access_queue queue z in
           (f cx cy cz, or_p mutator_p (or_p px (or_p py pz)), queue)
       | _ ->
           assert false)

let register_un_math_prim name prim =
  register_un_prim name `Pure
    (fun cx -> J.ECall (J.EDot (J.EVar "Math", prim), [cx]))

let register_bin_math_prim name prim =
  register_bin_prim name `Pure
    (fun cx cy -> J.ECall (J.EDot (J.EVar "Math", prim), [cx; cy]))

let _ =
  Code.add_reserved_name "Math";
  register_bin_prim "caml_array_unsafe_get" `Mutable
    (fun cx cy -> J.EAccess (cx, J.EBin (J.Plus, cy, one)));
  register_bin_prim "caml_string_get" `Mutable
    (fun cx cy -> J.ECall (J.EDot (cx, "safeGet"), [cy]));
  register_bin_prim "%int_add" `Pure
    (fun cx cy -> Js_simpl.eplus_int cx cy);
  register_bin_prim "%int_sub" `Pure
    (fun cx cy -> J.EBin (J.Minus, cx, cy));
  register_bin_prim "%direct_int_mul" `Pure
    (fun cx cy -> to_int (J.EBin (J.Mul, cx, cy)));
  register_bin_prim "%direct_int_div" `Pure
    (fun cx cy -> to_int (J.EBin (J.Div, cx, cy)));
  register_bin_prim "%direct_int_mod" `Pure
    (fun cx cy -> to_int (J.EBin (J.Mod, cx, cy)));
  register_bin_prim "%int_and" `Pure
    (fun cx cy -> J.EBin (J.Band, cx, cy));
  register_bin_prim "%int_or" `Pure
    (fun cx cy -> J.EBin (J.Bor, cx, cy));
  register_bin_prim "%int_xor" `Pure
    (fun cx cy -> J.EBin (J.Bxor, cx, cy));
  register_bin_prim "%int_lsl" `Pure
    (fun cx cy -> J.EBin (J.Lsl, cx, cy));
  register_bin_prim "%int_lsr" `Pure
    (fun cx cy -> J.EBin (J.Lsr, cx, cy));
  register_bin_prim "%int_asr" `Pure
    (fun cx cy -> J.EBin (J.Asr, cx, cy));
  register_un_prim "%int_neg" `Pure
    (fun cx -> J.EUn (J.Neg, cx));
  register_bin_prim "caml_eq_float" `Pure
    (fun cx cy -> bool (J.EBin (J.EqEq, float_val cx, float_val cy)));
  register_bin_prim "caml_neq_float" `Pure
    (fun cx cy -> bool (J.EBin (J.NotEq, float_val cx, float_val cy)));
  register_bin_prim "caml_ge_float" `Pure
    (fun cx cy -> bool (J.EBin (J.Le, float_val cy, float_val cx)));
  register_bin_prim "caml_le_float" `Pure
    (fun cx cy -> bool (J.EBin (J.Le, float_val cx, float_val cy)));
  register_bin_prim "caml_gt_float" `Pure
    (fun cx cy -> bool (J.EBin (J.Lt, float_val cy, float_val cx)));
  register_bin_prim "caml_lt_float" `Pure
    (fun cx cy -> bool (J.EBin (J.Lt, float_val cx, float_val cy)));
  register_bin_prim "caml_add_float" `Pure
    (fun cx cy -> val_float (J.EBin (J.Plus, float_val cx, float_val cy)));
  register_bin_prim "caml_sub_float" `Pure
    (fun cx cy -> val_float (J.EBin (J.Minus, float_val cx, float_val cy)));
  register_bin_prim "caml_mul_float" `Pure
    (fun cx cy -> val_float (J.EBin (J.Mul, float_val cx, float_val cy)));
  register_bin_prim "caml_div_float" `Pure
    (fun cx cy -> val_float (J.EBin (J.Div, float_val cx, float_val cy)));
  register_un_prim "caml_neg_float" `Pure
    (fun cx -> val_float (J.EUn (J.Neg, float_val cx)));
  register_bin_prim "caml_fmod_float" `Pure
    (fun cx cy -> val_float (J.EBin (J.Mod, float_val cx, float_val cy)));
  register_un_prim "caml_ml_string_length" `Pure
    (fun cx -> J.ECall (J.EDot (cx, "getLen"), []));
  register_tern_prim "caml_array_unsafe_set"
    (fun cx cy cz ->
       J.EBin (J.Eq, J.EAccess (cx, J.EBin (J.Plus, cy, one)), cz));
  register_tern_prim "caml_string_set"
    (fun cx cy cz -> J.ECall (J.EDot (cx, "safeSet"), [cy; cz]));
  register_un_prim "caml_alloc_dummy" `Pure (fun cx -> J.EArr []);
  register_un_prim "caml_obj_dup" `Mutable
    (fun cx -> J.ECall (J.EDot (cx, "slice"), []));
  register_un_prim "caml_int_of_float" `Pure to_int;
  register_un_math_prim "caml_abs_float" "abs";
  register_un_math_prim "caml_acos_float" "acos";
  register_un_math_prim "caml_asin_float" "asin";
  register_un_math_prim "caml_atan_float" "atan";
  register_bin_math_prim "caml_atan2_float" "atan2";
  register_un_math_prim "caml_ceil_float" "ceil";
  register_un_math_prim "caml_cos_float" "cos";
  register_un_math_prim "caml_exp_float" "exp";
  register_un_math_prim "caml_floor_float" "floor";
  register_un_math_prim "caml_log_float" "log";
  register_bin_math_prim "caml_power_float" "pow";
  register_un_math_prim "caml_sin_float" "sin";
  register_un_math_prim "caml_sqrt_float" "sqrt";
  register_un_math_prim "caml_tan_float" "tan";
  register_un_prim "caml_js_from_bool" `Pure
    (fun cx -> J.EUn (J.Not, J.EUn (J.Not, cx)));
  register_un_prim "caml_js_to_bool" `Pure to_int;
  register_un_prim "caml_js_from_string" `Mutable
    (fun cx -> J.ECall (J.EDot (cx, "toString"), []));
  register_un_prim "caml_js_to_string" `Mutable
    (fun cx ->
       Primitive.mark_used "MlString";
       J.ENew (J.EVar "MlWrappedString", Some [cx]));
  register_tern_prim "caml_js_set"
    (fun cx cy cz -> J.EBin (J.Eq, J.EAccess (cx, cy), cz));
  register_bin_prim "caml_js_get" `Mutable
    (fun cx cy -> J.EAccess (cx, cy));
  register_bin_prim "caml_js_equals" `Mutable
    (fun cx cy -> bool (J.EBin (J.EqEq, cx, cy)));
  register_bin_prim "caml_js_instanceof" `Pure
    (fun cx cy -> bool (J.EBin(J.InstanceOf, cx, cy)));
  register_un_prim "caml_js_typeof" `Pure
    (fun cx -> J.EUn(J.Typeof, cx))

(****)

let varset_disjoint s s' = not (VarSet.exists (fun x -> VarSet.mem x s') s)

let rec group_closures_rec closures req =
  match closures with
    [] ->
      ([], VarSet.empty)
  | ((var, vars, _) as elt) :: rem ->
      let req = VarSet.union vars req in
      let (closures', prov) = group_closures_rec rem req in
      if varset_disjoint prov req then
        ([elt] :: closures', VarSet.singleton var)
      else
        match closures' with
          []     -> assert false
        | l :: r -> ((elt :: l) :: r, VarSet.add var prov)

let group_closures l = fst (group_closures_rec l VarSet.empty)

let rec collect_closures ctx l =
  match l with
    Let (x, Closure (args, ((pc, _) as cont))) :: rem ->
      let all_vars = AddrMap.find pc ctx.Ctx.mutated_vars in
      let vars = VarSet.remove x all_vars in
      let fun_name =
        if not (VarSet.is_empty vars) && VarSet.mem x all_vars then
          Some (Var.to_string x)
        else
          None
      in
      let cl =
        J.EFun (fun_name, List.map Var.to_string args,
                compile_closure ctx cont)
      in
      let (l', rem') = collect_closures ctx rem in
      ((x, vars, cl) :: l', rem')
  | _ ->
    ([], l)

(****)

and translate_expr ctx queue x e =
  match e with
    Const i ->
      (int i, const_p, queue)
  | Apply (x, l, Some n) when n = List.length l ->
      let ((px, cx), queue) = access_queue queue x in
      let (args, prop, queue) =
        List.fold_right
          (fun x (args, prop, queue) ->
             let ((prop', cx), queue) =
               access_queue queue x in (cx :: args, or_p prop prop', queue))
          l ([], or_p px mutator_p, queue)
      in
      (J.ECall (cx, args), prop, queue)
  | Apply (x, l, _) ->
      let (args, prop, queue) =
        List.fold_right
          (fun x (args, prop, queue) ->
             let ((prop', cx), queue) =
               access_queue queue x in (cx :: args, or_p prop prop', queue))
          (x :: l) ([], mutator_p, queue)
      in
      let y = get_apply_fun (List.length l) in
      (J.ECall (J.EVar (Var.to_string y), args),
       prop, queue)
  | Block (tag, a) ->
      let (contents, prop, queue) =
        List.fold_right
          (fun x (args, prop, queue) ->
             let ((prop', cx), queue) = access_queue queue x in
             (Some cx :: args, or_p prop prop', queue))
          (Array.to_list a) ([], const_p, queue)
      in
      (J.EArr (Some (int tag) :: contents), prop, queue)
  | Field (x, n) ->
      let ((px, cx), queue) = access_queue queue x in
      (J.EAccess (cx, int (n + 1)), or_p px mutable_p, queue)
  | Closure (args, ((pc, _) as cont)) ->
      (*FIX: should flush only the closure free variables...*)
      (*FIX: if there are several closures in a row, we should process them
        simultaneously (possibly recursive functions)*)
      let all_vars = AddrMap.find pc ctx.Ctx.mutated_vars in
      let vars =
        all_vars
        >> VarSet.remove x
        >> VarSet.elements
        >> List.map Var.to_string
      in
      let fun_name =
        if vars <> [] && VarSet.mem x all_vars then
          Some (Var.to_string x)
        else
          None
      in
      let cl =
        J.EFun (fun_name, List.map Var.to_string args,
                compile_closure ctx cont)
      in
      let cl =
        if vars = [] then cl else
        J.ECall (J.EFun (None, vars,
                         [J.Statement (J.Return_statement (Some cl))]),
                 List.map (fun x -> J.EVar x) vars)
      in
      (cl, flush_p, queue)
  | Constant c ->
      (constant c, const_p, queue)
  | Prim (p, l) ->
      begin match p, l with
        Vectlength, [Pv x] ->
          let ((px, cx), queue) = access_queue queue x in
          (J.EBin (J.Minus, J.EDot (cx, "length"), one), px, queue)
      | Array_get, [Pv x; Pv y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EAccess (cx, J.EBin (J.Plus, cy, one)),
           or_p mutable_p (or_p px py), queue)
      | Extern "caml_js_var", [Pc (String nm)] ->
          Code.add_reserved_name nm;  (*XXX HACK *)
          (J.EVar nm, const_p, queue)
      | Extern "caml_js_const", [Pc (String nm)] ->
          Code.add_reserved_name nm;  (*XXX HACK *)
          (J.EVar nm, const_p, queue)
      | Extern "caml_js_opt_call", Pv f :: Pv o :: l ->
          let ((pf, cf), queue) = access_queue queue f in
          let ((po, co), queue) = access_queue queue o in
          let (args, prop, queue) =
            List.fold_right
              (fun x (args, prop, queue) ->
                 let x = match x with Pv x -> x | _ -> assert false in
                 let ((prop', cx), queue) = access_queue queue x in
                 (cx :: args, or_p prop prop', queue))
              l ([], mutator_p, queue)
          in
          (J.ECall (J.EDot (cf, "call"), co :: args),
           or_p (or_p pf po) prop, queue)
      | Extern "caml_js_opt_fun_call", Pv f :: l ->
          let ((pf, cf), queue) = access_queue queue f in
          let (args, prop, queue) =
            List.fold_right
              (fun x (args, prop, queue) ->
                 let x = match x with Pv x -> x | _ -> assert false in
                 let ((prop', cx), queue) = access_queue queue x in
                 (cx :: args, or_p prop prop', queue))
              l ([], mutator_p, queue)
          in
          (J.ECall (cf, args), or_p pf prop, queue)
      | Extern "caml_js_opt_meth_call", Pv o :: Pc (String m) :: l ->
          let ((po, co), queue) = access_queue queue o in
          let (args, prop, queue) =
            List.fold_right
              (fun x (args, prop, queue) ->
                 let x = match x with Pv x -> x | _ -> assert false in
                 let ((prop', cx), queue) = access_queue queue x in
                 (cx :: args, or_p prop prop', queue))
              l ([], mutator_p, queue)
          in
          (J.ECall (J.EDot (co, m), args), or_p po prop, queue)
      | Extern "caml_js_opt_new", Pv c :: l ->
          let ((pc, cc), queue) = access_queue queue c in
          let (args, prop, queue) =
            List.fold_right
              (fun x (args, prop, queue) ->
                 let x = match x with Pv x -> x | _ -> assert false in
                 let ((prop', cx), queue) = access_queue queue x in
                 (cx :: args, or_p prop prop', queue))
              l ([], mutator_p, queue)
          in
          (J.ENew (cc, if args = [] then None else Some args),
           or_p pc prop, queue)
      | Extern "caml_js_get", [Pv o; Pc (String f)] ->
          let ((po, co), queue) = access_queue queue o in
          (J.EDot (co, f), or_p po mutable_p, queue)
      | Extern "caml_js_set", [Pv o; Pc (String f); Pv v] ->
          let ((po, co), queue) = access_queue queue o in
          let ((pv, cv), queue) = access_queue queue v in
          (J.EBin (J.Eq, J.EDot (co, f), cv),
           or_p (or_p po pv) mutator_p, queue)
      | Extern "%object_literal", fields ->
          let rec build_fields l =
            match l with
              [] ->
                []
            | Pc (String nm) :: Pc (String v) :: r ->
                (J.PNS nm, J.EStr (v, `Bytes)) :: build_fields r
            | _ ->
                assert false
          in
          (J.EObj (build_fields fields), const_p, queue)
      | Extern name, l ->
          let name = Primitive.resolve name in
          begin match internal_prim name with
            Some f ->
              f l queue
          | None ->
              Primitive.mark_used name;
              Code.add_reserved_name name;  (*XXX HACK *)
                               (* FIX: this is done at the wrong time... *)
              let prim_kind = kind (Primitive.kind name) in
              let (args, prop, queue) =
                List.fold_right
                  (fun x (args, prop, queue) ->
                     let x = match x with Pv x -> x | _ -> assert false in
                     let ((prop', cx), queue) = access_queue queue x in
                     (cx :: args, or_p prop prop', queue))
                  l ([], prim_kind, queue)
              in
              (J.ECall (J.EVar name, args), prop, queue)
          end
      | Not, [Pv x] ->
          let ((px, cx), queue) = access_queue queue x in
          (J.EBin (J.Minus, one, cx), px, queue)
      | Lt, [Pv x; Pv y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (bool (J.EBin (J.Lt, cx, cy)), or_p px py, queue)
      | Le, [Pv x; Pv y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (bool (J.EBin (J.Le, cx, cy)), or_p px py, queue)
      | Eq, [Pv x; Pv y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (bool (J.EBin (J.EqEqEq, cx, cy)), or_p px py, queue)
      | Neq, [Pv x; Pv y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (bool (J.EBin (J.NotEqEq, cx, cy)), or_p px py, queue)
      | IsInt, [Pv x] ->
          let ((px, cx), queue) = access_queue queue x in
          (J.EBin(J.EqEqEq, J.EUn (J.Typeof, cx), J.EStr ("number", `Bytes)),
           px, queue)
(*
          (boolnot (J.EBin(J.InstanceOf, cx, J.EVar ("Array"))), px, queue)
*)
      | Ult, [Pv x; Pv y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (bool (J.EBin (J.Or, J.EBin (J.Lt, cy, int 0),
                         J.EBin (J.Lt, cx, cy))),
           or_p px py, queue)
      | WrapInt, [Pv x] ->
          let ((px, cx), queue) = access_queue queue x in
          (to_int cx, px, queue)
      | (Vectlength | Array_get | Not | IsInt | Eq |
         Neq | Lt | Le | Ult | WrapInt), _ ->
          assert false
      end

and translate_closures ctx expr_queue l =
  match l with
    [] ->
      ([], expr_queue)
  | [(x, vars, cl)] :: rem ->
      let vars =
        vars
        >> VarSet.elements
        >> List.map Var.to_string
      in
      let cl =
        if vars = [] then cl else
        J.ECall (J.EFun (None, vars,
                         [J.Statement (J.Return_statement (Some cl))]),
                 List.map (fun x -> J.EVar x) vars)
      in
      let (st, expr_queue) =
        match ctx.Ctx.live.(Var.idx x) with
          0 -> flush_queue expr_queue flush_p [J.Expression_statement cl]
        | 1 -> enqueue expr_queue flush_p x cl
        | _ -> flush_queue expr_queue flush_p
                 [J.Variable_statement [Var.to_string x, Some cl]]
      in
      let (st', expr_queue) = translate_closures ctx expr_queue rem in
      (st @ st', expr_queue)
  | l :: rem ->
      let names =
        List.fold_left (fun s (x, _, _) -> VarSet.add x s) VarSet.empty l in
      let vars =
        List.fold_left (fun s (_, s', _) -> VarSet.union s s') VarSet.empty l
      in
      let vars =
        VarSet.diff vars names
        >> VarSet.elements
        >> List.map Var.to_string
      in
      let defs =
        List.map (fun (x, _, cl) -> (Var.to_string x, Some cl)) l in
      let statement =
        if vars = [] then
          J.Variable_statement defs
        else begin
          let tbl = Var.fresh () in
          let arr =
            J.EArr
              (List.map (fun (x, _, _) -> Some (J.EVar (Var.to_string x))) l)
          in
          let assgn =
            List.fold_left
              (fun (l, n) (x, _, _) ->
                 ((Var.to_string x,
                   Some (J.EAccess (J.EVar (Var.to_string tbl), int n))) :: l,
                  n + 1))
              ([], 0) l
          in
          J.Variable_statement
            ((Var.to_string tbl,
              Some
                (J.ECall
                   (J.EFun (None, vars,
                            [J.Statement (J.Variable_statement defs);
                             J.Statement (J.Return_statement (Some arr))]),
                    List.map (fun x -> J.EVar x) vars))) ::
             List.rev (fst assgn))
        end
      in
      let (st, expr_queue) = flush_queue expr_queue flush_p [statement] in
      let (st', expr_queue) = translate_closures ctx expr_queue rem in
      (st @ st', expr_queue)

and translate_instr ctx expr_queue instr =
  match instr with
    [] ->
      ([], expr_queue)
  | Let (_, Closure _) :: _ ->
      let (l, rem) = collect_closures ctx instr in
      let l = group_closures l in
      let (st, expr_queue) = translate_closures ctx expr_queue l in
      let (instrs, expr_queue) = translate_instr ctx expr_queue rem in
      (st @ instrs, expr_queue)
  | i :: rem ->
      let (st, expr_queue) =
        match i with
          Let (x, e) ->
            let (ce, prop, expr_queue) = translate_expr ctx expr_queue x e in
            begin match ctx.Ctx.live.(Var.idx x) with
              0 -> flush_queue expr_queue prop [J.Expression_statement ce]
            | 1 -> enqueue expr_queue prop x ce
            | _ -> flush_queue expr_queue prop
                     [J.Variable_statement [Var.to_string x, Some ce]]
            end
        | Set_field (x, n, y) ->
            let ((px, cx), expr_queue) = access_queue expr_queue x in
            let ((py, cy), expr_queue) = access_queue expr_queue y in
            flush_queue expr_queue mutator_p
              [J.Expression_statement
                 (J.EBin (J.Eq, J.EAccess (cx, int (n + 1)), cy))]
        | Offset_ref (x, n) ->
(* FIX: may overflow.. *)
            let ((px, cx), expr_queue) = access_queue expr_queue x in
            flush_queue expr_queue mutator_p
              [J.Expression_statement
                 (J.EBin (J.PlusEq, (J.EAccess (cx, J.ENum 1.)), int n))]
        | Array_set (x, y, z) ->
            let ((px, cx), expr_queue) = access_queue expr_queue x in
            let ((py, cy), expr_queue) = access_queue expr_queue y in
            let ((pz, cz), expr_queue) = access_queue expr_queue z in
            flush_queue expr_queue mutator_p
              [J.Expression_statement
                 (J.EBin (J.Eq, J.EAccess (cx, J.EBin(J.Plus, cy, one)),
                          cz))]
      in
      let (instrs, expr_queue) = translate_instr ctx expr_queue rem in
      (st @ instrs, expr_queue)

and compile_block st queue pc frontier interm =
if queue <> [] && AddrSet.mem pc st.loops then
  flush_all queue (compile_block st [] pc frontier interm)
else begin
(*
Format.eprintf "(frontier: ";
AddrSet.iter (fun pc -> Format.eprintf "%d " pc) frontier;
Format.eprintf ")@.";
Format.eprintf "(interm: ";
AddrMap.iter (fun pc (pc', _) -> Format.eprintf " %d->%d " pc pc') interm;
Format.eprintf ")@.";
*)
  if pc >= 0 then begin
    if AddrSet.mem pc st.visited_blocks then begin
      Format.eprintf "!!!! %d@." pc; assert false
    end;
    st.visited_blocks <- AddrSet.add pc st.visited_blocks
  end;
  if debug () then begin
    if AddrSet.mem pc st.loops then Format.eprintf "@[<2>for(;;){@,";
    Format.eprintf "block %d;@ @?" pc
  end;
  if AddrSet.mem pc st.loops then begin
    let lab =
      match st.loop_stack with (_, (l, _)) :: _ -> l + 1 | [] -> 0 in
    st.loop_stack <- (pc, (lab, ref false)) :: st.loop_stack
  end;
  let succs = Hashtbl.find st.succs pc in
  let backs = Hashtbl.find st.backs pc in
  (* Remove limit *)
  if pc < 0 then List.iter (fun pc -> unprotect_preds st pc) succs;
  let succs = List.map (fun pc -> (pc, dominance_frontier st pc)) succs in
  let grey =
    List.fold_right
      (fun (_, frontier) grey -> AddrSet.union frontier grey)
      succs AddrSet.empty
  in
  let new_frontier = resolve_nodes interm grey in
(*
Format.eprintf "<<%d:" pc;
AddrSet.iter (fun pc -> Format.eprintf " %d" pc) frontier;
Format.eprintf ">>@ ";
*)
  let block = AddrMap.find pc st.blocks in
  let (seq, queue) = translate_instr st.ctx queue block.body in
  let body =
    seq @
    match block.branch with
      Code.Pushtrap ((pc1, args1), x, (pc2, args2), pc3) ->
  (* FIX: document this *)
        let grey =  dominance_frontier st pc2 in
        let grey' = resolve_nodes interm grey in
        let limit_body =
          (* We need to make sure that pc3 is live (indeed, the
             continuation may have been optimized away by inlining) *)
          AddrSet.is_empty grey' && pc3 >= 0 && Hashtbl.mem st.succs pc3 in
        let inner_frontier =
          if limit_body then AddrSet.add pc3 grey' else grey'
        in
        if limit_body then incr_preds st pc3;
        assert (AddrSet.cardinal inner_frontier <= 1);
        if debug () then Format.eprintf "@[<2>try {@,";
(*
Format.eprintf "===== %d ===== (%b)@." pc3 limit_body;
*)
        let body =
          compile_branch st [] (pc1, args1)
            None AddrSet.empty inner_frontier interm
        in
        if debug () then Format.eprintf "} catch {@,";
        let handler = compile_block st [] pc2 inner_frontier interm in
        let x =
          let block2 = AddrMap.find pc2 st.blocks in
          let m = Subst.build_mapping args2 block2.params in
          try VarMap.find x m with Not_found -> x
        in
        if debug () then Format.eprintf "}@]@ ";
        if limit_body then decr_preds st pc3;
        flush_all queue
          (J.Try_statement (Js_simpl.statement_list body,
                            Some (Var.to_string x,
                                  Js_simpl.statement_list handler),
                            None) ::
           if AddrSet.is_empty inner_frontier then [] else begin
             let pc = AddrSet.choose inner_frontier in
(*
Format.eprintf ">>Frontier: ";
AddrSet.iter (fun pc -> Format.eprintf "%d " pc) frontier;
Format.eprintf "@.";
*)
             if AddrSet.mem pc frontier then [] else
               compile_block st [] pc frontier interm
           end)
    | _ ->
(*
Format.eprintf "[[@.";
*)
        let (new_frontier, new_interm) =
          if AddrSet.cardinal new_frontier > 1 then begin
            let x = Code.Var.fresh () in
            let a = Array.of_list (AddrSet.elements new_frontier) in
            if debug () then Format.eprintf "@ var %a;" Code.Var.print x;
            let idx = st.interm_idx in
            st.interm_idx <- idx - 1;
(*
Format.eprintf "%d ====> " idx;
AddrSet.iter (fun pc -> Format.eprintf "%d " pc) new_frontier;
Format.eprintf "@.";
*)
            let cases = Array.map (fun pc -> (pc, [])) a in
            let switch =
              if Array.length cases > 2 then
                Code.Switch (x, cases, [||])
              else
                Code.Cond (IsTrue, x, cases.(1), cases.(0))
            in
            st.blocks <-
              AddrMap.add idx
                { params = []; handler = None; body = []; branch = switch }
              st.blocks;
            (* There is a branch from this switch to the members
               of the frontier. *)
            AddrSet.iter (fun pc -> incr_preds st pc) new_frontier;
            (* Put a limit: we are going to remove other branches
               to the members of the frontier (in compile_conditional),
               but they should remain in the frontier. *)
            AddrSet.iter (fun pc -> protect_preds st pc) new_frontier;
            Hashtbl.add st.succs idx (AddrSet.elements new_frontier);
            Hashtbl.add st.all_succs idx new_frontier;
            Hashtbl.add st.backs idx AddrSet.empty;
            (AddrSet.singleton idx,
             Array.fold_right
               (fun (pc, i) interm -> (AddrMap.add pc (idx, (x, i)) interm))
               (Array.mapi (fun i pc -> (pc, i)) a) interm)
          end else
            (new_frontier, interm)
        in
        assert (AddrSet.cardinal new_frontier <= 1);
        (* Beware evaluation order! *)
        let cond =
          compile_conditional
            st queue pc block.branch block.handler
            backs new_frontier new_interm succs in
(*
let res =
*)
        cond @
        if AddrSet.cardinal new_frontier = 0 then [] else begin
          let pc = AddrSet.choose new_frontier in
          if AddrSet.mem pc frontier then [] else
          compile_block st [] pc frontier interm
        end
(*
in
Format.eprintf "]]@.";
res
*)
  in
  if AddrSet.mem pc st.loops then begin
    let label =
      match st.loop_stack with
        (_, (l, used)) :: r -> st.loop_stack <- r; if !used then l else -1
      | []                  -> assert false
    in
    let st =
      J.For_statement
        (None, None, None,
         Js_simpl.block
           (if AddrSet.cardinal frontier > 0 then begin
              if debug () then
                Format.eprintf "@ break (%d); }@]"
                  (AddrSet.choose new_frontier);
              body @ [J.Break_statement None]
            end else begin
              if debug () then Format.eprintf "}@]";
              body
            end))
    in
    if label = -1 then
      [st]
    else
      [J.Labelled_statement (Code.string_of_ident label, st)]
  end else
    body
end

and compile_if st e cont1 cont2 handler backs frontier interm succs =
  let iftrue = compile_branch st [] cont1 handler backs frontier interm in
  let iffalse = compile_branch st [] cont2 handler backs frontier interm in
(*
Format.eprintf "====@.";
*)
  Js_simpl.if_statement e
    (Js_simpl.block iftrue) (never_continue st cont1 frontier interm succs)
    (Js_simpl.block iffalse) (never_continue st cont2 frontier interm succs)

and compile_conditional st queue pc last handler backs frontier interm succs =
  List.iter
    (fun (pc, _) -> if AddrMap.mem pc interm then decr_preds st pc) succs;
  if debug () then begin
    match last with
      Branch _ | Poptrap _ | Pushtrap _ -> ()
    | Return _ -> Format.eprintf "ret"
    | Raise _ -> Format.eprintf "raise"
    | Stop -> Format.eprintf "stop"
    | Cond _ -> Format.eprintf "@[<hv 2>cond{@,"
    | Switch _ -> Format.eprintf "@[<hv 2>switch{@,"
  end;
  let res =
  match last with
    Return x ->
      let ((px, cx), queue) = access_queue queue x in
      flush_all queue [J.Return_statement (Some cx)]
  | Raise x ->
      let ((px, cx), queue) = access_queue queue x in
      flush_all queue [J.Throw_statement cx]
  | Stop ->
      flush_all queue [J.Return_statement None]
  | Branch cont ->
      compile_branch st queue cont handler backs frontier interm
  | Cond (c, x, cont1, cont2) ->
      let ((px, cx), queue) = access_queue queue x in
      let e =
        match c with
          IsTrue         -> cx
        | CEq n          -> J.EBin (J.EqEqEq, int n, cx)
        | CLt n          -> J.EBin (J.Lt, int n, cx)
        | CUlt n         -> J.EBin (J.Or, J.EBin (J.Lt, cx, int 0),
                                          J.EBin (J.Lt, int n, cx))
        | CLe n          -> J.EBin (J.Le, int n, cx)
      in
      (* Some changes here may require corresponding changes
         in function [fold_children] above. *)
      flush_all queue
        (compile_if st e cont1 cont2 handler backs frontier interm succs)

  | Switch (x, a1, a2) ->
      (* Some changes here may require corresponding changes
         in function [fold_children] above. *)
      let build_switch e a =
        let a = Array.mapi (fun i cont -> (i, cont)) a in
        Array.stable_sort (fun (_, cont1) (_, cont2) -> compare cont1 cont2) a;
        let l = Array.to_list a in
        let l = list_group snd l in
        let l =
          List.sort
            (fun (_, l1) (_, l2) ->
               - compare (List.length l1) (List.length l2)) l in
        match l with
          [] ->
            assert false
        | [(cont, _)] ->
            Js_simpl.block
              (compile_branch st [] cont handler backs frontier interm)
        | [cont1, [(n, _)]; cont2, _] | [cont2, _; cont1, [(n, _)]] ->
            Js_simpl.block (compile_if st (J.EBin (J.EqEqEq, int n, e))
                              cont1 cont2 handler backs frontier interm succs)
        | (cont, l') :: rem ->
            let l =
              List.flatten
                (List.map
                   (fun (cont, l) ->
                      match List.rev l with
                        [] ->
                          assert false
                      | (i, _) :: r ->
                          List.rev
                            ((J.ENum (float i),
                              Js_simpl.statement_list
                                (compile_branch
                                   st [] cont handler backs frontier interm @
                                 if
                                   never_continue st cont frontier interm succs
                                 then
                                   []
                                 else
                                   [J.Break_statement None]))
                               ::
                             List.map
                             (fun (i, _) -> (J.ENum (float i), [])) r))
                   rem)
            in
            J.Switch_statement
              (e, l,
               Some (Js_simpl.statement_list
                       (compile_branch
                          st [] cont handler backs frontier interm)))
      in
      let (st, queue) =
        if Array.length a1 = 0 then
          let ((px, cx), queue) = access_queue queue x in
          ([build_switch (J.EAccess(cx, J.ENum 0.)) a2], queue)
        else if Array.length a2 = 0 then
          let ((px, cx), queue) = access_queue queue x in
          ([build_switch cx a1], queue)
        else
          (* The variable x is accessed several times,
             so we can directly refer to it *)
          (Js_simpl.if_statement
              (J.EBin(J.EqEqEq, J.EUn (J.Typeof, var x),
                      J.EStr ("number", `Bytes)))
              (build_switch (var x) a1)
              false
              (build_switch (J.EAccess(var x, J.ENum 0.)) a2)
              false,
           queue)
      in
      flush_all queue st
  | Pushtrap _ ->
      assert false
  | Poptrap cont ->
      flush_all queue (compile_branch st [] cont None backs frontier interm)
  in
  if debug () then begin
    match last with
      Branch _ | Poptrap _ | Pushtrap _ | Return _ | Raise _ | Stop -> ()
    | Switch _ | Cond _ -> Format.eprintf "}@]@ "
  end;
  res

and compile_argument_passing ctx queue (pc, args) backs continuation =
  if args = [] then
    continuation queue
  else begin
    let block = AddrMap.find pc ctx.Ctx.blocks in
    (* We flush on backward edged.  We do not need to flush on forward
       edges, as the block parameter variables are fresh. *)
    (*FIX: this is overly aggressive: we should instead keep track of
      dependencies between queued variables and take this into account
      to perform parallel renaming. *)
    let cont queue =
      parallel_renaming ctx block.params args continuation queue in
    if AddrSet.mem pc backs then
      flush_all queue (cont [])
    else
      cont queue
  end

and compile_exn_handling ctx queue (pc, args) handler continuation =
  if pc < 0 then
    continuation queue
  else
    let block = AddrMap.find pc ctx.Ctx.blocks in
    match block.handler with
        None ->
          continuation queue
      | Some (x0, (h_pc, h_args)) ->
        let old_args =
          match handler with
              Some (y, (old_pc, old_args)) ->
                assert (Var.compare x0 y = 0 && old_pc = h_pc &&
                    List.length old_args = List.length h_args);
                old_args
            | None ->
              []
        in
        (* When an extra block is inserted during code generation,
           args is [] *)
        let m =
          Subst.build_mapping (if args = [] then [] else block.params) args in
        let h_block = AddrMap.find h_pc ctx.Ctx.blocks in
        let rec loop continuation old args params queue =
          match args, params with
            [], [] ->
              continuation queue
          | x :: args, y :: params ->
(*Format.eprintf "ZZZ@.";*)
              let (z, old) =
                match old with [] -> (None, []) | z :: old -> (Some z, old)
              in
              let x' =
                try Some (VarMap.find x m) with Not_found -> Some x in
              if Var.compare x x0 = 0 || x' = z then
                loop continuation old args params queue
              else begin
               let ((px, cx), queue) = access_queue queue x in
(*Format.eprintf "%a := %a@." Var.print y Var.print x;*)
               let (st, queue) =
(*FIX: we should flush only the variables we need rather than doing this;
       do the same for closure free variables *)
                 match 2 (*ctx.Ctx.live.(Var.idx y)*) with
                   0 -> assert false
                 | 1 -> enqueue queue px y cx
                 | _ -> flush_queue queue px
                          [J.Variable_statement [Var.to_string y, Some cx]]
               in
               st @ loop continuation old args params queue
              end
          | _ ->
              assert false
        in
(*
Format.eprintf "%d ==> %d/%d/%d@." pc (List.length h_args) (List.length h_block.params) (List.length old_args);
*)
        loop continuation old_args h_args h_block.params queue

and compile_branch st queue ((pc, _) as cont) handler backs frontier interm =
  compile_argument_passing st.ctx queue cont backs (fun queue ->
  compile_exn_handling st.ctx queue cont handler (fun queue ->
  if AddrSet.mem pc backs then begin
    let label =
      match st.loop_stack with
        [] ->
          assert false
      | (pc', _) :: rem ->
          if pc = pc' then
            None
          else begin
            let (lab, used) = List.assoc pc rem in
            used := true;
            Some (Code.string_of_ident lab)
          end
    in
    if debug () then begin
      if label = None then
        Format.eprintf "continue;@ "
      else
        Format.eprintf "continue (%d);@ " pc
    end;
    flush_all queue [J.Continue_statement label]
  end else if AddrSet.mem pc frontier || AddrMap.mem pc interm then begin
    if debug () then Format.eprintf "(br %d)@ " pc;
    flush_all queue (compile_branch_selection pc interm)
  end else
    compile_block st queue pc frontier interm))

and compile_branch_selection pc interm =
  try
    let (pc, (x, i)) = AddrMap.find pc interm in
    if debug () then Format.eprintf "@ %a=%d;" Code.Var.print x i;
    J.Variable_statement [Var.to_string x, Some (int i)] ::
    compile_branch_selection pc interm
  with Not_found ->
    []

and compile_closure ctx (pc, args) =
  let st =
    { visited_blocks = AddrSet.empty; loops = AddrSet.empty; loop_stack = [];
      all_succs = Hashtbl.create 17; succs = Hashtbl.create 17;
      backs = Hashtbl.create 17; preds = Hashtbl.create 17;
      interm_idx = -1; ctx = ctx; blocks = ctx.Ctx.blocks }
  in
  build_graph st pc AddrSet.empty;
  let current_blocks = st.visited_blocks in
  st.visited_blocks <- AddrSet.empty;
  if debug () then Format.eprintf "@[<hov 2>closure{@,";
  let res =
    compile_branch st [] (pc, args) None
      AddrSet.empty AddrSet.empty AddrMap.empty
  in
  if
    AddrSet.cardinal st.visited_blocks <> AddrSet.cardinal current_blocks
  then begin
    Format.eprintf "Some blocks not compiled!@."; assert false
  end;
  if debug () then Format.eprintf "}@]@ ";
  Js_simpl.source_elements res

let compile_program standalone ctx pc =
  let res = compile_closure ctx (pc, []) in
  if debug () then Format.eprintf "@.@.";
(*
  Primitive.list_used ();
*)
  if standalone then
    let f = J.EFun (None, [], generate_apply_funs res) in
    [J.Statement (J.Expression_statement (J.ECall (f, [])))]
  else
    let f = J.EFun (None, [Var.to_string (Var.fresh ())],
                    generate_apply_funs res) in
    [J.Statement (J.Expression_statement f)]

(**********************)

let list_missing l =
  if l <> [] then begin
    Format.eprintf "Missing primitives:@.";
    List.iter (fun nm -> Format.eprintf "  %s@." nm) l
  end

let f ch ?(standalone=true) ((pc, blocks, _) as p) live_vars =
  let mutated_vars = Freevars.f p in
  let t' = Util.Timer.make () in
  let ctx = Ctx.initial blocks live_vars mutated_vars in
  let p = compile_program standalone ctx pc in
  if !compact then Pretty_print.set_compact ch true;
  if standalone then begin
    Pretty_print.string ch
      "// This program was compiled from OCaml by js_of_ocaml 1.0";
    Pretty_print.newline ch;
    let missing = Linker.resolve_deps !compact ch (Primitive.get_used ()) in
    list_missing missing
  end;
  Hashtbl.clear add_names;
  let res = Js_output.program ch p in
  if times () then Format.eprintf "  code gen.: %a@." Util.Timer.print t';
  res


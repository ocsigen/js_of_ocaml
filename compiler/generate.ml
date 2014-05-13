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

let debug = Option.Debug.find "gen"
let times = Option.Debug.find "times"

open Code
open Util

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

module Share = struct
  type 'a aux = {
    strings : 'a StringMap.t;
    applies : 'a IntMap.t;
    prims :   'a StringMap.t;
  }


  let empty_aux = { prims = StringMap.empty;
                    strings = StringMap.empty;
                    applies = IntMap.empty }

  type t = {
    mutable count : int aux;
    mutable vars : J.ident aux;
  }

  let add_string s t =
    let n = try StringMap.find s t.strings with Not_found -> 0 in
    {t with strings = StringMap.add s (n+1) t.strings}

  let add_prim s t =
    let n = try StringMap.find s t.prims with Not_found -> 0 in
    {t with prims  = StringMap.add s (n+1) t.prims}

  let add_special_prim_if_exists s t =
    if Primitive.exists s
    then {t with prims  = StringMap.add s (-1) t.prims}
    else t

  let add_apply i t =
    let n = try IntMap.find i t.applies with Not_found -> 0 in
    {t with applies = IntMap.add i (n+1) t.applies }

  let add_code_string s share =
    let share = add_string s share in
    add_prim "caml_new_string" share

  let add_code_istring s share =
    add_string s share

  let rec get_constant c t =
    match c with
      | String s -> add_code_string s t
      | IString s -> add_code_istring s t
      | Tuple (_,args) -> Array.fold_left (fun t c ->
        get_constant c t) t args
      | _ -> t

  let add_args args t =
    List.fold_left(fun t a ->
      match a with
        | Pc c -> get_constant c t
        | _ -> t) t args


  let get (_, blocks, _) : t =
    let count = AddrMap.fold
      (fun _ block share ->
        List.fold_left
          (fun share i ->
            match i with
              | Let (_, Constant c) -> get_constant c share
              | Let (_, Apply (_,args,false)) ->
                add_apply (List.length args) share
              | Let (_, Prim (Extern name, args)) ->
                let name = Primitive.resolve name in
                let share =
                  if Primitive.exists name
                  then add_prim name share
                  else share in
                add_args args share
              | Let (_, Prim (_, args)) ->
                add_args args share
              | _ -> share
          )
          share block.body)
      blocks empty_aux in

    let count = List.fold_left (fun acc x ->
        add_special_prim_if_exists x acc)
        count
        ["caml_trampoline";"caml_trampoline_return";"caml_wrap_exception"] in
    {count; vars = empty_aux}

  let get_string gen s t =
    (* disabled because it is done later on Js ast *)
    (* try *)
    (*   let c = StringMap.find s t.count.strings in *)
    (*   if c > 1 *)
    (*   then *)
    (*     try *)
    (*       J.EVar (StringMap.find s t.vars.strings) *)
    (*     with Not_found -> *)
    (*       let x = Var.fresh() in *)
    (*       Var.name x "str"; *)
    (*       let v = J.V x in *)
    (*       t.vars <- { t.vars with strings = StringMap.add s v t.vars.strings }; *)
    (*       J.EVar v *)
    (*   else *)
    (*     gen s *)
    (* with Not_found-> *)
      gen s

  let get_prim gen s t =
    let s = Primitive.resolve s in
    try
      let c = StringMap.find s t.count.prims in
      if c > 1 || c = -1
      then
        try
          J.EVar (StringMap.find s t.vars.prims)
        with Not_found ->
          let x = Var.fresh() in
          Code.Var.name x s;
          let v = J.V x in
          t.vars <- { t.vars with prims = StringMap.add s v t.vars.prims };
          J.EVar v
      else
        gen s
    with Not_found ->
      gen s


  let get_apply gen n t =
    try
      let c = IntMap.find n t.count.applies in
      if c > 1
      then
        try
          J.EVar (IntMap.find n t.vars.applies)
        with Not_found ->
          let x = Var.fresh() in
          Code.Var.name x (Printf.sprintf "caml_call_gen%d" n);
          let v = J.V x in
          t.vars <- { t.vars with applies = IntMap.add n v t.vars.applies };
          J.EVar v
      else
        gen n
    with Not_found ->
      gen n

end


module Ctx = struct
  type t =
    { mutable blocks : block AddrMap.t;
      live : int array;
      mutated_vars : VarSet.t AddrMap.t;
      share: Share.t }

  let initial b l v share =
    { blocks = b; live = l; mutated_vars = v; share }

end

let var x = J.EVar (J.V x)
let int n = J.ENum (float n)
let int32 n = J.ENum (Int32.to_float n)
let one = int 1
let zero = int 0
let bool e = J.ECond (e, one, zero)
let boolnot e = J.ECond (e, zero, one)
let val_float f = f (*J.EArr [Some (J.ENum 253.); Some f]*)
let float_val e = e (*J.EAccess (e, one)*)

(****)

let float_const f = val_float (J.ENum f)

let s_var name = J.EVar (J.S {J.name=name; J.var = None})

let str_js s = J.EStr (s,`Bytes)


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

let max_depth = 10

let rec constant_rec ~ctx x level instrs =
  match x with
    String s ->
      let e = Share.get_string str_js s ctx.Ctx.share in
      let p = Share.get_prim s_var "caml_new_string" ctx.Ctx.share in
      J.ECall (p,[e]), instrs
  | IString s ->
      Share.get_string str_js s ctx.Ctx.share, instrs
  | Float f ->
      float_const f, instrs
  | Float_array a ->
      J.EArr (Some (int Obj.double_array_tag) ::
              Array.to_list (Array.map (fun f -> Some (float_const f)) a)),
      instrs
  | Int64 i ->
      J.EArr [Some (int 255);
              Some (int (Int64.to_int i land 0xffffff));
              Some (int (Int64.to_int (Int64.shift_right i 24) land 0xffffff));
              Some (int (Int64.to_int (Int64.shift_right i 48) land 0xffff))],
      instrs
  | Tuple (tag, a) ->
      let split = level = max_depth in
      let level = if split then 0 else level + 1 in
      let (l, instrs) =
        List.fold_left
          (fun (l, instrs) cc ->
             let (js, instrs) = constant_rec ~ctx cc level instrs in
             js::l, instrs)
          ([], instrs) (Array.to_list a)
      in
      let (l, instrs) =
        if split then
          List.fold_left
            (fun (acc,instrs) js ->
               match js with
               | J.EArr _ ->
                   let v = Code.Var.fresh () in
                   Var.name v "partial";
                   let instrs =
                     J.Variable_statement [J.V v, Some (js,J.N)] :: instrs in
                   Some (J.EVar (J.V v))::acc,instrs
               | _ ->
                   Some js :: acc,instrs)
            ([],instrs) l
        else
          List.rev_map (fun x -> Some x) l, instrs
      in
      J.EArr (Some (int tag) :: l), instrs
  | Int i-> int32 i, instrs

let constant ~ctx x level =
  let (expr, instr) = constant_rec ~ctx x level [] in
  (expr, List.rev instr)


type queue_elt = {
  prop : int;
  cardinal : int;
  ce : J.expression;
  pc : int;
  deps : Code.VarSet.t
}

let access_queue queue x =
  try
    let elt = List.assoc x queue in
    if elt.cardinal = 1
    then
      (elt.prop,elt.ce), List.remove_assoc x queue
    else
      ((elt.prop,elt.ce), List.map (function (x',elt) when x = x' -> x',{elt with cardinal=pred elt.cardinal} | x -> x) queue)
  with Not_found ->
    ((const_p, var x), queue)

let access_queue' ~ctx queue x =
  match x with
    | Pc c ->
      let js,instrs = constant ~ctx c max_depth in
      assert (instrs = []); (* We only have simple constants here *)
      (const_p, js), queue
    | Pv x -> access_queue queue x

let access_queue_may_flush queue v x =
  let tx,queue = access_queue queue x in
  let _,instrs,queue = List.fold_left (fun (deps,instrs,queue) ((y,elt) as eq) ->
      if Code.VarSet.exists (fun p -> Code.VarSet.mem p deps) elt.deps
      then (Code.VarSet.add ( y) deps),
        (J.Variable_statement [J.V y,
                               Some (elt.ce,
                                     J.Loc (DebugAddr.of_addr elt.pc))]
         :: instrs),
        queue
      else  deps,instrs,(eq::queue)
    ) (Code.VarSet.singleton ( v),[],[]) queue
  in instrs,(tx,List.rev queue)



let should_flush cond prop = cond <> const_p && cond + prop >= flush_p

let flush_queue expr_queue prop l =
  let (instrs, expr_queue) =
    if prop >= flush_p then (expr_queue, []) else
      List.partition (fun (y, elt) -> should_flush prop elt.prop) expr_queue
  in
  let instrs =
    List.map (fun (x, elt) ->
                J.Variable_statement
                  [J.V x, Some (elt.ce,J.Loc (DebugAddr.of_addr elt.pc))])
      instrs
  in
  (List.rev_append instrs l, expr_queue)

let flush_all expr_queue l = fst (flush_queue expr_queue flush_p l)

let enqueue expr_queue prop x ce pc cardinal acc =
  let (instrs, expr_queue) =
    if Option.Optim.compact () then
      if is_mutable prop then
        flush_queue expr_queue prop []
      else
        [], expr_queue
    else flush_queue expr_queue flush_p []
  in
  let deps = Js_simpl.get_variable Code.VarSet.empty ce in
  let deps = List.fold_left (fun deps (x',elt) ->
      if Code.VarSet.mem ( x') deps
      then Code.VarSet.union elt.deps deps
      else deps) deps expr_queue
  in
  (instrs @ acc , (x, {prop; ce; pc; cardinal; deps}) :: expr_queue)

(****)

type state =
  { all_succs : (int, AddrSet.t) Hashtbl.t;
    succs : (int, int list) Hashtbl.t;
    backs : (int, AddrSet.t) Hashtbl.t;
    preds : (int, int) Hashtbl.t;
    mutable loops : AddrSet.t;
    mutable loop_stack : (addr * (J.Label.t * bool ref)) list;
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
  not (AddrSet.mem pc frontier || AddrMap.mem pc interm)
    &&
  AddrSet.is_empty d

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
      let instrs,((px, cx), queue) = access_queue_may_flush queue y x in
      let (st, queue) = flush_queue queue px (instrs@[J.Variable_statement [ J.V y, Some (cx,J.N)]])
      in
      st @ continuation queue)
    continuation l queue

(****)

let apply_fun_raw f params =
  let n = List.length params in
  J.ECond (J.EBin (J.EqEq, J.EDot (f, "length"),
                   J.ENum (float n)),
           J.ECall (f, params),
           J.ECall (s_var "caml_call_gen",
                    [f; J.EArr (List.map (fun x -> Some x) params)]))

let generate_apply_fun n =
  let f' = Var.fresh () in
  let f = J.V f' in
  Code.Var.name f' "fun";
  let params =
    Array.to_list (Array.init n (fun i ->
      let a = Var.fresh () in
      Var.name a ("var"^(string_of_int i));
      J.V a))
  in
  let f' = J.EVar f in
  let params' = List.map (fun x -> J.EVar x) params in
  J.EFun (None, f :: params,
          [J.Statement
              (J.Return_statement
                 (Some (apply_fun_raw f' params'), J.N))],
          J.N)

let apply_fun ctx f params =
  if Option.Optim.inline_callgen ()
  then apply_fun_raw f params
  else
    let y = Share.get_apply generate_apply_fun (List.length params) ctx.Ctx.share in
    J.ECall (y, f::params)

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
  Primitive.register name k None None;
  Hashtbl.add internal_primitives name (Some f)

let register_un_prim name k f =
  register_prim name k
    (fun l queue ctx ->
       match l with
         [x] ->
           let ((px, cx), queue) = access_queue' ~ctx queue x in
           (f cx, or_p (kind k) px, queue)
         | _ ->
           assert false)

let register_un_prim_ctx name k f =
  register_prim name k
    (fun l queue ctx ->
       match l with
         [x] ->
           let ((px, cx), queue) = access_queue' ~ctx queue x in
           (f ctx cx, or_p (kind k) px, queue)
         | _ ->
           assert false)

let register_bin_prim name k f =
  register_prim name k
    (fun l queue ctx ->
       match l with
         [x;y] ->
           let ((px, cx), queue) = access_queue' ~ctx queue x in
           let ((py, cy), queue) = access_queue' ~ctx queue y in
           (f cx cy, or_p (kind k) (or_p px py), queue)
         | _ -> assert false)

let register_tern_prim name f =
  register_prim name `Mutator
    (fun l queue ctx ->
       match l with
         [x;y;z] ->
           let ((px, cx), queue) = access_queue' ~ctx queue x in
           let ((py, cy), queue) = access_queue' ~ctx queue y in
           let ((pz, cz), queue) = access_queue' ~ctx queue z in
           (f cx cy cz, or_p mutator_p (or_p px (or_p py pz)), queue)
         | _ ->
           assert false)

let register_un_math_prim name prim =
  register_un_prim name `Pure
    (fun cx -> J.ECall (J.EDot (s_var "Math", prim), [cx]))

let register_bin_math_prim name prim =
  register_bin_prim name `Pure
    (fun cx cy -> J.ECall (J.EDot (s_var "Math", prim), [cx; cy]))

let _ =
  register_un_prim_ctx  "%caml_format_int_special" `Pure
    (fun ctx cx ->
      let p = Share.get_prim s_var "caml_new_string" ctx.Ctx.share in
      J.ECall (p, [J.EBin (J.Plus,str_js "",cx)]));
  register_bin_prim "caml_array_unsafe_get" `Mutable
    (fun cx cy -> J.EAccess (cx, J.EBin (J.Plus, cy, one)));
  register_bin_prim "caml_string_get" `Mutable
    (fun cx cy -> J.ECall (J.EDot (cx, "safeGet"), [cy]));
  register_bin_prim "%int_add" `Pure
    (fun cx cy -> to_int (J.EBin (J.Plus,cx,cy)));
  register_bin_prim "%int_sub" `Pure
    (fun cx cy -> to_int (J.EBin (J.Minus,cx,cy)));
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
    (fun cx cy -> to_int (J.EBin (J.Lsr, cx, cy)));
  register_bin_prim "%int_asr" `Pure
    (fun cx cy -> J.EBin (J.Asr, cx, cy));
  register_un_prim "%int_neg" `Pure
    (fun cx -> to_int (J.EUn (J.Neg, cx)));
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
    (fun cx -> J.ENew (s_var "MlWrappedString", Some [cx]));
  register_tern_prim "caml_js_set"
    (fun cx cy cz -> J.EBin (J.Eq, J.EAccess (cx, cy), cz));
  register_bin_prim "caml_js_get" `Mutable
    (fun cx cy -> J.EAccess (cx, cy));
  register_bin_prim "caml_js_delete" `Mutable
    (fun cx cy -> J.EUn(J.Delete, J.EAccess (cx, cy)));
  register_bin_prim "caml_js_equals" `Mutable
    (fun cx cy -> bool (J.EBin (J.EqEq, cx, cy)));
  register_bin_prim "caml_js_instanceof" `Pure
    (fun cx cy -> bool (J.EBin(J.InstanceOf, cx, cy)));
  register_un_prim "caml_js_typeof" `Pure
    (fun cx -> J.EUn(J.Typeof, cx))

(****)

let varset_disjoint s s' = not (VarSet.exists (fun x -> VarSet.mem x s') s)

let is_ident =
  let l = Array.init 256 (fun i ->
    let c = Char.chr i in
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' || c = '$'
    then 1
    else if (c >= '0' && c <='9')
    then 2
    else 0
  ) in
  fun s ->
    try
      for i = 0 to String.length s - 1 do
        let code = l.(Char.code(s.[i])) in
        if i = 0 then assert (code = 1) else assert (code >= 1)
      done;
      true
    with _ -> false

let ident_from_string s =
  match Util.split_char '.' s with
    | [] -> assert false
    | [x] ->
      if not (is_ident x)
      then Format.eprintf "Warning: %S is not a valid identifier; the generated code might be incorrect.@." x;
      s_var x
    | (x::xs) as l ->
      Format.eprintf "Warning: %S should be written (Js.Unsafe.variable %S)##%s@." s x (String.concat "##" xs);
      if not (List.for_all is_ident l)
      then Format.eprintf "Warning: %S is not a valid identifier; the generated code might be incorrect.@." s;
      List.fold_left (fun e i -> J.EDot(e,i)) (s_var x) xs

let rec group_closures_rec closures req =
  match closures with
    [] ->
      ([], VarSet.empty)
  | ((var, vars, req_tc, clo) as elt) :: rem ->
    let req = VarSet.union vars req in
    let req = VarSet.union req req_tc in
      let (closures', prov) = group_closures_rec rem req in
      match closures' with
      | [] ->
        ([elt] :: closures', VarSet.singleton var)
      | _ when varset_disjoint prov req ->
        ([elt] :: closures', VarSet.singleton var)
      | l :: r -> ((elt :: l) :: r, VarSet.add var prov)

let group_closures l = fst (group_closures_rec l VarSet.empty)

let rec collect_closures ctx l =
  match l with
    Let (x, Closure (args, ((pc, _) as cont))) :: rem ->
      let clo = compile_closure ctx cont in

      let all_vars = AddrMap.find pc ctx.Ctx.mutated_vars in

      let tc = (new Js_tailcall.tailcall) in
      ignore(tc#sources clo);
      let req_tc = (tc#get) in

      let vars = VarSet.remove x all_vars in
      let cl =
        J.EFun (None, List.map (fun v -> J.V v) args, clo, J.Loc (DebugAddr.of_addr pc))
      in
      let (l', rem') = collect_closures ctx rem in
      ((x, vars, req_tc, cl) :: l', rem')
  | _ ->
    ([], l)

(****)

and translate_expr ctx queue x e level =
  match e with
    Const i ->
      (int32 i, const_p, queue),[]
  | Apply (x, l, true) ->
      let ((px, cx), queue) = access_queue queue x in
      let (args, prop, queue) =
        List.fold_right
          (fun x (args, prop, queue) ->
             let ((prop', cx), queue) =
               access_queue queue x in (cx :: args, or_p prop prop', queue))
          l ([], or_p px mutator_p, queue)
      in
      (J.ECall (cx, args), prop, queue),[]
  | Apply (x, l, false) ->
      let (args, prop, queue) =
        List.fold_right
          (fun x (args, prop, queue) ->
             let ((prop', cx), queue) =
               access_queue queue x in (cx :: args, or_p prop prop', queue))
          l ([], mutator_p, queue)
      in
      let ((prop', f), queue) = access_queue queue x in
      let prop = or_p prop prop' in

      let e = apply_fun ctx f args in
      (e, prop, queue),[]
  | Block (tag, a) ->
      let (contents, prop, queue) =
        List.fold_right
          (fun x (args, prop, queue) ->
             let ((prop', cx), queue) = access_queue queue x in
             (Some cx :: args, or_p prop prop', queue))
          (Array.to_list a) ([], const_p, queue)
      in
      (J.EArr (Some (int tag) :: contents), prop, queue),[]
  | Field (x, n) ->
      let ((px, cx), queue) = access_queue queue x in
      (J.EAccess (cx, int (n + 1)), or_p px mutable_p, queue),[]
  | Closure _ ->
    (* this is done by translate_instr *)
      assert false
  | Constant c ->
      let js, instrs = constant ~ctx c level in
      (js, const_p, queue), instrs
  | Prim (p, l) ->
    let res = match p, l with
        Vectlength, [x] ->
        let ((px, cx), queue) = access_queue' ~ctx queue x in
        (J.EBin (J.Minus, J.EDot (cx, "length"), one), px, queue)
      | Array_get, [x; y] ->
        let ((px, cx), queue) = access_queue' ~ctx queue x in
        let ((py, cy), queue) = access_queue' ~ctx queue y in
        (J.EAccess (cx, J.EBin (J.Plus, cy, one)),
         or_p mutable_p (or_p px py), queue)
      | Extern "caml_js_var", [Pc (String nm)] ->
        (ident_from_string nm, const_p, queue)
      | Extern "caml_js_const", [Pc (String nm)] ->
        (ident_from_string nm, const_p, queue)
      | Extern "caml_js_expr", [Pc (String nm)] ->
        begin
          try
            let lex = Parse_js.lexer_from_string nm in
            let e = Parse_js.parse_expr lex in
            (e, const_p, queue)
          with Parse_js.Parsing_error pi ->
            failwith (Printf.sprintf "Parsing error %S at l:%d col:%d" nm (pi.Parse_info.line + 1) pi.Parse_info.col)
        end
      | Extern "%caml_js_opt_call", Pv f :: Pv o :: l ->
        let ((pf, cf), queue) = access_queue queue f in
        let ((po, co), queue) = access_queue queue o in
        let (args, prop, queue) =
          List.fold_right
            (fun x (args, prop, queue) ->
               let ((prop', cx), queue) = access_queue' ~ctx queue x in
               (cx :: args, or_p prop prop', queue)
            )
            l ([], mutator_p, queue)
        in
        (J.ECall (J.EDot (cf, "call"), co :: args),
         or_p (or_p pf po) prop, queue)
      | Extern "%caml_js_opt_fun_call", Pv f :: l ->
        let ((pf, cf), queue) = access_queue queue f in
        let (args, prop, queue) =
          List.fold_right
            (fun x (args, prop, queue) ->
               let ((prop', cx), queue) = access_queue' ~ctx queue x in
               (cx :: args, or_p prop prop', queue))
            l ([], mutator_p, queue)
        in
        (J.ECall (cf, args), or_p pf prop, queue)
      | Extern "%caml_js_opt_meth_call", Pv o :: Pc (String m) :: l ->
        let ((po, co), queue) = access_queue queue o in
        let (args, prop, queue) =
          List.fold_right
            (fun x (args, prop, queue) ->
               let ((prop', cx), queue) = access_queue' ~ctx queue x in
               (cx :: args, or_p prop prop', queue))
            l ([], mutator_p, queue)
        in
        (J.ECall (J.EDot (co, m), args), or_p po prop, queue)
      | Extern "%caml_js_opt_new", Pv c :: l ->
        let ((pc, cc), queue) = access_queue queue c in
        let (args, prop, queue) =
          List.fold_right
            (fun x (args, prop, queue) ->
               let ((prop', cx), queue) = access_queue' ~ctx queue x in
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
      | Extern "caml_js_delete", [Pv o; Pc (String f)] ->
        let ((po, co), queue) = access_queue queue o in
        (J.EUn(J.Delete, J.EDot (co, f)), or_p po mutator_p, queue)
      | Extern "%overrideMod", [Pc (String m);Pc (String f)] ->
        s_var (Printf.sprintf "caml_%s_%s" m f), const_p,queue
      | Extern "%overrideMod", _ ->
        assert false
      | Extern "%caml_js_opt_object", fields ->
        let rec build_fields queue l =
          match l with
              [] ->
              (const_p, [], queue)
            | Pc (String nm) :: x :: r ->
              let ((prop, cx), queue) = access_queue' ~ctx queue x in
              let (prop', r', queue') = build_fields queue r in
              (or_p prop prop', (J.PNS nm, cx) :: r', queue)
            | _ ->
              assert false
        in
        let (prop, fields, queue) = build_fields queue fields in
        (J.EObj fields, prop, queue)
      | Extern name, l ->
        begin
          let name = Primitive.resolve name in
          match internal_prim name with
            | Some f -> f l queue ctx
            | None ->
              if name.[0] = '%'
              then failwith (Printf.sprintf "Unresolved interal primitive: %s" name);
              let prim = Share.get_prim s_var name ctx.Ctx.share in
              let prim_kind = kind (Primitive.kind name) in
              let (args, prop, queue) =
                List.fold_right
                  (fun x (args, prop, queue) ->
                     let ((prop', cx), queue) = access_queue' ~ctx queue x in
                     (cx :: args, or_p prop prop', queue))
                  l ([], prim_kind, queue)
              in
              (J.ECall (prim, args), prop, queue)
        end
      | Not, [x] ->
        let ((px, cx), queue) = access_queue' ~ctx  queue x in
        (J.EBin (J.Minus, one, cx), px, queue)
      | Lt, [x; y] ->
        let ((px, cx), queue) = access_queue' ~ctx  queue x in
        let ((py, cy), queue) = access_queue' ~ctx  queue y in
        (bool (J.EBin (J.Lt, cx, cy)), or_p px py, queue)
      | Le, [x; y] ->
        let ((px, cx), queue) = access_queue' ~ctx  queue x in
        let ((py, cy), queue) = access_queue' ~ctx  queue y in
        (bool (J.EBin (J.Le, cx, cy)), or_p px py, queue)
      | Eq, [x; y] ->
        let ((px, cx), queue) = access_queue' ~ctx  queue x in
        let ((py, cy), queue) = access_queue' ~ctx  queue y in
        (bool (J.EBin (J.EqEqEq, cx, cy)), or_p px py, queue)
      | Neq, [x; y] ->
        let ((px, cx), queue) = access_queue' ~ctx  queue x in
        let ((py, cy), queue) = access_queue' ~ctx  queue y in
        (bool (J.EBin (J.NotEqEq, cx, cy)), or_p px py, queue)
      | IsInt, [x] ->
        let ((px, cx), queue) = access_queue' ~ctx  queue x in
        (J.EBin(J.EqEqEq, J.EUn (J.Typeof, cx), (Share.get_string str_js "number" ctx.Ctx.share)),
         px, queue)
      | Ult, [x; y] ->
        let ((px, cx), queue) = access_queue' ~ctx  queue x in
        let ((py, cy), queue) = access_queue' ~ctx  queue y in
        (bool (J.EBin (J.Or, J.EBin (J.Lt, cy, int 0),
                       J.EBin (J.Lt, cx, cy))),
         or_p px py, queue)
      | (Vectlength | Array_get | Not | IsInt | Eq |
         Neq | Lt | Le | Ult), _ ->
        assert false
    in res,[]

and translate_closures ctx expr_queue l pc =
  match l with
    [] ->
      ([], expr_queue)
  | [(x, vars, req_tc, cl)] :: rem ->
      let vars =
        vars
        >> VarSet.elements
        >> List.map (fun v -> J.V v)
      in
      let prim name = Share.get_prim s_var name ctx.Ctx.share in
      let defs =
        Js_tailcall.rewrite [x,cl,J.Loc (DebugAddr.of_addr pc),req_tc] prim in
      let rec return_last x = function
        | [] -> [J.Statement (J.Return_statement (Some (J.EVar (J.V x)),J.N))]
        | [J.Variable_statement l as sts]  ->
          let l' = List.rev l in
          begin match l' with
          | (J.V x',Some (e,pc)) :: rem when x = x' ->
              [J.Statement (J.Variable_statement (List.rev rem));
               J.Statement (J.Return_statement (Some e,pc))]
          | _ -> [J.Statement sts]
          end
        | y::xs -> J.Statement (y) :: return_last x xs
      in
      let statements =
        if vars = [] then defs else
          [J.Variable_statement [
            J.V x,
            Some (
              J.ECall (J.EFun (None, vars, return_last x defs, J.N),
                       List.map (fun x -> J.EVar x) vars), J.N)]]
      in

      let (st, expr_queue) =
        match ctx.Ctx.live.(Var.idx x),statements with
        | 0, _ -> assert false
        | 1, [J.Variable_statement [(J.V x',Some (e', _))]] when x == x' ->
            enqueue expr_queue flush_p x e' pc 1 []
        | _ -> flush_queue expr_queue flush_p statements
      in
      let (st', expr_queue) = translate_closures ctx expr_queue rem pc in
      (st @ st', expr_queue)
  | l :: rem ->
      let names =
        List.fold_left (fun s (x, _, _, _) -> VarSet.add x s) VarSet.empty l in
      let vars =
        List.fold_left (fun s (_, s', _, _) -> VarSet.union s s') VarSet.empty l
      in
      let vars =
        VarSet.diff vars names
        >> VarSet.elements
        >> List.map (fun v -> J.V v)
      in
      let defs' =
        List.map
          (fun (x, _, req_tc, cl) -> x,cl,J.Loc (DebugAddr.of_addr pc),req_tc)
          l
      in
      let prim name = Share.get_prim s_var name ctx.Ctx.share in
      let defs = Js_tailcall.rewrite defs' prim in
      let statements =
        if vars = [] then defs
        else begin
          let tbl = Var.fresh () in
          Var.name tbl "funenv";
          let arr =
            J.EArr
              (List.map (fun (x, _, _, _) -> Some (J.EVar (J.V x))) l)
          in
          let assgn =
            List.fold_left
              (fun (l, n) (x, _, _, _) ->
                 ((J.V x,
                   Some (J.EAccess (J.EVar (J.V tbl), int n),
                         J.Loc (DebugAddr.of_addr pc))) :: l,
                  n + 1))
              ([], 0) l
          in
          [J.Variable_statement
            ((J.V tbl,
              Some
                (J.ECall
                   (J.EFun (None, vars,
                            List.map (fun s -> J.Statement s) defs
                            @ [J.Statement (J.Return_statement (Some arr,J.N))],
                            J.N),
                    List.map (fun x -> J.EVar x) vars), J.N)) ::
              List.rev (fst assgn))]
        end
      in
      let (st, expr_queue) = flush_queue expr_queue flush_p statements in
      let (st', expr_queue) = translate_closures ctx expr_queue rem pc in
      (st @ st', expr_queue)

and translate_instr ctx expr_queue (pc : addr) instr =
  match instr with
    [] ->
      ([], expr_queue)
  | Let (_, Prim (Extern "debugger",_))::rem ->
    let ins =
      if Option.Optim.debugger ()
      then J.Debugger_statement J.N
      else J.Empty_statement J.N in
    let st = flush_all expr_queue [ins] in
    let (instrs, expr_queue) = translate_instr ctx [] pc rem in
    (st @ instrs, expr_queue)
  | Let (_, Closure _) :: _ ->
      let (l, rem) = collect_closures ctx instr in
      let l = group_closures l in
      let (st, expr_queue) = translate_closures ctx expr_queue l pc in
      let (instrs, expr_queue) = translate_instr ctx expr_queue pc rem in
      (st @ instrs, expr_queue)
  | i :: rem ->
      let (st, expr_queue) =
        match i with
          Let (x, e) ->
            let (ce, prop, expr_queue),instrs = translate_expr ctx expr_queue x e 0 in
            begin match ctx.Ctx.live.(Var.idx x),e with
            | 0,_ -> flush_queue expr_queue prop (instrs@[J.Expression_statement
                                                    (ce, J.Loc (DebugAddr.of_addr pc))])
            | 1,_ -> enqueue expr_queue prop x ce pc 1 instrs
            (* We could inline more.
               size_v : length of the variable after serialization
               size_c : length of the constant after serialization
               num : number of occurence
               size_c * n < size_v * n + size_v + 1 + size_c
            *)
            | n,(Const _| Constant (Int _|Float _)) ->
                   enqueue expr_queue prop x ce pc n instrs
            | _ -> flush_queue expr_queue prop
                     (instrs@
                      [J.Variable_statement
                         [J.V x,
                          Some (ce, J.Loc (DebugAddr.of_addr pc))]])
            end
        | Set_field (x, n, y) ->
            let ((px, cx), expr_queue) = access_queue expr_queue x in
            let ((py, cy), expr_queue) = access_queue expr_queue y in
            flush_queue expr_queue mutator_p
              [J.Expression_statement
                  ((J.EBin (J.Eq, J.EAccess (cx, int (n + 1)), cy)), J.Loc (DebugAddr.of_addr pc))]
        | Offset_ref (x, 1) ->
          (* FIX: may overflow.. *)
            let ((px, cx), expr_queue) = access_queue expr_queue x in
            flush_queue expr_queue mutator_p
              [J.Expression_statement
                  ((J.EUn (J.IncrA, (J.EAccess (cx, J.ENum 1.)))),
                  J.Loc (DebugAddr.of_addr pc))]
        | Offset_ref (x, n) ->
          (* FIX: may overflow.. *)
            let ((px, cx), expr_queue) = access_queue expr_queue x in
            flush_queue expr_queue mutator_p
              [J.Expression_statement
                  ((J.EBin (J.PlusEq, (J.EAccess (cx, J.ENum 1.)), int n)),
                  J.Loc (DebugAddr.of_addr pc))]
        | Array_set (x, y, z) ->
            let ((px, cx), expr_queue) = access_queue expr_queue x in
            let ((py, cy), expr_queue) = access_queue expr_queue y in
            let ((pz, cz), expr_queue) = access_queue expr_queue z in
            flush_queue expr_queue mutator_p
              [J.Expression_statement
                  ((J.EBin (J.Eq, J.EAccess (cx, J.EBin(J.Plus, cy, one)),
                            cz)),
                   J.Loc (DebugAddr.of_addr pc))]
      in
      let (instrs, expr_queue) = translate_instr ctx expr_queue pc rem in
      (st @ instrs, expr_queue)

and compile_block st queue (pc : addr) frontier interm =
if queue <> [] && AddrSet.mem pc st.loops then
  flush_all queue (compile_block st [] pc frontier interm)
else begin
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
      match st.loop_stack with (_, (l, _)) :: _ -> J.Label.succ l | [] -> J.Label.zero in
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
  let block = AddrMap.find pc st.blocks in
  let (seq, queue) = translate_instr st.ctx queue pc block.body in
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
        let body =
          compile_branch st [] (pc1, args1)
            None AddrSet.empty inner_frontier interm
        in
        if debug () then Format.eprintf "} catch {@,";
        let handler = compile_block st [] pc2 inner_frontier interm in
        let handler =
          if st.ctx.Ctx.live.(Var.idx x) > 0 && Option.Optim.excwrap ()
          then J.Expression_statement (
            J.EBin(
              J.Eq,
              J.EVar (J.V x),
              J.ECall (Share.get_prim s_var "caml_wrap_exception" st.ctx.Ctx.share,
                       [J.EVar (J.V x)])),J.N)
            ::handler
          else handler in
        let x =
          let block2 = AddrMap.find pc2 st.blocks in
          let m = Subst.build_mapping args2 block2.params in
          try VarMap.find x m with Not_found -> x
        in
        if debug () then Format.eprintf "}@]@ ";
        if limit_body then decr_preds st pc3;
        flush_all queue
          (J.Try_statement (body,
                            Some (J.V x,
                                  handler),
                            None,
                            J.Loc (DebugAddr.of_addr pc)) ::
           if AddrSet.is_empty inner_frontier then [] else begin
             let pc = AddrSet.choose inner_frontier in
             if AddrSet.mem pc frontier then [] else
               compile_block st [] pc frontier interm
           end)
    | _ ->
        let (new_frontier, new_interm) =
          if AddrSet.cardinal new_frontier > 1 then begin
            let x = Code.Var.fresh () in
            let a = Array.of_list (AddrSet.elements new_frontier) in
            if debug () then Format.eprintf "@ var %a;" Code.Var.print x;
            let idx = st.interm_idx in
            st.interm_idx <- idx - 1;
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
        cond @
        if AddrSet.cardinal new_frontier = 0 then [] else begin
          let pc = AddrSet.choose new_frontier in
          if AddrSet.mem pc frontier then [] else
          compile_block st [] pc frontier interm
        end
  in
  if AddrSet.mem pc st.loops then begin
    let label =
      match st.loop_stack with
        (_, (l, used)) :: r -> st.loop_stack <- r; if !used then Some l else None
      | []                  -> assert false
    in
    let st =
      J.For_statement
        (J.Left None, None, None,
         J.Block(
           (if AddrSet.cardinal frontier > 0 then begin
              if debug () then
                Format.eprintf "@ break (%d); }@]"
                  (AddrSet.choose new_frontier);
              body @ [J.Break_statement (None,J.N)]
            end else begin
              if debug () then Format.eprintf "}@]";
              body
            end),J.N),
        J.Loc (DebugAddr.of_addr pc))
    in
    match label with
      | None -> [st]
      | Some label -> [J.Labelled_statement (label, st, J.N)]
  end else
    body
end

and compile_if st e ?pc cont1 cont2 handler backs frontier interm succs =
  let iftrue = compile_branch st [] cont1 handler backs frontier interm in
  let iffalse = compile_branch st [] cont2 handler backs frontier interm in
  Js_simpl.if_statement e ?pc
    (J.Block (iftrue,J.N)) (never_continue st cont1 frontier interm succs)
    (J.Block (iffalse,J.N)) (never_continue st cont2 frontier interm succs)

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
  let loc = J.Loc (DebugAddr.of_addr pc) in
  let res =
  match last with
    Return x ->
      let ((px, cx), queue) = access_queue queue x in
      flush_all queue [J.Return_statement (Some cx,loc)]
  | Raise x ->
      let ((px, cx), queue) = access_queue queue x in
      flush_all queue [J.Throw_statement (cx,loc)]
  | Stop ->
      flush_all queue [J.Return_statement (None,loc)]
  | Branch cont ->
      compile_branch st queue cont handler backs frontier interm
  | Cond (c, x, cont1, cont2) ->
      let ((px, cx), queue) = access_queue queue x in
      let e =
        match c with
          IsTrue         -> cx
        | CEq n          -> J.EBin (J.EqEqEq, int32 n, cx)
        | CLt n          -> J.EBin (J.Lt, int32 n, cx)
        | CUlt n         -> J.EBin (J.Or, J.EBin (J.Lt, cx, int 0),
                                          J.EBin (J.Lt, int32 n, cx))
        | CLe n          -> J.EBin (J.Le, int32 n, cx)
      in
      (* Some changes here may require corresponding changes
         in function [fold_children] above. *)
      flush_all queue
        (compile_if st e ~pc:loc cont1 cont2 handler backs frontier interm succs)

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
            J.Block
              (compile_branch st [] cont handler backs frontier interm,J.N)
        | [cont1, [(n, _)]; cont2, _] | [cont2, _; cont1, [(n, _)]] ->
          J.Block (compile_if st (J.EBin (J.EqEqEq, int n, e))
                     cont1 cont2 handler backs frontier interm succs, J.N)
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
                              (compile_branch
                                 st [] cont handler backs frontier interm @
                               if
                                 never_continue st cont frontier interm succs
                               then
                                 []
                               else
                                 [J.Break_statement (None, J.N)]))
                             ::
                             List.map
                             (fun (i, _) -> (J.ENum (float i), [])) r))
                   rem)
            in
            J.Switch_statement
              (e, l,
               Some (compile_branch
                          st [] cont handler backs frontier interm), loc)
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
                      (Share.get_string str_js "number" st.ctx.Ctx.share)))
              ~pc:loc
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
  else
    let block = AddrMap.find pc ctx.Ctx.blocks in
    parallel_renaming ctx block.params args continuation queue

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
              let (z, old) =
                match old with [] -> (None, []) | z :: old -> (Some z, old)
              in
              let x' =
                try Some (VarMap.find x m) with Not_found -> Some x in
              if Var.compare x x0 = 0 || x' = z then
                loop continuation old args params queue
              else begin
               let ((px, cx), queue) = access_queue queue x in
               let (st, queue) =
(*FIX: we should flush only the variables we need rather than doing this;
       do the same for closure free variables *)
                 match 2 (*ctx.Ctx.live.(Var.idx y)*) with
                   0 -> assert false
                 | 1 -> enqueue queue px y cx pc 1 []
                 | _ -> flush_queue queue px
                          [J.Variable_statement
                             [J.V y,
                              Some (cx, J.Loc (DebugAddr.of_addr pc))]]
               in
               st @ loop continuation old args params queue
              end
          | _ ->
              assert false
        in
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
            Some lab
          end
    in
    if debug () then begin
      if label = None then
        Format.eprintf "continue;@ "
      else
        Format.eprintf "continue (%d);@ " pc
    end;
    flush_all queue [J.Continue_statement (label,J.Loc (DebugAddr.of_addr pc))]
  end else if AddrSet.mem pc frontier || AddrMap.mem pc interm then begin
    if debug () then Format.eprintf "(br %d)@ " pc;
    flush_all queue (compile_branch_selection pc interm)
  end else
    compile_block st queue pc frontier interm))

and compile_branch_selection pc interm =
  try
    let (pc, (x, i)) = AddrMap.find pc interm in
    if debug () then Format.eprintf "@ %a=%d;" Code.Var.print x i;
    J.Variable_statement [J.V x, Some (int i, J.Loc (DebugAddr.of_addr pc))] ::
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
  List.map (fun st -> J.Statement st ) res


let generate_shared_value ctx =
  let strings =
    J.Statement (
      J.Variable_statement (
        List.map (fun (s,v) -> v, Some (str_js s,J.N)) (StringMap.bindings ctx.Ctx.share.Share.vars.Share.strings)
        @ List.map (fun (s,v) -> v, Some (s_var s,J.N)) (StringMap.bindings ctx.Ctx.share.Share.vars.Share.prims)))
  in

  if not (Option.Optim.inline_callgen ())
  then
    let applies = List.map (fun (n,v) ->
        match generate_apply_fun n with
        | J.EFun (_,param,body,nid) ->
          J.Function_declaration (v,param,body,nid)
        | _ -> assert false) (IntMap.bindings ctx.Ctx.share.Share.vars.Share.applies) in
    strings::applies
  else [strings]

let compile_program ctx pc =
  let res = compile_closure ctx (pc, []) in
  let res = generate_shared_value ctx @ res in
  if debug () then Format.eprintf "@.@.";
  res

let f ((pc, blocks, _) as p) live_vars =
  let mutated_vars = Freevars.f p in
  let t' = Util.Timer.make () in
  let share = Share.get p in
  let ctx = Ctx.initial blocks live_vars mutated_vars share in
  let p = compile_program ctx pc in
  if times () then Format.eprintf "  code gen.: %a@." Util.Timer.print t';
  p

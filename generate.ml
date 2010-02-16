(*XXXX
Patterns:
  => if e1 then {if e2 then P else Q} else {if e3 then P else Q}
  => if e then return e1; return e2
  => if e then var x = e1; else var x = e2;
  => while (true) {.... if (e) continue; break; }
  => should generate conditionnals when single switch inserted
     during compilation

- CLEAN UP!!!

- Inlining? (Especially of functions that are used only once!)

- Can we avoid spurious conversions from boolean to integers???
  ===> explicit conversion to boolean; specialized "if" that operates
       on booleans directly

- scalable generation of caml_apply functions
  ==> use curry functions as the ocaml compilers does
  ==> we could generate explit closures in the code

- Variables
  ==> use fresh variables at each start of a block
      + add mapping from old to new variables
  ==> should we use short variables for innermost functions?
*)

let compact = true

(****)

open Util
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

let ch = open_out "/tmp/graph.dot"
let _ = Printf.fprintf ch "digraph G {\n"
let i = ref 0

(****)

let (>>) x f = f x

let fold_children blocks pc f accu =
  let (_, _, last) = IntMap.find pc blocks in
  match last with
    Return _ | Raise _ | Stop | Poptrap _ ->
      accu
  | Branch (pc', _) ->
Printf.fprintf ch "%d -> %d\n" pc pc';
      f pc' accu
  | Cond (_, _, (pc1, _), (pc2, _)) ->
Printf.fprintf ch "%d -> %d\n" pc pc1;
Printf.fprintf ch "%d -> %d\n" pc pc2;
      accu >> f pc1 >> f pc2
  | Pushtrap ((pc1, _), pc2, cont) when Code.is_dummy_cont cont ->
Printf.fprintf ch "%d -> %d\n" pc pc1;
Printf.fprintf ch "%d -> %d\n" pc pc2;
      accu >> f pc1 >> f pc2
  | Pushtrap ((pc1, _), pc2, (pc3, _)) ->
Printf.fprintf ch "%d -> %d\n" pc pc1;
Printf.fprintf ch "%d -> %d\n" pc pc2;
Printf.fprintf ch "%d -> %d\n" pc pc3;
      accu >> f pc1 >> f pc2 >> f pc3
  | Switch (_, a1, a2) ->
      let normalize a =
        a >> Array.to_list
          >> List.sort compare
          >> list_group (fun x -> x)
          >> List.map fst
          >> Array.of_list
      in
Array.iter (fun (pc', _) -> Printf.fprintf ch "%d -> %d\n" pc pc') (normalize a1);
Array.iter (fun (pc', _) -> Printf.fprintf ch "%d -> %d\n" pc pc') (normalize a2);
      accu >> Array.fold_right (fun (pc, _) accu -> f pc accu) (normalize a1)
           >> Array.fold_right (fun (pc, _) accu -> f pc accu) (normalize a2)

(****)

let traverse blocks pc =
  let rec traverse_rec pc (last_visit, next, queue) =
    let last_visit' = IntMap.add pc next last_visit in
    if not (IntMap.mem pc last_visit) then begin
      let (last_visit, next', queue) =
        fold_children blocks pc traverse_rec (last_visit', next, queue) in
      (last_visit, next' + 1, (pc, next, next') :: queue)
    end else
      (last_visit', next, queue)
  in
  let (last_visit, next, queue) = traverse_rec pc (IntMap.empty, 0, []) in
  (queue, last_visit, next)

type tree = Node of (Code.addr * tree list)

let build blocks pc prev =
  let (queue, last_visit, max) = traverse blocks pc in
  let rec build_rec queue last_visit min max prev =
    match queue with
      (pc, start, num) :: queue
          when min <= start && IntMap.find pc last_visit < max ->
(*Format.eprintf "%d (%d/%d/%d/%d/%d) {@." pc min start num (IntMap.find pc last_visit) max;*)
incr i; let nm = Format.sprintf "cluster_%d" !i in
Printf.fprintf ch "subgraph %s {\n" nm;
Printf.fprintf ch "%d\n" pc;
        let (queue, child_info) =
          build_rec queue last_visit start (num + 1) [] in
Printf.fprintf ch "}\n";
        build_rec queue last_visit min max (Node (pc, child_info) :: prev)
    | _ ->
(*
begin match queue with
(pc, start, num) :: queue ->
Format.eprintf "[%d (%d/%d/%d/%d/%d)]@." pc min start num (IntMap.find pc last_visit) max
  | _ -> ()
end;
*)
        (queue, prev)
  in
  let (queue, accu) = build_rec queue last_visit 0 max prev in
  assert (queue = []);
  accu

let rec tree_to_map (Node (pc, ch)) map =
  map >> IntMap.add pc (List.map (fun (Node (pc, _)) -> pc) ch)
      >> forest_to_map ch

and forest_to_map f map = List.fold_right tree_to_map f map

(*
let build blocks pc enter_block leave_block prev =
  let (queue, last_visit, max) = traverse blocks pc in
  let rec build_rec queue last_visit min max prev =
    match queue with
      (pc, start, num) :: queue
          when min <= start && IntMap.find pc last_visit < max ->
        let (prev, child_prev) = enter_block pc prev in
incr i; let nm = Format.sprintf "cluster_%d" !i in
Printf.fprintf ch "subgraph %s {\n" nm;
Printf.fprintf ch "%d\n" pc;
        Format.eprintf "@[<2>%d (%d <= %d <= %d <= %d < %d) {@," pc min start num (IntMap.find pc last_visit) max;
        let (queue, child_info) =
          build_rec queue last_visit start num child_prev in
        Format.eprintf "}@]";
Printf.fprintf ch "}\n";
        let prev = leave_block prev child_info in
        build_rec queue last_visit min max prev
    | _ ->
        (queue, prev)
  in
  Format.eprintf "@[";
  let (queue, accu) = build_rec queue last_visit 0 max prev in
  assert (queue = []);
  Format.eprintf "@]@.";
  accu
*)

let fold_children blocks pc f accu =
  let (_, _, last) = IntMap.find pc blocks in
  match last with
    Return _ | Raise _ | Stop | Poptrap _ ->
      accu
  | Branch (pc', _) ->
      f pc' accu
  | Cond (_, _, (pc1, _), (pc2, _)) ->
      accu >> f pc1 >> f pc2
  | Pushtrap ((pc1, _), pc2, cont) when Code.is_dummy_cont cont ->
      accu >> f pc1 >> f pc2
  | Pushtrap ((pc1, _), pc2, (pc3, _)) ->
      accu >> f pc1 >> f pc2 >> f pc3
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

let count_preds blocks pc =
  let rec count_rec pc count =
    if not (IntMap.mem pc count) then begin
      let count = IntMap.add pc 1 count in
      fold_children blocks pc count_rec count
    end else
      IntMap.add pc (IntMap.find pc count + 1) count
  in
  count_rec pc IntMap.empty

(****)

module Ctx = struct
  type t =
    { var_stream : Var.stream;
      mutable blocks : Code.block Util.IntMap.t;
      live : int array }

  let fresh_var ctx =
    let (x, stream) = Var.next ctx.var_stream in
    (x, {ctx with var_stream = stream})

  let initial b l = { var_stream = Var.make_stream (); blocks = b; live = l }

  let used_once ctx x = ctx.live.(Var.idx x) <= 1
end

let add_names = Hashtbl.create 101

let var x = J.EVar (Var.to_string x)
let int n = J.ENum (float n)
let one = int 1
let zero = int 0
let addr pc =
  if not compact then
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

(****)

let same_custom x y =
  Obj.field x 0 = Obj.field (Obj.repr y) 0

let rec constant x =
  if Obj.is_block x then begin
    let tag = Obj.tag x in
    if tag = Obj.string_tag then
      J.ENew (J.EVar ("MlString"), Some [J.EStr (Obj.magic x : string)])
    else if tag = Obj.double_tag then
      J.ENum (Obj.magic x : float)
    else if tag = Obj.double_array_tag then begin
      let a = (Obj.magic x : float array) in
      J.EArr (Some (int Obj.double_array_tag) ::
              Array.to_list (Array.map (fun f -> Some (J.ENum f)) a))
    end else if tag = Obj.custom_tag && same_custom x 0l then
      J.ENum (Int32.to_float (Obj.magic x : int32))
    else if tag = Obj.custom_tag && same_custom x 0n then
      J.ENum (Nativeint.to_float (Obj.magic x : nativeint))
    else if tag = Obj.custom_tag && same_custom x 0L then
      J.ENum (Int64.to_float (Obj.magic x : int64))
    else if tag < Obj.no_scan_tag then begin
      let a = Array.init (Obj.size x) (fun i -> Obj.field x i) in
      J.EArr (Some (int tag) ::
              Array.to_list (Array.map (fun x -> Some (constant x)) a))
    end else
      assert false
  end else
    int (Obj.magic x : int)

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

let access_queue queue x =
  try
    let res = List.assoc x queue in
    (res, List.remove_assoc x queue)
  with Not_found ->
    ((const_p, var x), queue)

let flush_queue expr_queue all l =
  let (instrs, expr_queue) =
    if all then (expr_queue, []) else
    List.partition (fun (_, (p, _)) -> is_mutable p) expr_queue
  in
  let instrs =
    List.map (fun (x, (_, ce)) ->
                J.Variable_statement
                  [Var.to_string x, Some ce]) instrs
  in
  (List.rev_append instrs l, expr_queue)

let flush_all expr_queue l = fst (flush_queue expr_queue true l)

let enqueue expr_queue prop x ce =
  let (instrs, expr_queue) =
    if is_mutator prop then begin
      flush_queue expr_queue (prop >= flush_p) []
    end else
      [], expr_queue
  in
  (instrs, (x, (prop, ce)) :: expr_queue)

(****)

type state =
  { all_succs : (int, IntSet.t) Hashtbl.t;
    succs : (int, int list) Hashtbl.t;
    backs : (int, IntSet.t) Hashtbl.t;
    preds : (int, int) Hashtbl.t;
    mutable loops : IntSet.t;
    mutable visited_blocks : IntSet.t;
    mutable interm_idx : int;
    ctx : Ctx.t; mutable blocks : Code.block Util.IntMap.t }

let get_preds st pc = try Hashtbl.find st.preds pc with Not_found -> 0
let incr_preds st pc = Hashtbl.replace st.preds pc (get_preds st pc + 1)
let decr_preds st pc = Hashtbl.replace st.preds pc (get_preds st pc - 1)

(* This as to be kept in sync with the way we build conditionals
   and switches! *)
let fold_children blocks pc f accu =
  let (_, _, last) = IntMap.find pc blocks in
  match last with
    Return _ | Raise _ | Stop ->
      accu
  | Branch (pc', _) | Poptrap (pc', _) ->
      f pc' accu
  | Cond (_, _, (pc1, _), (pc2, _)) | Pushtrap ((pc1, _), pc2, _) ->
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
  if not (IntSet.mem pc st.visited_blocks) then begin
    st.visited_blocks <- IntSet.add pc st.visited_blocks;
    let anc = IntSet.add pc anc in
    let s = Code.fold_children st.blocks pc IntSet.add IntSet.empty in
    Hashtbl.add st.all_succs pc s;
    let backs = IntSet.inter s anc in
    Hashtbl.add st.backs pc backs;

    let s = fold_children st.blocks pc (fun x l -> x :: l) [] in
    let succs = List.filter (fun pc -> not (IntSet.mem pc anc)) s in
    Hashtbl.add st.succs pc succs;
    IntSet.iter (fun pc' -> st.loops <- IntSet.add pc' st.loops) backs;
    List.iter (fun pc' -> build_graph st pc' anc) succs;
    List.iter (fun pc' -> incr_preds st pc') succs
  end

let rec dominance_frontier_rec st pc visited grey =
  let n = get_preds st pc in
  let v = try IntMap.find pc visited with Not_found -> 0 in
  if v < n then begin
    let v = v + 1 in
    let visited = IntMap.add pc v visited in
    if v = n then begin
      let grey = IntSet.remove pc grey in
      let s = Hashtbl.find st.succs pc in
      List.fold_right
        (fun pc' (visited, grey) ->
           dominance_frontier_rec st pc' visited grey)
        s (visited, grey)
    end else begin
      (visited, if v = 1 then IntSet.add pc grey else grey)
    end
  end else
    (visited, grey)

let dominance_frontier st pc =
  snd (dominance_frontier_rec st pc IntMap.empty IntSet.empty)

(* Block of code that never continues (either returns, throws an exception
   or loops back) *)
let never_continue st (pc, _) frontier interm =
  not (IntSet.mem pc frontier || IntMap.mem pc interm)
    &&
  IntSet.is_empty (dominance_frontier st pc)

let rec resolve_node interm pc =
  try
    resolve_node interm (fst (IntMap.find pc interm))
  with Not_found ->
    pc

let resolve_nodes interm s =
  IntSet.fold (fun pc s' -> IntSet.add (resolve_node interm pc) s')
    s IntSet.empty

(****)

let prim_kinds = ["caml_int64_float_of_bits", const_p]

let rec translate_expr ctx queue e =
  match e with
    Const i ->
      (int i, const_p, queue)
  | Apply (x, l) ->
(*XXX Continuation...*)
      let (args, prop, queue) =
        List.fold_right
          (fun x (args, prop, queue) ->
             let ((prop', cx), queue) =
               access_queue queue x in (cx :: args, or_p prop prop', queue))
          (x :: l) ([], mutator_p, queue)
      in
      (J.ECall (J.EVar (Format.sprintf "caml_call_%d" (List.length l)), args),
       prop, queue)
  | Direct_apply (x, l) ->
      let ((px, cx), queue) = access_queue queue x in
      let (args, prop, queue) =
        List.fold_right
          (fun x (args, prop, queue) ->
             let ((prop', cx), queue) =
               access_queue queue x in (cx :: args, or_p prop prop', queue))
          l ([], or_p px mutator_p, queue)
      in
      (J.ECall (cx, args), prop, queue)
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
  | Closure (args, pc) ->
      (J.EFun (None, List.map Var.to_string args,
               compile_closure ctx pc),
       flush_p, queue)
  | Constant c ->
      (constant c, const_p, queue)
  | Prim (p, l) ->
      begin match p, l with
        Vectlength, [x] ->
          let ((px, cx), queue) = access_queue queue x in
          (J.EBin (J.Minus, J.EDot (cx, "length"), one), px, queue)
      | Array_get, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EAccess (cx, J.EBin (J.Plus, cy, one)),
           or_p mutable_p (or_p px py), queue)
      | C_call
            ("caml_array_get_addr"|"caml_array_get"|"caml_array_unsafe_get"),
            [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EAccess (cx, J.EBin (J.Plus, cy, one)),
                      or_p (or_p px py) mutable_p, queue)
      | C_call "caml_string_get", [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.ECall (J.EDot (cx, "charAt"), [cy]),
           or_p (or_p px py) mutable_p, queue)
      | C_call "caml_ml_string_length", [x] ->
          let ((px, cx), queue) = access_queue queue x in
          (J.EDot (cx, "length"), px, queue)
      | C_call name, l ->
Code.add_reserved_name name;  (*XXX HACK *)
(*XXX Tail call *)
          let prim_kind =
            try List.assoc name prim_kinds with Not_found -> mutator_p in
          let (args, prop, queue) =
            List.fold_right
              (fun x (args, prop, queue) ->
                 let ((prop', cx), queue) = access_queue queue x in
                 (cx :: args, or_p prop prop', queue))
              l ([], prim_kind, queue)
          in
          (J.ECall (J.EVar name, args), prop, queue)
      | Not, [x] ->
          let ((px, cx), queue) = access_queue queue x in
          (J.EBin (J.Minus, one, cx), px, queue)
      | Neg, [x] ->
          let ((px, cx), queue) = access_queue queue x in
          (J.EUn (J.Neg, cx), px, queue)
      | Add, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EBin (J.Plus, cx, cy), or_p px py, queue)
      | Sub, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EBin (J.Minus, cx, cy), or_p px py, queue)
      | Mul, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EBin (J.Mul, cx, cy), or_p px py, queue)
      | Div, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EBin (J.Div, cx, cy), or_p px py, queue)
      | Mod, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EBin (J.Mod, cx, cy), or_p px py, queue)
      | Offset n, [x] ->
          let ((px, cx), queue) = access_queue queue x in
          if n > 0 then
            (J.EBin (J.Plus, cx, int n), px, queue)
          else
            (J.EBin (J.Minus, cx, int (-n)), px, queue)
      | Lsl, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EBin (J.Lsl, cx, cy), or_p px py, queue)
      | Lsr, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EBin (J.Lsr, cx, cy), or_p px py, queue)
      | Asr, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EBin (J.Asr, cx, cy), or_p px py, queue)
      | Lt, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (bool (J.EBin (J.Lt, cx, cy)), or_p px py, queue)
      | Le, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (bool (J.EBin (J.Le, cx, cy)), or_p px py, queue)
      | Eq, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (bool (J.EBin (J.EqEqEq, cx, cy)), or_p px py, queue)
      | Neq, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (bool (J.EBin (J.NotEqEq, cx, cy)), or_p px py, queue)
      | IsInt, [x] ->
          let ((px, cx), queue) = access_queue queue x in
          (boolnot (J.EBin(J.InstanceOf, var x, J.EVar ("Array"))), px, queue)
      | And, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EBin (J.Band, cx, cy), or_p px py, queue)
      | Or, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EBin (J.Bor, cx, cy), or_p px py, queue)
      | Xor, [x; y] ->
          let ((px, cx), queue) = access_queue queue x in
          let ((py, cy), queue) = access_queue queue y in
          (J.EBin (J.Bxor, cx, cy), or_p px py, queue)
      | Ult, [x; y] ->
(*XXX*)
Format.eprintf "Primitive [ULT] not implemented!!!@.";
         (J.EQuote "ult", const_p, queue)
      | (Vectlength | Array_get | Not | Neg | IsInt | Add | Sub |
         Mul | Div | Mod | And | Or | Xor | Lsl | Lsr | Asr | Eq |
         Neq | Lt | Le | Ult | Offset _), _ ->
          assert false
      end
  | Variable x ->
      let ((px, cx), queue) = access_queue queue x in
(*XXXX??? mutable? *)
      (cx, or_p mutator_p px, queue)

and translate_instr ctx expr_queue instr =
  match instr with
    [] ->
      ([], expr_queue)
  | i :: rem ->
      let (st, expr_queue) =
        match i with
          Let (x, e) ->
            let (ce, prop, expr_queue) = translate_expr ctx expr_queue e in
            begin match ctx.Ctx.live.(Var.idx x) with
              0 -> flush_queue expr_queue (prop >= flush_p)
                     [J.Expression_statement ce]
            | 1 -> enqueue expr_queue prop x ce
            | _ -> flush_queue expr_queue (prop >= flush_p)
                     [J.Variable_statement [Var.to_string x, Some ce]]
            end
        | Assign (x, y) ->
            let ((px, cx), expr_queue) = access_queue expr_queue x in
            let ((py, cy), expr_queue) = access_queue expr_queue y in
            flush_queue expr_queue false
              [J.Expression_statement (J.EBin (J.Eq, cx, cy))]
        | Set_field (x, n, y) ->
            let ((px, cx), expr_queue) = access_queue expr_queue x in
            let ((py, cy), expr_queue) = access_queue expr_queue y in
            flush_queue expr_queue false
              [J.Expression_statement
                 (J.EBin (J.Eq, J.EAccess (cx, int (n + 1)), cy))]
        | Offset_ref (x, n) ->
            let ((px, cx), expr_queue) = access_queue expr_queue x in
            flush_queue expr_queue false
              [J.Expression_statement
                 (J.EBin (J.PlusEq, (J.EAccess (cx, J.ENum 1.)), int n))]
        | Array_set (x, y, z) ->
            let ((px, cx), expr_queue) = access_queue expr_queue x in
            let ((py, cy), expr_queue) = access_queue expr_queue y in
            let ((pz, cz), expr_queue) = access_queue expr_queue z in
            flush_queue expr_queue false
              [J.Expression_statement
                 (J.EBin (J.Eq, J.EAccess (cx, J.EBin(J.Plus, cy, one)),
                          cz))]
      in
      let (instrs, expr_queue) = translate_instr ctx expr_queue rem in
      (st @ instrs, expr_queue)

and translate_last ctx tree count doReturn queue last =
  match last with
    Return x ->
      assert doReturn;
      let ((px, cx), queue) = access_queue queue x in
      flush_all queue [J.Return_statement (Some cx)]
  | Raise x ->
      let ((px, cx), queue) = access_queue queue x in
      flush_all queue [J.Throw_statement cx]
  | Stop ->
      flush_all queue []
  | Branch cont ->
      branch ctx tree count doReturn queue cont
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
      flush_all queue
      [Js_simpl.if_statement
         e
         (Js_simpl.block (branch ctx tree count doReturn [] cont1))
         (Some (Js_simpl.block
                  (branch ctx tree count doReturn [] cont2)))]
  | Switch (x, a1, a2) ->
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
            Js_simpl.block (branch ctx tree count doReturn [] cont)
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
                                (branch ctx tree count doReturn [] cont))
                               ::
                             List.map
                             (fun (i, _) -> (J.ENum (float i), [])) r))
                   rem)
            in
            J.Switch_statement
              (e, l, Some (Js_simpl.statement_list
                             (branch ctx tree count doReturn [] cont)))
(*
        let l =
              Array.to_list
                (Array.mapi
                   (fun i cont ->
                      (J.ENum (float i), branch ctx tree count [] cont))
                   a)
            in
            J.Switch_statement (e, l, None)
          *)
      in
      let (st, queue) =
        if Array.length a1 = 0 then
          let ((px, cx), queue) = access_queue queue x in
          ([build_switch (J.EAccess(cx, J.ENum 0.)) a2], queue)
        else if Array.length a2 = 0 then
          let ((px, cx), queue) = access_queue queue x in
          ([build_switch cx a1], queue)
        else
          ([Js_simpl.if_statement
              (J.EBin (J.InstanceOf, var x, J.EVar ("Array")))
              (build_switch (J.EAccess(var x, J.ENum 0.)) a2)
              (Some (build_switch (var x) a1))],
           queue)
      in
      flush_all queue st
  | Pushtrap (cont1, pc, cont2) ->
      let var =
        match IntMap.find pc ctx.Ctx.blocks with
          (Some y, _, _) -> Var.to_string y
              (* FIX: make sure this is *always* a fresh variable *)
        | _              -> "_"
      in
      let handler_body =
        Js_simpl.statement_list
          (translate_block_contents ctx tree count doReturn pc []) in
      flush_all queue
        (J.Try_statement (Js_simpl.statement_list
                            (branch ctx tree count false [] cont1),
                          Some (var, handler_body), None) ::
         if Code.is_dummy_cont cont2 then [] else branch ctx tree count doReturn [] cont2)
(*
      let invoke_handler =
        [J.Statement (J.Return_statement
                        (Some (J.ECall (J.EVar (addr pc), []))))]
      in

        match param with
          Some y -> 
            J.Statement (J.Expression_statement
                           (J.EBin (J.Eq, var y, J.EVar "x"))) ::
            invoke_handler
        | _ ->
            invoke_handler
      in
      flush_all queue
        (J.Expression_statement
           (J.ECall (J.EVar "caml_push_trap",
                     [J.EFun (None, ["x"], body)])) ::
           branch ctx tree count [] cont1)
*)
  | Poptrap _ ->
      flush_all queue [(*J.Expression_statement (J.EVar "poptrap")*)]

and translate_block_contents ctx tree count doReturn pc expr_queue =
  let ch = IntMap.find pc tree in
  let ch =
    List.fold_right
      (fun pc prev ->
         if IntMap.find pc count > 1 then
           translate_block ctx tree count prev pc
         else
           prev)
      ch []
  in
  let (param, instr, last) = IntMap.find pc ctx.Ctx.blocks in
  let (seq, expr_queue) = translate_instr ctx expr_queue instr in
  let seq' = translate_last ctx tree count doReturn expr_queue last in
  seq  @ ch @ seq'

and translate_block ctx tree count prev pc =
  let (param, instr, last) = IntMap.find pc ctx.Ctx.blocks in
  let body =
    Js_simpl.source_elements
      (translate_block_contents ctx tree count true pc []) in
  let prev =
    J.Variable_statement [addr pc, Some (J.EFun (None, [], body))] :: prev in
  match param with
    None ->
      prev
  | Some x ->
      J.Variable_statement [Var.to_string x, None] :: prev

and branch ctx tree count doReturn queue (pc, arg) =
  if IntMap.find pc count > 1 then begin
    let br =
      if doReturn then
        [J.Return_statement (Some (J.ECall (J.EVar (addr pc), [])))]
      else
        [J.Expression_statement (J.ECall (J.EVar (addr pc), []))]
    in
    match arg with
      None ->
        flush_all queue br
    | Some x ->
        match IntMap.find pc ctx.Ctx.blocks with
          (Some y, _, _) ->
            let ((px, cx), queue) = access_queue queue x in
            flush_all queue
              (J.Expression_statement (J.EBin (J.Eq, var y, cx)) :: br)
        | _ ->
            assert false
  end else
    match arg with
      None ->
        let cont = translate_block_contents ctx tree count doReturn pc [] in
        flush_all queue cont
    | Some x ->
        match IntMap.find pc ctx.Ctx.blocks with
          (Some y, _, _) ->
            let ((px, cx), queue) = access_queue queue x in
            let (st, queue) =
              match ctx.Ctx.live.(Var.idx y) with
                0 -> assert false
(*
                  flush_queue expr_queue (px >= flush_p)
                        [J.Expression_statement cx]
*)
              | 1 -> enqueue queue px y cx
              | _ -> flush_queue queue (px >= flush_p)
                       [J.Variable_statement [Var.to_string y, Some cx]]
            in
            st @ translate_block_contents ctx tree count doReturn pc queue
        | _ ->
            assert false

and translate_body ctx pc toplevel =
  let tree = build ctx.Ctx.blocks pc [] in
  let count = count_preds ctx.Ctx.blocks pc in
  let tree = forest_to_map tree IntMap.empty in
  (*build ctx.Ctx.blocks pc enter_block leave_block [] @*)
(*
  if toplevel then
    [J.Statement (J.Expression_statement (J.ECall (J.EVar (addr pc), [])))]
  else
*)
    Js_simpl.source_elements (branch ctx tree count true [] (pc, None))

(**********************)

and compile_block st queue pc frontier interm =
  if pc >= 0 then begin
    if IntSet.mem pc st.visited_blocks then begin
      Format.eprintf "!!!! %d@." pc; assert false
    end;
    st.visited_blocks <- IntSet.add pc st.visited_blocks
  end;
  if IntSet.mem pc st.loops then Format.eprintf "@[<2>while (1) {@,";
  Format.eprintf "block %d;" pc;
  let succs = Hashtbl.find st.succs pc in
  let backs = Hashtbl.find st.backs pc in
  let grey =
    List.fold_right
      (fun pc grey -> IntSet.union (dominance_frontier st pc) grey)
      succs IntSet.empty
  in
  let new_frontier = resolve_nodes interm grey in
  let (_, instr, last) = IntMap.find pc st.blocks in
  let (seq, queue) = translate_instr st.ctx queue instr in
  let body =
    seq @
    match last with
      Code.Pushtrap ((pc1, _), pc2, ((pc3, _) as cont)) ->
  (* FIX: document this *)
        let grey =  dominance_frontier st pc2 in
        let grey' = resolve_nodes interm grey in
        let limit_body =
          IntSet.is_empty grey'&& not (Code.is_dummy_cont cont) in
        let inner_frontier =
          if limit_body then IntSet.add pc3 grey' else grey'
        in
        if limit_body then incr_preds st pc3;
        assert (IntSet.cardinal inner_frontier <= 1);
        let var =
          match IntMap.find pc2 st.ctx.Ctx.blocks with
            (Some y, _, _) -> Var.to_string y
                (* FIX: make sure this is *always* a fresh variable *)
          | _              -> "_"
        in
        Format.eprintf "@[<2>try {@,";
        let body = compile_block st [] pc1 inner_frontier interm in
        Format.eprintf "} catch {@,";
        let handler = compile_block st [] pc2 inner_frontier interm in
        Format.eprintf "}@]";
        if limit_body then decr_preds st pc3;
        flush_all queue
          (J.Try_statement (Js_simpl.statement_list body,
                            Some (var, Js_simpl.statement_list handler),
                            None) ::
           if IntSet.is_empty inner_frontier then [] else begin
             let pc = IntSet.choose inner_frontier in
             if IntSet.mem pc frontier then [] else
               compile_block st [] pc frontier interm
           end)
    | _ ->
        let (new_frontier, new_interm) =
          if IntSet.cardinal new_frontier > 1 then begin
            let x = Code.Var.fresh () in
            let a = Array.of_list (IntSet.elements new_frontier) in
            Format.eprintf "@ var %a;" Code.Var.print x;
            let idx = st.interm_idx in
            st.interm_idx <- idx - 1;
            let cases = Array.map (fun pc -> (pc, None)) a in
            let switch =
              if Array.length cases > 2 then
                Code.Switch (x, cases, [||])
              else
                Code.Cond (IsTrue, x, cases.(1), cases.(0))
            in
            st.blocks <- IntMap.add idx (None, [], switch) st.blocks;
            IntSet.iter (fun pc -> incr_preds st pc) new_frontier;
            Hashtbl.add st.succs idx (IntSet.elements new_frontier);
            Hashtbl.add st.all_succs idx new_frontier;
            Hashtbl.add st.backs idx IntSet.empty;
            (IntSet.singleton idx,
             Array.fold_right
               (fun (pc, i) interm -> (IntMap.add pc (idx, (x, i)) interm))
               (Array.mapi (fun i pc -> (pc, i)) a) interm)
          end else
            (new_frontier, interm)
        in
        assert (IntSet.cardinal new_frontier <= 1);
        (* Beware evaluation order! *)
        let cond =
          compile_conditional st queue pc last backs new_frontier new_interm in
        cond @
        if IntSet.cardinal new_frontier = 0 then [] else begin
          let pc = IntSet.choose new_frontier in
          if IntSet.mem pc frontier then [] else
          compile_block st [] pc frontier interm
        end
  in
(*XXXXX *)
  if IntSet.mem pc st.loops then begin
    [J.While_statement
       (J.EBool true,
        Js_simpl.block
          (if IntSet.cardinal new_frontier > 0 then begin
             Format.eprintf "@ break; }@]";
             body @ [J.Break_statement None]
           end else begin
             Format.eprintf "}@]";
             body
           end))]
  end else
    body

and compile_conditional st queue pc last backs frontier interm =
  let succs = Hashtbl.find st.succs pc in
  List.iter (fun pc -> if IntMap.mem pc interm then decr_preds st pc) succs;
  Format.eprintf "@[<2>switch{";
  let res =
  match last with
    Return x ->
      let ((px, cx), queue) = access_queue queue x in
      flush_all queue [J.Return_statement (Some cx)]
  | Raise x ->
      let ((px, cx), queue) = access_queue queue x in
      flush_all queue [J.Throw_statement cx]
  | Stop ->
      flush_all queue []
  | Branch cont ->
      compile_branch st queue cont backs frontier interm
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
      let iftrue = compile_branch st [] cont1 backs frontier interm in
      let iffalse = compile_branch st [] cont2 backs frontier interm in
      flush_all queue
        (if never_continue st cont1 frontier interm then
           Js_simpl.if_statement e (Js_simpl.block iftrue) None ::
           iffalse
         else if never_continue st cont2 frontier interm then
           Js_simpl.if_statement
             (Js_simpl.enot e) (Js_simpl.block iffalse) None ::
           iftrue
         else
           [Js_simpl.if_statement e (Js_simpl.block iftrue)
              (Some (Js_simpl.block iffalse))])
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
            Js_simpl.block (compile_branch st [] cont backs frontier interm)
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
                                   st [] cont backs frontier interm @
                                 if never_continue st cont frontier interm then
                                   []
                                 else
                                   [J.Break_statement None]))
                               ::
                             List.map
                             (fun (i, _) -> (J.ENum (float i), [])) r))
                   rem)
            in
            J.Switch_statement
              (e, l, Some (Js_simpl.statement_list
                             (compile_branch st [] cont backs frontier interm)))
      in
      let (st, queue) =
        if Array.length a1 = 0 then
          let ((px, cx), queue) = access_queue queue x in
          ([build_switch (J.EAccess(cx, J.ENum 0.)) a2], queue)
        else if Array.length a2 = 0 then
          let ((px, cx), queue) = access_queue queue x in
          ([build_switch cx a1], queue)
        else
          ([Js_simpl.if_statement
              (J.EBin(J.InstanceOf, var x, J.EVar ("Array")))
              (build_switch (J.EAccess(var x, J.ENum 0.)) a2)
              (Some (build_switch (var x) a1))],
           queue)
      in
      flush_all queue st
  | Pushtrap _ ->
      assert false
  | Poptrap cont ->
      flush_all queue (compile_branch st [] cont backs frontier interm)
  in
  Format.eprintf "}@.";
  res

and compile_argument_passing ctx queue (pc, arg) continuation =
  match arg with
    None ->
      continuation queue
  | Some x ->
      match IntMap.find pc ctx.Ctx.blocks with
        (Some y, _, _) ->
          let ((px, cx), queue) = access_queue queue x in
          let (st, queue) =
            match ctx.Ctx.live.(Var.idx y) with
              0 -> assert false
            | 1 -> enqueue queue px y cx
            | _ -> flush_queue queue (px >= flush_p)
                     [J.Variable_statement [Var.to_string y, Some cx]]
          in
          st @ continuation queue
      | _ ->
          assert false

and compile_branch st queue ((pc, arg) as cont) backs frontier interm =
  compile_argument_passing st.ctx queue cont
    (fun queue ->
       if IntSet.mem pc backs then begin
         Format.eprintf "@ continue;";
         flush_all queue [J.Continue_statement None]
       end else if IntSet.mem pc frontier || IntMap.mem pc interm then begin
         Format.eprintf "@ (br %d)" pc;
         flush_all queue (compile_branch_selection pc interm)
       end else
         compile_block st queue pc frontier interm)

and compile_branch_selection pc interm =
  try
    let (pc, (x, i)) = IntMap.find pc interm in
    Format.eprintf "@ %a=%d;" Code.Var.print x i;
    J.Variable_statement [Var.to_string x, Some (int i)] ::
    compile_branch_selection pc interm
  with Not_found ->
    []

and compile_closure ctx pc =
  let st =
    { visited_blocks = IntSet.empty; loops = IntSet.empty;
      all_succs = Hashtbl.create 17; succs = Hashtbl.create 17;
      backs = Hashtbl.create 17; preds = Hashtbl.create 17;
      interm_idx = -1; ctx = ctx; blocks = ctx.Ctx.blocks }
  in
  build_graph st pc IntSet.empty;
  let current_blocks = st.visited_blocks in
  st.visited_blocks <- IntSet.empty;
  Format.eprintf "@[<2>closure{";
  let res = compile_block st [] pc IntSet.empty IntMap.empty in
  if
    IntSet.cardinal st.visited_blocks <> IntSet.cardinal current_blocks
  then begin
    Format.eprintf "Some blocks not compiled!@."; assert false
  end;
  Format.eprintf "}@]";
  Js_simpl.source_elements res

let compile_program ctx pc =
  let res = compile_closure ctx pc in Format.eprintf "@.@."; res

(**********************)

let f (pc, blocks, _) live_vars =
  let ctx = Ctx.initial blocks live_vars in
(*
  let p = translate_body ctx pc true in
  let p = [J.Function_declaration ("start", [], p);
           J.Statement (J.Expression_statement
                          (J.ECall (J.EVar "start", [])))] in
*)
  let p = compile_program ctx pc in
if compact then Format.set_margin 999999998;
Format.printf "%a" Js_output.program p
;Printf.fprintf ch "}\n"
; close_out ch

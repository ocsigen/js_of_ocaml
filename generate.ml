(*XXXX
- CLEAN UP!!!

- Throw ???
  need to pass an argument!

- Inline internal functions
  ===> move branches upwards in conditional
               if (e) { i1; return f() } else { i2; return f() }
                              ===>
               if (e) { i1 } else { i2 }; return f()

- CPS style

- Inlining? (Especially of functions that are used only once!)

- Can we avoid spurious conversions from boolean to integers???
  ===> explicit conversion to boolean; specialized "if" that operates
       on booleans directly

- compact identifier names

- scalable generation of caml_apply functions
  ==> use curry functions as the ocaml compilers does
  ==> we could generate explit closures in the code

- Variables
  ==> use fresh variables at each start of a block
      + add mapping from old to new variables
  ==> should we use short variables for innermost functions?
*)

let single_line = true

(****)

open Util
open Code
module J = Javascript

(****)

let ch = open_out "/tmp/graph.dot"
let _ = Printf.fprintf ch "digraph G {\n"
let i = ref 0

(****)

let (>>) x f = f x

let fold_children blocks pc f accu =
  let (_, _, last) = IntMap.find pc blocks in
  match last with
    Return _ | Raise _ | Stop ->
      accu
  | Branch (pc', _) | Poptrap (pc', _) ->
Printf.fprintf ch "%d -> %d\n" pc pc';
      f pc' accu
  | Cond (_, _, (pc1, _), (pc2, _)) | Pushtrap ((pc1, _), pc2, _) ->
Printf.fprintf ch "%d -> %d\n" pc pc1;
Printf.fprintf ch "%d -> %d\n" pc pc2;
      accu >> f pc1 >> f pc2
  | Switch (_, a1, a2) ->
Array.iter (fun (pc', _) -> Printf.fprintf ch "%d -> %d\n" pc pc') a1;
Array.iter (fun (pc', _) -> Printf.fprintf ch "%d -> %d\n" pc pc') a2;
      accu >> Array.fold_right (fun (pc, _) -> f pc) a1
           >> Array.fold_right (fun (pc, _) -> f pc) a2

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
incr i; let nm = Format.sprintf "cluster_%d" !i in
Printf.fprintf ch "subgraph %s {\n" nm;
Printf.fprintf ch "%d\n" pc;
        let (queue, child_info) =
          build_rec queue last_visit start (num + 1) [] in
Printf.fprintf ch "}\n";
        build_rec queue last_visit min max (Node (pc, child_info) :: prev)
    | _ ->
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
      blocks : Code.block Util.IntMap.t;
      live : int array }

  let fresh_var ctx =
    let (x, stream) = Var.next ctx.var_stream in
    (x, {ctx with var_stream = stream})

  let initial b l = { var_stream = Var.make_stream (); blocks = b; live = l }

  let used_once ctx x = ctx.live.(Var.idx x) <= 1
end

let var x = J.EVar (Var.to_string x)
let int n = J.ENum (float n)
let one = int 1
let zero = int 0
let addr pc = Format.sprintf "f%d" pc
let bool e = J.ECond (e, one, zero)

(****)

let same_custom x y =
  Obj.field x 0 = Obj.field (Obj.repr y) 0

let rec constant x =
  if Obj.is_block x then begin
    let tag = Obj.tag x in
    if tag = Obj.string_tag then
      J.EStr (Obj.magic x : string)
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

let block l = match l with [s] -> s | _ -> J.Block l

let prim_kinds = ["caml_int64_float_of_bits", const_p]

let source_elements l =
  List.fold_right
    (fun st rem ->
       match st, rem with
         J.Variable_statement [addr, Some (J.EFun (None, params, body))], _ ->
           J.Function_declaration (addr, params, body) :: rem
       | J.Variable_statement l1,
         J.Statement (J.Variable_statement l2) :: rem' ->
           J.Statement (J.Variable_statement (l1 @ l2)) :: rem'
       | _ ->
           J.Statement st :: rem)
    l []

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
      (J.EFun (None, List.map Var.to_string args, translate_body ctx pc false),
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
          (J.EAccess (cx, cy), or_p (or_p px py) mutable_p, queue)
      | C_call "caml_ml_string_length", [x] ->
          let ((px, cx), queue) = access_queue queue x in
          (J.EDot (cx, "length"), px, queue)
      | C_call name, l ->
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
      |_ ->
         (J.EQuote "prim", const_p, queue)
(*
 | IsInt
  |  | Mul | Div | Mod | And | Or | Xor
 | Ult
*)
      end
  | Variable x ->
      let ((px, cx), queue) = access_queue queue x in
      (cx, px, queue)

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
            flush_queue expr_queue false
              [J.Expression_statement (J.EBin (J.Eq, var x, var y))]
        | Set_field (x, n, y) ->
            flush_queue expr_queue false
              [J.Expression_statement
                 (J.EBin (J.Eq, J.EAccess (var x, int (n + 1)), var y))]
        | Offset_ref (x, n) ->
            flush_queue expr_queue false
            [J.Expression_statement (J.EBin (J.PlusEq, var x, int n))]
        | Array_set (x, y, z) ->
            flush_queue expr_queue false
              [J.Expression_statement
                 (J.EBin (J.Eq, J.EAccess (var x, J.EBin(J.Plus, var y, one)),
                          var z))]
      in
      let (instrs, expr_queue) = translate_instr ctx expr_queue rem in
      (st @ instrs, expr_queue)

and translate_last ctx tree count queue last =
  match last with
    Return x ->
      let ((px, cx), queue) = access_queue queue x in
      flush_all queue [J.Return_statement (Some cx)]
  | Raise x ->
(*XXXXXXXXXXXXXX*)
      let ((px, cx), queue) = access_queue queue x in
      flush_all queue [J.Throw_statement cx]
  | Stop ->
      flush_all queue []
  | Branch cont ->
      branch ctx tree count queue cont
  | Cond (c, x, cont1, cont2) ->
      let ((px, cx), queue) = access_queue queue x in
      let e =
        match c with
          IsTrue         -> cx
        | CEq n          -> J.EBin (J.EqEqEq, cx, int n)
        | CLt n | CUlt n -> J.EBin (J.Lt, cx, int n)
        | CLe n          -> J.EBin (J.Le, cx, int n)
      in
      flush_all queue
      [J.If_statement (e,
                       block (branch ctx tree count [] cont1),
                       Some (block (branch ctx tree count [] cont2)))]
  | Switch (x, a1, a2) ->
      flush_all queue
      [J.Expression_statement (J.EQuote "swtch")]
  | Pushtrap (cont, pc, _) ->
      let invoke_handler =
        [J.Statement (J.Return_statement
                        (Some (J.ECall (J.EVar (addr pc), []))))]
      in
      let body =
        match IntMap.find pc ctx.Ctx.blocks with
          (Some y, _, _) ->
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
           branch ctx tree count [] cont)
  | Poptrap _ ->
      flush_all queue []

and translate_block_contents ctx tree count pc expr_queue =
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
  let seq' = translate_last ctx tree count expr_queue last in
  seq  @ ch @ seq'

and translate_block ctx tree count prev pc =
  let (param, instr, last) = IntMap.find pc ctx.Ctx.blocks in
  let body = source_elements (translate_block_contents ctx tree count pc []) in
  let prev =
    J.Variable_statement [addr pc, Some (J.EFun (None, [], body))] :: prev in
  match param with
    None ->
      prev
  | Some x ->
      J.Variable_statement [Var.to_string x, None] :: prev

and branch ctx tree count queue (pc, arg) =
  if IntMap.find pc count > 1 then begin
    let br = [J.Return_statement (Some (J.ECall (J.EVar (addr pc), [])))] in
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
        let cont = translate_block_contents ctx tree count pc [] in
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
            st @ translate_block_contents ctx tree count pc queue
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
    source_elements (branch ctx tree count [] (pc, None))

let f (pc, blocks, _) live_vars =
  let ctx = Ctx.initial blocks live_vars in
  let p = translate_body ctx pc true in
if single_line then Format.set_margin 999999998;
  Format.printf "\
function jsoo_inject(x) {
switch (typeof x){
case \"object\":
  return x[1];
default:
  return null;
}}
var init_time = (new Date ()).getTime () * 0.001;
function caml_sys_time () {
  return ((new Date ()).getTime () * 0.001 - init_time);
}
function caml_int64_float_of_bits(x) {return x;}
function caml_register_named_value(dz,dx) { return ;}
function caml_sys_get_argv(xx) { return [0, \"foo\", [0, \"foo\", \"bar\"]]; }
function caml_sys_get_config (e) { return [0, \"Unix\", 32]; }
function caml_js_params (e) { return [0]; }
function jsoo_eval(s) { return eval(s); }
function caml_format_int(fmt, i) { return String(i); }
function caml_ml_string_length (s) { return s.length; }
function caml_greaterequal (x, y) {return (x >= y);}
function caml_lessequal (x, y) {return (x <= y);}
function caml_make_vect (len, init){
 b = new Array();
 b[0]= 0;
 for (i = 1; i <= len; i++) b[i]=init;
return b;
}
function caml_make_array (a) { return a; }
function caml_hash_univ_param (count, limit, obj) {
hash_accu = 0;
switch (typeof obj) {
case \"string\":
for (var p = 0;p < obj.length - 1; p++) hash_accu = hash_accu*19+obj[p];
return hash_accu;
default:
document.write(\"hash:\", typeof obj);
}
}
function caml_string_get (s, i) {
return s[i];
}
function caml_array_get_addr (array, index) {
return array[index];
}
function caml_array_set_addr (array, index, newval) {
 array[index]=newval;
return 0;
}
function jsoo_get (f, o){
document.write(\"{\", o, \"|\", f, \"}\");
res = o[f];
document.write(\"==>\", res, \".\");
return res;
//return o[f];
}
function jsoo_call (d, args, o){
document.write(\"call{\", d, \"|\", args.slice(1), \"|\", o, \"}\");
res = o.apply (d, args.slice(1));
document.write(\"==>\", res, \".\");
return res;
//return o.apply (d, args.slice(1));
}
function caml_call_1 (f, v1) {
switch (f[2]) {
case 1:
  return f[1](v1);
case 2:
  return [247, function(v2){return f[1](v1,v2);}, 1]
case 3:
  return [247, function(v2,v3){return f[1](v1,v2,v3);}, 2]
default:
  document.write(f[2]);
  document.write(\"(1)\");
}
}
function caml_call_2 (f, v1, v2) {
switch (f[2]) {
case 2:
  return f[1](v1, v2);
case 3:
  return [247, function(v3){return f[1](v1,v2,v3);}, 1]
default:
  document.write(f[2]);
  document.write(\"(2)\");
}
}
function caml_call_3 (f, v1, v2, v3) {
switch (f[2]) {
case 3:
  return f[1](v1, v2,v3);
default:
  document.write(f);
  document.write(\"(3)\");
}
}
function thread_new (clos, arg) { }
function caml_js_http_get_with_status (url) {
document.write (url);
    var xmlhttp = false;
    var vm = this;
    /* get request object */
    xmlhttp = new XMLHttpRequest();
    /* do request */
//        xmlhttp.onreadystatechange = function () {
//            vm.thread_notify_all (xmlhttp);
//        }
        xmlhttp.open(\"GET\", url, false);
        xmlhttp.send(null);
        var b = [0, xmlhttp.status, xmlhttp.responseText];
        return b;
//        vm.thread_wait (xmlhttp, cont);
}

@.\
%a" Js_output.program p
;Printf.fprintf ch "}\n"
; close_out ch

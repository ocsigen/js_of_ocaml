open! Stdlib
open Code
module W = Wa_ast

(*
LLVM type checker does not work well. It does not handle 'br', and
there is a bug with `return` in clang 15.
Use 'clang-16 --target=wasm32 -Wa,--no-type-check' to disable it.
https://github.com/llvm/llvm-project/issues/56935
https://github.com/llvm/llvm-project/issues/58438
*)

(* binaryen does not support block input parameters
   https://github.com/WebAssembly/binaryen/issues/5047 *)

type context =
  { constants : (Var.t, W.expression) Hashtbl.t
  ; mutable data_segments : (bool * W.data list) Var.Map.t
  ; mutable other_fields : W.module_field list
  }

let make_context () =
  { constants = Hashtbl.create 128; data_segments = Var.Map.empty; other_fields = [] }

type var =
  | Local of int
  | Expr of W.expression t

and state =
  { var_count : int
  ; vars : var Var.Map.t
  ; instrs : W.instruction list
  ; context : context
  }

and 'a t = state -> 'a * state

type expression = Wa_ast.expression t

let ( let* ) (type a b) (e : a t) (f : a -> b t) : b t =
 fun st ->
  let v, st = e st in
  f v st

let return x st = x, st

let expression_list f l =
  let rec loop acc l =
    match l with
    | [] -> return (List.rev acc)
    | x :: r ->
        let* x = f x in
        loop (x :: acc) r
  in
  loop [] l

let register_data_segment x ~active v st =
  st.context.data_segments <- Var.Map.add x (active, v) st.context.data_segments;
  (), st

let get_context st = st.context, st

let register_global name typ init st =
  st.context.other_fields <-
    W.Global { name = S name; typ; init } :: st.context.other_fields;
  (), st

let var x st =
  try Var.Map.find x st.vars, st
  with Not_found -> (
    try Expr (return (Hashtbl.find st.context.constants x)), st
    with Not_found ->
      Format.eprintf "ZZZ %a@." Var.print x;
      Local 0, st)

let add_var x ({ var_count; vars; _ } as st) =
  match Var.Map.find_opt x vars with
  | Some (Local i) -> i, st
  | Some (Expr _) -> assert false
  | None ->
      let i = var_count in
      let vars = Var.Map.add x (Local i) vars in
      i, { st with var_count = var_count + 1; vars }

let define_var x e st = (), { st with vars = Var.Map.add x (Expr e) st.vars }

let instr i : unit t = fun st -> (), { st with instrs = i :: st.instrs }

let blk l st =
  let instrs = st.instrs in
  let (), st = l { st with instrs = [] } in
  List.rev st.instrs, { st with instrs }

module Arith = struct
  let binary op e e' =
    let* e = e in
    let* e' = e' in
    return (W.BinOp (I32 op, e, e'))

  let unary op e =
    let* e = e in
    return (W.UnOp (I32 op, e))

  let ( + ) e e' =
    let* e = e in
    let* e' = e' in
    return
      (match e, e' with
      | W.BinOp (I32 Add, e1, W.Const (I32 n)), W.Const (I32 n') ->
          let n'' = Int32.add n n' in
          if Int32.equal n'' 0l
          then e1
          else W.BinOp (I32 Add, e1, W.Const (I32 (Int32.add n n')))
      | W.Const (I32 n), W.Const (I32 n') -> W.Const (I32 (Int32.add n n'))
      | W.Const (I32 0l), _ -> e'
      | _, W.Const (I32 0l) -> e
      | W.ConstSym (sym, offset), W.Const (I32 n) ->
          W.ConstSym (sym, offset + Int32.to_int n)
      | W.Const _, _ -> W.BinOp (I32 Add, e', e)
      | _ -> W.BinOp (I32 Add, e, e'))

  let ( - ) e e' =
    let* e = e in
    let* e' = e' in
    return
      (match e, e' with
      | W.BinOp (I32 Add, e1, W.Const (I32 n)), W.Const (I32 n') ->
          let n'' = Int32.sub n n' in
          if Int32.equal n'' 0l then e1 else W.BinOp (I32 Add, e1, W.Const (I32 n''))
      | W.Const (I32 n), W.Const (I32 n') -> W.Const (I32 (Int32.sub n n'))
      | _, W.Const (I32 n) ->
          if Int32.equal n 0l then e else W.BinOp (I32 Add, e, W.Const (I32 (Int32.neg n)))
      | _ -> W.BinOp (I32 Sub, e, e'))

  let ( * ) = binary Mul

  let ( lsl ) e e' =
    let* e = e in
    let* e' = e' in
    return
      (match e, e' with
      | W.Const (I32 n), W.Const (I32 n') when Poly.(n' < 31l) ->
          W.Const (I32 (Int32.shift_left n (Int32.to_int n')))
      | _ -> W.BinOp (I32 Shl, e, e'))

  let ( lsr ) = binary (Shr U)

  let ( asr ) = binary (Shr S)

  let ( land ) = binary And

  let ( lor ) = binary Or

  let ( lxor ) = binary Xor

  let ( < ) = binary (Lt S)

  let ( <= ) = binary (Le S)

  let ( = ) = binary Eq

  let ( <> ) = binary Ne

  let ult = binary (Lt U)

  let eqz = unary Eqz

  let const n = return (W.Const (I32 n))
end

let load x =
  let* x = var x in
  match x with
  | Local x -> return (W.LocalGet x)
  | Expr e -> e

let tee x e =
  let* e = e in
  let* i = add_var x in
  return (W.LocalTee (i, e))

let store x e =
  let* e = e in
  let* i = add_var x in
  instr (LocalSet (i, e))

let assign x e =
  let* x = var x in
  let* e = e in
  match x with
  | Local x -> instr (W.LocalSet (x, e))
  | Expr _ -> assert false

let seq l e =
  let* instrs = blk l in
  let* e = e in
  return (W.Seq (instrs, e))

let drop e =
  let* e = e in
  instr (Drop e)

let loop ty l =
  let* instrs = blk l in
  instr (Loop (ty, instrs))

let block ty l =
  let* instrs = blk l in
  instr (Block (ty, instrs))

let if_ ty e l1 l2 =
  let* e = e in
  let* instrs1 = blk l1 in
  let* instrs2 = blk l2 in
  match e with
  | W.UnOp (I32 Eqz, e') -> instr (If (ty, e', instrs2, instrs1))
  | _ -> instr (If (ty, e, instrs1, instrs2))

let function_body ~context ~body =
  let st = { var_count = 0; vars = Var.Map.empty; instrs = []; context } in
  let (), st = body st in
  st.var_count, List.rev st.instrs

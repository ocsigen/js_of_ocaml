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

type constant_global =
  { init : W.expression option
  ; constant : bool
  }

type context =
  { constants : (Var.t, W.expression) Hashtbl.t
  ; mutable data_segments : (bool * W.data list) Var.Map.t
  ; mutable constant_globals : constant_global Var.Map.t
  ; mutable other_fields : W.module_field list
  ; mutable imports : (Var.t * Wa_ast.import_desc) StringMap.t StringMap.t
  ; types : (string, Var.t) Hashtbl.t
  ; mutable closure_envs : Var.t Var.Map.t
        (** GC: mapping of recursive functions to their shared environment *)
  ; mutable apply_funs : Var.t IntMap.t
  ; mutable cps_apply_funs : Var.t IntMap.t
  ; mutable curry_funs : Var.t IntMap.t
  ; mutable cps_curry_funs : Var.t IntMap.t
  ; mutable dummy_funs : Var.t IntMap.t
  ; mutable cps_dummy_funs : Var.t IntMap.t
  ; mutable init_code : W.instruction list
  }

let make_context () =
  { constants = Hashtbl.create 128
  ; data_segments = Var.Map.empty
  ; constant_globals = Var.Map.empty
  ; other_fields = []
  ; imports = StringMap.empty
  ; types = Hashtbl.create 128
  ; closure_envs = Var.Map.empty
  ; apply_funs = IntMap.empty
  ; cps_apply_funs = IntMap.empty
  ; curry_funs = IntMap.empty
  ; cps_curry_funs = IntMap.empty
  ; dummy_funs = IntMap.empty
  ; cps_dummy_funs = IntMap.empty
  ; init_code = []
  }

type var =
  | Local of int * W.value_type option
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

let get_data_segment x st = Var.Map.find x st.context.data_segments, st

let get_context st = st.context, st

let register_constant x e st =
  Hashtbl.add st.context.constants x e;
  (), st

type type_def =
  { supertype : Wa_ast.var option
  ; final : bool
  ; typ : Wa_ast.str_type
  }

let register_type nm gen_typ st =
  let context = st.context in
  let { supertype; final; typ }, st = gen_typ () st in
  ( (try Hashtbl.find context.types nm
     with Not_found ->
       let name = Var.fresh_n nm in
       context.other_fields <-
         Type [ { name; typ; supertype; final } ] :: context.other_fields;
       Hashtbl.add context.types nm name;
       name)
  , st )

let register_global name ?(constant = false) typ init st =
  st.context.other_fields <- W.Global { name; typ; init } :: st.context.other_fields;
  (match name with
  | S _ -> ()
  | V name ->
      st.context.constant_globals <-
        Var.Map.add
          name
          { init = (if not typ.mut then Some init else None)
          ; constant = (not typ.mut) || constant
          }
          st.context.constant_globals);
  (), st

let global_is_constant name =
  let* ctx = get_context in
  return
    (match Var.Map.find_opt name ctx.constant_globals with
    | Some { constant = true; _ } -> true
    | _ -> false)

let get_global name =
  let* ctx = get_context in
  return
    (match Var.Map.find_opt name ctx.constant_globals with
    | Some { init; _ } -> init
    | _ -> None)

let register_import ?(import_module = "env") ~name typ st =
  ( (try
       let x, typ' =
         StringMap.find name (StringMap.find import_module st.context.imports)
       in
       (*ZZZ error message*)
       assert (Poly.equal typ typ');
       x
     with Not_found ->
       let x = Var.fresh_n name in
       st.context.imports <-
         StringMap.update
           import_module
           (fun m ->
             Some
               (match m with
               | None -> StringMap.singleton name (x, typ)
               | Some m -> StringMap.add name (x, typ) m))
           st.context.imports;
       x)
  , st )

let register_init_code code st =
  let st' = { var_count = 0; vars = Var.Map.empty; instrs = []; context = st.context } in
  let (), st' = code st' in
  st.context.init_code <- st'.instrs @ st.context.init_code;
  (), st

let set_closure_env f env st =
  st.context.closure_envs <- Var.Map.add f env st.context.closure_envs;
  (), st

let get_closure_env f st = Var.Map.find f st.context.closure_envs, st

let is_closure f st = Var.Map.mem f st.context.closure_envs, st

let var x st =
  try Var.Map.find x st.vars, st
  with Not_found -> Expr (return (Hashtbl.find st.context.constants x)), st

let add_var ?typ x ({ var_count; vars; _ } as st) =
  match Var.Map.find_opt x vars with
  | Some (Local (i, typ')) ->
      assert (Poly.equal typ typ');
      i, st
  | Some (Expr _) -> assert false
  | None ->
      let i = var_count in
      let vars = Var.Map.add x (Local (i, typ)) vars in
      i, { st with var_count = var_count + 1; vars }

let define_var x e st = (), { st with vars = Var.Map.add x (Expr e) st.vars }

let instr i : unit t = fun st -> (), { st with instrs = i :: st.instrs }

let instrs l : unit t = fun st -> (), { st with instrs = List.rev_append l st.instrs }

let blk l st =
  let instrs = st.instrs in
  let (), st = l { st with instrs = [] } in
  List.rev st.instrs, { st with instrs }

let cast ?(nullable = false) typ e =
  let* e = e in
  match typ, e with
  | W.I31, W.I31New _ -> return e
  | _ -> return (W.RefCast ({ W.nullable; typ }, e))

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

  let ( / ) = binary (Div S)

  let ( mod ) = binary (Rem S)

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

  let uge = binary (Ge U)

  let eqz = unary Eqz

  let const n = return (W.Const (I32 n))

  let to_int31 n =
    let* n = n in
    match n with
    | W.I31Get (S, n') -> return n'
    | _ -> return (W.I31New n)

  let of_int31 n =
    let* n = n in
    match n with
    | W.I31New (Const (I32 n)) -> return (W.Const (I32 (Int31.wrap n)))
    | _ -> return (W.I31Get (S, n))
end

let is_small_constant e =
  match e with
  | W.ConstSym _ | W.Const _ | W.I31New (W.Const _) | W.RefFunc _ -> return true
  | W.GlobalGet (V name) -> global_is_constant name
  | _ -> return false

let load x =
  let* x = var x in
  match x with
  | Local (x, _) -> return (W.LocalGet x)
  | Expr e -> e

let tee ?typ x e =
  let* e = e in
  let* b = is_small_constant e in
  if b
  then
    let* () = register_constant x e in
    return e
  else
    let* i = add_var ?typ x in
    return (W.LocalTee (i, e))

let rec store ?(always = false) ?typ x e =
  let* e = e in
  match e with
  | W.Seq (l, e') ->
      let* () = instrs l in
      store ~always ?typ x (return e')
  | _ ->
      let* b = is_small_constant e in
      if b && not always
      then register_constant x e
      else
        let* i = add_var ?typ x in
        instr (LocalSet (i, e))

let assign x e =
  let* x = var x in
  let* e = e in
  match x with
  | Local (x, _) -> instr (W.LocalSet (x, e))
  | Expr _ -> assert false

let seq l e =
  let* instrs = blk l in
  let* e = e in
  return (W.Seq (instrs, e))

let drop e =
  let* e = e in
  match e with
  | W.Seq (l, e') ->
      let* b = is_small_constant e' in
      if b then instrs l else instr (Drop e)
  | _ -> instr (Drop e)

let push e =
  let* e = e in
  match e with
  | W.Seq (l, e') ->
      let* () = instrs l in
      instr (Push e')
  | _ -> instr (Push e)

let loop ty l =
  let* instrs = blk l in
  instr (Loop (ty, instrs))

let block ty l =
  let* instrs = blk l in
  instr (Block (ty, instrs))

let block_expr ty l =
  let* instrs = blk l in
  return (W.BlockExpr (ty, instrs))

let if_ ty e l1 l2 =
  let* e = e in
  let* instrs1 = blk l1 in
  let* instrs2 = blk l2 in
  match e with
  | W.UnOp (I32 Eqz, e') -> instr (If (ty, e', instrs2, instrs1))
  | _ -> instr (If (ty, e, instrs1, instrs2))

let try_ ty body handlers =
  let* body = blk body in
  let tags = List.map ~f:fst handlers in
  let* handler_bodies = expression_list blk (List.map ~f:snd handlers) in
  instr (Try (ty, body, List.combine tags handler_bodies, None))

let need_apply_fun ~cps ~arity st =
  let ctx = st.context in
  ( (if cps
     then (
       try IntMap.find arity ctx.cps_apply_funs
       with Not_found ->
         let x = Var.fresh_n (Printf.sprintf "cps_apply_%d" arity) in
         ctx.cps_apply_funs <- IntMap.add arity x ctx.cps_apply_funs;
         x)
     else
       try IntMap.find arity ctx.apply_funs
       with Not_found ->
         let x = Var.fresh_n (Printf.sprintf "apply_%d" arity) in
         ctx.apply_funs <- IntMap.add arity x ctx.apply_funs;
         x)
  , st )

let need_curry_fun ~cps ~arity st =
  let ctx = st.context in
  ( (if cps
     then (
       try IntMap.find arity ctx.cps_curry_funs
       with Not_found ->
         let x = Var.fresh_n (Printf.sprintf "cps_curry_%d" arity) in
         ctx.cps_curry_funs <- IntMap.add arity x ctx.cps_curry_funs;
         x)
     else
       try IntMap.find arity ctx.curry_funs
       with Not_found ->
         let x = Var.fresh_n (Printf.sprintf "curry_%d" arity) in
         ctx.curry_funs <- IntMap.add arity x ctx.curry_funs;
         x)
  , st )

let need_dummy_fun ~cps ~arity st =
  let ctx = st.context in
  ( (if cps
     then (
       try IntMap.find arity ctx.cps_dummy_funs
       with Not_found ->
         let x = Var.fresh_n (Printf.sprintf "cps_dummy_%d" arity) in
         ctx.cps_dummy_funs <- IntMap.add arity x ctx.cps_dummy_funs;
         x)
     else
       try IntMap.find arity ctx.dummy_funs
       with Not_found ->
         let x = Var.fresh_n (Printf.sprintf "dummy_%d" arity) in
         ctx.dummy_funs <- IntMap.add arity x ctx.dummy_funs;
         x)
  , st )

let init_code context = instrs context.init_code

let function_body ~context ~value_type ~param_count ~body =
  let st = { var_count = 0; vars = Var.Map.empty; instrs = []; context } in
  let (), st = body st in
  let local_count, body = st.var_count, List.rev st.instrs in
  let local_types = Array.make local_count None in
  Var.Map.iter
    (fun _ v ->
      match v with
      | Local (i, typ) -> local_types.(i) <- typ
      | Expr _ -> ())
    st.vars;
  let body = Wa_tail_call.f body in
  let locals =
    local_types
    |> Array.map ~f:(fun v -> Option.value ~default:value_type v)
    |> (fun a -> Array.sub a ~pos:param_count ~len:(Array.length a - param_count))
    |> Array.to_list
  in
  locals, body

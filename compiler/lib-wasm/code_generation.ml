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
module W = Wasm_ast

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
  ; typ : W.value_type
  }

type context =
  { constants : W.expression Var.Hashtbl.t
  ; mutable data_segments : string Var.Map.t
  ; mutable constant_globals : constant_global Var.Map.t
  ; mutable other_fields : W.module_field list
  ; mutable imports : (Var.t * Wasm_ast.import_desc) StringMap.t StringMap.t
  ; type_names : Var.t String.Hashtbl.t
  ; types : Wasm_ast.type_field Var.Hashtbl.t
  ; mutable closure_envs : Var.t Var.Map.t
        (** GC: mapping of recursive functions to their shared environment *)
  ; closure_types : (W.value_type option list, int) Hashtbl.t
  ; mutable apply_funs : Var.t IntMap.t
  ; mutable cps_apply_funs : Var.t IntMap.t
  ; mutable curry_funs : Var.t IntMap.t
  ; mutable cps_curry_funs : Var.t IntMap.t
  ; mutable dummy_funs : Var.t IntMap.t
  ; mutable cps_dummy_funs : Var.t IntMap.t
  ; mutable init_code : W.instruction list
  ; mutable fragments : Javascript.expression StringMap.t
  ; mutable globalized_variables : Var.Set.t
  ; value_type : W.value_type
  ; mutable unit_name : string option
  }

let make_context ~value_type =
  { constants = Var.Hashtbl.create 128
  ; data_segments = Var.Map.empty
  ; constant_globals = Var.Map.empty
  ; other_fields = []
  ; imports = StringMap.empty
  ; type_names = String.Hashtbl.create 128
  ; types = Var.Hashtbl.create 128
  ; closure_envs = Var.Map.empty
  ; closure_types = Poly.Hashtbl.create 128
  ; apply_funs = IntMap.empty
  ; cps_apply_funs = IntMap.empty
  ; curry_funs = IntMap.empty
  ; cps_curry_funs = IntMap.empty
  ; dummy_funs = IntMap.empty
  ; cps_dummy_funs = IntMap.empty
  ; init_code = []
  ; fragments = StringMap.empty
  ; globalized_variables = Var.Set.empty
  ; value_type
  ; unit_name = None
  }

type var =
  | Local of int * Var.t * W.value_type option
  | Expr of W.expression t

and state =
  { var_count : int
  ; vars : var Var.Map.t
  ; instrs : W.instruction list
  ; context : context
  }

and 'a t = state -> 'a * state

type expression = Wasm_ast.expression t

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

let register_data_segment x v st =
  st.context.data_segments <- Var.Map.add x v st.context.data_segments;
  (), st

let get_context st = st.context, st

let register_constant x e st =
  Var.Hashtbl.add st.context.constants x e;
  (), st

type type_def =
  { supertype : Wasm_ast.var option
  ; final : bool
  ; typ : Wasm_ast.str_type
  }

let register_type nm gen_typ st =
  let context = st.context in
  let { supertype; final; typ }, st = gen_typ () st in
  ( (try String.Hashtbl.find context.type_names nm
     with Not_found ->
       let name = Var.fresh_n nm in
       let type_field = { Wasm_ast.name; typ; supertype; final } in
       context.other_fields <- Type [ type_field ] :: context.other_fields;
       String.Hashtbl.add context.type_names nm name;
       Var.Hashtbl.add context.types name type_field;
       name)
  , st )

let rec type_index_sub ty ty' st =
  if Var.equal ty ty'
  then true, st
  else
    let type_field = Var.Hashtbl.find st.context.types ty in
    match type_field.supertype with
    | None -> false, st
    | Some ty -> type_index_sub ty ty' st

let heap_type_sub (ty : W.heap_type) (ty' : W.heap_type) st =
  match ty, ty' with
  | Func, Func
  | Extern, Extern
  | (Any | Eq | Struct | Array | I31 | None_ | Type _), Any
  | (Eq | Struct | Array | I31 | None_ | Type _), Eq
  | (None_ | Struct), Struct -> true, st
  | (None_ | Array), Array -> true, st
  | (None_ | I31), I31 -> true, st
  | None_, None_ -> true, st
  | Type t, Struct ->
      ( (let type_field = Var.Hashtbl.find st.context.types t in
         match type_field.typ with
         | Struct _ -> true
         | Array _ | Func _ -> false)
      , st )
  | Type t, Array ->
      ( (let type_field = Var.Hashtbl.find st.context.types t in
         match type_field.typ with
         | Array _ -> true
         | Struct _ | Func _ -> false)
      , st )
  | Type t, Type t' -> type_index_sub t t' st
  | None_, Type t ->
      ( (let type_field = Var.Hashtbl.find st.context.types t in
         match type_field.typ with
         | Struct _ | Array _ -> true
         | Func _ -> false)
      , st )
  (* Func and Extern are only in suptyping relation with themselves *)
  | Func, _
  | _, Func
  | Extern, _
  | _, Extern
  (* Any has no supertype *)
  | Any, _
  (* I31, struct, array and none have no other subtype *)
  | _, (I31 | Type _ | Struct | Array | None_) -> false, st

let register_global name ?exported_name ?(constant = false) typ init st =
  st.context.other_fields <-
    W.Global { name; exported_name; typ; init } :: st.context.other_fields;
  st.context.constant_globals <-
    Var.Map.add
      name
      { init = (if not typ.mut then Some init else None)
      ; constant = (not typ.mut) || constant
      ; typ = typ.typ
      }
      st.context.constant_globals;
  (), st

let global_is_registered name =
  let* ctx = get_context in
  return (Var.Map.mem name ctx.constant_globals)

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

let register_fragment name f st =
  let context = st.context in
  if not (StringMap.mem name context.fragments)
  then context.fragments <- StringMap.add name (f ()) context.fragments;
  (), st

let set_closure_env f env st =
  st.context.closure_envs <- Var.Map.add f env st.context.closure_envs;
  (), st

let get_closure_env f st = Var.Map.find f st.context.closure_envs, st

let is_closure f st = Var.Map.mem f st.context.closure_envs, st

let unit_name st = st.context.unit_name, st

let var x st =
  try Var.Map.find x st.vars, st
  with Not_found -> Expr (return (Var.Hashtbl.find st.context.constants x)), st

let add_var ?typ x ({ var_count; vars; _ } as st) =
  match Var.Map.find_opt x vars with
  | Some (Local (_, x', typ')) ->
      assert (Poly.equal typ typ');
      x', st
  | Some (Expr _) -> assert false
  | None ->
      let i = var_count in
      let vars = Var.Map.add x (Local (i, x, typ)) vars in
      x, { st with var_count = var_count + 1; vars }

let define_var x e st = (), { st with vars = Var.Map.add x (Expr e) st.vars }

let instr i : unit t = fun st -> (), { st with instrs = i :: st.instrs }

let instrs l : unit t = fun st -> (), { st with instrs = List.rev_append l st.instrs }

let blk l st =
  let instrs = st.instrs in
  let (), st = l { st with instrs = [] } in
  List.rev st.instrs, { st with instrs }

let event loc : unit t =
 fun st ->
  ( ()
  , match st.instrs with
    | Event _ :: instrs | instrs -> { st with instrs = Event loc :: instrs } )

let hidden_location =
  { Parse_info.src = Some Wasm_source_map.blackbox_filename
  ; name = None
  ; col = 0
  ; line = 1
  ; idx = 0
  }

let no_event = event hidden_location

let cast ?(nullable = false) typ e =
  let* e = e in
  match typ, e with
  | W.I31, W.RefI31 _ -> return e
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
      | W.Const (I32 n), W.Const (I32 n') when Int32.(n' < 31l) ->
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
    | _ -> return (W.RefI31 n)

  let wrap31 n = Targetint.(of_int32_truncate n |> to_int32)

  let of_int31 n =
    let* n = n in
    match n with
    | W.RefI31 (Const (I32 n)) -> return (W.Const (I32 (wrap31 n)))
    | _ -> return (W.I31Get (S, n))
end

let is_small_constant e =
  match e with
  | W.Const _ | W.RefI31 (W.Const _) | W.RefFunc _ -> return true
  | W.GlobalGet name -> global_is_constant name
  | _ -> return false

let load x =
  let* x = var x in
  match x with
  | Local (_, x, _) -> return (W.LocalGet x)
  | Expr e -> e

let rec variable_type x st =
  match Var.Map.find_opt x st.vars with
  | Some (Local (_, _, typ)) -> typ, st
  | Some (Expr e) ->
      (let* e = e in
       expression_type e)
        st
  | None -> None, st

and expression_type (e : W.expression) st =
  match e with
  | Const _
  | UnOp _
  | BinOp _
  | I32WrapI64 _
  | I64ExtendI32 _
  | F32DemoteF64 _
  | F64PromoteF32 _
  | BlockExpr _
  | Call _
  | RefFunc _
  | Call_ref _
  | I31Get _
  | ArrayGet _
  | ArrayLen _
  | RefTest _
  | RefEq _
  | RefNull _
  | Try _
  | Br_on_null _ -> None, st
  | LocalGet x | LocalTee (x, _) -> variable_type x st
  | GlobalGet x ->
      ( (try
           let typ = (Var.Map.find x st.context.constant_globals).typ in
           if Poly.equal typ st.context.value_type
           then None
           else
             Some
               (match typ with
               | Ref { typ; nullable = true } -> Ref { typ; nullable = false }
               | _ -> typ)
         with Not_found -> None)
      , st )
  | Seq (_, e') -> expression_type e' st
  | Pop typ -> Some typ, st
  | RefI31 _ -> Some (Ref { nullable = false; typ = I31 }), st
  | ArrayNew (ty, _, _)
  | ArrayNewFixed (ty, _)
  | ArrayNewData (ty, _, _, _)
  | StructNew (ty, _) -> Some (Ref { nullable = false; typ = Type ty }), st
  | StructGet (_, ty, i, _) -> (
      match (Var.Hashtbl.find st.context.types ty).typ with
      | Struct l -> (
          match (List.nth l i).typ with
          | Value typ ->
              (if Poly.equal typ st.context.value_type then None else Some typ), st
          | Packed _ -> assert false)
      | Array _ | Func _ -> assert false)
  | RefCast (typ, _) | Br_on_cast (_, _, typ, _) | Br_on_cast_fail (_, typ, _, _) ->
      Some (Ref typ), st
  | IfExpr (_, _, _, _) | ExternConvertAny _ | AnyConvertExtern _ -> None, st

let tee ?typ x e =
  let* e = e in
  let* b = is_small_constant e in
  if b
  then
    let* () = register_constant x e in
    return e
  else
    let* typ =
      match typ with
      | Some _ -> return typ
      | None -> expression_type e
    in
    let* i = add_var ?typ x in
    return (W.LocalTee (i, e))

let should_make_global x st = Var.Set.mem x st.context.globalized_variables, st

let value_type st = st.context.value_type, st

let get_constant x st = Var.Hashtbl.find_opt st.context.constants x, st

let placeholder_value typ f =
  let* c = get_constant typ in
  match c with
  | None ->
      let x = Var.fresh () in
      let* () = register_constant typ (W.GlobalGet x) in
      let* () =
        register_global
          ~constant:true
          x
          { mut = false; typ = Ref { nullable = false; typ = Type typ } }
          (f typ)
      in
      return (W.GlobalGet x)
  | Some c -> return c

let array_placeholder typ = placeholder_value typ (fun typ -> ArrayNewFixed (typ, []))

let default_value val_typ st =
  match val_typ with
  | W.Ref { typ = I31 | Eq | Any; _ } -> (W.RefI31 (Const (I32 0l)), val_typ, None), st
  | W.Ref { typ = Type typ; nullable = false } -> (
      match (Var.Hashtbl.find st.context.types typ).typ with
      | Array _ ->
          (let* placeholder = array_placeholder typ in
           return (placeholder, val_typ, None))
            st
      | Struct _ | Func _ ->
          ( ( W.RefNull (Type typ)
            , W.Ref { typ = Type typ; nullable = true }
            , Some { W.typ = Type typ; nullable = false } )
          , st ))
  | I32 -> (Const (I32 0l), val_typ, None), st
  | F32 -> (Const (F32 0.), val_typ, None), st
  | I64 -> (Const (I64 0L), val_typ, None), st
  | F64 -> (Const (F64 0.), val_typ, None), st
  | W.Ref { nullable = true; _ }
  | W.Ref { typ = Func | Extern | Struct | Array | None_; _ } -> assert false

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
        let* b = should_make_global x in
        if b
        then
          let* () =
            let* b = global_is_registered x in
            if b
            then return ()
            else
              let* typ =
                match typ with
                | Some typ -> return typ
                | None -> (
                    if always
                    then value_type
                    else
                      let* typ = expression_type e in
                      match typ with
                      | None -> value_type
                      | Some typ -> return typ)
              in
              let* default, typ', cast = default_value typ in
              let* () =
                register_constant
                  x
                  (match cast with
                  | Some typ -> W.RefCast (typ, W.GlobalGet x)
                  | None -> W.GlobalGet x)
              in
              register_global ~constant:true x { mut = true; typ = typ' } default
          in
          instr (GlobalSet (x, e))
        else
          let* typ =
            match typ with
            | Some _ -> return typ
            | None -> if always then return None else expression_type e
          in
          let* i = add_var ?typ x in
          instr (LocalSet (i, e))

let assign x e =
  let* x = var x in
  let* e = e in
  match x with
  | Local (_, x, _) -> instr (W.LocalSet (x, e))
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
      let* () = instrs l in
      if b then return () else instr (Drop e')
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

let try_expr ty body handlers =
  let* body = blk body in
  return (W.Try (ty, body, handlers))

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

let function_body ~context ~param_names ~body =
  let st = { var_count = 0; vars = Var.Map.empty; instrs = []; context } in
  let (), st = body st in
  let local_count, body = st.var_count, List.rev st.instrs in
  let local_types = Array.make local_count (Var.fresh (), None) in
  List.iteri ~f:(fun i x -> local_types.(i) <- x, None) param_names;
  Var.Map.iter
    (fun _ v ->
      match v with
      | Local (i, x, typ) -> local_types.(i) <- x, typ
      | Expr _ -> ())
    st.vars;
  let body = Tail_call.f body in
  let param_count = List.length param_names in
  let locals =
    local_types
    |> Array.map ~f:(fun (x, v) -> x, Option.value ~default:context.value_type v)
    |> (fun a -> Array.sub a ~pos:param_count ~len:(Array.length a - param_count))
    |> Array.to_list
  in
  locals, body

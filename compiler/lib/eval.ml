(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2013 Hugo Heuzard
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
open Flow

let static_env = Hashtbl.create 17

let clear_static_env () = Hashtbl.clear static_env

let set_static_env s value = Hashtbl.add static_env s value

let get_static_env s = try Some (Hashtbl.find static_env s) with Not_found -> None

let int_unop l f =
  match l with
  | [ Int i ] -> Some (Int (f i))
  | _ -> None

let int_binop l f =
  match l with
  | [ Int i; Int j ] -> Some (Int (f i j))
  | _ -> None

(* For when the underlying function takes an [int] (not [t]) as its second argument *)
let shift_op l f =
  match l with
  | [ Int i; Int j ] -> Some (Int (f i (Targetint.to_int_exn j)))
  | _ -> None

let float_binop_aux (l : constant list) (f : float -> float -> 'a) : 'a option =
  let args =
    match l with
    | [ Float i; Float j ] -> Some (i, j)
    | [ Int i; Int j ] -> Some (Targetint.to_float i, Targetint.to_float j)
    | [ Int i; Float j ] -> Some (Targetint.to_float i, j)
    | [ Float i; Int j ] -> Some (i, Targetint.to_float j)
    | _ -> None
  in
  match args with
  | None -> None
  | Some (i, j) -> Some (f i j)

let float_binop (l : constant list) (f : float -> float -> float) : constant option =
  match float_binop_aux l f with
  | Some x -> Some (Float x)
  | None -> None

let float_unop (l : constant list) (f : float -> float) : constant option =
  match l with
  | [ Float i ] -> Some (Float (f i))
  | [ Int i ] -> Some (Float (f (Targetint.to_float i)))
  | _ -> None

let bool' b = Int Targetint.(if b then one else zero)

let bool b = Some (bool' b)

let float_binop_bool l f =
  match float_binop_aux l f with
  | Some b -> bool b
  | None -> None

let eval_prim x =
  match x with
  | Not, [ Int i ] -> bool (Targetint.is_zero i)
  | Lt, [ Int i; Int j ] -> bool Targetint.(i < j)
  | Le, [ Int i; Int j ] -> bool Targetint.(i <= j)
  | Eq, [ Int i; Int j ] -> bool Targetint.(i = j)
  | Neq, [ Int i; Int j ] -> bool Targetint.(i <> j)
  | Ult, [ Int i; Int j ] -> bool (Targetint.(j < zero) || Targetint.(i < j))
  | Extern name, l -> (
      let name = Primitive.resolve name in
      match name, l with
      (* int *)
      | "%int_add", _ -> int_binop l Targetint.add
      | "%int_sub", _ -> int_binop l Targetint.sub
      | "%direct_int_mul", _ -> int_binop l Targetint.mul
      | "%direct_int_div", [ _; Int x ] when Targetint.is_zero x -> None
      | "%direct_int_div", _ -> int_binop l Targetint.div
      | "%direct_int_mod", _ -> int_binop l Targetint.rem
      | "%int_and", _ -> int_binop l Targetint.logand
      | "%int_or", _ -> int_binop l Targetint.logor
      | "%int_xor", _ -> int_binop l Targetint.logxor
      | "%int_lsl", _ -> shift_op l Targetint.shift_left
      | "%int_lsr", _ -> shift_op l Targetint.shift_right_logical
      | "%int_asr", _ -> shift_op l Targetint.shift_right
      | "%int_neg", _ -> int_unop l Targetint.neg
      (* float *)
      | "caml_eq_float", _ -> float_binop_bool l Float.( = )
      | "caml_neq_float", _ -> float_binop_bool l Float.( <> )
      | "caml_ge_float", _ -> float_binop_bool l Float.( >= )
      | "caml_le_float", _ -> float_binop_bool l Float.( <= )
      | "caml_gt_float", _ -> float_binop_bool l Float.( > )
      | "caml_lt_float", _ -> float_binop_bool l Float.( < )
      | "caml_add_float", _ -> float_binop l ( +. )
      | "caml_sub_float", _ -> float_binop l ( -. )
      | "caml_mul_float", _ -> float_binop l ( *. )
      | "caml_div_float", _ -> float_binop l ( /. )
      | "caml_fmod_float", _ -> float_binop l mod_float
      | "caml_int_of_float", [ Float f ] -> (
          match Targetint.of_float_opt f with
          | None -> None
          | Some f -> Some (Int f))
      (* Math *)
      | "caml_neg_float", _ -> float_unop l ( ~-. )
      | "caml_abs_float", _ -> float_unop l abs_float
      | "caml_acos_float", _ -> float_unop l acos
      | "caml_asin_float", _ -> float_unop l asin
      | "caml_atan_float", _ -> float_unop l atan
      | "caml_atan2_float", _ -> float_binop l atan2
      | "caml_ceil_float", _ -> float_unop l ceil
      | "caml_cos_float", _ -> float_unop l cos
      | "caml_exp_float", _ -> float_unop l exp
      | "caml_floor_float", _ -> float_unop l floor
      | "caml_log_float", _ -> float_unop l log
      | "caml_power_float", _ -> float_binop l ( ** )
      | "caml_sin_float", _ -> float_unop l sin
      | "caml_sqrt_float", _ -> float_unop l sqrt
      | "caml_tan_float", _ -> float_unop l tan
      | ("caml_string_get" | "caml_string_unsafe_get"), [ String s; Int pos ] ->
          let pos = Targetint.to_int_exn pos in
          if Config.Flag.safe_string () && pos >= 0 && pos < String.length s
          then Some (Int (Targetint.of_int_exn (Char.code s.[pos])))
          else None
      | "caml_string_equal", [ String s1; String s2 ] -> bool (String.equal s1 s2)
      | "caml_string_notequal", [ String s1; String s2 ] ->
          bool (not (String.equal s1 s2))
      | "caml_sys_getenv", [ String s ] -> (
          match get_static_env s with
          | Some env -> Some (String env)
          | None -> None)
      | "caml_sys_const_word_size", [ _ ] -> Some (Int (Targetint.of_int_exn 32))
      | "caml_sys_const_int_size", [ _ ] ->
          Some (Int (Targetint.of_int_exn (Targetint.num_bits ())))
      | "caml_sys_const_big_endian", [ _ ] -> Some (Int Targetint.zero)
      | "caml_sys_const_naked_pointers_checked", [ _ ] -> Some (Int Targetint.zero)
      | _ -> None)
  | _ -> None

let the_length_of ~target info x =
  get_approx
    info
    (fun x ->
      match Flow.Info.def info x with
      | Some (Constant (String s)) -> Some (Targetint.of_int_exn (String.length s))
      | Some (Prim (Extern "caml_create_string", [ arg ]))
      | Some (Prim (Extern "caml_create_bytes", [ arg ])) -> the_int ~target info arg
      | None | Some _ -> None)
    None
    (fun u v ->
      match u, v with
      | Some l, Some l' when Targetint.(l = l') -> Some l
      | _ -> None)
    x

type is_int =
  | Y
  | N
  | Unknown

let is_int info x =
  match x with
  | Pv x ->
      get_approx
        info
        (fun x ->
          match Flow.Info.def info x with
          | Some (Constant (Int _)) -> Y
          | Some (Constant (NativeInt _ | Int32 _)) ->
              (* These Wasm-specific constants are boxed *)
              N
          | Some (Block (_, _, _, _) | Constant _) -> N
          | None | Some _ -> Unknown)
        Unknown
        (fun u v ->
          match u, v with
          | Y, Y -> Y
          | N, N -> N
          | _ -> Unknown)
        x
  | Pc (Int _) -> Y
  | Pc (NativeInt _ | Int32 _) ->
      (* These Wasm-specific constants are boxed *)
      N
  | Pc _ -> N

let the_tag_of info x get =
  match x with
  | Pv x ->
      get_approx
        info
        (fun x ->
          match Flow.Info.def info x with
          | Some (Block (j, _, _, mut)) ->
              if Flow.Info.possibly_mutable info x
              then (
                assert (Poly.(mut = Maybe_mutable));
                None)
              else get j
          | Some (Constant (Tuple (j, _, _))) -> get j
          | None | Some _ -> None)
        None
        (fun u v ->
          match u, v with
          | Some i, Some j when Poly.(i = j) -> u
          | _ -> None)
        x
  | Pc (Tuple (j, _, _)) -> get j
  | _ -> None

let the_cont_of info x (a : cont array) =
  (* The value of [x] might be meaningless when we're inside a dead code.
     The proper fix would be to remove the deadcode entirely.
     Meanwhile, add guards to prevent Invalid_argument("index out of bounds")
     see https://github.com/ocsigen/js_of_ocaml/issues/485 *)
  let get i = if i >= 0 && i < Array.length a then Some a.(i) else None in
  get_approx
    info
    (fun x ->
      match Flow.Info.def info x with
      | Some (Prim (Extern "%direct_obj_tag", [ b ])) -> the_tag_of info b get
      | Some (Constant (Int j)) -> get (Targetint.to_int_exn j)
      | None | Some _ -> None)
    None
    (fun u v ->
      match u, v with
      | Some i, Some j when Poly.(i = j) -> u
      | _ -> None)
    x

(* If [constant_js_equal a b = Some v], then [caml_js_equals a b = v]). *)
let constant_js_equal a b =
  match a, b with
  | Int i, Int j -> Some (Targetint.equal i j)
  | Float a, Float b -> Some (Float.ieee_equal a b)
  | NativeString a, NativeString b -> Some (Native_string.equal a b)
  | String a, String b when Config.Flag.use_js_string () -> Some (String.equal a b)
  | Int _, Float _ | Float _, Int _ -> None
  (* All other values may be distinct objects and thus different by [caml_js_equals]. *)
  | String _, _
  | _, String _
  | NativeString _, _
  | _, NativeString _
  | Float_array _, _
  | _, Float_array _
  | Int64 _, _
  | _, Int64 _
  | Int32 _, _
  | _, Int32 _
  | NativeInt _, _
  | _, NativeInt _
  | Tuple _, _
  | _, Tuple _ -> None

let eval_instr ~target info i =
  match i with
  | Let (x, Prim (Extern (("caml_equal" | "caml_notequal") as prim), [ y; z ])) -> (
      match the_const_of ~target info y, the_const_of ~target info z with
      | Some e1, Some e2 -> (
          match Code.Constant.ocaml_equal e1 e2 with
          | None -> [ i ]
          | Some c ->
              let c =
                match prim with
                | "caml_equal" -> c
                | "caml_notequal" -> not c
                | _ -> assert false
              in
              let c = Constant (bool' c) in
              Flow.Info.update_def info x c;
              [ Let (x, c) ])
      | _ -> [ i ])
  | Let (x, Prim (Extern ("caml_js_equals" | "caml_js_strict_equals"), [ y; z ])) -> (
      match the_const_of ~target info y, the_const_of ~target info z with
      | Some e1, Some e2 -> (
          match constant_js_equal e1 e2 with
          | None -> [ i ]
          | Some c ->
              let c = Constant (bool' c) in
              Flow.Info.update_def info x c;
              [ Let (x, c) ])
      | _ -> [ i ])
  | Let (x, Prim (Extern "caml_ml_string_length", [ s ])) -> (
      let c =
        match s with
        | Pc (String s) -> Some (Targetint.of_int_exn (String.length s))
        | Pv v -> the_length_of ~target info v
        | _ -> None
      in
      match c with
      | None -> [ i ]
      | Some c ->
          let c = Constant (Int c) in
          Flow.Info.update_def info x c;
          [ Let (x, c) ])
  | Let
      ( _
      , Prim
          ( ( Extern
                ( "caml_array_unsafe_get"
                | "caml_array_unsafe_set"
                | "caml_floatarray_unsafe_get"
                | "caml_floatarray_unsafe_set"
                | "caml_array_unsafe_set_addr" )
            | Array_get )
          , _ ) ) ->
      (* Fresh parameters can be introduced for these primitives
           in Specialize_js, which would make the call to [the_const_of]
           below fail. *)
      [ i ]
  | Let (x, Prim (IsInt, [ y ])) -> (
      match is_int info y with
      | Unknown -> [ i ]
      | (Y | N) as b ->
          let c = Constant (bool' Poly.(b = Y)) in
          Flow.Info.update_def info x c;
          [ Let (x, c) ])
  | Let (x, Prim (Extern "%direct_obj_tag", [ y ])) -> (
      match the_tag_of info y (fun x -> Some x) with
      | Some tag ->
          let c = Constant (Int (Targetint.of_int_exn tag)) in
          Flow.Info.update_def info x c;
          [ Let (x, c) ]
      | None -> [ i ])
  | Let (x, Prim (Extern "caml_sys_const_backend_type", [ _ ])) ->
      let jsoo = Code.Var.fresh () in
      let backend_name =
        match target with
        | `JavaScript -> "js_of_ocaml"
        | `Wasm -> "wasm_of_ocaml"
      in
      [ Let (jsoo, Constant (String backend_name))
      ; Let (x, Block (0, [| jsoo |], NotArray, Immutable))
      ]
  | Let (_, Prim (Extern ("%resume" | "%perform" | "%reperform"), _)) ->
      [ i ] (* We need that the arguments to this primitives remain variables *)
  | Let (x, Prim (prim, prim_args)) -> (
      let prim_args' = List.map prim_args ~f:(fun x -> the_const_of ~target info x) in
      let res =
        if
          List.for_all prim_args' ~f:(function
            | Some _ -> true
            | _ -> false)
        then
          eval_prim
            ( prim
            , List.map prim_args' ~f:(function
                | Some c -> c
                | None -> assert false) )
        else None
      in
      match res with
      | Some c ->
          let c = Constant c in
          Flow.Info.update_def info x c;
          [ Let (x, c) ]
      | _ ->
          [ Let
              ( x
              , Prim
                  ( prim
                  , List.map2 prim_args prim_args' ~f:(fun arg (c : constant option) ->
                        match c, target with
                        | Some (Int _ as c), _ -> Pc c
                        | Some (Int32 _ | NativeInt _ | NativeString _), `Wasm ->
                            (* Avoid duplicating the constant here as it would cause an
                               allocation *)
                            arg
                        | Some (Int32 _ | NativeInt _), `JavaScript -> assert false
                        | Some ((Float _ | NativeString _) as c), `JavaScript -> Pc c
                        | Some (String _ as c), `JavaScript
                          when Config.Flag.use_js_string () -> Pc c
                        | Some _, _
                        (* do not be duplicated other constant as
                            they're not represented with constant in javascript. *)
                        | None, _ -> arg) ) )
          ])
  | _ -> [ i ]

type cond_of =
  | Zero
  | Non_zero
  | Unknown

let the_cond_of info x =
  get_approx
    info
    (fun x ->
      match Flow.Info.def info x with
      | Some (Constant (Int x)) -> if Targetint.is_zero x then Zero else Non_zero
      | Some
          (Constant
             ( Int32 _
             | NativeInt _
             | Float _
             | Tuple _
             | String _
             | NativeString _
             | Float_array _
             | Int64 _ )) -> Non_zero
      | Some (Block (_, _, _, _)) -> Non_zero
      | Some (Field _ | Closure _ | Prim _ | Apply _ | Special _) -> Unknown
      | None -> Unknown)
    Unknown
    (fun u v ->
      match u, v with
      | Zero, Zero -> Zero
      | Non_zero, Non_zero -> Non_zero
      | _ -> Unknown)
    x

let eval_branch info l =
  match l with
  | Cond (x, ftrue, ffalse) as b -> (
      if Poly.(ftrue = ffalse)
      then Branch ftrue
      else
        match the_cond_of info x with
        | Zero -> Branch ffalse
        | Non_zero -> Branch ftrue
        | Unknown -> b)
  | Switch (x, a) as b -> (
      match the_cont_of info x a with
      | Some cont -> Branch cont
      | None -> b)
  | _ as b -> b

exception May_raise

let rec do_not_raise pc visited blocks =
  if Addr.Set.mem pc visited
  then visited
  else
    let visited = Addr.Set.add pc visited in
    let b = Addr.Map.find pc blocks in
    List.iter b.body ~f:(fun i ->
        match i with
        | Event _
        | Array_set (_, _, _)
        | Offset_ref (_, _)
        | Set_field (_, _, _, _)
        | Assign _ -> ()
        | Let (_, e) -> (
            match e with
            | Block (_, _, _, _) | Field (_, _, _) | Constant _ | Closure _ -> ()
            | Apply _ -> raise May_raise
            | Special _ -> ()
            | Prim (Extern name, _) when Primitive.is_pure name -> ()
            | Prim (Extern _, _) -> raise May_raise
            | Prim (_, _) -> ()));
    match b.branch with
    | Raise _ -> raise May_raise
    | Stop | Return _ | Poptrap _ -> visited
    | Branch (pc, _) -> do_not_raise pc visited blocks
    | Cond (_, (pc1, _), (pc2, _)) ->
        let visited = do_not_raise pc1 visited blocks in
        let visited = do_not_raise pc2 visited blocks in
        visited
    | Switch (_, a1) ->
        let visited =
          Array.fold_left a1 ~init:visited ~f:(fun visited (pc, _) ->
              do_not_raise pc visited blocks)
        in
        visited
    | Pushtrap _ -> raise May_raise

let drop_exception_handler blocks =
  Addr.Map.fold
    (fun pc _ blocks ->
      match Addr.Map.find pc blocks with
      | { branch = Pushtrap (((addr, _) as cont1), _x, _cont2); _ } as b -> (
          try
            let visited = do_not_raise addr Addr.Set.empty blocks in
            let b = { b with branch = Branch cont1 } in
            let blocks = Addr.Map.add pc b blocks in
            let blocks =
              Addr.Set.fold
                (fun pc2 blocks ->
                  let b = Addr.Map.find pc2 blocks in
                  let branch =
                    match b.branch with
                    | Poptrap cont -> Branch cont
                    | x -> x
                  in
                  let b = { b with branch } in
                  Addr.Map.add pc2 b blocks)
                visited
                blocks
            in
            blocks
          with May_raise -> blocks)
      | _ -> blocks)
    blocks
    blocks

let eval ~target info blocks =
  Addr.Map.map
    (fun block ->
      let body = List.concat_map block.body ~f:(eval_instr ~target info) in
      let branch = eval_branch info block.branch in
      { block with Code.body; Code.branch })
    blocks

let f info p =
  let blocks = eval ~target:(Config.target ()) info p.blocks in
  let blocks = drop_exception_handler blocks in
  { p with blocks }

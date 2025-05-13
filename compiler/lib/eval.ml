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

let times = Debug.find "times"

let stats = Debug.find "stats"

let debug_stats = Debug.find "stats-debug"

let static_env = String.Hashtbl.create 17

let clear_static_env () = String.Hashtbl.clear static_env

let set_static_env s value = String.Hashtbl.add static_env s value

let get_static_env s =
  try Some (String.Hashtbl.find static_env s) with Not_found -> None

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

let float f : constant = Float (Int64.bits_of_float f)

let float_binop_aux (l : constant list) (f : float -> float -> 'a) : 'a option =
  let args =
    match l with
    | [ Float i; Float j ] -> Some (Int64.float_of_bits i, Int64.float_of_bits j)
    | _ -> None
  in
  match args with
  | None -> None
  | Some (i, j) -> Some (f i j)

let float_binop (l : constant list) (f : float -> float -> float) : constant option =
  match float_binop_aux l f with
  | Some x -> Some (float x)
  | None -> None

let float_unop (l : constant list) (f : float -> float) : constant option =
  match l with
  | [ Float i ] -> Some (float (f (Int64.float_of_bits i)))
  | _ -> None

let bool' b = Int Targetint.(if b then one else zero)

let bool b = Some (bool' b)

let float_unop_bool (l : constant list) (f : float -> bool) =
  match l with
  | [ Float i ] -> bool (f (Int64.float_of_bits i))
  | _ -> None

let float_binop_bool l f =
  match float_binop_aux l f with
  | Some b -> bool b
  | None -> None

let int32 i = Some (Int32 i)

let int32_unop (l : constant list) (f : int32 -> int32) : constant option =
  match l with
  | [ Int32 i ] -> Some (Int32 (f i))
  | _ -> None

let int32_binop (l : constant list) (f : int32 -> int32 -> int32) : constant option =
  match l with
  | [ Int32 i; Int32 j ] -> Some (Int32 (f i j))
  | _ -> None

let int32_shiftop (l : constant list) (f : int32 -> int -> int32) : constant option =
  match l with
  | [ Int32 i; Int j ] -> Some (Int32 (f i (Targetint.to_int_exn j)))
  | _ -> None

let int64 i = Some (Int64 i)

let int64_unop (l : constant list) (f : int64 -> int64) : constant option =
  match l with
  | [ Int64 i ] -> Some (Int64 (f i))
  | _ -> None

let int64_binop (l : constant list) (f : int64 -> int64 -> int64) : constant option =
  match l with
  | [ Int64 i; Int64 j ] -> Some (Int64 (f i j))
  | _ -> None

let int64_shiftop (l : constant list) (f : int64 -> int -> int64) : constant option =
  match l with
  | [ Int64 i; Int j ] -> Some (Int64 (f i (Targetint.to_int_exn j)))
  | _ -> None

let nativeint i = Some (NativeInt i)

let nativeint_unop (l : constant list) (f : int32 -> int32) : constant option =
  match l with
  | [ NativeInt i ] -> Some (NativeInt (f i))
  | _ -> None

let nativeint_binop (l : constant list) (f : int32 -> int32 -> int32) : constant option =
  match l with
  | [ NativeInt i; NativeInt j ] -> Some (NativeInt (f i j))
  | _ -> None

let nativeint_shiftop (l : constant list) (f : int32 -> int -> int32) : constant option =
  match l with
  | [ NativeInt i; Int j ] -> Some (NativeInt (f i (Targetint.to_int_exn j)))
  | _ -> None

let eval_prim x =
  match x with
  | Not, [ Int i ] -> bool (Targetint.is_zero i)
  | Lt, [ Int i; Int j ] -> bool Targetint.(i < j)
  | Le, [ Int i; Int j ] -> bool Targetint.(i <= j)
  | Eq, [ Int i; Int j ] -> bool Targetint.(i = j)
  | Neq, [ Int i; Int j ] -> bool Targetint.(i <> j)
  | Ult, [ Int i; Int j ] -> bool (Targetint.unsigned_lt i j)
  | Extern name, l -> (
      match name, l with
      (* int *)
      | "%int_add", _ -> int_binop l Targetint.add
      | "%int_sub", _ -> int_binop l Targetint.sub
      | ("%int_mul" | "%direct_int_mul"), _ -> int_binop l Targetint.mul
      | "%direct_int_div", _ -> int_binop l Targetint.div
      | "%int_div", [ _; Int i ] when not (Targetint.is_zero i) ->
          int_binop l Targetint.div
      | "%direct_int_mod", _ -> int_binop l Targetint.rem
      | "%int_mod", [ _; Int i ] when not (Targetint.is_zero i) ->
          int_binop l Targetint.rem
      | "%int_and", _ -> int_binop l Targetint.logand
      | "%int_or", _ -> int_binop l Targetint.logor
      | "%int_xor", _ -> int_binop l Targetint.logxor
      | "%int_lsl", _ -> shift_op l Targetint.shift_left
      | "%int_lsr", _ -> shift_op l Targetint.shift_right_logical
      | "%int_asr", _ -> shift_op l Targetint.shift_right
      | "%int_neg", _ -> int_unop l Targetint.neg
      | "caml_int_compare", _ ->
          int_binop l Targetint.(fun i j -> of_int_exn (compare i j))
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
          match Targetint.of_float_opt (Int64.float_of_bits f) with
          | None -> None
          | Some f -> Some (Int f))
      | "caml_float_of_int", [ Int i ] -> Some (float (Targetint.to_float i))
      (* Math *)
      | "caml_neg_float", _ -> float_unop l ( ~-. )
      | "caml_abs_float", _ -> float_unop l abs_float
      | "caml_acos_float", _ -> float_unop l acos
      | "caml_asin_float", _ -> float_unop l asin
      | "caml_atan_float", _ -> float_unop l atan
      | "caml_atan2_float", _ -> float_binop l atan2
      | "caml_hypot_float", _ -> float_binop l hypot
      | "caml_ceil_float", _ -> float_unop l ceil
      | "caml_floor_float", _ -> float_unop l floor
      | "caml_trunc_float", _ -> float_unop l Float.trunc
      | "caml_round_float", _ -> float_unop l Float.round
      | "caml_cos_float", _ -> float_unop l cos
      | "caml_exp_float", _ -> float_unop l exp
      | "caml_exp2_float", _ -> float_unop l Float.exp2
      | "caml_expm1_float", _ -> float_unop l expm1
      | "caml_log_float", _ -> float_unop l log
      | "caml_log1p_float", _ -> float_unop l log1p
      | "caml_log2_float", _ -> float_unop l Float.log2
      | "caml_log10_float", _ -> float_unop l log10
      | "caml_cosh_float", _ -> float_unop l cosh
      | "caml_sinh_float", _ -> float_unop l sinh
      | "caml_tanh_float", _ -> float_unop l tanh
      | "caml_acosh_float", _ -> float_unop l Float.acosh
      | "caml_asinh_float", _ -> float_unop l Float.asinh
      | "caml_atanh_float", _ -> float_unop l Float.atanh
      | "caml_power_float", _ -> float_binop l ( ** )
      | "caml_sin_float", _ -> float_unop l sin
      | "caml_sqrt_float", _ -> float_unop l sqrt
      | "caml_cbrt_float", _ -> float_unop l Float.cbrt
      | "caml_tan_float", _ -> float_unop l tan
      | "caml_copysign_float", _ -> float_binop l copysign
      | "caml_signbit_float", _ -> float_unop_bool l Float.sign_bit
      | "caml_erf_float", _ -> float_unop l Float.erf
      | "caml_erfc_float", _ -> float_unop l Float.erfc
      | "caml_nextafter_float", _ -> float_binop l Float.next_after
      | "caml_float_compare", [ Float i; Float j ] ->
          Some
            (Int
               (Targetint.of_int_exn
                  (Float.compare (Int64.float_of_bits i) (Int64.float_of_bits j))))
      | "caml_ldexp_float", [ Float f; Int i ] ->
          Some (float (ldexp (Int64.float_of_bits f) (Targetint.to_int_exn i)))
      (* int32 *)
      | "caml_int32_bits_of_float", [ Float f ] ->
          int32 (Int32.bits_of_float (Int64.float_of_bits f))
      | "caml_int32_float_of_bits", [ Int32 i ] -> Some (float (Int32.float_of_bits i))
      | "caml_int32_of_float", [ Float f ] ->
          int32 (Int32.of_float (Int64.float_of_bits f))
      | "caml_int32_to_float", [ Int32 i ] -> Some (float (Int32.to_float i))
      | "caml_int32_neg", _ -> int32_unop l Int32.neg
      | "caml_int32_add", _ -> int32_binop l Int32.add
      | "caml_int32_sub", _ -> int32_binop l Int32.sub
      | "caml_int32_mul", _ -> int32_binop l Int32.mul
      | "caml_int32_and", _ -> int32_binop l Int32.logand
      | "caml_int32_or", _ -> int32_binop l Int32.logor
      | "caml_int32_xor", _ -> int32_binop l Int32.logxor
      | "caml_int32_div", [ _; Int32 i ] when not (Int32.equal i 0l) ->
          int32_binop l Int32.div
      | "caml_int32_mod", [ _; Int32 i ] when not (Int32.equal i 0l) ->
          int32_binop l Int32.rem
      | "caml_int32_shift_left", _ -> int32_shiftop l Int32.shift_left
      | "caml_int32_shift_right", _ -> int32_shiftop l Int32.shift_right
      | "caml_int32_shift_right_unsigned", _ -> int32_shiftop l Int32.shift_right_logical
      | "caml_int32_compare", [ Int32 i; Int32 j ] ->
          Some (Int (Targetint.of_int_exn (Int32.compare i j)))
      | "caml_int32_to_int", [ Int32 i ] -> Some (Int (Targetint.of_int32_truncate i))
      | "caml_int32_of_int", [ Int i ] -> int32 (Targetint.to_int32 i)
      | "caml_nativeint_of_int32", [ Int32 i ] -> Some (NativeInt i)
      | "caml_nativeint_to_int32", [ NativeInt i ] -> Some (Int32 i)
      (* nativeint *)
      | "caml_nativeint_bits_of_float", [ Float f ] ->
          nativeint (Int32.bits_of_float (Int64.float_of_bits f))
      | "caml_nativeint_float_of_bits", [ NativeInt i ] ->
          Some (float (Int32.float_of_bits i))
      | "caml_nativeint_of_float", [ Float f ] ->
          nativeint (Int32.of_float (Int64.float_of_bits f))
      | "caml_nativeint_to_float", [ NativeInt i ] -> Some (float (Int32.to_float i))
      | "caml_nativeint_neg", _ -> nativeint_unop l Int32.neg
      | "caml_nativeint_add", _ -> nativeint_binop l Int32.add
      | "caml_nativeint_sub", _ -> nativeint_binop l Int32.sub
      | "caml_nativeint_mul", _ -> nativeint_binop l Int32.mul
      | "caml_nativeint_and", _ -> nativeint_binop l Int32.logand
      | "caml_nativeint_or", _ -> nativeint_binop l Int32.logor
      | "caml_nativeint_xor", _ -> nativeint_binop l Int32.logxor
      | "caml_nativeint_div", [ _; NativeInt i ] when not (Int32.equal i 0l) ->
          nativeint_binop l Int32.div
      | "caml_nativeint_mod", [ _; NativeInt i ] when not (Int32.equal i 0l) ->
          nativeint_binop l Int32.rem
      | "caml_nativeint_shift_left", _ -> nativeint_shiftop l Int32.shift_left
      | "caml_nativeint_shift_right", _ -> nativeint_shiftop l Int32.shift_right
      | "caml_nativeint_shift_right_unsigned", _ ->
          nativeint_shiftop l Int32.shift_right_logical
      | "caml_nativeint_compare", [ NativeInt i; NativeInt j ] ->
          Some (Int (Targetint.of_int_exn (Int32.compare i j)))
      | "caml_nativeint_to_int", [ Int32 i ] -> Some (Int (Targetint.of_int32_truncate i))
      | "caml_nativeint_of_int", [ Int i ] -> nativeint (Targetint.to_int32 i)
      (* int64 *)
      | "caml_int64_bits_of_float", [ Float f ] -> int64 f
      | "caml_int64_float_of_bits", [ Int64 i ] -> Some (Float i)
      | "caml_int64_of_float", [ Float f ] ->
          int64 (Int64.of_float (Int64.float_of_bits f))
      | "caml_int64_to_float", [ Int64 i ] -> Some (float (Int64.to_float i))
      | "caml_int64_neg", _ -> int64_unop l Int64.neg
      | "caml_int64_add", _ -> int64_binop l Int64.add
      | "caml_int64_sub", _ -> int64_binop l Int64.sub
      | "caml_int64_mul", _ -> int64_binop l Int64.mul
      | "caml_int64_and", _ -> int64_binop l Int64.logand
      | "caml_int64_or", _ -> int64_binop l Int64.logor
      | "caml_int64_xor", _ -> int64_binop l Int64.logxor
      | "caml_int64_div", [ _; Int64 i ] when not (Int64.equal i 0L) ->
          int64_binop l Int64.div
      | "caml_int64_mod", [ _; Int64 i ] when not (Int64.equal i 0L) ->
          int64_binop l Int64.rem
      | "caml_int64_shift_left", _ -> int64_shiftop l Int64.shift_left
      | "caml_int64_shift_right", _ -> int64_shiftop l Int64.shift_right
      | "caml_int64_shift_right_unsigned", _ -> int64_shiftop l Int64.shift_right_logical
      | "caml_int64_compare", [ Int64 i; Int64 j ] ->
          Some (Int (Targetint.of_int_exn (Int64.compare i j)))
      | "caml_int64_to_int", [ Int64 i ] ->
          Some (Int (Targetint.of_int32_truncate (Int64.to_int32 i)))
      | "caml_int64_of_int", [ Int i ] -> int64 (Int64.of_int32 (Targetint.to_int32 i))
      | "caml_int64_to_int32", [ Int64 i ] -> int32 (Int64.to_int32 i)
      | "caml_int64_of_int32", [ Int32 i ] -> int64 (Int64.of_int32 i)
      | "caml_int64_to_nativeint", [ Int64 i ] -> nativeint (Int64.to_int32 i)
      | "caml_int64_of_nativeint", [ NativeInt i ] -> int64 (Int64.of_int32 i)
      (* others *)
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

let the_length_of info x =
  get_approx
    info
    (fun x ->
      match Flow.Info.def info x with
      | Some (Constant (String s)) -> Some (Targetint.of_int_exn (String.length s))
      | Some (Prim (Extern "caml_create_string", [ arg ]))
      | Some (Prim (Extern "caml_create_bytes", [ arg ])) -> the_int info arg
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
  | Pc _ -> N

let the_tag_of info x get equal =
  match x with
  | Pv x ->
      get_approx
        info
        (fun x ->
          match Flow.Info.def info x with
          | Some (Block (j, _, _, mut)) ->
              if Flow.Info.possibly_mutable info x
              then (
                assert (
                  match mut with
                  | Maybe_mutable -> true
                  | Immutable -> false);
                None)
              else get j
          | Some (Constant (Tuple (j, _, _))) -> get j
          | None | Some _ -> None)
        None
        (fun u v ->
          match u, v with
          | Some i, Some j when equal i j -> u
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
      | Some (Prim (Extern "%direct_obj_tag", [ b ])) -> the_tag_of info b get cont_equal
      | Some (Constant (Int j)) -> get (Targetint.to_int_exn j)
      | None | Some _ -> None)
    None
    (fun u v ->
      match u, v with
      | Some i, Some j when cont_equal i j -> u
      | _ -> None)
    x

let rec int_predicate deep info pred x (i : Targetint.t) =
  if deep > 2
  then None
  else
    (* The value of [x] might be meaningless when we're inside a dead code.
     The proper fix would be to remove the deadcode entirely.
     Meanwhile, add guards to prevent Invalid_argument("index out of bounds")
     see https://github.com/ocsigen/js_of_ocaml/issues/485 *)
    get_approx
      info
      (fun x ->
        match Flow.Info.def info x with
        | Some (Prim (Extern "%direct_obj_tag", [ b ])) ->
            the_tag_of info b (fun j -> Some (pred (Targetint.of_int_exn j) i)) Bool.equal
        | Some (Prim (Extern "%int_sub", [ Pv a; Pc (Int b) ])) ->
            int_predicate (deep + 1) info (fun x y -> pred (Targetint.sub x b) y) a i
        | Some (Prim (Extern "%int_add", [ Pv a; Pc (Int b) ])) ->
            int_predicate (deep + 1) info (fun x y -> pred (Targetint.add x b) y) a i
        | Some (Constant (Int j)) -> Some (pred j i)
        | None | Some _ -> None)
      None
      (fun u v ->
        match u, v with
        | Some i, Some j when Bool.equal i j -> u
        | _ -> None)
      x

(* If [constant_js_equal a b = Some v], then [caml_js_equals a b = v]). *)
let constant_js_equal a b =
  match a, b with
  | Int i, Int j -> Some (Targetint.equal i j)
  | Float a, Float b ->
      Some (Float.ieee_equal (Int64.float_of_bits a) (Int64.float_of_bits b))
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

(* [eval_prim] does not distinguish the two constants *)
let constant_equal a b =
  match a, b with
  | Int i, Int j -> Targetint.equal i j
  | Float a, Float b -> Int64.equal a b
  | NativeString a, NativeString b -> Native_string.equal a b
  | String a, String b -> String.equal a b
  | Int32 a, Int32 b -> Int32.equal a b
  | NativeInt a, NativeInt b -> Int32.equal a b
  | Int64 a, Int64 b -> Int64.equal a b
  (* We don't need to compare other constants, so let's just return false. *)
  | Tuple _, Tuple _ -> false
  | Float_array _, Float_array _ -> false
  | (Int _ | Float _ | Int64 _ | Int32 _ | NativeInt _), _ -> false
  | (String _ | NativeString _), _ -> false
  | (Float_array _ | Tuple _), _ -> false

let eval_instr update_count inline_constant ~target info i =
  match i with
  | Let (x, Prim (Extern (("caml_equal" | "caml_notequal") as prim), [ y; z ])) -> (
      let eq e1 e2 =
        match Code.Constant.ocaml_equal e1 e2 with
        | None -> false
        | Some e -> e
      in
      match the_const_of ~eq info y, the_const_of ~eq info z with
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
              incr update_count;
              [ Let (x, c) ])
      | _ -> [ i ])
  | Let (x, Prim (Extern ("caml_js_equals" | "caml_js_strict_equals"), [ y; z ])) -> (
      let eq e1 e2 =
        match constant_js_equal e1 e2 with
        | None -> false
        | Some e -> e
      in
      match the_const_of ~eq info y, the_const_of ~eq info z with
      | Some e1, Some e2 -> (
          match constant_js_equal e1 e2 with
          | None -> [ i ]
          | Some c ->
              let c = Constant (bool' c) in
              Flow.Info.update_def info x c;
              incr update_count;
              [ Let (x, c) ])
      | _ -> [ i ])
  | Let (x, Prim (Extern "caml_ml_string_length", [ s ])) -> (
      let c =
        match s with
        | Pc (String s) -> Some (Targetint.of_int_exn (String.length s))
        | Pv v -> the_length_of info v
        | _ -> None
      in
      match c with
      | None -> [ i ]
      | Some c ->
          let c = Constant (Int c) in
          Flow.Info.update_def info x c;
          incr update_count;
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
      | Y ->
          let c = Constant (bool' true) in
          Flow.Info.update_def info x c;
          [ Let (x, c) ]
      | N ->
          let c = Constant (bool' false) in
          Flow.Info.update_def info x c;
          incr update_count;
          [ Let (x, c) ])
  | Let
      ( x
      , Prim
          ( ((Eq | Neq | Lt | Le | Ult) as prim)
          , ([ (Pv y as fst); Pc (Int j) ] | [ (Pc (Int j) as fst); Pv y ]) ) ) -> (
      let pred =
        match prim with
        | Eq -> fun a b -> Targetint.equal a b
        | Neq -> fun a b -> not (Targetint.equal a b)
        | Lt -> fun a b -> Targetint.( < ) a b
        | Le -> fun a b -> Targetint.( <= ) a b
        | Ult -> fun a b -> Targetint.unsigned_lt a b
        | _ -> assert false
      in
      let pred =
        match fst with
        | Pv _ -> pred
        | Pc _ -> fun a b -> pred b a
      in
      match int_predicate 0 info pred y j with
      | Some b ->
          let c = Constant (bool' b) in
          Flow.Info.update_def info x c;
          incr update_count;
          [ Let (x, c) ]
      | None -> [ i ])
  | Let (x, Prim (Extern "%direct_obj_tag", [ y ])) -> (
      match the_tag_of info y (fun x -> Some x) ( = ) with
      | Some tag ->
          let c = Constant (Int (Targetint.of_int_exn tag)) in
          Flow.Info.update_def info x c;
          incr update_count;
          [ Let (x, c) ]
      | None -> [ i ])
  | Let (x, Prim (Extern "caml_sys_const_backend_type", [ _ ])) ->
      let jsoo = Code.Var.fresh () in
      let backend_name =
        match target with
        | `JavaScript -> "js_of_ocaml"
        | `Wasm -> "wasm_of_ocaml"
      in
      incr update_count;
      [ Let (jsoo, Constant (String backend_name))
      ; Let (x, Block (0, [| jsoo |], NotArray, Immutable))
      ]
  | Let (_, Prim (Extern ("%resume" | "%perform" | "%reperform"), _)) ->
      [ i ] (* We need that the arguments to this primitives remain variables *)
  | Let (x, Prim (prim, prim_args)) -> (
      let prim_args' =
        List.map prim_args ~f:(fun x -> the_const_of ~eq:constant_equal info x)
      in
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
          incr update_count;
          [ Let (x, c) ]
      | _ ->
          [ Let
              ( x
              , Prim
                  ( prim
                  , List.map2 prim_args prim_args' ~f:(fun arg (c : constant option) ->
                        match arg with
                        | Pc _ -> arg
                        | Pv _ -> (
                            match c, target with
                            | Some (Int _ as c), _ ->
                                incr inline_constant;
                                Pc c
                            | Some (Int32 _ | NativeInt _ | NativeString _), `Wasm ->
                                (* Avoid duplicating the constant here as it would cause an
                               allocation *)
                                arg
                            | Some ((Int32 _ | NativeInt _) as c), `JavaScript ->
                                incr inline_constant;
                                Pc c
                            | Some ((Float _ | NativeString _) as c), `JavaScript ->
                                incr inline_constant;
                                Pc c
                            | Some (String _ as c), `JavaScript
                              when Config.Flag.use_js_string () ->
                                incr inline_constant;
                                Pc c
                            | Some _, _
                            (* do not be duplicated other constant as
                            they're not represented with constant in javascript. *)
                            | None, _ -> arg)) ) )
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

let eval_branch update_branch info l =
  match l with
  | Cond (x, ftrue, ffalse) as b -> (
      match the_cond_of info x with
      | Zero ->
          incr update_branch;
          Branch ffalse
      | Non_zero ->
          incr update_branch;
          Branch ftrue
      | Unknown -> b)
  | Switch (x, a) as b -> (
      match the_cont_of info x a with
      | Some cont ->
          incr update_branch;
          Branch cont
      | None -> b)
  | _ as b -> b

exception May_raise

let rec do_not_raise pc visited rewrite blocks =
  if Addr.Set.mem pc visited
  then visited, rewrite
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
    | Stop | Return _ -> visited, rewrite
    | Poptrap _ -> visited, pc :: rewrite
    | Branch (pc, _) -> do_not_raise pc visited rewrite blocks
    | Cond (_, (pc1, _), (pc2, _)) ->
        let visited, rewrite = do_not_raise pc1 visited rewrite blocks in
        let visited, rewrite = do_not_raise pc2 visited rewrite blocks in
        visited, rewrite
    | Switch (_, a1) ->
        let visited, rewrite =
          Array.fold_left
            a1
            ~init:(visited, rewrite)
            ~f:(fun (visited, rewrite) (pc, _) -> do_not_raise pc visited rewrite blocks)
        in
        visited, rewrite
    | Pushtrap _ -> raise May_raise

let drop_exception_handler drop_count blocks =
  Addr.Map.fold
    (fun pc _ blocks ->
      match Addr.Map.find pc blocks with
      | { branch = Pushtrap (((addr, _) as cont1), _x, _cont2); _ } as b -> (
          match do_not_raise addr Addr.Set.empty [] blocks with
          | exception May_raise -> blocks
          | _visited, rewrite ->
              incr drop_count;
              let b = { b with branch = Branch cont1 } in
              let blocks = Addr.Map.add pc b blocks in
              let blocks =
                List.fold_left
                  ~f:(fun blocks pc2 ->
                    Addr.Map.update
                      pc2
                      (function
                        | Some ({ branch = Poptrap cont; _ } as b) ->
                            Some { b with branch = Branch cont }
                        | None | Some _ -> assert false)
                      blocks)
                  rewrite
                  ~init:blocks
              in
              blocks)
      | _ -> blocks)
    blocks
    blocks

let eval update_count update_branch inline_constant ~target info blocks =
  Addr.Map.map
    (fun block ->
      let body =
        List.concat_map
          block.body
          ~f:(eval_instr update_count inline_constant ~target info)
      in
      let branch = eval_branch update_branch info block.branch in
      { block with Code.body; Code.branch })
    blocks

let f info p =
  Code.invariant p;
  let previous_p = p in
  let update_count = ref 0 in
  let update_branch = ref 0 in
  let inline_constant = ref 0 in
  let drop_count = ref 0 in
  let t = Timer.make () in
  let blocks =
    eval
      update_count
      update_branch
      inline_constant
      ~target:(Config.target ())
      info
      p.blocks
  in
  let blocks = drop_exception_handler drop_count blocks in
  let p = { p with blocks } in
  if times () then Format.eprintf "  eval: %a@." Timer.print t;
  if stats ()
  then
    Format.eprintf
      "Stats - eval: %d optimizations, %d inlined cst, %d dropped exception handlers, %d \
       branch updated@."
      !update_count
      !inline_constant
      !drop_count
      !update_branch;
  if debug_stats ()
  then
    Code.check_updates
      ~name:"eval"
      previous_p
      p
      ~updates:(!update_count + !inline_constant + !drop_count + !update_branch);
  let p = Deadcode.remove_unused_blocks p in
  Code.invariant p;
  p

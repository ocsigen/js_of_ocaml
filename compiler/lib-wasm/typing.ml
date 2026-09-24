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
open Global_flow

let debug = Debug.find "typing"

let times = Debug.find "times"

let can_unbox_parameters fun_info f =
  (* We can unbox the parameters of a function when all its call sites
     are known, and only this function is called there. It would be
     more robust to deal with more cases by using an intermediate
     function that unbox the parameters. When several functions can be
     call from the same call site, one could enforce somehow that they
     have the same signature. *)
  Call_graph_analysis.direct_calls_only fun_info f

let can_unbox_return_value fun_info f =
  (* Unboxing return values can unoptimize a tail call. Since we are
     never unboxing then reboxing a value, this can only happen once
     in a sequence of tail calls, so this is not an issue. *)
  Call_graph_analysis.direct_calls_only fun_info f

module Integer = struct
  type kind =
    | Ref
    | Large_normalized
    | Large_unnormalized
    | Small_normalized
    | Small_unnormalized

  let max_i31s = lazy (Targetint.to_int64 (Lazy.force Targetint.max_i31s))

  let min_i31s = lazy (Targetint.to_int64 (Lazy.force Targetint.min_i31s))

  let join r r' =
    match r, r' with
    | Ref, Ref -> Ref
    | Ref, (Small_normalized | Small_unnormalized)
    | (Small_normalized | Small_unnormalized), Ref
      when Config.Flag.portable_int () -> Large_normalized
    | Large_unnormalized, _ | _, Large_unnormalized -> Large_unnormalized
    | Large_normalized, _ | _, Large_normalized -> Large_normalized
    | Small_unnormalized, _ | _, Small_unnormalized -> Small_unnormalized
    | Small_normalized, _ | _, Small_normalized -> Small_normalized

  let kind_of_targetint i =
    if Config.Flag.portable_int () && not (Targetint.is_within_i31s i)
    then Large_normalized
    else Small_normalized
end

type boxed_number =
  | Int32
  | Int64
  | Nativeint
  | Float
  | Float32

type boxed_status =
  | Boxed
  | Unboxed

module Bigarray = struct
  let make ~kind ~layout : Optimization_hint.Bigarray.t =
    { unsafe = false
    ; kind =
        (match kind with
        | 0 -> Float32
        | 1 -> Float64
        | 2 -> Int8_signed
        | 3 -> Int8_unsigned
        | 4 -> Int16_signed
        | 5 -> Int16_unsigned
        | 6 -> Int32
        | 7 -> Int64
        | 8 -> Int
        | 9 -> Nativeint
        | 10 -> Complex32
        | 11 -> Complex64
        | 12 -> Int8_unsigned
        | 13 -> Float16
        | _ -> assert false)
    ; layout =
        (match layout with
        | 0 -> C
        | 1 -> Fortran
        | _ -> assert false)
    }

  let print f { Optimization_hint.Bigarray.kind; layout; _ } =
    Format.fprintf
      f
      "bigarray{%s,%s}"
      (match kind with
      | Float32 -> "float32"
      | Float32_t -> "float32_t"
      | Float64 -> "float64"
      | Int8_signed -> "sint8"
      | Int8_unsigned -> "uint8"
      | Int16_signed -> "sint16"
      | Int16_unsigned -> "uint16"
      | Int32 -> "int32"
      | Int64 -> "int64"
      | Int -> "int"
      | Nativeint -> "nativeint"
      | Complex32 -> "complex32"
      | Complex64 -> "complex64"
      | Float16 -> "float16")
      (match layout with
      | C -> "C"
      | Fortran -> "Fortran")

  let equal
      { Optimization_hint.Bigarray.unsafe; kind; layout }
      { Optimization_hint.Bigarray.unsafe = unsafe'; kind = kind'; layout = layout' } =
    Bool.equal unsafe unsafe' && phys_equal kind kind' && phys_equal layout layout'
end

type typ =
  | Top
  | Int of Integer.kind
  | Number of boxed_number * boxed_status
  | Tuple of typ array
      (** This value is a block or an integer; if it's an integer, an
          overapproximation of the possible values of each of its
          fields is given by the array of types *)
  | Bigarray of Optimization_hint.Bigarray.t
  | Null
  | Bot

module Domain = struct
  type t = typ

  let rec join t t' =
    match t, t' with
    | Bot, t | t, Bot -> t
    | Int r, Int r' -> Int (Integer.join r r')
    | Number (n, b), Number (n', b') ->
        if Poly.equal n n'
        then
          Number
            ( n
            , match b, b' with
              | Unboxed, _ | _, Unboxed -> Unboxed
              | Boxed, Boxed -> Boxed )
        else Top
    | Tuple t, Tuple t' ->
        let l = Array.length t in
        let l' = Array.length t' in
        Tuple
          (if l = l'
           then Array.map2 ~f:join t t'
           else
             Array.init (max l l') ~f:(fun i ->
                 if i < l then if i < l' then join t.(i) t'.(i) else t.(i) else t'.(i)))
    | (Int _ | Null), Tuple _ -> t'
    | Tuple _, (Int _ | Null) -> t
    | Bigarray b, Bigarray b' when Bigarray.equal b b' -> t
    | Null, Null -> Null
    | Top, _ | _, Top -> Top
    | (Int _ | Number _ | Tuple _ | Bigarray _ | Null), _ -> Top

  let join_set ?(others = false) f s =
    if others then Top else Var.Set.fold (fun x a -> join (f x) a) s Bot

  let rec equal t t' =
    match t, t' with
    | Top, Top | Bot, Bot -> true
    | Int t, Int t' -> Poly.equal t t'
    | Number (t, b), Number (t', b') -> Poly.equal t t' && Poly.equal b b'
    | Tuple t, Tuple t' ->
        Array.length t = Array.length t' && Array.for_all2 ~f:equal t t'
    | Bigarray b, Bigarray b' -> Bigarray.equal b b'
    | Null, Null -> true
    | (Top | Tuple _ | Int _ | Number _ | Bigarray _ | Null | Bot), _ -> false

  let bot = Bot

  let depth_threshold = 4

  let rec depth t =
    match t with
    | Top | Bot | Number _ | Int _ | Bigarray _ | Null -> 0
    | Tuple l -> 1 + Array.fold_left ~f:(fun acc t' -> max (depth t') acc) l ~init:0

  let rec truncate depth t =
    match t with
    | Top | Bot | Number _ | Int _ | Bigarray _ | Null -> t
    | Tuple l ->
        if depth = 0
        then Top
        else Tuple (Array.map ~f:(fun t' -> truncate (depth - 1) t') l)

  let limit t = if depth t > depth_threshold then truncate depth_threshold t else t

  let box t =
    match t with
    | Int _ -> Int Ref
    | Number (n, _) -> Number (n, Boxed)
    | _ -> t

  let rec print f t =
    match t with
    | Top -> Format.fprintf f "top"
    | Bot -> Format.fprintf f "bot"
    | Int k ->
        Format.fprintf
          f
          "int{%s}"
          (match k with
          | Ref -> "ref"
          | Large_normalized -> "large_normalized"
          | Large_unnormalized -> "large_unnormalized"
          | Small_normalized -> "small_normalized"
          | Small_unnormalized -> "small_unnormalized")
    | Number (n, b) ->
        Format.fprintf
          f
          "%s{%s}"
          (match n with
          | Int32 -> "int32"
          | Int64 -> "int64"
          | Nativeint -> "nativeint"
          | Float -> "float"
          | Float32 -> "float32")
          (match b with
          | Boxed -> "boxed"
          | Unboxed -> "unboxed")
    | Bigarray b -> Bigarray.print f b
    | Null -> Format.fprintf f "null"
    | Tuple t ->
        Format.fprintf
          f
          "(%a)"
          (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ",") print)
          (Array.to_list t)
end

let update_deps st { blocks; _ } =
  let add_dep st x y = Var.Tbl.set st.deps y (x :: Var.Tbl.get st.deps y) in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, Block (_, lst, _, _)) -> Array.iter ~f:(fun y -> add_dep st x y) lst
          | Let
              ( x
              , Prim
                  ( Extern
                      ( ( "%int_and"
                        | "%int_or"
                        | "%int_xor"
                        | "%int_add"
                        | "%int_sub"
                        | "%int_mul"
                        | "%direct_int_mul"
                        | "%int_neg"
                        | "%int_div"
                        | "%direct_int_div"
                        | "caml_ba_get_1"
                        | "caml_ba_get_2"
                        | "caml_ba_get_3"
                        | "caml_ba_get_generic" )
                      , _ )
                  , lst ) ) ->
              (* The return type of these primitives depend on the input type *)
              List.iter
                ~f:(fun p ->
                  match p with
                  | Pc _ -> ()
                  | Pv y -> add_dep st x y)
                lst
          | _ -> ()))
    blocks

let mark_boxed_function_parameters ~fun_info { blocks; _ } =
  let boxed_function_parameters = Var.ISet.empty () in
  let set x = Var.ISet.add boxed_function_parameters x in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (x, Closure (params, _, _)) when not (can_unbox_parameters fun_info x) ->
              List.iter ~f:set params
          | _ -> ()))
    blocks;
  boxed_function_parameters

let repr_to_type (r : Optimization_hint.repr) : typ =
  match r with
  | Value -> Top
  | Float -> Number (Float, Boxed)
  | Int32 -> Number (Int32, Boxed)
  | Nativeint -> Number (Nativeint, Boxed)
  | Int64 -> Number (Int64, Boxed)
  | Int -> Int Ref

let collect_parameter_type_hints { blocks; _ } =
  let h = Var.Hashtbl.create 16 in
  Addr.Map.iter
    (fun _ block ->
      List.iter block.body ~f:(fun i ->
          match i with
          | Let (_, Closure (params, _, (Some { params = param_reprs; _ }, _))) ->
              List.iter2
                ~f:(fun p r ->
                  match repr_to_type r with
                  | Top -> ()
                  | t -> Var.Hashtbl.replace h p t)
                params
                param_reprs
          | _ -> ()))
    blocks;
  h

type st =
  { global_flow_state : Global_flow.state
  ; global_flow_info : Global_flow.info
  ; boxed_function_parameters : Var.ISet.t
  ; parameter_type_hints : typ Var.Hashtbl.t
  ; fun_info : Call_graph_analysis.t
  }

let rec constant_type (c : constant) =
  match c with
  | Int i -> Int (Integer.kind_of_targetint i)
  | Int32 _ -> Number (Int32, Unboxed)
  | Int64 _ -> Number (Int64, Unboxed)
  | NativeInt _ -> Number (Nativeint, Unboxed)
  | Float _ -> Number (Float, Unboxed)
  | Float32 _ -> Number (Float32, Unboxed)
  | Tuple (_, a, _) -> Tuple (Array.map ~f:(fun c' -> Domain.box (constant_type c')) a)
  | Null_ -> Null
  | _ -> Top

(* The wasm conversion primitive needed to coerce a value of type [from]
   into representation [into], or [None] if the representations already
   match or no conversion applies. This is the single source of truth for
   the box/unbox/tag/untag representation lattice; [Lcm] is built on top of
   it rather than re-deriving the same case analysis. *)
let conversion_prim ~(from : typ) ~(into : typ) : prim option =
  match from, into with
  | Number (Int32, Unboxed), Number (Int32, Unboxed)
  | Number (Int64, Unboxed), Number (Int64, Unboxed)
  | Number (Float, Unboxed), Number (Float, Unboxed)
  | ( Int (Small_normalized | Small_unnormalized | Large_normalized | Large_unnormalized)
    , Int (Small_normalized | Small_unnormalized | Large_normalized | Large_unnormalized)
    ) -> None
  | _, Int (Small_normalized | Small_unnormalized | Large_normalized | Large_unnormalized)
    when Config.Flag.portable_int () ->
      (* Untagged to a 64-bit integer, then implicitly truncated if needed, so
         that the untagging is shared by all uses *)
      Some Wasm_untag_large_int
  | _, Int (Small_normalized | Small_unnormalized) -> Some Wasm_untag_int
  | Int (Small_normalized | Small_unnormalized), (Int Ref | Top) -> Some Wasm_tag_int
  | Int (Large_normalized | Large_unnormalized), (Int Ref | Top)
    when Config.Flag.portable_int () -> Some Wasm_tag_large_int
  | Int _, _ | _, Int _ -> None
  | Number (_, Unboxed), Number (_, Unboxed) -> None
  | _, Number (Int32, Unboxed) -> Some Wasm_unbox_i32
  | _, Number (Int64, Unboxed) -> Some Wasm_unbox_i64
  | _, Number (Float, Unboxed) -> Some Wasm_unbox_f64
  | Number (Int32, Unboxed), _ -> Some Wasm_box_i32
  | Number (Int64, Unboxed), _ -> Some Wasm_box_i64
  | Number (Float, Unboxed), _ -> Some Wasm_box_f64
  | _ -> None

let arg_type ~approx arg =
  match arg with
  | Pc c -> constant_type c
  | Pv x -> Var.Tbl.get approx x

let bigarray_element_type (kind : Optimization_hint.Bigarray.kind) =
  match kind with
  | Float16 | Float32 | Float64 -> Number (Float, Unboxed)
  | Float32_t -> Number (Float32, Unboxed)
  | Int8_signed | Int8_unsigned | Int16_signed | Int16_unsigned -> Int Small_normalized
  | Int ->
      if Config.Flag.portable_int ()
      then Int Large_unnormalized
      else Int Small_unnormalized
  | Int32 -> Number (Int32, Unboxed)
  | Int64 -> Number (Int64, Unboxed)
  | Nativeint -> Number (Nativeint, Unboxed)
  | Complex32 | Complex64 -> Tuple [| Number (Float, Boxed); Number (Float, Boxed) |]

let bigarray_type ~approx ba =
  match arg_type ~approx ba with
  | Bot -> Bot
  | Bigarray { kind; _ } -> bigarray_element_type kind
  | _ -> Top

let primitive_types = String.Hashtbl.create 16

let prim_type ~st ~approx prim hint args =
  match prim with
  | ("%int_and" | "%int_or" | "%int_xor") when Config.Flag.portable_int () -> (
      (* Bitwise operations preserve sign-extension, from 31 or from 63 bits *)
      let small t =
        match t with
        | Bot | Int Small_normalized -> true
        | _ -> false
      in
      let normalized t =
        match t with
        | Bot | Int (Ref | Small_normalized | Large_normalized) -> true
        | _ -> false
      in
      (* [x land c] is in [\[0, c\]] when [c] is non-negative *)
      let small_non_negative_constant p =
        match
          match p with
          | Pc c -> Some c
          | Pv y -> (
              match st.global_flow_state.defs.(Var.idx y) with
              | Expr (Constant c) -> Some c
              | Phi _ | Expr _ -> None)
        with
        | Some (Int c) -> Targetint.(c >= zero) && Targetint.is_within_i31s c
        | _ -> false
      in
      match args, List.map ~f:(fun x -> arg_type ~approx x) args with
      | [ x; y ], [ t; t' ] ->
          if
            (small t && small t')
            || String.equal prim "%int_and"
               && (small_non_negative_constant x || small_non_negative_constant y)
          then Int Small_normalized
          else if normalized t && normalized t'
          then Int Large_normalized
          else Int Large_unnormalized
      | _ -> Int Large_unnormalized)
  | "%int_add"
  | "%int_sub"
  | "%int_mul"
  | "%direct_int_mul"
  | "%int_neg"
  | "%int_div"
  | "%direct_int_div"
    when Config.Flag.portable_int () ->
      (* These operations cannot overflow 63 bits when applied to 31-bit
         integers. For a division, it is enough for the dividend to be a
         31-bit integer. *)
      let small t =
        match t with
        | Bot | Int (Small_normalized | Small_unnormalized) -> true
        | _ -> false
      in
      let types = List.map ~f:(fun x -> arg_type ~approx x) args in
      if
        match prim, types with
        | ("%int_div" | "%direct_int_div"), t :: _ -> small t
        | _ -> List.for_all ~f:small types
      then Int Large_normalized
      else Int Large_unnormalized
  | "%int_and" -> (
      match List.map ~f:(fun x -> arg_type ~approx x) args with
      | [ (Bot | Int (Ref | Small_normalized)); _ ]
      | [ _; (Bot | Int (Ref | Small_normalized)) ] -> Int Small_normalized
      | _ -> Int Small_unnormalized)
  | "%int_or" | "%int_xor" -> (
      match List.map ~f:(fun x -> arg_type ~approx x) args with
      | [ (Bot | Int (Ref | Small_normalized)); (Bot | Int (Ref | Small_normalized)) ] ->
          Int Small_normalized
      | _ -> Int Small_unnormalized)
  | "caml_ba_create" -> (
      match args with
      | [ Pc (Int kind); Pc (Int layout); _ ] ->
          Bigarray
            (Bigarray.make
               ~kind:(Targetint.to_int_exn kind)
               ~layout:(Targetint.to_int_exn layout))
      | _ -> Top)
  | "caml_ba_get_1" | "caml_ba_get_2" | "caml_ba_get_3" -> (
      match hint, args with
      | Some (Optimization_hint.Hint_bigarray { kind; _ }), _ ->
          bigarray_element_type kind
      | _, ba :: _ -> bigarray_type ~approx ba
      | _, [] -> Top)
  | "caml_ba_get_generic" -> (
      match args with
      | ba :: Pv indices :: _ -> (
          match st.global_flow_state.defs.(Var.idx indices) with
          | Expr (Block _) -> bigarray_type ~approx ba
          | _ -> Top)
      | [] | [ _ ] | _ :: Pc _ :: _ -> Top)
  | _ -> (
      match String.Hashtbl.find_opt primitive_types prim with
      | Some (_, _, typ) -> typ
      | None -> Top)

let reset () = String.Hashtbl.reset primitive_types

let register_prim nm ?args ~unbox typ =
  String.Hashtbl.replace primitive_types nm (args, unbox, typ)

let prim_sig nm =
  match String.Hashtbl.find_opt primitive_types nm with
  | Some (args, _, typ) -> args, typ
  | None -> None, Top

let propagate st approx x : Domain.t =
  match st.global_flow_state.defs.(Var.idx x) with
  | Phi { known; others; unit } -> (
      let res = Domain.join_set ~others (fun y -> Var.Tbl.get approx y) known in
      let res = if unit then Domain.join (Int Small_normalized) res else res in
      let res =
        if Var.ISet.mem st.boxed_function_parameters x then Domain.box res else res
      in
      match res with
      | Top -> (
          match Var.Hashtbl.find_opt st.parameter_type_hints x with
          | Some t -> t
          | None -> Top)
      | _ -> res)
  | Expr e -> (
      match e with
      | Constant c -> constant_type c
      | Closure _ -> Top
      | Block (_, lst, _, _) ->
          Tuple
            (Array.mapi
               ~f:(fun i y ->
                 match st.global_flow_state.mutable_fields.(Var.idx x) with
                 | All_fields -> Top
                 | Some_fields s when IntSet.mem i s -> Top
                 | Some_fields _ | No_field ->
                     Domain.limit (Domain.box (Var.Tbl.get approx y)))
               lst)
      | Field (_, _, Float) -> Number (Float, Unboxed)
      | Field (y, n, Non_float) -> (
          match Var.Tbl.get approx y with
          | Tuple t -> if n < Array.length t then t.(n) else Bot
          | Top -> Top
          | _ -> Bot)
      | Prim
          ( Extern
              (("caml_check_bound" | "caml_check_bound_float" | "caml_check_bound_gen"), _)
          , [ Pv y; _ ] ) -> Var.Tbl.get approx y
      | Prim ((Array_get | Extern ("caml_array_unsafe_get", _)), [ Pv y; _ ]) -> (
          match Var.Tbl.get st.global_flow_info.info_approximation y with
          | Values { known; others } ->
              Domain.join_set
                ~others
                (fun z ->
                  match st.global_flow_state.defs.(Var.idx z) with
                  | Expr (Block (_, lst, _, _)) ->
                      let m =
                        match st.global_flow_state.mutable_fields.(Var.idx z) with
                        | No_field -> false
                        | Some_fields _ | All_fields -> true
                      in
                      if m
                      then Top
                      else
                        Domain.box
                          (Array.fold_left
                             ~f:(fun acc t -> Domain.join (Var.Tbl.get approx t) acc)
                             ~init:Domain.bot
                             lst)
                  | Expr (Closure _) -> Bot
                  | Phi _ | Expr _ -> assert false)
                known
          | Top -> Top)
      | Prim (Array_get, _) -> Top
      | Prim ((Vectlength _ | Not | IsInt | Eq | Neq | Lt | Le | Ult | Wasm_untag_int), _)
        -> Int Small_normalized
      | Prim (Wasm_untag_large_int, _) -> Int Large_normalized
      | Prim ((Wasm_tag_int | Wasm_tag_large_int), _) -> Int Ref
      | Prim (Wasm_unbox_i32, _) -> Number (Int32, Unboxed)
      | Prim (Wasm_unbox_i64, _) -> Number (Int64, Unboxed)
      | Prim (Wasm_unbox_f64, _) -> Number (Float, Unboxed)
      | Prim (Wasm_box_i32, _) -> Number (Int32, Boxed)
      | Prim (Wasm_box_i64, _) -> Number (Int64, Boxed)
      | Prim (Wasm_box_f64, _) -> Number (Float, Boxed)
      | Prim (Extern (prim, hint), args) -> prim_type ~st ~approx prim hint args
      | Special _ -> Top
      | Apply { f; args; _ } -> (
          match Var.Tbl.get st.global_flow_info.info_approximation f with
          | Values { known; others } ->
              Domain.join_set
                ~others
                (fun g ->
                  match st.global_flow_state.defs.(Var.idx g) with
                  | Expr (Closure (params, _, _))
                    when List.length args = List.length params ->
                      let res =
                        Domain.join_set
                          (fun y ->
                            match st.global_flow_state.defs.(Var.idx y) with
                            | Expr
                                (Prim
                                   ( Extern ("caml_ba_create", _)
                                   , [ Pv kind; Pv layout; _ ] )) -> (
                                let m =
                                  List.fold_left2
                                    ~f:(fun m p a -> Var.Map.add p a m)
                                    ~init:Var.Map.empty
                                    params
                                    args
                                in
                                try
                                  match
                                    ( st.global_flow_state.defs.(Var.idx
                                                                   (Var.Map.find kind m))
                                    , st.global_flow_state.defs.(Var.idx
                                                                   (Var.Map.find layout m))
                                    )
                                  with
                                  | ( Expr (Constant (Int kind))
                                    , Expr (Constant (Int layout)) ) ->
                                      Bigarray
                                        (Bigarray.make
                                           ~kind:(Targetint.to_int_exn kind)
                                           ~layout:(Targetint.to_int_exn layout))
                                  | _ -> raise Not_found
                                with Not_found -> Var.Tbl.get approx y)
                            | _ -> Var.Tbl.get approx y)
                          (Var.Map.find g st.global_flow_state.return_values)
                      in
                      if can_unbox_return_value st.fun_info g then res else Domain.box res
                  | Expr (Closure (_, _, _)) ->
                      (* The function is partially applied or over applied *)
                      Top
                  | Expr (Block _) -> Bot
                  | Phi _ | Expr _ -> assert false)
                known
          | Top -> Top))

module G = Dgraph.Make_Imperative (Var) (Var.ISet) (Var.Tbl)
module Solver = G.Solver (Domain)

let solver st =
  let associated_list h x = Var.Hashtbl.find_opt h x |> Option.value ~default:[] in
  let g =
    { G.domain = st.global_flow_state.vars
    ; G.iter_children =
        (fun f x ->
          List.iter ~f (Var.Tbl.get st.global_flow_state.deps x);
          List.iter
            ~f:(fun g ->
              List.iter ~f (associated_list st.global_flow_state.function_call_sites g))
            (associated_list st.global_flow_state.functions_from_returned_value x))
    }
  in
  Solver.f () g (propagate st)

let type_specialized_primitive types global_flow_state name args =
  match name with
  | "caml_greaterthan"
  | "caml_greaterequal"
  | "caml_lessthan"
  | "caml_lessequal"
  | "caml_equal"
  | "caml_notequal"
  | "caml_compare" -> (
      match List.map ~f:(arg_type ~approx:types) args with
      | [ Int _; Int _ ]
      | [ Number (Int32, _); Number (Int32, _) ]
      | [ Number (Int64, _); Number (Int64, _) ]
      | [ Number (Nativeint, _); Number (Nativeint, _) ]
      | [ Number (Float, _); Number (Float, _) ]
      | [ Number (Float32, _); Number (Float32, _) ] -> true
      | _ -> false)
  | "caml_ba_get_1"
  | "caml_ba_get_2"
  | "caml_ba_get_3"
  | "caml_ba_set_1"
  | "caml_ba_set_2"
  | "caml_ba_set_3" -> (
      match args with
      | Pv x :: _ -> (
          match Var.Tbl.get types x with
          | Bigarray _ -> true
          | _ -> false)
      | _ -> false)
  | "caml_ba_get_generic" | "caml_ba_set_generic" -> (
      match args with
      | Pv x :: Pv indices :: _ -> (
          match Var.Tbl.get types x, global_flow_state.defs.(Var.idx indices) with
          | Bigarray _, Expr (Block _) -> true
          | _ -> false)
      | _ -> false)
  | _ -> false

(* Without the LCM pass ([lazy_boxing] false), we box numbers eagerly if
   the boxed value is ever used: the variable gets a boxed type, and so do
   the values it is computed from (the inputs of a phi, the values returned
   by the function called).

   With the LCM pass ([lazy_boxing] true), numbers are boxed where the boxed
   value is used, and [Lcm] places these conversions. We only record which
   variables are used boxed. Constants are still boxed eagerly, since this
   is free. A phi or a call result keeps a boxed type if it
   is used boxed and one of the values it is computed from is already
   boxed, so as not to unbox a boxed value only to box it again; in the
   case of a call, the function then returns a boxed value. The set of such
   functions is returned. *)
let box_numbers ~lazy_boxing p st types =
  let should_box = Var.ISet.empty () in
  let boxed_uses = Var.ISet.empty () in
  let call_results = Var.Hashtbl.create 16 in
  let rec box y =
    if lazy_boxing
    then Var.ISet.add boxed_uses y
    else if not (Var.ISet.mem should_box y)
    then (
      Var.ISet.add should_box y;
      let typ = Var.Tbl.get types y in
      (match typ with
      | Number (n, Unboxed) -> Var.Tbl.set types y (Number (n, Boxed))
      | _ -> ());
      match typ with
      | Number (_, Unboxed) | Top -> (
          match st.global_flow_state.defs.(Var.idx y) with
          | Expr (Apply { f; _ }) -> (
              match Global_flow.get_unique_closure st.global_flow_info f with
              | None -> ()
              | Some (g, _) ->
                  if can_unbox_return_value st.fun_info g
                  then
                    let s = Var.Map.find g st.global_flow_info.info_return_vals in
                    Var.Set.iter box s)
          | Expr _ -> ()
          | Phi { known; _ } -> Var.Set.iter box known)
      | Number (_, Boxed) | Int _ | Tuple _ | Bigarray _ | Null | Bot -> ())
  in
  (* An argument passed to a block parameter which is not unboxed is used
     boxed *)
  let check_cont (pc', args) =
    let b' = Addr.Map.find pc' p.blocks in
    List.iter2
      ~f:(fun param arg ->
        match Var.Tbl.get types param with
        | Number (_, Boxed) | Top -> box arg
        | Number (_, Unboxed) | Int _ | Tuple _ | Bigarray _ | Null | Bot -> ())
      b'.params
      args
  in
  Code.fold_closures
    p
    (fun name_opt _ (pc, _) _ () ->
      traverse
        { fold = Code.fold_children }
        (fun pc () ->
          let b = Addr.Map.find pc p.blocks in
          List.iter
            ~f:(fun i ->
              match i with
              | Let (_, e) -> (
                  match e with
                  | Apply { f; args; _ } ->
                      (match Global_flow.get_unique_closure st.global_flow_info f with
                      | Some (g, _) when can_unbox_return_value st.fun_info g ->
                          (* [x] is the result of the call *)
                          let x =
                            match i with
                            | Let (x, _) -> x
                            | _ -> assert false
                          in
                          Var.Hashtbl.replace
                            call_results
                            g
                            (x
                            :: (Var.Hashtbl.find_opt call_results g
                               |> Option.value ~default:[]))
                      | Some _ | None -> ());
                      if
                        match Global_flow.get_unique_closure st.global_flow_info f with
                        | None -> true
                        | Some (g, _) -> not (can_unbox_parameters st.fun_info g)
                      then List.iter ~f:box args
                  | Block (tag, lst, _, _) -> if tag <> 254 then Array.iter ~f:box lst
                  | Prim (Extern (s, _), args) ->
                      if
                        not
                          ((String.Hashtbl.mem primitive_types s
                           &&
                           let _, unbox, _ = String.Hashtbl.find primitive_types s in
                           unbox)
                          || type_specialized_primitive types st.global_flow_state s args
                          )
                      then
                        List.iter
                          ~f:(fun a ->
                            match a with
                            | Pv y -> box y
                            | Pc _ -> ())
                          args
                  | Prim ((Eq | Neq), args) ->
                      List.iter
                        ~f:(fun a ->
                          match a with
                          | Pv y -> box y
                          | Pc _ -> ())
                        args
                  | Prim
                      ( ( Wasm_unbox_i32
                        | Wasm_unbox_i64
                        | Wasm_unbox_f64
                        | Wasm_untag_int
                        | Wasm_untag_large_int )
                      , args ) ->
                      List.iter
                        ~f:(fun a ->
                          match a with
                          | Pv y -> box y
                          | Pc _ -> ())
                        args
                  | Prim
                      ( ( Vectlength _
                        | Array_get
                        | Not
                        | IsInt
                        | Lt
                        | Le
                        | Ult
                        | Wasm_box_i32
                        | Wasm_box_i64
                        | Wasm_box_f64
                        | Wasm_tag_int
                        | Wasm_tag_large_int )
                      , _ )
                  | Field _ | Closure _ | Constant _ | Special _ -> ())
              | Set_field (_, _, Non_float, y) | Array_set (_, _, y) -> box y
              | Assign _ | Offset_ref _ | Set_field (_, _, Float, _) | Event _ -> ())
            b.body;
          match b.branch with
          | Return y ->
              Option.iter
                ~f:(fun g -> if not (can_unbox_return_value st.fun_info g) then box y)
                name_opt
          | Branch cont | Poptrap cont -> if lazy_boxing then check_cont cont
          | Cond (_, cont1, cont2) | Pushtrap (cont1, _, cont2) ->
              if lazy_boxing
              then (
                check_cont cont1;
                check_cont cont2)
          | Switch (_, conts) -> if lazy_boxing then Array.iter ~f:check_cont conts
          | Raise _ | Stop -> ())
        pc
        p.blocks
        ())
    ();
  let boxed_returns = Var.ISet.empty () in
  if lazy_boxing
  then (
    let is_boxed x =
      match Var.Tbl.get types x with
      | Number (_, Boxed) -> true
      | Number (_, Unboxed) | Top | Int _ | Tuple _ | Bigarray _ | Null | Bot -> false
    in
    let set_boxed x =
      match Var.Tbl.get types x with
      | Number (n, Unboxed) -> Var.Tbl.set types x (Number (n, Boxed))
      | Number (_, Boxed) | Top | Int _ | Tuple _ | Bigarray _ | Null | Bot -> ()
    in
    let changed = ref true in
    while !changed do
      changed := false;
      (* Values that become used boxed, as they flow into a boxed variable *)
      let pending = ref [] in
      Var.ISet.iter
        (fun y ->
          match Var.Tbl.get types y with
          | Number (_, Unboxed) -> (
              match st.global_flow_state.defs.(Var.idx y) with
              | Phi { known; _ } ->
                  if Var.Set.exists is_boxed known
                  then (
                    set_boxed y;
                    changed := true;
                    pending := Var.Set.elements known @ !pending)
              | Expr (Apply { f; _ }) -> (
                  match Global_flow.get_unique_closure st.global_flow_info f with
                  | Some (g, _) when can_unbox_return_value st.fun_info g ->
                      let s = Var.Map.find g st.global_flow_info.info_return_vals in
                      if (not (Var.ISet.mem boxed_returns g)) && Var.Set.exists is_boxed s
                      then (
                        Var.ISet.add boxed_returns g;
                        changed := true;
                        List.iter
                          ~f:set_boxed
                          (Var.Hashtbl.find_opt call_results g |> Option.value ~default:[]);
                        pending := Var.Set.elements s @ !pending)
                  | Some _ | None -> ())
              | Expr _ -> ())
          | Number (_, Boxed) | Top | Int _ | Tuple _ | Bigarray _ | Null | Bot -> ())
        boxed_uses;
      List.iter ~f:(fun x -> Var.ISet.add boxed_uses x) !pending
    done;
    (* Boxing a constant is free (it is a static value), so constants used
        boxed are boxed eagerly; this is done last, so that a boxed constant
        does not force a phi to be boxed. *)
    Var.ISet.iter
      (fun y ->
        match Var.Tbl.get types y with
        | Number (_, Unboxed) -> (
            match st.global_flow_state.defs.(Var.idx y) with
            | Expr (Constant _) -> set_boxed y
            | Expr _ | Phi _ -> ())
        | Number (_, Boxed) | Top | Int _ | Tuple _ | Bigarray _ | Null | Bot -> ())
      boxed_uses);
  boxed_returns

let print_opt types global_flow_state f e =
  match e with
  | Prim (Extern (name, _), args)
    when type_specialized_primitive types global_flow_state name args ->
      Format.fprintf f " OPT"
  | _ -> ()

type t =
  { types : typ Var.Tbl.t
  ; return_types : typ Var.Hashtbl.t
  ; extra_types : typ Var.Hashtbl.t
  }

let f ~global_flow_state ~global_flow_info ~fun_info ~deadcode_sentinel p =
  let t = Timer.make () in
  update_deps global_flow_state p;
  let boxed_function_parameters = mark_boxed_function_parameters ~fun_info p in
  let parameter_type_hints = collect_parameter_type_hints p in
  let st =
    { global_flow_state
    ; global_flow_info
    ; boxed_function_parameters
    ; parameter_type_hints
    ; fun_info
    }
  in
  let types = solver st in
  Var.Tbl.set types deadcode_sentinel (Int Small_normalized);
  let boxed_returns = box_numbers ~lazy_boxing:(Config.Flag.lcm ()) p st types in
  if times () then Format.eprintf "  type analysis: %a@." Timer.print t;
  if debug ()
  then (
    Var.ISet.iter
      (fun x ->
        match global_flow_state.defs.(Var.idx x) with
        | Expr _ -> ()
        | Phi _ ->
            let t = Var.Tbl.get types x in
            if not (Domain.equal t Top)
            then Format.eprintf "%a: %a@." Var.print x Domain.print t)
      global_flow_state.vars;
    Print.program
      Format.err_formatter
      (fun _ i ->
        match i with
        | Instr (Let (x, e)) ->
            Format.asprintf
              "{%a}%a"
              Domain.print
              (Var.Tbl.get types x)
              (print_opt types global_flow_state)
              e
        | _ -> "")
      p);
  let return_types = Var.Hashtbl.create 128 in
  Code.fold_closures
    p
    (fun name_opt _ _ _ () ->
      Option.iter
        ~f:(fun f ->
          if can_unbox_return_value fun_info f
          then
            let s = Var.Map.find f global_flow_info.info_return_vals in
            let t = Var.Set.fold (fun x t -> Domain.join (Var.Tbl.get types x) t) s Bot in
            Var.Hashtbl.replace
              return_types
              f
              (if Var.ISet.mem boxed_returns f then Domain.box t else t))
        name_opt)
    ();
  { types; return_types; extra_types = Var.Hashtbl.create 128 }

let var_type info x =
  let idx = Var.idx x in
  if idx < Var.Tbl.length info.types
  then Var.Tbl.get info.types x
  else Var.Hashtbl.find_opt info.extra_types x |> Option.value ~default:Top

let set_var_type info x t =
  let idx = Var.idx x in
  if idx < Var.Tbl.length info.types
  then Var.Tbl.set info.types x t
  else Var.Hashtbl.replace info.extra_types x t

let return_type info f =
  Var.Hashtbl.find_opt info.return_types f |> Option.value ~default:Top

let join = Domain.join

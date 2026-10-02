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
    | Normalized
    | Unnormalized

  let join r r' =
    match r, r' with
    | Unnormalized, _ | _, Unnormalized -> Unnormalized
    | Ref, Ref -> Ref
    | _ -> Normalized
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

type array_kind =
  { float : bool  (** May be a float array *)
  ; value : bool  (** May be a non-empty block *)
  ; cast : bool
        (** Only when [float] is false: represented by a Wasm block
            reference, the value being cast where it is defined *)
  }

let empty_array = { float = false; value = false; cast = false }

let float_array = { float = true; value = false; cast = false }

let value_array = { float = false; value = true; cast = false }

let any_array = { float = true; value = true; cast = false }

type typ =
  | Top
  | Int of Integer.kind
  | Number of boxed_number * boxed_status
  | Tuple of
      { fields : typ array
      ; block : bool
      ; cast : bool
            (** Only when [block] holds: represented by a Wasm block
                reference, the value being cast where it is defined *)
      }
      (** This value is a block (not a float array) if [block] holds, a
          block or an integer otherwise; if it's a block, an
          overapproximation of the possible values of each of its
          fields is given by [fields] *)
  | Bigarray of Optimization_hint.Bigarray.t
  | Array of array_kind
      (** An array: the empty array (which is a block with no field), a
          float array if [float] holds, or a non-empty block if [value]
          holds *)
  | Null
  | Bot

let array_kind t : Optimization_hint.array_kind =
  match t with
  | Array { float = false; _ } -> Value
  | Array { value = false; _ } -> Float
  | Tuple { block = true; _ } -> Value
  | Top | Int _ | Number _ | Tuple _ | Bigarray _ | Array _ | Null | Bot -> Generic

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
    | ( Tuple { fields = t; block = b; cast = c }
      , Tuple { fields = t'; block = b'; cast = c' } ) ->
        let l = Array.length t in
        let l' = Array.length t' in
        Tuple
          { fields =
              (if l = l'
               then Array.map2 ~f:join t t'
               else
                 Array.init (max l l') ~f:(fun i ->
                     if i < l then if i < l' then join t.(i) t'.(i) else t.(i) else t'.(i)))
          ; block = b && b'
          ; cast = c && c'
          }
    | (Int _ | Null), Tuple { fields; _ } | Tuple { fields; _ }, (Int _ | Null) ->
        Tuple { fields; block = false; cast = false }
    | Bigarray b, Bigarray b' when Bigarray.equal b b' -> t
    | Array a, Array a' ->
        Array
          { float = a.float || a'.float
          ; value = a.value || a'.value
          ; cast = a.cast && a'.cast
          }
    | Array { float = false; value = false; cast = c }, Tuple ({ cast = c'; _ } as r)
    | Tuple ({ cast = c'; _ } as r), Array { float = false; value = false; cast = c } ->
        Tuple { r with cast = c && c' }
    | Array a, Tuple { block = true; cast; _ } | Tuple { block = true; cast; _ }, Array a
      -> Array { a with value = true; cast = a.cast && cast }
    | Array { float = false; value = false; _ }, (Int _ | Null)
    | (Int _ | Null), Array { float = false; value = false; _ } ->
        Tuple { fields = [||]; block = false; cast = false }
    | Null, Null -> Null
    | Top, _ | _, Top -> Top
    | (Int _ | Number _ | Tuple _ | Bigarray _ | Array _ | Null), _ -> Top

  let join_set ?(others = false) f s =
    if others then Top else Var.Set.fold (fun x a -> join (f x) a) s Bot

  let rec equal t t' =
    match t, t' with
    | Top, Top | Bot, Bot -> true
    | Int t, Int t' -> Poly.equal t t'
    | Number (t, b), Number (t', b') -> Poly.equal t t' && Poly.equal b b'
    | ( Tuple { fields = t; block = b; cast = c }
      , Tuple { fields = t'; block = b'; cast = c' } ) ->
        Bool.equal b b'
        && Bool.equal c c'
        && Array.length t = Array.length t'
        && Array.for_all2 ~f:equal t t'
    | Bigarray b, Bigarray b' -> Bigarray.equal b b'
    | Array a, Array a' ->
        Bool.equal a.float a'.float
        && Bool.equal a.value a'.value
        && Bool.equal a.cast a'.cast
    | Null, Null -> true
    | (Top | Tuple _ | Int _ | Number _ | Bigarray _ | Array _ | Null | Bot), _ -> false

  let bot = Bot

  let depth_threshold = 4

  let rec depth t =
    match t with
    | Top | Bot | Number _ | Int _ | Bigarray _ | Array _ | Null -> 0
    | Tuple { fields = l; _ } ->
        1 + Array.fold_left ~f:(fun acc t' -> max (depth t') acc) l ~init:0

  let rec truncate depth t =
    match t with
    | Top | Bot | Number _ | Int _ | Bigarray _ | Array _ | Null -> t
    | Tuple ({ fields; _ } as r) ->
        if depth = 0
        then Top
        else
          Tuple
            { r with fields = Array.map ~f:(fun t' -> truncate (depth - 1) t') fields }

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
          | Normalized -> "normalized"
          | Unnormalized -> "unnormalized")
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
    | Array { float; value; cast } ->
        Format.fprintf
          f
          "array{%s%s}"
          (match float, value with
          | false, false -> "empty"
          | true, false -> "float"
          | false, true -> "value"
          | true, true -> "any")
          (if cast then "!" else "")
    | Null -> Format.fprintf f "null"
    | Tuple { fields; block; cast } ->
        Format.fprintf
          f
          "(%a)%s"
          (Format.pp_print_list ~pp_sep:(fun f () -> Format.fprintf f ",") print)
          (Array.to_list fields)
          (if cast then "!" else if block then "" else "?")
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
                        | "caml_make_vect"
                        | "caml_array_make"
                        | "caml_uniform_array_make"
                        | "caml_array_sub"
                        | "caml_array_sub_local"
                        | "caml_array_append"
                        | "caml_array_append_local"
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
  | Int _ -> Int Normalized
  | Int32 _ -> Number (Int32, Unboxed)
  | Int64 _ -> Number (Int64, Unboxed)
  | NativeInt _ -> Number (Nativeint, Unboxed)
  | Float _ -> Number (Float, Unboxed)
  | Float32 _ -> Number (Float32, Unboxed)
  | Tuple (_, [||], _) -> Array empty_array
  | Tuple (_, a, _) ->
      Tuple
        { fields = Array.map ~f:(fun c' -> Domain.box (constant_type c')) a
        ; block = true
        ; cast = false
        }
  | Float_array _ -> Array float_array
  | Null_ -> Null
  | _ -> Top

let arg_type ~approx arg =
  match arg with
  | Pc c -> constant_type c
  | Pv x -> Var.Tbl.get approx x

let bigarray_element_type (kind : Optimization_hint.Bigarray.kind) =
  match kind with
  | Float16 | Float32 | Float64 -> Number (Float, Unboxed)
  | Float32_t -> Number (Float32, Unboxed)
  | Int8_signed | Int8_unsigned | Int16_signed | Int16_unsigned -> Int Normalized
  | Int -> Int Unnormalized
  | Int32 -> Number (Int32, Unboxed)
  | Int64 -> Number (Int64, Unboxed)
  | Nativeint -> Number (Nativeint, Unboxed)
  | Complex32 | Complex64 ->
      (* Complex numbers are float records *)
      Array float_array

let bigarray_type ~approx ba =
  match arg_type ~approx ba with
  | Bot -> Bot
  | Bigarray { kind; _ } -> bigarray_element_type kind
  | _ -> Top

let primitive_types = String.Hashtbl.create 16

let prim_type ~st ~approx prim hint args =
  match prim with
  | "%int_and" -> (
      (* A non-negative operand clears the bits above the 31 low bits; a
         negative one, such as [-1], keeps them *)
      let non_negative_constant a =
        match a with
        | Pc (Int c) -> Targetint.(compare c zero) >= 0
        | Pc _ | Pv _ -> false
      in
      if List.exists ~f:non_negative_constant args
      then Int Normalized
      else
        match List.map ~f:(fun x -> arg_type ~approx x) args with
        | [ (Bot | Int (Ref | Normalized)); (Bot | Int (Ref | Normalized)) ] ->
            Int Normalized
        | _ -> Int Unnormalized)
  | "%int_or" | "%int_xor" -> (
      match List.map ~f:(fun x -> arg_type ~approx x) args with
      | [ (Bot | Int (Ref | Normalized)); (Bot | Int (Ref | Normalized)) ] ->
          Int Normalized
      | _ -> Int Unnormalized)
  | "caml_make_vect" | "caml_array_make" | "caml_uniform_array_make" -> (
      match args with
      | [ _; init ] -> (
          (* The runtime builds a float array exactly when the initial
             value is a boxed float *)
          match arg_type ~approx init with
          | Bot -> Bot
          | Number (Float, _) -> Array float_array
          | Top -> (
              (* Float constants have a [Number] type *)
              match init with
              | Pc _ -> Array value_array
              | Pv y -> (
                  match st.global_flow_state.defs.(Var.idx y) with
                  | Expr (Constant _ | Closure _) -> Array value_array
                  | Expr _ | Phi _ -> Array any_array))
          | Int _
          | Number ((Int32 | Int64 | Nativeint | Float32), _)
          | Tuple _ | Bigarray _ | Array _ | Null -> Array value_array)
      | _ -> Top)
  | "caml_floatarray_create"
  | "caml_make_float_vect"
  | "caml_array_create_float"
  | "caml_floatarray_create_local"
  | "caml_floatarray_make"
  | "caml_floatarray_sub"
  | "caml_floatarray_append"
  | "caml_floatarray_concat" -> Array float_array
  | "caml_array_sub"
  | "caml_array_sub_local"
  | "caml_array_append"
  | "caml_array_append_local" -> (
      (* The result is either the empty array or has the
         representation of one of the array arguments *)
      let array_arg a =
        match arg_type ~approx a with
        | Bot -> Bot
        | Array _ as t -> t
        | _ -> Array any_array
      in
      match prim, args with
      | ("caml_array_sub" | "caml_array_sub_local"), [ a; _; _ ] -> array_arg a
      | ("caml_array_append" | "caml_array_append_local"), [ a; a' ] ->
          Domain.join (array_arg a) (array_arg a')
      | _ -> Top)
  | "caml_array_concat" | "caml_array_concat_local" -> Array any_array
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
      | Some (_, typ) -> typ
      | None -> Top)

let reset () = String.Hashtbl.reset primitive_types

let register_prim nm ~unbox typ = String.Hashtbl.replace primitive_types nm (unbox, typ)

let propagate st approx x : Domain.t =
  match st.global_flow_state.defs.(Var.idx x) with
  | Phi { known; others; unit } -> (
      let res = Domain.join_set ~others (fun y -> Var.Tbl.get approx y) known in
      let res = if unit then Domain.join (Int Unnormalized) res else res in
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
      | Block (254, _, _, _) -> Array float_array
      | Block (_, [||], _, _) -> Array empty_array
      | Block (_, lst, _, _) ->
          Tuple
            { fields =
                Array.mapi
                  ~f:(fun i y ->
                    match st.global_flow_state.mutable_fields.(Var.idx x) with
                    | All_fields -> Top
                    | Some_fields s when IntSet.mem i s -> Top
                    | Some_fields _ | No_field ->
                        Domain.limit (Domain.box (Var.Tbl.get approx y)))
                  lst
            ; block = true
            ; cast = false
            }
      | Field (_, _, Float) -> Number (Float, Unboxed)
      | Field (y, n, Non_float) -> (
          match Var.Tbl.get approx y with
          | Tuple { fields = t; _ } -> if n < Array.length t then t.(n) else Bot
          | Top -> Top
          | Array { float = false; value = false; _ } -> Bot
          | Array _ -> Top
          | _ -> Bot)
      | Prim
          ( Extern
              (("caml_check_bound" | "caml_check_bound_float" | "caml_check_bound_gen"), _)
          , [ Pv y; _ ] ) -> Var.Tbl.get approx y
      | Prim (Extern ("caml_array_unsafe_get", _), [ Pv y; _ ])
        when Poly.equal (array_kind (Var.Tbl.get approx y)) Float ->
          Number (Float, Unboxed)
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
      | Prim ((Vectlength _ | Not | IsInt | Eq | Neq | Lt | Le | Ult), _) ->
          Int Normalized
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
  | "caml_array_unsafe_set" -> (
      (* Compiled as [caml_floatarray_unsafe_set], which takes an
         unboxed float *)
      match args with
      | a :: _ -> (
          match array_kind (arg_type ~approx:types a) with
          | Float -> true
          | Value | Generic -> false)
      | [] -> false)
  | "caml_ba_get_generic" | "caml_ba_set_generic" -> (
      match args with
      | Pv x :: Pv indices :: _ -> (
          match Var.Tbl.get types x, global_flow_state.defs.(Var.idx indices) with
          | Bigarray _, Expr (Block _) -> true
          | _ -> false)
      | _ -> false)
  | _ -> false

let box_numbers p st types =
  (* We box numbers eagerly if the boxed value is ever used. *)
  let should_box = Var.ISet.empty () in
  let rec box y =
    if not (Var.ISet.mem should_box y)
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
      | Number (_, Boxed) | Int _ | Tuple _ | Bigarray _ | Array _ | Null | Bot -> ())
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
                      if
                        match Global_flow.get_unique_closure st.global_flow_info f with
                        | None -> true
                        | Some (g, _) -> not (can_unbox_parameters st.fun_info g)
                      then List.iter ~f:box args
                  | Block (tag, lst, _, _) -> if tag <> 254 then Array.iter ~f:box lst
                  | Prim (Extern (s, _), args) ->
                      if
                        not
                          (String.Hashtbl.mem primitive_types s
                           && fst (String.Hashtbl.find primitive_types s)
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
                  | Prim ((Vectlength _ | Array_get | Not | IsInt | Lt | Le | Ult), _)
                  | Field _ | Closure _ | Constant _ | Special _ -> ())
              | Set_field (_, _, Non_float, y) | Array_set (_, _, y) -> box y
              | Assign _ | Offset_ref _ | Set_field (_, _, Float, _) | Event _ -> ())
            b.body;
          match b.branch with
          | Return y ->
              Option.iter
                ~f:(fun g -> if not (can_unbox_return_value st.fun_info g) then box y)
                name_opt
          | Raise _ | Stop | Branch _ | Cond _ | Switch _ | Pushtrap _ | Poptrap _ -> ())
        pc
        p.blocks
        ())
    ()

let can_cast st y =
  (* The parameters of functions with unknown call sites have the
     generic type *)
  not (Var.ISet.mem st.boxed_function_parameters y)

let castable_functions st types =
  (* Functions whose return values can all be cast. Casting the result
     of a call to another function would take place just after the
     call, and prevent a tail call: in a cycle of tail calls, this
     would make the stack grow. So, we only propagate casts to the
     return values of these functions. *)
  let defs = st.global_flow_state.defs in
  let return_vals = st.global_flow_info.info_return_vals in
  let unique_callee y =
    match defs.(Var.idx y) with
    | Expr (Apply { f; _ }) -> (
        match Global_flow.get_unique_closure st.global_flow_info f with
        | Some (g, _) when can_unbox_return_value st.fun_info g -> Some g
        | _ -> None)
    | _ -> None
  in
  (* The values the return values come from, through phis *)
  let vars = ref [] in
  let seen = Var.ISet.empty () in
  let rec collect y =
    if not (Var.ISet.mem seen y)
    then (
      Var.ISet.add seen y;
      vars := y :: !vars;
      match defs.(Var.idx y) with
      | Phi { known; _ } -> Var.Set.iter collect known
      | Expr _ -> ())
  in
  Var.Map.iter (fun _ s -> Var.Set.iter collect s) return_vals;
  let bad = Var.ISet.empty () in
  List.iter !vars ~f:(fun y ->
      let ok =
        can_cast st y
        &&
        match Var.Tbl.get types y with
        | Tuple { block = true; _ } | Array { float = false; _ } -> (
            match defs.(Var.idx y) with
            | Expr (Apply _) -> Option.is_some (unique_callee y)
            | Phi _ | Expr _ -> true)
        | Bot -> true
        | Top | Int _ | Number _ | Tuple _ | Bigarray _ | Array _ | Null -> false
      in
      if not ok then Var.ISet.add bad y);
  let is_bad_function g =
    Var.Set.exists (Var.ISet.mem bad) (Var.Map.find g return_vals)
  in
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter !vars ~f:(fun y ->
        if not (Var.ISet.mem bad y)
        then
          let b =
            match defs.(Var.idx y) with
            | Phi { known; _ } -> Var.Set.exists (Var.ISet.mem bad) known
            | Expr _ -> (
                match unique_callee y with
                | Some g -> is_bad_function g
                | None -> false)
          in
          if b
          then (
            Var.ISet.add bad y;
            changed := true))
  done;
  fun g -> can_unbox_return_value st.fun_info g && not (is_bad_function g)

let cast_blocks p st types =
  (* A value known to be a block is given the type of a Wasm block
     reference, and is cast where it is defined, if it is used as a
     block. This is propagated to the values it comes from, so that it
     is cast only once. *)
  let castable = castable_functions st types in
  let visited = Var.ISet.empty () in
  let rec cast y =
    if (not (Var.ISet.mem visited y)) && can_cast st y
    then (
      Var.ISet.add visited y;
      match Var.Tbl.get types y with
      | Tuple ({ block = true; _ } as r) ->
          Var.Tbl.set types y (Tuple { r with cast = true });
          propagate y
      | Array ({ float = false; _ } as a) ->
          Var.Tbl.set types y (Array { a with cast = true });
          propagate y
      | Top | Int _ | Number _ | Tuple _ | Bigarray _ | Array _ | Null | Bot -> ())
  and propagate y =
    match st.global_flow_state.defs.(Var.idx y) with
    | Phi { known; _ } -> Var.Set.iter cast known
    | Expr (Apply { f; _ }) -> (
        match Global_flow.get_unique_closure st.global_flow_info f with
        | None -> ()
        | Some (g, _) ->
            if castable g
            then Var.Set.iter cast (Var.Map.find g st.global_flow_info.info_return_vals))
    | Expr
        (Prim
           ( Extern
               ( ("caml_check_bound" | "caml_check_bound_gen" | "caml_check_bound_float")
               , _ )
           , Pv z :: _ )) -> cast z
    | Expr _ -> ()
  in
  let cast_arg a =
    match a with
    | Pv y -> cast y
    | Pc _ -> ()
  in
  Code.fold_closures
    p
    (fun _ _ (pc, _) _ () ->
      traverse
        { fold = Code.fold_children }
        (fun pc () ->
          let b = Addr.Map.find pc p.blocks in
          List.iter
            ~f:(fun i ->
              match i with
              | Let (_, e) -> (
                  match e with
                  | Field (y, _, Non_float) -> cast y
                  | Prim
                      ( ( Vectlength _ | Array_get
                        | Extern
                            ( ( "caml_check_bound"
                              | "caml_check_bound_gen"
                              | "caml_check_bound_float"
                              | "caml_array_unsafe_get"
                              | "caml_array_unsafe_set"
                              | "caml_array_unsafe_set_addr"
                              | "%direct_obj_tag" )
                            , _ ) )
                      , a :: _ ) -> cast_arg a
                  | Field (_, _, Float)
                  | Prim _ | Apply _ | Block _ | Closure _ | Constant _ | Special _ -> ())
              | Set_field (y, _, Non_float, _) | Offset_ref (y, _) | Array_set (y, _, _)
                -> cast y
              | Assign _ | Set_field (_, _, Float, _) | Event _ -> ())
            b.body)
        pc
        p.blocks
        ())
    ()

let print_opt types global_flow_state f e =
  match e with
  | Prim (Extern (name, _), args)
    when type_specialized_primitive types global_flow_state name args ->
      Format.fprintf f " OPT"
  | _ -> ()

type t =
  { types : typ Var.Tbl.t
  ; return_types : typ Var.Hashtbl.t
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
  Var.Tbl.set types deadcode_sentinel (Int Normalized);
  box_numbers p st types;
  cast_blocks p st types;
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
            Var.Hashtbl.replace
              return_types
              f
              (Var.Set.fold (fun x t -> Domain.join (Var.Tbl.get types x) t) s Bot))
        name_opt)
    ();
  { types; return_types }

let var_type info x = Var.Tbl.get info.types x

let return_type info f =
  Var.Hashtbl.find_opt info.return_types f |> Option.value ~default:Top

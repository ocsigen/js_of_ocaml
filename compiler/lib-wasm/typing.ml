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

type boxed_status =
  | Boxed
  | Unboxed

module Bigarray = struct
  type kind =
    | Float16
    | Float32
    | Float64
    | Int8_signed
    | Int8_unsigned
    | Int16_signed
    | Int16_unsigned
    | Int32
    | Int64
    | Int
    | Nativeint
    | Complex32
    | Complex64

  type layout =
    | C
    | Fortran

  type t =
    { kind : kind
    ; layout : layout
    }

  let make ~kind ~layout =
    { kind =
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

  let print f { kind; layout } =
    Format.fprintf
      f
      "bigarray{%s,%s}"
      (match kind with
      | Float32 -> "float32"
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

  let equal { kind; layout } { kind = kind'; layout = layout' } =
    phys_equal kind kind' && phys_equal layout layout'
end

type typ =
  | Top
  | Int of Integer.kind
  | Number of boxed_number * boxed_status
  | Tuple of typ array
      (** This value is a block or an integer; if it's an integer, an
          overapproximation of the possible values of each of its
          fields is given by the array of types *)
  | Bigarray of Bigarray.t
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
    | Int _, Tuple _ -> t'
    | Tuple _, Int _ -> t
    | Bigarray b, Bigarray b' when Bigarray.equal b b' -> t
    | Top, _ | _, Top -> Top
    | (Int _ | Number _ | Tuple _ | Bigarray _), _ -> Top

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
    | (Top | Tuple _ | Int _ | Number _ | Bigarray _ | Bot), _ -> false

  let bot = Bot

  let depth_threshold = 4

  let rec depth t =
    match t with
    | Top | Bot | Number _ | Int _ | Bigarray _ -> 0
    | Tuple l -> 1 + Array.fold_left ~f:(fun acc t' -> max (depth t') acc) l ~init:0

  let rec truncate depth t =
    match t with
    | Top | Bot | Number _ | Int _ | Bigarray _ -> t
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
          | Float -> "float")
          (match b with
          | Boxed -> "boxed"
          | Unboxed -> "unboxed")
    | Bigarray b -> Bigarray.print f b
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
                      ( "%int_and"
                      | "%int_or"
                      | "%int_xor"
                      | "caml_ba_get_1"
                      | "caml_ba_get_2"
                      | "caml_ba_get_3"
                      | "caml_ba_get_generic" )
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

type st =
  { global_flow_state : Global_flow.state
  ; global_flow_info : Global_flow.info
  ; boxed_function_parameters : Var.ISet.t
  ; fun_info : Call_graph_analysis.t
  }

let rec constant_type (c : constant) =
  match c with
  | Int _ -> Int Normalized
  | Int32 _ -> Number (Int32, Unboxed)
  | Int64 _ -> Number (Int64, Unboxed)
  | NativeInt _ -> Number (Nativeint, Unboxed)
  | Float _ -> Number (Float, Unboxed)
  | Tuple (_, a, _) -> Tuple (Array.map ~f:(fun c' -> Domain.box (constant_type c')) a)
  | _ -> Top

let arg_type ~approx arg =
  match arg with
  | Pc c -> constant_type c
  | Pv x -> Var.Tbl.get approx x

let bigarray_element_type (kind : Bigarray.kind) =
  match kind with
  | Float16 | Float32 | Float64 -> Number (Float, Unboxed)
  | Int8_signed | Int8_unsigned | Int16_signed | Int16_unsigned -> Int Normalized
  | Int -> Int Unnormalized
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

let prim_type ~st ~approx prim args =
  match prim with
  | "%int_and" -> (
      match List.map ~f:(fun x -> arg_type ~approx x) args with
      | [ (Bot | Int (Ref | Normalized)); _ ] | [ _; (Bot | Int (Ref | Normalized)) ] ->
          Int Normalized
      | _ -> Int Unnormalized)
  | "%int_or" | "%int_xor" -> (
      match List.map ~f:(fun x -> arg_type ~approx x) args with
      | [ (Bot | Int (Ref | Normalized)); (Bot | Int (Ref | Normalized)) ] ->
          Int Normalized
      | _ -> Int Unnormalized)
  | "caml_ba_create" -> (
      match args with
      | [ Pc (Int kind); Pc (Int layout); _ ] ->
          Bigarray
            (Bigarray.make
               ~kind:(Targetint.to_int_exn kind)
               ~layout:(Targetint.to_int_exn layout))
      | _ -> Top)
  | "caml_ba_get_1" | "caml_ba_get_2" | "caml_ba_get_3" -> (
      match args with
      | ba :: _ -> bigarray_type ~approx ba
      | [] -> Top)
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
  | Phi { known; others; unit } ->
      let res = Domain.join_set ~others (fun y -> Var.Tbl.get approx y) known in
      let res = if unit then Domain.join (Int Unnormalized) res else res in
      if Var.ISet.mem st.boxed_function_parameters x then Domain.box res else res
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
          ( Extern ("caml_check_bound" | "caml_check_bound_float" | "caml_check_bound_gen")
          , [ Pv y; _ ] ) -> Var.Tbl.get approx y
      | Prim ((Array_get | Extern "caml_array_unsafe_get"), [ Pv y; _ ]) -> (
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
      | Prim ((Vectlength | Not | IsInt | Eq | Neq | Lt | Le | Ult), _) -> Int Normalized
      | Prim (Extern prim, args) -> prim_type ~st ~approx prim args
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
                                (Prim (Extern "caml_ba_create", [ Pv kind; Pv layout; _ ]))
                              -> (
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
      | [ Number (Float, _); Number (Float, _) ] -> true
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
      | Number (_, Boxed) | Int _ | Tuple _ | Bigarray _ | Bot -> ())
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
                  | Prim (Extern s, args) ->
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
                  | Prim ((Vectlength | Array_get | Not | IsInt | Lt | Le | Ult), _)
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

let print_opt types global_flow_state f e =
  match e with
  | Prim (Extern name, args)
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
  let st = { global_flow_state; global_flow_info; boxed_function_parameters; fun_info } in
  let types = solver st in
  Var.Tbl.set types deadcode_sentinel (Int Normalized);
  box_numbers p st types;
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

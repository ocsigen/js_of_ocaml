(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2010 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
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

let specialize_instr info i =
  match i with
  | Let (x, Prim (Extern "caml_format_int", [ y; z ])) -> (
      match the_string_of info y with
      | Some "%d" -> (
          match the_int info z with
          | Some i -> Let (x, Constant (String (Int32.to_string i)))
          | None -> Let (x, Prim (Extern "%caml_format_int_special", [ z ])))
      | _ -> i)
  | Let (x, Prim (Extern "%caml_format_int_special", [ z ])) -> (
      match the_int info z with
      | Some i -> Let (x, Constant (String (Int32.to_string i)))
      | None -> i)
  (* inline the String constant argument so that generate.ml can attempt to parse it *)
  | Let
      ( x
      , Prim
          ( Extern (("caml_js_var" | "caml_js_expr" | "caml_pure_js_expr") as prim)
          , [ (Pv _ as y) ] ) )
    when Config.Flag.safe_string () -> (
      match the_string_of info y with
      | Some s -> Let (x, Prim (Extern prim, [ Pc (String s) ]))
      | _ -> i)
  | Let (x, Prim (Extern ("caml_register_named_value" as prim), [ y; z ])) -> (
      match the_string_of info y with
      | Some s when Primitive.need_named_value s ->
          Let (x, Prim (Extern prim, [ Pc (String s); z ]))
      | Some _ -> Let (x, Constant (Int 0l))
      | None -> i)
  | Let (x, Prim (Extern "caml_js_call", [ f; o; a ])) -> (
      match the_def_of info a with
      | Some (Block (_, a, _)) ->
          let a = Array.map a ~f:(fun x -> Pv x) in
          Let (x, Prim (Extern "%caml_js_opt_call", f :: o :: Array.to_list a))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_fun_call", [ f; a ])) -> (
      match the_def_of info a with
      | Some (Block (_, a, _)) ->
          let a = Array.map a ~f:(fun x -> Pv x) in
          Let (x, Prim (Extern "%caml_js_opt_fun_call", f :: Array.to_list a))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_meth_call", [ o; m; a ])) -> (
      match the_string_of info m with
      | Some m when String.is_ascii m -> (
          match the_def_of info a with
          | Some (Block (_, a, _)) ->
              let a = Array.map a ~f:(fun x -> Pv x) in
              Let
                ( x
                , Prim
                    ( Extern "%caml_js_opt_meth_call"
                    , o :: Pc (NativeString m) :: Array.to_list a ) )
          | _ -> i)
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_new", [ c; a ])) -> (
      match the_def_of info a with
      | Some (Block (_, a, _)) ->
          let a = Array.map a ~f:(fun x -> Pv x) in
          Let (x, Prim (Extern "%caml_js_opt_new", c :: Array.to_list a))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_object", [ a ])) -> (
      try
        let a =
          match the_def_of info a with
          | Some (Block (_, a, _)) -> a
          | _ -> raise Exit
        in
        let a =
          Array.map a ~f:(fun x ->
              match the_def_of info (Pv x) with
              | Some (Block (_, [| k; v |], _)) ->
                  let k =
                    match the_string_of info (Pv k) with
                    | Some s when String.is_ascii s -> Pc (NativeString s)
                    | _ -> raise Exit
                  in
                  [ k; Pv v ]
              | _ -> raise Exit)
        in
        Let (x, Prim (Extern "%caml_js_opt_object", List.flatten (Array.to_list a)))
      with Exit -> i)
  | Let (x, Prim (Extern "caml_js_get", [ o; (Pv _ as f) ])) -> (
      match the_native_string_of info f with
      | Some s -> Let (x, Prim (Extern "caml_js_get", [ o; Pc (NativeString s) ]))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_set", [ o; (Pv _ as f); v ])) -> (
      match the_native_string_of info f with
      | Some s -> Let (x, Prim (Extern "caml_js_set", [ o; Pc (NativeString s); v ]))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_delete", [ o; (Pv _ as f) ])) -> (
      match the_native_string_of info f with
      | Some s -> Let (x, Prim (Extern "caml_js_delete", [ o; Pc (NativeString s) ]))
      | _ -> i)
  | Let (x, Prim (Extern ("caml_jsstring_of_string" | "caml_js_from_string"), [ y ])) -> (
      match the_string_of info y with
      | Some s when String.is_ascii s -> Let (x, Constant (NativeString s))
      | _ -> i)
  | Let (x, Prim (Extern "%int_mul", [ y; z ])) -> (
      match the_int info y, the_int info z with
      | Some j, _ when Int32.(abs j < 0x200000l) ->
          Let (x, Prim (Extern "%direct_int_mul", [ y; z ]))
      | _, Some j when Int32.(abs j < 0x200000l) ->
          Let (x, Prim (Extern "%direct_int_mul", [ y; z ]))
      | _ -> i)
  | Let (x, Prim (Extern "%int_div", [ y; z ])) -> (
      match the_int info z with
      | Some j when Int32.(j <> 0l) -> Let (x, Prim (Extern "%direct_int_div", [ y; z ]))
      | _ -> i)
  | Let (x, Prim (Extern "%int_mod", [ y; z ])) -> (
      match the_int info z with
      | Some j when Int32.(j <> 0l) -> Let (x, Prim (Extern "%direct_int_mod", [ y; z ]))
      | _ -> i)
  | _ -> i

let specialize_instrs info l =
  let rec aux info checks l acc =
    match l with
    | [] -> List.rev acc
    | i :: r -> (
        (* We make bound checking explicit. Then, we can remove duplicated
           bound checks. Also, it appears to be more efficient to inline
           the array access. The bound checking function returns the array,
           which allows to produce more compact code. *)
        match i with
        | Let (x, Prim (Extern "caml_array_get", [ y; z ]))
        | Let (x, Prim (Extern "caml_array_get_float", [ y; z ]))
        | Let (x, Prim (Extern "caml_array_get_addr", [ y; z ])) ->
            let idx =
              match the_int info z with
              | Some idx -> `Cst idx
              | None -> `Var z
            in
            if List.mem (y, idx) ~set:checks
            then
              let acc = Let (x, Prim (Extern "caml_array_unsafe_get", [ y; z ])) :: acc in
              aux info checks r acc
            else
              let y' = Code.Var.fresh () in
              let acc =
                Let (x, Prim (Extern "caml_array_unsafe_get", [ Pv y'; z ]))
                :: Let (y', Prim (Extern "caml_check_bound", [ y; z ]))
                :: acc
              in
              aux info ((y, idx) :: checks) r acc
        | Let (x, Prim (Extern "caml_array_set", [ y; z; t ]))
        | Let (x, Prim (Extern "caml_array_set_float", [ y; z; t ]))
        | Let (x, Prim (Extern "caml_array_set_addr", [ y; z; t ])) ->
            let idx =
              match the_int info z with
              | Some idx -> `Cst idx
              | None -> `Var z
            in
            if List.mem (y, idx) ~set:checks
            then
              let acc =
                Let (x, Prim (Extern "caml_array_unsafe_set", [ y; z; t ])) :: acc
              in
              aux info checks r acc
            else
              let y' = Code.Var.fresh () in
              let acc =
                Let (x, Prim (Extern "caml_array_unsafe_set", [ Pv y'; z; t ]))
                :: Let (y', Prim (Extern "caml_check_bound", [ y; z ]))
                :: acc
              in
              aux info ((y, idx) :: checks) r acc
        | _ ->
            let i = specialize_instr info i in
            aux info checks r (i :: acc))
  in
  aux info [] l []

let specialize_all_instrs info p =
  let blocks =
    Addr.Map.map
      (fun block -> { block with Code.body = specialize_instrs info block.body })
      p.blocks
  in
  { p with blocks }

(****)

let f info p = specialize_all_instrs info p

let f_once p =
  let rec loop acc l =
    match l with
    | [] -> List.rev acc
    | i :: r -> (
        match i with
        | Let
            ( x
            , (Prim
                 ( Extern
                     ( "caml_array_set"
                     | "caml_array_unsafe_set"
                     | "caml_array_set_float"
                     | "caml_array_set_addr"
                     | "caml_array_unsafe_set_float"
                     | "caml_floatarray_unsafe_set" )
                 , [ _; _; _ ] ) as p) ) ->
            let x' = Code.Var.fork x in
            let acc = Let (x', p) :: Let (x, Constant (Int 0l)) :: acc in
            loop acc r
        | _ -> loop (i :: acc) r)
  in
  let blocks =
    Addr.Map.map (fun block -> { block with Code.body = loop [] block.body }) p.blocks
  in
  { p with blocks }

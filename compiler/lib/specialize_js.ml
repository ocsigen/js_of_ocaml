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

let specialize_instr ~target info i =
  match i, target with
  | Let (x, Prim (Extern "caml_format_int", [ y; z ])), `JavaScript -> (
      (* We can implement the special case where the format string is "%s" in JavaScript
         in a concise and efficient way with [""+x]. It does not make as much sense in
         Wasm to have a special case for this. *)
      match the_string_of ~target info y with
      | Some "%d" -> (
          match the_int ~target info z with
          | Some i -> Let (x, Constant (String (Targetint.to_string i)))
          | None -> Let (x, Prim (Extern "%caml_format_int_special", [ z ])))
      | _ -> i)
  | Let (x, Prim (Extern "%caml_format_int_special", [ z ])), `JavaScript -> (
      match the_int ~target info z with
      | Some i -> Let (x, Constant (String (Targetint.to_string i)))
      | None -> i)
  (* inline the String constant argument so that generate.ml can attempt to parse it *)
  | ( Let
        ( x
        , Prim
            ( Extern (("caml_js_var" | "caml_js_expr" | "caml_pure_js_expr") as prim)
            , [ (Pv _ as y) ] ) )
    , target ) -> (
      match the_string_of ~target info y with
      | Some s -> Let (x, Prim (Extern prim, [ Pc (String s) ]))
      | _ -> i)
  | Let (x, Prim (Extern ("caml_register_named_value" as prim), [ y; z ])), _ -> (
      match the_string_of ~target info y with
      | Some s when Primitive.need_named_value s ->
          Let (x, Prim (Extern prim, [ Pc (String s); z ]))
      | Some _ -> Let (x, Constant (Int Targetint.zero))
      | None -> i)
  | Let (x, Prim (Extern "caml_js_call", [ f; o; a ])), _ -> (
      match the_def_of info a with
      | Some (Block (_, a, _, _)) ->
          let a = Array.map a ~f:(fun x -> Pv x) in
          Let (x, Prim (Extern "%caml_js_opt_call", f :: o :: Array.to_list a))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_fun_call", [ f; a ])), _ -> (
      match the_def_of info a with
      | Some (Block (_, a, _, _)) ->
          let a = Array.map a ~f:(fun x -> Pv x) in
          Let (x, Prim (Extern "%caml_js_opt_fun_call", f :: Array.to_list a))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_meth_call", [ o; m; a ])), _ -> (
      match the_string_of ~target info m with
      | Some m when Javascript.is_ident m -> (
          match the_def_of info a with
          | Some (Block (_, a, _, _)) ->
              let a = Array.map a ~f:(fun x -> Pv x) in
              Let
                ( x
                , Prim
                    ( Extern "%caml_js_opt_meth_call"
                    , o
                      :: Pc (NativeString (Native_string.of_string m))
                      :: Array.to_list a ) )
          | _ -> i)
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_new", [ c; a ])), _ -> (
      match the_def_of info a with
      | Some (Block (_, a, _, _)) ->
          let a = Array.map a ~f:(fun x -> Pv x) in
          Let (x, Prim (Extern "%caml_js_opt_new", c :: Array.to_list a))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_object", [ a ])), _ -> (
      try
        let a =
          match the_def_of info a with
          | Some (Block (_, a, _, _)) -> a
          | _ -> raise Exit
        in
        let a =
          Array.map a ~f:(fun x ->
              match the_def_of info (Pv x) with
              | Some (Block (_, [| k; v |], _, _)) ->
                  let k =
                    match the_string_of ~target info (Pv k) with
                    | Some s when String.is_valid_utf_8 s ->
                        Pc (NativeString (Native_string.of_string s))
                    | Some _ | None -> raise Exit
                  in
                  [ k; Pv v ]
              | Some (Constant (Tuple (0, [| String k; v |], (NotArray | Unknown))))
                when String.is_valid_utf_8 k ->
                  [ Pc (NativeString (Native_string.of_string k)); Pc v ]
              | Some _ | None -> raise Exit)
        in
        Let (x, Prim (Extern "%caml_js_opt_object", List.flatten (Array.to_list a)))
      with Exit -> i)
  | Let (x, Prim (Extern "caml_js_get", [ o; (Pv _ as f) ])), _ -> (
      match the_native_string_of ~target info f with
      | Some s -> Let (x, Prim (Extern "caml_js_get", [ o; Pc (NativeString s) ]))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_set", [ o; (Pv _ as f); v ])), _ -> (
      match the_native_string_of ~target info f with
      | Some s -> Let (x, Prim (Extern "caml_js_set", [ o; Pc (NativeString s); v ]))
      | _ -> i)
  | Let (x, Prim (Extern "caml_js_delete", [ o; (Pv _ as f) ])), _ -> (
      match the_native_string_of ~target info f with
      | Some s -> Let (x, Prim (Extern "caml_js_delete", [ o; Pc (NativeString s) ]))
      | _ -> i)
  | Let (x, Prim (Extern ("caml_jsstring_of_string" | "caml_js_from_string"), [ y ])), _
    -> (
      match the_string_of ~target info y with
      | Some s when String.is_valid_utf_8 s ->
          Let (x, Constant (NativeString (Native_string.of_string s)))
      | Some _ | None -> i)
  | Let (x, Prim (Extern "caml_jsbytes_of_string", [ y ])), _ -> (
      match the_string_of ~target info y with
      | Some s -> Let (x, Constant (NativeString (Native_string.of_bytestring s)))
      | None -> i)
  | Let (x, Prim (Extern "%int_mul", [ y; z ])), `JavaScript -> (
      let limit = Targetint.of_int_exn 0x200000 in
      (* Using * to multiply integers in JavaScript yields a float; and if the
           float is large enough, some bits can be lost. So, in the general case,
           we have to use Math.imul. There is no such issue in Wasm. *)
      match the_int ~target info y, the_int ~target info z with
      | Some j, _ when Targetint.(abs j < limit) ->
          Let (x, Prim (Extern "%direct_int_mul", [ y; z ]))
      | _, Some j when Targetint.(abs j < limit) ->
          Let (x, Prim (Extern "%direct_int_mul", [ y; z ]))
      | _ -> i)
  | Let (x, Prim (Extern "%int_div", [ y; z ])), _ -> (
      match the_int ~target info z with
      | Some j when not (Targetint.is_zero j) ->
          Let (x, Prim (Extern "%direct_int_div", [ y; z ]))
      | _ -> i)
  | Let (x, Prim (Extern "%int_mod", [ y; z ])), _ -> (
      match the_int ~target info z with
      | Some j when not (Targetint.is_zero j) ->
          Let (x, Prim (Extern "%direct_int_mod", [ y; z ]))
      | _ -> i)
  | _, _ -> i

let skip_event cont (Event _ :: l | l) = cont l

let recognize_string_length cont =
  skip_event
  @@ fun l ->
  match l with
  | (Let (len, Prim (Extern "caml_ml_string_length", [ Pv str ])) as i) :: l ->
      cont i ~len ~str l
  | _ -> None

let recognize_int_add ~x ~y cont =
  skip_event
  @@ fun l ->
  match l with
  | (Let (res, Prim (Extern "%int_add", [ Pv x'; Pv y' ])) as i) :: l
    when Code.Var.equal x x' && Code.Var.equal y y' -> cont i ~res l
  | _ -> None

let recognize_create_bytes ~len cont =
  skip_event
  @@ fun l ->
  match l with
  | Let (bytes, Prim (Extern "caml_create_bytes", [ Pv len' ])) :: l
    when Code.Var.equal len len' -> cont ~bytes l
  | _ -> None

let recognize_blit_string ~str ~bytes ~ofs ~len cont =
  skip_event
  @@ fun l ->
  match l with
  | Let
      ( _
      , Prim
          (Extern "caml_blit_string", [ Pv str'; Pc (Int zero); Pv bytes'; ofs'; Pv len' ])
      )
    :: l
    when Code.Var.equal str str'
         && Targetint.is_zero zero
         && Code.Var.equal bytes bytes'
         && Code.Var.equal len len'
         &&
         match ofs, ofs' with
         | Pc (Int ofs), Pc (Int ofs') -> Targetint.equal ofs ofs'
         | Pv ofs, Pv ofs' -> Code.Var.equal ofs ofs'
         | _ -> false -> cont l
  | _ -> None

let recognize_string_of_bytes ~bytes cont =
  skip_event
  @@ fun l ->
  match l with
  | Let (str, Prim (Extern "caml_string_of_bytes", [ Pv bytes' ])) :: l
    when Code.Var.equal bytes bytes' -> cont ~str l
  | _ -> None

let recognize_empty_body cont =
  skip_event @@ fun l -> if List.is_empty l then cont () else None

let specialize_string_concat l =
  Option.value
    ~default:l
    (l
    |> recognize_string_length
       @@ fun len1 ~len:alen ~str:a ->
       recognize_string_length
       @@ fun len2 ~len:blen ~str:b ->
       recognize_int_add ~x:alen ~y:blen
       @@ fun len3 ~res:len ->
       recognize_create_bytes ~len
       @@ fun ~bytes ->
       recognize_blit_string ~str:a ~bytes ~ofs:(Pc (Int Targetint.zero)) ~len:alen
       @@ recognize_blit_string ~str:b ~bytes ~ofs:(Pv alen) ~len:blen
       @@ recognize_string_of_bytes ~bytes
       @@ fun ~str ->
       recognize_empty_body
       @@ fun () ->
       Some
         [ len1
         ; len2
         ; len3
         ; Let (str, Prim (Extern "caml_string_concat", [ Pv a; Pv b ]))
         ; Let (bytes, Prim (Extern "caml_bytes_of_string", [ Pv str ]))
         ])

let specialize_instrs ~target info l =
  let rec aux info checks l acc =
    match l with
    | [] -> List.rev acc
    | i :: r -> (
        (* We make bound checking explicit. Then, we can remove duplicated
           bound checks. Also, it appears to be more efficient to inline
           the array access. The bound checking function returns the array,
           which allows to produce more compact code. *)
        match i with
        | Let
            ( x
            , Prim
                ( Extern
                    (( "caml_array_get"
                     | "caml_array_get_float"
                     | "caml_floatarray_get"
                     | "caml_array_get_addr" ) as prim)
                , [ y; z ] ) ) ->
            let idx =
              match the_int ~target info z with
              | Some idx -> `Cst idx
              | None -> `Var z
            in
            let instr y =
              let prim =
                match prim with
                | "caml_array_get" -> Extern "caml_array_unsafe_get"
                | "caml_array_get_float" | "caml_floatarray_get" ->
                    Extern "caml_floatarray_unsafe_get"
                | "caml_array_get_addr" -> Array_get
                | _ -> assert false
              in
              Let (x, Prim (prim, [ y; z ]))
            in
            if List.mem (y, idx) ~set:checks
            then
              let acc = instr y :: acc in
              aux info checks r acc
            else
              let check =
                match prim with
                | "caml_array_get" -> "caml_check_bound_gen"
                | "caml_array_get_float" | "caml_floatarray_get" ->
                    "caml_check_bound_float"
                | "caml_array_get_addr" -> "caml_check_bound"
                | _ -> assert false
              in
              let y' = Code.Var.fresh () in
              let acc = instr (Pv y') :: Let (y', Prim (Extern check, [ y; z ])) :: acc in
              aux info ((y, idx) :: checks) r acc
        | Let
            ( x
            , Prim
                ( Extern
                    (( "caml_array_set"
                     | "caml_array_set_float"
                     | "caml_floatarray_set"
                     | "caml_array_set_addr" ) as prim)
                , [ y; z; t ] ) ) ->
            let idx =
              match the_int ~target info z with
              | Some idx -> `Cst idx
              | None -> `Var z
            in
            let instr y =
              let prim =
                match prim with
                | "caml_array_set" -> "caml_array_unsafe_set"
                | "caml_array_set_float" | "caml_floatarray_set" ->
                    "caml_floatarray_unsafe_set"
                | "caml_array_set_addr" -> "caml_array_unsafe_set_addr"
                | _ -> assert false
              in
              Let (x, Prim (Extern prim, [ y; z; t ]))
            in
            if List.mem (y, idx) ~set:checks
            then
              let acc = instr y :: acc in
              aux info checks r acc
            else
              let check =
                match prim with
                | "caml_array_set" -> "caml_check_bound_gen"
                | "caml_array_set_float" | "caml_floatarray_set" ->
                    "caml_check_bound_float"
                | "caml_array_set_addr" -> "caml_check_bound"
                | _ -> assert false
              in
              let y' = Code.Var.fresh () in
              let acc = instr (Pv y') :: Let (y', Prim (Extern check, [ y; z ])) :: acc in
              aux info ((y, idx) :: checks) r acc
        | _ ->
            let i = specialize_instr ~target info i in
            aux info checks r (i :: acc))
  in
  aux info [] l []

let specialize_all_instrs ~target info p =
  let blocks =
    Addr.Map.map
      (fun block ->
        { block with
          Code.body = specialize_instrs ~target info (specialize_string_concat block.body)
        })
      p.blocks
  in
  { p with blocks }

(****)

let f info p = specialize_all_instrs ~target:(Config.target ()) info p

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
                     | "caml_floatarray_set"
                     | "caml_array_set_addr"
                     | "caml_array_unsafe_set_float"
                     | "caml_floatarray_unsafe_set" )
                 , [ _; _; _ ] ) as p) ) ->
            let x' = Code.Var.fork x in
            let acc = Let (x', p) :: Let (x, Constant (Int Targetint.zero)) :: acc in
            loop acc r
        | _ -> loop (i :: acc) r)
  in
  let blocks =
    Addr.Map.map (fun block -> { block with Code.body = loop [] block.body }) p.blocks
  in
  { p with blocks }

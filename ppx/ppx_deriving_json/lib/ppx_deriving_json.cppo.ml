(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright Vasilis Papavasileiou 2015
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

let deriver = "json"

(* Copied (and adapted) this from ppx_deriving repo (commit
   e2079fa8f3460055bf990461f295c6c4b391fafc) ; we get an empty set of
   let bindings with ppx_deriving 3.0 *)
let sanitize expr = [%expr
  (let open! Ppx_deriving_runtime in [%e expr]) [@ocaml.warning "-A"]]

let runtimename = "Deriving_Json"

let rt name = Ast_convenience.evar (Printf.sprintf "%s.%s" runtimename name)
let rt_t arg = Ast_convenience.tconstr (Printf.sprintf "%s.t" runtimename) [arg]
let lexer_ident name = Printf.sprintf "%s_lexer.%s" runtimename name
let lexbuf_t = Ast_convenience.tconstr (lexer_ident "lexbuf") []
let lexer name = Ast_convenience.evar (lexer_ident name)

let var_ptuple l =
  List.map Ast_convenience.pvar l |> Ast_helper.Pat.tuple

let map_loc f {Location.txt; loc} =
  {Location.txt = f txt; loc}

let suffix_lid {Location.txt; loc} ~suffix =
  let txt = Ppx_deriving.mangle_lid (`Suffix suffix) txt in
  Ast_helper.Exp.ident {txt; loc} ~loc

let suffix_decl ({Parsetree.ptype_loc = loc; _} as d) ~suffix =
  (let s =
     Ppx_deriving.mangle_type_decl (`Suffix suffix) d |>
     Longident.parse
   in
   Location.mkloc s loc) |> Ast_helper.Exp.ident ~loc

let suffix_decl_p ({Parsetree.ptype_loc = loc; _} as d) ~suffix =
  (let s = Ppx_deriving.mangle_type_decl (`Suffix suffix) d in
   Location.mkloc s loc) |> Ast_helper.Pat.var ~loc

let rec fresh_vars ?(acc = []) n =
  if n <= 0 then
    List.rev acc
  else
    let acc = Ppx_deriving.fresh_var acc :: acc in
    fresh_vars ~acc (n - 1)

let label_of_constructor = map_loc (fun c -> Longident.Lident c)

let wrap_write r ~pattern = [%expr fun buf [%p pattern] -> [%e r]]

let buf_expand r = [%expr fun buf -> [%e r]]

let seqlist = function
  | h :: l ->
    let f acc e = [%expr [%e acc]; [%e e]] in
    List.fold_left f h l
  | [] ->
    [%expr ()]

let check_record_fields =
  List.iter @@ function
  | {Parsetree.pld_mutable = Mutable; _} ->
    Location.raise_errorf
      "%s cannot be derived for mutable records" deriver
  | {pld_type = {ptyp_desc = Ptyp_poly _; _}; _} ->
    Location.raise_errorf
      "%s cannot be derived for polymorphic records" deriver
  | _ ->
    ()

let maybe_tuple_type = function
  | [y] -> y
  | l -> Ast_helper.Typ.tuple l

let pattern_of_record l =
  let l =
    let f {Parsetree.pld_name; _} =
      label_of_constructor pld_name,
      Ast_helper.Pat.var pld_name
    in
    List.map f l
  in
  Ast_helper.Pat.record l Asttypes.Closed

let rec write_tuple_contents l ly ~tag ~poly =
  let e =
    let f arg y =
      let e = write_body_of_type y ~arg ~poly in
      [%expr Buffer.add_string buf ","; [%e e]]
    in
    List.map2 f l ly |> seqlist
  and s = Ast_convenience.str ("[" ^ string_of_int tag) in [%expr
    Buffer.add_string buf [%e s];
    [%e e];
    Buffer.add_string buf "]"]

and write_body_of_tuple_type l ~arg ~poly ~tag =
  let n = List.length l in
  let vars = fresh_vars n in
  let e = write_tuple_contents vars l ~tag ~poly
  and p = var_ptuple vars in
  [%expr let [%p p] = [%e arg] in [%e e]]

and write_poly_case r ~arg ~poly =
  match r with
#if OCAML_VERSION < (4, 06, 0)
  | Parsetree.Rtag (label, _, _, l) ->
#else
  | Parsetree.Rtag ({txt=label;_}, _, _, l) ->
#endif
    let i = Ppx_deriving.hash_variant label
    and n = List.length l in
    let v = Ppx_deriving.fresh_var [] in
    let lhs =
      (if n = 0 then None else Some (Ast_convenience.pvar v)) |>
      Ast_helper.Pat.variant label
    and rhs =
      match l with
      | [] ->
        let e = Ast_convenience.int i in
        [%expr [%e rt "Json_int.write"] buf [%e e]]
      | _ ->
        let l = [[%type: int]; maybe_tuple_type l]
        and arg = Ast_helper.Exp.tuple Ast_convenience.[int i; evar v] in
        write_body_of_tuple_type l ~arg ~poly ~tag:0
    in
    Ast_helper.Exp.case lhs rhs
  | Rinherit ({ptyp_desc = Ptyp_constr (lid, _); ptyp_loc; _} as y) ->
    Ast_helper.Exp.case
      (Ast_helper.Pat.(alias (type_ lid)) (Location.mkloc arg ptyp_loc))
      (write_body_of_type y ~arg ~poly)
  | Rinherit {ptyp_loc; _} ->
    Location.raise_errorf ~loc:ptyp_loc
      "%s write case cannot be derived" deriver

and write_body_of_type y ~(arg : string) ~poly =
  let arg = Ast_convenience.evar arg
  and arg' = arg in
  match y with
  | [%type: unit] ->
    [%expr [%e rt "Json_unit.write"] buf [%e arg]]
  | [%type: int] ->
    [%expr [%e rt "Json_int.write"] buf [%e arg]]
  | [%type: int32] | [%type: Int32.t] ->
    [%expr [%e rt "Json_int32.write"] buf [%e arg]]
  | [%type: int64] | [%type: Int64.t] ->
    [%expr [%e rt "Json_int64.write"] buf [%e arg]]
  | [%type: nativeint] | [%type: Nativeint.t] ->
    [%expr [%e rt "Json_nativeint.write"] buf [%e arg]]
  | [%type: float] ->
    [%expr [%e rt "Json_float.write"] buf [%e arg]]
  | [%type: bool] ->
    [%expr [%e rt "Json_bool.write"] buf [%e arg]]
  | [%type: char] ->
    [%expr [%e rt "Json_char.write"] buf [%e arg]]
  | [%type: string] ->
    [%expr [%e rt "Json_string.write"] buf [%e arg]]
  | [%type: bytes] ->
    [%expr [%e rt "Json_bytes.write"] buf [%e arg]]
  | [%type: [%t? y] list] ->
    let e = write_of_type y ~poly in
    [%expr [%e rt "write_list"] [%e e] buf [%e arg]]
  | [%type: [%t? y] ref] ->
    let e = write_of_type y ~poly in
    [%expr [%e rt "write_ref"] [%e e] buf [%e arg]]
  | [%type: [%t? y] option] ->
    let e = write_of_type y ~poly in
    [%expr [%e rt "write_option"] [%e e] buf [%e arg]]
  | [%type: [%t? y] array] ->
    let e = write_of_type y ~poly in
    [%expr [%e rt "write_array"] [%e e] buf [%e arg]]
  | { Parsetree.ptyp_desc = Ptyp_var v; _} when poly ->
    [%expr [%e Ast_convenience.evar ("poly_" ^ v)] buf [%e arg]]
  | { Parsetree.ptyp_desc = Ptyp_tuple l; _ } ->
    write_body_of_tuple_type l ~arg ~poly ~tag:0
  | { Parsetree.ptyp_desc = Ptyp_variant (l, _, _);  _ } ->
    Ast_helper.Exp.match_ arg
      (List.map (write_poly_case ~arg:arg' ~poly) l)
  | { Parsetree.ptyp_desc = Ptyp_constr (lid, l); _ } ->
    let e = suffix_lid lid ~suffix:"to_json"
    and l = List.map (write_of_type ~poly) l in
    [%expr [%e Ast_convenience.app e l] buf [%e arg]]
  | { Parsetree.ptyp_loc; _ } ->
    Location.raise_errorf ~loc:ptyp_loc
      "%s_write cannot be derived for %s"
      deriver (Ppx_deriving.string_of_core_type y)

and write_of_type y ~poly =
  let arg = "a" in
  let pattern = Ast_convenience.pvar arg in
  wrap_write (write_body_of_type y ~arg ~poly) ~pattern

and write_body_of_record ~tag l =
  let l =
    let f {Parsetree.pld_name = {txt; _}; _} = txt in
    List.map f l
  and ly =
    let f {Parsetree.pld_type; _} = pld_type in
    List.map f l
  in
  write_tuple_contents l ly ~tag ~poly:true

and write_of_record ?(tag=0) _d l =
  let pattern = pattern_of_record l
  and e = write_body_of_record ~tag l in
  wrap_write e ~pattern

let recognize_case_of_constructor i l =
  let lhs =
    match l with
    | [] -> [%pat? `Cst  [%p Ast_convenience.pint i]]
    | _  -> [%pat? `NCst [%p Ast_convenience.pint i]]
  in
  Ast_helper.Exp.case lhs [%expr true]

let recognize_body_of_poly_variant l ~loc =
  let l =
    let f = function
#if OCAML_VERSION < (4, 06, 0)
      | Parsetree.Rtag (label, _, _, l) ->
#else
      | Parsetree.Rtag ({txt=label;_}, _, _, l) ->
#endif
        let i = Ppx_deriving.hash_variant label in
        recognize_case_of_constructor i l
      | Rinherit {ptyp_desc = Ptyp_constr (lid, _); _} ->
        let guard = [%expr [%e suffix_lid lid ~suffix:"recognize"] x] in
        Ast_helper.Exp.case ~guard [%pat? x] [%expr true]
      | _ ->
        Location.raise_errorf ~loc
          "%s_recognize cannot be derived" deriver
    and default = Ast_helper.Exp.case [%pat? _] [%expr false] in
    List.map f l @ [default]
  in
  Ast_helper.Exp.function_ l

let tag_error_case ?(typename="") () =
  let y = Ast_convenience.str typename in
  Ast_helper.Exp.case
    [%pat? _]
    [%expr [%e lexer "tag_error"] ~typename:[%e y] buf]

let maybe_tuple_type = function
  | [y] -> y
  | l -> Ast_helper.Typ.tuple l

let rec read_poly_case ?decl y = function
#if OCAML_VERSION < (4, 06, 0)
  | Parsetree.Rtag (label, _, _, l) ->
#else
  | Parsetree.Rtag ({txt=label;_}, _, _, l) ->
#endif
    let i = Ppx_deriving.hash_variant label |> Ast_convenience.pint in
    (match l with
     | [] ->
       Ast_helper.Exp.case [%pat? `Cst [%p i]]
         (Ast_helper.Exp.variant label None)
     | l ->
       Ast_helper.Exp.case [%pat? `NCst [%p i]] [%expr
         [%e lexer "read_comma"] buf;
         let v = [%e read_body_of_type ?decl (maybe_tuple_type l)] in
         [%e lexer "read_rbracket"] buf;
         [%e Ast_helper.Exp.variant label (Some [%expr v])]])
  | Rinherit {ptyp_desc = Ptyp_constr (lid, l); _} ->
    let guard = [%expr [%e suffix_lid lid ~suffix:"recognize"] x]
    and e =
      let e = suffix_lid lid ~suffix:"of_json_with_tag"
      and l = List.map (read_of_type ?decl) l in
      [%expr ([%e Ast_convenience.app e l] buf x :> [%t y])]
    in
    Ast_helper.Exp.case ~guard [%pat? x] e
  | Rinherit {ptyp_loc; _} ->
    Location.raise_errorf ~loc:ptyp_loc
      "%s read case cannot be derived" deriver

and read_of_poly_variant ?decl l y ~loc:_ =
  List.map (read_poly_case ?decl y) l @ [tag_error_case ()] |>
  Ast_helper.Exp.function_ |>
  buf_expand

and read_tuple_contents ?decl l ~f =
  let n = List.length l in
  let lv = fresh_vars n in
  let f v y acc =
    let e = read_body_of_type ?decl y in [%expr
      [%e lexer "read_comma"] buf;
      let [%p Ast_convenience.pvar v] = [%e e] in
      [%e acc]]
  and acc = List.map Ast_convenience.evar lv |> f in
  let acc = [%expr [%e lexer "read_rbracket"] buf; [%e acc]] in
  List.fold_right2 f lv l acc

and read_body_of_tuple_type ?decl l = [%expr
  [%e lexer "read_lbracket"] buf;
  ignore ([%e lexer "read_tag_1"] 0 buf);
  [%e read_tuple_contents ?decl l ~f:Ast_helper.Exp.tuple]]

and read_of_record_raw ?decl ?(return = (fun x -> x)) l =
  let f =
    let f {Parsetree.pld_name; _} e = label_of_constructor pld_name, e in
    fun l' -> return (Ast_helper.Exp.record (List.map2 f l l') None)
  and l =
    let f {Parsetree.pld_type; _} = pld_type in
    List.map f l
  in
  read_tuple_contents l ?decl ~f

and read_of_record decl l =
  let e = read_of_record_raw ~decl l in
  [%expr
      [%e lexer "read_lbracket"] buf;
      ignore ([%e lexer "read_tag_2"] 0 254 buf);
      [%e e]] |> buf_expand

and read_body_of_type ?decl y =
  let poly = match decl with Some _ -> true | _ -> false in
  match y with
  | [%type: unit] ->
    [%expr [%e rt "Json_unit.read"] buf]
  | [%type: int] ->
    [%expr [%e rt "Json_int.read"] buf]
  | [%type: int32] | [%type: Int32.t] ->
    [%expr [%e rt "Json_int32.read"] buf]
  | [%type: int64] | [%type: Int64.t] ->
    [%expr [%e rt "Json_int64.read"] buf]
  | [%type: nativeint] | [%type: Nativeint.t] ->
    [%expr [%e rt "Json_nativeint.read"] buf]
  | [%type: float] ->
    [%expr [%e rt "Json_float.read"] buf]
  | [%type: bool] ->
    [%expr [%e rt "Json_bool.read"] buf]
  | [%type: char] ->
    [%expr [%e rt "Json_char.read"] buf]
  | [%type: string] ->
    [%expr [%e rt "Json_string.read"] buf]
  | [%type: bytes] ->
    [%expr [%e rt "Json_bytes.read"] buf]
  | [%type: [%t? y] list] ->
    [%expr [%e rt "read_list"] [%e read_of_type ?decl y] buf]
  | [%type: [%t? y] ref] ->
    [%expr [%e rt "read_ref"] [%e read_of_type ?decl y] buf]
  | [%type: [%t? y] option] ->
    [%expr [%e rt "read_option"] [%e read_of_type ?decl y] buf]
  | [%type: [%t? y] array] ->
    [%expr [%e rt "read_array"] [%e read_of_type ?decl y] buf]
  | { Parsetree.ptyp_desc = Ptyp_tuple l; _ } ->
    read_body_of_tuple_type l ?decl
  | { Parsetree.ptyp_desc = Ptyp_variant (l, _, _); ptyp_loc = loc; _ } ->
    let e =
      (match decl with
       | Some decl ->
         let e = suffix_decl decl ~suffix:"of_json_with_tag"
         and l =
           let {Parsetree.ptype_params = l; _} = decl
           and f (y, _) = read_of_type y ~decl in
           List.map f l
         in
         Ast_convenience.app e l
       | None ->
         read_of_poly_variant l y ~loc)
    and tag = [%expr [%e lexer "read_vcase"] buf] in
    [%expr [%e e] buf [%e tag]]
  | { Parsetree.ptyp_desc = Ptyp_var v; _ } when poly ->
    [%expr [%e Ast_convenience.evar ("poly_" ^ v)] buf]
  | { Parsetree.ptyp_desc = Ptyp_constr (lid, l); _ } ->
    let e = suffix_lid lid ~suffix:"of_json"
    and l = List.map (read_of_type ?decl) l in
    [%expr [%e Ast_convenience.app e l] buf]
  | { Parsetree.ptyp_loc; _ } ->
    Location.raise_errorf ~loc:ptyp_loc
      "%s_read cannot be derived for %s" deriver
      (Ppx_deriving.string_of_core_type y)

and read_of_type ?decl y =
  read_body_of_type ?decl y |> buf_expand

let json_of_type ?decl y =
  let read = read_of_type ?decl y
  and write =
    let poly = match decl with Some _ -> true | _ -> false in
    write_of_type y ~poly in
  [%expr [%e rt "make"] [%e write] [%e read]]

let fun_str_wrap d e y ~f ~suffix =
  let e = Ppx_deriving.poly_fun_of_type_decl d e |> sanitize
  and v = suffix_decl_p d ~suffix
  and y = Ppx_deriving.poly_arrow_of_type_decl f d y in
  Ast_helper.(Vb.mk (Pat.constraint_ v y) e)

let read_str_wrap d e =
  let f y = [%type: [%t lexbuf_t] -> [%t y]]
  and suffix = "of_json" in
  let y = f (Ppx_deriving.core_type_of_type_decl d) in
  fun_str_wrap d e y ~f ~suffix

let read_tag_str_wrap d e =
  let f y = [%type: [%t lexbuf_t] -> [%t y]]
  and suffix = "of_json_with_tag"
  and y =
    let y = Ppx_deriving.core_type_of_type_decl d in
    [%type: [%t lexbuf_t] ->
          [`NCst of int | `Cst of int] -> [%t y]]
  in
  fun_str_wrap d e y ~f ~suffix

let write_str_wrap d e =
  let f y = [%type: Buffer.t -> [%t y] -> unit]
  and suffix = "to_json" in
  let y =
    let y = Ppx_deriving.core_type_of_type_decl d in
    (match d with
     | {ptype_manifest =
          Some {ptyp_desc = Parsetree.Ptyp_variant (_, _, _); _}; _} ->
       [%type: [> [%t y]]]
     | _ ->
       y) |> f
  in
  fun_str_wrap d e y ~f ~suffix

let recognize_str_wrap d e =
  let v = suffix_decl_p d ~suffix:"recognize"
  and y = [%type: [`NCst of int | `Cst of int] -> bool] in
  Ast_helper.(Vb.mk (Pat.constraint_ v y) e)

let json_poly_type d =
  let f y = rt_t y in
  let y = f (Ppx_deriving.core_type_of_type_decl d) in
  Ppx_deriving.poly_arrow_of_type_decl f d y

let json_str_wrap d e =
  let v = suffix_decl_p d ~suffix:"json"
  and e = Ppx_deriving.(poly_fun_of_type_decl d e)
  and y = json_poly_type d in
  Ast_helper.(Vb.mk (Pat.constraint_ v y) e)

let json_str d =
  let write =
    let f acc id =
#if OCAML_VERSION < (4, 05, 0)
      let id = id in
#else
      let id = id.Location.txt in
#endif
      let poly = Ast_convenience.evar ("poly_" ^ id) in
      [%expr [%e acc] ([%e rt "write"] [%e poly])]
    and acc = suffix_decl d ~suffix:"to_json" in
    Ppx_deriving.fold_left_type_decl f acc d
  and read =
    let f acc id =
#if OCAML_VERSION < (4, 05, 0)
      let id = id in
#else
      let id = id.Location.txt in
#endif
      let poly = Ast_convenience.evar ("poly_" ^ id) in
      [%expr [%e acc] ([%e rt "read"] [%e poly])]
    and acc = suffix_decl d ~suffix:"of_json" in
    Ppx_deriving.fold_left_type_decl f acc d
  in
  [%expr [%e rt "make"] [%e write] [%e read]] |>
  json_str_wrap d

let write_decl_of_type d y =
  (let e = write_body_of_type y ~arg:"a" ~poly:true in
   [%expr fun buf a -> [%e e]]) |> write_str_wrap d

let read_decl_of_type decl y =
  read_body_of_type y ~decl |> buf_expand |> read_str_wrap decl

let json_decls_of_type decl y =
  let recognize, read_tag =
    match y with
    | { Parsetree.ptyp_desc = Ptyp_variant (l, _, _);
        ptyp_loc = loc; _ } ->
      Some (recognize_body_of_poly_variant l ~loc
            |> recognize_str_wrap decl),
      Some (read_of_poly_variant l y ~decl ~loc
            |> read_tag_str_wrap decl)
    | _ ->
      None, None
  in
  write_decl_of_type decl y,
  read_decl_of_type decl y,
  json_str decl,
  recognize, read_tag

let write_case (i, i', l) {Parsetree.pcd_name; pcd_args; _} =
  let i, i', lhs, rhs =
    match pcd_args with
#if OCAML_VERSION >= (4, 03, 0)
    | Pcstr_tuple [] | Pcstr_record [] ->
#else
    | [] ->
#endif
      i + 1,
      i',
      None,
      [%expr [%e rt "Json_int.write"] buf
               [%e Ast_convenience.int i]]
#if OCAML_VERSION >= (4, 03, 0)
    | Pcstr_tuple ([ _ ] as args) ->
#else
    | [ _ ] as args ->
#endif
      let v = Ppx_deriving.fresh_var [] in
      i,
      i' + 1,
      Some (Ast_convenience.pvar v),
      write_tuple_contents [v] args ~tag:i' ~poly:true
#if OCAML_VERSION >= (4, 03, 0)
    | Pcstr_tuple args ->
#else
    | args ->
#endif
      let vars = fresh_vars (List.length args) in
      i,
      i' + 1,
      Some (var_ptuple vars),
      write_tuple_contents vars args ~tag:i' ~poly:true
#if OCAML_VERSION >= (4, 03, 0)
    | Pcstr_record args ->
      i,
      i' + 1,
      Some (pattern_of_record args),
      write_body_of_record args ~tag:i'
#endif
  in
  i, i',
  Ast_helper.
    (Exp.case (Pat.construct (label_of_constructor pcd_name) lhs)
       rhs) :: l

let write_decl_of_variant d l =
  (let _, _, l = List.fold_left write_case (0, 0, []) l in
   Ast_helper.Exp.function_ l) |> buf_expand |>
  write_str_wrap d

let read_case ?decl (i, i', l)
    {Parsetree.pcd_name; pcd_args; _} =
  let f l =
    Ast_helper.Exp.construct
      (label_of_constructor pcd_name)
      (match l with
       | [] ->  None
       | [e] -> Some e
       | l ->   Some (Ast_helper.Exp.tuple l))
  in
  match pcd_args with
#if OCAML_VERSION >= (4, 03, 0)
  | Pcstr_tuple [] | Pcstr_record [] ->
#else
  | [] ->
#endif
    i + 1, i',
    Ast_helper.Exp.case
      [%pat? `Cst [%p Ast_convenience.pint i]]
      (Ast_helper.Exp.construct (label_of_constructor pcd_name) None)
    :: l
#if OCAML_VERSION >= (4, 03, 0)
  | Pcstr_tuple pcd_args ->
#else
  | pcd_args ->
#endif
    let expr = read_tuple_contents ?decl pcd_args ~f in
    let case = Ast_helper.Exp.case [%pat? `NCst [%p Ast_convenience.pint i']] expr in
    i, i' + 1, case :: l
#if OCAML_VERSION >= (4, 03, 0)
  | Pcstr_record pcd_args ->
    let patt = [%pat? `NCst [%p Ast_convenience.pint i']]
    and expr =
      let return e =
        Ast_helper.Exp.construct
          (label_of_constructor pcd_name)
          (Some e)
      in
      read_of_record_raw ?decl pcd_args ~return
    in
    i, i' + 1, Ast_helper.Exp.case patt expr :: l
#endif

let read_decl_of_variant decl l =
  (let _, _, l = List.fold_left (read_case ~decl) (0, 0, []) l
   and e = [%expr [%e lexer "read_case"] buf] in
   Ast_helper.Exp.match_ e (l @ [tag_error_case ()])) |>
  buf_expand |>
  read_str_wrap decl

let json_decls_of_variant d l =
  write_decl_of_variant d l, read_decl_of_variant d l, json_str d,
  None, None

let write_decl_of_record d l =
  write_of_record d l |> write_str_wrap d

let read_decl_of_record d l =
  read_of_record d l |> read_str_wrap d

let json_decls_of_record d l =
  check_record_fields l;
  write_decl_of_record d l, read_decl_of_record d l, json_str d,
  None, None

let json_str_of_decl ({Parsetree.ptype_loc; _} as d) =
  Ast_helper.with_default_loc ptype_loc @@ fun () ->
  match d with
  | { Parsetree.ptype_kind = Ptype_variant l; _ } ->
    json_decls_of_variant d l
  | { ptype_kind = Ptype_record l; _ } ->
    json_decls_of_record d l
  | { ptype_manifest = Some y; _ } ->
    json_decls_of_type d y
  | _ ->
    Location.raise_errorf "%s cannot be derived for %s" deriver
      (Ppx_deriving.mangle_type_decl (`Suffix "") d)

let read_sig_of_decl ({Parsetree.ptype_loc; _} as d) =
  (let s =
     let s = Ppx_deriving.mangle_type_decl (`Suffix "of_json") d in
     Location.mkloc s ptype_loc
   and y =
     let f y = [%type: [%t lexbuf_t] -> [%t y]] in
     let y = f (Ppx_deriving.core_type_of_type_decl d) in
     Ppx_deriving.poly_arrow_of_type_decl f d y
   in
   Ast_helper.Val.mk s y) |> Ast_helper.Sig.value

let recognize_sig_of_decl ({Parsetree.ptype_loc; _} as d) =
  (let s =
     let s = Ppx_deriving.mangle_type_decl (`Suffix "recognize") d in
     Location.mkloc s ptype_loc
   and y = [%type: [ `NCst of int  | `Cst of int ] -> bool] in
   Ast_helper.Val.mk s y) |> Ast_helper.Sig.value

let read_with_tag_sig_of_decl ({Parsetree.ptype_loc; _} as d) =
  (let s =
     let s =
       Ppx_deriving.mangle_type_decl (`Suffix "of_json_with_tag") d
     in
     Location.mkloc s ptype_loc
   and y =
     let f y = [%type: [%t lexbuf_t] -> [%t y]] in
     let y =
       let y = Ppx_deriving.core_type_of_type_decl d in
       f [%type: [ `NCst of int  | `Cst of int ] -> [%t y]]
     in
     Ppx_deriving.poly_arrow_of_type_decl f d y
   in
   Ast_helper.Val.mk s y) |> Ast_helper.Sig.value

let write_sig_of_decl ({Parsetree.ptype_loc; _} as d) =
  (let s =
     let s = Ppx_deriving.mangle_type_decl (`Suffix "to_json") d in
     Location.mkloc s ptype_loc
   and y =
     let f y = [%type: Buffer.t -> [%t y] -> unit] in
     let y = f (Ppx_deriving.core_type_of_type_decl d) in
     Ppx_deriving.poly_arrow_of_type_decl f d y
   in
   Ast_helper.Val.mk s y) |> Ast_helper.Sig.value

let json_sig_of_decl ({Parsetree.ptype_loc; _} as d) =
  (let s =
     let s = Ppx_deriving.mangle_type_decl (`Suffix "json") d in
     Location.mkloc s ptype_loc
   and y =
     let f y = rt_t y in
     let y = f (Ppx_deriving.core_type_of_type_decl d) in
     Ppx_deriving.poly_arrow_of_type_decl f d y
   in
   Ast_helper.Val.mk s y) |> Ast_helper.Sig.value

let sigs_of_decl ({Parsetree.ptype_loc; _} as d) =
  Ast_helper.with_default_loc ptype_loc @@ fun () ->
  let l = [
    read_sig_of_decl d;
    write_sig_of_decl d;
    json_sig_of_decl d
  ] in
  match d with
  | { Parsetree.ptype_manifest =
        Some {Parsetree.ptyp_desc = Parsetree.Ptyp_variant _; _}; _} ->
    read_with_tag_sig_of_decl d :: recognize_sig_of_decl d :: l
  | _ ->
    l

let register_for_expr s f =
  let core_type ({Parsetree.ptyp_loc; _} as y) =
    let f () = f y |> sanitize in
    Ast_helper.with_default_loc ptyp_loc f
  in
  Ppx_deriving.(create s ~core_type () |> register)

let _ =
  register_for_expr "of_json" @@ fun y -> [%expr
    fun s ->
      [%e read_of_type y]
        ([%e lexer "init_lexer"] (Lexing.from_string s))]

let _ =
  register_for_expr "to_json" @@ fun y -> [%expr
    fun x ->
      let buf = Buffer.create 50 in
      [%e write_of_type y ~poly:false] buf x;
      Buffer.contents buf]

let _ =
  let core_type ({Parsetree.ptyp_loc; _} as y) =
    let f () = json_of_type y |> sanitize in
    Ast_helper.with_default_loc ptyp_loc f
  and type_decl_str ~options:_ ~path:_ l =
    let lw, lr, lj, lp, lrv =
      let f d (lw, lr, lj, lp, lrv) =
        let w, r, j, p, rv = json_str_of_decl d in
        w :: lw, r :: lr, j :: lj,
        (match p with Some p -> p :: lp | None -> lp),
        (match rv with Some rv -> rv :: lrv | None -> lrv)
      and acc = [], [], [], [], [] in
      List.fold_right f l acc
    and f = Ast_helper.Str.value Asttypes.Recursive
    and f' = Ast_helper.Str.value Asttypes.Nonrecursive in
    let l = [f (lrv @ lr); f lw; f' lj] in
    match lp with [] -> l | _ -> f lp :: l
  and type_decl_sig ~options:_ ~path:_ l =
    List.map sigs_of_decl l |> List.flatten
  in
  Ppx_deriving.
    (create "json" ~core_type ~type_decl_str ~type_decl_sig ()
     |> register)

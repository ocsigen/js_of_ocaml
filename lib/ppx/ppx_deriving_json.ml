(* Js_of_ocaml
 * http://www.ocsigen.org
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
  (let open! Ppx_deriving_runtime in [%e expr]) [@ocaml.warning "-A"]
]

let rec fresh_vars ?(acc = []) n =
  if n <= 0 then
    List.rev acc
  else
    let acc = Ppx_deriving.fresh_var acc :: acc in
    fresh_vars ~acc (n - 1)

let unreachable_case () = {
  Parsetree.pc_lhs = [%pat? _ ];
  pc_guard = None;
  pc_rhs = [%expr assert false]
}

let label_of_constructor {Location.txt} =
  Longident.Lident txt |> Location.mknoloc

let wrap_write r ~pattern =
  [%expr fun buf [%p pattern] -> [%e r]]

let wrap_read r = [%expr fun buf -> [%e r]]

let json_attr attrs =
  Ppx_deriving.attr ~deriver "json" attrs |>
  Ppx_deriving.Arg.(get_attr ~deriver expr)

let seqlist = function
  | h :: l ->
    let f acc e = [%expr [%e acc]; [%e e]] in
    List.fold_left f h l
  | [] ->
    [%expr ()]

let check_record_fields =
  let f = function
    | {Parsetree.pld_mutable = Mutable} ->
      Location.raise_errorf
        "%s cannot be derived for mutable records" deriver
    | {pld_type = {ptyp_desc = Ptyp_poly _}} ->
      Location.raise_errorf
        "%s cannot be derived for polymorphic records" deriver
    | _ ->
      ()
  in
  List.iter f

let rec write_tuple_contents l ly tag ~poly =
  let e =
    let f v y =
      let arg = Ast_convenience.evar v in
      let e = write_body_of_type y ~arg ~poly in
      [%expr Buffer.add_string buf ","; [%e e]]
    in
    List.map2 f l ly |> seqlist
  and s =
    Asttypes.Const_string ("[" ^ string_of_int tag, None) |>
    Ast_helper.Exp.constant
  in [%expr
    Buffer.add_string buf [%e s];
    [%e e];
    Buffer.add_string buf "]"
  ]

and write_body_of_tuple_type l ~arg ~poly ~tag =
  let n = List.length l in
  let vars = fresh_vars n in
  let e = write_tuple_contents vars l tag ~poly
  and p =
    List.map Ast_convenience.pvar vars |>
    Ast_helper.Pat.tuple
  in
  [%expr let [%p p] = [%e arg] in [%e e]]

and write_case_of_constructor i f l =
  let n = List.length l in
  let vars = fresh_vars n in
  let pc_lhs =
    (match vars with
     | [] ->
       None
     | [v] ->
       Some (Ast_convenience.pvar v)
     | _ ->
       Some (List.map Ast_convenience.pvar vars |>
             Ast_helper.Pat.tuple)) |> f
  and pc_guard = None
  and pc_rhs = write_tuple_contents vars l i ~poly:true in
  {Parsetree.pc_lhs; pc_guard; pc_rhs}

and write_of_variant l =
  let e =
    (let f i { Parsetree.pcd_name; pcd_args; pcd_loc = loc } =
       let f =
         label_of_constructor pcd_name |>
         Ast_helper.Pat.construct in
       write_case_of_constructor i f pcd_args
     in
     List.mapi f l) |> Ast_helper.Exp.function_
  in
  [%expr fun buf -> [%e e]]

and write_body_of_poly_variant l ~arg ~loc ~poly =
  (let f i = function
     | Parsetree.Rtag (label, _, _, l) ->
       let i = Ppx_deriving.hash_variant label
       and f = Ast_helper.Pat.variant label in
       write_case_of_constructor i f l
     | Rinherit
         ({ptyp_desc = (Ptyp_constr (lid, _) as c)} as y) ->
       let pc_lhs = Ast_helper.Pat.type_ lid
       and pc_guard = None
       and pc_rhs =
         (* let arg = [%expr ([%e arg] :> [> [%t y]])] in *)
         write_body_of_type y ~arg ~poly in
       {Parsetree.pc_lhs; pc_guard; pc_rhs}
   in
   List.mapi f l @ [unreachable_case ()]) |> Ast_helper.Exp.match_ arg

and write_body_of_type y ~arg ~poly =
  match y with
  | [%type: unit] ->
    [%expr Deriving_Json.Json_unit.write buf [%e arg]]
  | [%type: int] ->
    [%expr Deriving_Json.Json_int.write buf [%e arg]]
  | [%type: int32] | [%type: Int32.t] ->
    [%expr Deriving_Json.Json_int32.write buf [%e arg]]
  | [%type: int64] | [%type: Int64.t] ->
    [%expr Deriving_Json.Json_int64.write buf [%e arg]]
  | [%type: nativeint] | [%type: Nativeint.t] ->
    [%expr Deriving_Json.Json_nativeint.write buf [%e arg]]
  | [%type: float] ->
    [%expr Deriving_Json.Json_float.write buf [%e arg]]
  | [%type: bool] ->
    [%expr Deriving_Json.Json_bool.write buf [%e arg]]
  | [%type: char] ->
    [%expr Deriving_Json.Json_char.write buf [%e arg]]
  | [%type: string] ->
    [%expr Deriving_Json.Json_string.write buf [%e arg]]
  | [%type: bytes] ->
    [%expr Deriving_Json.Json_bytes.write buf [%e arg]]
  | [%type: [%t? y] list] ->
    let e = [%expr [%e write_of_type y ~poly]] in
    [%expr Deriving_Json.write_list [%e e] buf [%e arg]]
  | [%type: [%t? y] ref] ->
    let e = [%expr [%e write_of_type y ~poly]] in
    [%expr Deriving_Json.write_ref [%e e] buf [%e arg]]
  | [%type: [%t? y] option] ->
    let e = [%expr [%e write_of_type y ~poly]] in
    [%expr Deriving_Json.write_option [%e e] buf [%e arg]]
  | [%type: [%t? y] array] ->
    let e = [%expr [%e write_of_type y ~poly]] in
    [%expr Deriving_Json.write_array [%e e] buf [%e arg]]
  | { Parsetree.ptyp_desc = Ptyp_var v } when poly ->
    [%expr [%e Ast_convenience.evar ("poly_" ^ v)] buf [%e arg]]
  | { Parsetree.ptyp_desc = Ptyp_tuple l } ->
    write_body_of_tuple_type l ~arg ~poly ~tag:0
  | { Parsetree.ptyp_desc = Ptyp_variant (l, _, _); ptyp_loc = loc } ->
    write_body_of_poly_variant l ~arg ~loc ~poly
  | { Parsetree.ptyp_desc = Ptyp_constr ({Asttypes.txt}, l) } ->
    let e =
      Ppx_deriving.mangle_lid (`Suffix "to_json") txt |>
      Location.mknoloc |>
      Ast_helper.Exp.ident
    and l = List.map (write_of_type ~poly) l in
    [%expr [%e Ast_convenience.app e l] buf [%e arg]]
  | { Parsetree.ptyp_loc } ->
    Location.raise_errorf ~loc:ptyp_loc
      "%s_write cannot be derived for %s"
      deriver (Ppx_deriving.string_of_core_type y)

and write_of_type y ~poly : Parsetree.expression =
  match json_attr y.Parsetree.ptyp_attributes with
  | Some fn ->
    fn
  | None ->
    let v = "a" in
    let arg = Ast_convenience.evar v
    and pattern = Ast_convenience.pvar v in
    wrap_write (write_body_of_type y ~arg ~poly) ~pattern

and write_of_record l =
  check_record_fields l;
  let pattern =
    let l =
      let f {Parsetree.pld_name} =
        (let {Location.txt} = pld_name in
         Location.mknoloc (Longident.Lident txt)),
        Ast_helper.Pat.var pld_name
      in
      List.map f l
    in
    (* CHECKME: what is the closed_flag for? *)
    Ast_helper.Pat.record l Asttypes.Closed
  and e =
    let l =
      let f {Parsetree.pld_name = {txt}} = txt in
      List.map f l
    and ly =
      let f {Parsetree.pld_type} = pld_type in
      List.map f l
    in
    write_tuple_contents l ly 0 ~poly:true
  in
  wrap_write e ~pattern

let recognize_case_of_constructor i l =
  let pc_lhs =
    let p = Ast_helper.Pat.constant (Asttypes.Const_int i) in
    match l with
    | [] ->
      [%pat? `Cst [%p p]]
    | _ ->
      [%pat? `NCst [%p p]]
  and pc_guard = None
  and pc_rhs = [%expr true] in
  {Parsetree.pc_lhs; pc_guard; pc_rhs}

let recognize_body_of_poly_variant l ~loc =
  let l =
    let f = function
      | Parsetree.Rtag (label, _, _, l) ->
        let i = Ppx_deriving.hash_variant label
        and f = Ast_helper.Exp.variant label in
        recognize_case_of_constructor i l
      | Rinherit {ptyp_desc = Ptyp_constr ({txt}, _)} ->
        let pc_lhs = [%pat? x]
        and pc_guard =
          let e =
            Ppx_deriving.mangle_lid (`Suffix "recognize") txt |>
            Location.mknoloc |>
            Ast_helper.Exp.ident
          in
          Some [%expr [%e e] x]
        and pc_rhs = [%expr true] in
        {Parsetree.pc_lhs; pc_guard; pc_rhs}
      | _ ->
        Location.raise_errorf ~loc
          "%s_recognize cannot be derived" deriver
    and default =
      let pc_lhs = [%pat? _]
      and pc_guard = None
      and pc_rhs = [%expr false] in
      {Parsetree.pc_lhs; pc_guard; pc_rhs}
    in
    List.map f l @ [default]
  in
  Ast_helper.Exp.function_ l

let rec read_of_poly_variant ?decl l y ~loc =
  let f = function
    | Parsetree.Rtag (label, _, _, l) ->
      let i = Ppx_deriving.hash_variant label
      and f = Ast_helper.Exp.variant label in
      read_case_of_constructor ?decl i f l
    | Rinherit ({ptyp_desc = Ptyp_constr ({txt}, _)} as y') ->
      let pc_lhs = [%pat? x]
      and pc_guard =
        let e =
          Ppx_deriving.mangle_lid (`Suffix "recognize") txt |>
          Location.mknoloc |>
          Ast_helper.Exp.ident
        in
        Some [%expr [%e e] x]
      and pc_rhs =
        let e = read_body_of_type ?decl y' in
        [%expr ([%e e] :> [%t y])]
      in
      {Parsetree.pc_lhs; pc_guard; pc_rhs}
  and default =
    let pc_lhs = [%pat? _]
    and pc_guard = None
    and pc_rhs =
      (* FIXME: typename *)
      [%expr Deriving_Json_lexer.tag_error ~typename:"" buf]
    in
    {Parsetree.pc_lhs; pc_guard; pc_rhs}
  in [%expr
    fun buf ->
      [%e List.map f l @ [default] |> Ast_helper.Exp.function_]
  ]

and read_tuple_contents ?decl l ~f =
  let n = List.length l in
  let lv = fresh_vars n in
  let f v y acc =
    let e = read_body_of_type ?decl y in [%expr
      Deriving_Json_lexer.read_comma buf;
      let [%p Ast_convenience.pvar v] = [%e e] in
      [%e acc]
    ]
  and acc = List.map Ast_convenience.evar lv |> f in
  let acc = [%expr Deriving_Json_lexer.read_rbracket buf; [%e acc]] in
  List.fold_right2 f lv l acc

and read_body_of_tuple_type ?decl l = [%expr
  Deriving_Json_lexer.read_lbracket buf;
  ignore (Deriving_Json_lexer.read_tag_1 0 buf);
  [%e read_tuple_contents ?decl l ~f:Ast_helper.Exp.tuple]
]

and read_of_record l =
  check_record_fields l;
  let e =
    let f =
      let f {Parsetree.pld_name = {txt}} e =
        (Longident.Lident txt |> Location.mknoloc), e
      in
      fun l' -> Ast_helper.Exp.record (List.map2 f l l') None
    and l =
      let f {Parsetree.pld_type} = pld_type in
      List.map f l
    in
    read_tuple_contents l ~f
  in [%expr
    Deriving_Json_lexer.read_lbracket buf;
    ignore (Deriving_Json_lexer.read_tag_2 0 254 buf);
    [%e e]
  ] |> wrap_read

and read_body_of_type ?decl y =
  let poly = match decl with Some _ -> true | _ -> false in
  match y with
  | [%type: unit] ->
    [%expr Deriving_Json.Json_unit.read buf]
  | [%type: int] ->
    [%expr Deriving_Json.Json_int.read buf]
  | [%type: int32] | [%type: Int32.t] ->
    [%expr Deriving_Json.Json_int32.read buf]
  | [%type: int64] | [%type: Int64.t] ->
    [%expr Deriving_Json.Json_int64.read buf]
  | [%type: nativeint] | [%type: Nativeint.t] ->
    [%expr Deriving_Json.Json_nativeint.read buf]
  | [%type: float] ->
    [%expr Deriving_Json.Json_float.read buf]
  | [%type: bool] ->
    [%expr Deriving_Json.Json_bool.read buf]
  | [%type: char] ->
    [%expr Deriving_Json.Json_char.read buf]
  | [%type: string] ->
    [%expr Deriving_Json.Json_string.read buf]
  | [%type: bytes] ->
    [%expr Deriving_Json.Json_bytes.read buf]
  | [%type: [%t? y] list] ->
    let e = [%expr [%e read_of_type ?decl y]] in
    [%expr Deriving_Json.read_list [%e e] buf]
  | [%type: [%t? y] ref] ->
    let e = [%expr [%e read_of_type ?decl y]] in
    [%expr Deriving_Json.read_ref [%e e] buf]
  | [%type: [%t? y] option] ->
    let e = [%expr [%e read_of_type ?decl y]] in
    [%expr Deriving_Json.read_option [%e e] buf]
  | [%type: [%t? y] array] ->
    let e = [%expr [%e read_of_type ?decl y]] in
    [%expr Deriving_Json.read_array [%e e] buf]
  | { Parsetree.ptyp_desc = Ptyp_tuple l } ->
    read_body_of_tuple_type l ?decl
  | { Parsetree.ptyp_desc = Ptyp_variant (l, _, _); ptyp_loc = loc } ->
    let e =
      (match decl with
       | Some decl ->
         let e =
           Ppx_deriving.mangle_type_decl
             (`Suffix "of_json_with_tag") decl |>
           Ast_convenience.evar
         and l =
           let {Parsetree.ptype_params = l} = decl
           and f (y, _) = read_of_type y ~decl in
           List.map f l
         in
         Ast_convenience.app e l
       | None ->
         read_of_poly_variant l y ~loc)
    and tag = [%expr Deriving_Json_lexer.read_vcase buf] in
    [%expr [%e e] buf [%e tag]]
  | { Parsetree.ptyp_desc = Ptyp_var v } when poly ->
    [%expr [%e Ast_convenience.evar ("poly_" ^ v)] buf]
  | { Parsetree.ptyp_desc = Ptyp_constr ({Asttypes.txt}, l) } ->
    let e =
      Ppx_deriving.mangle_lid (`Suffix "of_json") txt |>
      Location.mknoloc |>
      Ast_helper.Exp.ident
    and l = List.map (read_of_type ?decl) l in
    [%expr [%e Ast_convenience.app e l] buf]
  | { Parsetree.ptyp_loc } ->
    Location.raise_errorf ~loc:ptyp_loc
      "%s_read cannot be derived for %s" deriver
      (Ppx_deriving.string_of_core_type y)

and read_case_of_constructor ?decl i f l =
  let pc_lhs =
    let p = Ast_helper.Pat.constant (Asttypes.Const_int i) in
    match l with
    | [] ->
      [%pat? `Cst [%p p]]
    | _ ->
      [%pat? `NCst [%p p]]
  and pc_guard = None
  and pc_rhs =
    let f l =
      let e =
        match l with
        | [] ->  None
        | [e] -> Some e
        | l ->   Some (Ast_helper.Exp.tuple l)
      in
      f e
    in
    read_tuple_contents ?decl l ~f
  in
  {Parsetree.pc_lhs; pc_guard; pc_rhs}

and read_of_variant l ~decl =
  (let l =
     let f i { Parsetree.pcd_name; pcd_args; pcd_loc = loc } =
       let f =
         let label = label_of_constructor pcd_name in
         Ast_helper.Exp.construct label
       in
       read_case_of_constructor i f pcd_args ~decl
     and default =
       let pc_lhs = [%pat? _]
       and pc_guard = None
       and pc_rhs =
         (* FIXME: typename *)
         [%expr Deriving_Json_lexer.tag_error ~typename:"" buf]
       in
       {Parsetree.pc_lhs; pc_guard; pc_rhs}
     in
     List.mapi f l @ [default]
   and e = [%expr Deriving_Json_lexer.read_case buf] in
   Ast_helper.Exp.match_ e l) |> wrap_read

and read_of_type ?decl y =
  match json_attr y.Parsetree.ptyp_attributes with
  | Some fn ->
    fn
  | None ->
    wrap_read (read_body_of_type ?decl y)

let json_of_type ?decl y =
  match json_attr y.Parsetree.ptyp_attributes with
  | Some fn ->
    fn
  | None ->
    let read = read_of_type ?decl y
    and write =
      let poly = match decl with Some _ -> true | _ -> false in
      write_of_type y ~poly in
    [%expr Deriving_Json.make [%e write] [%e read]]

let fun_str_wrap d e y ~f ~suffix =
  let e = Ppx_deriving.poly_fun_of_type_decl d e |> sanitize
  and v =
    Ppx_deriving.mangle_type_decl (`Suffix suffix) d |>
    Ast_convenience.pvar
  and y = Ppx_deriving.poly_arrow_of_type_decl f d y in
  Ast_helper.(Vb.mk (Pat.constraint_ v y) e)

let read_str_wrap d e =
  let f y = [%type: Deriving_Json_lexer.lexbuf -> [%t y]]
  and suffix = "of_json" in
  let y = f (Ppx_deriving.core_type_of_type_decl d)in
  fun_str_wrap d e y ~f ~suffix

let read_tag_str_wrap d e =
  let f y = [%type: Deriving_Json_lexer.lexbuf -> [%t y]]
  and suffix = "of_json_with_tag"
  and y =
    let y = Ppx_deriving.core_type_of_type_decl d in
    [%type: Deriving_Json_lexer.lexbuf ->
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
          Some {ptyp_desc = Parsetree.Ptyp_variant (_, _, _)}} ->
       [%type: [> [%t y]]]
     | _ ->
       y) |> f
  and write = true in
  fun_str_wrap d e y ~f ~suffix

let recognize_str_wrap d e =
  let v =
    Ppx_deriving.mangle_type_decl (`Suffix "recognize") d |>
    Ast_convenience.pvar
  and y = [%type: [`NCst of int | `Cst of int] -> bool] in
  Ast_helper.(Vb.mk (Pat.constraint_ v y) e)

let json_of_variant l ~decl =
  let read = read_of_variant l ~decl
  and write = write_of_variant l in
  [%expr Deriving_Json.make [%e write] [%e read]]

let json_of_record l =
  let read = read_of_record l and write = write_of_record l in
  [%expr Deriving_Json.make [%e write] [%e read]]

let json_poly_type d =
  let f y = [%type: [%t y] Deriving_Json.t] in
  let y = f (Ppx_deriving.core_type_of_type_decl d) in
  Ppx_deriving.poly_arrow_of_type_decl f d y

let json_str_wrap d e =
  let v =
    Ppx_deriving.mangle_type_decl (`Suffix "json") d |>
    Ast_convenience.pvar
  and e = Ppx_deriving.(poly_fun_of_type_decl d e)
  and y = json_poly_type d in
  Ast_helper.(Vb.mk (Pat.constraint_ v y) e)

let json_str d =
  let write =
    let f acc id =
      let poly = Ast_convenience.evar ("poly_" ^ id) in
      [%expr [%e acc] (Deriving_Json.write [%e poly])]
    and acc =
      Ppx_deriving.mangle_type_decl (`Suffix "to_json") d |>
      Ast_convenience.evar
    in
    Ppx_deriving.fold_left_type_decl f acc d
  and read =
    let f acc id =
      let poly = Ast_convenience.evar ("poly_" ^ id) in
      [%expr [%e acc] (Deriving_Json.read [%e poly])]
    and acc =
      Ppx_deriving.mangle_type_decl (`Suffix "of_json") d |>
      Ast_convenience.evar
    in
    Ppx_deriving.fold_left_type_decl f acc d
  in
  [%expr Deriving_Json.make [%e write] [%e read]] |>
  json_str_wrap d

let write_decl_of_type d y =
  (let e =
     let arg = Ast_convenience.evar "a" in
     write_body_of_type y ~arg ~poly:true
   in
   [%expr fun buf a -> [%e e]]) |> write_str_wrap d

let read_decl_of_type decl y =
  (let e = read_body_of_type y ~decl in
   [%expr fun buf -> [%e e]]) |> read_str_wrap decl

let json_decls_of_type decl y =
  let recognize, read_tag =
    match y with
    | { Parsetree.ptyp_desc = Ptyp_variant (l, _, _);
        ptyp_loc = loc } ->
      Some (recognize_body_of_poly_variant l ~loc |>
            recognize_str_wrap decl),
      Some (read_of_poly_variant l y ~decl ~loc |>
            read_tag_str_wrap decl)
    | _ ->
      None, None
  in
  write_decl_of_type decl y,
  read_decl_of_type decl y,
  json_str decl,
  recognize, read_tag

let write_decl_of_variant d l =
  write_of_variant l |> write_str_wrap d

let read_decl_of_variant decl l =
  read_of_variant l ~decl |> read_str_wrap decl

let json_decls_of_variant d l =
  write_decl_of_variant d l, read_decl_of_variant d l, json_str d,
  None, None

let write_decl_of_record d l =
  write_of_record l |> write_str_wrap d

let read_decl_of_record d l =
  read_of_record l |> read_str_wrap d

let json_decls_of_record d l =
  write_decl_of_record d l, read_decl_of_record d l, json_str d,
  None, None

let json_str_of_decl d =
  match d with
  | { Parsetree.ptype_manifest = Some y } ->
    json_decls_of_type d y
  | { ptype_kind = Ptype_variant l } ->
    json_decls_of_variant d l
  | { ptype_kind = Ptype_record l } ->
    json_decls_of_record d l
  | _ ->
    Location.raise_errorf "%s cannot be derived" deriver

let _ =
  let core_type y =
    (let r = read_of_type y
     and x = [%expr
       Deriving_Json_lexer.init_lexer (Lexing.from_string s)
     ] in
     [%expr fun s -> [%e r] [%e x]]) |>
    sanitize
  in
  Ppx_deriving.(create "of_json" ~core_type () |> register)

let _ =
  let core_type y =
    (let e = write_of_type y ~poly:false in [%expr
       fun x ->
         let buf = Buffer.create 50 in
         [%e e] buf x;
         Buffer.contents buf
     ]) |>
    sanitize
  in
  Ppx_deriving.(create "to_json" ~core_type () |> register)

let _ =
  let core_type y = json_of_type y |> sanitize
  and type_decl_str ~options ~path l =
    let lw, lr, lj, lp, lrv =
      let f d (lw, lr, lj, lp, lrv) =
        let w, r, j, p, rv = json_str_of_decl d in
        w :: lw, r :: lr, j :: lj,
        (match p with Some p -> p :: lp | None -> lp),
        (match rv with Some rv -> rv :: lrv | None -> lrv)
      and acc = [], [], [], [], [] in
      List.fold_right f l acc
    and f l = Ast_helper.Str.value Asttypes.Recursive l
    and f' l = Ast_helper.Str.value Asttypes.Nonrecursive l in
    [f lp; f (lrv @ lr); f lw; f' lj]
  in
  Ppx_deriving.(create "json" ~core_type ~type_decl_str () |> register)

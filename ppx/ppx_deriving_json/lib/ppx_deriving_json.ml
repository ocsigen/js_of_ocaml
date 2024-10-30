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

open Ppxlib
open StdLabels
open Ppxlib.Ast
open Ppxlib.Ast_helper
open Ppxlib.Parsetree

let nolabel = Nolabel

let unflatten l =
  match l with
  | [] -> None
  | hd :: tl ->
      Some
        (List.fold_left
           ~f:(fun p s -> Longident.Ldot (p, s))
           ~init:(Longident.Lident hd)
           tl)

let rec split_at_dots s pos =
  try
    let dot = String.index_from s pos '.' in
    String.sub s ~pos ~len:(dot - pos) :: split_at_dots s (dot + 1)
  with Not_found -> [ String.sub s ~pos ~len:(String.length s - pos) ]

let parse_lid s =
  let components = split_at_dots s 0 in
  let assert_lid =
    String.iteri ~f:(fun i c ->
        match i, c with
        | 0, ('a' .. 'z' | '_') -> ()
        | 0, _ -> assert false
        | _, ('a' .. 'z' | 'A' .. 'Z' | '_' | '\'' | '0' .. '9') -> ()
        | _ -> assert false)
  in
  let assert_uid =
    String.iteri ~f:(fun i c ->
        match i, c with
        | 0, 'A' .. 'Z' -> ()
        | 0, _ -> assert false
        | _, ('a' .. 'z' | 'A' .. 'Z' | '_' | '\'' | '0' .. '9') -> ()
        | _ -> assert false)
  in
  let rec check = function
    | [] -> assert false
    | "" :: _ -> assert false
    | [ s ] -> assert_lid s
    | modul :: rest ->
        assert_uid modul;
        check rest
  in
  check components;
  match unflatten components with
  | None -> assert false
  | Some v -> v

let mkloc txt loc = { txt; loc }

let str ?loc ?attrs s = Exp.constant ?loc ?attrs (Const.string s)

let int ?loc ?attrs x = Exp.constant ?loc ?attrs (Const.int x)

let pint ?loc ?attrs x = Pat.constant ?loc ?attrs (Const.int x)

let lid ?(loc = !default_loc) s = mkloc (parse_lid s) loc

let pvar ?(loc = !default_loc) ?attrs s = Pat.var ~loc ?attrs (mkloc s loc)

let evar ?loc ?attrs s = Exp.ident ?loc ?attrs (lid ?loc s)

let tconstr ?loc ?attrs c l = Typ.constr ?loc ?attrs (lid ?loc c) l

let app ?loc ?attrs f l =
  if l = [] then f else Exp.apply ?loc ?attrs f (List.map ~f:(fun a -> nolabel, a) l)

let loc = Location.none

let mangle ?(fixpoint = "t") affix name =
  match name = fixpoint, affix with
  | true, (`Prefix x | `Suffix x) -> x
  | true, `PrefixSuffix (p, s) -> p ^ "_" ^ s
  | false, `PrefixSuffix (p, s) -> p ^ "_" ^ name ^ "_" ^ s
  | false, `Prefix x -> x ^ "_" ^ name
  | false, `Suffix x -> name ^ "_" ^ x

let mangle_type_decl ?fixpoint affix { ptype_name = { txt = name; _ }; _ } =
  mangle ?fixpoint affix name

let mangle_lid ?fixpoint affix lid : Longident.t =
  match (lid : Longident.t) with
  | Lident s -> Lident (mangle ?fixpoint affix s)
  | Ldot (p, s) -> Ldot (p, mangle ?fixpoint affix s)
  | Lapply _ -> assert false

let var_name_of_int i =
  let letter = "abcdefghijklmnopqrstuvwxyz" in
  let rec loop i =
    if i < 26 then [ letter.[i] ] else letter.[i mod 26] :: loop (i / 26)
  in
  String.concat ~sep:"" (List.map ~f:(String.make 1) (loop i))

let fresh_var bound =
  let rec loop i =
    let var_name = var_name_of_int i in
    if List.mem var_name ~set:bound then loop (i + 1) else var_name
  in
  loop 0

let string_of_core_type typ : string =
  let typ = { typ with ptyp_attributes = [] } in
  Format.asprintf "%a" Ppxlib.Pprintast.core_type typ

let core_type_of_type_decl { ptype_name = name; ptype_params; _ } =
  let name = mkloc (Longident.Lident name.txt) name.loc in
  Typ.constr name (List.map ~f:fst ptype_params)

let fold_right_type_params fn params accum =
  List.fold_right
    ~f:(fun (param, _) accum ->
      match param with
      | { ptyp_desc = Ptyp_any; _ } -> accum
      | { ptyp_desc = Ptyp_var name; _ } ->
          let name = mkloc name param.ptyp_loc in
          fn name accum
      | _ -> assert false)
    params
    ~init:accum

let fold_right_type_decl fn { ptype_params; _ } accum =
  fold_right_type_params fn ptype_params accum

let fold_left_type_params fn accum params =
  List.fold_left
    ~f:(fun accum (param, _) ->
      match param with
      | { ptyp_desc = Ptyp_any; _ } -> accum
      | { ptyp_desc = Ptyp_var name; _ } ->
          let name = mkloc name param.ptyp_loc in
          fn accum name
      | _ -> assert false)
    ~init:accum
    params

let fold_left_type_decl fn accum { ptype_params; _ } =
  fold_left_type_params fn accum ptype_params

let poly_arrow_of_type_decl fn type_decl typ =
  fold_right_type_decl
    (fun name typ ->
      let name = name.txt in
      Typ.arrow nolabel (fn (Typ.var name)) typ)
    type_decl
    typ

let poly_fun_of_type_decl type_decl expr =
  fold_right_type_decl
    (fun name expr ->
      let name = name.txt in
      Exp.fun_ nolabel None (pvar ("poly_" ^ name)) expr)
    type_decl
    expr

let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := (223 * !accu) + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land ((1 lsl 31) - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let deriver = "json"

let runtimename = "Deriving_Json"

let rt name = evar (Printf.sprintf "%s.%s" runtimename name)

let rt_t arg = tconstr (Printf.sprintf "%s.t" runtimename) [ arg ]

let lexer_ident name = Printf.sprintf "%s_lexer.%s" runtimename name

let lexbuf_t = tconstr (lexer_ident "lexbuf") []

let lexer name = evar (lexer_ident name)

let var_ptuple l = List.map ~f:pvar l |> Pat.tuple

let map_loc f { Location.txt; loc } = { Location.txt = f txt; loc }

let suffix_lid { Location.txt; loc } ~suffix =
  let txt = mangle_lid (`Suffix suffix) txt in
  Exp.ident { txt; loc } ~loc

let suffix_decl ({ Parsetree.ptype_loc = loc; _ } as d) ~suffix =
  (let s = mangle_type_decl (`Suffix suffix) d |> parse_lid in
   mkloc s loc)
  |> Exp.ident ~loc

let suffix_decl_p ({ Parsetree.ptype_loc = loc; _ } as d) ~suffix =
  (let s = mangle_type_decl (`Suffix suffix) d in
   mkloc s loc)
  |> Pat.var ~loc

let rec fresh_vars ?(acc = []) n =
  if n <= 0
  then List.rev acc
  else
    let acc = fresh_var acc :: acc in
    fresh_vars ~acc (n - 1)

let label_of_constructor = map_loc (fun c -> Longident.Lident c)

let wrap_write r ~pattern = [%expr fun buf [%p pattern] -> [%e r]]

let buf_expand r = [%expr fun buf -> [%e r]]

let seqlist = function
  | h :: l ->
      let f acc e =
        [%expr
          [%e acc];
          [%e e]]
      in
      List.fold_left ~f ~init:h l
  | [] -> [%expr ()]

let check_record_fields =
  List.iter ~f:(function
    | { pld_type = { ptyp_desc = Ptyp_poly _; _ }; _ } ->
        Location.raise_errorf "%s cannot be derived for polymorphic records" deriver
    | _ -> ())

let maybe_tuple_type = function
  | [ y ] -> y
  | l -> Ast_helper.Typ.tuple l

let pattern_of_record l =
  let l =
    let f { Parsetree.pld_name; _ } = label_of_constructor pld_name, Pat.var pld_name in
    List.map ~f l
  in
  Pat.record l Asttypes.Closed

let rec write_tuple_contents l ly ~tag ~poly =
  let e =
    let f arg y =
      let e = write_body_of_type y ~arg ~poly in
      [%expr
        Buffer.add_string buf ",";
        [%e e]]
    in
    List.map2 ~f l ly |> seqlist
  and s = str ("[" ^ string_of_int tag) in
  [%expr
    Buffer.add_string buf [%e s];
    [%e e];
    Buffer.add_string buf "]"]

and write_body_of_tuple_type l ~arg ~poly ~tag =
  let n = List.length l in
  let vars = fresh_vars n in
  let e = write_tuple_contents vars l ~tag ~poly and p = var_ptuple vars in
  [%expr
    let [%p p] = [%e arg] in
    [%e e]]

and write_poly_case r ~arg ~poly =
  match r.prf_desc with
  | Parsetree.Rtag ({ txt = label; _ }, _, l) ->
      let i = hash_variant label and n = List.length l in
      let v = fresh_var [] in
      let lhs = (if n = 0 then None else Some (pvar v)) |> Pat.variant label
      and rhs =
        match l with
        | [] ->
            let e = int i in
            [%expr [%e rt "Json_int.write"] buf [%e e]]
        | _ ->
            let l = [ [%type: int]; maybe_tuple_type l ]
            and arg = Exp.tuple [ int i; evar v ] in
            write_body_of_tuple_type l ~arg ~poly ~tag:0
      in
      Exp.case lhs rhs
  | Rinherit ({ ptyp_desc = Ptyp_constr (lid, _); ptyp_loc; _ } as y) ->
      Exp.case
        (Pat.(alias (type_ lid)) (mkloc arg ptyp_loc))
        (write_body_of_type y ~arg ~poly)
  | Rinherit { ptyp_loc; _ } ->
      Location.raise_errorf ~loc:ptyp_loc "%s write case cannot be derived" deriver

and write_body_of_type y ~(arg : string) ~poly =
  let arg = evar arg and arg' = arg in
  match y with
  | [%type: unit] -> [%expr [%e rt "Json_unit.write"] buf [%e arg]]
  | [%type: int] -> [%expr [%e rt "Json_int.write"] buf [%e arg]]
  | [%type: int32] | [%type: Int32.t] -> [%expr [%e rt "Json_int32.write"] buf [%e arg]]
  | [%type: int64] | [%type: Int64.t] -> [%expr [%e rt "Json_int64.write"] buf [%e arg]]
  | [%type: nativeint] | [%type: Nativeint.t] ->
      [%expr [%e rt "Json_nativeint.write"] buf [%e arg]]
  | [%type: float] -> [%expr [%e rt "Json_float.write"] buf [%e arg]]
  | [%type: bool] -> [%expr [%e rt "Json_bool.write"] buf [%e arg]]
  | [%type: char] -> [%expr [%e rt "Json_char.write"] buf [%e arg]]
  | [%type: string] -> [%expr [%e rt "Json_string.write"] buf [%e arg]]
  | [%type: bytes] -> [%expr [%e rt "Json_bytes.write"] buf [%e arg]]
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
  | { Parsetree.ptyp_desc = Ptyp_var v; _ } when poly ->
      [%expr [%e evar ("poly_" ^ v)] buf [%e arg]]
  | { Parsetree.ptyp_desc = Ptyp_tuple l; _ } ->
      write_body_of_tuple_type l ~arg ~poly ~tag:0
  | { Parsetree.ptyp_desc = Ptyp_variant (l, _, _); _ } ->
      Exp.match_ arg (List.map ~f:(write_poly_case ~arg:arg' ~poly) l)
  | { Parsetree.ptyp_desc = Ptyp_constr (lid, l); _ } ->
      let e = suffix_lid lid ~suffix:"to_json"
      and l = List.map ~f:(write_of_type ~poly) l in
      [%expr [%e app e l] buf [%e arg]]
  | { Parsetree.ptyp_loc; _ } ->
      Location.raise_errorf
        ~loc:ptyp_loc
        "%s_write cannot be derived for %s"
        deriver
        (string_of_core_type y)

and write_of_type y ~poly =
  let arg = "a" in
  let pattern = pvar arg in
  wrap_write (write_body_of_type y ~arg ~poly) ~pattern

and write_body_of_record ~tag l =
  let l =
    let f { Parsetree.pld_name = { txt; _ }; _ } = txt in
    List.map ~f l
  and ly =
    let f { Parsetree.pld_type; _ } = pld_type in
    List.map ~f l
  in
  write_tuple_contents l ly ~tag ~poly:true

and write_of_record ?(tag = 0) _d l =
  let pattern = pattern_of_record l and e = write_body_of_record ~tag l in
  wrap_write e ~pattern

let recognize_case_of_constructor i l =
  let lhs =
    match l with
    | [] -> [%pat? `Cst [%p pint i]]
    | _ -> [%pat? `NCst [%p pint i]]
  in
  Exp.case lhs [%expr true]

let recognize_body_of_poly_variant l ~loc =
  let l =
    let f x =
      match x.prf_desc with
      | Parsetree.Rtag ({ txt = label; _ }, _, l) ->
          let i = hash_variant label in
          recognize_case_of_constructor i l
      | Rinherit { ptyp_desc = Ptyp_constr (lid, _); _ } ->
          let guard = [%expr [%e suffix_lid lid ~suffix:"recognize"] x] in
          Exp.case ~guard [%pat? x] [%expr true]
      | _ -> Location.raise_errorf ~loc "%s_recognize cannot be derived" deriver
    and default = Exp.case [%pat? _] [%expr false] in
    List.map ~f l @ [ default ]
  in
  Exp.function_ l

let tag_error_case ?(typename = "") () =
  let y = str typename in
  Exp.case [%pat? _] [%expr [%e lexer "tag_error"] ~typename:[%e y] buf]

let maybe_tuple_type = function
  | [ y ] -> y
  | l -> Ast_helper.Typ.tuple l

let rec read_poly_case ?decl y x =
  match x.prf_desc with
  | Parsetree.Rtag ({ txt = label; _ }, _, l) -> (
      let i = hash_variant label |> pint in
      match l with
      | [] -> Exp.case [%pat? `Cst [%p i]] (Exp.variant label None)
      | l ->
          Exp.case
            [%pat? `NCst [%p i]]
            [%expr
              [%e lexer "read_comma"] buf;
              let v = [%e read_body_of_type ?decl (maybe_tuple_type l)] in
              [%e lexer "read_rbracket"] buf;
              [%e Exp.variant label (Some [%expr v])]])
  | Rinherit { ptyp_desc = Ptyp_constr (lid, l); _ } ->
      let guard = [%expr [%e suffix_lid lid ~suffix:"recognize"] x]
      and e =
        let e = suffix_lid lid ~suffix:"of_json_with_tag"
        and l = List.map ~f:(read_of_type ?decl) l in
        [%expr ([%e app e l] buf x :> [%t y])]
      in
      Exp.case ~guard [%pat? x] e
  | Rinherit { ptyp_loc; _ } ->
      Location.raise_errorf ~loc:ptyp_loc "%s read case cannot be derived" deriver

and read_of_poly_variant ?decl l y ~loc:_ =
  List.map ~f:(read_poly_case ?decl y) l @ [ tag_error_case () ]
  |> Exp.function_
  |> buf_expand

and read_tuple_contents ?decl l ~f =
  let n = List.length l in
  let lv = fresh_vars n in
  let f v y acc =
    let e = read_body_of_type ?decl y in
    [%expr
      [%e lexer "read_comma"] buf;
      let [%p pvar v] = [%e e] in
      [%e acc]]
  and acc = List.map ~f:evar lv |> f in
  let acc =
    [%expr
      [%e lexer "read_rbracket"] buf;
      [%e acc]]
  in
  List.fold_right2 ~f lv l ~init:acc

and read_body_of_tuple_type ?decl l =
  [%expr
    [%e lexer "read_lbracket"] buf;
    ignore ([%e lexer "read_tag_1"] 0 buf);
    [%e read_tuple_contents ?decl l ~f:Exp.tuple]]

and read_of_record_raw ?decl ?(return = fun x -> x) l =
  let f =
    let f { Parsetree.pld_name; _ } e = label_of_constructor pld_name, e in
    fun l' -> return (Exp.record (List.map2 ~f l l') None)
  and l =
    let f { Parsetree.pld_type; _ } = pld_type in
    List.map ~f l
  in
  read_tuple_contents l ?decl ~f

and read_of_record decl l =
  let e = read_of_record_raw ~decl l in
  [%expr
    [%e lexer "read_lbracket"] buf;
    ignore ([%e lexer "read_tag_2"] 0 254 buf);
    [%e e]]
  |> buf_expand

and read_body_of_type ?decl y =
  let poly =
    match decl with
    | Some _ -> true
    | _ -> false
  in
  match y with
  | [%type: unit] -> [%expr [%e rt "Json_unit.read"] buf]
  | [%type: int] -> [%expr [%e rt "Json_int.read"] buf]
  | [%type: int32] | [%type: Int32.t] -> [%expr [%e rt "Json_int32.read"] buf]
  | [%type: int64] | [%type: Int64.t] -> [%expr [%e rt "Json_int64.read"] buf]
  | [%type: nativeint] | [%type: Nativeint.t] -> [%expr [%e rt "Json_nativeint.read"] buf]
  | [%type: float] -> [%expr [%e rt "Json_float.read"] buf]
  | [%type: bool] -> [%expr [%e rt "Json_bool.read"] buf]
  | [%type: char] -> [%expr [%e rt "Json_char.read"] buf]
  | [%type: string] -> [%expr [%e rt "Json_string.read"] buf]
  | [%type: bytes] -> [%expr [%e rt "Json_bytes.read"] buf]
  | [%type: [%t? y] list] -> [%expr [%e rt "read_list"] [%e read_of_type ?decl y] buf]
  | [%type: [%t? y] ref] -> [%expr [%e rt "read_ref"] [%e read_of_type ?decl y] buf]
  | [%type: [%t? y] option] -> [%expr [%e rt "read_option"] [%e read_of_type ?decl y] buf]
  | [%type: [%t? y] array] -> [%expr [%e rt "read_array"] [%e read_of_type ?decl y] buf]
  | { Parsetree.ptyp_desc = Ptyp_tuple l; _ } -> read_body_of_tuple_type l ?decl
  | { Parsetree.ptyp_desc = Ptyp_variant (l, _, _); ptyp_loc = loc; _ } ->
      let e =
        match decl with
        | Some ({ ptype_manifest = Some typ; _ } as decl) when typ = y ->
            let e = suffix_decl decl ~suffix:"of_json_with_tag"
            and l =
              let { Parsetree.ptype_params = l; _ } = decl
              and f (y, _) = read_of_type y ~decl in
              List.map ~f l
            in
            app e l
        | Some _ | None -> read_of_poly_variant l y ~loc
      and tag = [%expr [%e lexer "read_vcase"] buf] in
      [%expr [%e e] buf [%e tag]]
  | { Parsetree.ptyp_desc = Ptyp_var v; _ } when poly ->
      [%expr [%e evar ("poly_" ^ v)] buf]
  | { Parsetree.ptyp_desc = Ptyp_constr (lid, l); _ } ->
      let e = suffix_lid lid ~suffix:"of_json"
      and l = List.map ~f:(read_of_type ?decl) l in
      [%expr [%e app e l] buf]
  | { Parsetree.ptyp_loc; _ } ->
      Location.raise_errorf
        ~loc:ptyp_loc
        "%s_read cannot be derived for %s"
        deriver
        (string_of_core_type y)

and read_of_type ?decl y = read_body_of_type ?decl y |> buf_expand

let json_of_type ?decl y =
  let read = read_of_type ?decl y
  and write =
    let poly =
      match decl with
      | Some _ -> true
      | _ -> false
    in
    write_of_type y ~poly
  in
  [%expr [%e rt "make"] [%e write] [%e read]]

let fun_str_wrap d e y ~f ~suffix =
  let e = poly_fun_of_type_decl d e
  and v = suffix_decl_p d ~suffix
  and y = poly_arrow_of_type_decl f d y in
  Ast_helper.(Vb.mk (Pat.constraint_ v y) e)

let read_str_wrap d e =
  let f y = [%type: [%t lexbuf_t] -> [%t y]] and suffix = "of_json" in
  let y = f (core_type_of_type_decl d) in
  fun_str_wrap d e y ~f ~suffix

let read_tag_str_wrap d e =
  let f y = [%type: [%t lexbuf_t] -> [%t y]]
  and suffix = "of_json_with_tag"
  and y =
    let y = core_type_of_type_decl d in
    [%type: [%t lexbuf_t] -> [ `NCst of int | `Cst of int ] -> [%t y]]
  in
  fun_str_wrap d e y ~f ~suffix

let write_str_wrap d e =
  let f y = [%type: Buffer.t -> [%t y] -> unit] and suffix = "to_json" in
  let y =
    let y = core_type_of_type_decl d in
    (match d with
    | { ptype_manifest = Some { ptyp_desc = Parsetree.Ptyp_variant (_, _, _); _ }; _ } ->
        [%type: [> [%t y] ]]
    | _ -> y)
    |> f
  in
  fun_str_wrap d e y ~f ~suffix

let recognize_str_wrap d e =
  let v = suffix_decl_p d ~suffix:"recognize"
  and y = [%type: [ `NCst of int | `Cst of int ] -> bool] in
  Ast_helper.(Vb.mk (Pat.constraint_ v y) e)

let json_poly_type d =
  let f y = rt_t y in
  let y = f (core_type_of_type_decl d) in
  poly_arrow_of_type_decl f d y

let json_str_wrap d e =
  let v = suffix_decl_p d ~suffix:"json"
  and e = poly_fun_of_type_decl d e
  and y = json_poly_type d in
  Ast_helper.(Vb.mk (Pat.constraint_ v y) e)

let json_str d =
  let write =
    let f acc id =
      let id = id.Location.txt in
      let poly = evar ("poly_" ^ id) in
      [%expr [%e acc] ([%e rt "write"] [%e poly])]
    and acc = suffix_decl d ~suffix:"to_json" in
    fold_left_type_decl f acc d
  and read =
    let f acc id =
      let id = id.Location.txt in
      let poly = evar ("poly_" ^ id) in
      [%expr [%e acc] ([%e rt "read"] [%e poly])]
    and acc = suffix_decl d ~suffix:"of_json" in
    fold_left_type_decl f acc d
  in
  [%expr [%e rt "make"] [%e write] [%e read]] |> json_str_wrap d

let write_decl_of_type d y =
  (let e = write_body_of_type y ~arg:"a" ~poly:true in
   [%expr fun buf a -> [%e e]])
  |> write_str_wrap d

let read_decl_of_type decl y =
  read_body_of_type y ~decl |> buf_expand |> read_str_wrap decl

let json_decls_of_type decl y =
  let recognize, read_tag =
    match y with
    | { Parsetree.ptyp_desc = Ptyp_variant (l, _, _); ptyp_loc = loc; _ } ->
        ( Some (recognize_body_of_poly_variant l ~loc |> recognize_str_wrap decl)
        , Some (read_of_poly_variant l y ~decl ~loc |> read_tag_str_wrap decl) )
    | _ -> None, None
  in
  write_decl_of_type decl y, read_decl_of_type decl y, json_str decl, recognize, read_tag

let write_case (i, i', l) { Parsetree.pcd_name; pcd_args; _ } =
  let i, i', lhs, rhs =
    match pcd_args with
    | Pcstr_tuple [] | Pcstr_record [] ->
        i + 1, i', None, [%expr [%e rt "Json_int.write"] buf [%e int i]]
    | Pcstr_tuple ([ _ ] as args) ->
        let v = fresh_var [] in
        i, i' + 1, Some (pvar v), write_tuple_contents [ v ] args ~tag:i' ~poly:true
    | Pcstr_tuple args ->
        let vars = fresh_vars (List.length args) in
        ( i
        , i' + 1
        , Some (var_ptuple vars)
        , write_tuple_contents vars args ~tag:i' ~poly:true )
    | Pcstr_record args ->
        i, i' + 1, Some (pattern_of_record args), write_body_of_record args ~tag:i'
  in
  ( i
  , i'
  , Ast_helper.(Exp.case (Pat.construct (label_of_constructor pcd_name) lhs) rhs) :: l )

let write_decl_of_variant d l =
  (let _, _, l = List.fold_left ~f:write_case ~init:(0, 0, []) l in
   Exp.function_ l)
  |> buf_expand
  |> write_str_wrap d

let read_case ?decl (i, i', l) { Parsetree.pcd_name; pcd_args; _ } =
  let f l =
    Exp.construct
      (label_of_constructor pcd_name)
      (match l with
      | [] -> None
      | [ e ] -> Some e
      | l -> Some (Exp.tuple l))
  in
  match pcd_args with
  | Pcstr_tuple [] | Pcstr_record [] ->
      ( i + 1
      , i'
      , Exp.case
          [%pat? `Cst [%p pint i]]
          (Exp.construct (label_of_constructor pcd_name) None)
        :: l )
  | Pcstr_tuple pcd_args ->
      let expr = read_tuple_contents ?decl pcd_args ~f in
      let case = Exp.case [%pat? `NCst [%p pint i']] expr in
      i, i' + 1, case :: l
  | Pcstr_record pcd_args ->
      let patt = [%pat? `NCst [%p pint i']]
      and expr =
        let return e = Exp.construct (label_of_constructor pcd_name) (Some e) in
        read_of_record_raw ?decl pcd_args ~return
      in
      i, i' + 1, Exp.case patt expr :: l

let read_decl_of_variant decl l =
  (let _, _, l = List.fold_left ~f:(read_case ~decl) ~init:(0, 0, []) l
   and e = [%expr [%e lexer "read_case"] buf] in
   Exp.match_ e (l @ [ tag_error_case () ]))
  |> buf_expand
  |> read_str_wrap decl

let json_decls_of_variant d l =
  write_decl_of_variant d l, read_decl_of_variant d l, json_str d, None, None

let write_decl_of_record d l = write_of_record d l |> write_str_wrap d

let read_decl_of_record d l = read_of_record d l |> read_str_wrap d

let json_decls_of_record d l =
  check_record_fields l;
  write_decl_of_record d l, read_decl_of_record d l, json_str d, None, None

let json_str_of_decl ({ Parsetree.ptype_loc; _ } as d) =
  Ast_helper.with_default_loc ptype_loc
  @@ fun () ->
  match d with
  | { Parsetree.ptype_kind = Ptype_variant l; _ } -> json_decls_of_variant d l
  | { ptype_kind = Ptype_record l; _ } -> json_decls_of_record d l
  | { ptype_manifest = Some y; _ } -> json_decls_of_type d y
  | _ ->
      Location.raise_errorf
        "%s cannot be derived for %s"
        deriver
        (mangle_type_decl (`Suffix "") d)

let read_sig_of_decl ({ Parsetree.ptype_loc; _ } as d) =
  (let s =
     let s = mangle_type_decl (`Suffix "of_json") d in
     mkloc s ptype_loc
   and y =
     let f y = [%type: [%t lexbuf_t] -> [%t y]] in
     let y = f (core_type_of_type_decl d) in
     poly_arrow_of_type_decl f d y
   in
   Ast_helper.Val.mk s y)
  |> Ast_helper.Sig.value

let recognize_sig_of_decl ({ Parsetree.ptype_loc; _ } as d) =
  (let s =
     let s = mangle_type_decl (`Suffix "recognize") d in
     mkloc s ptype_loc
   and y = [%type: [ `NCst of int | `Cst of int ] -> bool] in
   Ast_helper.Val.mk s y)
  |> Ast_helper.Sig.value

let read_with_tag_sig_of_decl ({ Parsetree.ptype_loc; _ } as d) =
  (let s =
     let s = mangle_type_decl (`Suffix "of_json_with_tag") d in
     mkloc s ptype_loc
   and y =
     let f y = [%type: [%t lexbuf_t] -> [%t y]] in
     let y =
       let y = core_type_of_type_decl d in
       f [%type: [ `NCst of int | `Cst of int ] -> [%t y]]
     in
     poly_arrow_of_type_decl f d y
   in
   Ast_helper.Val.mk s y)
  |> Ast_helper.Sig.value

let write_sig_of_decl ({ Parsetree.ptype_loc; _ } as d) =
  (let s =
     let s = mangle_type_decl (`Suffix "to_json") d in
     mkloc s ptype_loc
   and y =
     let f y = [%type: Buffer.t -> [%t y] -> unit] in
     let y = f (core_type_of_type_decl d) in
     poly_arrow_of_type_decl f d y
   in
   Ast_helper.Val.mk s y)
  |> Ast_helper.Sig.value

let json_sig_of_decl ({ Parsetree.ptype_loc; _ } as d) =
  (let s =
     let s = mangle_type_decl (`Suffix "json") d in
     mkloc s ptype_loc
   and y =
     let f y = rt_t y in
     let y = f (core_type_of_type_decl d) in
     poly_arrow_of_type_decl f d y
   in
   Ast_helper.Val.mk s y)
  |> Ast_helper.Sig.value

let sigs_of_decl ({ Parsetree.ptype_loc; _ } as d) =
  Ast_helper.with_default_loc ptype_loc
  @@ fun () ->
  let l = [ read_sig_of_decl d; write_sig_of_decl d; json_sig_of_decl d ] in
  match d with
  | { Parsetree.ptype_manifest =
        Some { Parsetree.ptyp_desc = Parsetree.Ptyp_variant _; _ }
    ; _
    } -> read_with_tag_sig_of_decl d :: recognize_sig_of_decl d :: l
  | _ -> l

let core_type_exp ({ Parsetree.ptyp_loc; _ } as y) =
  let f () = json_of_type y in
  Ast_helper.with_default_loc ptyp_loc f

let type_decl_str ~loc:_ ~path:_ (_, l) =
  let lw, lr, lj, lp, lrv =
    let f d (lw, lr, lj, lp, lrv) =
      let w, r, j, p, rv = json_str_of_decl d in
      ( w :: lw
      , r :: lr
      , j :: lj
      , (match p with
        | Some p -> p :: lp
        | None -> lp)
      , match rv with
        | Some rv -> rv :: lrv
        | None -> lrv )
    and acc = [], [], [], [], [] in
    List.fold_right ~f l ~init:acc
  and f = Ast_helper.Str.value Asttypes.Recursive
  and f' = Ast_helper.Str.value Asttypes.Nonrecursive in
  let l = [ f (lrv @ lr); f lw; f' lj ] in
  match lp with
  | [] -> l
  | _ -> f lp :: l

let type_decl_sig ~loc:_ ~path:_ (_, l) = List.map ~f:sigs_of_decl l |> List.flatten

module Of_json = struct
  let name = "of_json"

  let extension ~loc ~path:_ ctyp =
    [%expr
      fun s -> [%e read_of_type ctyp] ([%e lexer "init_lexer"] (Lexing.from_string s))]

  let deriver = Ppxlib.Deriving.add name ~extension
end

module Json_of = struct
  let name = "json_of"

  let extension ~loc ~path:_ ctyp =
    [%expr
      fun x ->
        let buf = Buffer.create 50 in
        [%e write_of_type ctyp ~poly:false] buf x;
        Buffer.contents buf]

  let deriver = Ppxlib.Deriving.add name ~extension
end

module To_json = struct
  let name = "to_json"

  let extension ~loc ~path:_ ctyp =
    [%expr
      fun x ->
        let buf = Buffer.create 50 in
        [%e write_of_type ctyp ~poly:false] buf x;
        Buffer.contents buf]

  let deriver = Ppxlib.Deriving.add name ~extension
end

module Json = struct
  let name = "json"

  let str_type_decl = Ppxlib.Deriving.Generator.make_noarg type_decl_str

  let sig_type_decl = Ppxlib.Deriving.Generator.make_noarg type_decl_sig

  let extension ~loc:_ ~path:_ ctyp = core_type_exp ctyp

  let deriver = Ppxlib.Deriving.add name ~str_type_decl ~sig_type_decl ~extension
end

let json_of = Json_of.deriver

let to_json = To_json.deriver

let of_json = Of_json.deriver

let json = Json.deriver

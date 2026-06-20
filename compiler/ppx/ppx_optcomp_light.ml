(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
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

(** Minimal version of ppx_optcomp
    It only support the following attribute
    {[
      [@if ocaml_version < (4,12,0)]
    ]}
    on module (Pstr_module),
    toplevel bindings (Pstr_value, Pstr_primitive)
    toplevel extensions, e.g. [let%expect_test "..." = ... [@@if ...]]
    (Pstr_extension)
    pattern in case (pc_lhs)
    and module in signature (Psig_module)

    For toplevel extensions to be gated we must run before context-free
    rewriters such as ppx_expect expand them; this is achieved by
    registering the implementation transformation as a [Before]
    instrumentation (see registration at the end of the file).
*)

open StdLabels
open Ppxlib.Parsetree

module Version : sig
  type t

  val of_list : int list -> t

  val compare : t -> t -> int

  val current : t

  type extra_prefix =
    | Plus
    | Tilde

  val extra : (extra_prefix * string) option
end = struct
  type t = int list

  let of_list l = l

  let split_char ~sep p =
    let len = String.length p in
    let rec split beg cur =
      if cur >= len
      then if cur - beg > 0 then [ String.sub p ~pos:beg ~len:(cur - beg) ] else []
      else if sep p.[cur]
      then String.sub p ~pos:beg ~len:(cur - beg) :: split (cur + 1) (cur + 1)
      else split beg (cur + 1)
    in
    split 0 0

  let split v =
    match
      split_char
        ~sep:(function
          | '+' | '-' | '~' -> true
          | _ -> false)
        v
    with
    | [] -> assert false
    | x :: _ ->
        List.map
          (split_char
             ~sep:(function
               | '.' -> true
               | _ -> false)
             x)
          ~f:int_of_string

  let current = split Sys.ocaml_version

  let compint (a : int) b = compare a b

  let rec compare v v' =
    match v, v' with
    | [ x ], [ y ] -> compint x y
    | [], [] -> 0
    | [], y :: _ -> compint 0 y
    | x :: _, [] -> compint x 0
    | x :: xs, y :: ys -> (
        match compint x y with
        | 0 -> compare xs ys
        | n -> n)

  type extra_prefix =
    | Plus
    | Tilde

  type release_info = { extra : (extra_prefix * string) option }

  let extra =
    (* Sys.ocaml_release is only available since OCaml 4.14. For older
       version of OCaml, [ocaml_release.extra] will evaluate to
       [None]. *)
    let ocaml_release = { extra = None } in
    ignore ocaml_release.extra;
    match
      let open! Sys in
      ocaml_release.extra
    with
    | None -> None
    | Some (Plus, tag) -> Some (Plus, tag)
    | Some (Tilde, tag) -> Some (Tilde, tag)
end

exception Invalid of Location.t

type op =
  | LE
  | GE
  | GT
  | LT
  | NEQ
  | EQ
  | AND
  | OR
  | NOT

type e =
  | Version of Version.t
  | Tuple of e list
  | Bool of bool
  | Int of int
  | String of string

let get_bin_op = function
  | { pexp_desc = Pexp_ident { txt = Lident str; _ }; pexp_loc = loc; _ } -> (
      match str with
      | "<=" -> LE
      | ">=" -> GE
      | ">" -> GT
      | "<" -> LT
      | "=" -> EQ
      | "<>" -> NEQ
      | "&&" -> AND
      | "||" -> OR
      | _ -> raise (Invalid loc))
  | { pexp_loc = loc; _ } -> raise (Invalid loc)

let get_un_op = function
  | { pexp_desc = Pexp_ident { txt = Lident str; _ }; pexp_loc = loc; _ } -> (
      match str with
      | "not" -> NOT
      | _ -> raise (Invalid loc))
  | { pexp_loc = loc; _ } -> raise (Invalid loc)

let version = function
  | Version v -> v
  | Tuple l ->
      Version.of_list
        (List.map l ~f:(function
          | Int i -> i
          | _ -> raise (Invalid Location.none)))
  | Bool _ | Int _ | String _ -> raise (Invalid Location.none)

let keep loc (attrs : attributes) =
  let ifs =
    List.filter attrs ~f:(function
      | { attr_name = { txt = "if"; _ }; _ } -> true
      | _ -> false)
  in
  match ifs with
  | [] -> true
  | _ -> (
      try
        let keep_one ({ attr_payload; attr_loc; _ } as attr) =
          Ppxlib.Attribute.mark_as_handled_manually attr;
          let e =
            match attr_payload with
            | PStr [ { pstr_desc = Pstr_eval (e, []); _ } ] -> e
            | _ -> raise (Invalid attr_loc)
          in
          let rec eval e =
            let open Ppxlib.Ast_pattern in
            let loc = e.pexp_loc in
            match
              (parse_res
                 (pexp_ident (lident (string "ocaml_version"))
                 >>| (fun () -> Version Version.current)
                 ||| (pexp_ident (lident (string "ast_version"))
                     >>| fun () -> Int Ppxlib.Selected_ast.version)
                 ||| (pexp_ident (lident (string "oxcaml"))
                     >>| fun () ->
                     Bool
                       (match Version.extra with
                       | Some (Plus, "ox") -> true
                       | _ -> false))
                 ||| (pexp_construct (lident (string "true")) drop >>| fun () -> Bool true)
                 ||| (pexp_construct (lident (string "false")) drop
                     >>| fun () -> Bool false)
                 ||| (pexp_constant (pconst_integer __ none)
                     >>| fun () d -> Int (int_of_string d))
                 ||| (pexp_tuple __ >>| fun () l -> Tuple (List.map l ~f:eval))
                 ||| (pexp_apply __ __
                     >>| fun () op l ->
                     match l with
                     | [ (Nolabel, a); (Nolabel, b) ] -> (
                         let op = get_bin_op op in
                         let a = eval a in
                         let b = eval b in
                         match op with
                         | LE | GE | LT | GT | NEQ | EQ ->
                             let comp =
                               match a, b with
                               | Version _, _ | _, Version _ ->
                                   Version.compare (version a) (version b)
                               | Int a, Int b -> compare a b
                               | _ -> raise (Invalid loc)
                             in
                             let op =
                               match op with
                               | LE -> ( <= )
                               | GE -> ( >= )
                               | LT -> ( < )
                               | GT -> ( > )
                               | EQ -> ( = )
                               | NEQ -> ( <> )
                               | _ -> assert false
                             in
                             Bool (op comp 0)
                         | AND -> (
                             match a, b with
                             | Bool a, Bool b -> Bool (a && b)
                             | _ -> raise (Invalid loc))
                         | OR -> (
                             match a, b with
                             | Bool a, Bool b -> Bool (a || b)
                             | _ -> raise (Invalid loc))
                         | NOT -> raise (Invalid loc))
                     | [ (Nolabel, a) ] -> (
                         let op = get_un_op op in
                         let a = eval a in
                         match op, a with
                         | NOT, Bool b -> Bool (not b)
                         | NOT, _ -> raise (Invalid loc)
                         | _ -> raise (Invalid loc))
                     | _ -> raise (Invalid loc))))
                loc
                e
                ()
            with
            | Ok res -> res
            | Error _ -> raise (Invalid loc)
          in
          match eval e with
          | Bool b -> b
          | Int _ | String _ | Tuple _ | Version _ -> raise (Invalid loc)
        in
        let keep = List.for_all ~f:keep_one ifs in
        if false
        then
          if not keep
          then
            Printf.eprintf
              "dropping %s:%d\n%!"
              loc.Location.loc_start.pos_fname
              loc.Location.loc_start.pos_lnum
          else
            Printf.eprintf
              "keep %s:%d\n%!"
              loc.Location.loc_start.pos_fname
              loc.Location.loc_start.pos_lnum;
        keep
      with Invalid loc -> Location.raise_errorf ~loc "Invalid attribute format")

let filter_map ~f l =
  let l =
    List.fold_left
      ~f:(fun acc x ->
        match f x with
        | Some x -> x :: acc
        | None -> acc)
      ~init:[]
      l
  in
  List.rev l

let rec filter_pattern = function
  | { ppat_desc = Ppat_or (p1, p2); _ } as p -> (
      match filter_pattern p1, filter_pattern p2 with
      | None, None -> None
      | Some p1, None -> Some p1
      | None, Some p2 -> Some p2
      | Some p1, Some p2 -> Some { p with ppat_desc = Ppat_or (p1, p2) })
  | { ppat_attributes; ppat_loc; _ } as p ->
      if keep ppat_loc ppat_attributes then Some p else None

let drop_attr =
  let attr =
    { attr_name = Location.mknoloc "ppx_optcomp_light.dropped"
    ; attr_loc = Location.none
    ; attr_payload = PStr []
    }
  in
  Ppxlib.Attribute.mark_as_handled_manually attr;
  attr

let drop_str loc = { pstr_desc = Pstr_attribute drop_attr; pstr_loc = loc }

let drop_sig loc = { psig_desc = Psig_attribute drop_attr; psig_loc = loc }

let is_if_attr = function
  | { attr_name = { txt = "if"; _ }; _ } -> true
  | _ -> false

let strip_if attrs = List.filter attrs ~f:(fun a -> not (is_if_attr a))

(* Attributes that gate a toplevel extension item such as
   [let%expect_test "..." = ... [@@if ...]]. The parser attaches the [@@if]
   to the binding inside the extension payload, so we look there (and on the
   extension item itself) for the gating attributes. *)
let extension_if_attrs ((_, payload) : extension) ext_attrs =
  let inner =
    match payload with
    | PStr [ { pstr_desc = Pstr_value (_, vbs); _ } ] ->
        List.concat (List.map vbs ~f:(fun vb -> vb.pvb_attributes))
    | PStr [ { pstr_desc = Pstr_eval (_, attrs); _ } ] -> attrs
    | _ -> []
  in
  List.filter (ext_attrs @ inner) ~f:is_if_attr

(* Remove the [@if] attributes once they have been evaluated, so that the
   downstream rewriters (e.g. ppx_expect) do not choke on a leftover
   attribute in an unexpected position. *)
let strip_extension_if ((name, payload) : extension) ext_attrs =
  let payload =
    match payload with
    | PStr [ ({ pstr_desc = Pstr_value (r, vbs); _ } as item) ] ->
        let vbs =
          List.map vbs ~f:(fun vb ->
              { vb with pvb_attributes = strip_if vb.pvb_attributes })
        in
        PStr [ { item with pstr_desc = Pstr_value (r, vbs) } ]
    | other -> other
  in
  (name, payload), strip_if ext_attrs

(* A floating attribute [@@@if cond] gates the rest of the enclosing
   structure: when [cond] is false every following item is dropped, when it is
   true the marker itself is removed. This is convenient to gate a whole test
   module on an OCaml version without repeating [@@if] on each item. *)
let truncate_floating_str items =
  let rec loop = function
    | [] -> []
    | { pstr_desc = Pstr_attribute attr; pstr_loc; _ } :: rest when is_if_attr attr ->
        if keep pstr_loc [ attr ] then loop rest else []
    | item :: rest -> item :: loop rest
  in
  loop items

let traverse =
  object (self)
    inherit Ppxlib.Ast_traverse.map as super

    method! structure items = super#structure (truncate_floating_str items)

    method! structure_item item =
      (* Handle gated toplevel extensions (e.g. [let%expect_test]) before
         recursing, so the whole extension is dropped before context-free
         rewriters expand it. *)
      match item.pstr_desc with
      | Pstr_extension (ext, ext_attrs) -> (
          match extension_if_attrs ext ext_attrs with
          | [] -> self#structure_item_default item
          | attrs ->
              if keep item.pstr_loc attrs
              then
                let ext, ext_attrs = strip_extension_if ext ext_attrs in
                super#structure_item
                  { item with pstr_desc = Pstr_extension (ext, ext_attrs) }
              else drop_str item.pstr_loc)
      | _ -> self#structure_item_default item

    method private structure_item_default item =
      let item = super#structure_item item in
      match item.pstr_desc with
      | Pstr_module { pmb_attributes; pmb_loc; _ } ->
          if keep pmb_loc pmb_attributes then item else drop_str pmb_loc
      | Pstr_primitive { pval_attributes; pval_loc; _ } ->
          if keep pval_loc pval_attributes then item else drop_str pval_loc
      | Pstr_value (r, l) -> (
          let l =
            filter_map l ~f:(fun b ->
                if keep b.pvb_loc b.pvb_attributes then Some b else None)
          in
          match l with
          | [] -> drop_str Location.none
          | _ -> { item with pstr_desc = Pstr_value (r, l) })
      | _ -> item

    method! cases cases =
      let cases =
        filter_map cases ~f:(fun case ->
            match filter_pattern case.pc_lhs with
            | None -> None
            | Some pattern -> Some { case with pc_lhs = pattern })
      in
      super#cases cases

    method! signature_item item =
      let item = super#signature_item item in
      match item.psig_desc with
      | Psig_module { pmd_attributes; pmd_loc; _ } ->
          if keep pmd_loc pmd_attributes then item else drop_sig pmd_loc
      | _ -> item
  end

let () =
  Ppxlib.Driver.register_transformation
    ~instrument:
      (Ppxlib.Driver.Instrument.make
         traverse#structure
         ~position:Ppxlib.Driver.Instrument.Before)
    ~intf:traverse#signature
    "ppx_optcomp_light"

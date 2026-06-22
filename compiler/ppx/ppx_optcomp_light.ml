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
      [@if os_type <> "Win32"]
    ]}
    The supported predicates are [ocaml_version], [ast_version], [oxcaml]
    and [os_type] (a string, e.g. "Unix" / "Win32" / "Cygwin"), combined
    with the usual comparison and boolean operators (see
    {!Ppx_light_predicate.Predicate}). They can be placed
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
module Predicate = Ppx_light_predicate.Predicate

let keep _loc (attrs : attributes) =
  let ifs =
    List.filter attrs ~f:(function
      | { attr_name = { txt = "if"; _ }; _ } -> true
      | _ -> false)
  in
  match ifs with
  | [] -> true
  | _ -> (
      let keep_one ({ attr_payload; attr_loc; _ } as attr) =
        Ppxlib.Attribute.mark_as_handled_manually attr;
        let e =
          match attr_payload with
          | PStr [ { pstr_desc = Pstr_eval (e, []); _ } ] -> e
          | _ -> raise (Predicate.Invalid attr_loc)
        in
        Predicate.eval_compile_time (Predicate.parse e)
      in
      try List.for_all ~f:keep_one ifs
      with Predicate.Invalid loc -> Location.raise_errorf ~loc "Invalid attribute format")

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

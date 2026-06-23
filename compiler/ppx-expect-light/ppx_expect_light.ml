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

(** A small, self-contained reimplementation of the [ppx_expect] rewriter,
    paired with {!Ppx_expect_light_runtime}.

    It expands:
    - [let%expect_test "description" = body] (optionally [_] for no
      description, optionally carrying a [\[@tags "a", "b"\]] attribute) into a
      call to [Ppx_expect_light_runtime.register_test].
    - [%expect {| ... |}] / [%expect] assertion points into
      [Ppx_expect_light_runtime.check ~index]. Consecutive assertion points are
      coalesced into a single group of variants checked against the output
      captured since the previous group.
    - [%expect {| ... |} \[@when COND\]] conditional variants. [COND] is a
      predicate in the {!Ppx_light_predicate.Predicate} grammar; it is reified
      into a runtime test against {!Ppx_expect_light_runtime.Axes}. A plain
      [%expect] (no [\[@when\]]) is the default variant of its group.
    - [%expect.output] into [Ppx_expect_light_runtime.read_output ()], returning
      the output captured since the previous assertion point. *)

open StdLabels
open Ppxlib
module Predicate = Ppx_light_predicate.Predicate

let runtime = "Ppx_expect_light_runtime"

(* One variant of an assertion point, gathered from a single [%expect ...]. *)
type variant =
  { is_default : bool
  ; condition : expression option (* reified [@when] predicate, [bool]-valued *)
  ; expected : string
  ; payload_start : int
  ; payload_end : int
  }

let invalid ~loc fmt = Location.raise_errorf ~loc fmt

(* Pull the optional [@when COND] off the literal (or off the [Pstr_eval] item)
   and reify it into a [bool] expression evaluated against the runtime axes. *)
let when_condition ~expr_attrs ~eval_attrs =
  let find attrs =
    List.find_opt attrs ~f:(fun (a : attribute) -> String.equal a.attr_name.txt "when")
  in
  match
    match find expr_attrs with
    | Some _ as a -> a
    | None -> find eval_attrs
  with
  | None -> None
  | Some attr ->
      Attribute.mark_as_handled_manually attr;
      let loc = attr.attr_loc in
      let cond =
        match attr.attr_payload with
        | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> e
        | _ -> invalid ~loc "[@when] expects a single predicate expression"
      in
      let t =
        try Predicate.parse cond
        with Predicate.Invalid loc -> invalid ~loc "invalid [@when] predicate"
      in
      let e =
        try Predicate.reify ~loc:cond.pexp_loc t
        with Predicate.Invalid loc ->
          invalid ~loc "[@when] predicate is not meaningful at runtime"
      in
      Some e

(* Parse the payload of an [%expect ...] node into a {!variant}. *)
let variant_of_payload ~ext_loc (payload : payload) : variant =
  match payload with
  | PStr [] ->
      (* Bare [%expect]: no literal yet. Promotion will insert one just before
         the closing [\]] of the extension. *)
      let p = ext_loc.loc_end.pos_cnum - 1 in
      { is_default = true
      ; condition = None
      ; expected = ""
      ; payload_start = p
      ; payload_end = p
      }
  | PStr [ { pstr_desc = Pstr_eval (lit, eval_attrs); _ } ] -> (
      match lit.pexp_desc with
      | Pexp_constant (Pconst_string (s, _, _)) ->
          let condition = when_condition ~expr_attrs:lit.pexp_attributes ~eval_attrs in
          { is_default = Option.is_none condition
          ; condition
          ; expected = s
          ; payload_start = lit.pexp_loc.loc_start.pos_cnum
          ; payload_end = lit.pexp_loc.loc_end.pos_cnum
          }
      | _ -> invalid ~loc:lit.pexp_loc "[%%expect] expects a string literal payload")
  | _ -> invalid ~loc:ext_loc "[%%expect] expects a string literal payload"

type point =
  | Expect of variant
  | Output
  | Other

let classify (e : expression) : point =
  match e.pexp_desc with
  | Pexp_extension ({ txt = "expect"; _ }, payload) ->
      Expect (variant_of_payload ~ext_loc:e.pexp_loc payload)
  | Pexp_extension ({ txt = "expect.output"; loc }, payload) -> (
      match payload with
      | PStr [] -> Output
      | _ -> invalid ~loc "[%%expect.output] does not take a payload")
  | _ -> Other

let rec flatten_seq (e : expression) =
  match e.pexp_desc with
  | Pexp_sequence (a, b) -> a :: flatten_seq b
  | _ -> [ e ]

let build_seq ~loc = function
  | [] -> [%expr ()]
  | l ->
      let rec go = function
        | [] -> [%expr ()]
        | [ x ] -> x
        | x :: tl ->
            { (Ast_builder.Default.esequence ~loc [ x; go tl ]) with pexp_loc = loc }
      in
      go l

(* Rewrite a test body, allocating a group per run of adjacent assertion points
   and replacing each [%expect ...]/[%expect.output] with the matching runtime
   call. Returns the rewritten body and the groups in index order. *)
let transform_body (body : expression) : expression * variant list array =
  let groups = ref [] in
  let count = ref 0 in
  let alloc_group variants =
    let idx = !count in
    incr count;
    groups := variants :: !groups;
    idx
  in
  let check ~loc idx =
    [%expr
      [%e Ast_builder.Default.evar ~loc (runtime ^ ".check")]
        ~index:[%e Ast_builder.Default.eint ~loc idx]]
  in
  let mapper =
    object (self)
      inherit Ast_traverse.map as super

      method! expression e =
        match e.pexp_desc with
        | Pexp_sequence _ -> self#sequence e
        | _ -> (
            match classify e with
            | Expect v ->
                let loc = e.pexp_loc in
                check ~loc (alloc_group [ v ])
            | Output ->
                let loc = e.pexp_loc in
                [%expr [%e Ast_builder.Default.evar ~loc (runtime ^ ".read_output")] ()]
            | Other -> super#expression e)

      method private sequence e =
        let loc = e.pexp_loc in
        let stmts = flatten_seq e in
        let rec go = function
          | [] -> []
          | stmt :: rest -> (
              match classify stmt with
              | Expect v ->
                  let run, rest = gather_run [ v ] rest in
                  check ~loc:stmt.pexp_loc (alloc_group run) :: go rest
              | Output | Other -> self#expression stmt :: go rest)
        and gather_run acc = function
          | stmt :: rest -> (
              match classify stmt with
              | Expect v -> gather_run (v :: acc) rest
              | Output | Other -> List.rev acc, stmt :: rest)
          | [] -> List.rev acc, []
        in
        build_seq ~loc (go stmts)
    end
  in
  let body = mapper#expression body in
  body, Array.of_list (List.rev !groups)

(* -- Code generation for [register_test] ----------------------------------- *)

let estring ~loc s = Ast_builder.Default.estring ~loc s

let evariant ~loc (v : variant) : expression =
  let condition =
    match v.condition with
    | Some c -> [%expr fun () -> [%e c]]
    | None -> [%expr fun () -> false]
  in
  [%expr
    { Ppx_expect_light_runtime.condition = [%e condition]
    ; is_default = [%e Ast_builder.Default.ebool ~loc v.is_default]
    ; expected = [%e estring ~loc v.expected]
    ; payload_start = [%e Ast_builder.Default.eint ~loc v.payload_start]
    ; payload_end = [%e Ast_builder.Default.eint ~loc v.payload_end]
    }]

let eexpectations ~loc (groups : variant list array) : expression =
  Ast_builder.Default.pexp_array
    ~loc
    (Array.to_list
       (Array.map groups ~f:(fun variants ->
            Ast_builder.Default.pexp_array ~loc (List.map variants ~f:(evariant ~loc)))))

(* [@tags "a", "b"] (or [@tags "a"]) on the test description. *)
let tags_of_attributes (attrs : attribute list) : string list =
  match
    List.find_opt attrs ~f:(fun (a : attribute) -> String.equal a.attr_name.txt "tags")
  with
  | None -> []
  | Some attr -> (
      Attribute.mark_as_handled_manually attr;
      let loc = attr.attr_loc in
      let one (e : expression) =
        match e.pexp_desc with
        | Pexp_constant (Pconst_string (s, _, _)) -> s
        | _ -> invalid ~loc "[@tags] expects string literals"
      in
      match attr.attr_payload with
      | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> (
          (* A tuple [@tags "a", "b"] is read via [Ast_pattern.pexp_tuple]:
             OxCaml's ppxlib changes the raw [Pexp_tuple] payload, but the
             pattern exposes a portable [expression list]. *)
          match
            Ast_pattern.parse_res Ast_pattern.(pexp_tuple __) e.pexp_loc e (fun l -> l)
          with
          | Ok l -> List.map l ~f:one
          | Error _ -> [ one e ])
      | _ -> invalid ~loc "[@tags] expects string literals")

(* [@when COND] on the test description gates whether the test runs at all,
   using the same predicate grammar as [%expect ... [@when]] (a runtime check,
   so it can mention backend/engine which [@@if] cannot). *)
let when_of_attributes (attrs : attribute list) : expression option =
  match
    List.find_opt attrs ~f:(fun (a : attribute) -> String.equal a.attr_name.txt "when")
  with
  | None -> None
  | Some attr ->
      Attribute.mark_as_handled_manually attr;
      let loc = attr.attr_loc in
      let cond =
        match attr.attr_payload with
        | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> e
        | _ -> invalid ~loc "[@when] expects a single predicate expression"
      in
      let t =
        try Predicate.parse cond
        with Predicate.Invalid loc -> invalid ~loc "invalid [@when] predicate"
      in
      Some
        (try Predicate.reify ~loc:cond.pexp_loc t
         with Predicate.Invalid loc ->
           invalid ~loc "[@when] predicate is not meaningful at runtime")

(* dune passes the library name being preprocessed as the [library-name]
   ppxlib cookie; record it so each test knows which library it belongs to. *)
let library_name = ref ""

let () =
  Driver.Cookies.add_simple_handler
    "library-name"
    Ast_pattern.(estring __)
    ~f:(function
      | Some name -> library_name := name
      | None -> ())

let expand_test ~loc (vb : value_binding) : structure_item =
  let description, tags =
    match vb.pvb_pat.ppat_desc with
    | Ppat_constant (Pconst_string (s, _, _)) ->
        s, tags_of_attributes vb.pvb_pat.ppat_attributes
    | Ppat_any -> "", tags_of_attributes vb.pvb_pat.ppat_attributes
    | _ -> invalid ~loc:vb.pvb_pat.ppat_loc "let%%expect_test expects a string or _ name"
  in
  let pos = vb.pvb_loc.loc_start in
  let filename = pos.pos_fname in
  let line_number = pos.pos_lnum in
  (* Character offsets of the test on its starting line, like ppx_inline_test,
     so the [-verbose] location reads identically. *)
  let start_pos = pos.pos_cnum - pos.pos_bol in
  let end_pos = vb.pvb_loc.loc_end.pos_cnum - pos.pos_bol in
  let body, groups = transform_body vb.pvb_expr in
  let expectations = eexpectations ~loc groups in
  let tags = Ast_builder.Default.elist ~loc (List.map tags ~f:(estring ~loc)) in
  let condition =
    match when_of_attributes vb.pvb_pat.ppat_attributes with
    | Some c -> [%expr fun () -> [%e c]]
    | None -> [%expr fun () -> true]
  in
  [%stri
    let () =
      Ppx_expect_light_runtime.register_test
        ~lib:[%e estring ~loc !library_name]
        ~filename:[%e estring ~loc filename]
        ~description:[%e estring ~loc description]
        ~line_number:[%e Ast_builder.Default.eint ~loc line_number]
        ~start_pos:[%e Ast_builder.Default.eint ~loc start_pos]
        ~end_pos:[%e Ast_builder.Default.eint ~loc end_pos]
        ~tags:[%e tags]
        ~condition:[%e condition]
        ~expectations:[%e expectations]
        (fun () -> [%e body])]

let impl_mapper =
  object
    inherit Ast_traverse.map as super

    method! structure_item item =
      match item.pstr_desc with
      | Pstr_extension (({ txt = "expect_test"; _ }, payload), _) -> (
          match payload with
          | PStr [ { pstr_desc = Pstr_value (Nonrecursive, [ vb ]); _ } ] ->
              expand_test ~loc:item.pstr_loc vb
          | _ -> invalid ~loc:item.pstr_loc "let%%expect_test expects a single binding")
      | _ -> super#structure_item item
  end

let () =
  Driver.register_transformation "ppx_expect_light" ~impl:(fun str ->
      impl_mapper#structure str)

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

open StdLabels
open Ppxlib

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

(* The parsed grammar: a generic boolean expression over atoms. Resolution of
   the atoms (and which ones are meaningful) is deferred to the interpreters
   [eval_compile_time] and [reify]. *)
type t =
  | Ident of Location.t * string
  | Bool of Location.t * bool
  | Int of Location.t * int
  | String of Location.t * string
  | Tuple of Location.t * t list
  | App of Location.t * string * t list

let loc_of = function
  | Ident (loc, _)
  | Bool (loc, _)
  | Int (loc, _)
  | String (loc, _)
  | Tuple (loc, _)
  | App (loc, _, _) -> loc

let known_op = function
  | "<=" | ">=" | ">" | "<" | "=" | "<>" | "&&" | "||" | "not" -> true
  | _ -> false

let op_name (e : expression) =
  match e.pexp_desc with
  | Pexp_ident { txt = Lident s; _ } when known_op s -> s
  | _ -> raise (Invalid e.pexp_loc)

(* [Ppxlib.Parsetree] is pinned to a fixed AST version, so matching it directly
   is simple — except for [Pexp_tuple]: OxCaml's ppxlib gives its payload
   per-element labels ([(string option * expression) list]), which this single
   source can't pattern-match portably. [Ast_pattern.pexp_tuple] exposes the
   unlabeled [expression list] on both, so tuples are routed through it. *)
let rec parse (e : expression) : t =
  let loc = e.pexp_loc in
  match e.pexp_desc with
  | Pexp_ident { txt = Lident s; _ } -> Ident (loc, s)
  | Pexp_construct ({ txt = Lident "true"; _ }, None) -> Bool (loc, true)
  | Pexp_construct ({ txt = Lident "false"; _ }, None) -> Bool (loc, false)
  | Pexp_constant (Pconst_integer (d, None)) -> Int (loc, int_of_string d)
  | Pexp_constant (Pconst_string (s, _, _)) -> String (loc, s)
  | Pexp_apply (op, args) ->
      let op = op_name op in
      let args =
        List.map args ~f:(function
          | Nolabel, e -> parse e
          | (Labelled _ | Optional _), _ -> raise (Invalid loc))
      in
      App (loc, op, args)
  | _ -> (
      match Ast_pattern.parse_res Ast_pattern.(pexp_tuple __) loc e (fun l -> l) with
      | Ok l -> Tuple (loc, List.map l ~f:parse)
      | Error _ -> raise (Invalid loc))

(* -- Compile-time evaluation ----------------------------------------------- *)

type value =
  | V_version of Version.t
  | V_bool of bool
  | V_int of int
  | V_string of string
  | V_tuple of value list

let version_of_value loc = function
  | V_version v -> v
  | V_tuple l ->
      Version.of_list
        (List.map l ~f:(function
          | V_int i -> i
          | _ -> raise (Invalid loc)))
  | V_bool _ | V_int _ | V_string _ -> raise (Invalid loc)

let rec eval_value (t : t) : value =
  match t with
  | Ident (_, "ocaml_version") -> V_version Version.current
  | Ident (_, "ast_version") -> V_int Ppxlib.Selected_ast.version
  | Ident (_, "oxcaml") ->
      V_bool
        (match Version.extra with
        | Some (Plus, "ox") -> true
        | _ -> false)
  | Ident (_, "os_type") -> V_string Sys.os_type
  | Ident (_, "arch_sixtyfour") -> V_bool (Sys.word_size = 64)
  | Ident (_, "introspect") ->
      let introspection_enabled = false in
      ignore introspection_enabled;
      let open! Sys in
      V_bool introspection_enabled
  | Ident (loc, _) -> raise (Invalid loc)
  | Bool (_, b) -> V_bool b
  | Int (_, i) -> V_int i
  | String (_, s) -> V_string s
  | Tuple (_, l) -> V_tuple (List.map l ~f:eval_value)
  | App (loc, op, args) -> eval_app loc op args

and eval_app loc op args =
  match op, args with
  | "not", [ a ] -> (
      match eval_value a with
      | V_bool b -> V_bool (not b)
      | _ -> raise (Invalid loc))
  | "&&", [ a; b ] -> (
      match eval_value a, eval_value b with
      | V_bool a, V_bool b -> V_bool (a && b)
      | _ -> raise (Invalid loc))
  | "||", [ a; b ] -> (
      match eval_value a, eval_value b with
      | V_bool a, V_bool b -> V_bool (a || b)
      | _ -> raise (Invalid loc))
  | ("<=" | ">=" | ">" | "<" | "=" | "<>"), [ a; b ] ->
      let a = eval_value a in
      let b = eval_value b in
      let comp =
        match a, b with
        | (V_version _ | V_tuple _), _ | _, (V_version _ | V_tuple _) ->
            Version.compare (version_of_value loc a) (version_of_value loc b)
        | V_int a, V_int b -> compare a b
        | V_string a, V_string b -> compare a b
        | _ -> raise (Invalid loc)
      in
      let f =
        match op with
        | "<=" -> ( <= )
        | ">=" -> ( >= )
        | "<" -> ( < )
        | ">" -> ( > )
        | "=" -> ( = )
        | "<>" -> ( <> )
        | _ -> assert false
      in
      V_bool (f comp 0)
  | _ -> raise (Invalid loc)

let eval_compile_time t =
  match eval_value t with
  | V_bool b -> b
  | _ -> raise (Invalid (loc_of t))

(* -- Reification to runtime code ------------------------------------------- *)

(* Atoms that are only meaningful at compile time. *)
let compile_time_only = function
  | "ast_version" | "arch_sixtyfour" | "introspect" -> true
  | _ -> false

(* Every other identifier is an axis: [resolve] maps it to the fully qualified
   name of its runtime value. Its type ([bool], [string], [int] or [int list])
   follows from the context it is used in. *)
let axis ~resolve (t : t) : expression =
  match t with
  | Ident (loc, name) when not (compile_time_only name) -> (
      match resolve name with
      | Some path -> Ast_builder.Default.evar ~loc path
      | None -> raise (Invalid loc))
  | _ -> raise (Invalid (loc_of t))

let reify_string ~resolve (t : t) : expression =
  let loc = loc_of t in
  match t with
  | String (_, s) -> Ast_builder.Default.estring ~loc s
  | _ -> axis ~resolve t

let reify_int ~resolve (t : t) : expression =
  let loc = loc_of t in
  match t with
  | Int (_, i) -> Ast_builder.Default.eint ~loc i
  | _ -> axis ~resolve t

let reify_version ~resolve (t : t) : expression =
  let loc = loc_of t in
  match t with
  | Tuple (_, l) -> Ast_builder.Default.elist ~loc (List.map l ~f:(reify_int ~resolve))
  | _ -> axis ~resolve t

(* The type of a comparison is given by its literal side. *)
type kind =
  | K_version
  | K_string
  | K_int
  | K_unknown

let kind_of (t : t) : kind =
  match t with
  | Tuple _ -> K_version
  | String _ -> K_string
  | Int _ -> K_int
  | _ -> K_unknown

let rel_to_int loc op (a : expression) (b : expression) : expression =
  (* [a] and [b] are int-valued expressions; combine with the comparison. *)
  match op with
  | "<=" -> [%expr [%e a] <= [%e b]]
  | ">=" -> [%expr [%e a] >= [%e b]]
  | "<" -> [%expr [%e a] < [%e b]]
  | ">" -> [%expr [%e a] > [%e b]]
  | "=" -> [%expr [%e a] = [%e b]]
  | "<>" -> [%expr [%e a] <> [%e b]]
  | _ -> raise (Invalid loc)

let rec reify_bool ~resolve (t : t) : expression =
  let loc = loc_of t in
  match t with
  | Bool (_, b) -> Ast_builder.Default.ebool ~loc b
  | Ident _ -> axis ~resolve t
  | App (loc, "not", [ a ]) -> [%expr not [%e reify_bool ~resolve a]]
  | App (loc, "&&", [ a; b ]) ->
      [%expr [%e reify_bool ~resolve a] && [%e reify_bool ~resolve b]]
  | App (loc, "||", [ a; b ]) ->
      [%expr [%e reify_bool ~resolve a] || [%e reify_bool ~resolve b]]
  | App (loc, (("<=" | ">=" | ">" | "<" | "=" | "<>") as op), [ a; b ]) ->
      reify_cmp ~resolve loc op a b
  | _ -> raise (Invalid loc)

and reify_cmp ~resolve loc op a b =
  let kind =
    match kind_of a, kind_of b with
    | K_unknown, k | k, K_unknown -> k
    | k, _ -> k
  in
  match kind with
  | K_version ->
      let cmp =
        [%expr
          Ppx_devtools_expect_runtime.version_compare
            [%e reify_version ~resolve a]
            [%e reify_version ~resolve b]]
      in
      rel_to_int loc op cmp [%expr 0]
  | K_string ->
      (* Fully qualify: the test module may [open] something that shadows
         [String] (e.g. js_of_ocaml's [Typed_array]). *)
      let cmp =
        [%expr
          Stdlib.String.compare [%e reify_string ~resolve a] [%e reify_string ~resolve b]]
      in
      rel_to_int loc op cmp [%expr 0]
  | K_int -> rel_to_int loc op (reify_int ~resolve a) (reify_int ~resolve b)
  | K_unknown -> raise (Invalid loc)

let reify ~resolve ~loc t =
  let e = reify_bool ~resolve t in
  { e with pexp_loc = loc }

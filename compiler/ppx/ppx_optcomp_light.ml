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
    and pattern in case (pc_lhs)
*)

open StdLabels
open Ppxlib.Parsetree

module Version : sig
  type t

  val of_list : int list -> t

  val compare : t -> t -> int

  val current : t
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
        let keep_one { attr_payload; attr_loc; _ } =
          let e =
            match attr_payload with
            | PStr [ { pstr_desc = Pstr_eval (e, []); _ } ] -> e
            | _ -> raise (Invalid attr_loc)
          in
          let loc = e.pexp_loc in
          let rec eval = function
            | { pexp_desc = Pexp_ident { txt = Lident "ocaml_version"; _ }; _ } ->
                Version Version.current
            | { pexp_desc = Pexp_ident { txt = Lident "ast_version"; _ }; _ } ->
                Int Ppxlib.Selected_ast.version
            | { pexp_desc = Pexp_construct ({ txt = Lident "true"; _ }, None); _ } ->
                Bool true
            | { pexp_desc = Pexp_construct ({ txt = Lident "false"; _ }, None); _ } ->
                Bool false
            | { pexp_desc = Pexp_constant (Pconst_integer (d, None)); _ } ->
                Int (int_of_string d)
            | { pexp_desc = Pexp_tuple l; _ } -> Tuple (List.map l ~f:eval)
            | { pexp_desc = Pexp_apply (op, [ (Nolabel, a); (Nolabel, b) ]); pexp_loc; _ }
              -> (
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
                      | _ -> raise (Invalid pexp_loc)
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
            | { pexp_desc = Pexp_apply (op, [ (Nolabel, a) ]); _ } -> (
                let op = get_un_op op in
                let a = eval a in
                match op, a with
                | NOT, Bool b -> Bool (not b)
                | NOT, _ -> raise (Invalid loc)
                | _ -> raise (Invalid loc))
            | _ -> raise (Invalid loc)
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

let traverse =
  object
    inherit Ppxlib.Ast_traverse.map as super

    method! structure items =
      let items =
        filter_map items ~f:(fun item ->
            match item.pstr_desc with
            | Pstr_module { pmb_attributes; pmb_loc; _ } ->
                if keep pmb_loc pmb_attributes then Some item else None
            | Pstr_primitive { pval_attributes; pval_loc; _ } ->
                if keep pval_loc pval_attributes then Some item else None
            | Pstr_value (r, l) -> (
                let l =
                  filter_map l ~f:(fun b ->
                      if keep b.pvb_loc b.pvb_attributes then Some b else None)
                in
                match l with
                | [] -> None
                | _ -> Some { item with pstr_desc = Pstr_value (r, l) })
            | _ -> Some item)
      in
      super#structure items

    method! cases cases =
      let cases =
        filter_map cases ~f:(fun case ->
            match filter_pattern case.pc_lhs with
            | None -> None
            | Some pattern -> Some { case with pc_lhs = pattern })
      in
      super#cases cases
  end

let () =
  Ppxlib.Driver.register_transformation ~impl:traverse#structure "ppx_optcomp_light"

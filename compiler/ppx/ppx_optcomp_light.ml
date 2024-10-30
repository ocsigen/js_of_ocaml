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

(** Minimal version of ppx_optcomp It only support the following attribute
    {[
      [@if ocaml_version < (4,12,0)]
    ]}
    on module (Pstr_module), toplevel bindings (Pstr_value, Pstr_primitive) and pattern in
    case (pc_lhs) *)

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

exception Invalid

let get_env s =
  match Properties.get s with
  | None -> Sys.getenv s
  | Some p -> p

let keep loc (attrs : attributes) =
  try
    let keep =
      List.for_all attrs ~f:(function
        | { attr_name = { txt = ("if" | "ifnot") as ifnot; _ }; attr_payload; _ } -> (
            let norm =
              match ifnot with
              | "if" -> fun x -> x
              | "ifnot" -> fun x -> not x
              | _ -> assert false
            in
            match attr_payload with
            | PStr
                [ { pstr_desc =
                      Pstr_eval
                        ( { pexp_desc = Pexp_construct ({ txt = Lident ident; _ }, None)
                          ; _
                          }
                        , [] )
                  ; _
                  }
                ] ->
                let b =
                  match bool_of_string (get_env ident) with
                  | true -> true
                  | false -> false
                  | exception _ -> false
                in
                norm b
            | PStr
                [ { pstr_desc =
                      Pstr_eval
                        ( { pexp_desc = Pexp_apply (op, [ (Nolabel, a); (Nolabel, b) ])
                          ; _
                          }
                        , [] )
                  ; _
                  }
                ] ->
                let get_op = function
                  | { pexp_desc = Pexp_ident { txt = Lident str; _ }; _ } -> (
                      match str with
                      | "<=" -> ( <= )
                      | ">=" -> ( >= )
                      | ">" -> ( > )
                      | "<" -> ( < )
                      | "<>" -> ( <> )
                      | "=" -> ( = )
                      | _ -> raise Invalid)
                  | _ -> raise Invalid
                in
                let eval = function
                  | { pexp_desc = Pexp_ident { txt = Lident "ocaml_version"; _ }; _ } ->
                      Version.current
                  | { pexp_desc = Pexp_tuple l; _ } ->
                      let l =
                        List.map l ~f:(function
                          | { pexp_desc = Pexp_constant (Pconst_integer (d, None)); _ } ->
                              int_of_string d
                          | _ -> raise Invalid)
                      in
                      Version.of_list l
                  | _ -> raise Invalid
                in
                let op = get_op op in
                let a = eval a in
                let b = eval b in
                norm (op (Version.compare a b) 0)
            | _ -> raise Invalid)
        | _ -> true)
    in
    if false && not keep
    then
      Printf.eprintf
        "dropping %s:%d\n%!"
        loc.Location.loc_start.pos_fname
        loc.Location.loc_start.pos_lnum;
    keep
  with Invalid -> Location.raise_errorf ~loc "Invalid attribute format"

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

(* TODO: This class is useful while we transition to ppxlib.0.17 that provides the `cases`
   method. Remove this once we drop support for ppxlib < 0.17 *)
class map =
  object (self)
    inherit Ppxlib.Ast_traverse.map as super

    method cases = self#list self#case [@@ocaml.warning "-7"]

    method expression_desc : expression_desc -> expression_desc =
      fun x ->
        match x with
        | Pexp_function a ->
            let a = self#cases a in
            Pexp_function a
        | Pexp_match (a, b) ->
            let a = self#expression a in
            let b = self#cases b in
            Pexp_match (a, b)
        | Pexp_try (a, b) ->
            let a = self#expression a in
            let b = self#cases b in
            Pexp_try (a, b)
        | _ -> super#expression_desc x
    [@@ocaml.warning "-7"]
  end

let traverse =
  object
    inherit map as super

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

    method! cases =
      filter_map ~f:(fun case ->
          match filter_pattern case.pc_lhs with
          | None -> None
          | Some pattern -> Some { case with pc_lhs = pattern })
  end

let () =
  Ppxlib.Driver.register_transformation ~impl:traverse#structure "ppx_optcomp_light"

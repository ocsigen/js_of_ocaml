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
      [@if ocaml_version < (4,8,0)]
    ]}
    on module (Pstr_module) and pattern in case (pc_lhs)
*)

open StdLabels
open Migrate_parsetree
open OCaml_407.Ast
open Parsetree

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
      then if cur - beg > 0 then [String.sub p ~pos:beg ~len:(cur - beg)] else []
      else if p.[cur] = sep
      then String.sub p ~pos:beg ~len:(cur - beg) :: split (cur + 1) (cur + 1)
      else split beg (cur + 1)
    in
    split 0 0

  let split v =
    match split_char ~sep:'+' v with
    | [] -> assert false
    | x :: _ -> List.map (split_char ~sep:'.' x) ~f:int_of_string

  let current = split Sys.ocaml_version

  let compint (a : int) b = compare a b

  let rec compare v v' =
    match v, v' with
    | [x], [y] -> compint x y
    | [], [] -> 0
    | [], y :: _ -> compint 0 y
    | x :: _, [] -> compint x 0
    | x :: xs, y :: ys -> (
      match compint x y with
      | 0 -> compare xs ys
      | n -> n)
end

exception Invalid

let keep loc (attrs : attributes) =
  try
    let keep =
      List.for_all attrs ~f:(function
          | {Location.txt = "if"; _}, attr_payload -> (
            match attr_payload with
            | PStr
                [ { pstr_desc =
                      Pstr_eval
                        ( {pexp_desc = Pexp_apply (op, [(Nolabel, a); (Nolabel, b)]); _}
                        , [] )
                  ; _ } ] ->
                let get_op = function
                  | {pexp_desc = Pexp_ident {txt = Lident str; _}; _} -> (
                    match str with
                    | "<=" -> ( <=)
                    | ">=" -> ( >=)
                    | ">" -> ( >)
                    | "<" -> ( <)
                    | "<>" -> ( <>)
                    | "=" -> ( =)
                    | _ -> raise Invalid)
                  | _ -> raise Invalid
                in
                let eval = function
                  | {pexp_desc = Pexp_ident {txt = Lident "ocaml_version"; _}; _} ->
                      Version.current
                  | {pexp_desc = Pexp_tuple l; _} ->
                      let l =
                        List.map l ~f:(function
                            | {pexp_desc = Pexp_constant (Pconst_integer (d, None)); _}
                              ->
                                int_of_string d
                            | _ -> raise Invalid)
                      in
                      Version.of_list l
                  | _ -> raise Invalid
                in
                let op = get_op op in
                let a = eval a in
                let b = eval b in
                op (Version.compare a b) 0
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
  | {ppat_desc = Ppat_or (p1, p2); _} as p -> (
    match filter_pattern p1, filter_pattern p2 with
    | None, None -> None
    | Some p1, None -> Some p1
    | None, Some p2 -> Some p2
    | Some p1, Some p2 -> Some {p with ppat_desc = Ppat_or (p1, p2)})
  | {ppat_attributes; ppat_loc; _} as p ->
      if keep ppat_loc ppat_attributes then Some p else None

let mapper =
  { Ast_mapper.default_mapper with
    cases =
      (fun mapper cases ->
        let cases =
          filter_map cases ~f:(fun case ->
              match filter_pattern case.pc_lhs with
              | None -> None
              | Some pattern -> Some {case with pc_lhs = pattern})
        in
        Ast_mapper.default_mapper.cases mapper cases)
  ; structure =
      (fun mapper items ->
        let items =
          List.filter items ~f:(fun item ->
              match item.pstr_desc with
              | Pstr_module {pmb_attributes; pmb_loc; _} -> keep pmb_loc pmb_attributes
              | _ -> true)
        in
        Ast_mapper.default_mapper.structure mapper items) }

let () =
  Driver.register
    ~name:"ppx_optcomp_light"
    Migrate_parsetree.Versions.ocaml_407
    (fun _config _cookies -> mapper)

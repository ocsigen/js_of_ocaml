(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2024 Hugo Heuzard
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

open! Stdlib

type t =
  | Bot of string
  | Block of t list
  | Function of
      { arity : int
      ; pure : bool
      ; res : t
      }

let rec to_string (shape : t) =
  match shape with
  | Bot s -> if true then "N" else Printf.sprintf "N(%s)" s
  | Block l -> "[" ^ String.concat ~sep:"," (List.map ~f:to_string l) ^ "]"
  | Function { arity; _ } -> Printf.sprintf "F(%d)" arity

type key =
  | Name of string
  | Var of Code.Var.t

module Hashtbl = Hashtbl.Make (struct
  type t = key

  let equal a b = Poly.(a = b)

  let hash = function
    | Name s -> Hashtbl.hash s
    | Var x -> Code.Var.idx x
end)

let state : t Hashtbl.t = Hashtbl.create 17

let set_shape ~name shape = Hashtbl.add state (Name name) shape

let get_shape ~name = Hashtbl.find_opt state (Name name)

let assign x shape = Hashtbl.add state (Var x) shape

let propagate x offset target =
  match Hashtbl.find_opt state (Var x) with
  | None -> ()
  | Some (Bot _ | Function _) -> ()
  | Some (Block l) -> Hashtbl.replace state (Var target) (List.nth l offset)

let get x = Hashtbl.find_opt state (Var x)

let reset () =
  Hashtbl.to_seq_keys state
  |> Seq.filter (function
         | Name _ -> false
         | Var _ -> true)
  |> List.of_seq
  |> List.iter ~f:(Hashtbl.remove state)

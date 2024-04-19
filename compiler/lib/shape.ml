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
  | Top of string
  | Block of t list
  | Function of
      { arity : int
      ; pure : bool
      ; res : t
      }

let rec to_string (shape : t) =
  match shape with
  | Top s -> if true then "N" else Printf.sprintf "N(%s)" s
  | Block l -> "[" ^ String.concat ~sep:"," (List.map ~f:to_string l) ^ "]"
  | Function { arity; _ } -> Printf.sprintf "F(%d)" arity

module Store = struct
  module T = Hashtbl.Make (struct
    type t = string

    let equal (a : t) (b : t) = String.equal a b

    let hash = Hashtbl.hash
  end)

  let t = T.create 17

  let set ~name shape = T.replace t name shape

  let get ~name = T.find_opt t name

  let load ~name:_ _dirs = None
end

module State = struct
  type key = Code.Var.t

  module T = Hashtbl.Make (struct
    type t = key

    let equal a b = Poly.(a = b)

    let hash = Code.Var.idx
  end)

  let t = T.create 17

  let assign x shape = T.add t x shape

  let propagate x offset target =
    match T.find_opt t x with
    | None -> ()
    | Some (Top _ | Function _) -> ()
    | Some (Block l) -> T.replace t target (List.nth l offset)

  let get x = T.find_opt t x

  let reset () = T.clear t
end

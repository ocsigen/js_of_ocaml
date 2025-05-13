(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2020 Hugo Heuzard
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
open Stdlib

module File = struct
  type t =
    { name : string
    ; content : string
    ; fragments : string option
    }

  let name t = t.name

  let content t = t.content

  let fragments t = t.fragments

  let create ~name ~content = { name; content; fragments = None }
end

let tbl = String.Hashtbl.create 17

let register ~name ~content ~fragments =
  let name = "+" ^ name in
  let t = { File.name; content; fragments } in
  if String.Hashtbl.mem tbl name
  then warn "The builtin runtime file %S was registered multiple time" name;
  String.Hashtbl.add tbl name t;
  t

let find name = try Some (String.Hashtbl.find tbl name) with Not_found -> None

let all () = String.Hashtbl.fold (fun _ v acc -> v :: acc) tbl []

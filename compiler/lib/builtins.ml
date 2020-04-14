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
  (* TODO: Storing more information here (e.g. primitives with arity &
     purity) would allow us to not parse the file if we're not
     linking. *)
  type t =
    { name : string
    ; content : string
    }

  let name t = t.name

  let content t = t.content
end

let tbl = Hashtbl.create 17

let register ~name ~content =
  let name = "+" ^ name in
  let t = { File.name; content } in
  if Hashtbl.mem tbl name
  then warn "The builtin runtime file %S was registered multiple time" name;
  Hashtbl.add tbl name t;
  t

let find name = try Some (Hashtbl.find tbl name) with Not_found -> None

let all () = Hashtbl.fold (fun _ v acc -> v :: acc) tbl []

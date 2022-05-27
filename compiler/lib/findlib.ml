(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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

let find_pkg_dir_ref = ref (fun _ -> None)

let set_find_pkg_dir f = find_pkg_dir_ref := f

let find_pkg_dir pkg = !find_pkg_dir_ref pkg

let path_require_findlib path =
  match String.drop_prefix path ~prefix:"+" with
  | None -> None
  | Some suffix -> (
      match String.lsplit2 ~on:'/' suffix with
      | None -> Some ("stdlib", suffix)
      | Some (pkg, suffix) -> Some (pkg, suffix))

let find paths name =
  match path_require_findlib name with
  | Some (pkg, suffix) -> (
      match find_pkg_dir pkg with
      | None -> None
      | Some dir -> Fs.find_in_path [] (Filename.concat dir suffix))
  | None -> Fs.find_in_path paths name

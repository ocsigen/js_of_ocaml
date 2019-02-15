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

let find_pkg_dir_ref = ref (fun _ -> raise Not_found)

let set_find_pkg_dir f = find_pkg_dir_ref := f

let find_pkg_dir pkg = !find_pkg_dir_ref pkg

let path_require_findlib path =
  if path <> "" && path.[0] = '+'
  then Some (String.sub path 1 (String.length path - 1))
  else None

let rec find_in_findlib_paths ?(pkg = "stdlib") paths name =
  match paths with
  | [] -> raise Not_found
  | path :: rem ->
      let file =
        match path_require_findlib path with
        | Some path -> Filename.concat (Filename.concat (find_pkg_dir pkg) path) name
        | None -> Filename.concat path name
      in
      if Sys.file_exists file then file else find_in_findlib_paths rem name

(* Js_of_ocaml toplevel
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Jérôme Vouillon
 * Laboratoire PPS - CNRS Université Paris Diderot
 * Copyright (C) 2016 OCamlPro, Grégoire Henry
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

(* load file using a synchronous XMLHttpRequest *)
let load_resource_aux url =
  try
    let xml = XmlHttpRequest.create () in
    xml##_open(Js.string "GET", url, Js._false);
    xml##send(Js.null);
    if xml##status = 200 then Some (xml##responseText) else None
  with _ -> None

let load_resource scheme (_,suffix) =
  let url = (Js.string scheme)##concat(suffix) in
  load_resource_aux url

let setup_pseudo_fs () =
  Sys_js.register_autoload' "/dev/" (fun s -> Some (Js.string ""));
  Sys_js.register_autoload' "/http/"  (fun s -> load_resource "http://" s);
  Sys_js.register_autoload' "/https/" (fun s -> load_resource "https://" s);
  Sys_js.register_autoload' "/ftp/"   (fun s -> load_resource "ftp://" s);
  Sys_js.register_autoload' "/" (fun (_,s) -> load_resource_aux ((Js.string "filesys/")##concat(s)))

let () = setup_pseudo_fs ()

(* Js_of_ocaml library
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2011 Pierre Chambart
 * Laboratoire PPS - CNRS Université Paris Diderot
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

open Js
open Dom_html

class type formData = object
  method append : js_string t -> js_string t -> unit meth
  method append_blob : js_string t -> File.blob t -> unit meth
end

let formData : formData t constr = Js.Unsafe.global ## _FormData

let formData_form : (formElement t -> formData t) constr =
  Js.Unsafe.global ## _FormData

type form_elt =
  [ `String of js_string t
  | `File of File.file t ]

type form_contents =
  [ `Fields of (string * form_elt) list ref
  | `FormData of formData t ]

let rec filter_map f = function
  | [] -> []
  | v::q ->
    match f v with
      | None -> filter_map f q
      | Some v' -> v' :: (filter_map f q)

class type submittableElement = object
  method disabled : bool t prop
  method name : js_string t readonly_prop
  method value : js_string t prop
end

let have_content (elt:submittableElement t) =
  elt##name##length > 0 && not (Js.to_bool (elt##disabled))

let get_textarea_val ?(get=false) (elt:textAreaElement t) =
  if have_content (elt:>submittableElement t)
  then
    let name = to_string (elt##name) in
    [name,`String (elt##value)]
  else []

let get_select_val (elt:selectElement t) =
  if have_content (elt:>submittableElement t)
  then
    let name = to_string (elt##name) in
    if to_bool (elt##multiple)
    then
      let options = Array.init ( elt##options##length )
	(fun i -> Opt.to_option elt##options##item(i)) in
      filter_map (function
	| None -> None
	| Some e ->
	  if Js.to_bool e##selected
	  then Some (name,`String (e##value))
	  else None)
	(Array.to_list options)
    else [name,`String (elt##value)]
  else []

class type file_input = object
  inherit inputElement
  method files : File.fileList t optdef readonly_prop
  method multiple : bool optdef readonly_prop
end

let get_input_val ?(get=false) (elt:inputElement t) =
  if have_content (elt:>submittableElement t)
  then
    let name = to_string (elt##name) in
    let value = elt##value in
    match to_bytestring (elt##_type##toLowerCase ()) with
      | "checkbox"
      | "radio" ->
	if to_bool (elt##checked)
	then [name,`String value]
	else []
      | "submit"
      | "reset" -> []
      | "text"
      | "password" -> [name,`String value]
      | "file" ->
	if get
	then [name,`String value]
	else
	  let elt = (Unsafe.coerce elt:file_input t) in
	  (match Optdef.to_option (elt##files) with
	    | None -> []
	    | Some list ->
	      if list##length = 0
	      then [name,`String (Js.string "")]
	      else
		match Optdef.to_option (elt##multiple) with
		  | None
		  | Some false ->
		    (match Opt.to_option (list##item(0)) with
		      | None -> []
		      | Some file -> [name,`File file])
		  | Some true ->
		    filter_map (fun f ->
		      match Opt.to_option f with
			| None -> None
			| Some file -> Some (name,`File file))
		      (Array.to_list (Array.init (list##length)
					(fun i -> list##item(i)))))
      | _ -> [name,`String value]
  else []

let form_elements ?get (form:formElement t) =
  let length = form##elements##length in
  let elements = Array.to_list
    (Array.init length
       (fun i -> Opt.to_option (form##elements##item(i)))) in
  let contents =
    List.flatten
      (List.map
	 (function
	   | None -> [] (* shouldn't happen *)
	   | Some v ->
	     match tagged v with
	       | Select v -> get_select_val v
	       | Input v -> get_input_val ?get v
	       | Textarea v -> get_textarea_val v
	       | _ -> [] )
	 elements)
  in
  contents

let append (form_contents:form_contents) (form_elt:string * form_elt) =
  match form_contents with
    | `Fields list -> list := form_elt::!list
    | `FormData f ->
      match form_elt with
	| name, `String s -> f##append(string name, s)
	| name, `File file -> f##append_blob(string name, (file :> File.blob t))

let empty_form_contents () =
  match Optdef.to_option (Js.def formData) with
    | None -> `Fields (ref [])
    | Some constr -> `FormData ( jsnew constr() )

let post_form_contents form =
  let contents = empty_form_contents () in
  List.iter (append contents) (form_elements form);
  contents

let get_form_contents form =
  List.map (function
  | name, `String s -> name, to_string s
  | _ -> assert false) (form_elements ~get:true form)

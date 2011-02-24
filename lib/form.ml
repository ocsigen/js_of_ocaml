open Js
open Dom_html

class type formData = object
  method append : js_string t -> js_string t -> unit meth
  method append_blob : js_string t -> File.blob t -> unit meth
end

let formData : formData t constr Optdef.t =
  Js.Unsafe.variable "window.FormData"

let formData_form : (formElement t -> formData t) constr Optdef.t =
  Js.Unsafe.variable "window.FormData"

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

let get_select_val (elt:selectElement t) =
  let name = to_string (elt##name) in
  if to_bool (elt##multiple)
  then
    let options = Array.init ( elt##options##length )
      (fun i -> Optdef.to_option elt##options##item(i)) in
    filter_map (function
      | None -> None
      | Some e ->
	if e##selected
	then Some (name,`String (e##value))
	else None)
      (Array.to_list options)
  else [name,`String (elt##value)]

class type file_input = object
  inherit inputElement
  method files : File.fileList t optdef readonly_prop
  method multiple : bool optdef readonly_prop
end

let get_input_val ?(get=false) (elt:inputElement t) =
  let name = to_string (elt##name) in
  let value = elt##value in
  match String.lowercase (to_string (elt##_type)) with
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
	    match Optdef.to_option (elt##multiple) with
	      | None
	      | Some false ->
		(match Optdef.to_option (list##item(0)) with
		  | None -> []
		  | Some file -> [name,`File file])
	      | Some true ->
		filter_map (fun f ->
		  match Optdef.to_option f with
		    | None -> None
		    | Some file -> Some (name,`File file))
		  (Array.to_list (Array.init (list##length) (fun i -> list##item(i)))))
    | _ -> [name,`String value]

let form_contents ?get (form:formElement t) =
  let length = form##elements##length in
  let elements = Array.to_list
    (Array.init length
       (fun i -> Optdef.to_option (form##elements##item(i)))) in
  let contents =
    List.flatten
      (List.map
	 (function
	   | None -> [] (* shouldn't happen *)
	   | Some v ->
	     match tagged v with
	       | Select v -> get_select_val v
	       | Input v -> get_input_val ?get v
	       | _ -> [] (* shouldn't happen *) )
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

let post_form_contents form =
  match Optdef.to_option formData with
    | None -> `Fields (ref (form_contents form))
    | Some constr ->
      let formdata = jsnew constr() in
      List.iter (append (`FormData formdata)) (form_contents form);
      `FormData formdata

let get_form_contents form =
  List.map (function
  | name, `String s -> name, to_string s
  | _ -> assert false) (form_contents ~get:true form)


# Module `Js_of_ocaml.Form`

This module provides functions to manipulate forms.

```ocaml
class type  formData = object ... end
```
```ocaml
val formData : formData Js.t Js.constr
```
```ocaml
val formData_form : (Dom_html.formElement Js.t -> formData Js.t) Js.constr
```
```ocaml
type form_elt = [ 
  | `String of Js.js_string Js.t
  | `File of File.file Js.t
 ]
```
```ocaml
type form_contents = [ 
  | `Fields of (string * form_elt) list Stdlib.ref
  | `FormData of formData Js.t
 ]
```
```ocaml
val append : form_contents -> (string * form_elt) -> unit
```
```ocaml
val post_form_contents : Dom_html.formElement Js.t -> form_contents
```
```ocaml
val get_form_contents : Dom_html.formElement Js.t -> (string * string) list
```
```ocaml
val empty_form_contents : unit -> form_contents
```
```ocaml
val form_elements : 
  ?get:bool ->
  Dom_html.formElement Js.t ->
  (string * form_elt) list
```
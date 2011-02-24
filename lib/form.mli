open Js

class type formData = object
  method append : js_string t -> js_string t -> unit meth
  method append_blob : js_string t -> File.blob t -> unit meth
end

val formData : formData t constr Optdef.t
val formData_form : (Dom_html.formElement t -> formData t) constr Optdef.t 

type form_elt =
  [ `String of js_string t
  | `File of File.file t ]

type form_contents =
  [ `Fields of (string * form_elt) list ref
  | `FormData of formData t ]

val append : form_contents -> string * form_elt -> unit

val post_form_contents : Dom_html.formElement t -> form_contents

val get_form_contents : Dom_html.formElement t -> (string * string) list

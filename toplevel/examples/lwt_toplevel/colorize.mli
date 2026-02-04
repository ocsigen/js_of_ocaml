open Js_of_ocaml
open Js_of_ocaml_tyxml

val text : a_class:string -> string -> [> Html_types.div_content ] Tyxml_js.Html.elt

val ocaml : a_class:string -> string -> [> Html_types.div_content ] Tyxml_js.Html.elt

val highlight :
  [ `Pos of int ] -> [ `Last | `Pos of int ] -> Dom_html.element Js.t -> unit

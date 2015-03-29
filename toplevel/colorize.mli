val text  : a_class:string -> string
	    -> [> Html5_types.div_content ] Tyxml_js.Html5.elt

val ocaml : a_class:string -> string
	    -> [> Html5_types.div_content ] Tyxml_js.Html5.elt


val highlight : [`Pos of int] -> [`Last | `Pos of int]
		-> Dom_html.element Js.t -> unit

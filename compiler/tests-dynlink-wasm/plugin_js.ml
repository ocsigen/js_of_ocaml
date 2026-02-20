external pure_js_expr : string -> 'a = "caml_pure_js_expr"

external js_to_string : 'a -> string = "caml_js_to_string"

let () =
  let s = pure_js_expr "'hello from JS'" in
  print_endline (js_to_string s)

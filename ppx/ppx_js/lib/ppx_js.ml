include Ppx_js_internal
let () = wrapper := Some "Js_of_ocaml"

module Converter =
  Migrate_parsetree.Versions.Convert
    (Migrate_parsetree.OCaml_405)
    (Migrate_parsetree.OCaml_current)

let js_mapper _ = Converter.copy_mapper mapper

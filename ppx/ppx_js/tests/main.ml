open Migrate_parsetree
let () =
  let module Converter =
    Migrate_parsetree.Versions.Convert
      (Migrate_parsetree.OCaml_406)
      (Migrate_parsetree.OCaml_current)
  in
  let mapper = Converter.copy_mapper Ppx_js.mapper in
  Toplevel_expect_test.run (fun _ -> mapper)

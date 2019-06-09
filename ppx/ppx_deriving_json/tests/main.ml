let _ = Deriving_Json.read

let _ = Ppx_deriving_json.json

let () = Toplevel_expect_test.run Migrate_parsetree.Driver.run_as_ast_mapper

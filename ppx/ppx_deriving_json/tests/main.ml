let mapper _args =
  let structure _ st =
    Ppxlib_ast.Selected_ast.of_ocaml Structure st
    |> Ppxlib.Driver.map_structure
    |> Ppxlib.Selected_ast.to_ocaml Structure
  in
  { Ast_mapper.default_mapper with structure }

let () = Toplevel_expect_test.run mapper

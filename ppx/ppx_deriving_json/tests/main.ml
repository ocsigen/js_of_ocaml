let mapper _args =
  (*this mapper can only be used the way it is defined here, i.e.
    no other fields can be updated.*)
  let structure _ st =
    Ppxlib_ast.Selected_ast.of_ocaml Structure st
    |> Ppxlib.Driver.map_structure
    |> Ppxlib.Selected_ast.to_ocaml Structure
  in
  { Ast_mapper.default_mapper with structure }

let () = Toplevel_expect_test.run mapper

module String_set = Set.Make (String)

let print_stub s =
  Printf.printf
    "void %s () {\n\
    \  fprintf(stderr, \"Unimplemented Javascript primitive %s!\\n\");\n\
    \  exit(1);\n\
     }\n"
    s
    s

let () =
  let mls = ref [] in
  Arg.parse
    []
    (fun ml -> if not (Filename.check_suffix ml ".pp.ml") then mls := ml :: !mls)
    "generate dummy js stubs";
  let externals = ref String_set.empty in
  let value_description _mapper desc =
    let l = List.filter (fun x -> x.[0] <> '%') desc.Parsetree.pval_prim in
    externals := List.fold_right String_set.add l !externals;
    desc
  in
  let mapper = {Ast_mapper.default_mapper with value_description} in
  List.iter
    (fun ml ->
      let in_ = open_in ml in
      (try
         Location.input_name := ml;
         let lex = Lexing.from_channel in_ in
         let impl = Parse.implementation lex in
         let (_ : Parsetree.structure) = mapper.structure mapper impl in
         ()
       with exn -> Location.report_exception Format.std_formatter exn);
      close_in_noerr in_)
    !mls;
  print_endline "#include <stdlib.h>";
  print_endline "#include <stdio.h>";
  String_set.iter print_stub !externals

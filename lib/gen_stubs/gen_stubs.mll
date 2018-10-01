{
module String_set = Set.Make(String)
}

let ws = [' ' '\t']*

rule externals acc = parse
  | "\n" ws "external" ([^ '=']+) '=' ws '"' { read_external acc lexbuf }
  | _ { externals acc lexbuf }
  | eof { acc }

and read_external acc = parse
  | '%' [^ '"']* '"' { externals acc lexbuf }
  | ([^ '%' '"']* as ext) '"' { externals (String_set.add ext acc) lexbuf }
  | _ { failwith "invalid external" }

{
let print_stub s = Printf.printf
  "void %s () {\n  \
     fprintf(stderr, \"Unimplemented Javascript primitive %s!\\n\");\n  \
     exit(1);\n\
  }\n" s s

let () =
  let mls = ref [] in
  Arg.parse []
      (fun ml -> mls := ml :: !mls)
      "generate js stubs";

  let externals =
    ListLabels.fold_left ~init:String_set.empty ~f:(fun acc ml ->
      let in_ = open_in ml in
      let lex = Lexing.from_channel in_ in
      let acc = externals acc lex in
      close_in_noerr in_;
      acc) !mls
  in

  print_endline "#include <stdlib.h>";
  print_endline "#include <stdio.h>";
  String_set.iter print_stub externals
}

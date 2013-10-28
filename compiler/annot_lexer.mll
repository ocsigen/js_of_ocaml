{
open Annot_parser
}

let identifier = ['a'-'z''A'-'Z''_']+

rule initial = parse
  | "Provides" {TProvides}
  | "Requires" {TRequires}
  | "pure" {TAnnot `Pure }
  | "const" {TAnnot `Pure }
  | "mutable" {TAnnot `Mutable }
  | "mutator" {TAnnot `Mutator }
  | ['a'-'z''A'-'Z''$''_']['a'-'z''A'-'Z''$''_''0'-'9']* {
      let x = Lexing.lexeme lexbuf in
      (* Printf.eprintf "%S\n" x; *)
      TIdent x}
  | "," {TComma}
  | ":" {TSemi}
  | [' ''\t']+ { initial lexbuf }
  | eof { EOF }
  | ['\n'] {EOL}
  | _ { TOTHER(Lexing.lexeme lexbuf) }

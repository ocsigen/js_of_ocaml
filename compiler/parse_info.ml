type t = int * int


let t_of_lexbuf lexbuf : t =
  let l = lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
  and c = lexbuf.Lexing.lex_curr_p.Lexing.pos_bol
  and b = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum in
  (* Printf.printf "l%d c%d b%d" l c b; *)
  l,c

type t = {
  name : string;
  col : int;
  line : int;
  idx : int;
}

val zero : t

type lineinfo

val make_lineinfo_from_file : string -> lineinfo
val make_lineinfo_from_string : string -> lineinfo

val t_of_lexbuf : lineinfo -> Lexing.lexbuf -> t

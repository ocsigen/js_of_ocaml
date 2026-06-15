
# Module `Stdlib.Lexing`

```ocaml
type position = Stdlib__Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
}
```
```ocaml
val dummy_pos : position
```
```ocaml
type lexbuf = Stdlib__Lexing.lexbuf = {
  refill_buff : lexbuf -> unit;
  mutable lex_buffer : bytes;
  mutable lex_buffer_len : int;
  mutable lex_abs_pos : int;
  mutable lex_start_pos : int;
  mutable lex_curr_pos : int;
  mutable lex_last_pos : int;
  mutable lex_last_action : int;
  mutable lex_eof_reached : bool;
  mutable lex_mem : int array;
  mutable lex_start_p : position;
  mutable lex_curr_p : position;
}
```
```ocaml
val from_channel : ?with_positions:bool -> Stdlib.in_channel -> lexbuf
```
```ocaml
val from_string : ?with_positions:bool -> string -> lexbuf
```
```ocaml
val from_function : ?with_positions:bool -> (bytes -> int -> int) -> lexbuf
```
```ocaml
val set_position : lexbuf -> position -> unit
```
```ocaml
val set_filename : lexbuf -> string -> unit
```
```ocaml
val with_positions : lexbuf -> bool
```
```ocaml
val lexeme : lexbuf -> string
```
```ocaml
val lexeme_char : lexbuf -> int -> char
```
```ocaml
val lexeme_start : lexbuf -> int
```
```ocaml
val lexeme_end : lexbuf -> int
```
```ocaml
val lexeme_start_p : lexbuf -> position
```
```ocaml
val lexeme_end_p : lexbuf -> position
```
```ocaml
val new_line : lexbuf -> unit
```
```ocaml
val flush_input : lexbuf -> unit
```
```ocaml
val sub_lexeme : lexbuf -> int -> int -> string
```
```ocaml
val sub_lexeme_opt : lexbuf -> int -> int -> string option
```
```ocaml
val sub_lexeme_char : lexbuf -> int -> char
```
```ocaml
val sub_lexeme_char_opt : lexbuf -> int -> char option
```
```ocaml
type lex_tables = Stdlib__Lexing.lex_tables = {
  lex_base : string;
  lex_backtrk : string;
  lex_default : string;
  lex_trans : string;
  lex_check : string;
  lex_base_code : string;
  lex_backtrk_code : string;
  lex_default_code : string;
  lex_trans_code : string;
  lex_check_code : string;
  lex_code : string;
}
```
```ocaml
val engine : lex_tables -> int -> lexbuf -> int
```
```ocaml
val new_engine : lex_tables -> int -> lexbuf -> int
```
```ocaml
val range_to_string : (position * position) -> string
```
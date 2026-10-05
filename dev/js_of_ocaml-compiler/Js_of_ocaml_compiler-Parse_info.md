# Module `Js_of_ocaml_compiler.Parse_info`

```ocaml
type t = {
  src : string option; (* Path of the source file, when it is known to exist; source maps and source excerpts use it. *)
  name : string option; (* Name of the source file, as given by the producer. *)
  col : int; (* Column, 0-based (pos_cnum - pos_bol), in code points for JavaScript sources and in bytes for OCaml sources. *)
  line : int; (* Line, 1-based (pos_lnum). *)
  idx : int; (* Offset from the start of the lexed buffer (pos_cnum), in code points. Only set by the JavaScript lexer; 0 for OCaml positions. *)
}
```
A source position, as a `Lexing.position` with the file name split into the name the producer knew and the path resolved on disk.

Positions come from two producers with slightly different conventions:

- the JavaScript lexer ([`t_of_pos`](./#val-t_of_pos), [`t_of_lexbuf`](./#val-t_of_lexbuf)), which counts in code points and knows the file only by the name it was given, so `src` and `name` are the same, possibly `Some ""` for an anonymous string;
- the OCaml debug events of a bytecode file ([`t_of_position`](./#val-t_of_position)), whose positions are those of the OCaml compiler, in bytes, with `name` the file name recorded by the compiler (relative to where it ran) and `src` the path where js\_of\_ocaml found the source, if it did.
```ocaml
val zero : t
```
No position: no file, line 0, column 0\.

```ocaml
val equal : t -> t -> bool
```
```ocaml
val t_of_lexbuf : Stdlib.Lexing.lexbuf -> t
```
Position of the start of the current lexeme.

```ocaml
val t_of_pos : Stdlib.Lexing.position -> t
```
Position from the JavaScript lexer: `src` and `name` are both the `pos_fname`.

```ocaml
val start_position : t -> Stdlib.Lexing.position
```
Inverse of [`t_of_pos`](./#val-t_of_pos).

```ocaml
val t_of_position : src:string option -> Stdlib.Lexing.position -> t
```
Position from an OCaml debug event: `name` is the `pos_fname` of the event, `src` the resolved path of the source file.

```ocaml
val file : t -> string option
```
The file to show for the position: `name`, or `src` when the name is unknown; `None` when neither is known.

```ocaml
module Debug : sig ... end
```
Locations in debugging output, where the printed values should match the fields: the IR printer and the location comments of `--debug-info`.

```ocaml
module Diagnostic : sig ... end
```
Locations in error and warning messages.

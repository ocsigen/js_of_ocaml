# Module `Parse_info.Diagnostic`

Locations in error and warning messages.

```ocaml
val to_string : t -> string
```
`file:line:col` with the column starting from 1, as editors and compilers expect, or `line L, column C` when the file is not known.

```ocaml
val with_excerpt : t -> string -> string
```
`with_excerpt t message` is `to_string t ^ ": " ^ message` followed, when the file can be read, by the offending line and a marker under the column. The excerpt assumes the conventions of the JavaScript lexer: columns count code points and lines end at `"\r\n"`, `'\n'`, `'\r'`, U+2028 and U+2029. Long lines are cut to a window of 100 code points around the column.

# Module `Parse_info.Debug`

Locations in debugging output, where the printed values should match the fields: the IR printer and the location comments of `--debug-info`.

```ocaml
val to_string : t -> string
```
`file:line:col` with the line and column as stored (column 0-based), or `"?"` when no file name is known.

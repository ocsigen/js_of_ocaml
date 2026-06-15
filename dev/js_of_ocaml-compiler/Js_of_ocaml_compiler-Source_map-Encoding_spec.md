
# Module `Source_map.Encoding_spec`

```ocaml
type t = {
  output_file : string option; (* Source map file (None means generate inline. *)
  source_map : Standard.t; (* Source map to extend. *)
  keep_empty : bool; (* Don't add anything to the source map (for js_of_ocaml's "empty sourcemap" option. *)
}
```
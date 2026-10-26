# Module `Source_map.Index`

```ocaml
type section = {
  offset : Offset.t;
  map : Standard.t;
}
```
```ocaml
type t = {
  version : int;
  file : string option;
  sections : section list;
}
```
```ocaml
val to_standard : t -> Standard.t
```
Flatten an index map into a single standard map. The sources, names and source contents of the sections are concatenated (without deduplication) and the mappings are combined directly in their encoded form, without decoding them. The source root is kept if all sections share it; otherwise, each section's root is prepended to its sources.

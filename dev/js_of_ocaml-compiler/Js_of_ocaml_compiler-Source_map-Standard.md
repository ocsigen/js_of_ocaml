
# Module `Source_map.Standard`

```ocaml
type t = {
  version : int;
  file : string option;
  sourceroot : string option;
  sources : string list;
  sources_content : Source_content.t option list option;
  names : string list;
  mappings : Mappings.t; (* Left uninterpreted, since most useful operations can be performed efficiently directly on the encoded form, and a full decoding can be costly for big sourcemaps. *)
  ignore_list : string list;
}
```
```ocaml
val filter_map : t -> f:(int -> int option) -> t
```
If `f l` returns `Some l'`, map line `l` to `l'` (in the generated file) in the returned debug mappings. If `f l` returns `None`, remove debug mappings which concern line `l` of the generated file.

```ocaml
val merge : t list -> t option
```
Merge two lists of debug mappings. The time cost of the merge is more than linear in function of the size of the input mappings.

```ocaml
val empty : inline_source_content:bool -> t
```
```ocaml
val of_string : ?tmp_buf:Stdlib.Buffer.t -> string -> t
```
```ocaml
val of_file : ?tmp_buf:Stdlib.Buffer.t -> string -> t
```
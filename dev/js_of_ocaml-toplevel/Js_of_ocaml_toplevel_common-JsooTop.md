
# Module `Js_of_ocaml_toplevel_common.JsooTop`

Helper for Js\_of\_ocaml Toplevel.

```ocaml
val use : Stdlib.Format.formatter -> string -> bool
```
`use fmt content` Execute commands `content`. It does not print types nor values.

```ocaml
val execute : 
  bool ->
  ?pp_code:Stdlib.Format.formatter ->
  ?highlight_location:(Location.t -> unit) ->
  Stdlib.Format.formatter ->
  string ->
  unit
```
`execute print fmt content` Execute `content`. `print` says whether the values and types of the results should be printed. `pp_code` formatter can be use to output ocaml source during lexing.

```ocaml
val initialize : unit -> unit
```
Initialize Js\_of\_ocaml toplevel.

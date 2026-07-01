
# Module `Js_of_ocaml_toplevel_common.Direct`

Helper for Js\_of\_ocaml Toplevel.

```ocaml
val use : ?print_outcome:bool -> Stdlib.Format.formatter -> string -> bool
```
`use fmt content` Execute commands `content`, returning `true` if every phrase succeeded. `print_outcome` says whether the computed values and their types should be printed to `fmt`; it defaults to `false` (silent).

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
Initialize Js\_of\_ocaml toplevel. Idempotent.

```ocaml
val reset_toplevel_env : unit -> unit
```
Reset the toplevel environment (as on a `#reset`/restart), discarding all user bindings while keeping the runtime cmi directory on the load path so libraries stay resolvable.

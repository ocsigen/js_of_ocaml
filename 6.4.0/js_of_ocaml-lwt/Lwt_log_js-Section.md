
# Module `Lwt_log_js.Section`

```ocaml
type t = Lwt_log_core.section
```
```ocaml
val make : string -> Lwt_log_core.section
```
```ocaml
val name : Lwt_log_core.section -> string
```
```ocaml
val main : Lwt_log_core.section
```
```ocaml
val level : Lwt_log_core.section -> Lwt_log_core.level
```
```ocaml
val set_level : Lwt_log_core.section -> Lwt_log_core.level -> unit
```
```ocaml
val reset_level : Lwt_log_core.section -> unit
```

# Module `Js_of_ocaml.Sys_js`

Javascript specific Sys functions.


### Io.

```ocaml
val set_channel_flusher : Stdlib.out_channel -> (string -> unit) -> unit
```
Set a callback to be called when an out\_channel flush its buffer. `set_channel_flusher chan cb` install the callback `cb` for `chan` out\_channel. `cb` will be called with the string to flush.

```ocaml
val set_channel_filler : Stdlib.in_channel -> (unit -> string) -> unit
```
Set a callback to be called when an in\_channel wants to fill its buffer. `set_channel_filler chan cb` install the called `cb` for `chan` in\_channel. The string returned by `cb` will be appended to the channel buffer.


### Pseudo filesystem.

```ocaml
val mount_point : unit -> string list
```
```ocaml
val unmount : path:string -> unit
```
```ocaml
val mount : 
  path:string ->
  (prefix:string -> path:string -> string option) ->
  unit
```
Register a callback to the `path` to dynamically load missing files. Whenever a file is missing in `path`, the callback is used to optionally get the content of the file. `mount ~path f` register the callback `f` to the path `path`. The callback `f` receives `(prefix,suffix)` where:

- `prefix` is the path the function has been registered to.
- `Filename.contact prefix suffix` is the absolute filename .
```ocaml
val read_file : name:string -> string
```
`read_file name` returns the content of the file `name`. Raise `Sys_error` if the file does not exist.

```ocaml
val create_file : name:string -> content:string -> unit
```
Register a file to a Pseudo Filesystem. `create_file ~name ~content` register the file `name` with content `content` so it can be opened with `open_in name`

```ocaml
val update_file : name:string -> content:string -> unit
```
Update a file in the Pseudo Filesystem. `update_file ~name ~content` update the file `name` with content `content`


### Information.

```ocaml
val js_of_ocaml_version : string
```
`js_of_ocaml_version` is the version of Js\_of\_ocaml. It is a string of the form `"major.minor[.patchlevel][+additional-info]"`, where `major`, `minor`, and `patchlevel` are integers, and `additional-info` is an arbitrary string. The `[.patchlevel]` and `[+additional-info]` parts may be absent.

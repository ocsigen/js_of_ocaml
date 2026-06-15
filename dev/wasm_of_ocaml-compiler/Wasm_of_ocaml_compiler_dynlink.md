
# Module `Wasm_of_ocaml_compiler_dynlink`

```ocaml
val loadfile : string -> unit
```
`loadfile filename` dynamically loads a pre-compiled `.wasmo/.wasma` file.

Raises `Failure` if the file cannot be read or the module cannot be instantiated.

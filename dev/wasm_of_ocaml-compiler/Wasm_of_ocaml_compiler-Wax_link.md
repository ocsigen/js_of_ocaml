# Module `Wasm_of_ocaml_compiler.Wax_link`

```ocaml
type value = 
  | Bool of bool
  | String of string
  | Version of int * int * int
```
```ocaml
val value_equal : value -> value -> bool
```
```ocaml
type source = 
  | Binary (* Binary file (assembled already, passed through) *)
  | File (* Not read yet *)
  | Contents of string (* File contents to assemble *)
```
```ocaml
type input = {
  module_name : string; (* Name under which the module should be imported (used to fill the Binary.link_input record) *)
  file : string; (* File originally containing the module *)
  source : source; (* Information about the file, including possibly the already read file contents *)
}
```
```ocaml
val with_preprocessed_files : 
  variables:(string * value) list ->
  inputs:input list ->
  (Binaryen.link_input list -> 'a) ->
  'a
```
Preprocess and assemble runtime modules using the Wax toolchain.

Each text input (WAT or Wax, by extension) has its conditional annotations specialized against `variables`, and is assembled to a WebAssembly binary temporary file. Binary inputs are passed through unchanged. The resulting files are handed to `action` as [`Binaryen.link_input`](./Wasm_of_ocaml_compiler-Binaryen.md#type-link_input)s, suitable for our linker ([`Runtime.link`](./Wasm_of_ocaml_compiler-Runtime.md#val-link)), which only accepts binary inputs.

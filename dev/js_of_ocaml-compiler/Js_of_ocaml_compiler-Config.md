
# Module `Js_of_ocaml_compiler.Config`

```ocaml
module Flag : sig ... end
```
```ocaml
module Param : sig ... end
```
This module contains parameters that may be modified through command-line flags.


### Parameters that are constant across a program run

These parameters should be set at most once at the beginning of the program.

```ocaml
val target : unit -> [ `JavaScript | `Wasm ]
```
```ocaml
val set_target : [ `JavaScript | `Wasm ] -> unit
```
```ocaml
type effects_backend = [ 
  | `Disabled
  | `Cps
  | `Double_translation
  | `Jspi
  | `Native
 ]
```
```ocaml
val effects : unit -> effects_backend
```
```ocaml
val set_effects_backend : effects_backend -> unit
```
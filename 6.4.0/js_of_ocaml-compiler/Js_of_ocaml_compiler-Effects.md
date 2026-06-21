
# Module `Js_of_ocaml_compiler.Effects`

```ocaml
type trampolined_calls = Code.Var.Set.t
```
```ocaml
type in_cps = Code.Var.Set.t
```
```ocaml
val f : 
  flow_info:Global_flow.info ->
  live_vars:Deadcode.variable_uses ->
  Code.program ->
  Code.program * trampolined_calls * in_cps
```
Perform a partial CPS transform in order to translate a program that uses effect handler primitives to a program with only function calls, preserving the semantics.

In addition, if double translation is enabled, some functions are defined in two versions (direct-style and CPS) and the generated program switches to CPS versions when entering the first effect handler, and back to direct style when exiting it. In addition to this dynamic behavior, the transform performs a static analysis to detect which functions do not need to be CPS-transformed. As a consequence, some functions become pairs of functions while others remain in a single version. This functions returns the set of call sites that require trampolining, as well as the set of call sites that require the callee to be a pair with a CPS component.

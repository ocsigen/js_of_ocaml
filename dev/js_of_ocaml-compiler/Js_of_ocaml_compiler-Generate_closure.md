# Module `Js_of_ocaml_compiler.Generate_closure`

```ocaml
val f : trampolined_calls:Code.Var.Set.t -> Code.program -> Code.program
```
Implement tail calls between mutually recursive functions with trampolines. `trampolined_calls` are the calls which return a trampoline when the stack is too deep (see [`Effects.f`](./Js_of_ocaml_compiler-Effects.md#val-f)).

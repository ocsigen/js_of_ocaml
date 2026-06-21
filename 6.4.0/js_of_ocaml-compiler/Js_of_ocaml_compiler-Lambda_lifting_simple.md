
# Module `Js_of_ocaml_compiler.Lambda_lifting_simple`

```ocaml
val f : 
  to_lift:Code.Var.Set.t ->
  Code.program ->
  Code.program * Code.Var.t Js_of_ocaml_compiler.Code.Var.Map.t
```
Lambda-lift all functions of the program that are in `to_lift`. All functions are lifted to toplevel. Functions that may be mutually recursive are lifted together. Also yields a map from the original function names to the names of their lambda-lifted counterparts. E.g. consider:

let y \= \-3 in (\* ... \*) let rec fib n \= match n with \| 0 \| 1 \-\> 1 \| \_ \-\> fib (n-1) \+ fib (n-2) \+ y in fib 42

After lambda-lifting of `fib`, it will look like:

let y \= \-3 in (\* ... \*) let fib' y \= let rec fib\_l n \= match n with \| 0 \| 1 \-\> 1 \| \_ \-\> fib\_l (n-1) \+ fib\_l (n-2) \+ y in fib\_l in let fib \= fib' y in fib 42

`fib_l` is the lifted version of `fib`, `fib'` is the lifting closure.

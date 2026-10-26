# Module `Ppx_devtools_expect`

Inline expect tests, with conditional snapshots (`[%expect {| ... |} [@when COND]]`).

The identifiers of `[@when]` predicates are axes. The predefined ones are defined in [`Ppx_devtools_expect_runtime.Axes`](./Ppx_devtools_expect_runtime-Axes.md).

```ocaml
val register_axes : (string -> string option) -> unit
```
`register_axes f` adds axes: `f x` gives the fully qualified name of the runtime value of the identifier `x`, or `None` if `f` does not know `x`. Resolvers registered later take precedence, so they can override the predefined axes.

It is meant to be called by a ppx library whose `(ppx_runtime_libraries ...)` provides the values, for instance:

```ocaml
  let () =
    Ppx_devtools_expect.register_axes (function
      | ("quickjs" | "node") as x -> Some ("My_axes." ^ x)
      | _ -> None)
```

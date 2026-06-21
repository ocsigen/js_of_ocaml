
# Module `Js_of_ocaml.Effect_js`

Javascript-specific effect functions.

```ocaml
val assume_no_perform : (unit -> 'a) -> 'a
```
Passing a function `f` as argument of \`assume\_no\_perform\` guarantees that, when compiling with \`--effects=double-translation\`, the direct-style version of `f` is called, which is faster than the CPS version. As a consequence, performing an effect in a transitive callee of `f` will raise \`Effect.Unhandled\`, regardless of any effect handlers installed before the call to \`assume\_no\_perform\`, unless a new effect handler was installed in the meantime.

This behaviour is the same when double translation is disabled.

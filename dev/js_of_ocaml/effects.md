
# Effect handlers

Js\_of\_ocaml supports effect handlers with the `--effects=cps` flag. This is based on partially transforming the program to continuation-passing style. As a consequence, [tail calls](./tailcall.md) can be fully optimized (when CPS transformed).

This is not the default because the generated code can be slower, larger, and less readable. The transformation uses an analysis to detect parts of the code that cannot involve effects and keeps them in direct style. The analysis is especially effective on monomorphic code but less effective when higher-order functions are heavily used (`Lwt`, `Async`, `Incremental`).


## Double translation mode

An alternative CPS transform is provided with `--effects=double-translation`. It keeps a direct-style version of the transformed functions in addition to the CPS version. The choice of running the CPS version is delayed to run time.

Since CPS code is usually slower, this can avoid performance degradations. You can also ensure that some code runs in direct style using [`Jsoo_runtime.Effect.assume_no_perform`](./../js_of_ocaml-compiler/Jsoo_runtime-Effect.md#val-assume_no_perform).


## Wasm\_of\_ocaml

Wasm\_of\_ocaml provides three effect handler implementations:

- **`--effects=jspi`** (default) uses the JavaScript-Promise Integration extension. It does not require any code transformation but requires a runtime that supports JSPI.
- **`--effects=cps`** uses the same CPS transformation as js\_of\_ocaml.
- **`--effects=native`** uses the [WebAssembly Stack Switching proposal](https://github.com/WebAssembly/stack-switching) (typed continuations). It provides the best performance but requires a runtime with support for the WasmFX extension (currently available, behind the `--experimental-wasm-wasmfx` flag, in Chrome 148 or higher, or in a recent Node.js canary release (V8 version 14\.7.100 or higher)).

## Dune integration

Dune is aware of the `--effects` option. You can add it to the js\_of\_ocaml flags wherever you want to use it:

```
(js_of_ocaml (flags ...))
```

### Whole workspace setup

To enable effects for the entire workspace, add this to a `dune` or `dune-workspace` file at the root:

```
(env
 (_
  (js_of_ocaml
   (flags (:standard --effects=double-translation))
   (build_runtime_flags (:standard --effects=double-translation)))))
```
This setup supports both separate and whole program compilation.


### Per-executable setup

To enable effect handlers for specific executables:

```
(executable
 (name main)
 (js_of_ocaml
  (flags (:standard --effects=double-translation))))
```

## See also

- [Command line options](./options.md#effects) \- Full list of `--effects` modes
- [Tailcall optimization](./tailcall.md) \- How tail calls interact with effects
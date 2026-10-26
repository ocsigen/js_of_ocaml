# Module `Ppx_devtools_predicate.Predicate`

Shared predicate grammar used by `ppx_devtools-optcomp` (compile-time gating via `@if`/`@@if`/`@@@if`) and by `ppx_devtools-expect` (runtime gating via `%expect.when ...`).

The grammar is a boolean expression over a few atoms combined with the usual comparison and boolean operators (`<= >= > < = <>`, `&& || not`):

- `ocaml_version` — the running OCaml version, compared against an `(maj, min, patch)` tuple.
- `ast_version` — the ppxlib selected AST version (compile-time only).
- `arch_sixtyfour` — whether the toolchain targets 64-bit (`Sys.word_size = 64`); compile-time only.
- `oxcaml` — whether the compiler is the OxCaml flavour.
- `os_type` — `Sys.os_type` ("Unix" / "Win32" / "Cygwin").
- `introspect` — whether the compiler was built with introspection support (`Sys.introspection_enabled`); compile-time only.
- `backend` — `"native"` / `"js"` / `"wasm"`; runtime only (see [`reify`](./#val-reify)).
plus the runtime-only shorthands `js`, `wasm`, `native`, `win32`, `unix`, `cygwin`.

At runtime, identifiers are resolved by a function given to [`reify`](./#val-reify), so further axes can be added.

```ocaml
module Version : sig ... end
```
```ocaml
type t
```
```ocaml
exception Invalid of Ppxlib.Location.t
```
```ocaml
val parse : Ppxlib.expression -> t
```
Parse a predicate expression into [`t`](./#type-t). Raises [`Invalid`](./#exception-Invalid) on syntactically unexpected constructs.

```ocaml
val eval_compile_time : t -> bool
```
Evaluate the predicate now, at preprocessing time. Resolves `ocaml_version`, `ast_version`, `arch_sixtyfour`, `oxcaml`, `introspect` and `os_type`. Raises [`Invalid`](./#exception-Invalid) on constructs that are not meaningful at compile time (e.g. `backend`, the runtime shorthands or extension axes).

```ocaml
val reify : 
  resolve:(string -> string option) ->
  loc:Ppxlib.Location.t ->
  t ->
  Ppxlib.expression
```
Reify the predicate into an OCaml expression of type `bool` that evaluates it at runtime.

Identifiers are axes: `resolve x` gives the fully qualified name of the runtime value of `x`. Its expected type depends on how it is used: `bool` on its own, otherwise the type of the literal it is compared with (`string`, `int`, or `int list` for a version tuple).

Raises [`Invalid`](./#exception-Invalid) on identifiers that `resolve` does not know (such as the compile-time-only `ast_version`) and on comparisons between two identifiers.

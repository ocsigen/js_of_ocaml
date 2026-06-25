# Inline tests: `ppx_expect_light` + `ppx_optcomp_light`

js_of_ocaml ships its own, self-contained inline-test toolchain (a small
`ppx_expect` replacement) so the test suite can run unmodified across the
JavaScript, WebAssembly and native backends and across the OCaml versions we
support. It lives in three sibling directories:

| directory | what it provides |
|---|---|
| `compiler/ppx-expect-light/` | the `let%expect_test` / `[%expect]` rewriter + a runner |
| `compiler/ppx-optcomp-light/` | compile-time gating with `[@@@if]` / `[@@if]` |
| `compiler/ppx-light-predicate/` | the predicate grammar shared by both |

Tests are predominantly inline expect tests (not cram), so most of what you
write looks like `let%expect_test … [%expect {| … |}]`.

## 1. Declaring a test library (dune)

```dune
(library
 (name my_tests)
 (inline_tests
  (modes js wasm best))      ; run under js_of_ocaml, wasm_of_ocaml, and native
 (preprocess
  (pps ppx_optcomp_light ppx_expect_light)))
```

- Add `ppx_optcomp_light` to `(pps …)` only if the module uses `[@@if]` /
  `[@@@if]` (it is harmless otherwise, but keep the list minimal).
- `(modes …)`: `js`, `wasm`, `best` (native). Pick what makes sense; a test that
  uses `Js.*` values cannot run `best`, so use `(modes js wasm)`.
- Each **source file** is its own inline-test *partition*, so grouping many
  modules into one library does **not** cost test parallelism — prefer one
  library over a library-per-module.

## 2. Writing a test

```ocaml
let%expect_test "addition" =
  Printf.printf "%d" (1 + 2);
  [%expect {| 3 |}]
```

- The description is a string literal, or `_` for an anonymous test.
- `[%expect {| … |}]` checks everything printed to stdout/stderr since the
  previous assertion point. Consecutive `[%expect]`s are coalesced into one
  group (see conditional snapshots below).
- `[%expect.output]` returns the captured output **as a string** instead of
  checking it, e.g. `let s = [%expect.output] in …`.

Run `dune runtest <dir>`; if the output changed and the new output is correct,
accept it with `dune promote`.

## 3. Gating: two mechanisms, two purposes

There are two independent ways to make a test behave differently across
configurations. **Choosing the right one matters:**

| | `[@@if]` / `[@@@if]` (`ppx_optcomp_light`) | `[@when]` / `[@tags]` (`ppx_expect_light`) |
|---|---|---|
| when | **compile time** — the gated code is *removed* | **run time** — the code is compiled, the test is *skipped* (or a snapshot variant chosen) |
| use when | the code **wouldn't compile** on the excluded config (uses an API/syntax that doesn't exist there) | the code compiles everywhere but should be **skipped** or produce **different output** on some backend/engine/version |
| atoms | compile-time only (see §3.3) | runtime axes (see §3.3) |

Rule of thumb: reach for `[@when]`; use `[@@if]` only to keep code *compiling*
on a version/arch that lacks an API.

### 3.1 Compile-time gating — `[@@@if]` and `[@@if]`

`[@@@if COND]` is a **floating** attribute: when `COND` is false, every
following item in the file is dropped; when true, the marker is removed. Use it
to gate a whole module on an OCaml version:

```ocaml
(* In_channel.input_all is OCaml >= 5.1 only *)
[@@@if ocaml_version >= (5, 1, 0)]

let%expect_test _ = …
```

`[@@if COND]` gates a **single** item (a `let`, a `let%expect_test`, an
`external`, a module …). Prefer it to a whole-file `[@@@if]` when only a few
items need the gate, so the rest of the file still builds on older compilers:

```ocaml
let%expect_test "float16 round-trip" =
  …
  [%expect {| … |}]
[@@if ocaml_version >= (5, 2, 0)]
```

### 3.2 Run-time gating — `[@when]` and `[@tags]`

`[@when COND]` on the test description skips the whole test at run time when
`COND` is false:

```ocaml
let%expect_test ("uses process.env.TZ" [@when not quickjs]) = …

let%expect_test (_ [@when ocaml_version >= (5, 0, 0)]) = …
```

`[@when COND]` on a `[%expect]` selects a **conditional snapshot**: list one
variant per condition plus a plain default. The matching variant is checked;
the plain one is the default. Use this to run **one** test everywhere when only
its *output* differs:

```ocaml
let%expect_test "float32 marshalling" =
  print_string (Marshal.to_string 1.0s []);
  [%expect ({| <js bytes> |} [@when js])];
  [%expect {| <native/wasm bytes> |}]   (* default *)
```

`[@tags "a", "b"]` attaches inline-test tags that the runner drops by config.
Built-in tags:

`disabled`, `js-only` / `no-js`, `wasm-only` / `no-wasm`, `native-only`,
`quickjs-only` / `no-quickjs`, `wasi-only` / `no-wasi`,
`64-bits-only` / `32-bits-only`.

```ocaml
let%expect_test ("JS calling convention" [@tags "js-only"]) = …
```

(`[@when]` and `[@tags]` can be combined on the same test.)

### 3.3 Predicate grammar

Both ppxs parse the same expression grammar
(`Ppx_light_predicate.Predicate`): boolean `not`, `&&`, `||`; comparisons `=`,
`<>`, `<`, `<=`, `>`, `>=`; integer/string literals; and version tuples like
`(5, 2, 0)` compared against `ocaml_version`.

The **atoms differ** because one is evaluated at compile time and the other at
run time:

- Compile time (`[@@if]` / `[@@@if]`): `ocaml_version`, `ast_version`,
  `oxcaml`, `os_type`, `arch_sixtyfour`.
- Run time (`[@when]`): `backend` (shorthands `js` / `wasm` / `native`),
  `host_engine` (shorthands `node` / `quickjs` / `wasi`), `target_engine`,
  `os_type` (shorthands `win32` / `unix` / `cygwin`), `ocaml_version`,
  `effects` (`"disabled"` / `"cps"` / …), `oxcaml`.

So `[@when js && not quickjs]`, `[@when effects <> "cps"]`,
`[@when not native]`, `[@@if os_type <> "Win32"]`,
`[@@if arch_sixtyfour]` are all valid; but a backend atom like `js` is **only**
available at run time (`[@when]`), and `arch_sixtyfour` **only** at compile time
(`[@@if]`).

## 4. Running tests

```bash
dune runtest <dir>                              # js + native, default profile
dune promote                                    # accept changed output
WASM_OF_OCAML=true dune build @runtest-wasm     # wasm
dune build @runtest --profile quickjs           # run JS under QuickJS (qjs)
make tests                                       # the whole suite (needs a recent Node)
```

A failing snapshot prints a patdiff and writes a `*.corrected` file; `dune
promote` copies it back over the source.

## 5. Running a single file or test

Each file is its own partition (keyed by basename). The inline-test runner is
built next to the library, e.g.
`_build/default/<dir>/.<lib>.inline-tests/inline-test-runner.bc.js` (and
`.exe` for native, `.bc.wasm.js` for wasm). Invoke it directly:

```bash
R=_build/default/compiler/tests-jsoo/.jsoo_testsuite.inline-tests/inline-test-runner.bc.js

node "$R" inline-test-runner <lib> -list-partitions          # list partitions
node "$R" inline-test-runner <lib> -partition test_foo.ml -verbose
node "$R" inline-test-runner <lib> -only-test test_foo.ml:42 # a file, or file:line
node "$R" inline-test-runner <lib> -matching "substring"     # by description
```

Useful flags: `-verbose` (list every test, incl. `(skipped)` ones),
`-partition <file.ml>`, `-only-test <file.ml[:line]>`, `-matching <substr>`,
`-drop-tag <t>` / `-require-tag <t>`, `-list-partitions`.

When iterating on one test, run just its directory (`dune runtest <dir>`) or the
runner with `-only-test`, not the whole suite.

## 6. Choosing a gate — worked decisions

- *Test uses `In_channel.input_all` (5.1+)* → won't compile on 5.0 →
  `[@@if ocaml_version >= (5, 1, 0)]`.
- *Test compiles everywhere but its digest differs because `Random` changed in
  5.0* → conditional snapshot `[%expect ({| … |} [@when ocaml_version >= (5, 0, 0)])]`
  + a default.
- *Test exercises JS calling conventions, meaningless on wasm/native* →
  `[@tags "js-only"]` (or `[@when js]`).
- *Codegen snapshot whose stdlib field indices shift at 5.5* → promote to the
  5.5 output and `[@@@if ocaml_version >= (5, 5, 0)]` (older jobs skip it).
- *`Condition.wait` blocks forever natively but the test should run on js/wasm*
  → `[@when not native]`.

## See also

- `compiler/tests-jsoo/` — the largest collection of real examples.
- `compiler/tests-ocaml/REAME.md` — the imported OCaml testsuite and the
  `tools/sync_testsuite.exe` tool.

# Real-world benchmark candidates

Notes on real-world programs considered as benchmarks, beyond the ones
already in this directory. Candidates were found among the opam reverse
dependencies of `js_of_ocaml` and on GitHub (projects with
`js_of_ocaml`/`wasm_of_ocaml` build rules). A good candidate has a
JavaScript or Web build upstream, a deterministic workload that runs under
Node.js, and covers something the existing benchmarks do not.

## Added

| Benchmark | What it exercises | Workload |
| --- | --- | --- |
| `benchmark-query-json` | JSON lexing, parsing and printing (menhir, sedlex); effect handlers, so the JavaScript version uses `--effects cps` | 4 queries on the files of its own benchmark suite |
| `benchmark-herd` | CPU-bound enumeration of candidate executions (herdtools7 memory model simulator); large program | the 70 AArch64 litmus tests of its catalogue |
| `benchmark-alt-ergo` | Arbitrary-precision arithmetic through `zarith_stubs_js`, hash-consing, backtracking search (SMT solver) | the slowest problems of its test suite |

Measured with OCaml 5.4, before the benchmarks moved to OCaml 5.5
(native → JavaScript / Wasm):

- query-json, identity on a 97 MB file: 5.8 s → 42.7 s with `--effects cps`
  (29.8 s with `--effects double-translation`) / 10.6 s;
- herd, 70 AArch64 litmus tests: 0.72 s → 4.6 s / 1.7 s;
- Alt-Ergo, heaviest problem: 0.44 s → 4.8 s / 1.6 s. In JavaScript, even
  trivial problems take about 0.6 s (about 0.25 s in Wasm): startup is slow.

Notes:

- The Wasm implementation of Zarith (`runtime.wat`) comes with
  `zarith_stubs_js` v0.18 (v0.17.0 has none), installed with the Jane
  Street packages (see `bench.Dockerfile`).
- Alt-Ergo depends on dolmen, which requires `menhir < 20260112`: the
  benchmark image uses that older menhir throughout.
- Alt-Ergo's default command-line entry point uses `Unix.setitimer`; the
  benchmark uses its Node.js entry point (`src/bin/js/main_text_js.ml`)
  with a small patch disabling the timers. Alt-Ergo is distributed under
  the OCamlPro Non-Commercial License.
- Executables built with dune only in native mode need `(modes byte exe)`
  to get a bytecode executable to compile.

## Future candidates

- **stanc3** (Stan compiler, released as `stanc.js`): the JavaScript
  version works and produces the same C++ as the native compiler (10 large
  models: 11.6 s vs 1.8 s natively). Its master branch targets OCaml
  5.5.1, like the benchmark image, and only uses Jane Street ppxs (no
  `core`), whose v0.18 versions have Wasm stubs. Remaining blocker: it pins
  exact versions, including `menhir = 20260209`, which conflicts with
  Alt-Ergo's dolmen (`< 20260112`), so its dependencies would need relaxed
  constraints. `--O1` is too slow for a benchmark (12 s per model
  natively).
- **Catala** (compiler for law specifications): usable as a compile-time
  benchmark (43.5 MB bytecode: 6.3 s / 593 MB with js_of_ocaml, 9.9 s /
  618 MB with wasm_of_ocaml). Running it is harder: the command-line
  interpreter loads the compiled modules of its standard library with
  Dynlink, and the Web interpreter (`compiler/web`) only takes single-file
  programs. A realistic workload would be the housing benefits tests of
  `catala-examples` (`aides_logement`).
- **Reason's `refmt`** (`js/refmt.ml` in reasonml/reason): large menhir
  parser, parsing and printing workload. Not tried.
- **merlin-js**: completion and type queries; needs `.cmi` files at
  runtime. Not in opam. Not tried.
- **jsCoq / rocq-lsp**: Rocq compiled to JavaScript; very large compile
  input, proof checking at runtime. Heavy setup. Not tried.
- **Belenios** (`src/platform/js`): verifiable voting, big-number
  cryptography. Not in opam. Not tried.
- **Grain compiler** (built with js_of_ocaml): esy/npm setup, depends on
  binaryen.js. Not tried.
- Others seen: liquidsoap-js, sherlodoc (documentation search), vg (vector
  graphics rendering), hazel and memtrace_viewer (DOM applications,
  compile time only), o1js/Mina (needs Rust), slipshow, vult, eff.

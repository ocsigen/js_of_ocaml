# Differential fuzzers

`jsoo_fuzz` generates random programs, compiles them with `js_of_ocaml`
and `wasm_of_ocaml` under random flags and effect modes, runs them with
node, and compares the output with an oracle.

```
dune build tools/fuzz/jsoo_fuzz.exe
_build/default/tools/fuzz/jsoo_fuzz.exe CAMPAIGN [OPTIONS]
```

## Campaigns

| Campaign   | Programs | Oracle |
|------------|----------|--------|
| `whole`    | generated OCaml programs (closures, exceptions, effects, objects, functors, recursive modules, lazy values, int32/int64, floats...) | ocamlrun |
| `numeric`  | numeric kernels: loops, references, exception handlers, closures, arrays, bigarrays and records over floats, int32, int64, nativeint and integers, for the placement of the box/unbox and tag/untag conversions in `wasm_of_ocaml` | ocamlrun |
| `separate` | the same programs, split into modules | ocamlrun; each program is compiled whole and separately (`compile` of each unit, then `link`) |
| `prims`    | runtime primitives on random inputs: formatting and parsing of numbers, int32/int64, floats, compare, hash, Marshal, strings, bytes, Random, Digest, bigarrays | ocamlrun, line by line |
| `interop`  | programs using the js_of_ocaml library and its ppx: calls in both directions, callbacks, objects, arrays, strings, `Opt`/`Optdef`, exceptions; JavaScript expressions given to `Js.Unsafe.js_expr` | the JavaScript output with optimizations disabled; each `js_expr` is also compared with `eval` of its source (lines reporting `DIFF`) |
| `runtime`  | programs with user runtime files (`//Provides`, `//Requires`, `//Alias`, `//Weakdef`, `//If`, `//Version`, `//Always`, macros, named values) and Wasm primitives importing them | the same program where the primitives are written in OCaml, run by ocamlrun |
| `corpus`   | existing bytecode programs, given on the command line | the output with the default flags |

The generated programs avoid target-dependent behavior: integers stay
within 15 or 16 bits (30 bits in the `numeric` campaign, where
overflowing arithmetic is masked), recursion is bounded, floats are
printed in hexadecimal. In the `prims` campaign, the tests whose result
legitimately differs in JavaScript (hash and Marshal of int32 values and
floats, which are JavaScript numbers there) are marked `(* jsdiff *)` and
ignored for that target.

When the compiler supports `--debug interp` (the IR reference
interpreter), the `whole` and `corpus` campaigns also use it: the
compilation fails if a pass changes the behavior of the program, and the
interpreter's output is compared with the oracle's.

## Options

- `--js-of-ocaml PATH`, `--wasm-of-ocaml PATH`: the compilers to test
  (default: the ones in `PATH`); `--no-wasm` only tests `js_of_ocaml`,
  `--no-js` only tests `wasm_of_ocaml` (whole, separate, prims and
  corpus campaigns).
- `--js-flags FLAGS`, `--wasm-flags FLAGS`: flags added to every
  compilation, for instance `--wasm-flags "--debug lcm-check"`.
- `--js-options NAMES`, `--wasm-options NAMES`: options (as in
  `--disable`) added to the ones disabled at random, to test options
  that are not known to the fuzzer, for instance on a branch.
- `--seed N`, `-n N`: the seeds (default 1 to 100). A seed determines
  the program and the configurations, so a problem can be reproduced by
  running the campaign with the same seed.
- `-j N`: number of parallel jobs (default 3).
- `-o DIR`: output directory (default `_fuzz`). The standard library
  compiled for separate compilation is cached there.

The environment variable `JSOO_FUZZ_WRAPPER` gives a command prefix for
the compilers and node, for instance to cap their memory use:

```
JSOO_FUZZ_WRAPPER="systemd-run --user --scope -q -p MemoryMax=3G" jsoo_fuzz whole ...
```

## Reports

Each problem is printed (`problem: seed N: ...`), and the files needed to
reproduce it are saved in `DIR/N/` with a `report.txt` (flags, expected
and actual outputs). At the end, the outcomes are counted.

`jsoo_fuzz gen KIND SEED` prints the program of a seed (`whole`,
`prims`, `interop`); `jsoo_fuzz gen runtime SEED DIR` writes the files of
a `runtime` program.

`jsoo_fuzz reduce --js-of-ocaml PATH --flags FLAGS --pattern TEXT
FILE.ml` removes the toplevel items of a program (separated by blank
lines) while node's output on the compiled program still contains
`TEXT`, and prints the result.

## Requirements

- OCaml 5.1 or later, with findlib; the `interop` campaign needs the
  `js_of_ocaml` and `js_of_ocaml-ppx` packages installed.
- node; Binaryen for `wasm_of_ocaml`. The `native` effect mode is run
  with `node --experimental-wasm-wasmfx`.

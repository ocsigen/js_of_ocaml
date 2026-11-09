# Calls through block fields: `.call(null, …)`

Part of the [performance notes](../README.md). Benchmark: [`call.js`](call.js).

## What the compiler does

When a known function is called with its exact arity through a block
field, typically a function from another compilation unit (`Stdlib[28]`)
or a record of closures, the call used to be emitted as

```js
Stdlib[28].call(null, a, b)
```

instead of `Stdlib[28](a, b)` (`compiler/lib/generate.ml`,
`apply_fun_raw`, `apply_directly`), with the comment "Make sure we are
performing a regular call, not a (slower) method call" (de272311e8,
January 2023, part of #1397). In the
[review of #1397](https://github.com/ocsigen/js_of_ocaml/pull/1397#discussion_r1554831912),
Jérôme Vouillon explained that without it, the improved arity analysis of
that PR resulted in slower code: on ocamlc compiled with `--opt 3`
(OCaml 4.14), running `ocamlc -c` on `benchmarks/sources/ml/*.ml` (three
times over), it was close to 8% faster on his machine. It mostly matters for separate compilation
(the default in dune's dev profile):
`compiler/tests-full/stdlib.cma.expected.js` contained 648 such calls. In
whole-program compilation, module fields are usually bound to variables
and called directly (see the counts below).

Since #2613, such calls are emitted as `Stdlib[28](a, b)`. With `--enable
callgen`, calls to unknown functions also check the arity inline, and
their direct branch is emitted as `(0, M[i])(a, b)`, see Semantics below.

## V8 mechanism

A method call `M[1](a, b)` passes `M` as the receiver; the callee, OCaml
code, ignores `this`. In the benchmark below, a method call and a call
without receiver (`(0, M[1])(a, b)`) take the same time in every tier.

`M[1].call(null, a, b)` calls the builtin `Function.prototype.call`.
TurboFan's
[`ReduceFunctionPrototypeCall`](https://github.com/v8/v8/blob/054f2fd063f69f44ff0dd35faf0c8e0b8a3f3c44/src/compiler/js-call-reducer.cc#L3474)
turns it into a direct call of `M[1]`, but marks that call
`CallFeedbackRelation::kUnrelated` ([L3512](https://github.com/v8/v8/blob/054f2fd063f69f44ff0dd35faf0c8e0b8a3f3c44/src/compiler/js-call-reducer.cc#L3512)): the call-target
feedback was collected for the call to `Function.prototype.call`, not for
`M[1]`. `ReduceJSCall` then does not use the feedback at all
([L5322](https://github.com/v8/v8/blob/054f2fd063f69f44ff0dd35faf0c8e0b8a3f3c44/src/compiler/js-call-reducer.cc#L5322)), so unless the target is a constant, TurboFan
**cannot inline the callee**. `node --trace-turbo-inlining` shows it: the
callee of `M[1](…)` is inlined, the callee of `M[1].call(null, …)` is not
even considered.

## Benchmark

`call.js`: a block of 8 small strict functions, called in a loop through
`M[1](…)` (method call), `M[1].call(null, …)` (what Js_of_ocaml used to
emit) and `(0, M[1])(…)` (a plain call without receiver).
The `-poly` variants call 8 different functions at the same site.

## Results

```
# call.js  node v25.8.2
method-mono          1.37 ns/iter   100%
call-null-mono       6.62 ns/iter   482%
comma-mono           1.38 ns/iter   100%
method-poly          6.47 ns/iter   472%
call-null-poly       6.81 ns/iter   496%
# call.js  node v26.0.0
method-mono          1.13 ns/iter   100%
call-null-mono       5.34 ns/iter   472%
comma-mono           1.06 ns/iter    94%
method-poly          5.53 ns/iter   489%
call-null-poly       5.41 ns/iter   478%
# call.js  node v25.8.2  --no-turbo-inlining
method-mono          4.20 ns/iter   100%
call-null-mono       5.52 ns/iter   132%
comma-mono           4.49 ns/iter   107%
method-poly          5.61 ns/iter   134%
call-null-poly       6.99 ns/iter   166%
# call.js  node v25.8.2  --no-turbofan        (Ignition, Sparkplug, Maglev)
method-mono          4.56 ns/iter   100%
call-null-mono       6.20 ns/iter   136%
comma-mono           4.51 ns/iter    99%
# call.js  node v25.8.2  --no-turbofan --no-maglev --no-sparkplug
method-mono         45.24 ns/iter   100%
call-null-mono      59.55 ns/iter   132%
comma-mono          48.85 ns/iter   108%
```

### Real programs

The compiler was patched to emit `M[1](…)` (`plain`) or `(0, M[1])(…)`
(`comma`) instead of `M[1].call(null, …)`. By default, only exact calls
take this path; unknown callees go through `caml_callN`.

| output | static `.call(null` sites |
|---|---:|
| ocamlc, separate (linked) | 7160 |
| ocamlc, whole-program | 683 |
| small benchmarks, whole-program | 0, except nucleic (1) and raytrace (3) |

Run times on node 25.8.2: median of the per-round ratios against the
current output, with interleaved runs. Benchmarks that execute no such
call, used as controls, vary between −3.6% and +2.1%, so the noise floor
is about ±3%.

| benchmark | calls executed | rounds | plain | comma |
|---|---:|---:|---|---|
| ocamlc, separate | 7.2 M | 51 | −4.6% (faster in 40/51) | −3.8% |
| raytrace, whole-program | 2.0 M | 30 | −4.6% (faster in 24/30) | −3.5% |
| ocamlc, whole-program | 0.8 M | 30 | 0.0% | +0.5% |
| nucleic, whole-program | 0.28 M | 30 | +0.4% | +1.1% |

With `plain`, separately compiled ocamlc is also 2.7% smaller (−0.5%
gzipped). The `compiler/tests-full/stdlib.cma.expected.js` snapshot
(pretty-printed, with debug locations) is 1.3% smaller. In all builds,
ocamlc produced byte-identical `.cmo` files.

### The #1397 workload

The workload from the #1397 review, with the current compiler: ocamlc
from OCaml 5.5 compiled with `--opt 3` (whole program; 694 `.call(null`
sites, none in the plain version), running `ocamlc -c` on the 20 files of
`benchmarks/sources/ml`, each listed three times. The two versions run
one after the other, in alternating order; the ratio is the median of the
per-round ratios. The laptop was on battery (`powersave` governor), so
the noise is high:

| node (V8) | rounds | `.call(null` | plain | plain / `.call(null` | plain faster in | per-round ratios |
|---|---:|---:|---:|---:|---:|---:|
| 20.19.1 (11.3) | 15 | 8.63 s | 8.92 s | +0.8% | 6/15 | 0.94 to 1.06 |
| 22.22.1 (12.4) | 15 | 10.59 s | 10.81 s | +1.7% | 3/15 | 0.97 to 1.12 |
| 25.8.2 (14.1) | 15 | 6.87 s | 7.58 s | +0.1% | 7/15 | 0.66 to 1.20 |
| 25.8.2 (14.1) | 21 | 9.14 s | 8.55 s | −2.0% | 14/21 | 0.77 to 1.17 |

The 8% improvement does not reproduce: on every version, the difference
is within ±2%, in both directions. These runs can rule out an 8% effect,
not one of 1 to 2%. They do not say what the 2024 measurement saw: it
compared code produced by the compiler of that time, from OCaml 4.14
bytecode.

### Semantics

The receiver is only observable by a function that reads `this`.
Exact calls only target known OCaml functions, which do not, unless
they use `Js.Unsafe.js_expr "this"`; such a function used to see `null`
and now sees the block.

Calls to unknown functions go through `caml_callN` by default. When the
arity matches, it calls a local variable (`f(a0)`), so `this` is
`undefined`; otherwise `caml_call_gen` uses `f.apply(null, …)`. With `--enable callgen`,
they check the arity inline instead, and the callee may be any JavaScript
function typed as an OCaml function, for instance a browser builtin
obtained with `Js.Unsafe.js_expr`. Browser builtins can reject a receiver
of the wrong type, so this branch uses `(0, M[i])(…)`, which passes `this`
undefined, as `caml_callN` does. A test (a JavaScript function returning a description of
`this`, stored in a functor result) prints `undefined` with and without
`--enable callgen`.

## Verdict

**Counterproductive.** At a monomorphic site, `.call(null, …)` is about
5× slower than a method call, because it prevents inlining. Without
inlining (`--no-turbo-inlining`, or the lower tiers), it is still 32% to
36% slower. On the real programs where such calls are hot, a plain call
is 4.6% faster, and nothing got slower beyond noise. The 8% gain once
measured for `.call(null, …)` on whole-program ocamlc does not reproduce
with the current compiler (within ±2% on node 20, 22 and 25). Changed in #2613:
exact calls are plain calls, and calls with an inline arity check use
`(0, f)(…)`.

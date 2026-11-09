# Switch size limit

Part of the [performance notes](../README.md).

Benchmark: [`switch.js`](switch.js). On Node.js it runs each variant in
its own process and prints a table; on other JavaScript shells it runs
the one variant given on the command line (see its header):

```
node compiler/perf-notes/switch/switch.js
V8_FLAGS="--no-turbofan --no-maglev --no-sparkplug" node compiler/perf-notes/switch/switch.js
jsc compiler/perf-notes/switch/switch.js -- split8-128
```

## What the compiler does

`build_switch` (`compiler/lib/generate.ml`) splits a large switch into a
binary tree of `if (k <= x)` tests whose leaves are smaller switches.

Until #2612, a switch was split when it had more than `switch_size`
cases, with a default of 60. The comment in `compiler/lib/config.ml`
read: "V8 'optimize' switches with less than 128 case. 60 seams to perform
well." Issue #225, which proposed the splitting, links to Bluebird's
[Optimization killers](https://github.com/petkaantonov/bluebird/wiki/Optimization-killers#4-switch-case)
page: "a switch-case statement could only have up to 128 case-clauses,
more than that and the function containing the switch statement was not
optimizable".

Since #2612, the default `switch_size` is 256 (`--set switch_size=N`).
A switch is also split when its case values span more than 1000, the limit
for a jump table in JavaScriptCore (see below).

## V8 mechanism

The 128-case limit was in Crankshaft, V8's optimizing compiler at the
time. Up to V8 3.28 (2014), `VisitSwitchStatement` bailed out of
optimization above `kCaseClauseLimit = 128` clauses
([`src/hydrogen.cc`](https://github.com/v8/v8/blob/3.28.73/src/hydrogen.cc#L4827-L4834)); V8 3.29
removed the check. Crankshaft still compiled a switch as a sequence of
tests, one per clause, until its last version
([V8 5.8](https://github.com/v8/v8/blob/5.8.283/src/crankshaft/hydrogen.cc#L4540-L4560)). Since V8
5.9 (2017), JavaScript only runs on Ignition and TurboFan
([V8 blog](https://v8.dev/blog/launching-ignition-and-turbofan)).

Ignition compiles a dense switch on small integers to a
`SwitchOnSmiNoFeedback` bytecode, a jump table (checked with
`--print-bytecode`). TurboFan emits either a jump table or a binary
search, comparing their estimated space and time costs from the number of
cases and the range of their values
([`VisitSwitch`](https://github.com/v8/v8/blob/054f2fd063f69f44ff0dd35faf0c8e0b8a3f3c44/src/compiler/backend/x64/instruction-selector-x64.cc#L4205)
on x64).

## Benchmark

`switch.js`: a function with a dense switch of N cases (`flat-N`),
against the same function split in halves until the leaves have at most
60 cases, as `build_switch` used to do (`split-N`). The case indices are
pseudo-random (xorshift32). `SIZES=…` picks the sizes, and `VARIANTS=…`
the variants, including the `splitM-N` ones below.

## Results

```
# switch.js  node v25.8.2
flat-32          8.04 ns/iter   100%
flat-60         14.03 ns/iter   174%
flat-128        14.09 ns/iter   175%
split-128       17.49 ns/iter   217%
flat-129        14.09 ns/iter   175%
split-129       13.72 ns/iter   171%
flat-256        14.59 ns/iter   181%
split-256       15.82 ns/iter   197%
flat-1024       15.34 ns/iter   191%
split-1024      20.66 ns/iter   257%
# switch.js  node v25.8.2  --no-turbofan        (Ignition, Sparkplug, Maglev)
flat-32         20.90 ns/iter   100%
flat-60         20.05 ns/iter    96%
flat-128        20.83 ns/iter   100%
split-128       21.04 ns/iter   101%
flat-129        20.75 ns/iter    99%
split-129       21.92 ns/iter   105%
flat-256        22.07 ns/iter   106%
split-256       21.73 ns/iter   104%
flat-1024       24.66 ns/iter   118%
split-1024      33.95 ns/iter   162%
# switch.js  node v25.8.2  --no-turbofan --no-maglev --no-sparkplug
flat-32         75.53 ns/iter   100%
flat-60         78.81 ns/iter   104%
flat-128        65.84 ns/iter    87%
split-128      102.53 ns/iter   136%
flat-129        77.88 ns/iter   103%
split-129      112.78 ns/iter   149%
flat-256        78.16 ns/iter   103%
split-256      106.80 ns/iter   141%
flat-1024       63.41 ns/iter    84%
split-1024     123.46 ns/iter   163%
```

In optimized code the timings vary by about 20% between runs (flat-60:
14.0 ns here, 17.3 ns in the table below). There is no cliff at 128 cases,
and splitting brings no measurable gain: it costs 35% more at 1024 cases.
Ignition alone is where splitting costs most: from 36% (256 cases) to 95%
(1024 cases) more.

Real program: ocamlc (the `benchmarks/benchmark-ocamlc` setup, `--opt 2`)
compiled with the old default `switch_size=60` and with `switch_size=100000`
(no splitting). Only a handful of its switches have more than 60 cases
(the outputs differ by 65 bytes), and the run times are within 1%
(median of 5 runs: 14.19 s and 14.10 s).

## Other engines

On shells other than Node.js, `switch.js` runs one variant per process.
A run lasts about 50 ms per round, and the script prints the median of 10
rounds. `splitM-N` variants have leaves of at most M cases. Each value
below is the median of 3 runs, in ns/iter. The engines are SpiderMonkey
140.8 (`js140` shell, interpreter only: `--no-blinterp --no-baseline
--no-ion`), JavaScriptCore from WebKitGTK 2.52.6 (interpreter only:
`--useJIT=false`) and QuickJS 2025-04-26. The V8 column (node 25.8.2) was
measured the same way, before the Node.js and shell versions of the
benchmark were merged; on Node.js, `switch.js` now uses the harness.

| Variant | V8 | SM | SM interp | JSC | JSC interp | QuickJS |
|---|--:|--:|--:|--:|--:|--:|
| flat-32 | 11.7 | 21.9 | 218 | 12.8 | 59.9 | 468 |
| flat-60 | 17.3 | 22.1 | 210 | 13.3 | 56.0 | 752 |
| flat-128 | 19.4 | 22.2 | 195 | 13.6 | 64.8 | 1455 |
| split-128 | 19.3 | 23.0 | 228 | 12.4 | 90.9 | 522 |
| split8-128 | 23.6 | 28.3 | 259 | 18.8 | 109 | 341 |
| flat-256 | 20.2 | 23.2 | 195 | 12.4 | 69.0 | 2665 |
| split-256 | 22.0 | 27.1 | 246 | 12.5 | 96.8 | 555 |
| split8-256 | 27.4 | 32.8 | 300 | 17.1 | 117 | 352 |
| flat-1024 | 19.9 | 26.0 | 217 | 14.6 | 444 | 10419 |
| split-1024 | 30.3 | 38.5 | 301 | 19.7 | 115 | 589 |
| split8-1024 | 40.6 | 43.8 | 360 | 28.0 | 146 | 404 |

- **SpiderMonkey** behaves like V8. There is no cliff, and splitting costs
  up to 50% in optimized code and up to 40% in the interpreter.
- **JavaScriptCore**: the JIT tiers are flat up to 2000 cases. Splitting is
  neutral up to 256 cases, and slower beyond. The interpreter has a cliff.
  [`CaseBlockNode::tryTableSwitch`](https://github.com/WebKit/WebKit/blob/8e16cbe44f3a7e6aec2a641d612cba2b641cbf87/Source/JavaScriptCore/bytecompiler/NodesCodegen.cpp#L4989)
  only emits a jump table when the case values span at most 1000 and the
  span divided by the number of cases is below 15
  ([`switchJumpTableAmountThreshold`](https://github.com/WebKit/WebKit/blob/8e16cbe44f3a7e6aec2a641d612cba2b641cbf87/Source/JavaScriptCore/runtime/OptionsList.h#L122)).
  Otherwise the switch is a list of (value, target) pairs. The interpreter
  [leaves its fast path](https://github.com/WebKit/WebKit/blob/8e16cbe44f3a7e6aec2a641d612cba2b641cbf87/Source/JavaScriptCore/llint/LowLevelInterpreter64.asm#L2431) for
  these, and [`slow_path_switch_imm`](https://github.com/WebKit/WebKit/blob/8e16cbe44f3a7e6aec2a641d612cba2b641cbf87/Source/JavaScriptCore/llint/LLIntSlowPaths.cpp#L1861)
  scans the pairs one by one (WebKit commit 8e16cbe44f). On a dense switch: 61 ns at 1001 cases,
  393 ns at 1002 and 730 ns at 2000. With the JIT enabled the timings stay
  flat, so the cliff only affects code running in the interpreter. The
  density condition makes a list of switches spanning at most 1000 with
  fewer than 1 case in 15, so of at most 66 cases.
- **QuickJS** behaves as if a switch were a chain of comparisons: its cost grows linearly with the number of cases (flat-8 227 ns,
  flat-1024 10.4 µs). Here splitting is a large win, and smaller leaves are
  better: split8 beats split60 by about 35%, and leaves of 2 or 4 cases
  are slightly faster still (306 ns at 128 cases, against 341 for 8).

## Switch sizes in real programs

The programs below were compiled with `--opt 2 --set switch_size=100000`
(no splitting) and their `switch` statements counted with acorn. herd,
catala, stanc3 and alt-ergo are OCaml 5.4 bytecode, compiled with
js_of_ocaml 6.4.0; the others are OCaml 5.5 bytecode, compiled with the
current compiler. The range is the largest case value minus the
smallest. It leaves out the runtime's `caml_compare`, which has a
hand-written switch on tags (19 cases, 247 to 12520) in every program.
"Max cases" includes herd's 558-case switch, a `for (;;) switch`
dispatch loop that `build_switch` does not produce.

| Program | Switches | > 60 cases | > 128 | > 256 | Max cases | Max range |
|---|--:|--:|--:|--:|--:|--:|
| ocamlc | 1229 | 3 | 0 | 0 | 105 | 103 |
| ppx_js tests | 1680 | 3 | 0 | 0 | 105 | 103 |
| js_of_ocaml | 628 | 4 | 2 | 0 | 155 | 157 |
| lwt toplevel | 2903 | 9 | 2 | 0 | 155 | 157 |
| fiat-crypto (tl1) | 1597 | 7 | 2 | 0 | 155 | 157 |
| catala | 928 | 9 | 0 | 0 | 120 | 552 |
| stanc3 | 807 | 4 | 1 | 1 | 448 | 728 |
| alt-ergo | 2082 | 31 | 4 | 2 | 342 | 619 |
| herd | 4509 | 45 | 17 | 2 | 558 | 1189 |
| wasm_of_ocaml | 2620 | 105 | 5 | 2 | 711 | 1366 |

Few switches have more than 60 cases: 4% in wasm_of_ocaml, at most 1.5%
in the other programs. The largest one in wasm_of_ocaml is the `message`
function of wax-lib's `parser_messages.ml`, which Menhir generates from
`parser_messages.messages`. Three switches have a range above 1000: 711
cases over 0..1366 and 569 cases over 0..1054 in wasm_of_ocaml, and 361
cases over 0..1189 in herd. The JSC interpreter cliff applies to such
sparse switches too (`--useJIT=false`, 1 case in 2: 66 ns at range 1000,
237 ns at 1002; the 711-case shape: 294 ns flat, 105 ns split at 60, 91 ns
split at 300).

## Verdict

**Not validated, obsolete** for V8, SpiderMonkey and JavaScriptCore. The
limit of 60 dates from Crankshaft. In these measurements, splitting brings
no gain on current V8 and SpiderMonkey in any tier, nor on JavaScriptCore
for switches whose case values span at most 1000.

Implemented in #2612: the default `switch_size` is now 256, and switches
spanning more than 1000 are split too. Compiled with `--set
switch_size=256`, the four programs above with switches of more than 256
cases have no remaining switch from `build_switch` spanning more than 1000
(the widest spans 946, in herd). QuickJS is the one engine where splitting
pays off: leaves of 2 to 8 cases were the fastest measured.

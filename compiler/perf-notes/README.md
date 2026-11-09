# Performance notes

These notes record choices made by the Js_of_ocaml compiler and runtime
for the performance of the generated code, with the measurements that
check them on current JavaScript engines. Each choice has its own
sub-directory, with a `README.md` and its benchmarks. The `README.md`
describes what the compiler does, the engine mechanism behind it, the
benchmark, its results, and a verdict:

- **validated**: the benchmark shows the expected benefit on current
  engines;
- **not validated**: no measurable effect (the choice may be kept for
  other reasons);
- **counterproductive**: the alternative is faster on current engines.

When adding an entry, create a sub-directory following the same
structure, and list it in the summary below. Where possible, also measure
a real program, since micro-benchmarks exaggerate effects in both
directions. Only state what was measured or can be checked in a source
(link to it).

## Running the benchmarks

The benchmarks are plain JavaScript files that mimic the shape of the
generated code. On Node.js, they use the harness in `common.js`, which
runs each variant in its own process, so that type feedback is not shared
between variants:

```
node compiler/perf-notes/switch/switch.js             # all variants, as a table
node compiler/perf-notes/switch/switch.js flat-128    # one variant, as JSON
V8_FLAGS="--no-turbofan --no-maglev --no-sparkplug" node compiler/perf-notes/switch/switch.js
PROCS=5 node compiler/perf-notes/switch/switch.js     # more processes per variant
```

`V8_FLAGS` is passed to the child processes; the example above measures
the interpreter (Ignition) only. `PROCS` (default 3) sets the number of
processes per variant. Each process warms up for 300 ms, then times 10
rounds, and the table reports the median over all rounds of all processes.

Unless stated otherwise, the numbers in these notes were measured on an
8-core x86-64 Linux machine.

## Summary

| Choice | Where | Verdict |
|---|---|---|
| [Switch size limit](switch/README.md) | `config.ml`, `switch_size`; `generate.ml`, `build_switch` | **not validated** (obsolete; changed in #2612) |

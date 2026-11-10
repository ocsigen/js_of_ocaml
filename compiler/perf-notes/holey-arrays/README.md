# Holey arrays

Part of the [performance notes](../README.md). Benchmark:
[`holey.js`](holey.js).

```
node compiler/perf-notes/holey-arrays/holey.js
node --allow-natives-syntax compiler/perf-notes/holey-arrays/holey.js kinds
```

## What the compiler does

OCaml blocks are JavaScript arrays. V8 distinguishes _packed_ arrays,
which have a value at every index, from _holey_ arrays, which may have
missing elements (holes). Until #2609, Js_of_ocaml created holey arrays
in several places:

- the dead fields of a block literal, whose value is never read, were
  compiled to holes: `[0, a, , b]` (`compiler/lib/generate.ml`,
  `deadcode_sentinel`, since the global dead code elimination of #1503);
- runtime functions allocated blocks with `new Array(n)`, which creates
  an array of `n` holes, then wrote every element in a loop: `Array.make`
  (`caml_make_vect`), `Obj.new_block`, `Array.sub`, `Array.append`, the
  float arrays read by the unmarshaller, `Str` results, `Sys.readdir`,
  ...;
- `caml_update_dummy` (recursive values) filled an empty array from the
  end.

Since #2609, dead fields are compiled to `0`, and the runtime allocates
packed arrays (`runtime/js/array.js`):

- `caml_array_alloc_packed` creates `new Array(n + 1).fill(init)`, then
  writes the tag;
- `caml_floatarray_alloc_packed` creates `new Array(n + 1).fill(-0)`,
  then writes the elements. Filling with `-0`, which is not a small
  integer, makes V8 store the elements as unboxed doubles from the start.
  `Specialize_js` compiles `Array.make` with a constant float to
  `caml_array_make_float`, which uses this function;
- `Array.sub` uses `slice` and `Array.append` uses `concat`, which keep
  the representation of their arguments;
- the unmarshaller allocates each block with its final size, and
  `caml_update_dummy` fills the array from the start.

## V8 mechanism

V8 gives each array an _elements kind_, which says how its elements are
stored: small integers (`SMI`), unboxed doubles (`DOUBLE`) or arbitrary
values (`ELEMENTS`), each in a packed or a holey variant. These six
kinds are ordered from the most specific (`PACKED_SMI_ELEMENTS`) to the
most general (`HOLEY_ELEMENTS`), and an array only moves towards more
general kinds (with one exception, `fill`, described below). Storing a
float into a `SMI` array converts it to `DOUBLE`, which copies the
elements to a new buffer; storing an object converts it to `ELEMENTS`.
A holey array stays holey, even once every element has been written.

The elements kind is part of the array's _map_, V8's description of the
layout of an object. Optimized code checks the map before an access. An
access that has seen arrays of several kinds must test several maps, and
code optimized for one map is thrown away ("deoptimized", with the
reason "wrong map") when it meets another.

Holey arrays are contagious: a packed literal starts creating holey
arrays once the arrays it creates reach an assignment `b[i] = v` that
also receives holey arrays. This happens in two steps:

1. V8 records, for each assignment `b[i] = v` in the code, the maps of
   the arrays it has received (its _inline cache_). Once it has received
   arrays of several elements kinds, it converts each array it gets to
   the most general of these kinds before writing
   ([`KeyedStoreIC::StoreElementPolymorphicHandlers`](https://github.com/v8/v8/blob/9a59d631cd1de170232e819cdbe0a8e819dc477e/src/ic/ic.cc#L2958),
   whose comment reads "The code below is doing pessimistic elements
   transitions"). An update of a mutable field that receives both packed
   blocks from a literal and holey blocks from the runtime converts the
   packed blocks to holey ones.
2. When a recently allocated array (one still in the young generation)
   changes elements kind, V8 also changes the kind recorded for the place
   in the code that allocated it, its _allocation site_. For a literal,
   V8 converts the template array that the literal copies, so every array
   the literal creates afterwards is holey. The code that depended on the
   old kind is deoptimized
   ([`JSObject::TransitionElementsKind`](https://github.com/v8/v8/blob/9a59d631cd1de170232e819cdbe0a8e819dc477e/src/objects/js-objects.cc#L5760)
   →
   [`UpdateAllocationSite`](https://github.com/v8/v8/blob/9a59d631cd1de170232e819cdbe0a8e819dc477e/src/objects/js-objects.cc#L5729)
   →
   [`AllocationSite::DigestTransitionFeedback`](https://github.com/v8/v8/blob/9a59d631cd1de170232e819cdbe0a8e819dc477e/src/objects/allocation-site-inl.h#L231)).

The arrays of this literal then reach other assignments, which convert
the arrays of other literals, and so on. `holey.js kinds` shows the
effect: the literal `[0, k, k]` creates `PACKED_ELEMENTS` arrays until
some of them go through a function doing `b[1] = v` that also receives
arrays from `new Array(3)`. From then on, the same literal creates
`HOLEY_ELEMENTS` arrays. In one run of ocamlc, 520 block literals created
holey arrays (see Real programs below). Removing the holes from the
literals and fixing `caml_update_dummy` made no difference on ocamlc: the
arrays allocated by the runtime were enough to make the literals holey,
so all the sources of holey arrays had to be removed.

Two more details matter for the runtime functions:

- The `new Array(n)` in a runtime function is a single allocation site
  for all the arrays this function creates, so V8 records there the most
  general kind it has seen. Once `Array.make` has created an array of
  objects, it creates `ELEMENTS` arrays, in which each float is stored as
  a separately allocated number. `fill` sets the elements kind from the
  fill value when it replaces all the elements
  ([`TryFastArrayFill`](https://github.com/v8/v8/blob/9a59d631cd1de170232e819cdbe0a8e819dc477e/src/builtins/builtins-array.cc#L274)).
- Appending an element (with `push`, or by writing at index `length`)
  grows the buffer holding the elements to `old + old / 2 + 16` slots
  ([`NewElementsCapacity`](https://github.com/v8/v8/blob/9a59d631cd1de170232e819cdbe0a8e819dc477e/src/objects/js-objects.h#L694)):
  a block built from `[tag]` by appending 3 fields has room for 19
  elements.

`Array.prototype.fill` is implemented in C++. V8's optimizing compiler,
TurboFan, replaces calls to several array functions with inline code
(`src/compiler/js-call-reducer.cc`), but not calls to `fill`, so each call
goes through a call into C++.

## Benchmark

`holey.js` compares four ways of creating an array, named by suffix:
`-loop` is the runtime before #2609 (`new Array(n)`, then a loop),
`-fill` the current one (`fill`), `-float` the current one for float
arrays (`fill(-0)`, then a loop), and `-literal` an alternative for
small blocks: a `switch` on the size, with one literal per size up to 8
(`[0, init, init, init, init]`). The variants are:

- `kernel-M`: the inner loop of fannkuch_redux (copying a permutation,
  reversing prefixes) on two int arrays created once with `Array.make`.
- `alloc-M`: `Array.make 4 x` in a loop.
- `float-M`: `Array.make 16 0.`, then writing floats into the array and
  summing them.
- `list-K`: updating and summing a list of 1000 records
  `[0, key, dead, next]` built at 4 literals, with the dead field
  compiled to `0` (`packed`), to a hole (`holey`), or half of each
  (`both`). `contaminated` uses packed literals, but the update loop has
  first run on a list that also contains blocks from `new Array(3)`, as
  in step 1 above.

In a program, a single runtime function creates the arrays for all the
calls to `Array.make`, with values of all types. The `-mixed` variants
first call this function with integers, floats and objects (the float
function with floats only), so that its allocation site has the most
general kind. The variants without `-mixed` correspond to a small
program, which creates a single kind of array.

## Results

Node 26 (V8 canary of February 2026), each variant in 9 processes,
interleaved. Times are in ns per iteration: the median over the
processes of the per-process median, with the minimum in parentheses.

| Variant | `loop` (before) | `fill` (now) | `float` / `literal` |
|---|--:|--:|--:|
| kernel | 116.9 (108.3) | 93.9 (91.7) | |
| kernel-mixed | 113.2 (107.0) | 99.0 (96.2) | |
| alloc | 12.6 (11.9) | 55.1 (53.0) | `literal` 11.9 (11.4) |
| alloc-mixed | 65.5 (61.9) | 59.6 (54.2) | `literal` 12.3 (11.5) |
| float | 35.8 (33.0) | 169.3 (161.7) | `float` 101.9 (90.3) |
| float-mixed | 170.5 (153.4) | 127.0 (112.6) | `float` 112.7 (100.2) |

| Variant | packed | holey | both | contaminated |
|---|--:|--:|--:|--:|
| list | 6.8 (6.5) | 7.7 (7.1) | 7.4 (7.2) | 7.4 (6.9) |

- Loops over packed arrays are 13% to 20% faster (`kernel`), close to
  the gain on fannkuch_redux below. In optimized code, reading an element
  of a holey array includes a test for a hole.
- Holey literals cost 9% to 13% (`list`), and the contaminated packed
  literals cost as much as holey ones. Here the elements kinds stop
  changing after the start, so this benchmark does not show the repeated
  deoptimizations seen in real programs.
- `fill` costs about 40 ns per call on a small block, the cost of the
  call into C++ (`alloc`: 55 ns against 12.6). The old loop is just as
  slow once its allocation site has the most general kind
  (`alloc-mixed`). One literal per size is as fast as the old loop in a
  small program, and 5× faster than both in a large one.
- Float arrays: with `fill(0)`, writing the first float converts the
  array to `DOUBLE` and copies it (`float-fill`, 169 ns), which
  `fill(-0)` avoids (102 ns). In a small program that only creates float
  arrays with `Array.make`, the old runtime was faster (36 ns): the
  allocation site of its `new Array(n)` had the kind `HOLEY_DOUBLE`, so
  no conversion took place. Once that site has also seen other values
  (`float-mixed`), each float written is allocated separately: 171 ns,
  against 113 ns now.

### Real programs

Measured when #2609 was written, with node 26:

- ocamlc (`benchmarks/benchmark-ocamlc` setup): the number of holey
  blocks allocated goes from 738k, created by 520 literals, to 0.
  Deoptimizations caused by a failed check go from 654 to 298, and those
  with the reason "wrong map" from 455 to 67. Before, some functions were
  deoptimized over and over at the same field access (85 times for one
  of them), while the inline cache of this access kept the same two maps,
  packed and holey. Run time is 2% to 4% lower, and peak memory 4% lower
  (the unmarshaller used to build blocks by appending one field at a
  time).
- Small benchmarks (`benchmarks/`): fannkuch_redux −17%, soli −11%,
  fft −4%, fannkuch_redux_2 −4%, geometric mean −2.3%, no slowdown.

## Other engines

SpiderMonkey and JavaScriptCore do not record elements kinds in
allocation sites, as V8 does. On shells other than Node.js, `holey.js`
runs one variant per process. Each value below is the median of 5 runs,
in ns per iteration, with the minimum and maximum in parentheses. The
engines are SpiderMonkey 159.0b5 (`sm`) and JavaScriptCore 324672
(`jsc`), installed with jsvu.

| Variant | SM | JSC |
|---|--:|--:|
| kernel-loop | 150.3 (145-166) | 101.3 (94-111) |
| kernel-fill | 145.8 (142-168) | 105.7 (94-111) |
| kernel-loop-mixed | 156.6 (148-170) | 116.7 (103-119) |
| kernel-fill-mixed | 147.9 (146-169) | 106.9 (102-110) |
| alloc-loop | 18.4 (17-20) | 8.2 (8-8) |
| alloc-fill | 27.3 (26-32) | 32.5 (32-36) |
| alloc-literal | 16.6 (16-18) | 7.2 (7-8) |
| alloc-loop-mixed | 28.0 (24-31) | 8.6 (8-9) |
| alloc-fill-mixed | 30.5 (29-31) | 18.0 (18-20) |
| alloc-literal-mixed | 16.5 (16-17) | 7.4 (7-8) |
| float-loop | 83.9 (80-95) | 29.9 (29-34) |
| float-fill | 110.5 (102-121) | 39.4 (39-45) |
| float-float | 128.8 (123-145) | 53.6 (46-55) |
| float-loop-mixed | 92.2 (89-106) | 47.8 (41-48) |
| float-fill-mixed | 109.5 (93-121) | 51.4 (45-61) |
| float-float-mixed | 125.0 (123-143) | 45.8 (42-53) |
| list-packed | 8.1 (8-8) | 6.7 (6-7) |
| list-holey | 8.3 (8-9) | 7.5 (7-8) |
| list-both | 8.0 (8-9) | 8.9 (9-10) |
| list-contaminated | 8.2 (8-9) | 6.5 (6-7) |

- Packed arrays make no difference to the loop over arrays (`kernel`):
  the differences are within the noise on both engines.
- `fill` makes small allocations slower: +9 ns on SpiderMonkey and
  +24 ns on JavaScriptCore in a small program (`alloc`), +2.5 ns and
  +9 ns in a large one. One literal per size is the fastest on both
  engines.
- `Array.make` with a float is 35% to 55% slower on SpiderMonkey with the
  current runtime (`float-float` against `float-loop`). On
  JavaScriptCore, it is up to 80% slower in a small program, and as fast
  in a large one.
- On JavaScriptCore, literals with holes cost 12% (`list-holey`), and
  mixing packed and holey arrays at the same accesses 33% (`list-both`).
  The contamination only occurs on V8.

## Verdict

**Validated** on V8. Loops over holey arrays are slower. More
importantly, an assignment that receives both packed and holey arrays
converts the packed ones and records the conversion in their allocation
site, so the holey arrays created by the runtime made packed block
literals holey (520 literals in ocamlc), with accesses seeing several
maps and repeated deoptimizations. Removing all the holey arrays makes
ocamlc 2% to 4% faster and the small benchmarks 2.3% faster. Compiling
dead fields to `0` also helps on JavaScriptCore.

`fill` makes small blocks slower to allocate, by about 40 ns on V8. In a
small program that only creates float arrays, `Array.make` with a float
is also slower than before (102 ns against 36 for 16 floats). One literal
per size for small blocks avoids the first cost on all three engines
(`alloc-literal`). It is not implemented, and was not measured for float
arrays. On SpiderMonkey and JavaScriptCore, the runtime changes bring no
gain to offset these costs.

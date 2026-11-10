// Js_of_ocaml
// http://www.ocsigen.org/js_of_ocaml/
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

// Holey and packed arrays: V8 records whether an array may contain holes
// (missing elements) as part of its elements kind, which says how its
// elements are stored. Js_of_ocaml used to create holey arrays for blocks
// with dead fields ([0, a, , b]) and in the runtime ([new Array(n)]
// followed by a loop). We compare the old and the current ways of
// creating blocks, and the code using them (see README.md).
//
// On Node.js, with the harness of ../common.js (one process per variant):
//
//   node holey.js                          all variants
//   VARIANTS=kernel-loop,kernel-fill node holey.js
//
// On other JavaScript shells, one variant per run, printing its time in
// ns/iter (the median of 10 rounds):
//
//   sm holey.js kernel-fill                (SpiderMonkey)
//   jsc holey.js -- kernel-fill            (JavaScriptCore)
//
// With V8, the elements kinds can be printed:
//
//   node --allow-natives-syntax holey.js kinds

// Array.make, as in the runtime before #2609 (caml_make_vect): the array
// is holey, although every element gets written
function make_loop(len, init) {
  var l = (len + 1) | 0;
  var b = new Array(l);
  b[0] = 0;
  for (var i = 1; i < l; i++) b[i] = init;
  return b;
}

// Array.make, as in the current runtime (caml_array_alloc_packed): filling
// the whole array makes it packed
function make_fill(len, init) {
  var b = new Array(len + 1).fill(init);
  b[0] = 0;
  return b;
}

// An alternative for small arrays: one literal per size (packed, and
// allocated inline by optimized code)
function make_literal(len, init) {
  switch (len) {
    case 1:
      return [0, init];
    case 2:
      return [0, init, init];
    case 3:
      return [0, init, init, init];
    case 4:
      return [0, init, init, init, init];
    case 5:
      return [0, init, init, init, init, init];
    case 6:
      return [0, init, init, init, init, init, init];
    case 7:
      return [0, init, init, init, init, init, init, init];
    case 8:
      return [0, init, init, init, init, init, init, init, init];
  }
  return make_fill(len, init);
}

// Array.make with a float, as in the current runtime
// (caml_floatarray_alloc_packed): -0 is not a small integer, so the array
// gets double elements from the start
function make_float(len, init) {
  var b = new Array(len + 1).fill(-0);
  b[0] = 0;
  for (var i = 1; i <= len; i++) b[i] = init;
  return b;
}

const makers = {
  loop: make_loop,
  fill: make_fill,
  literal: make_literal,
  float: make_float,
};

// In a program, one runtime function creates all the arrays, and V8
// records at each place where it allocates an array the most general
// elements kind it has seen: once Array.make has created an array of
// objects, [new Array(l)] in [make_loop] creates arrays of arbitrary
// values, in which each float is allocated separately. The [-mixed] variants reproduce this by first calling the
// function with integers, floats and objects ([make_float] only with
// floats); the other variants only use the function for the benchmark,
// as in a small program.
function warm_up(make) {
  const inits = make === make_float ? [0, 1.5] : [0, 1.5, {}];
  for (let k = 0; k < 100; k++)
    for (const init of inits) for (const len of [1, 4, 10, 16]) make(len, init);
}

// xorshift32 inputs, so that nothing is constant
function inputs(n, mod) {
  const a = new Int32Array(n);
  let seed = 2463534242;
  for (let i = 0; i < n; i++) {
    seed ^= seed << 13;
    seed ^= seed >>> 17;
    seed ^= seed << 5;
    a[i] = (seed >>> 0) % mod;
  }
  return a;
}

// kernel-M: the inner loop of fannkuch_redux on two int arrays created
// once with Array.make: copy a permutation, then reverse prefixes until
// the first element is 0. One iteration = one permutation.
function kernel(make) {
  const n = 10;
  const perm = make(n, 0);
  const tmp = make(n, 0);
  const rnd = inputs(4096, 1 << 30);
  return (iters) => {
    let acc = 0;
    for (let r = 0; r < iters; r++) {
      // a pseudo-random permutation of 0 .. n-1
      for (let i = 1; i <= n; i++) perm[i] = i - 1;
      let s = rnd[r & 4095];
      for (let i = n; i > 1; i--) {
        const j = 1 + (s % i);
        s = (s / i) | 0;
        const t = perm[i];
        perm[i] = perm[j];
        perm[j] = t;
      }
      for (let i = 1; i <= n; i++) tmp[i] = perm[i];
      let k = tmp[1];
      while (k !== 0) {
        for (let i = 1, j = k + 1; i < j; i++, j--) {
          const t = tmp[i];
          tmp[i] = tmp[j];
          tmp[j] = t;
        }
        acc++;
        k = tmp[1];
      }
    }
    return acc;
  };
}

// alloc-M: Array.make 4 x, the result being kept alive for a while
function alloc(make) {
  const keep = make_fill(1023, 0);
  return (iters) => {
    for (let r = 0; r < iters; r++) keep[1 + (r & 1023)] = make(4, r);
    let acc = 0;
    for (let i = 1; i <= 1024; i++) acc = (acc + keep[i][2]) | 0;
    return acc;
  };
}

// float-M: Array.make 16 0., then fill the array with floats and sum
// them. With [loop] and [fill], the array starts with small integer
// elements and is converted (and copied) on the first float store.
function float(make) {
  return (iters) => {
    let acc = 0;
    for (let r = 0; r < iters; r++) {
      const a = make(16, 0);
      for (let i = 1; i <= 16; i++) a[i] = r * 0.5 + i;
      for (let i = 1; i <= 16; i++) acc += a[i];
    }
    return acc;
  };
}

// list-K: a list of 1000 records { key; dead; next } built at 4 literal
// sites, as generated for [{ key; dead = _; next }] when the [dead] field
// is never read: [0, k, , n] before #2609 (holey), [0, k, 0, n] now
// (packed); both: half the sites of each kind. One iteration = one
// record visited twice (an update, then a sum).
//
// list-contaminated: the literals are packed, but the update loop has
// first been run on a list mixing blocks from these literals and blocks
// allocated by the old runtime (make_loop). The assignment [l[1] = ...]
// then converts the packed blocks it gets to holey ones, and V8 records
// the conversion at the literal that created them: the literals now
// create holey arrays, although they contain no hole.
function list(kind) {
  const site = (hole) =>
    new Function(
      `"use strict"; return function (k, n) { return [0, k, ${hole ? "" : "0"}, n]; }`,
    )();
  const sites = [];
  for (let s = 0; s < 4; s++)
    sites.push(site(kind === "holey" || (kind === "both" && s & 1)));
  const update = (head) => {
    for (let l = head; l !== 0; l = l[3]) l[1] = (l[1] + 1) | 0;
  };
  if (kind === "contaminated") {
    let l = 0;
    for (let i = 0; i < 100; i++) {
      l = sites[i & 3](i, l);
      const b = make_loop(3, 0);
      b[1] = i;
      b[3] = l;
      l = b;
    }
    for (let k = 0; k < 10; k++) update(l);
  }
  let l = 0;
  for (let i = 0; i < 1000; i++) l = sites[i & 3](i, l);
  const head = l;
  return (iters) => {
    let acc = 0;
    for (let r = 0; r < iters; r += 1000) {
      update(head);
      for (let l = head; l !== 0; l = l[3]) acc = (acc + l[1]) | 0;
    }
    return acc;
  };
}

function make(name) {
  const [kind, arg, mixed] = name.split("-");
  if (mixed === "mixed" && makers[arg]) warm_up(makers[arg]);
  else if (mixed !== undefined) throw new Error(`bad variant ${name}`);
  switch (kind) {
    case "kernel":
      if (makers[arg]) return kernel(makers[arg]);
      break;
    case "alloc":
      if (makers[arg]) return alloc(makers[arg]);
      break;
    case "float":
      if (makers[arg]) return float(makers[arg]);
      break;
    case "list":
      if (["packed", "holey", "both", "contaminated"].includes(arg))
        return list(arg);
      break;
  }
  throw new Error(`bad variant ${name}`);
}

const ALL = [];
for (const [kind, iters, ms] of [
  ["kernel", 2e6, ["loop", "fill"]],
  ["alloc", 2e6, ["loop", "fill", "literal"]],
  ["float", 2e6, ["loop", "fill", "float"]],
])
  for (const suffix of ["", "-mixed"])
    for (const m of ms) ALL.push([`${kind}-${m}${suffix}`, iters]);
for (const k of ["packed", "holey", "both", "contaminated"])
  ALL.push([`list-${k}`, 2e7]);

// With --allow-natives-syntax: the elements kinds of the arrays created by
// each function, and how a store site shared with holey arrays makes a
// literal site holey
function kinds() {
  const holey = new Function("a", "return %HasHoleyElements(a)");
  const smi = new Function("a", "return %HasSmiElements(a)");
  const dbl = new Function("a", "return %HasDoubleElements(a)");
  const kind = (a) =>
    (holey(a) ? "HOLEY_" : "PACKED_") +
    (smi(a) ? "SMI_" : dbl(a) ? "DOUBLE_" : "") +
    "ELEMENTS";
  for (const m of Object.keys(makers))
    console.log(`make_${m}(4, 0)`.padEnd(20), kind(makers[m](4, 0)));
  // biome-ignore lint/suspicious/noSparseArray: a dead field, before #2609
  console.log("[0, 1, , 2]".padEnd(20), kind([0, 1, , 2]));
  // A literal site, and a store site (a mutable field update) that also
  // sees blocks allocated by the old runtime
  const site = (k) => [0, k, k];
  const set = (b, v) => {
    b[1] = v;
  };
  const before = site({});
  for (let i = 0; i < 100; i++) {
    set(site({}), i);
    set(make_loop(2, {}), i);
  }
  console.log("literal site, before", kind(before));
  console.log("literal site, after ", kind(site({})));
}

if (typeof require === "function" && typeof module === "object") {
  if (process.argv[2] === "kinds") kinds();
  else {
    const { main } = require("../common.js");
    let names;
    // A single variant, as a child process of the harness or directly
    if (process.argv[2] !== undefined) names = [process.argv[2]];
    else if (process.env.VARIANTS) names = process.env.VARIANTS.split(",");
    else names = ALL.map((v) => v[0]);
    main(
      names.map((name) => ({
        name,
        iters: (ALL.find((v) => v[0] === name) || [name, 2e6])[1],
        make: () => make(name),
      })),
      __filename,
    );
  }
} else {
  const g = globalThis;
  const out = typeof g.print === "function" ? g.print : console.log;
  const now =
    typeof performance === "object"
      ? () => performance.now()
      : () => Date.now();
  const args = typeof g.scriptArgs === "object" ? g.scriptArgs : g.arguments;
  const name = args[0]?.endsWith(".js") ? args[1] : args[0];
  const run = make(name);
  // Calibrate a round lasting about 50 ms, warm up, take the median of 10
  let iters = 8000; // a multiple of the list length
  for (;;) {
    const t = now();
    run(iters);
    if (now() - t > 50) break;
    iters *= 2;
  }
  const t0 = now();
  while (now() - t0 < 300) run(iters);
  const r = [];
  for (let k = 0; k < 10; k++) {
    const t = now();
    run(iters);
    r.push(((now() - t) * 1e6) / iters);
  }
  r.sort((a, b) => a - b);
  out(`${name} ${((r[4] + r[5]) / 2).toFixed(2)}`);
}

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

// Size of switch statements: js_of_ocaml splits a large switch into a
// binary tree of [if (k <= x)] tests whose leaves are smaller switches
// (generate.ml, [build_switch]). We compare a flat dense switch with the
// split one, for several numbers of cases.
//
// A variant is flat-N (one switch with N cases) or splitM-N (the N cases
// split in halves until the leaves have at most M cases; split-N stands
// for split60-N, the limit before #2612).
//
// On Node.js, with the harness of ../common.js (one process per variant):
//
//   node switch.js                         flat-N and split-N for N in
//                                          32, 60, 128, 256, 1024
//   SIZES=100,200 node switch.js           choose the sizes
//   VARIANTS=flat-128,split8-128 node switch.js
//
// On other JavaScript shells, one variant per run, printing its time in
// ns/iter (the median of 10 rounds):
//
//   js140 switch.js split-128              (SpiderMonkey)
//   jsc switch.js -- split8-128            (JavaScriptCore)
//   qjs switch.js flat-128                 (QuickJS)

function cases(lo, hi) {
  let s = "switch (x) {\n";
  for (let k = lo; k < hi; k++)
    s += `case ${k}: return (y + ${k * 7 + 1}) | 0;\n`;
  return `${s}default: return y;\n}\n`;
}

function split(max, lo, hi) {
  if (hi - lo <= max) return cases(lo, hi);
  const mid = (lo + hi) >> 1;
  return `if (${mid} <= x) {\n${split(max, mid, hi)}} else {\n${split(max, lo, mid)}}\n`;
}

// Returns the hot loop for a variant: [run(iters)] calls the function
// [iters] times on pseudo-random case indices and returns a checksum.
function make(name) {
  const [kind, size] = name.split("-");
  const n = +size;
  let body;
  if (kind === "flat") body = cases(0, n);
  else if (kind.startsWith("split"))
    body = split(kind === "split" ? 60 : +kind.slice(5), 0, n);
  if (body === undefined || !(n > 0)) throw new Error(`bad variant ${name}`);
  const f = new Function("x", "y", `"use strict";\n${body}`);
  const inputs = new Int32Array(4096);
  // xorshift32; the case index is unpredictable for any n
  let seed = 2463534242;
  for (let i = 0; i < inputs.length; i++) {
    seed ^= seed << 13;
    seed ^= seed >>> 17;
    seed ^= seed << 5;
    inputs[i] = (seed >>> 0) % n;
  }
  return (iters) => {
    let acc = 0;
    for (let i = 0; i < iters; i++) acc = f(inputs[i & 4095], acc);
    return acc;
  };
}

function shell(name) {
  const out = typeof print === "function" ? print : console.log;
  const now =
    typeof performance === "object"
      ? () => performance.now()
      : () => Date.now();
  const run = make(name);
  // Calibrate a round lasting about 50 ms, warm up, take the median of 10
  let iters = 4096;
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

if (typeof require === "function" && typeof module === "object") {
  const { main } = require("../common.js");
  let names;
  // A single variant, as a child process of the harness or directly
  if (process.argv[2] !== undefined) names = [process.argv[2]];
  else if (process.env.VARIANTS) names = process.env.VARIANTS.split(",");
  else {
    names = [];
    for (const n of (process.env.SIZES || "32,60,128,256,1024").split(",")) {
      names.push(`flat-${n}`);
      if (+n > 60) names.push(`split-${n}`);
    }
  }
  main(
    names.map((name) => ({ name, iters: 1e7, make: () => make(name) })),
    __filename,
  );
} else {
  const g = globalThis;
  const args = typeof g.scriptArgs === "object" ? g.scriptArgs : g.arguments;
  // QuickJS puts the script name first in scriptArgs
  shell(args[0]?.endsWith(".js") ? args[1] : args[0]);
}

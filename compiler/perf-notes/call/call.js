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

// Calling a function stored in a block (typically a module field):
// js_of_ocaml used to emit [M[1].call(null, x, y)] rather than
// [M[1](x, y)], so that the call is a plain function call and not a method
// call with the block as receiver (see README.md and generate.ml,
// [apply_directly]).
//
// A variant is FORM-mono (always the same callee at the call site) or
// FORM-poly (8 different callees), where FORM is
//   method      M[1](x, y)
//   call-null   M[1].call(null, x, y)   (js_of_ocaml before #2613)
//   comma       (0, M[1])(x, y)
//   local       const f = M[1]; f(x, y)
//
// On Node.js, with the harness of ../common.js (one process per variant):
//
//   node call.js                      all variants
//   VARIANTS=method-mono,local-mono node call.js
//
// On other JavaScript shells, one variant per run, printing its time in
// ns/iter (the median of 10 rounds):
//
//   js140 call.js method-mono              (SpiderMonkey)
//   jsc call.js -- method-mono             (JavaScriptCore)
//   qjs call.js method-mono                (QuickJS)

// A module block with 8 functions, as produced for a compilation unit
function make_module() {
  const M = [0];
  for (let k = 0; k < 8; k++)
    M.push(
      new Function("x", "y", `"use strict"; return (x * ${k + 3} + y) | 0;`),
    );
  return M;
}

const loops = {
  "method-mono": (M) => (n) => {
    let acc = 0;
    for (let i = 0; i < n; i++) acc = M[1](i, acc);
    return acc;
  },
  "call-null-mono": (M) => (n) => {
    let acc = 0;
    for (let i = 0; i < n; i++) acc = M[1].call(null, i, acc);
    return acc;
  },
  "comma-mono": (M) => (n) => {
    let acc = 0;
    for (let i = 0; i < n; i++) acc = (0, M[1])(i, acc);
    return acc;
  },
  "local-mono": (M) => (n) => {
    let acc = 0;
    for (let i = 0; i < n; i++) {
      const f = M[1];
      acc = f(i, acc);
    }
    return acc;
  },
  "method-poly": (M) => (n) => {
    let acc = 0;
    for (let i = 0; i < n; i++) acc = M[1 + (i & 7)](i, acc);
    return acc;
  },
  "call-null-poly": (M) => (n) => {
    let acc = 0;
    for (let i = 0; i < n; i++) acc = M[1 + (i & 7)].call(null, i, acc);
    return acc;
  },
  "comma-poly": (M) => (n) => {
    let acc = 0;
    for (let i = 0; i < n; i++) acc = (0, M[1 + (i & 7)])(i, acc);
    return acc;
  },
  "local-poly": (M) => (n) => {
    let acc = 0;
    for (let i = 0; i < n; i++) {
      const f = M[1 + (i & 7)];
      acc = f(i, acc);
    }
    return acc;
  },
};

function make(name) {
  const loop = loops[name];
  if (!loop) throw new Error(`bad variant ${name}`);
  return loop(make_module());
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
  if (process.argv[2] !== undefined) names = [process.argv[2]];
  else if (process.env.VARIANTS) names = process.env.VARIANTS.split(",");
  else names = Object.keys(loops);
  main(
    names.map((name) => ({ name, iters: 2e7, make: () => make(name) })),
    __filename,
  );
} else {
  const g = globalThis;
  const args = typeof g.scriptArgs === "object" ? g.scriptArgs : g.arguments;
  // QuickJS puts the script name first in scriptArgs
  shell(args[0]?.endsWith(".js") ? args[1] : args[0]);
}

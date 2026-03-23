// biome-ignore lint/suspicious/noRedundantUseStrict: needed for non-module execution
"use strict";

// Benchmark: call.bind(DV.getInt32) vs (x,y,z) => x.getInt32(y,z) as Wasm imports.
// Works with Node.js, SpiderMonkey (sm), and JavaScriptCore (jsc).
//
// The two variants use wasm modules with different wire bytes (different custom
// sections) so that V8 does not share a NativeModule between them, which would
// revoke well-known-import optimizations.

var DV = DataView.prototype;
var call = Function.prototype.call;

// Portable output
var log =
  typeof console !== "undefined"
    ? function (s) {
        console.log(s);
      }
    : print;

// Portable high-resolution timer (returns milliseconds)
var now =
  typeof performance !== "undefined" && performance.now
    ? function () {
        return performance.now();
      }
    : typeof preciseTime !== "undefined"
      ? function () {
          return preciseTime() * 1000;
        }
      : function () {
          return Date.now();
        };

var WARMUP = 1e6;
var ITERS = 1e8;
var ROUNDS = 5;

// Wasm module: imports (env.fn : externref * i32 * i32 -> i32),
// exports bench(externref, i32) -> i32.
// Loop calls fn(dv, (i*4)&1020, 1) and sums the results.
// Two copies with distinct custom sections so V8 gives each its own NativeModule.
var wasmA = new Uint8Array([
  0, 97, 115, 109, 1, 0, 0, 0, 1, 14, 2, 96, 3, 111, 127, 127, 1, 127, 96, 2,
  111, 127, 1, 127, 2, 10, 1, 3, 101, 110, 118, 2, 102, 110, 0, 0, 3, 2, 1, 1,
  7, 9, 1, 5, 98, 101, 110, 99, 104, 0, 1, 10, 45, 1, 43, 1, 2, 127, 3, 64, 32,
  2, 32, 0, 32, 3, 65, 2, 116, 65, 252, 7, 113, 65, 1, 16, 0, 106, 33, 2, 32, 3,
  65, 1, 106, 33, 3, 32, 3, 32, 1, 73, 13, 0, 11, 32, 2, 11, 0, 2, 1, 65,
]);
var wasmB = new Uint8Array([
  0, 97, 115, 109, 1, 0, 0, 0, 1, 14, 2, 96, 3, 111, 127, 127, 1, 127, 96, 2,
  111, 127, 1, 127, 2, 10, 1, 3, 101, 110, 118, 2, 102, 110, 0, 0, 3, 2, 1, 1,
  7, 9, 1, 5, 98, 101, 110, 99, 104, 0, 1, 10, 45, 1, 43, 1, 2, 127, 3, 64, 32,
  2, 32, 0, 32, 3, 65, 2, 116, 65, 252, 7, 113, 65, 1, 16, 0, 106, 33, 2, 32, 3,
  65, 1, 106, 33, 3, 32, 3, 32, 1, 73, 13, 0, 11, 32, 2, 11, 0, 2, 1, 66,
]);

var buf = new ArrayBuffer(1024);
var dv = new DataView(buf);
var u8 = new Uint8Array(buf);
for (var i = 0; i < u8.length; i++) u8[i] = i & 0xff;

var bound = call.bind(DV.getInt32);
var wrapper = function (x, y, z) {
  return x.getInt32(y, z);
};

function median(arr) {
  var sorted = arr.slice().sort(function (a, b) {
    return a - b;
  });
  return sorted[Math.floor(sorted.length / 2)];
}

function run(label, benchFn) {
  benchFn(dv, WARMUP);
  var times = [];
  for (var r = 0; r < ROUNDS; r++) {
    var t0 = now();
    benchFn(dv, ITERS);
    var t1 = now();
    times.push(t1 - t0);
  }
  return { label: label, times: times, med: median(times) };
}

var modBound = new WebAssembly.Instance(new WebAssembly.Module(wasmA), {
  env: { fn: bound },
});
var modWrapper = new WebAssembly.Instance(new WebAssembly.Module(wasmB), {
  env: { fn: wrapper },
});

var rBound = run("call.bind", modBound.exports.bench);
var rWrapper = run("wrapper  ", modWrapper.exports.bench);

log("getInt32 — call.bind(DV.getInt32) vs (x,y,z) => x.getInt32(y,z)");
log("");
log(
  rBound.label +
    "  " +
    rBound.times
      .map(function (t) {
        return t.toFixed(0) + "ms";
      })
      .join("  "),
);
log(
  rWrapper.label +
    "  " +
    rWrapper.times
      .map(function (t) {
        return t.toFixed(0) + "ms";
      })
      .join("  "),
);
log("");
log(rBound.label + "  median: " + rBound.med.toFixed(0) + "ms");
log(rWrapper.label + "  median: " + rWrapper.med.toFixed(0) + "ms");
var ratio = rBound.med / rWrapper.med;
log("");
log(
  "ratio (call.bind / wrapper): " +
    ratio.toFixed(2) +
    "  → " +
    (ratio < 0.95 ? "call.bind wins" : ratio > 1.05 ? "wrapper wins" : "~same"),
);

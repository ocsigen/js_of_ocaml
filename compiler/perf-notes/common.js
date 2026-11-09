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

// Micro-benchmark harness for the performance notes: each sub-directory
// describes one code generation choice in its README.md, with its benchmarks.
//
// A benchmark file calls [main(variants)], where each variant is
//   { name, iters, make: () => (n) => checksum }
// [make] builds the hot loop; the loop runs [n] iterations of the code
// shape under test and returns a value that depends on all of them, so
// that nothing is optimized away.
//
//   node bench.js            run every variant, print a table
//   node bench.js NAME       run a single variant, print one JSON line
//
// Each variant runs in its own process, so that type feedback, allocation
// sites and protectors are not shared between variants. V8 flags for the
// child processes can be passed through the V8_FLAGS environment variable,
// e.g. V8_FLAGS="--no-turbofan --no-maglev --no-sparkplug" to measure the
// interpreter only. PROCS (default 3) sets the number of processes per
// variant; the reported time is the median over all rounds of all processes.

const { execFileSync } = require("node:child_process");

const ROUNDS = 10;
const WARMUP_MS = 300;

function median(a) {
  const b = a.slice().sort((x, y) => x - y);
  const m = b.length >> 1;
  return b.length & 1 ? b[m] : (b[m - 1] + b[m]) / 2;
}

function measure(v) {
  const loop = v.make();
  const n = v.iters;
  let check;
  const start = performance.now();
  while (performance.now() - start < WARMUP_MS) check = loop(n);
  const times = [];
  for (let r = 0; r < ROUNDS; r++) {
    const t0 = performance.now();
    check = loop(n);
    times.push(((performance.now() - t0) * 1e6) / n);
  }
  return { name: v.name, times, check: String(check) };
}

function main(variants, file) {
  const arg = process.argv[2];
  if (arg !== undefined) {
    const v = variants.find((v) => v.name === arg);
    if (!v) throw new Error(`unknown variant ${arg}`);
    process.stdout.write(`${JSON.stringify(measure(v))}\n`);
    return;
  }
  const flags = (process.env.V8_FLAGS || "").split(/\s+/).filter((s) => s);
  const procs = Number(process.env.PROCS || 3);
  const rows = [];
  for (const v of variants) {
    let times = [];
    let check;
    for (let p = 0; p < procs; p++) {
      const out = execFileSync(process.execPath, [...flags, file, v.name], {
        encoding: "utf8",
      });
      const r = JSON.parse(out);
      times = times.concat(r.times);
      check = r.check;
    }
    rows.push({ name: v.name, ns: median(times), check });
  }
  console.log(
    `# ${require("node:path").basename(file)}  node ${process.version}` +
      (flags.length ? `  ${flags.join(" ")}` : ""),
  );
  const width = Math.max(...rows.map((r) => r.name.length));
  const base = rows[0].ns;
  for (const r of rows)
    console.log(
      `${r.name.padEnd(width)}  ${r.ns.toFixed(2).padStart(9)} ns/iter` +
        `  ${((r.ns / base) * 100).toFixed(0).padStart(4)}%`,
    );
}

module.exports = { main };

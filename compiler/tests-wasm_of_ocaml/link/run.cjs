// Wasm_of_ocaml compiler
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

// Usage: node run.cjs FILE.wasm [EXPORT...]
// Compile the module (which validates it). Without export names, print
// the kinds and names of its exports and imports. Otherwise, instantiate
// it, then call the given exported functions without arguments and print
// their results.
const fs = require("node:fs");
const [file, ...names] = process.argv.slice(2);
const mod = new WebAssembly.Module(fs.readFileSync(file));
if (names.length === 0) {
  for (const e of WebAssembly.Module.imports(mod))
    console.log(`import ${e.kind} ${e.module} ${e.name}`);
  for (const e of WebAssembly.Module.exports(mod))
    console.log(`${e.kind} ${e.name}`);
} else {
  const instance = new WebAssembly.Instance(mod, {});
  for (const name of names) console.log(`${name}: ${instance.exports[name]()}`);
}

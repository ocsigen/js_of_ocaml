// Wasm_of_ocaml runtime support
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

() => async (args) => {
  // biome-ignore lint/suspicious/noRedundantUseStrict:
  "use strict";

  const emitWarning = globalThis.process.emitWarning;
  globalThis.process.emitWarning = function (...args) {
    if (args[1] !== "ExperimentalWarning") emitWarning(...args);
  };

  const { link, src } = args;

  const { argv, env } = require("node:process");
  const { WASI } = require("node:wasi");
  const wasi = new WASI({
    version: "preview1",
    args: argv.slice(1),
    env,
    preopens: { ".": ".", "/tmp": "/tmp" },
    returnOnExit: false,
  });
  const imports = wasi.getImportObject();
  function loadRelative(src) {
    const path = require("node:path");
    const f = path.join(path.dirname(require.main.filename), src);
    return require("node:fs/promises").readFile(f);
  }
  async function instantiateModule(code) {
    return WebAssembly.instantiate(await code, imports);
  }
  async function instantiateFromDir() {
    imports.env = {};
    imports.OCaml = {};
    const deps = [];
    async function loadModule(module, isRuntime) {
      const sync = module[1].constructor !== Array;
      async function instantiate() {
        const code = loadRelative(src + "/" + module[0] + ".wasm");
        await Promise.all(sync ? deps : module[1].map((i) => deps[i]));
        const wasmModule = await instantiateModule(code);
        Object.assign(
          isRuntime ? imports.env : imports.OCaml,
          wasmModule.instance.exports,
        );
      }
      const promise = instantiate();
      deps.push(promise);
      return promise;
    }
    async function loadModules(lst) {
      for (const module of lst) {
        await loadModule(module);
      }
    }
    await loadModule(link[0], 1);
    if (link.length > 1) {
      await loadModule(link[1]);
      const workers = new Array(20)
        .fill(link.slice(2).values())
        .map(loadModules);
      await Promise.all(workers);
    }
    return { instance: { exports: Object.assign(imports.env, imports.OCaml) } };
  }
  const wasmModule = await instantiateFromDir();

  wasi.start(wasmModule.instance);
};

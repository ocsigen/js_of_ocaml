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

(js) => async (args) => {
  // biome-ignore lint/suspicious/noRedundantUseStrict: needed for non-module execution
  "use strict";
  const { link, src, generated, disable_effects } = args;

  const isNode = globalThis.process?.versions?.node;

  const math = {
    cos: Math.cos,
    sin: Math.sin,
    tan: Math.tan,
    acos: Math.acos,
    asin: Math.asin,
    atan: Math.atan,
    cosh: Math.cosh,
    sinh: Math.sinh,
    tanh: Math.tanh,
    acosh: Math.acosh,
    asinh: Math.asinh,
    atanh: Math.atanh,
    cbrt: Math.cbrt,
    exp: Math.exp,
    expm1: Math.expm1,
    log: Math.log,
    log1p: Math.log1p,
    log2: Math.log2,
    log10: Math.log10,
    atan2: Math.atan2,
    hypot: Math.hypot,
    pow: Math.pow,
    fmod: (x, y) => x % y,
  };

  const on_windows = isNode && globalThis.process.platform === "win32";

  // Virtual filesystem for embedded files (e.g. CMIs for toplevel). The tables
  // live here (they are populated from [args.files] below); the file
  // operations that consult them live in runtime/js/wasm.js and receive this
  // object through the [get_vfs] binding.
  const vfs = {
    files: new Map(), // path -> Uint8Array
    dirs: new Set(), // directory paths
    fds: new Map(), // fd -> { data, offset }
    next_fd: 1000000,
    // The virtual filesystem uses "/" as path separator. On Windows, the
    // program manipulates paths with "\" (Sys.os_type is "Win32"), so
    // normalize paths on registration and before each lookup.
    path: on_windows ? (p) => p.replaceAll("\\", "/") : (p) => p,
  };

  function register_virtual_file(name, content) {
    name = vfs.path(name);
    vfs.files.set(name, content);
    let dir = name;
    while (true) {
      const i = dir.lastIndexOf("/");
      if (i <= 0) break;
      dir = dir.slice(0, i);
      vfs.dirs.add(dir);
    }
  }

  if (args.files) {
    for (const [name, data] of Object.entries(args.files)) {
      register_virtual_file(
        name,
        Uint8Array.from(atob(data), (c) => c.charCodeAt(0)),
      );
    }
  }

  var start_fiber;

  function make_suspending(f) {
    return WebAssembly?.Suspending ? new WebAssembly.Suspending(f) : f;
  }
  function make_promising(f) {
    return !disable_effects && WebAssembly?.promising && f
      ? WebAssembly.promising(f)
      : f;
  }

  const decoder = new TextDecoder("utf-8", { ignoreBOM: 1 });
  const encoder = new TextEncoder();

  function getenv(n) {
    if (isNode && globalThis.process.env[n] !== undefined)
      return globalThis.process.env[n];
    return globalThis.jsoo_env?.[n];
  }

  let record_backtrace_flag = 0;

  for (const l of getenv("OCAMLRUNPARAM")?.split(",") || []) {
    if (l === "b") record_backtrace_flag = 1;
    if (l.startsWith("b=")) record_backtrace_flag = +l.slice(2) ? 1 : 0;
  }

  const on_arm64 = globalThis.process?.arch === "arm64";

  const bindings = {
    jstag:
      WebAssembly.JSTag ||
      // ZZZ not supported in Safari yet
      new WebAssembly.Tag({ parameters: ["externref"], results: [] }),
    identity: (x) => x,
    from_bool: (x) => !!x,
    get: (x, y) => x[y],
    set: (x, y, z) => (x[y] = z),
    delete: (x, y) => delete x[y],
    instanceof: (x, y) => x instanceof y,
    is_js_error: (x) => x instanceof Error,
    to_js_string: (x) => String(x),
    typeof: (x) => typeof x,
    // biome-ignore lint/suspicious/noDoubleEquals: ..
    equals: (x, y) => x == y,
    strict_equals: (x, y) => x === y,
    fun_call: (f, o, args) => f.apply(o, args),
    meth_call: (o, f, args) => o[f].apply(o, args),
    new_array: (n) => new Array(n),
    new_obj: () => ({}),
    new: (c, args) => new c(...args),
    global_this: globalThis,
    iter_props: (o, f) => {
      for (var nm in o) if (Object.hasOwn(o, nm)) f(nm);
    },
    array_length: (a) => a.length,
    array_get: (a, i) => a[i],
    array_set: (a, i, v) => (a[i] = v),
    read_string: (l) => decoder.decode(new Uint8Array(buffer, 0, l)),
    read_string_stream: (l, stream) =>
      decoder.decode(new Uint8Array(buffer, 0, l), { stream }),
    append_string: (s1, s2) => s1 + s2,
    write_string: (s) => {
      var start = 0,
        len = s.length;
      for (;;) {
        const { read, written } = encoder.encodeInto(
          s.slice(start),
          out_buffer,
        );
        len -= read;
        if (!len) return written;
        caml_extract_bytes(written);
        start += read;
      }
    },
    wrap_callback: (f) =>
      function (...args) {
        if (args.length === 0) {
          args = [undefined];
        }
        return caml_callback(f, args.length, args, 1);
      },
    wrap_callback_args: (f) =>
      function (...args) {
        return caml_callback(f, 1, [args], 0);
      },
    wrap_callback_strict: (arity, f) =>
      function (...args) {
        args.length = arity;
        return caml_callback(f, arity, args, 0);
      },
    wrap_callback_unsafe: (f) =>
      function (...args) {
        return caml_callback(f, args.length, args, 2);
      },
    wrap_meth_callback: (f) =>
      function (...args) {
        args.unshift(this);
        return caml_callback(f, args.length, args, 1);
      },
    wrap_meth_callback_args: (f) =>
      function (...args) {
        return caml_callback(f, 2, [this, args], 0);
      },
    wrap_meth_callback_strict: (arity, f) =>
      function (...args) {
        args.length = arity;
        args.unshift(this);
        return caml_callback(f, args.length, args, 0);
      },
    wrap_meth_callback_unsafe: (f) =>
      function (...args) {
        args.unshift(this);
        return caml_callback(f, args.length, args, 2);
      },
    get_vfs: () => vfs,
    exit: (n) => isNode && globalThis.process.exit(n),
    argv: () => (isNode ? globalThis.process.argv.slice(1) : ["a.out"]),
    on_windows: +on_windows,
    on_arm64: +on_arm64,
    getenv,
    backtrace_status: () => record_backtrace_flag,
    record_backtrace: (b) => (record_backtrace_flag = b),
    getcwd: () => (isNode ? globalThis.process.cwd() : "/static"),
    start_fiber: (x) => start_fiber(x),
    suspend_fiber: make_suspending((f, env) => new Promise((k) => f(k, env))),
    resume_fiber: (k, v) => k(v),
    // The dynamic-linking loaders (in runtime/js/wasm.js) instantiate new
    // modules against these; expose them through a single accessor.
    get_link_state: () => ({ imports, options }),
  };
  const string_ops = {
    test: (v) => +(typeof v === "string"),
    compare: (s1, s2) => (s1 < s2 ? -1 : +(s1 > s2)),
    decodeStringFromUTF8Array: () => "",
    encodeStringToUTF8Array: () => 0,
    fromCharCodeArray: () => "",
    length: (s) => s.length,
    intoCharCodeArray: () => 0,
  };
  const imports = Object.assign(
    {
      Math: math,
      bindings,
      js,
      "wasm:js-string": string_ops,
      "wasm:text-decoder": string_ops,
      "wasm:text-encoder": string_ops,
      str: new globalThis.Proxy(
        {},
        {
          get(_, prop) {
            return prop;
          },
        },
      ),
      env: {},
    },
    generated,
  );
  const options = {
    builtins: ["js-string", "text-decoder", "text-encoder"],
    importedStringConstants: "str",
  };

  function loadRelative(src) {
    const path = require("node:path");
    const f = path.join(path.dirname(require.main.filename), src);
    return require("node:fs/promises").readFile(f);
  }
  const fetchBase = globalThis?.document?.currentScript?.src;
  function fetchRelative(src) {
    const url = fetchBase ? new URL(src, fetchBase) : src;
    return fetch(url);
  }
  const loadCode = isNode ? loadRelative : fetchRelative;
  async function instantiateModule(code) {
    return isNode
      ? WebAssembly.instantiate(await code, imports, options)
      : WebAssembly.instantiateStreaming(code, imports, options);
  }
  async function instantiateFromDir() {
    imports.OCaml = {};
    const deps = [];
    async function loadModule(module, isRuntime) {
      const sync = module[1].constructor !== Array;
      async function instantiate() {
        const code = loadCode(src + "/" + module[0] + ".wasm");
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

  var {
    caml_callback,
    caml_start_fiber,
    caml_handle_uncaught_exception,
    caml_buffer,
    caml_extract_bytes,
    _initialize,
  } = wasmModule.instance.exports;

  var buffer = caml_buffer?.buffer;
  var out_buffer = buffer && new Uint8Array(buffer, 0, buffer.length);

  start_fiber = make_promising(caml_start_fiber);
  var _initialize = make_promising(_initialize);
  if (globalThis.process?.on) {
    globalThis.process.on("uncaughtException", (err, _origin) =>
      caml_handle_uncaught_exception(err),
    );
  } else if (globalThis.addEventListener) {
    globalThis.addEventListener(
      "error",
      (event) => event.error && caml_handle_uncaught_exception(event.error),
    );
  }
  await _initialize();
};

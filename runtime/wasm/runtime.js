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
  const { link, src, generated, enable_jspi, native_effects } = args;

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
    // No longer used by the compiler or the runtime (see caml_fmod_f64),
    // but still imported by some libraries (Base)
    fmod: (x, y) => x % y,
  };

  const typed_arrays = [
    Float32Array,
    Float64Array,
    Int8Array,
    Uint8Array,
    Int16Array,
    Uint16Array,
    Int32Array,
    Int32Array,
    Int32Array,
    Int32Array,
    Float32Array,
    Float64Array,
    Uint8Array,
    Uint16Array,
    Uint8ClampedArray,
  ];

  const fs = isNode && require("node:fs");

  const on_windows = isNode && globalThis.process.platform === "win32";

  // Windows console device names (CON, CONIN$, CONOUT$). Node resolves a
  // relative path against the current directory and prefixes it with \\?\,
  // which turns off the recognition of device names, so they must reach it
  // as \\.\CONIN$.
  const console_name = on_windows
    ? (p) => {
        const m = /^(?:[\\/]{2}[.?][\\/])?(CON|CONIN\$|CONOUT\$)$/i.exec(p);
        return m ? `\\\\.\\${m[1].toUpperCase()}` : p;
      }
    : (p) => p;

  // Pseudo filesystem, as in the JavaScript runtime: a table of mount
  // points, each bound to a device. A device is either the real file
  // system (node:fs, [dev] is null) or an in-memory file system. Under
  // node, the root is the real file system; otherwise, it is in memory.
  // There is always an in-memory device at /static/, which holds the
  // embedded files. Sys_js.mount adds in-memory devices that load missing
  // files through a callback.

  const fs_errors = {
    EPERM: [1, "operation not permitted"],
    ENOENT: [2, "no such file or directory"],
    EBADF: [9, "bad file descriptor"],
    EBUSY: [16, "resource busy or locked"],
    EEXIST: [17, "file already exists"],
    EXDEV: [18, "cross-device link not permitted"],
    ENOTDIR: [20, "not a directory"],
    EISDIR: [21, "illegal operation on a directory"],
    EINVAL: [22, "invalid argument"],
    ENOTEMPTY: [39, "directory not empty"],
  };

  // Errors have the shape of the ones of node:fs, so that they are
  // converted to Sys_error / Unix.Unix_error in the same way. The message
  // of Sys_error is the one of the JavaScript runtime.
  function fs_error(code, syscall, path) {
    const [errno, msg] = fs_errors[code];
    const e = new Error(`${code}: ${msg}, ${syscall} '${path}'`);
    e.toString = () =>
      code === "ENOENT" ? `${path}: No such file or directory` : e.message;
    return Object.assign(e, { errno: -errno, code, syscall, path });
  }

  let next_ino = 1;

  function mem_node(mode) {
    const t = Date.now();
    return { ino: next_ino++, mode, atimeMs: t, mtimeMs: t, ctimeMs: t };
  }

  const mem_dir = () => Object.assign(mem_node(0o777), { entries: new Map() });

  const mem_file = (data, mode) =>
    Object.assign(mem_node(mode), { data, size: data.length });

  // A lazy file: the file exists and is listed in its directory, but its
  // content is only loaded when it is needed: when the file is read or
  // written, or to get its size if [size] was not given. If [load] fails,
  // it is called again on the next use.
  class MemLazyFile {
    constructor(load, size) {
      Object.assign(this, mem_node(0o666));
      this._load = load;
      this._size = size;
    }
    force() {
      if (this._load) {
        this._data = this._load();
        this._size = this._data.length;
        this._load = null;
      }
    }
    get data() {
      this.force();
      return this._data;
    }
    // Replace the content, without loading it
    set data(data) {
      this._load = null;
      this._data = data;
    }
    get size() {
      if (this._size == null) this.force();
      return this._size;
    }
    set size(size) {
      this._size = size;
    }
  }

  function mem_resize(n, size) {
    // No need to load the content of a lazy file to discard it
    if (size === 0) n.data = new Uint8Array(0);
    else if (size > n.data.length) {
      const data = new Uint8Array(Math.max(size, 2 * n.data.length));
      data.set(n.data.subarray(0, n.size));
      n.data = data;
    } else if (size < n.size) n.data.fill(0, size, n.size);
    n.size = size;
    n.mtimeMs = n.ctimeMs = Date.now();
  }

  function mem_stat(n) {
    const d = !!n.entries;
    return {
      isFile: () => !d,
      isDirectory: () => d,
      dev: 0,
      ino: n.ino,
      mode: n.mode,
      nlink: 1,
      uid: 1,
      gid: 1,
      rdev: 0,
      size: d ? 0 : n.size,
      atimeMs: n.atimeMs,
      mtimeMs: n.mtimeMs,
      ctimeMs: n.ctimeMs,
    };
  }

  // In the methods below, [parts] is the path relative to the root of the
  // device, split into components, and [p] is the path given by the
  // program, for error reporting.
  class MemDevice {
    constructor(autoload) {
      this.root = mem_dir();
      this.autoload = autoload;
    }
    find(parts, sc, p, len = parts.length) {
      let n = this.root;
      for (let i = 0; n && i < len; i++) {
        if (!n.entries) throw fs_error("ENOTDIR", sc, p);
        n = n.entries.get(parts[i]);
      }
      return n;
    }
    // Add a file, creating the missing parent directories
    register(parts, n, sc, p) {
      let d = this.root;
      const name = parts.at(-1);
      for (let i = 0; i < parts.length - 1; i++) {
        let n = d.entries.get(parts[i]);
        if (!n) d.entries.set(parts[i], (n = mem_dir()));
        if (!n.entries) throw fs_error("ENOTDIR", sc, p);
        d = n;
      }
      if (name === undefined || d.entries.has(name))
        throw fs_error("EEXIST", sc, p);
      d.entries.set(name, n);
      return n;
    }
    lookup(parts, sc, p) {
      const n = this.find(parts, sc, p);
      if (n || !this.autoload || !parts.length) return n;
      const data = this.autoload(parts.join("/"));
      return data
        ? this.register(parts, mem_file(data, 0o666), sc, p)
        : undefined;
    }
    get(parts, sc, p) {
      const n = this.lookup(parts, sc, p);
      if (!n) throw fs_error("ENOENT", sc, p);
      return n;
    }
    get_dir(parts, sc, p) {
      const n = this.get(parts, sc, p);
      if (!n.entries) throw fs_error("ENOTDIR", sc, p);
      return n;
    }
    get_file(parts, sc, p) {
      const n = this.get(parts, sc, p);
      if (n.entries) throw fs_error("EISDIR", sc, p);
      return n;
    }
    parent(parts, sc, p) {
      // The root of a device can be neither created, removed nor renamed
      if (!parts.length) throw fs_error("EBUSY", sc, p);
      const d = this.find(parts, sc, p, parts.length - 1);
      if (!d) throw fs_error("ENOENT", sc, p);
      if (!d.entries) throw fs_error("ENOTDIR", sc, p);
      return d;
    }
    exists(parts) {
      try {
        return this.lookup(parts) ? 1 : 0;
      } catch (e) {
        if (e.code === "ENOTDIR") return 0;
        throw e;
      }
    }
    open(parts, flags, perm, p) {
      let n = this.lookup(parts, "open", p);
      if (n) {
        if (n.entries) throw fs_error("EISDIR", "open", p);
        if ((flags & 80) === 80) throw fs_error("EEXIST", "open", p);
        if (flags & 32) mem_resize(n, 0);
      } else {
        if (!(flags & 16)) throw fs_error("ENOENT", "open", p);
        const d = this.parent(parts, "open", p);
        n = mem_file(new Uint8Array(0), perm & 0o7777);
        d.entries.set(parts.at(-1), n);
      }
      const fd = next_mem_fd++;
      mem_fds.set(fd, { node: n, pos: 0, flags });
      return fd;
    }
    mkdir(parts, mode, p) {
      if (this.lookup(parts, "mkdir", p)) throw fs_error("EEXIST", "mkdir", p);
      const n = mem_dir();
      n.mode = mode & 0o7777;
      this.parent(parts, "mkdir", p).entries.set(parts.at(-1), n);
    }
    rmdir(parts, p) {
      const n = this.get_dir(parts, "rmdir", p);
      if (n.entries.size) throw fs_error("ENOTEMPTY", "rmdir", p);
      this.parent(parts, "rmdir", p).entries.delete(parts.at(-1));
    }
    unlink(parts, p) {
      this.get_file(parts, "unlink", p);
      this.parent(parts, "unlink", p).entries.delete(parts.at(-1));
    }
    link(parts, new_parts, p) {
      const n = this.get(parts, "link", p);
      if (n.entries) throw fs_error("EPERM", "link", p);
      const d = this.parent(new_parts, "link", p);
      if (d.entries.has(new_parts.at(-1))) throw fs_error("EEXIST", "link", p);
      d.entries.set(new_parts.at(-1), n);
    }
    rename(parts, new_parts, o, p) {
      const n = this.get(parts, "rename", o);
      const d = this.parent(parts, "rename", o);
      if (
        n.entries &&
        new_parts.length > parts.length &&
        parts.every((c, i) => c === new_parts[i])
      )
        throw fs_error("EINVAL", "rename", p);
      const new_d = this.parent(new_parts, "rename", p);
      const old = new_d.entries.get(new_parts.at(-1));
      if (old === n) return;
      if (old) {
        if (n.entries) {
          if (!old.entries) throw fs_error("ENOTDIR", "rename", p);
          if (old.entries.size) throw fs_error("ENOTEMPTY", "rename", p);
        } else if (old.entries) throw fs_error("EISDIR", "rename", p);
      }
      d.entries.delete(parts.at(-1));
      new_d.entries.set(new_parts.at(-1), n);
    }
    opendir(parts, p) {
      const names = [...this.get_dir(parts, "opendir", p).entries.keys()];
      let i = 0;
      const check = (sc) => {
        if (i < 0) throw fs_error("EBADF", sc, p);
      };
      return {
        readSync: () => {
          check("readdir");
          return i < names.length ? { name: names[i++] } : null;
        },
        closeSync: () => {
          check("closedir");
          i = -1;
        },
      };
    }
  }

  const mem_fds = new Map(); // fd -> { node, pos, flags }
  let next_mem_fd = 1000000;

  function mem_fd(fd, sc) {
    const f = mem_fds.get(fd);
    // Without node, all the files are in memory
    if (!f && !fs) throw fs_error("EBADF", sc, fd);
    return f;
  }

  function mem_read(f, b, o, l, p) {
    if (f.flags & 2) throw fs_error("EBADF", "read", "");
    const pos = p == null ? f.pos : Number(p);
    // Get the content first: the declared size of a lazy file is replaced
    // by the actual one when the content is loaded
    const data = f.node.data;
    const n = Math.max(0, Math.min(l, f.node.size - pos));
    b.set(data.subarray(pos, pos + n), o);
    f.pos = pos + n;
    return n;
  }

  function mem_write(f, b, o, l, p) {
    if (!(f.flags & 6)) throw fs_error("EBADF", "write", "");
    if (typeof b === "string") {
      b = encoder.encode(b);
      o = 0;
      l = b.length;
    }
    const n = f.node;
    n.data; // load the content of a lazy file
    const pos = f.flags & 8 ? n.size : p == null ? f.pos : Number(p);
    if (pos + l > n.size) mem_resize(n, pos + l);
    n.data.set(b.subarray(o, o + l), pos);
    n.mtimeMs = n.ctimeMs = Date.now();
    f.pos = pos + l;
    return l;
  }

  // The mount points. On Windows, a path either starts with a drive
  // ([C:/lib]) or has none: [/static/...] is not resolved against the
  // current drive, as in the JavaScript runtime.
  const mounts = [];

  function mount(path, dev) {
    const parts = split_path(path);
    if (!parts) throw new Error(`${path}: cannot mount`);
    const m = { path: join_path(parts, 1), parts, dev };
    mounts.push(m);
    return m;
  }

  // The current directory, when it is in an in-memory device
  let mem_cwd = isNode ? null : "/static";

  const getcwd = () => mem_cwd ?? globalThis.process.cwd();

  // Absolute path, split into components. On Windows, the drive, if any,
  // is the first component, in upper case ("C:"). Returns null for the
  // Windows paths which always belong to the real file system: UNC paths
  // (//server/share/...) and device paths (//./..., //?/...).
  function split_path(p) {
    if (on_windows) p = p.replaceAll("\\", "/");
    if (!(p.startsWith("/") || (on_windows && /^[a-zA-Z]:\//.test(p)))) {
      const cwd = getcwd();
      p = `${on_windows ? cwd.replaceAll("\\", "/") : cwd}/${p}`;
    }
    const parts = [];
    if (on_windows) {
      if (p.startsWith("//")) return null;
      if (p[1] === ":") {
        parts.push(p.slice(0, 2).toUpperCase());
        p = p.slice(2);
      }
    }
    const root = parts.length;
    for (const c of p.split("/")) {
      if (c === "..") {
        if (parts.length > root) parts.pop();
      } else if (c !== "" && c !== ".") parts.push(c);
    }
    return parts;
  }

  // The inverse of split_path, possibly with a trailing slash
  function join_path(parts, slash) {
    const drive = on_windows && /^[A-Z]:$/.test(parts[0]);
    const s = (drive ? "" : "/") + parts.join("/");
    return s.endsWith("/") || !(slash || (drive && parts.length === 1))
      ? s
      : `${s}/`;
  }

  // Find the device a path belongs to (the longest mount point which is a
  // prefix of the path). Returns undefined for the real file system.
  function resolve(p, sc) {
    if (p === "") {
      if (fs) return;
      throw fs_error("ENOENT", sc, p);
    }
    const parts = split_path(p);
    if (!parts) return;
    let res;
    for (const m of mounts) {
      if (
        (!res || m.parts.length > res.parts.length) &&
        m.parts.length <= parts.length &&
        m.parts.every((c, i) => c === parts[i])
      )
        res = m;
    }
    if (res?.dev) return { dev: res.dev, parts: parts.slice(res.parts.length) };
    if (!fs) throw fs_error("ENOENT", sc, p);
  }

  mount("/", fs ? null : new MemDevice());
  mount("/static", new MemDevice());

  function register_node(name, n) {
    const r = resolve(name, "open");
    if (!r) throw new Error(`${name}: cannot register file`);
    r.dev.register(r.parts, n, "open", name);
  }

  const register_file = (name, content) =>
    register_node(name, mem_file(content, 0o666));

  const register_lazy_file = (name, load, size) =>
    register_node(name, new MemLazyFile(load, size));

  // The content of a file given from JavaScript: a Uint8Array, or a string
  // with one character per byte, possibly encoded in base64.
  const file_content = (content, base64) =>
    typeof content === "string"
      ? Uint8Array.from(base64 ? atob(content) : content, (c) =>
          c.charCodeAt(0),
        )
      : new Uint8Array(content);

  if (args.files) {
    for (const [name, data] of Object.entries(args.files)) {
      register_file(name, file_content(data, 1));
    }
  }

  // The host can create files while the program runs, as with the
  // JavaScript runtime. Fails if the file exists. The content can be an
  // object { load, size } for a lazy file: [load] is called to get the
  // actual content when the file is first used, and [size], if given,
  // is the size reported until then.
  globalThis.jsoo_create_file = (name, content, base64) =>
    typeof content?.load === "function"
      ? register_lazy_file(
          name,
          () => file_content(content.load(), base64),
          content.size,
        )
      : register_file(name, file_content(content, base64));

  // Files registered before the runtime was loaded, for instance by the
  // output of [js_of_ocaml build-fs]
  for (const { name, content, base64 } of globalThis.jsoo_fs_tmp ?? [])
    globalThis.jsoo_create_file(name, content, base64);
  globalThis.jsoo_fs_tmp = [];

  const fs_cst = fs?.constants;

  const access_flags = fs
    ? [fs_cst.R_OK, fs_cst.W_OK, fs_cst.X_OK, fs_cst.F_OK]
    : [];

  const open_flags = fs
    ? [
        fs_cst.O_RDONLY,
        fs_cst.O_WRONLY,
        fs_cst.O_RDWR,
        fs_cst.O_APPEND,
        fs_cst.O_CREAT,
        fs_cst.O_TRUNC,
        fs_cst.O_EXCL,
        fs_cst.O_NONBLOCK,
        fs_cst.O_NOCTTY,
        fs_cst.O_DSYNC,
        fs_cst.O_SYNC,
      ]
    : [];

  var out_channels = {
    map: new WeakMap(),
    set: new Set(),
    finalization: new FinalizationRegistry((ref) =>
      out_channels.set.delete(ref),
    ),
  };

  function register_channel(ch) {
    const ref = new WeakRef(ch);
    out_channels.map.set(ch, ref);
    out_channels.set.add(ref);
    out_channels.finalization.register(ch, ref, ch);
  }

  function unregister_channel(ch) {
    const ref = out_channels.map.get(ch);
    if (ref) {
      out_channels.map.delete(ch);
      out_channels.set.delete(ref);
      out_channels.finalization.unregister(ch);
    }
  }

  function channel_list() {
    return [...out_channels.set].map((ref) => ref.deref()).filter((ch) => ch);
  }

  var start_fiber;

  function make_suspending(f) {
    return enable_jspi && WebAssembly?.Suspending
      ? new WebAssembly.Suspending(f)
      : f;
  }
  function make_promising(f) {
    return enable_jspi && WebAssembly?.promising && f
      ? WebAssembly.promising(f)
      : f;
  }

  const decoder = new TextDecoder("utf-8", { ignoreBOM: 1 });
  const encoder = new TextEncoder();

  // View on the scratch area used to convert between OCaml and JavaScript
  // strings: the first caml_buffer_size bytes of the linear memory owned by
  // the C runtime module (see jsstring.wat).  The C code's own data sits above
  // the scratch area, so the view must stop there; that code can also grow the
  // memory, which detaches the ArrayBuffer, so the view is created on demand.
  // A detached buffer has length 0, and so does the initial dummy view, so a
  // single length check covers both the first use and each growth.
  var scratch = new Uint8Array(0);
  function scratch_buffer() {
    if (scratch.byteLength === 0)
      scratch = new Uint8Array(caml_buffer.buffer, 0, caml_buffer_size.value);
    return scratch;
  }

  function hash_int(h, d) {
    d = Math.imul(d, 0xcc9e2d51 | 0);
    d = (d << 15) | (d >>> 17); // ROTL32(d, 15);
    d = Math.imul(d, 0x1b873593);
    h ^= d;
    h = (h << 13) | (h >>> 19); //ROTL32(h, 13);
    return (((h + (h << 2)) | 0) + (0xe6546b64 | 0)) | 0;
  }
  function hash_string(h, s) {
    // A string whose code units all fit in a byte (every ASCII string, all of
    // Latin-1) is mixed four code units per word, like a byte string, which
    // matches the JS runtime. This is done in a single pass, which also
    // records whether some code unit does not fit in a byte.
    var len = s.length,
      h0 = h,
      any = 0,
      i,
      w,
      c0,
      c1,
      c2,
      c3;
    for (i = 0; i + 4 <= len; i += 4) {
      c0 = s.charCodeAt(i);
      c1 = s.charCodeAt(i + 1);
      c2 = s.charCodeAt(i + 2);
      c3 = s.charCodeAt(i + 3);
      any |= c0 | c1 | c2 | c3;
      h = hash_int(h, c0 | (c1 << 8) | (c2 << 16) | (c3 << 24));
    }
    w = 0;
    switch (len & 3) {
      // biome-ignore lint/suspicious/noFallthroughSwitchClause: falls through
      case 3:
        c2 = s.charCodeAt(i + 2);
        any |= c2;
        w = c2 << 16;
      // falls through
      // biome-ignore lint/suspicious/noFallthroughSwitchClause: falls through
      case 2:
        c1 = s.charCodeAt(i + 1);
        any |= c1;
        w |= c1 << 8;
      // falls through
      case 1:
        c0 = s.charCodeAt(i);
        any |= c0;
        w |= c0;
        h = hash_int(h, w);
    }
    if (any <= 0xff) return h ^ len;
    // Genuine Unicode text: start again, mixing two 16-bit code units per
    // word, so no information is lost (packing them four per word at byte
    // offsets would overlap their high bits).
    h = h0;
    for (i = 0; i + 2 <= len; i += 2) {
      w = s.charCodeAt(i) | (s.charCodeAt(i + 1) << 16);
      h = hash_int(h, w);
    }
    if (len & 1) h = hash_int(h, s.charCodeAt(i));
    return h ^ len;
  }

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

  function alloc_stat(s, large) {
    var kind;
    if (s.isFile()) {
      kind = 0;
    } else if (s.isDirectory()) {
      kind = 1;
    } else if (s.isCharacterDevice()) {
      kind = 2;
    } else if (s.isBlockDevice()) {
      kind = 3;
    } else if (s.isSymbolicLink()) {
      kind = 4;
    } else if (s.isFIFO()) {
      kind = 5;
    } else if (s.isSocket()) {
      kind = 6;
    }
    return caml_alloc_stat(
      large,
      s.dev,
      s.ino | 0,
      kind,
      s.mode & 0o7777,
      s.nlink,
      s.uid,
      s.gid,
      s.rdev,
      BigInt(s.size),
      s.atimeMs / 1000,
      s.mtimeMs / 1000,
      s.ctimeMs / 1000,
    );
  }

  const on_arm64 = globalThis.process?.arch === "arm64";

  // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/stack
  const isV8 = new Error().stack?.includes("\n    at ") ?? false;

  const call = Function.prototype.call;
  const DV = DataView.prototype;

  // Calls into OCaml from JavaScript. With native effects (stack
  // switching), effects cannot be performed across JavaScript frames, so
  // the effect_allowed flag is cleared for the duration of the call:
  // %perform then raises Effect.Unhandled instead of trapping with an
  // unhandled suspend. The flag is restored in a finally block so that a
  // Wasm trap unwinding through this frame cannot leave it stale.
  function callback(f, count, args, kind) {
    if (!native_effects) return caml_callback(f, count, args, kind);
    const saved = effect_allowed.value;
    effect_allowed.value = 0;
    try {
      return caml_callback(f, count, args, kind);
    } finally {
      effect_allowed.value = saved;
    }
  }

  // Exact decimal expansion through BigInt, for precisions beyond
  // toFixed/toExponential's limit of 100 and for %f of values >= 1e21
  function exact_scaled(x, k) {
    // round_half_even(x * 10^k) as a BigInt, x finite and >= 0
    const dv = new DataView(new ArrayBuffer(8));
    dv.setFloat64(0, x);
    const hi = dv.getUint32(0);
    const lo = dv.getUint32(4);
    const eb = (hi >>> 20) & 0x7ff;
    // x is m * 2^e
    let m = (BigInt(hi & 0xfffff) << 32n) | BigInt(lo);
    let e = -1074;
    if (eb !== 0) {
      m |= 1n << 52n;
      e = eb - 1075;
    }
    let num = m;
    let den = 1n;
    if (k >= 0) num *= 10n ** BigInt(k);
    else den = 10n ** BigInt(-k);
    if (e >= 0) num <<= BigInt(e);
    else den <<= BigInt(-e);
    let q = num / den;
    const r2 = (num % den) * 2n;
    if (r2 > den || (r2 === den && q & 1n)) q += 1n;
    return q;
  }
  function exact_fixed(x, prec) {
    // toFixed, exactly
    let q = exact_scaled(x, prec).toString();
    if (prec === 0) return q;
    if (q.length <= prec) q = "0".repeat(prec + 1 - q.length) + q;
    return `${q.slice(0, q.length - prec)}.${q.slice(q.length - prec)}`;
  }
  function exact_exponential(x, prec) {
    // toExponential, exactly
    if (x === 0) return `${prec > 0 ? `0.${"0".repeat(prec)}` : "0"}e+0`;
    let e10 = Math.floor(Math.log10(x));
    for (;;) {
      // we want round(x * 10^(prec - e10)) to have exactly prec + 1
      // digits; the estimate of e10 is off by at most one (and
      // rounding can add a digit), so adjust and retry
      const s = exact_scaled(x, prec - e10).toString();
      if (s.length === prec + 1) {
        const m = prec > 0 ? `${s.charAt(0)}.${s.slice(1)}` : s;
        return `${m}e${e10 < 0 ? "-" : "+"}${Math.abs(e10)}`;
      }
      e10 += s.length - (prec + 1);
    }
  }

  const bindings = {
    jstag:
      WebAssembly.JSTag ||
      // ZZZ not supported in Safari yet
      new WebAssembly.Tag({ parameters: ["externref"], results: [] }),
    identity: (x) => x,
    true: true,
    false: false,
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
    read_string: (l) => decoder.decode(scratch_buffer().subarray(0, l)),
    read_string_stream: (l, stream) =>
      decoder.decode(scratch_buffer().subarray(0, l), { stream }),
    append_string: (s1, s2) => s1 + s2,
    write_string: (s) => {
      var start = 0,
        len = s.length;
      for (;;) {
        const { read, written } = encoder.encodeInto(
          s.slice(start),
          scratch_buffer(),
        );
        len -= read;
        if (!len) return written;
        caml_extract_bytes(written);
        start += read;
      }
    },
    ta_create: (k, sz) => new typed_arrays[k](sz),
    ta_normalize: (a) =>
      a instanceof Uint32Array
        ? new Int32Array(a.buffer, a.byteOffset, a.length)
        : a,
    ta_kind: (a) => typed_arrays.findIndex((c) => a instanceof c),
    ta_length: (a) => a.length,
    ta_get_i32: (a, i) => a[i],
    ta_fill: (a, v) => a.fill(v),
    ta_blit: (s, d) => d.set(s),
    ta_subarray: (a, i, j) => a.subarray(i, j),
    ta_set: (a, b, i) => a.set(b, i),
    ta_new: (len) => new Uint8Array(len),
    ta_copy: (ta, t, s, e) => ta.copyWithin(t, s, e),
    ta_bytes: (a) =>
      new Uint8Array(a.buffer, a.byteOffset, a.length * a.BYTES_PER_ELEMENT),
    dv_make: (a) => new DataView(a.buffer, a.byteOffset, a.byteLength),
    dv_get_f64: call.bind(DV.getFloat64),
    dv_get_f32: call.bind(DV.getFloat32),
    dv_get_i64: call.bind(DV.getBigInt64),
    // 2026-03-16: using call.bind is faster in V8 which recognize the
    // primitive and generate inlined call, but calling a JavaScript
    // function is currently faster in other engines which does not
    // have this optimization yet. Use benchmarks/bench_dv_getint32.js
    // for performance comparisons.
    dv_get_i32: isV8 ? call.bind(DV.getInt32) : (x, y, z) => x.getInt32(y, z),
    dv_get_i16: call.bind(DV.getInt16),
    dv_get_ui16: call.bind(DV.getUint16),
    dv_get_i8: call.bind(DV.getInt8),
    dv_get_ui8: call.bind(DV.getUint8),
    dv_set_f64: call.bind(DV.setFloat64),
    dv_set_f32: call.bind(DV.setFloat32),
    dv_set_i64: call.bind(DV.setBigInt64),
    dv_set_i32: call.bind(DV.setInt32),
    dv_set_i16: call.bind(DV.setInt16),
    dv_set_i8: call.bind(DV.setInt8),
    littleEndian: new Uint8Array(new Uint32Array([1]).buffer)[0],
    wrap_callback: (f) =>
      function (...args) {
        if (args.length === 0) {
          args = [undefined];
        }
        return callback(f, args.length, args, 1);
      },
    wrap_callback_args: (f) =>
      function (...args) {
        return callback(f, 1, [args], 0);
      },
    wrap_callback_strict: (arity, f) =>
      function (...args) {
        args.length = arity;
        return callback(f, arity, args, 0);
      },
    wrap_callback_unsafe: (f) =>
      function (...args) {
        return callback(f, args.length, args, 2);
      },
    wrap_meth_callback: (f) =>
      function (...args) {
        args.unshift(this);
        return callback(f, args.length, args, 1);
      },
    wrap_meth_callback_args: (f) =>
      function (...args) {
        return callback(f, 2, [this, args], 0);
      },
    wrap_meth_callback_strict: (arity, f) =>
      function (...args) {
        args.length = arity;
        args.unshift(this);
        return callback(f, args.length, args, 0);
      },
    wrap_meth_callback_unsafe: (f) =>
      function (...args) {
        args.unshift(this);
        return callback(f, args.length, args, 2);
      },
    wrap_fun_arguments: (f) =>
      function (...args) {
        return f(args);
      },
    // The sign of |v| - a, where v is the exact value of the float literal s
    // (a finite literal accepted by caml_parse_float) and a is a positive
    // double
    float32_exact_compare: (s, a) => {
      s = s.replace(/_/g, "").trim();
      const c = s.charAt(0);
      if (c === "-" || c === "+") s = s.slice(1);
      // v = num * base^exp
      let num, base, exp;
      let m =
        /^0[xX]([0-9a-fA-F]*)\.?([0-9a-fA-F]*)(?:[pP]([-+]?[0-9]+))?$/.exec(s);
      if (m) {
        num = BigInt(`0x0${m[1]}${m[2]}`);
        base = 2n;
        exp = (+m[3] || 0) - 4 * m[2].length;
      } else {
        m = /^([0-9]*)\.?([0-9]*)(?:[eE]([-+]?[0-9]+))?$/.exec(s);
        num = BigInt(`0${m[1]}${m[2]}`);
        base = 10n;
        exp = (+m[3] || 0) - m[2].length;
      }
      // a = mant * 2^e
      const dv = new DataView(new ArrayBuffer(8));
      dv.setFloat64(0, a);
      const hi = dv.getUint32(0);
      const eb = (hi >>> 20) & 0x7ff;
      let mant = (BigInt(hi & 0xfffff) << 32n) | BigInt(dv.getUint32(4));
      let e = -1074;
      if (eb !== 0) {
        mant |= 1n << 52n;
        e = eb - 1075;
      }
      let l = num;
      let r = mant;
      if (exp >= 0) l *= base ** BigInt(exp);
      else r *= base ** BigInt(-exp);
      if (e >= 0) r <<= BigInt(e);
      else l <<= BigInt(-e);
      return l > r ? 1 : l < r ? -1 : 0;
    },
    // Digits of x (finite, >= 0) as x.toFixed(prec) or x.toExponential(prec)
    // would give them. These functions round ties away from zero; the
    // caller rounds them to even (the exact BigInt versions, used beyond
    // their limits, already round ties to even).
    format_float: (x, prec, fixed) => {
      if (fixed)
        return prec > 100 || x >= 1e21 ? exact_fixed(x, prec) : x.toFixed(prec);
      return prec > 100 ? exact_exponential(x, prec) : x.toExponential(prec);
    },
    gettimeofday: () => Date.now() / 1000,
    times: () => {
      if (globalThis.process?.cpuUsage) {
        var t = globalThis.process.cpuUsage();
        return caml_alloc_times(t.user / 1e6, t.system / 1e6);
      } else {
        var t = performance.now() / 1000;
        return caml_alloc_times(t, 0);
      }
    },
    gmtime: (t) => {
      var d = new Date(t * 1000);
      var d_num = d.getTime();
      var januaryfirst = new Date(Date.UTC(d.getUTCFullYear(), 0, 1)).getTime();
      var doy = Math.floor((d_num - januaryfirst) / 86400000);
      return caml_alloc_tm(
        d.getUTCSeconds(),
        d.getUTCMinutes(),
        d.getUTCHours(),
        d.getUTCDate(),
        d.getUTCMonth(),
        d.getUTCFullYear() - 1900,
        d.getUTCDay(),
        doy,
        false,
      );
    },
    localtime: (t) => {
      var d = new Date(t * 1000);
      // tm_yday is the distance in days to January 1; compute it in UTC so
      // that the one-hour DST shift of wall-clock arithmetic does not apply
      var doy = Math.floor(
        (Date.UTC(d.getFullYear(), d.getMonth(), d.getDate()) -
          Date.UTC(d.getFullYear(), 0, 1)) /
          86400000,
      );
      var jan = new Date(d.getFullYear(), 0, 1);
      var jul = new Date(d.getFullYear(), 6, 1);
      var stdTimezoneOffset = Math.max(
        jan.getTimezoneOffset(),
        jul.getTimezoneOffset(),
      );
      var isdst = d.getTimezoneOffset() < stdTimezoneOffset;
      // Europe/Dublin uses "negative DST": the IANA database marks winter
      // (GMT) as the daylight deviation from its summer standard (IST), so
      // the native runtime reports tm_isdst inverted from the offset
      // heuristic. The offsets alone cannot distinguish it from
      // Europe/London, so match it by name (guarded to the GMT/+1 signature
      // to avoid the Intl lookup for every other zone).
      if (
        stdTimezoneOffset === 0 &&
        jan.getTimezoneOffset() !== jul.getTimezoneOffset() &&
        globalThis.Intl?.DateTimeFormat?.().resolvedOptions().timeZone ===
          "Europe/Dublin"
      )
        isdst = !isdst;
      return caml_alloc_tm(
        d.getSeconds(),
        d.getMinutes(),
        d.getHours(),
        d.getDate(),
        d.getMonth(),
        d.getFullYear() - 1900,
        d.getDay(),
        doy,
        isdst,
      );
    },
    mktime: (year, month, day, h, m, s) =>
      new Date(year, month, day, h, m, s).getTime(),
    random_seed: () => crypto.getRandomValues(new Int32Array(12)),
    access: (p, flags) => {
      const r = resolve(p, "access");
      if (r) return void r.dev.get(r.parts, "access", p);
      fs.accessSync(
        p,
        access_flags.reduce((f, v, i) => (flags & (1 << i) ? f | v : f), 0),
      );
    },
    open: (p, flags, perm) => {
      const r = resolve(p, "open");
      if (r) return r.dev.open(r.parts, flags, perm, p);
      return fs.openSync(
        console_name(p),
        open_flags.reduce((f, v, i) => (flags & (1 << i) ? f | v : f), 0),
        perm,
      );
    },
    close: (fd) => {
      if (mem_fds.delete(fd)) return;
      // Without node, there is nothing to do for the standard channels
      if (fs) fs.closeSync(fd);
      else if (fd > 2) throw fs_error("EBADF", "close", fd);
    },
    write: (fd, b, o, l, p) => {
      const f = mem_fds.get(fd);
      if (f) return mem_write(f, b, o, l, p);
      return fs
        ? fs.writeSync(fd, b, o, l, p === null ? p : Number(p))
        : (console[fd === 2 ? "error" : "log"](
            typeof b === "string" ? b : decoder.decode(b.slice(o, o + l)),
          ),
          l);
    },
    read: (fd, b, o, l, p) => {
      const f = mem_fd(fd, "read");
      return f ? mem_read(f, b, o, l, p) : fs.readSync(fd, b, o, l, p);
    },
    fsync: (fd) => {
      if (!mem_fd(fd, "fsync")) fs.fsyncSync(fd);
    },
    file_size: (fd) => {
      const f = mem_fd(fd, "fstat");
      return f ? BigInt(f.node.size) : fs.fstatSync(fd, { bigint: true }).size;
    },
    register_channel,
    unregister_channel,
    channel_list,
    exit: (n) => isNode && globalThis.process.exit(n),
    // like caml_argv in the JavaScript runtime: globalThis.scriptArgs is
    // [program, ...arguments] (QuickJS), and can be set by a page
    argv: () =>
      isNode
        ? globalThis.process.argv.slice(1)
        : Array.isArray(globalThis.scriptArgs) &&
            globalThis.scriptArgs.length > 0
          ? globalThis.scriptArgs
          : ["a.out"],
    on_windows: +on_windows,
    on_arm64: +on_arm64,
    getenv,
    backtrace_status: () => record_backtrace_flag,
    record_backtrace: (b) => (record_backtrace_flag = b),
    system: (c) => {
      var res = require("node:child_process").spawnSync(c, {
        shell: true,
        stdio: "inherit",
      });
      if (res.error) throw res.error;
      return res.signal ? 255 : res.status;
    },
    // In a browser there is no tty and no [require]; report false instead of
    // throwing on the undefined [require].
    isatty: (fd) => (isNode ? +require("node:tty").isatty(fd) : 0),
    getuid: () =>
      globalThis.process?.getuid ? globalThis.process.getuid() : 1,
    geteuid: () =>
      globalThis.process?.geteuid ? globalThis.process.geteuid() : 1,
    getgid: () =>
      globalThis.process?.getgid ? globalThis.process.getgid() : 1,
    getegid: () =>
      globalThis.process?.getegid ? globalThis.process.getegid() : 1,
    time: () => performance.now(),
    getcwd,
    chdir: (p) => {
      const r = resolve(p, "chdir");
      if (r) {
        r.dev.get_dir(r.parts, "chdir", p);
        mem_cwd = join_path(split_path(p));
      } else {
        // Leaving a device for the directory the process is already in
        // does not need process.chdir, which is not available in a
        // worker thread.
        const parts = split_path(p);
        if (
          !parts ||
          join_path(parts) !== join_path(split_path(globalThis.process.cwd()))
        )
          globalThis.process.chdir(p);
        mem_cwd = null;
      }
    },
    mkdir: (p, m) => {
      const r = resolve(p, "mkdir");
      return r ? r.dev.mkdir(r.parts, m, p) : fs.mkdirSync(p, m);
    },
    rmdir: (p) => {
      const r = resolve(p, "rmdir");
      return r ? r.dev.rmdir(r.parts, p) : fs.rmdirSync(p);
    },
    link: (d, s) => {
      const r = resolve(d, "link");
      const r2 = resolve(s, "link");
      if (r?.dev !== r2?.dev) throw fs_error("EXDEV", "link", s);
      return r ? r.dev.link(r.parts, r2.parts, s) : fs.linkSync(d, s);
    },
    symlink: (t, p, kind) => {
      // No symbolic link in the in-memory devices
      if (resolve(p, "symlink")) throw fs_error("EPERM", "symlink", p);
      fs.symlinkSync(t, p, [null, "file", "dir"][kind]);
    },
    readlink: (p) => {
      const r = resolve(p, "readlink");
      if (!r) return fs.readlinkSync(p);
      r.dev.get(r.parts, "readlink", p);
      throw fs_error("EINVAL", "readlink", p);
    },
    unlink: (p) => {
      const r = resolve(p, "unlink");
      return r ? r.dev.unlink(r.parts, p) : fs.unlinkSync(p);
    },
    read_dir: (p) => {
      const r = resolve(p, "scandir");
      return r
        ? [...r.dev.get_dir(r.parts, "scandir", p).entries.keys()]
        : fs.readdirSync(p);
    },
    // node's Dir.readSync does not report "." and ".."; native does
    opendir: (p) => {
      const r = resolve(p, "opendir");
      return {
        dir: r ? r.dev.opendir(r.parts, p) : fs.opendirSync(p),
        dots: [".", ".."],
      };
    },
    readdir: (d) => {
      if (d.dots.length > 0) return d.dots.shift();
      var n = d.dir.readSync()?.name;
      return n === undefined ? null : n;
    },
    closedir: (d) => {
      d.dots = []; // a read on the closed handle must fail, not return "." / ".."
      d.dir.closeSync();
    },
    stat: (p, l) => {
      const r = resolve(p, "stat");
      return alloc_stat(
        r ? mem_stat(r.dev.get(r.parts, "stat", p)) : fs.statSync(p),
        l,
      );
    },
    lstat: (p, l) => {
      const r = resolve(p, "lstat");
      return alloc_stat(
        r ? mem_stat(r.dev.get(r.parts, "lstat", p)) : fs.lstatSync(p),
        l,
      );
    },
    fstat: (fd, l) => {
      const f = mem_fd(fd, "fstat");
      return alloc_stat(f ? mem_stat(f.node) : fs.fstatSync(fd), l);
    },
    chmod: (p, perms) => {
      const r = resolve(p, "chmod");
      if (r) r.dev.get(r.parts, "chmod", p).mode = perms & 0o7777;
      else fs.chmodSync(p, perms);
    },
    fchmod: (fd, perms) => {
      const f = mem_fd(fd, "fchmod");
      if (f) f.node.mode = perms & 0o7777;
      else fs.fchmodSync(fd, perms);
    },
    file_exists: (p) => {
      if (!fs && p === "") return 0;
      const r = resolve(p);
      return r ? r.dev.exists(r.parts) : +fs.existsSync(p);
    },
    filepath_exists: (p) => {
      if (!fs && p === "") return 0;
      const r = resolve(p);
      if (r) return r.dev.exists(r.parts);
      try {
        fs.statSync(p);
        return 1;
      } catch (e) {
        if (e.code === "ENOENT" || e.code === "ENOTDIR") return 0;
        throw e;
      }
    },
    // Follow symlinks, like native and the JS runtime (statSync, not lstat).
    is_directory: (p) => {
      const r = resolve(p, "stat");
      return r
        ? +!!r.dev.get(r.parts, "stat", p).entries
        : +fs.statSync(p).isDirectory();
    },
    is_file: (p) => {
      const r = resolve(p, "stat");
      return r
        ? +!r.dev.get(r.parts, "stat", p).entries
        : +fs.statSync(p).isFile();
    },
    utimes: (p, a, m) => {
      const r = resolve(p, "utime");
      if (!r) return fs.utimesSync(p, a, m);
      const n = r.dev.get(r.parts, "utime", p);
      n.atimeMs = a * 1000;
      n.mtimeMs = m * 1000;
    },
    truncate: (p, l) => {
      const r = resolve(p, "open");
      if (!r) return fs.truncateSync(p, l);
      if (l < 0) throw fs_error("EINVAL", "truncate", p);
      mem_resize(r.dev.get_file(r.parts, "open", p), l);
    },
    ftruncate: (fd, l) => {
      const f = mem_fd(fd, "ftruncate");
      if (!f) return fs.ftruncateSync(fd, l);
      if (l < 0 || !(f.flags & 6)) throw fs_error("EINVAL", "ftruncate", "");
      mem_resize(f.node, l);
    },
    rename: (o, n) => {
      const r = resolve(o, "rename");
      const r2 = resolve(n, "rename");
      if (r?.dev !== r2?.dev) throw fs_error("EXDEV", "rename", n);
      if (r) return r.dev.rename(r.parts, r2.parts, o, n);
      var n_stat;
      if (
        on_windows &&
        (n_stat = fs.statSync(n, { throwIfNoEntry: false })) &&
        fs.statSync(o, { throwIfNoEntry: false })?.isDirectory()
      ) {
        if (n_stat.isDirectory()) {
          if (!n.startsWith(o))
            try {
              fs.rmdirSync(n);
            } catch {}
        } else {
          var e = new Error(
            `ENOTDIR: not a directory, rename '${o}' -> '${n}'`,
          );
          throw Object.assign(e, {
            errno: -20,
            code: "ENOTDIR",
            syscall: "rename",
            path: n,
          });
        }
      }
      fs.renameSync(o, n);
    },
    tmpdir: () => (isNode ? require("node:os").tmpdir() : "/tmp"),
    start_fiber: (x) => start_fiber(x),
    suspend_fiber: make_suspending((f, env) => new Promise((k) => f(k, env))),
    resume_fiber: (k, v) => k(v),
    weak_new: (v) => new WeakRef(v),
    weak_deref: (w) => {
      var v = w.deref();
      return v === undefined ? null : v;
    },
    weak_map_new: () => new WeakMap(),
    map_new: () => new Map(),
    map_get: (m, x) => {
      var v = m.get(x);
      return v === undefined ? null : v;
    },
    map_set: (m, x, v) => m.set(x, v),
    map_get_or_set: (m, x, v) => {
      var r = m.get(x);
      if (r === undefined) {
        m.set(x, v);
        return null;
      }
      return r;
    },
    map_delete: (m, x) => m.delete(x),
    hash_string,
    log: (x) => console.log(x),
    register_fragments: (unitName, fragmentsSource) => {
      // biome-ignore lint/security/noGlobalEval: ..
      const frags = eval?.(fragmentsSource);
      imports[unitName + ".fragments"] = frags;
    },
    load_module: (wasmBytes) => {
      const module = new WebAssembly.Module(wasmBytes, options);
      const inst = new WebAssembly.Instance(module, imports);
      Object.assign(imports.OCaml, inst.exports);
      return inst.exports["_dynlink.init"]();
    },
    load_wasmo: (zipBytes) => {
      // Parse ZIP to extract code.wasm and link_order (uncompressed ZIP)
      const dv = new DataView(
        zipBytes.buffer,
        zipBytes.byteOffset,
        zipBytes.byteLength,
      );
      const len = zipBytes.byteLength;
      // Find End of Central Directory record (search backwards)
      let eocdOff = len - 22;
      while (eocdOff >= 0 && dv.getUint32(eocdOff, true) !== 0x06054b50)
        eocdOff--;
      if (eocdOff < 0) throw new Error("Invalid ZIP: EOCD not found");
      const cdOff = dv.getUint32(eocdOff + 16, true);
      const cdEntries = dv.getUint16(eocdOff + 10, true);
      // Scan central directory for code.wasm and link_order
      const entries = {};
      let off = cdOff;
      for (let i = 0; i < cdEntries; i++) {
        if (dv.getUint32(off, true) !== 0x02014b50)
          throw new Error("Invalid ZIP: bad CD entry");
        const nameLen = dv.getUint16(off + 28, true);
        const extraLen = dv.getUint16(off + 30, true);
        const commentLen = dv.getUint16(off + 32, true);
        const localOff = dv.getUint32(off + 42, true);
        const name = decoder.decode(
          zipBytes.subarray(off + 46, off + 46 + nameLen),
        );
        const size = dv.getUint32(off + 24, true);
        const localNameLen = dv.getUint16(localOff + 26, true);
        const localExtraLen = dv.getUint16(localOff + 28, true);
        const dataOff = localOff + 30 + localNameLen + localExtraLen;
        entries[name] = zipBytes.subarray(dataOff, dataOff + size);
        off += 46 + nameLen + extraLen + commentLen;
      }
      if (!entries["code.wasm"])
        throw new Error("code.wasm not found in .wasmo");
      const module = new WebAssembly.Module(entries["code.wasm"], options);
      const inst = new WebAssembly.Instance(module, imports);
      Object.assign(imports.OCaml, inst.exports);
      const names = decoder.decode(entries.link_order).split("\x00");
      for (const name of names) inst.exports[name + ".init"]();
    },
    register_file,
    register_lazy_file: (name, size, f) =>
      register_lazy_file(name, () => caml_fs_load(f), size < 0 ? null : size),
    read_file: (name) => {
      const r = resolve(name, "open");
      if (r) {
        const n = r.dev.lookup(r.parts, "open", name);
        return n?.data ? n.data.subarray(0, n.size) : null;
      }
      return fs.statSync(name, { throwIfNoEntry: false })?.isFile()
        ? fs.readFileSync(name)
        : null;
    },
    mount_autoload: (path, f) => {
      const m = mount(
        path,
        new MemDevice((name) => caml_fs_autoload(f, m.path, name)),
      );
    },
    unmount: (path) => {
      const parts = split_path(path);
      const i = mounts.findLastIndex(
        (m) =>
          m.parts.length === parts?.length &&
          m.parts.every((c, i) => c === parts[i]),
      );
      if (i >= 0) mounts.splice(i, 1);
    },
    mount_points: () => mounts.map((m) => m.path).reverse(),
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
  // Counters of the representation conversions executed by code compiled
  // with [--debug count-conversions] (per kind, imported from module
  // "conversions") or [--debug count-conversion-sites] (also per site,
  // imported from module "conversion-sites"), created on demand. The nonzero
  // ones are printed on exit, the busiest sites only.
  function make_counters(counters) {
    return new globalThis.Proxy(
      {},
      {
        get(_, name) {
          if (!Object.hasOwn(counters, name))
            counters[name] = new globalThis.WebAssembly.Global(
              { value: "i64", mutable: true },
              0n,
            );
          return counters[name];
        },
      },
    );
  }
  const conversion_counters = {};
  const site_counters = {};
  if (isNode)
    globalThis.process.on("exit", () => {
      const counts = Object.entries(conversion_counters)
        .filter(([_, g]) => g.value !== 0n)
        .sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0))
        .map(([name, g]) => `${name}=${g.value}`);
      if (counts.length) fs.writeSync(2, `conversions: ${counts.join(" ")}\n`);
      const sites = Object.entries(site_counters)
        .filter(([_, g]) => g.value !== 0n)
        .sort(([_a, a], [_b, b]) =>
          b.value > a.value ? 1 : b.value < a.value ? -1 : 0,
        )
        .slice(0, 40);
      for (const [name, g] of sites)
        fs.writeSync(2, `${String(g.value).padStart(14)} ${name}\n`);
    });
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
      conversions: make_counters(conversion_counters),
      "conversion-sites": make_counters(site_counters),
    },
    generated,
  );
  const options = {
    builtins: ["js-string", "text-decoder", "text-encoder"],
    importedStringConstants: "str",
  };

  function loadRelative(src) {
    const path = require("node:path");
    const f = path.join(path.dirname(module.filename), src);
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
    effect_allowed,
    caml_alloc_times,
    caml_alloc_tm,
    caml_alloc_stat,
    caml_fs_autoload,
    caml_fs_load,
    caml_start_fiber,
    caml_handle_uncaught_exception,
    caml_buffer,
    caml_buffer_size,
    caml_extract_bytes,
    _initialize,
  } = wasmModule.instance.exports;

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

// Js_of_ocaml runtime support
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

// Self-contained primitives for the wasm_of_ocaml runtime. These used to live
// in the `bindings` object of runtime/wasm/runtime.js, which is always emitted
// in full. Moving the ones that depend on no closure-local state here lets the
// JS linker (and wasm-metadce) drop them from the output when the program does
// not use them. They are imported from the "js" module namespace in the .wat
// files, like the graphics helpers.

// Registry of open output channels, so the runtime can flush them all at
// exit. Holds weak references keyed by the channel, plus a finalization
// registry that drops entries when a channel is garbage-collected.
//Provides: out_channels
//If: wasm
var out_channels = {
  map: new WeakMap(),
  set: new Set(),
  finalization: new FinalizationRegistry((ref) => out_channels.set.delete(ref)),
};

//Provides: register_channel
//Requires: out_channels
//If: wasm
function register_channel(ch) {
  const ref = new WeakRef(ch);
  out_channels.map.set(ch, ref);
  out_channels.set.add(ref);
  out_channels.finalization.register(ch, ref, ch);
}

//Provides: unregister_channel
//Requires: out_channels
//If: wasm
function unregister_channel(ch) {
  const ref = out_channels.map.get(ch);
  if (ref) {
    out_channels.map.delete(ch);
    out_channels.set.delete(ref);
    out_channels.finalization.unregister(ch);
  }
}

//Provides: channel_list
//Requires: out_channels
//If: wasm
function channel_list() {
  return [...out_channels.set].map((ref) => ref.deref()).filter((ch) => ch);
}

// The time functions below return the record fields as a plain JS array; the
// WAT side reads the array and allocates the OCaml record. This keeps the
// fragment free of any dependency on the Wasm exports (caml_alloc_tm /
// caml_alloc_times), which live inside the runtime.js closure.

//Provides: times
//If: wasm
function times() {
  // [tms_utime, tms_stime]; the WAT allocation pads the other two fields.
  if (globalThis.process?.cpuUsage) {
    var t = globalThis.process.cpuUsage();
    return [t.user / 1e6, t.system / 1e6];
  }
  return [performance.now() / 1000, 0];
}

//Provides: gmtime
//If: wasm
function gmtime(t) {
  var d = new Date(t * 1000);
  var d_num = d.getTime();
  var januaryfirst = new Date(Date.UTC(d.getUTCFullYear(), 0, 1)).getTime();
  var doy = Math.floor((d_num - januaryfirst) / 86400000);
  return [
    d.getUTCSeconds(),
    d.getUTCMinutes(),
    d.getUTCHours(),
    d.getUTCDate(),
    d.getUTCMonth(),
    d.getUTCFullYear() - 1900,
    d.getUTCDay(),
    doy,
    0,
  ];
}

//Provides: localtime
//If: wasm
function localtime(t) {
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
  return [
    d.getSeconds(),
    d.getMinutes(),
    d.getHours(),
    d.getDate(),
    d.getMonth(),
    d.getFullYear() - 1900,
    d.getDay(),
    doy,
    isdst ? 1 : 0,
  ];
}

//Provides: format_float
//If: wasm
function format_float(prec, conversion, pad, x) {
  // Exact decimal expansion through BigInt, for precisions beyond
  // toFixed/toExponential's limit of 100 and for %f of values >= 1e21
  function decompose(x) {
    // x (finite, >= 0) is m * 2^e
    var dv = new DataView(new ArrayBuffer(8));
    dv.setFloat64(0, x);
    var hi = dv.getUint32(0),
      lo = dv.getUint32(4);
    var eb = (hi >>> 20) & 0x7ff;
    var m = (BigInt(hi & 0xfffff) << 32n) | BigInt(lo);
    if (eb === 0) return [m, -1074];
    return [m | (1n << 52n), eb - 1075];
  }
  function exact_scaled(x, k) {
    // round_half_even(x * 10^k) as a BigInt
    var d = decompose(x);
    var num = d[0],
      den = 1n;
    if (k >= 0) num *= 10n ** BigInt(k);
    else den = 10n ** BigInt(-k);
    if (d[1] >= 0) num <<= BigInt(d[1]);
    else den <<= BigInt(-d[1]);
    var q = num / den,
      r2 = (num % den) * 2n;
    if (r2 > den || (r2 === den && q & 1n)) q += 1n;
    return q;
  }
  function exact_fixed(x, prec) {
    // toFixed, exactly
    var q = exact_scaled(x, prec).toString();
    if (prec === 0) return q;
    if (q.length <= prec) q = "0".repeat(prec + 1 - q.length) + q;
    return q.slice(0, q.length - prec) + "." + q.slice(q.length - prec);
  }
  function exact_exponential(x, prec) {
    // toExponential, exactly
    if (x === 0) return (prec > 0 ? "0." + "0".repeat(prec) : "0") + "e+0";
    var e10 = Math.floor(Math.log10(x));
    for (;;) {
      // we want round(x * 10^(prec - e10)) to have exactly prec + 1
      // digits; the estimate of e10 is off by at most one (and
      // rounding can add a digit), so adjust and retry
      var s = exact_scaled(x, prec - e10).toString();
      if (s.length === prec + 1) {
        var m = prec > 0 ? s.charAt(0) + "." + s.slice(1) : s;
        return m + "e" + (e10 < 0 ? "-" : "+") + Math.abs(e10);
      }
      e10 += s.length - (prec + 1);
    }
  }
  function toExponential(x, prec) {
    return prec > 100 ? exact_exponential(x, prec) : x.toExponential(prec);
  }
  function toFixed(x, dp) {
    if (dp > 100 || x >= 1e21) return exact_fixed(x, dp);
    return x.toFixed(dp);
  }
  switch (conversion) {
    case 0:
      var s = toExponential(x, prec);
      // exponent should be at least two digits
      var i = s.length;
      if (s.charAt(i - 3) === "e") s = s.slice(0, i - 1) + "0" + s.slice(i - 1);
      break;
    case 1:
      s = toFixed(x, prec);
      break;
    case 2:
      prec = prec ? prec : 1;
      s = toExponential(x, prec - 1);
      var j = s.indexOf("e");
      var exp = +s.slice(j + 1);
      if (exp < -4 || x >= 1e21 || x.toFixed(0).length > prec) {
        // remove trailing zeroes
        var i = j - 1;
        while (s.charAt(i) === "0") i--;
        if (s.charAt(i) === ".") i--;
        s = s.slice(0, i + 1) + s.slice(j);
        i = s.length;
        if (s.charAt(i - 3) === "e")
          s = s.slice(0, i - 1) + "0" + s.slice(i - 1);
        break;
      } else {
        var p = prec;
        if (exp < 0) {
          p -= exp + 1;
          s = toFixed(x, p);
        } else while (((s = toFixed(x, p)), s.length > prec + 1)) p--;
        if (p) {
          // remove trailing zeroes
          var i = s.length - 1;
          while (s.charAt(i) === "0") i--;
          if (s.charAt(i) === ".") i--;
          s = s.slice(0, i + 1);
        }
      }
      break;
  }
  return pad ? " " + s : s;
}

// ---- Filesystem ----
// These operations used to live in the `bindings` object of
// runtime/wasm/runtime.js. The ones that consult the virtual filesystem
// receive the `vfs` object (kept in the closure, populated from the embedded
// files) as an extra first parameter; the WAT side passes `get_vfs()`.

//Provides: wasm_fs
//If: wasm
var wasm_fs = globalThis.process?.versions?.node && require("node:fs");

//Provides: wasm_on_windows
//If: wasm
var wasm_on_windows =
  globalThis.process?.versions?.node && globalThis.process.platform === "win32";

//Provides: wasm_access_flags
//Requires: wasm_fs
//If: wasm
var wasm_access_flags = wasm_fs
  ? [
      wasm_fs.constants.R_OK,
      wasm_fs.constants.W_OK,
      wasm_fs.constants.X_OK,
      wasm_fs.constants.F_OK,
    ]
  : [];

//Provides: wasm_open_flags
//Requires: wasm_fs
//If: wasm
var wasm_open_flags = wasm_fs
  ? [
      wasm_fs.constants.O_RDONLY,
      wasm_fs.constants.O_WRONLY,
      wasm_fs.constants.O_RDWR,
      wasm_fs.constants.O_APPEND,
      wasm_fs.constants.O_CREAT,
      wasm_fs.constants.O_TRUNC,
      wasm_fs.constants.O_EXCL,
      wasm_fs.constants.O_NONBLOCK,
      wasm_fs.constants.O_NOCTTY,
      wasm_fs.constants.O_DSYNC,
      wasm_fs.constants.O_SYNC,
    ]
  : [];

//Provides: access
//Requires: wasm_fs, wasm_access_flags
//If: wasm
function access(p, flags) {
  return wasm_fs.accessSync(
    p,
    wasm_access_flags.reduce((f, v, i) => (flags & (1 << i) ? f | v : f), 0),
  );
}

//Provides: open
//Requires: wasm_fs, wasm_open_flags
//If: wasm
function open(vfs, p, flags, perm) {
  const vp = vfs.path(p);
  if (vfs.files.has(vp) && !(flags & 2)) {
    const fd = vfs.next_fd++;
    vfs.fds.set(fd, { data: vfs.files.get(vp), offset: 0 });
    return fd;
  }
  return wasm_fs.openSync(
    p,
    wasm_open_flags.reduce((f, v, i) => (flags & (1 << i) ? f | v : f), 0),
    perm,
  );
}

//Provides: close
//Requires: wasm_fs
//If: wasm
function close(vfs, fd) {
  if (vfs.fds.has(fd)) {
    vfs.fds.delete(fd);
    return;
  }
  wasm_fs.closeSync(fd);
}

//Provides: wasm_write_decoder
//If: wasm
var wasm_write_decoder = new TextDecoder("utf-8", { ignoreBOM: 1 });

//Provides: write
//Requires: wasm_fs, wasm_write_decoder
//If: wasm
function write(fd, b, o, l, p) {
  return wasm_fs
    ? wasm_fs.writeSync(fd, b, o, l, p === null ? p : Number(p))
    : (console[fd === 2 ? "error" : "log"](
        typeof b === "string"
          ? b
          : wasm_write_decoder.decode(b.slice(o, o + l)),
      ),
      l);
}

//Provides: read
//Requires: wasm_fs
//If: wasm
function read(vfs, fd, b, o, l, p) {
  const vf = vfs.fds.get(fd);
  if (vf) {
    const pos = p === null ? vf.offset : Number(p);
    const n = Math.min(l, vf.data.length - pos);
    if (n <= 0) return 0;
    b.set(vf.data.subarray(pos, pos + n), o);
    vf.offset = pos + n;
    return n;
  }
  return wasm_fs.readSync(fd, b, o, l, p);
}

//Provides: fsync
//Requires: wasm_fs
//If: wasm
function fsync(fd) {
  return wasm_fs.fsyncSync(fd);
}

//Provides: file_size
//Requires: wasm_fs
//If: wasm
function file_size(vfs, fd) {
  const vf = vfs.fds.get(fd);
  if (vf) return BigInt(vf.data.length);
  return wasm_fs.fstatSync(fd, { bigint: true }).size;
}

//Provides: mkdir
//Requires: wasm_fs
//If: wasm
function mkdir(p, m) {
  return wasm_fs.mkdirSync(p, m);
}

//Provides: rmdir
//Requires: wasm_fs
//If: wasm
function rmdir(p) {
  return wasm_fs.rmdirSync(p);
}

//Provides: link
//Requires: wasm_fs
//If: wasm
function link(d, s) {
  return wasm_fs.linkSync(d, s);
}

//Provides: symlink
//Requires: wasm_fs
//If: wasm
function symlink(t, p, kind) {
  return wasm_fs.symlinkSync(t, p, [null, "file", "dir"][kind]);
}

//Provides: readlink
//Requires: wasm_fs
//If: wasm
function readlink(p) {
  return wasm_fs.readlinkSync(p);
}

//Provides: unlink
//Requires: wasm_fs
//If: wasm
function unlink(p) {
  return wasm_fs.unlinkSync(p);
}

//Provides: read_dir
//Requires: wasm_fs
//If: wasm
function read_dir(vfs, p) {
  const vp = vfs.path(p);
  const prefix = vp.endsWith("/") ? vp : vp + "/";
  const entries = new Set();
  for (const name of vfs.files.keys()) {
    if (name.startsWith(prefix)) {
      const rest = name.slice(prefix.length);
      const slash = rest.indexOf("/");
      entries.add(slash < 0 ? rest : rest.slice(0, slash));
    }
  }
  if (wasm_fs) {
    try {
      for (const e of wasm_fs.readdirSync(p)) entries.add(e);
    } catch (e) {
      if (entries.size === 0) throw e;
    }
  }
  return [...entries];
}

//Provides: opendir
//Requires: wasm_fs
//If: wasm
// node's Dir.readSync does not report "." and ".."; native does
function opendir(p) {
  return { dir: wasm_fs.opendirSync(p), dots: [".", ".."] };
}

//Provides: readdir
//If: wasm
function readdir(d) {
  if (d.dots.length > 0) return d.dots.shift();
  var n = d.dir.readSync()?.name;
  return n === undefined ? null : n;
}

//Provides: closedir
//If: wasm
function closedir(d) {
  d.dots = []; // a read on the closed handle must fail, not return "." / ".."
  d.dir.closeSync();
}

//Provides: chmod
//Requires: wasm_fs
//If: wasm
function chmod(p, perms) {
  return wasm_fs.chmodSync(p, perms);
}

//Provides: fchmod
//Requires: wasm_fs
//If: wasm
function fchmod(p, perms) {
  return wasm_fs.fchmodSync(p, perms);
}

//Provides: file_exists
//Requires: wasm_fs
//If: wasm
function file_exists(vfs, p) {
  const vp = vfs.path(p);
  if (vfs.files.has(vp) || vfs.dirs.has(vp)) return 1;
  return wasm_fs ? +wasm_fs.existsSync(p) : 0;
}

//Provides: is_directory
//Requires: wasm_fs
//If: wasm
function is_directory(vfs, p) {
  const vp = vfs.path(p);
  if (vfs.dirs.has(vp)) return 1;
  if (vfs.files.has(vp)) return 0;
  // Follow symlinks, like native and the JS runtime (statSync, not lstat).
  return +wasm_fs.statSync(p).isDirectory();
}

//Provides: is_file
//Requires: wasm_fs
//If: wasm
function is_file(vfs, p) {
  const vp = vfs.path(p);
  if (vfs.files.has(vp)) return 1;
  if (vfs.dirs.has(vp)) return 0;
  // Follow symlinks, like native and the JS runtime (statSync, not lstat).
  return +wasm_fs.statSync(p).isFile();
}

//Provides: utimes
//Requires: wasm_fs
//If: wasm
function utimes(p, a, m) {
  return wasm_fs.utimesSync(p, a, m);
}

//Provides: truncate
//Requires: wasm_fs
//If: wasm
function truncate(p, l) {
  return wasm_fs.truncateSync(p, l);
}

//Provides: ftruncate
//Requires: wasm_fs
//If: wasm
function ftruncate(fd, l) {
  return wasm_fs.ftruncateSync(fd, l);
}

//Provides: rename
//Requires: wasm_fs, wasm_on_windows
//If: wasm
function rename(o, n) {
  var n_stat;
  if (
    wasm_on_windows &&
    (n_stat = wasm_fs.statSync(n, { throwIfNoEntry: false })) &&
    wasm_fs.statSync(o, { throwIfNoEntry: false })?.isDirectory()
  ) {
    if (n_stat.isDirectory()) {
      if (!n.startsWith(o))
        try {
          wasm_fs.rmdirSync(n);
        } catch {}
    } else {
      var e = new Error(`ENOTDIR: not a directory, rename '${o}' -> '${n}'`);
      throw Object.assign(e, {
        errno: -20,
        code: "ENOTDIR",
        syscall: "rename",
        path: n,
      });
    }
  }
  wasm_fs.renameSync(o, n);
}

//Provides: tmpdir
//If: wasm
function tmpdir() {
  return require("node:os").tmpdir();
}

//Provides: register_file
//If: wasm
function register_file(vfs, name, data) {
  name = vfs.path(name);
  vfs.files.set(name, data);
  let dir = name;
  while (true) {
    const i = dir.lastIndexOf("/");
    if (i <= 0) break;
    dir = dir.slice(0, i);
    vfs.dirs.add(dir);
  }
}

//Provides: read_file
//If: wasm
function read_file(vfs, name) {
  return vfs.files.get(vfs.path(name)) ?? null;
}

// stat/lstat/fstat return the record fields as a plain JS array; the WAT side
// reads it and allocates the OCaml [stats] record (choosing the int or int64
// layout for [st_size] based on the [large] flag it already holds).
//Provides: wasm_stat
//If: wasm
function wasm_stat(s) {
  var kind;
  if (s.isFile()) kind = 0;
  else if (s.isDirectory()) kind = 1;
  else if (s.isCharacterDevice()) kind = 2;
  else if (s.isBlockDevice()) kind = 3;
  else if (s.isSymbolicLink()) kind = 4;
  else if (s.isFIFO()) kind = 5;
  else if (s.isSocket()) kind = 6;
  return [
    s.dev,
    s.ino | 0,
    kind,
    s.mode & 0o7777,
    s.nlink,
    s.uid,
    s.gid,
    s.rdev,
    s.size,
    s.atimeMs / 1000,
    s.mtimeMs / 1000,
    s.ctimeMs / 1000,
  ];
}

//Provides: stat
//Requires: wasm_fs, wasm_stat
//If: wasm
function stat(p) {
  return wasm_stat(wasm_fs.statSync(p));
}

//Provides: lstat
//Requires: wasm_fs, wasm_stat
//If: wasm
function lstat(p) {
  return wasm_stat(wasm_fs.lstatSync(p));
}

//Provides: fstat
//Requires: wasm_fs, wasm_stat
//If: wasm
function fstat(fd) {
  return wasm_stat(wasm_fs.fstatSync(fd));
}

// ---- Dynamic linking ----
// These instantiate new Wasm modules against the runtime's import object and
// options, which live in the runtime.js closure and are handed over through
// the get_link_state binding ({ imports, options }).

//Provides: register_fragments
//If: wasm
function register_fragments(state, unitName, fragmentsSource) {
  // biome-ignore lint/security/noGlobalEval: ..
  const frags = eval?.(fragmentsSource);
  state.imports[unitName + ".fragments"] = frags;
}

//Provides: load_module
//If: wasm
function load_module(state, wasmBytes) {
  const module = new WebAssembly.Module(wasmBytes, state.options);
  const inst = new WebAssembly.Instance(module, state.imports);
  Object.assign(state.imports.OCaml, inst.exports);
  return inst.exports["_dynlink.init"]();
}

//Provides: load_wasmo
//If: wasm
function load_wasmo(state, zipBytes) {
  const decoder = new TextDecoder("utf-8", { ignoreBOM: 1 });
  // Parse ZIP to extract code.wasm and link_order (uncompressed ZIP)
  const dv = new DataView(
    zipBytes.buffer,
    zipBytes.byteOffset,
    zipBytes.byteLength,
  );
  const len = zipBytes.byteLength;
  // Find End of Central Directory record (search backwards)
  let eocdOff = len - 22;
  while (eocdOff >= 0 && dv.getUint32(eocdOff, true) !== 0x06054b50) eocdOff--;
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
  if (!entries["code.wasm"]) throw new Error("code.wasm not found in .wasmo");
  const module = new WebAssembly.Module(entries["code.wasm"], state.options);
  const inst = new WebAssembly.Instance(module, state.imports);
  Object.assign(state.imports.OCaml, inst.exports);
  const names = decoder.decode(entries.link_order).split("\x00");
  for (const name of names) inst.exports[name + ".init"]();
}

// ---- Assorted self-contained primitives ----

//Provides: gettimeofday
//If: wasm
function gettimeofday() {
  return Date.now() / 1000;
}

//Provides: mktime
//If: wasm
function mktime(year, month, day, h, m, s) {
  return new Date(year, month, day, h, m, s).getTime();
}

//Provides: random_seed
//If: wasm
function random_seed() {
  return crypto.getRandomValues(new Int32Array(12));
}

//Provides: time
//If: wasm
function time() {
  return performance.now();
}

//Provides: weak_new
//If: wasm
function weak_new(v) {
  return new WeakRef(v);
}

//Provides: weak_deref
//If: wasm
function weak_deref(w) {
  var v = w.deref();
  return v === undefined ? null : v;
}

//Provides: weak_map_new
//If: wasm
function weak_map_new() {
  return new WeakMap();
}

//Provides: system
//If: wasm
function system(c) {
  var res = require("node:child_process").spawnSync(c, {
    shell: true,
    stdio: "inherit",
  });
  if (res.error) throw res.error;
  return res.signal ? 255 : res.status;
}

//Provides: isatty
//If: wasm
// In a browser there is no tty and no [require]; report false instead of
// throwing on the undefined [require].
function isatty(fd) {
  return globalThis.process?.versions?.node
    ? +require("node:tty").isatty(fd)
    : 0;
}

//Provides: getuid
//If: wasm
function getuid() {
  return globalThis.process?.getuid ? globalThis.process.getuid() : 1;
}

//Provides: geteuid
//If: wasm
function geteuid() {
  return globalThis.process?.geteuid ? globalThis.process.geteuid() : 1;
}

//Provides: getgid
//If: wasm
function getgid() {
  return globalThis.process?.getgid ? globalThis.process.getgid() : 1;
}

//Provides: getegid
//If: wasm
function getegid() {
  return globalThis.process?.getegid ? globalThis.process.getegid() : 1;
}

//Provides: log
//If: wasm
function log(x) {
  console.log(x);
}

//Provides: wrap_fun_arguments
//If: wasm
function wrap_fun_arguments(f) {
  return (...args) => f(args);
}

//Provides: wasm_hash_int
//If: wasm
function wasm_hash_int(h, d) {
  d = Math.imul(d, 0xcc9e2d51 | 0);
  d = (d << 15) | (d >>> 17); // ROTL32(d, 15);
  d = Math.imul(d, 0x1b873593);
  h ^= d;
  h = (h << 13) | (h >>> 19); //ROTL32(h, 13);
  return (((h + (h << 2)) | 0) + (0xe6546b64 | 0)) | 0;
}

//Provides: wasm_jsstring_is_bytes
//If: wasm
function wasm_jsstring_is_bytes(s) {
  // Whether every code unit fits in a byte, i.e. no code point above U+00FF.
  for (var i = 0; i < s.length; i++) if (s.charCodeAt(i) > 0xff) return false;
  return true;
}

//Provides: wasm_hash_mix_jsbytes
//Requires: wasm_hash_int
//If: wasm
function wasm_hash_mix_jsbytes(h, s) {
  // Mix a byte string four code units per word, like the JS runtime.
  var len = s.length,
    i,
    w;
  for (i = 0; i + 4 <= len; i += 4) {
    w =
      s.charCodeAt(i) |
      (s.charCodeAt(i + 1) << 8) |
      (s.charCodeAt(i + 2) << 16) |
      (s.charCodeAt(i + 3) << 24);
    h = wasm_hash_int(h, w);
  }
  w = 0;
  switch (len & 3) {
    // biome-ignore lint/suspicious/noFallthroughSwitchClause: falls through
    case 3:
      w = s.charCodeAt(i + 2) << 16;
    // falls through
    // biome-ignore lint/suspicious/noFallthroughSwitchClause: falls through
    case 2:
      w |= s.charCodeAt(i + 1) << 8;
    // falls through
    case 1:
      w |= s.charCodeAt(i);
      h = wasm_hash_int(h, w);
  }
  return h ^ len;
}

//Provides: hash_string
//Requires: wasm_hash_int, wasm_jsstring_is_bytes, wasm_hash_mix_jsbytes
//If: wasm
function hash_string(h, s) {
  // A string whose code units all fit in a byte (every ASCII string, all of
  // Latin-1) is mixed as bytes, leaving its hash unchanged and matching the
  // JS runtime.
  if (wasm_jsstring_is_bytes(s)) return wasm_hash_mix_jsbytes(h, s);
  // Genuine Unicode text: mix two 16-bit code units per word, so no
  // information is lost (packing them four per word at byte offsets would
  // overlap their high bits).
  var len = s.length,
    i,
    w;
  for (i = 0; i + 2 <= len; i += 2) {
    w = s.charCodeAt(i) | (s.charCodeAt(i + 1) << 16);
    h = wasm_hash_int(h, w);
  }
  if (len & 1) h = wasm_hash_int(h, s.charCodeAt(i));
  return h ^ len;
}

//Provides: map_new
//If: wasm
function map_new() {
  return new Map();
}

//Provides: map_get
//If: wasm
function map_get(m, x) {
  var v = m.get(x);
  return v === undefined ? null : v;
}

//Provides: map_set
//If: wasm
function map_set(m, x, v) {
  return m.set(x, v);
}

//Provides: map_delete
//If: wasm
function map_delete(m, x) {
  return m.delete(x);
}

//Provides: chdir
//If: wasm
function chdir(x) {
  return globalThis.process.chdir(x);
}

// ---- Typed arrays and DataViews (bigarray/bigstring/marshal) ----

//Provides: wasm_typed_arrays
//If: wasm
var wasm_typed_arrays = [
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

// See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/stack
//Provides: wasm_isV8
//If: wasm
var wasm_isV8 = new Error().stack?.includes("\n    at ") ?? false;

//Provides: wasm_dv_call
//If: wasm
var wasm_dv_call = Function.prototype.call;

//Provides: wasm_DV
//If: wasm
var wasm_DV = DataView.prototype;

//Provides: ta_create
//Requires: wasm_typed_arrays
//If: wasm
function ta_create(k, sz) {
  return new wasm_typed_arrays[k](sz);
}

//Provides: ta_normalize
//If: wasm
function ta_normalize(a) {
  return a instanceof Uint32Array
    ? new Int32Array(a.buffer, a.byteOffset, a.length)
    : a;
}

//Provides: ta_kind
//Requires: wasm_typed_arrays
//If: wasm
function ta_kind(a) {
  return wasm_typed_arrays.findIndex((c) => a instanceof c);
}

//Provides: ta_length
//If: wasm
function ta_length(a) {
  return a.length;
}

//Provides: ta_get_i32
//If: wasm
function ta_get_i32(a, i) {
  return a[i];
}

//Provides: ta_fill
//If: wasm
function ta_fill(a, v) {
  return a.fill(v);
}

//Provides: ta_blit
//If: wasm
function ta_blit(s, d) {
  return d.set(s);
}

//Provides: ta_subarray
//If: wasm
function ta_subarray(a, i, j) {
  return a.subarray(i, j);
}

//Provides: ta_set
//If: wasm
function ta_set(a, b, i) {
  return a.set(b, i);
}

//Provides: ta_new
//If: wasm
function ta_new(len) {
  return new Uint8Array(len);
}

//Provides: ta_copy
//If: wasm
function ta_copy(ta, t, s, e) {
  return ta.copyWithin(t, s, e);
}

//Provides: ta_bytes
//If: wasm
function ta_bytes(a) {
  return new Uint8Array(a.buffer, a.byteOffset, a.length * a.BYTES_PER_ELEMENT);
}

//Provides: dv_make
//If: wasm
function dv_make(a) {
  return new DataView(a.buffer, a.byteOffset, a.byteLength);
}

//Provides: dv_get_f64
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_get_f64 = wasm_dv_call.bind(wasm_DV.getFloat64);

//Provides: dv_get_f32
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_get_f32 = wasm_dv_call.bind(wasm_DV.getFloat32);

//Provides: dv_get_i64
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_get_i64 = wasm_dv_call.bind(wasm_DV.getBigInt64);

//Provides: dv_get_i32
//Requires: wasm_dv_call, wasm_DV, wasm_isV8
//If: wasm
// 2026-03-16: using call.bind is faster in V8 which recognize the primitive
// and generate inlined call, but calling a JavaScript function is currently
// faster in other engines which does not have this optimization yet. Use
// benchmarks/bench_dv_getint32.js for performance comparisons.
var dv_get_i32 = wasm_isV8
  ? wasm_dv_call.bind(wasm_DV.getInt32)
  : (x, y, z) => x.getInt32(y, z);

//Provides: dv_get_i16
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_get_i16 = wasm_dv_call.bind(wasm_DV.getInt16);

//Provides: dv_get_ui16
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_get_ui16 = wasm_dv_call.bind(wasm_DV.getUint16);

//Provides: dv_get_i8
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_get_i8 = wasm_dv_call.bind(wasm_DV.getInt8);

//Provides: dv_get_ui8
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_get_ui8 = wasm_dv_call.bind(wasm_DV.getUint8);

//Provides: dv_set_f64
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_set_f64 = wasm_dv_call.bind(wasm_DV.setFloat64);

//Provides: dv_set_f32
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_set_f32 = wasm_dv_call.bind(wasm_DV.setFloat32);

//Provides: dv_set_i64
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_set_i64 = wasm_dv_call.bind(wasm_DV.setBigInt64);

//Provides: dv_set_i32
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_set_i32 = wasm_dv_call.bind(wasm_DV.setInt32);

//Provides: dv_set_i16
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_set_i16 = wasm_dv_call.bind(wasm_DV.setInt16);

//Provides: dv_set_i8
//Requires: wasm_dv_call, wasm_DV
//If: wasm
var dv_set_i8 = wasm_dv_call.bind(wasm_DV.setInt8);

//Provides: littleEndian
//If: wasm
var littleEndian = new Uint8Array(new Uint32Array([1]).buffer)[0];

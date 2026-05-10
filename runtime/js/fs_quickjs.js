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

// Real-disk device backed by QuickJS-NG's [os] / [std] modules. Active
// when the runtime detects a qjs host (no `process`, but `os.open` is
// available). Mirrors the [MlNodeDevice] interface; primitives qjs
// does not expose ([access], [chmod], [truncate], [link]) raise
// "not implemented".

//Provides: fs_quickjs_supported
function fs_quickjs_supported() {
  return (
    globalThis.process?.versions?.node === undefined &&
    typeof globalThis.os?.open === "function"
  );
}
//Provides: fs_quickjs_supported
//If: browser
function fs_quickjs_supported() {
  return false;
}

// POSIX errno -> Unix-error-code string. Linux values; CI runs on
// Ubuntu so this is the only platform we rely on for now.
//Provides: caml_qjs_errno_code
function caml_qjs_errno_code(errno) {
  switch (-errno) {
    case 1:
      return "EPERM";
    case 2:
      return "ENOENT";
    case 5:
      return "EIO";
    case 9:
      return "EBADF";
    case 11:
      return "EAGAIN";
    case 13:
      return "EACCES";
    case 17:
      return "EEXIST";
    case 20:
      return "ENOTDIR";
    case 21:
      return "EISDIR";
    case 22:
      return "EINVAL";
    case 24:
      return "EMFILE";
    case 28:
      return "ENOSPC";
    case 29:
      return "ESPIPE";
    case 30:
      return "EROFS";
    case 32:
      return "EPIPE";
    case 36:
      return "ENAMETOOLONG";
    case 39:
      return "ENOTEMPTY";
    case 40:
      return "ELOOP";
    default:
      return "EUNKNOWN";
  }
}

//Provides: caml_raise_qjs_error
//Requires: caml_raise_system_error, caml_qjs_errno_code
function caml_raise_qjs_error(errno, syscall, path, raise_unix) {
  var code = caml_qjs_errno_code(errno);
  caml_raise_system_error(raise_unix, code, syscall, "qjs " + syscall, path);
}

//Provides: ocaml_stats_from_qjs_stats
//Requires: caml_int64_of_float
function ocaml_stats_from_qjs_stats(s, large) {
  var os = globalThis.os;
  var kind_bits = s.mode & (os.S_IFMT || 0o170000);
  var file_kind = 0;
  if (kind_bits === os.S_IFREG) file_kind = 0;
  else if (kind_bits === os.S_IFDIR) file_kind = 1;
  else if (kind_bits === os.S_IFCHR) file_kind = 2;
  else if (kind_bits === os.S_IFBLK) file_kind = 3;
  else if (kind_bits === os.S_IFLNK) file_kind = 4;
  else if (kind_bits === os.S_IFIFO) file_kind = 5;
  else if (kind_bits === os.S_IFSOCK) file_kind = 6;
  return BLOCK(
    0,
    s.dev | 0,
    s.ino | 0,
    file_kind,
    s.mode & 0o7777,
    s.nlink | 0,
    s.uid | 0,
    s.gid | 0,
    s.rdev | 0,
    large ? caml_int64_of_float(s.size) : s.size | 0,
    s.atime / 1000,
    s.mtime / 1000,
    s.ctime / 1000,
  );
}

//Provides: MlQuickJSDevice
//Requires: MlQuickJSFd, caml_raise_qjs_error, caml_raise_sys_error
//Requires: caml_string_of_jsstring, caml_failwith
//Requires: ocaml_stats_from_qjs_stats
class MlQuickJSDevice {
  constructor(root) {
    this.os = globalThis.os;
    this.root = root;
  }

  nm(name) {
    return this.root + name;
  }

  exists(name) {
    var r = this.os.stat(this.nm(name));
    return r[1] === 0 ? 1 : 0;
  }

  isFile(name) {
    var r = this.os.stat(this.nm(name));
    if (r[1] !== 0) caml_raise_sys_error("stat " + this.nm(name));
    return (r[0].mode & this.os.S_IFMT) === this.os.S_IFREG ? 1 : 0;
  }

  is_dir(name) {
    var r = this.os.stat(this.nm(name));
    if (r[1] !== 0) caml_raise_sys_error("stat " + this.nm(name));
    return (r[0].mode & this.os.S_IFMT) === this.os.S_IFDIR ? 1 : 0;
  }

  mkdir(name, mode, raise_unix) {
    var err = this.os.mkdir(this.nm(name), mode);
    if (err !== 0)
      caml_raise_qjs_error(err, "mkdir", this.nm(name), raise_unix);
    return 0;
  }

  // qjs has no rmdir; os.remove handles directories (fails if non-empty).
  rmdir(name, raise_unix) {
    var err = this.os.remove(this.nm(name));
    if (err !== 0)
      caml_raise_qjs_error(err, "rmdir", this.nm(name), raise_unix);
    return 0;
  }

  readdir(name, raise_unix) {
    var r = this.os.readdir(this.nm(name));
    if (r[1] !== 0)
      caml_raise_qjs_error(r[1], "readdir", this.nm(name), raise_unix);
    // qjs includes "." and ".." — strip to match Node.
    return r[0].filter(function (x) {
      return x !== "." && x !== "..";
    });
  }

  unlink(name, raise_unix) {
    var err = this.os.remove(this.nm(name));
    if (err !== 0)
      caml_raise_qjs_error(err, "unlink", this.nm(name), raise_unix);
    return 0;
  }

  utimes(name, atime, mtime, raise_unix) {
    if (atime === 0 && mtime === 0) {
      atime = Date.now() / 1000;
      mtime = atime;
    }
    var err = this.os.utimes(
      this.nm(name),
      Math.round(atime * 1000),
      Math.round(mtime * 1000),
    );
    if (err !== 0)
      caml_raise_qjs_error(err, "utimes", this.nm(name), raise_unix);
    return 0;
  }

  truncate(_name, _len, _raise_unix) {
    caml_failwith("MlQuickJSDevice.truncate: not implemented");
  }

  // qjs has no access(); fall back to stat for "f" (existence) only.
  access(name, _f, raise_unix) {
    var r = this.os.stat(this.nm(name));
    if (r[1] !== 0)
      caml_raise_qjs_error(r[1], "access", this.nm(name), raise_unix);
    return 0;
  }

  open(name, f, perms, raise_unix) {
    var os = this.os;
    var flags = 0;
    for (var key in f) {
      switch (key) {
        case "rdonly":
          flags |= os.O_RDONLY;
          break;
        case "wronly":
          flags |= os.O_WRONLY;
          break;
        case "rdwr":
          flags |= os.O_RDWR;
          break;
        case "append":
          flags |= os.O_APPEND;
          break;
        case "create":
          flags |= os.O_CREAT;
          break;
        case "truncate":
          flags |= os.O_TRUNC;
          break;
        case "excl":
          flags |= os.O_EXCL;
          break;
        // binary/text/nonblock/noctty/dsync/sync: not in qjs, ignored.
      }
    }
    var fd = os.open(this.nm(name), flags, perms || 0o666);
    if (fd < 0) caml_raise_qjs_error(fd, "open", this.nm(name), raise_unix);
    return new MlQuickJSFd(fd, f, this.nm(name));
  }

  rename(o, n, raise_unix) {
    var err = this.os.rename(this.nm(o), this.nm(n));
    if (err !== 0) caml_raise_qjs_error(err, "rename", this.nm(o), raise_unix);
  }

  stat(name, large, raise_unix) {
    var r = this.os.stat(this.nm(name));
    if (r[1] !== 0)
      caml_raise_qjs_error(r[1], "stat", this.nm(name), raise_unix);
    return ocaml_stats_from_qjs_stats(r[0], large);
  }

  lstat(name, large, raise_unix) {
    var r = this.os.lstat(this.nm(name));
    if (r[1] !== 0)
      caml_raise_qjs_error(r[1], "lstat", this.nm(name), raise_unix);
    return ocaml_stats_from_qjs_stats(r[0], large);
  }

  chmod(_name, _perms, _raise_unix) {
    caml_failwith("MlQuickJSDevice.chmod: not implemented");
  }

  link(_target, _path, _raise_unix) {
    caml_failwith("MlQuickJSDevice.link: not implemented");
  }

  symlink(_to_dir, target, path, raise_unix) {
    var err = this.os.symlink(target, this.nm(path));
    if (err !== 0)
      caml_raise_qjs_error(err, "symlink", this.nm(path), raise_unix);
    return 0;
  }

  readlink(name, raise_unix) {
    var r = this.os.readlink(this.nm(name));
    if (r[1] !== 0)
      caml_raise_qjs_error(r[1], "readlink", this.nm(name), raise_unix);
    return caml_string_of_jsstring(r[0]);
  }

  opendir(_name, _raise_unix) {
    caml_failwith("MlQuickJSDevice.opendir: not implemented");
  }
}

//Provides: MlQuickJSDevice
//If: browser
class MlQuickJSDevice {}

//Provides: MlQuickJSFd
//Requires: MlFile, caml_failwith, caml_raise_qjs_error, caml_raise_system_error
//Requires: ocaml_stats_from_qjs_stats
class MlQuickJSFd extends MlFile {
  constructor(fd, flags, path) {
    super();
    this.os = globalThis.os;
    this.fd = fd;
    this.flags = flags;
    this.path = path;
    if (flags.append) {
      var r = this.os.stat(path);
      this.offset = r[1] === 0 ? r[0].size : 0;
    } else {
      this.offset = 0;
    }
    this.seeked = false;
    flags.noSeek = false;
  }

  isatty() {
    return this.os.isatty(this.fd) ? 1 : 0;
  }

  length() {
    var cur = this.offset;
    var end = this.os.seek(this.fd, 0, 2 /* SEEK_END */);
    this.os.seek(this.fd, cur, 0 /* SEEK_SET */);
    return end | 0;
  }

  // qjs's os.read/os.write take an ArrayBuffer + byte offset.
  // The runtime hands us either a Uint8Array view or a raw buffer.

  read(a, buf_offset, len, raise_unix) {
    if (this.seeked) this.os.seek(this.fd, this.offset, 0);
    var ab = a.buffer || a;
    var base = a.byteOffset || 0;
    var n = this.os.read(this.fd, ab, base + buf_offset, len);
    if (n < 0) caml_raise_qjs_error(n, "read", this.path, raise_unix);
    this.offset += n;
    return n;
  }

  write(buf, buf_offset, len, raise_unix) {
    if (this.seeked) this.os.seek(this.fd, this.offset, 0);
    var ab = buf.buffer || buf;
    var base = buf.byteOffset || 0;
    var n = this.os.write(this.fd, ab, base + buf_offset, len);
    if (n < 0) caml_raise_qjs_error(n, "write", this.path, raise_unix);
    this.offset += n;
    return n;
  }

  seek(offset, whence, raise_unix) {
    switch (whence) {
      case 0:
        break;
      case 1:
        offset += this.offset;
        break;
      case 2:
        offset += this.length();
        break;
    }
    if (offset < 0)
      caml_raise_system_error(
        raise_unix,
        "EINVAL",
        "lseek",
        "invalid argument",
      );
    this.offset = offset;
    this.seeked = true;
    return this.offset;
  }

  pos() {
    return this.offset;
  }

  truncate(_len, _raise_unix) {
    caml_failwith("MlQuickJSFd.truncate: not implemented");
  }

  stat(large) {
    var r = this.os.stat(this.path);
    if (r[1] !== 0) caml_raise_qjs_error(r[1], "fstat", this.path, 1);
    return ocaml_stats_from_qjs_stats(r[0], large);
  }

  chmod(_perms) {
    caml_failwith("MlQuickJSFd.chmod: not implemented");
  }

  sync() {
    return 0;
  }

  close(raise_unix) {
    var err = this.os.close(this.fd);
    if (err !== 0) caml_raise_qjs_error(err, "close", this.path, raise_unix);
    return 0;
  }

  check_stream_semantics(_cmd) {
    return 0;
  }
}

//Provides: MlQuickJSFd
//If: browser
class MlQuickJSFd {}

// Stream FD wrapping the host's stdin / stdout / stderr (qjs fd 0 / 1 /
// 2) via os.read / os.write. Used as the qjs counterpart of MlNodeFd
// for the standard streams; opt-in like the rest of fs_quickjs.

//Provides: MlQuickJSStdFd
//Requires: MlFile, caml_raise_system_error
class MlQuickJSStdFd extends MlFile {
  constructor(fd, flags) {
    super();
    this.os = globalThis.os;
    this.fd = fd;
    this.flags = flags;
    flags.noSeek = true;
  }

  isatty() {
    return this.os.isatty(this.fd) ? 1 : 0;
  }

  length() {
    return 0;
  }

  read(a, buf_offset, len, raise_unix) {
    if (this.fd !== 0)
      caml_raise_system_error(
        raise_unix,
        "EBADF",
        "read",
        "bad file descriptor",
      );
    var ab = a.buffer || a;
    var base = a.byteOffset || 0;
    var n = this.os.read(this.fd, ab, base + buf_offset, len);
    return n < 0 ? 0 : n;
  }

  write(buf, buf_offset, len, raise_unix) {
    if (this.fd !== 1 && this.fd !== 2)
      caml_raise_system_error(
        raise_unix,
        "EBADF",
        "write",
        "bad file descriptor",
      );
    var ab = buf.buffer || buf;
    var base = buf.byteOffset || 0;
    var n = this.os.write(this.fd, ab, base + buf_offset, len);
    return n < 0 ? 0 : n;
  }

  seek(_offset, _whence, raise_unix) {
    caml_raise_system_error(raise_unix, "ESPIPE", "lseek", "illegal seek");
  }

  pos() {
    return -1;
  }

  truncate(_len, raise_unix) {
    caml_raise_system_error(
      raise_unix,
      "EINVAL",
      "ftruncate",
      "invalid argument",
    );
  }

  close(_raise_unix) {
    return 0;
  }

  check_stream_semantics(_cmd) {
    return 0;
  }
}

//Provides: MlQuickJSStdFd
//If: browser
class MlQuickJSStdFd {}

//Provides: caml_sys_open_for_quickjs
//Requires: MlQuickJSStdFd
function caml_sys_open_for_quickjs(fd, flags) {
  return new MlQuickJSStdFd(fd, flags);
}

//Provides: caml_sys_open_for_quickjs
//If: browser
function caml_sys_open_for_quickjs(_fd, _flags) {
  return null;
}

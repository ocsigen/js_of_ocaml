// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2014 Jérôme Vouillon, Hugo Heuzard
// Laboratoire PPS - CNRS Université Paris Diderot
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

//Provides: fs_node_supported
function fs_node_supported() {
  return globalThis.process?.versions?.node !== undefined;
}
//Provides: fs_node_supported
//If: browser
function fs_node_supported() {
  return false;
}

//Provides: MlNodeDevice
//Requires: MlNodeFd, caml_raise_sys_error, caml_string_of_jsstring
//Requires: caml_raise_nodejs_error, fs_node_stats_from_js
class MlNodeDevice {
  constructor(root) {
    this.fs = require("node:fs");
    this.root = root;
  }

  nm(name) {
    return this.root + name;
  }

  exists(name) {
    try {
      return this.fs.existsSync(this.nm(name)) ? 1 : 0;
    } catch (err) {
      return 0;
    }
  }

  isFile(name) {
    try {
      return this.fs.statSync(this.nm(name)).isFile() ? 1 : 0;
    } catch (err) {
      caml_raise_sys_error(err.toString());
    }
  }

  mkdir(name, mode, raise_unix) {
    try {
      this.fs.mkdirSync(this.nm(name), { mode: mode });
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  rmdir(name, raise_unix) {
    try {
      this.fs.rmdirSync(this.nm(name));
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  readdir(name, raise_unix) {
    try {
      return this.fs.readdirSync(this.nm(name));
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  is_dir(name) {
    try {
      return this.fs.statSync(this.nm(name)).isDirectory() ? 1 : 0;
    } catch (err) {
      caml_raise_sys_error(err.toString());
    }
  }

  unlink(name, raise_unix) {
    try {
      this.fs.unlinkSync(this.nm(name));
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  utimes(name, atime, mtime, raise_unix) {
    try {
      if (atime === 0 && mtime === 0) {
        atime = new Date().getTime() / 1000;
        mtime = atime;
      }
      this.fs.utimesSync(this.nm(name), atime, mtime);
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  truncate(name, len, raise_unix) {
    try {
      this.fs.truncateSync(this.nm(name), len | 0);
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  access(name, f, raise_unix) {
    var consts = require("node:fs").constants;
    var res = 0;
    for (var key in f) {
      switch (key) {
        case "r":
          res |= consts.R_OK;
          break;
        case "w":
          res |= consts.W_OK;
          break;
        case "x":
          res |=
            globalThis.process?.platform === "win32"
              ? consts.R_OK
              : consts.X_OK;
          break;
        case "f":
          res |= consts.F_OK;
          break;
      }
    }
    try {
      this.fs.accessSync(this.nm(name), res);
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  open(name, f, perms, raise_unix) {
    var consts = require("node:fs").constants;
    var res = 0;
    for (var key in f) {
      switch (key) {
        case "rdonly":
          res |= consts.O_RDONLY;
          break;
        case "wronly":
          res |= consts.O_WRONLY;
          break;
        case "rdwr":
          res |= consts.O_RDWR;
          break;
        case "append":
          res |= consts.O_APPEND;
          break;
        case "create":
          res |= consts.O_CREAT;
          break;
        case "truncate":
          res |= consts.O_TRUNC;
          break;
        case "excl":
          res |= consts.O_EXCL;
          break;
        case "binary":
          res |= consts.O_BINARY;
          break;
        case "text":
          res |= consts.O_TEXT;
          break;
        case "nonblock":
          res |= consts.O_NONBLOCK;
          break;
        case "noctty":
          res |= consts.O_NOCTTY;
          break;
        case "dsync":
          res |= consts.O_DSYNC;
          break;
        case "sync":
          res |= consts.O_SYNC;
          break;
      }
    }
    try {
      var fd = this.fs.openSync(this.nm(name), res, perms);
      return new MlNodeFd(fd, f);
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  rename(o, n, raise_unix) {
    if (globalThis.process?.platform === "win32") {
      try {
        var target = this.nm(n);
        var source = this.nm(o);
        var target_stats, source_stats;
        if (
          (target_stats = this.fs.statSync(target, {
            throwIfNoEntry: false,
          })) &&
          (source_stats = this.fs.statSync(source, {
            throwIfNoEntry: false,
          })) &&
          source_stats.isDirectory()
        ) {
          if (target_stats.isDirectory()) {
            if (!target.startsWith(source))
              try {
                this.fs.rmdirSync(target);
              } catch {}
          } else {
            var err = new Error(
              `ENOTDIR: not a directory, rename '${source}' -> '${target}'`,
            );
            throw Object.assign(err, {
              errno: -20,
              code: "ENOTDIR",
              syscall: "rename",
              path: target,
            });
          }
        }
        this.fs.renameSync(this.nm(o), this.nm(n));
      } catch (err) {
        caml_raise_nodejs_error(err, raise_unix);
      }
    } else {
      try {
        this.fs.renameSync(this.nm(o), this.nm(n));
      } catch (err) {
        caml_raise_nodejs_error(err, raise_unix);
      }
    }
  }

  stat(name, large, raise_unix) {
    try {
      var js_stats = this.fs.statSync(this.nm(name));
      return fs_node_stats_from_js(js_stats, large);
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  lstat(name, large, raise_unix) {
    try {
      var js_stats = this.fs.lstatSync(this.nm(name));
      return fs_node_stats_from_js(js_stats, large);
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  chmod(name, perms, raise_unix) {
    try {
      this.fs.chmodSync(this.nm(name), perms);
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  link(target, path, raise_unix) {
    try {
      this.fs.linkSync(this.nm(target), this.nm(path));
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  symlink(to_dir, target, path, raise_unix) {
    try {
      this.fs.symlinkSync(
        target,
        this.nm(path),
        to_dir === 0 ? null : to_dir[1] ? "dir" : "file",
      );
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  readlink(name, raise_unix) {
    try {
      var link = this.fs.readlinkSync(this.nm(name), "utf8");
      return caml_string_of_jsstring(link);
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  opendir(name, raise_unix) {
    try {
      return this.fs.opendirSync(this.nm(name));
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }
}

//Provides: fs_node_stats_from_js
//Requires: caml_int64_of_float
function fs_node_stats_from_js(js_stats, large) {
  /* ===Unix.file_kind===
   * type file_kind =
   *     S_REG                       (** Regular file *)
   *   | S_DIR                       (** Directory *)
   *   | S_CHR                       (** Character device *)
   *   | S_BLK                       (** Block device *)
   *   | S_LNK                       (** Symbolic link *)
   *   | S_FIFO                      (** Named pipe *)
   *   | S_SOCK                      (** Socket *)
   */
  var file_kind;
  if (js_stats.isFile()) {
    file_kind = 0;
  } else if (js_stats.isDirectory()) {
    file_kind = 1;
  } else if (js_stats.isCharacterDevice()) {
    file_kind = 2;
  } else if (js_stats.isBlockDevice()) {
    file_kind = 3;
  } else if (js_stats.isSymbolicLink()) {
    file_kind = 4;
  } else if (js_stats.isFIFO()) {
    file_kind = 5;
  } else if (js_stats.isSocket()) {
    file_kind = 6;
  }
  /* ===Unix.stats===
   * type stats =
   *  { st_dev : int;               (** Device number *)
   *    st_ino : int;               (** Inode number *)
   *    st_kind : file_kind;        (** Kind of the file *)
   *    st_perm : file_perm;        (** Access rights *)
   *    st_nlink : int;             (** Number of links *)
   *    st_uid : int;               (** User id of the owner *)
   *    st_gid : int;               (** Group ID of the file's group *)
   *    st_rdev : int;              (** Device ID (if special file) *)
   *    st_size : int;              (** Size in bytes *)
   *    st_atime : float;           (** Last access time *)
   *    st_mtime : float;           (** Last modification time *)
   *    st_ctime : float;           (** Last status change time *)
   *  }
   */
  return BLOCK(
    0,
    js_stats.dev,
    js_stats.ino | 0,
    file_kind,
    js_stats.mode,
    js_stats.nlink,
    js_stats.uid,
    js_stats.gid,
    js_stats.rdev,
    large ? caml_int64_of_float(js_stats.size) : js_stats.size | 0,
    js_stats.atimeMs / 1000,
    js_stats.mtimeMs / 1000,
    js_stats.ctimeMs / 1000,
  );
}

//Provides: MlNodeDevice
//If: browser
class MlNodeDevice {}

//Provides: MlNodeFd
//Requires: MlFile, caml_uint8_array_of_string, caml_uint8_array_of_bytes, caml_bytes_set, caml_raise_sys_error
//Requires: caml_raise_nodejs_error, caml_raise_system_error, fs_node_stats_from_js
class MlNodeFd extends MlFile {
  constructor(fd, flags) {
    super();
    this.fs = require("node:fs");
    this.fd = fd;
    this.flags = flags;
    try {
      var stats = this.fs.fstatSync(fd);
      flags.noSeek =
        stats.isCharacterDevice() || stats.isFIFO() || stats.isSocket();
    } catch (err) {
      // The fstat will fail on standard streams under Windows with node
      // 18 (and lower). See https://github.com/libuv/libuv/pull/3811.
      flags.noSeek = true;
    }
    this.offset = this.flags.append ? stats.size : 0;
    this.seeked = false;
  }

  truncate(len, raise_unix) {
    try {
      this.fs.ftruncateSync(this.fd, len | 0);
      if (this.offset > len) this.offset = len;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  length() {
    try {
      return this.fs.fstatSync(this.fd).size;
    } catch (err) {
      caml_raise_sys_error(err.toString());
    }
  }

  write(buf, buf_offset, len, raise_unix) {
    try {
      if (this.flags.noSeek || !this.seeked) {
        var written = this.fs.writeSync(this.fd, buf, buf_offset, len);
      } else {
        var written = this.fs.writeSync(
          this.fd,
          buf,
          buf_offset,
          len,
          this.offset,
        );
      }
      this.offset += written;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
    return written;
  }

  read(a, buf_offset, len, raise_unix) {
    try {
      if (this.flags.noSeek || !this.seeked) {
        var read = this.fs.readSync(this.fd, a, buf_offset, len);
      } else {
        var read = this.fs.readSync(this.fd, a, buf_offset, len, this.offset);
      }
      this.offset += read;
      return read;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  seek(offset, whence, raise_unix) {
    if (this.flags.noSeek) {
      caml_raise_system_error(raise_unix, "ESPIPE", "lseek", "illegal seek");
    }
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
    if (offset < 0) {
      caml_raise_system_error(
        raise_unix,
        "EINVAL",
        "lseek",
        "invalid argument",
      );
    }
    this.offset = offset;
    this.seeked = true;
    return this.offset;
  }

  stat(large) {
    try {
      var js_stats = this.fs.fstatSync(this.fd);
      return fs_node_stats_from_js(js_stats, large);
    } catch (err) {
      caml_raise_nodejs_error(err, /* raise Unix_error */ 1);
    }
  }

  chmod(perms) {
    try {
      this.fs.fchmodSync(this.fd, perms);
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, /* raise Unix_error */ 1);
    }
  }

  sync() {
    try {
      this.fs.fsyncSync(this.fd);
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, /* raise Unix_error */ 1);
    }
  }

  close(raise_unix) {
    try {
      this.fs.closeSync(this.fd);
      return 0;
    } catch (err) {
      caml_raise_nodejs_error(err, raise_unix);
    }
  }

  check_stream_semantics(cmd) {
    try {
      var js_stats = this.fs.fstatSync(this.fd);
    } catch (err) {
      caml_raise_nodejs_error(err, /* raise Unix_error */ 1, cmd);
    }
    if (
      !(
        js_stats.isFile() ||
        js_stats.isCharacterDevice() ||
        js_stats.isFIFO() ||
        js_stats.isSocket()
      )
    )
      caml_raise_system_error(
        /* raise Unix_error */ 1,
        "EINVAL",
        cmd,
        "invalid argument",
      );
  }
}

//Provides: MlNodeFd
//If: browser
class MlNodeFd {}

//Provides: caml_sys_open_for_node
//Requires: MlNodeFd
function caml_sys_open_for_node(fd, flags) {
  if (flags.altname) {
    try {
      var fs = require("node:fs");
      var fd2 = fs.openSync(flags.altname, "rs");
      return new MlNodeFd(fd2, flags);
    } catch (e) {}
  }
  return new MlNodeFd(fd, flags);
}

//Provides: caml_sys_open_for_node
//If: browser
function caml_sys_open_for_node(fd, flags) {
  return null;
}

//Provides: caml_raise_nodejs_error
//Requires: caml_raise_with_args, make_unix_err_args, caml_named_value
//Requires: caml_raise_sys_error
function caml_raise_nodejs_error(err, raise_unix, cmd) {
  var unix_error = caml_named_value("Unix.Unix_error");
  if (raise_unix && unix_error) {
    var args = make_unix_err_args(
      err.code,
      cmd || err.syscall,
      err.path,
      err.errno,
    );
    caml_raise_with_args(unix_error, args);
  } else {
    caml_raise_sys_error(err.toString());
  }
}

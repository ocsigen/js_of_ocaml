import { caml_ba_to_typed_array } from './bigarray.js';
import { caml_failwith, caml_raise_end_of_file, caml_raise_not_found, caml_raise_with_args } from './fail.js';
import { fs_node_supported } from './fs_node.js';
import { caml_sys_chdir, resolve_fs_device } from './fs.js';
import { caml_int64_to_float } from './int64.js';
import { MlChanid, caml_ml_open_descriptor_in, caml_ml_open_descriptor_out, caml_sys_fds } from './io.js';
import { caml_jsstring_of_string, caml_string_of_jsstring, caml_uint8_array_of_bytes } from './mlBytes.js';
import { caml_named_value } from './stdlib.js';
import { caml_raise_sys_error } from './sys.js';

//Provides: caml_unix_gettimeofday
//Alias: unix_gettimeofday
export function caml_unix_gettimeofday() {
  return new Date().getTime() / 1000;
}

//Provides: caml_unix_time
//Alias: unix_time
export function caml_unix_time() {
  return Math.floor(caml_unix_gettimeofday());
}

//Provides: caml_unix_times
//Alias: unix_times
export function caml_unix_times() {
  if (globalThis.process?.cpuUsage) {
    var t = globalThis.process.cpuUsage();
    return BLOCK(0, t.user / 1e6, t.system / 1e6, 0, 0);
  } else if (globalThis.performance?.now) {
    return BLOCK(0, globalThis.performance.now() / 1000, 0, 0, 0);
  } else {
    caml_failwith("caml_unix_times: not implemented");
  }
}

//Provides: caml_unix_gmtime
//Alias: unix_gmtime
export function caml_unix_gmtime(t) {
  var d = new Date(t * 1000);
  var d_num = d.getTime();
  var januaryfirst = new Date(Date.UTC(d.getUTCFullYear(), 0, 1)).getTime();
  var doy = Math.floor((d_num - januaryfirst) / 86400000);
  return BLOCK(
    0,
    d.getUTCSeconds(),
    d.getUTCMinutes(),
    d.getUTCHours(),
    d.getUTCDate(),
    d.getUTCMonth(),
    d.getUTCFullYear() - 1900,
    d.getUTCDay(),
    doy,
    false | 0 /* for UTC daylight savings time is false */,
  );
}

//Provides: caml_unix_localtime
//Alias: unix_localtime
export function caml_unix_localtime(t) {
  var d = new Date(t * 1000);
  var d_num = d.getTime();
  var januaryfirst = new Date(d.getFullYear(), 0, 1).getTime();
  var doy = Math.floor((d_num - januaryfirst) / 86400000);
  var jan = new Date(d.getFullYear(), 0, 1);
  var jul = new Date(d.getFullYear(), 6, 1);
  var stdTimezoneOffset = Math.max(
    jan.getTimezoneOffset(),
    jul.getTimezoneOffset(),
  );
  return BLOCK(
    0,
    d.getSeconds(),
    d.getMinutes(),
    d.getHours(),
    d.getDate(),
    d.getMonth(),
    d.getFullYear() - 1900,
    d.getDay(),
    doy,
    (d.getTimezoneOffset() < stdTimezoneOffset) |
      0 /* daylight savings time  field. */,
  );
}

//Provides: caml_unix_mktime
//Alias: unix_mktime
export function caml_unix_mktime(tm) {
  var d = new Date(tm[6] + 1900, tm[5], tm[4], tm[3], tm[2], tm[1]).getTime();
  var t = Math.floor(d / 1000);
  var tm2 = caml_unix_localtime(t);
  return BLOCK(0, t, tm2);
}
//Provides: caml_unix_startup const
//Alias: win_startup
export function caml_unix_startup() {}

//Provides: caml_unix_cleanup const
//Alias: win_cleanup
export function caml_unix_cleanup() {}

//Provides: caml_unix_filedescr_of_fd const
//Alias: win_handle_fd
export function caml_unix_filedescr_of_fd(x) {
  return x;
}

//Provides: caml_unix_isatty
//Alias: unix_isatty
export function caml_unix_isatty(fd) {
  var file = caml_unix_lookup_file(fd);
  if (!file.isatty) return 0;
  return file.isatty();
}

//Provides: caml_unix_isatty
//Alias: unix_isatty
//If: browser
export function caml_unix_isatty(_fileDescriptor) {
  return 0;
}

//Provides: unix_error
export var unix_error = [
  /* ===Unix.error===
   *
   * This array is in order of the variant in OCaml
   */
  "E2BIG",
  "EACCES",
  "EAGAIN",
  "EBADF",
  "EBUSY",
  "ECHILD",
  "EDEADLK",
  "EDOM",
  "EEXIST",
  "EFAULT",
  "EFBIG",
  "EINTR",
  "EINVAL",
  "EIO",
  "EISDIR",
  "EMFILE",
  "EMLINK",
  "ENAMETOOLONG",
  "ENFILE",
  "ENODEV",
  "ENOENT",
  "ENOEXEC",
  "ENOLCK",
  "ENOMEM",
  "ENOSPC",
  "ENOSYS",
  "ENOTDIR",
  "ENOTEMPTY",
  "ENOTTY",
  "ENXIO",
  "EPERM",
  "EPIPE",
  "ERANGE",
  "EROFS",
  "ESPIPE",
  "ESRCH",
  "EXDEV",
  "EWOULDBLOCK",
  "EINPROGRESS",
  "EALREADY",
  "ENOTSOCK",
  "EDESTADDRREQ",
  "EMSGSIZE",
  "EPROTOTYPE",
  "ENOPROTOOPT",
  "EPROTONOSUPPORT",
  "ESOCKTNOSUPPORT",
  "EOPNOTSUPP",
  "EPFNOSUPPORT",
  "EAFNOSUPPORT",
  "EADDRINUSE",
  "EADDRNOTAVAIL",
  "ENETDOWN",
  "ENETUNREACH",
  "ENETRESET",
  "ECONNABORTED",
  "ECONNRESET",
  "ENOBUFS",
  "EISCONN",
  "ENOTCONN",
  "ESHUTDOWN",
  "ETOOMANYREFS",
  "ETIMEDOUT",
  "ECONNREFUSED",
  "EHOSTDOWN",
  "EHOSTUNREACH",
  "ELOOP",
  "EOVERFLOW",
];

//Provides: make_unix_err_args
export function make_unix_err_args(code, syscall, path, errno) {
  var variant = unix_error.indexOf(code);
  if (variant < 0) {
    // Default if undefined
    if (errno == null) {
      errno = -9999;
    }
    // If none of the above variants, fallback to EUNKNOWNERR(int)
    // errno is expected to be positive
    variant = BLOCK(0, -errno);
  }
  var args = [
    variant,
    caml_string_of_jsstring(syscall || ""),
    caml_string_of_jsstring(path || ""),
  ];
  return args;
}

//Provides: caml_strerror
export function caml_strerror(errno) {
  const util = require("node:util");
  if (errno >= 0) {
    const code = unix_error[errno];
    return util
      .getSystemErrorMap()
      .entries()
      .find((x) => x[1][0] === code)[1][1];
  } else {
    return util.getSystemErrorMessage(errno);
  }
}

//Provides: caml_strerror
//If: browser
export function caml_strerror(errno) {
  const code = unix_error[errno];
  return code || "Unknown error " + errno;
}

//Provides: unix_error_message
//Alias: caml_unix_error_message
export function unix_error_message(err) {
  const errno = typeof err === "number" ? err : -err[1];
  return caml_string_of_jsstring(caml_strerror(errno));
}

//Provides: caml_unix_chdir
//Alias: unix_chdir
export function caml_unix_chdir(dir) {
  return caml_sys_chdir(dir, /* raise Unix_error */ true);
}

//Provides: caml_unix_stat
//Alias: unix_stat
export function caml_unix_stat(name) {
  var root = resolve_fs_device(name);
  if (!root.device.stat) {
    caml_failwith("caml_unix_stat: not implemented");
  }
  return root.device.stat(
    root.rest,
    /* large */ false,
    /* raise Unix_error */ true,
  );
}

//Provides: caml_unix_stat_64
//Alias: unix_stat_64
export function caml_unix_stat_64(name) {
  var root = resolve_fs_device(name);
  if (!root.device.stat) {
    caml_failwith("caml_unix_stat_64: not implemented");
  }
  return root.device.stat(
    root.rest,
    /* large */ true,
    /* raise Unix_error */ true,
  );
}

//Provides: caml_unix_lstat
//Alias: unix_lstat
export function caml_unix_lstat(name) {
  var root = resolve_fs_device(name);
  if (!root.device.lstat) {
    caml_failwith("caml_unix_lstat: not implemented");
  }
  return root.device.lstat(
    root.rest,
    /* large */ false,
    /* raise Unix_error */ true,
  );
}

//Provides: caml_unix_lstat_64
//Alias: unix_lstat_64
export function caml_unix_lstat_64(name) {
  var root = resolve_fs_device(name);
  if (!root.device.lstat) {
    caml_failwith("caml_unix_lstat_64: not implemented");
  }
  return root.device.lstat(
    root.rest,
    /* large */ true,
    /* raise Unix_error */ true,
  );
}

//Provides: caml_unix_chmod
//Alias: unix_chmod
export function caml_unix_chmod(name, perms) {
  var root = resolve_fs_device(name);
  if (!root.device.chmod) {
    caml_failwith("caml_unix_chmod: not implemented");
  }
  return root.device.chmod(root.rest, perms);
}

//Provides: caml_unix_rename
//Alias: unix_rename
export function caml_unix_rename(o, n) {
  var o_root = resolve_fs_device(o);
  var n_root = resolve_fs_device(n);
  if (o_root.device !== n_root.device)
    caml_raise_system_error(/* raise Unix_error */ 1, "EXDEV", "rename");
  if (!o_root.device.rename) caml_failwith("caml_sys_rename: not implemented");
  o_root.device.rename(o_root.rest, n_root.rest, /* raise Unix_error */ true);
}

//Provides: caml_unix_mkdir
//Alias: unix_mkdir
export function caml_unix_mkdir(name, perm) {
  var root = resolve_fs_device(name);
  if (!root.device.mkdir) {
    caml_failwith("caml_unix_mkdir: not implemented");
  }
  return root.device.mkdir(root.rest, perm, /* raise Unix_error */ true);
}

//Provides: caml_unix_rmdir
//Alias: unix_rmdir
export function caml_unix_rmdir(name) {
  var root = resolve_fs_device(name);
  if (!root.device.rmdir) {
    caml_failwith("caml_unix_rmdir: not implemented");
  }
  return root.device.rmdir(root.rest, /* raise Unix_error */ true);
}

//Provides: caml_unix_link
//Alias: unix_link
export function caml_unix_link(follow, src, dst) {
  var src_root = resolve_fs_device(src);
  var dst_root = resolve_fs_device(dst);
  if (!src_root.device.link) {
    caml_failwith("caml_unix_link: not implemented");
  }
  // We can't control whether a 'src' symlink is followed or not.
  // So we fail when 'follow' is set, as documented in the Unix module.
  if (typeof follow !== "number")
    caml_raise_system_error(/* raise Unix_error */ 1, "ENOSYS", "link");
  if (src_root.device !== dst_root.device)
    caml_raise_system_error(/* raise Unix_error */ 1, "EXDEV", "link");
  return src_root.device.link(
    src_root.rest,
    dst_root.rest,
    /* raise Unix_error */ true,
  );
}

//Provides: caml_unix_symlink
//Alias: unix_symlink
export function caml_unix_symlink(to_dir, src, dst) {
  var dst_root = resolve_fs_device(dst);
  if (!dst_root.device.symlink) {
    caml_failwith("caml_unix_symlink: not implemented");
  }
  return dst_root.device.symlink(
    to_dir,
    caml_jsstring_of_string(src),
    dst_root.rest,
    /* raise Unix_error */ true,
  );
}

//Provides: caml_unix_readlink
//Alias: unix_readlink
export function caml_unix_readlink(name) {
  var root = resolve_fs_device(name);
  if (!root.device.readlink) {
    caml_failwith("caml_unix_readlink: not implemented");
  }
  return root.device.readlink(root.rest, /* raise Unix_error */ true);
}

//Provides: caml_unix_unlink
//Alias: unix_unlink
export function caml_unix_unlink(name) {
  var root = resolve_fs_device(name);
  if (!root.device.unlink) {
    caml_failwith("caml_unix_unlink: not implemented");
  }
  root.device.unlink(root.rest, /* raise Unix_error */ true);
  return 0;
}

//Provides: caml_unix_utimes
//Alias: unix_utimes
export function caml_unix_utimes(name, atime, mtime) {
  var root = resolve_fs_device(name);
  if (!root.device.utimes) {
    caml_failwith("caml_unix_utimes: not implemented");
  }
  root.device.utimes(root.rest, atime, mtime, /* raise Unix_error */ true);
  return 0;
}

//Provides: caml_unix_truncate
//Alias: unix_truncate
export function caml_unix_truncate(name, len) {
  var root = resolve_fs_device(name);
  if (!root.device.truncate) {
    caml_failwith("caml_unix_truncate: not implemented");
  }
  root.device.truncate(root.rest, len, /* raise Unix_error */ true);
  return 0;
}

//Provides: caml_unix_truncate_64
//Alias: unix_truncate_64
export function caml_unix_truncate_64(name, len) {
  var root = resolve_fs_device(name);
  if (!root.device.truncate) {
    caml_failwith("caml_unix_truncate_64: not implemented");
  }
  root.device.truncate(
    root.rest,
    caml_int64_to_float(len),
    /* raise Unix_error */ true,
  );
  return 0;
}

//Provides: caml_unix_access
//Alias: unix_access
export function caml_unix_access(name, flags) {
  var f = {};
  while (flags) {
    switch (flags[1]) {
      case 0:
        f.r = 1;
        break;
      case 1:
        f.w = 1;
        break;
      case 2:
        f.x = 1;
        break;
      case 3:
        f.f = 1;
        break;
    }
    flags = flags[2];
  }
  var root = resolve_fs_device(name);
  if (!root.device.access) {
    caml_failwith("caml_unix_access: not implemented");
  }
  root.device.access(root.rest, f, /* raise Unix_error */ true);
  return 0;
}

//Provides: caml_unix_open
//Alias: unix_open
export function caml_unix_open(name, flags, perms) {
  var f = {};
  while (flags) {
    switch (flags[1]) {
      case 0:
        f.rdonly = 1;
        break;
      case 1:
        f.wronly = 1;
        break;
      case 2:
        f.rdwr = 1;
        break;
      case 3:
        f.nonblock = 1;
        break;
      case 4:
        f.append = 1;
        break;
      case 5:
        f.create = 1;
        break;
      case 6:
        f.truncate = 1;
        break;
      case 7:
        f.excl = 1;
        break;
      case 8:
        f.noctty = 1;
        break;
      case 9:
        f.dsync = 1;
        break;
      case 10:
        f.sync = 1;
        break;
    }
    flags = flags[2];
  }
  var root = resolve_fs_device(name);
  var file = root.device.open(root.rest, f, perms, /* raise Unix_error */ true);
  var idx = caml_sys_fds.length;
  var chanid = new MlChanid(idx);
  caml_sys_fds[idx] = { file: file, chanid: chanid };
  return idx | 0;
}

//Provides: caml_unix_lookup_file
export function caml_unix_lookup_file(fd, cmd) {
  var fd_desc = caml_sys_fds[fd];
  if (fd_desc === undefined)
    caml_raise_system_error(/* raise Unix_error */ 1, "EBADF", cmd);
  return fd_desc.file;
}

//Provides: caml_unix_fstat
//Alias: unix_fstat
export function caml_unix_fstat(fd) {
  var file = caml_unix_lookup_file(fd, "fstat");
  if (!file.stat) {
    caml_failwith("caml_unix_fstat: not implemented");
  }
  return file.stat(/* large */ false);
}

//Provides: caml_unix_fstat_64
//Alias: unix_fstat_64
export function caml_unix_fstat_64(fd) {
  var file = caml_unix_lookup_file(fd, "fstat");
  if (!file.stat) {
    caml_failwith("caml_unix_fstat64: not implemented");
  }
  return file.stat(/* large */ true);
}

//Provides: caml_unix_fchmod
//Alias: unix_fchmod
export function caml_unix_fchmod(fd, perms) {
  var file = caml_unix_lookup_file(fd, "fchmod");
  if (!file.chmod) {
    caml_failwith("caml_unix_fchmod: not implemented");
  }
  return file.chmod(perms);
}

//Provides: caml_unix_fsync
//Alias: unix_fsync
export function caml_unix_fsync(fd) {
  var file = caml_unix_lookup_file(fd, "fsync");
  if (!file.sync) {
    caml_failwith("caml_unix_fsync: not implemented");
  }
  return file.sync();
}

//Provides: caml_unix_write
//Alias: unix_write
export function caml_unix_write(fd, buf, pos, len) {
  var file = caml_unix_lookup_file(fd, "write");
  var a = caml_uint8_array_of_bytes(buf);
  var written = 0;
  while (len > 0) {
    var n = file.write(a, pos, len, /* raise unix_error */ 1);
    written += n;
    pos += n;
    len -= n;
  }
  return written;
}

//Provides: caml_unix_single_write
//Alias: unix_single_write
export function caml_unix_single_write(fd, buf, pos, len) {
  var file = caml_unix_lookup_file(fd, "write");
  if (len === 0) return 0;
  return file.write(
    caml_uint8_array_of_bytes(buf),
    pos,
    len,
    /* raise unix_error */ 1,
  );
}

//Provides: caml_unix_write_bigarray
//Alias: caml_unix_lookup_file
//Version: >= 5.2
export function caml_unix_write_bigarray(fd, buf, pos, len) {
  var a = caml_ba_to_typed_array(buf);
  var file = caml_unix_lookup_file(fd, "write");
  var written = 0;
  while (len > 0) {
    var n = file.write(a, pos, len, /* raise unix_error */ 1);
    written += n;
    pos += n;
    len -= n;
  }
  return written;
}

//Provides: caml_unix_read
//Alias: unix_read
export function caml_unix_read(fd, buf, pos, len) {
  var file = caml_unix_lookup_file(fd, "read");
  return file.read(
    caml_uint8_array_of_bytes(buf),
    pos,
    len,
    /* raise unix_error */ 1,
  );
}

//Provides: caml_unix_read_bigarray
//Alias: unix_read_bigarray
//Version: >= 5.2
export function caml_unix_read_bigarray(fd, buf, pos, len) {
  var a = caml_ba_to_typed_array(buf);
  var file = caml_unix_lookup_file(fd, "read");
  return file.read(a, pos, len, /* raise unix_error */ 1);
}

//Provides: caml_unix_lseek
//Alias: unix_lseek
export function caml_unix_lseek(fd, len, whence) {
  var file = caml_unix_lookup_file(fd, "lseek");
  return file.seek(len, whence, /* raise unix_error */ 1);
}

//Provides: caml_unix_lseek_64
//Alias: unix_lseek_64
export function caml_unix_lseek_64(fd, len, whence) {
  var file = caml_unix_lookup_file(fd, "lseek");
  return file.seek(caml_int64_to_float(len), whence, /* raise unix_error */ 1);
}

//Provides: caml_unix_ftruncate
//Alias: unix_ftruncate
export function caml_unix_ftruncate(fd, len) {
  var file = caml_unix_lookup_file(fd, "ftruncate");
  if (!file.truncate) {
    caml_failwith("caml_unix_ftruncate: not implemented");
  }
  file.truncate(len, /* raise unix_error */ 1);
  return 0;
}

//Provides: caml_unix_ftruncate_64
//Alias: unix_ftruncate_64
export function caml_unix_ftruncate_64(fd, len) {
  var file = caml_unix_lookup_file(fd, "ftruncate");
  if (!file.truncate) {
    caml_failwith("caml_unix_ftruncate_64: not implemented");
  }
  file.truncate(caml_int64_to_float(len), /* raise unix_error */ 1);
  return 0;
}

//Provides: caml_unix_close
//Alias: unix_close
export function caml_unix_close(fd) {
  var file = caml_unix_lookup_file(fd, "close");
  file.close(/* raise unix_error */ 1);
  return 0;
}

//Provides: caml_unix_inchannel_of_filedescr
//Alias: unix_inchannel_of_filedescr
//Alias: win_inchannel_of_filedescr
export function caml_unix_inchannel_of_filedescr(fd) {
  var file = caml_unix_lookup_file(fd, "in_channel_of_descr");
  file.check_stream_semantics("in_channel_of_descr");
  return caml_ml_open_descriptor_in(fd);
}

//Provides: caml_unix_outchannel_of_filedescr
//Alias: unix_outchannel_of_filedescr
//Alias: win_outchannel_of_filedescr
export function caml_unix_outchannel_of_filedescr(fd) {
  var file = caml_unix_lookup_file(fd, "out_channel_of_descr");
  file.check_stream_semantics("out_channel_of_descr");
  return caml_ml_open_descriptor_out(fd);
}

//Provides: caml_unix_getuid
//Alias: unix_getuid
export function caml_unix_getuid(_unit) {
  if (globalThis.process?.getuid) {
    return globalThis.process.getuid();
  }
  return 1;
}

//Provides: caml_unix_geteuid
//Alias: unix_geteuid
export function caml_unix_geteuid(_unit) {
  if (globalThis.process?.geteuid) {
    return globalThis.process.geteuid();
  }
  return 1;
}

//Provides: caml_unix_getgid
//Alias: unix_getgid
export function caml_unix_getgid(_unit) {
  if (globalThis.process?.getgid) {
    return globalThis.process.getgid();
  }
  return 1;
}

//Provides: caml_unix_getegid
//Alias: unix_getegid
export function caml_unix_getegid(_unit) {
  if (globalThis.process?.getegid) {
    return globalThis.process.getegid();
  }
  return 1;
}

//Provides: caml_unix_getpwnam
//Alias: unix_getpwnam
//Alias: caml_unix_getpwuid
//Alias: unix_getpwuid
//Alias: caml_unix_getgrnam
//Alias: unix_getgrnam
//Alias: caml_unix_getgrgid
//Alias: unix_getgrgid
export function caml_unix_getpwnam(_unit) {
  caml_raise_not_found();
}

//Provides: caml_unix_has_symlink
//Alias: unix_has_symlink
export function caml_unix_has_symlink(_unit) {
  return fs_node_supported() ? 1 : 0;
}

//Provides: caml_unix_opendir
//Alias: unix_opendir
export function caml_unix_opendir(path) {
  var root = resolve_fs_device(path);
  if (!root.device.opendir) {
    caml_failwith("caml_unix_opendir: not implemented");
  }
  var dir_handle = root.device.opendir(root.rest, /* raise Unix_error */ true);
  return { pointer: dir_handle, path: path };
}

//Provides: caml_unix_readdir
//Alias: unix_readdir
export function caml_unix_readdir(dir_handle) {
  var entry;
  try {
    entry = dir_handle.pointer.readSync();
  } catch (e) {
    caml_raise_system_error(/* raise Unix_error */ 1, "EBADF", "readdir");
  }
  if (entry === null) {
    caml_raise_end_of_file();
  } else {
    return caml_string_of_jsstring(entry.name);
  }
}

//Provides: caml_unix_closedir
//Alias: unix_closedir
export function caml_unix_closedir(dir_handle) {
  try {
    dir_handle.pointer.closeSync();
  } catch (e) {
    caml_raise_system_error(/* raise Unix_error */ 1, "EBADF", "closedir");
  }
}

//Provides: caml_unix_rewinddir
//Alias: unix_rewinddir
export function caml_unix_rewinddir(dir_handle) {
  caml_unix_closedir(dir_handle);
  var new_dir_handle = caml_unix_opendir(dir_handle.path);
  dir_handle.pointer = new_dir_handle.pointer;
  return 0;
}

//Provides: caml_unix_findfirst
//Alias: win_findfirst
export function caml_unix_findfirst(path) {
  // The Windows code adds this glob to the path, so we need to remove it
  var path_js = caml_jsstring_of_string(path);
  path_js = path_js.replace(/(^|[\\/])\*\.\*$/, "");
  path = caml_string_of_jsstring(path_js);
  // *.* is now stripped
  var dir_handle = caml_unix_opendir(path);
  var first_entry = caml_unix_readdir(dir_handle);
  // The Windows bindings type dir_handle as an `int` but it's not in JS
  return [0, first_entry, dir_handle];
}

//Provides: caml_unix_findnext
//Alias: win_findnext
export function caml_unix_findnext(dir_handle) {
  return caml_unix_readdir(dir_handle);
}

//Provides: caml_unix_findclose
//Alias: win_findclose
export function caml_unix_findclose(dir_handle) {
  return caml_unix_closedir(dir_handle);
}

//Provides: caml_unix_inet_addr_of_string const
//Alias: unix_inet_addr_of_string
export function caml_unix_inet_addr_of_string() {
  return 0;
}

//Provides: caml_raise_system_error
export function caml_raise_system_error(raise_unix, code, cmd, msg, path) {
  var unix_error = caml_named_value("Unix.Unix_error");
  if (raise_unix && unix_error)
    caml_raise_with_args(unix_error, make_unix_err_args(code, cmd, path));
  else {
    var msg = code + ": " + msg + ", " + cmd;
    if (path !== undefined) msg += " '" + path + "'";
    caml_raise_sys_error(msg);
  }
}

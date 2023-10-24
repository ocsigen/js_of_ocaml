//Provides: caml_unix_gettimeofday
//Alias: unix_gettimeofday
function caml_unix_gettimeofday () {
  return (new Date()).getTime() / 1000;
}

//Provides: caml_unix_time
//Requires: caml_unix_gettimeofday
//Alias: unix_time
function caml_unix_time () {
  return Math.floor(caml_unix_gettimeofday ());
}

//Provides: caml_unix_gmtime
//Alias: unix_gmtime
function caml_unix_gmtime (t) {
  var d = new Date (t * 1000);
  var d_num = d.getTime();
  var januaryfirst = (new Date(Date.UTC(d.getUTCFullYear(), 0, 1))).getTime();
  var doy = Math.floor((d_num - januaryfirst) / 86400000);
  return BLOCK(0, d.getUTCSeconds(), d.getUTCMinutes(), d.getUTCHours(),
               d.getUTCDate(), d.getUTCMonth(), d.getUTCFullYear() - 1900,
               d.getUTCDay(), doy,
               false | 0 /* for UTC daylight savings time is false */)
}

//Provides: caml_unix_localtime
//Alias: unix_localtime
function caml_unix_localtime (t) {
  var d = new Date (t * 1000);
  var d_num = d.getTime();
  var januaryfirst = (new Date(d.getFullYear(), 0, 1)).getTime();
  var doy = Math.floor((d_num - januaryfirst) / 86400000);
  var jan = new Date(d.getFullYear(), 0, 1);
  var jul = new Date(d.getFullYear(), 6, 1);
  var stdTimezoneOffset = Math.max(jan.getTimezoneOffset(), jul.getTimezoneOffset());
  return BLOCK(0, d.getSeconds(), d.getMinutes(), d.getHours(),
               d.getDate(), d.getMonth(), d.getFullYear() - 1900,
               d.getDay(), doy,
               (d.getTimezoneOffset() < stdTimezoneOffset) | 0 /* daylight savings time  field. */)
}

//Provides: caml_unix_mktime
//Requires: caml_unix_localtime
//Alias: unix_mktime
function caml_unix_mktime(tm){
  var d = (new Date(tm[6]+1900,tm[5],tm[4],tm[3],tm[2],tm[1])).getTime();
  var t = Math.floor(d / 1000);
  var tm2 = caml_unix_localtime(t);
  return BLOCK(0,t,tm2);
}
//Provides: caml_unix_startup const
//Alias: win_startup
function caml_unix_startup() {}

//Provides: caml_unix_cleanup const
//Alias: win_cleanup
function caml_unix_cleanup() {}

//Provides: caml_unix_filedescr_of_fd const
//Alias: win_handle_fd
function caml_unix_filedescr_of_fd(x) {return x;}

//Provides: caml_unix_isatty
//Requires: fs_node_supported
//Alias: unix_isatty
function caml_unix_isatty(fileDescriptor) {
  if(fs_node_supported()) {
    var tty = require('tty');
    return tty.isatty(fileDescriptor)?1:0;
  } else {
    return 0;
  }
}


//Provides: caml_unix_isatty
//Alias: unix_isatty
//If: browser
function caml_unix_isatty(fileDescriptor) {
  return 0;
}

//Provides: make_unix_err_args
//Requires: caml_string_of_jsstring
var unix_error = [
  /* ===Unix.error===
   *
   * This array is in order of the variant in OCaml
   */
  "E2BIG", "EACCES", "EAGAIN", "EBADF", "EBUSY", "ECHILD", "EDEADLK", "EDOM",
  "EEXIST", "EFAULT", "EFBIG", "EINTR", "EINVAL", "EIO", "EISDIR", "EMFILE",
  "EMLINK", "ENAMETOOLONG", "ENFILE", "ENODEV", "ENOENT", "ENOEXEC", "ENOLCK",
  "ENOMEM", "ENOSPC", "ENOSYS", "ENOTDIR", "ENOTEMPTY", "ENOTTY", "ENXIO",
  "EPERM", "EPIPE", "ERANGE", "EROFS", "ESPIPE", "ESRCH", "EXDEV", "EWOULDBLOCK",
  "EINPROGRESS", "EALREADY", "ENOTSOCK", "EDESTADDRREQ", "EMSGSIZE",
  "EPROTOTYPE", "ENOPROTOOPT", "EPROTONOSUPPORT", "ESOCKTNOSUPPORT",
  "EOPNOTSUPP", "EPFNOSUPPORT", "EAFNOSUPPORT", "EADDRINUSE", "EADDRNOTAVAIL",
  "ENETDOWN", "ENETUNREACH", "ENETRESET", "ECONNABORTED", "ECONNRESET", "ENOBUFS",
  "EISCONN", "ENOTCONN", "ESHUTDOWN", "ETOOMANYREFS", "ETIMEDOUT", "ECONNREFUSED",
  "EHOSTDOWN", "EHOSTUNREACH", "ELOOP", "EOVERFLOW"
];
function make_unix_err_args(code, syscall, path, errno) {
  var variant = unix_error.indexOf(code);
  if (variant < 0) {
    // Default if undefined
    if (errno == null) {
      errno = -9999
    }
    // If none of the above variants, fallback to EUNKNOWNERR(int)
    variant = BLOCK(0, errno);
  }
  var args = [
    variant,
    caml_string_of_jsstring(syscall || ""),
    caml_string_of_jsstring(path || "")
  ];
  return args;
}

//Provides: caml_unix_stat
//Requires: resolve_fs_device, caml_failwith
//Alias: unix_stat
function caml_unix_stat(name) {
  var root = resolve_fs_device(name);
  if (!root.device.stat) {
    caml_failwith("caml_unix_stat: not implemented");
  }
  return root.device.stat(root.rest, /* raise Unix_error */ true);
}

//Provides: caml_unix_stat_64
//Requires: caml_unix_stat, caml_int64_of_int32
//Alias: unix_stat_64
function caml_unix_stat_64(name) {
  var r = caml_unix_stat(name);
  r[9] = caml_int64_of_int32(r[9]);
  return r;
}

//Provides: caml_unix_lstat
//Requires: resolve_fs_device, caml_failwith
//Alias: unix_lstat
function caml_unix_lstat(name) {
  var root = resolve_fs_device(name);
  if (!root.device.lstat) {
    caml_failwith("caml_unix_lstat: not implemented");
  }
  return root.device.lstat(root.rest, /* raise Unix_error */ true);
}

//Provides: caml_unix_lstat_64
//Requires: caml_unix_lstat, caml_int64_of_int32
//Alias: unix_lstat_64
function caml_unix_lstat_64(name) {
  var r = caml_unix_lstat(name);
  r[9] = caml_int64_of_int32(r[9]);
  return r;
}

//Provides: caml_unix_mkdir
//Requires: resolve_fs_device, caml_failwith
//Alias: unix_mkdir
function caml_unix_mkdir(name, perm) {
  var root = resolve_fs_device(name);
  if (!root.device.mkdir) {
    caml_failwith("caml_unix_mkdir: not implemented");
  }
  return root.device.mkdir(root.rest, perm, /* raise Unix_error */ true);
}

//Provides: caml_unix_rmdir
//Requires: resolve_fs_device, caml_failwith
//Alias: unix_rmdir
function caml_unix_rmdir(name) {
  var root = resolve_fs_device(name);
  if (!root.device.rmdir) {
    caml_failwith("caml_unix_rmdir: not implemented");
  }
  return root.device.rmdir(root.rest, /* raise Unix_error */ true);
}

//Provides: caml_unix_symlink
//Requires: resolve_fs_device, caml_failwith
//Alias: unix_symlink
function caml_unix_symlink(to_dir, src, dst) {
  var src_root = resolve_fs_device(src);
  var dst_root = resolve_fs_device(dst);
  if(src_root.device != dst_root.device)
    caml_failwith("caml_unix_symlink: cannot symlink between two filesystems");
  if (!src_root.device.symlink) {
    caml_failwith("caml_unix_symlink: not implemented");
  }
  return src_root.device.symlink(to_dir, src_root.rest, dst_root.rest, /* raise Unix_error */ true);
}

//Provides: caml_unix_readlink
//Requires: resolve_fs_device, caml_failwith
//Alias: unix_readlink
function caml_unix_readlink(name) {
  var root = resolve_fs_device(name);
  if (!root.device.readlink) {
    caml_failwith("caml_unix_readlink: not implemented");
  }
  return root.device.readlink(root.rest, /* raise Unix_error */ true);
}

//Provides: caml_unix_unlink
//Requires: resolve_fs_device, caml_failwith
//Alias: unix_unlink
function caml_unix_unlink(name) {
  var root = resolve_fs_device(name);
  if (!root.device.unlink) {
    caml_failwith("caml_unix_unlink: not implemented");
  }
  return root.device.unlink(root.rest, /* raise Unix_error */ true);
}

//Provides: caml_unix_getuid
//Requires: caml_raise_not_found
//Alias: unix_getuid
function caml_unix_getuid(unit) {
  if(globalThis.process && globalThis.process.getuid){
    return globalThis.process.getuid();
  }
  caml_raise_not_found();
}

//Provides: caml_unix_getpwuid
//Requires: caml_raise_not_found
//Alias: unix_getpwuid
function caml_unix_getpwuid(unit) {
  caml_raise_not_found();
}

//Provides: caml_unix_has_symlink
//Requires: fs_node_supported
//Alias: unix_has_symlink
function caml_unix_has_symlink(unit) {
  return fs_node_supported()?1:0
}

//Provides: caml_unix_opendir
//Requires: resolve_fs_device, caml_failwith
//Alias: unix_opendir
function caml_unix_opendir(path) {
  var root = resolve_fs_device(path);
  if (!root.device.opendir) {
    caml_failwith("caml_unix_opendir: not implemented");
  }
  var dir_handle = root.device.opendir(root.rest, /* raise Unix_error */ true);
  return { pointer : dir_handle, path: path }
}

//Provides: caml_unix_readdir
//Requires: caml_raise_end_of_file
//Requires: caml_string_of_jsstring
//Requires: make_unix_err_args, caml_raise_with_args, caml_named_value
//Alias: unix_readdir
function caml_unix_readdir(dir_handle) {
  var entry;
  try {
      entry = dir_handle.pointer.readSync();
  } catch (e) {
      var unix_error = caml_named_value('Unix.Unix_error');
      caml_raise_with_args(unix_error, make_unix_err_args("EBADF", "readdir", dir_handle.path));
  }
  if (entry === null) {
      caml_raise_end_of_file();
  } else {
      return caml_string_of_jsstring(entry.name);
  }
}

//Provides: caml_unix_closedir
//Requires: make_unix_err_args, caml_raise_with_args, caml_named_value
//Alias: unix_closedir
function caml_unix_closedir(dir_handle) {
  try {
      dir_handle.pointer.closeSync();
  } catch (e) {
      var unix_error = caml_named_value('Unix.Unix_error');
      caml_raise_with_args(unix_error, make_unix_err_args("EBADF", "closedir", dir_handle.path));
  }
}

//Provides: caml_unix_rewinddir
//Requires: caml_unix_closedir, caml_unix_opendir
//Alias: unix_rewinddir
function caml_unix_rewinddir(dir_handle) {
  caml_unix_closedir(dir_handle);
  var new_dir_handle = caml_unix_opendir(dir_handle.path);
  dir_handle.pointer = new_dir_handle.pointer;
  return 0;
}

//Provides: caml_unix_findfirst
//Requires: caml_jsstring_of_string, caml_string_of_jsstring
//Requires: caml_unix_opendir, caml_unix_readdir
//Alias: win_findfirst
function caml_unix_findfirst(path) {
  // The Windows code adds this glob to the path, so we need to remove it
  var path_js = caml_jsstring_of_string(path);
  path_js = path_js.replace(/(^|[\\\/])\*\.\*$/, "");
  path = caml_string_of_jsstring(path_js);
  // *.* is now stripped
  var dir_handle = caml_unix_opendir(path);
  var first_entry = caml_unix_readdir(dir_handle);
  // The Windows bindings type dir_handle as an `int` but it's not in JS
  return [0, first_entry, dir_handle];
}

//Provides: caml_unix_findnext
//Requires: caml_unix_readdir
//Alias: win_findnext
function caml_unix_findnext(dir_handle) {
  return caml_unix_readdir(dir_handle);
}

//Provides: caml_unix_findclose
//Requires: caml_unix_closedir
//Alias: win_findclose
function caml_unix_findclose(dir_handle) {
  return caml_unix_closedir(dir_handle);
}


//Provides: caml_unix_inet_addr_of_string const
//Alias: unix_inet_addr_of_string
function caml_unix_inet_addr_of_string () {return 0;}



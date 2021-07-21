//Provides: unix_gettimeofday
function unix_gettimeofday () {
  return (new Date()).getTime() / 1000;
}

//Provides: unix_time
//Requires: unix_gettimeofday
function unix_time () {
  return Math.floor(unix_gettimeofday ());
}

//Provides: unix_gmtime
function unix_gmtime (t) {
  var d = new Date (t * 1000);
  var d_num = d.getTime();
  var januaryfirst = (new Date(Date.UTC(d.getUTCFullYear(), 0, 1))).getTime();
  var doy = Math.floor((d_num - januaryfirst) / 86400000);
  return BLOCK(0, d.getUTCSeconds(), d.getUTCMinutes(), d.getUTCHours(),
               d.getUTCDate(), d.getUTCMonth(), d.getUTCFullYear() - 1900,
               d.getUTCDay(), doy,
               false | 0 /* for UTC daylight savings time is false */)
}

//Provides: unix_localtime
function unix_localtime (t) {
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

//Provides: unix_mktime
//Requires: unix_localtime
function unix_mktime(tm){
  var d = (new Date(tm[6]+1900,tm[5],tm[4],tm[3],tm[2],tm[1])).getTime();
  var t = Math.floor(d / 1000);
  var tm2 = unix_localtime(t);
  return BLOCK(0,t,tm2);
}

//Provides: win_startup const
function win_startup() {}

//Provides: win_cleanup const
function win_cleanup() {}

//Provides: win_handle_fd const
function win_handle_fd(x) {return x;}

//Provides: unix_isatty
//Requires: fs_node_supported
function unix_isatty(fileDescriptor) {
  if(fs_node_supported()) {
    var tty = require('tty');
    return tty.isatty(fileDescriptor);
  } else {
    return false;
  }
}

//Provides: internal_raise_unix_error
//Requires: caml_named_value, caml_string_of_jsbytes
function internal_raise_unix_error(tag, name, param) {
  throw [
    0,
    caml_named_value('Unix.Unix_error'),
    tag,
    caml_string_of_jsbytes(name || ""),
    caml_string_of_jsbytes(param || "")
  ];
}

//Provides: internal_unix_error_from_js
function internal_unix_error_from_js(err) {
  /* ===Unix.error===
   *
   * This array is in order of the variant in OCaml
   */
  var errors = [
    'E2BIG', 'EACCES', 'EAGAIN', 'EBADF', 'EBUSY', 'ECHILD', 'EDEADLK', 'EDOM',
    'EEXIST', 'EFAULT', 'EFBIG', 'EINTR', 'EINVAL', 'EIO', 'EISDIR', 'EMFILE',
    'EMLINK', 'ENAMETOOLONG', 'ENFILE', 'ENODEV', 'ENOENT', 'ENOEXEC', 'ENOLCK',
    'ENOMEM', 'ENOSPC', 'ENOSYS', 'ENOTDIR', 'ENOTEMPTY', 'ENOTTY', 'ENXIO',
    'EPERM', 'EPIPE', 'ERANGE', 'EROFS', 'ESPIPE', 'ESRCH', 'EXDEV', 'EWOULDBLOCK',
    'EINPROGRESS', 'EALREADY', 'ENOTSOCK', 'EDESTADDRREQ', 'EMSGSIZE',
    'EPROTOTYPE', 'ENOPROTOOPT', 'EPROTONOSUPPORT', 'ESOCKTNOSUPPORT',
    'EOPNOTSUPP', 'EPFNOSUPPORT', 'EAFNOSUPPORT', 'EADDRINUSE', 'EADDRNOTAVAIL',
    'ENETDOWN', 'ENETUNREACH', 'ENETRESET', 'ECONNABORTED', 'ECONNRESET', 'ENOBUFS',
    'EISCONN', 'ENOTCONN', 'ESHUTDOWN', 'ETOOMANYREFS', 'ETIMEDOUT', 'ECONNREFUSED',
    'EHOSTDOWN', 'EHOSTUNREACH', 'ELOOP', 'EOVERFLOW'
  ];
  var variantId = errors.indexOf(err.code);
  if (variantId < 0) {
    // TODO: Make sure this works `EUNKNOWNERR(int)`
    return BLOCK(0, err.errno);
  } else {
    return variantId;
  }
}

//Provides: caml_stats_from_js
function caml_stats_from_js(js_stats) {
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
  }
  if (js_stats.isDirectory()) {
    file_kind = 1;
  }
  if (js_stats.isCharacterDevice()) {
    file_kind = 2;
  }
  if (js_stats.isBlockDevice()) {
    file_kind = 3;
  }
  if (js_stats.isSymbolicLink()) {
    file_kind = 4;
  }
  if (js_stats.isFIFO()) {
    file_kind = 5;
  }
  if (js_stats.isSocket()) {
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
    js_stats.ino,
    file_kind,
    js_stats.mode,
    js_stats.nlink,
    js_stats.uid,
    js_stats.gid,
    js_stats.rdev,
    js_stats.size,
    js_stats.atimeMs,
    js_stats.mtimeMs,
    js_stats.ctimeMs
  );
}

//Provides: unix_stat
//Requires: fs_node_supported, caml_stats_from_js
//Requires: caml_jsbytes_of_string
//Requires: internal_unix_error_from_js, internal_raise_unix_error
function unix_stat(filename) {
  if (fs_node_supported()) {
    var fs = require('fs');
    var js_filename = caml_jsbytes_of_string(filename);
    try {
      var js_stats = fs.statSync(js_filename);
      return caml_stats_from_js(js_stats);
    } catch (err) {
      internal_raise_unix_error(internal_unix_error_from_js(err), err.syscall, err.path);
    }
  } else {
    // TODO: What should happen if not in nodejs?
  }
}

//Provides: unix_lstat
//Requires: fs_node_supported, caml_stats_from_js
//Requires: caml_jsbytes_of_string
//Requires: internal_unix_error_from_js, internal_raise_unix_error
function unix_lstat(filename) {
  if (fs_node_supported()) {
    var fs = require('fs');
    var js_filename = caml_jsbytes_of_string(filename);
    try {
      var js_stats = fs.lstatSync(js_filename);
      return caml_stats_from_js(js_stats);
    } catch (err) {
      internal_raise_unix_error(internal_unix_error_from_js(err), err.syscall, err.path);
    }
  } else {
    // TODO: What should happen if not in nodejs?
  }
}

//Provides: unix_mkdir
//Requires: fs_node_supported, caml_jsbytes_of_string
//Requires: internal_unix_error_from_js, internal_raise_unix_error
function unix_mkdir(dirname, perm) {
  if (fs_node_supported()) {
    var fs = require('fs');
    var js_dirname = caml_jsbytes_of_string(dirname);
    if (!js_dirname.endsWith('/')) {
      js_dirname = js_dirname + '/';
    }
    try {
      fs.mkdirSync(js_dirname, perm);
    } catch (err) {
      internal_raise_unix_error(internal_unix_error_from_js(err), err.syscall, err.path);
    }
  } else {
    // TODO: What should happen if not in nodejs?
  }
}

//Provides: unix_readlink
//Requires: fs_node_supported, caml_jsbytes_of_string, caml_string_of_jsbytes
//Requires: internal_unix_error_from_js, internal_raise_unix_error
function unix_readlink(filename) {
  if (fs_node_supported()) {
    var fs = require('fs');
    var js_filename = caml_jsbytes_of_string(filename);
    try {
      var js_string = fs.readlinkSync(js_filename, 'utf8');
      return caml_string_of_jsbytes(js_string);
    } catch (err) {
      internal_raise_unix_error(internal_unix_error_from_js(err), err.syscall, err.path);
    }
  } else {
    // TODO: What should happen if not in nodejs?
  }
}

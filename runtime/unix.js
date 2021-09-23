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

//Provides: unix_stat
//Requires: resolve_fs_device, caml_failwith
function unix_stat(name) {
  var root = resolve_fs_device(name);
  if (!root.device.stat) {
    caml_failwith("unix_stat: not implemented");
  }
  return root.device.stat(root.rest);
}

//Provides: unix_lstat
//Requires: resolve_fs_device, caml_failwith
function unix_lstat(name) {
  var root = resolve_fs_device(name);
  if (!root.device.lstat) {
    caml_failwith("unix_lstat: not implemented");
  }
  return root.device.lstat(root.rest);
}

//Provides: unix_mkdir
//Requires: resolve_fs_device, caml_failwith
function unix_mkdir(name, perm) {
  var root = resolve_fs_device(name);
  if (!root.device.mkdir) {
    caml_failwith("unix_mkdir: not implemented");
  }
  return root.device.mkdir(root.rest, perm);
}

//Provides: unix_rmdir
//Requires: resolve_fs_device, caml_failwith
function unix_rmdir(name) {
  var root = resolve_fs_device(name);
  if (!root.device.rmdir) {
    caml_failwith("unix_rmdir: not implemented");
  }
  return root.device.rmdir(root.rest);
}

//Provides: unix_symlink
//Requires: resolve_fs_device, caml_failwith
function unix_symlink(to_dir, src, dst) {
  var src_root = resolve_fs_device(src);
  var dst_root = resolve_fs_device(dst);
  if (!src_root.device.symlink) {
    caml_failwith("unix_symlink: not implemented");
  }
  // TODO: Is dst_root.rest correct here?
  return src_root.device.symlink(to_dir, src_root.rest, dst_root.rest);
}

//Provides: unix_readlink
//Requires: resolve_fs_device, caml_failwith
function unix_readlink(name) {
  var root = resolve_fs_device(name);
  if (!root.device.readlink) {
    caml_failwith("unix_readlink: not implemented");
  }
  return root.device.readlink(root.rest);
}

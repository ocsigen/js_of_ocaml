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

///////////// Dummy filesystem

import { caml_failwith } from './fail.js';
import { MlFakeDevice } from './fs_fake.js';
import { MlNodeDevice, fs_node_supported, jsoo_is_win32 } from './fs_node.js';
import { caml_jsstring_of_string, caml_string_of_jsbytes, caml_string_of_jsstring, caml_string_of_uint8_array } from './mlBytes.js';
import { caml_raise_sys_error } from './sys.js';
import { caml_raise_system_error } from './unix.js';

//Provides: caml_trailing_slash
export function caml_trailing_slash(name) {
  return name.slice(-1) !== "/" ? name + "/" : name;
}

//Provides: caml_current_dir
if (fs_node_supported() && globalThis.process && globalThis.process.cwd)
  var caml_current_dir = globalThis.process.cwd().replace(/\\/g, "/");
else var caml_current_dir = "/static";
caml_current_dir = caml_trailing_slash(caml_current_dir);

//Provides: caml_get_root
export function caml_get_root(path) {
  var x = path_is_absolute(path);
  if (!x) return;
  return x[0] + "/";
}

//Provides: caml_root
export let caml_root =
  caml_get_root(caml_current_dir) ||
  caml_failwith("unable to compute caml_root");

//Provides: MlFile
export function MlFile() {}

//Provides: path_is_absolute
function make_path_is_absolute() {
  function posix(path) {
    if (path.charAt(0) === "/") return ["", path.slice(1)];
    return;
  }

  function win32(path) {
    // https://github.com/nodejs/node/blob/b3fcc245fb25539909ef1d5eaa01dbf92e168633/lib/path.js#L56
    var splitDeviceRe =
      /^([a-zA-Z]:|[\\/]{2}[^\\/]+[\\/]+[^\\/]+)?([\\/])?([\s\S]*?)$/;
    var result = splitDeviceRe.exec(path);
    var device = result[1] || "";
    var isUnc = device.length > 0 && device.charAt(1) !== ":";

    // UNC paths are always absolute
    if (result[2] || isUnc) {
      var root = result[1] || "";
      var sep = result[2] || "";
      return [root, path.slice(root.length + sep.length)];
    }
    return;
  }
  return jsoo_is_win32 ? win32 : posix;
}
export let path_is_absolute = make_path_is_absolute();

//Provides: caml_make_path
export function caml_make_path(name) {
  name = caml_jsstring_of_string(name);
  if (!path_is_absolute(name)) name = caml_current_dir + name;
  var comp0 = path_is_absolute(name);
  var comp = comp0[1].split(/[/\\]/);
  var ncomp = [];
  for (var i = 0; i < comp.length; i++) {
    switch (comp[i]) {
      case "..":
        ncomp.pop();
        break;
      case ".":
        break;
      case "":
        break;
      default:
        ncomp.push(comp[i]);
        break;
    }
  }
  ncomp.unshift(comp0[0]);
  ncomp.orig = name;
  return ncomp;
}

//Provides:jsoo_mount_point
export let jsoo_mount_point = [];
if (fs_node_supported()) {
  jsoo_mount_point.push({
    path: caml_root,
    device: new MlNodeDevice(caml_root),
  });
} else {
  jsoo_mount_point.push({
    path: caml_root,
    device: new MlFakeDevice(caml_root),
  });
}
jsoo_mount_point.push({
  path: "/static/",
  device: new MlFakeDevice("/static/"),
});

//Provides:caml_list_mount_point
export function caml_list_mount_point() {
  var prev = 0;
  for (var i = 0; i < jsoo_mount_point.length; i++) {
    var old = prev;
    prev = [0, caml_string_of_jsstring(jsoo_mount_point[i].path), old];
  }
  return prev;
}

//Provides: resolve_fs_device
export function resolve_fs_device(name) {
  var path = caml_make_path(name);
  var name = path.join("/");
  var name_slash = caml_trailing_slash(name);
  var res;
  for (var i = 0; i < jsoo_mount_point.length; i++) {
    var m = jsoo_mount_point[i];
    if (
      name_slash.search(m.path) === 0 &&
      (!res || res.path.length < m.path.length)
    )
      res = {
        path: m.path,
        device: m.device,
        rest: name.slice(m.path.length, name.length),
      };
  }
  if (!res && fs_node_supported()) {
    var root = caml_get_root(name);
    if (root?.match(/^[a-zA-Z]:\/$/)) {
      var m = { path: root, device: new MlNodeDevice(root) };
      jsoo_mount_point.push(m);
      res = {
        path: m.path,
        device: m.device,
        rest: name.slice(m.path.length, name.length),
      };
    }
  }
  if (res) return res;
  caml_raise_sys_error("no device found for " + name_slash);
}

//Provides: caml_mount_autoload
export function caml_mount_autoload(name, f) {
  var path = caml_make_path(name);
  var name = caml_trailing_slash(path.join("/"));
  jsoo_mount_point.push({ path: name, device: new MlFakeDevice(name, f) });
  return 0;
}

//Provides: caml_unmount
export function caml_unmount(name) {
  var path = caml_make_path(name);
  var name = caml_trailing_slash(path.join("/"));
  var idx = -1;
  for (var i = 0; i < jsoo_mount_point.length; i++)
    if (jsoo_mount_point[i].path === name) idx = i;
  if (idx > -1) jsoo_mount_point.splice(idx, 1);
  return 0;
}

//Provides: caml_sys_getcwd
//Alias: caml_unix_getcwd
//Alias: unix_getcwd
export function caml_sys_getcwd() {
  return caml_string_of_jsstring(caml_current_dir);
}

//Provides: caml_sys_chdir
export function caml_sys_chdir(dir, raise_unix) {
  var root = resolve_fs_device(dir);
  if (root.device.is_dir(root.rest)) {
    if (root.rest)
      caml_current_dir = caml_trailing_slash(root.path + root.rest);
    else caml_current_dir = root.path;
    return 0;
  } else if (root.device.exists(root.rest)) {
    caml_raise_system_error(
      raise_unix,
      "ENOTDIR",
      "chdir",
      "not a directory",
      caml_jsstring_of_string(dir),
    );
  } else {
    caml_raise_no_such_file(caml_jsstring_of_string(dir), raise_unix);
  }
}

//Provides: caml_raise_no_such_file
export function caml_raise_no_such_file(name, raise_unix) {
  caml_raise_system_error(
    raise_unix,
    "ENOENT",
    "no such file or directory",
    name,
  );
}

//Provides: caml_sys_file_exists
export function caml_sys_file_exists(name) {
  var root = resolve_fs_device(name);
  return root.device.exists(root.rest);
}

//Provides: caml_sys_read_directory
export function caml_sys_read_directory(name) {
  var root = resolve_fs_device(name);
  var a = root.device.readdir(root.rest);
  var l = new Array(a.length + 1);
  l[0] = 0;
  for (var i = 0; i < a.length; i++) l[i + 1] = caml_string_of_jsstring(a[i]);
  return l;
}

//Provides: caml_sys_remove
export function caml_sys_remove(name) {
  var root = resolve_fs_device(name);
  return root.device.unlink(root.rest);
}

//Provides: caml_sys_is_directory
export function caml_sys_is_directory(name) {
  var root = resolve_fs_device(name);
  var a = root.device.is_dir(root.rest);
  return a ? 1 : 0;
}

//Provides: caml_sys_rename
export function caml_sys_rename(o, n) {
  var o_root = resolve_fs_device(o);
  var n_root = resolve_fs_device(n);
  if (o_root.device !== n_root.device)
    caml_failwith("caml_sys_rename: cannot move file between two filesystem");
  if (!o_root.device.rename) caml_failwith("caml_sys_rename: not implemented");
  o_root.device.rename(o_root.rest, n_root.rest);
}

//Provides: caml_sys_mkdir
export function caml_sys_mkdir(name, perm) {
  var root = resolve_fs_device(name);
  root.device.mkdir(root.rest, perm);
  return 0;
}

//Provides: caml_sys_rmdir
export function caml_sys_rmdir(name) {
  var root = resolve_fs_device(name);
  root.device.rmdir(root.rest);
  return 0;
}

//Provides: caml_ba_map_file
export function caml_ba_map_file(_vfd, _kind, _layout, _shared, _dims, _pos) {
  // var data = caml_sys_fds[vfd];
  caml_failwith("caml_ba_map_file not implemented");
}

//Provides: caml_ba_map_file_bytecode
export function caml_ba_map_file_bytecode(argv, _argn) {
  // argn === 6
  return caml_ba_map_file(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

//Provides: jsoo_create_file_extern
export function jsoo_create_file_extern(name, content) {
  if (globalThis.jsoo_create_file) globalThis.jsoo_create_file(name, content);
  else {
    if (!globalThis.jsoo_fs_tmp) globalThis.jsoo_fs_tmp = [];
    globalThis.jsoo_fs_tmp.push({ name: name, content: content });
  }
  return 0;
}

//Provides: caml_fs_init
export function caml_fs_init() {
  var tmp = globalThis.jsoo_fs_tmp;
  if (tmp) {
    for (var i = 0; i < tmp.length; i++) {
      jsoo_create_file(tmp[i].name, tmp[i].content);
    }
  }
  globalThis.jsoo_create_file = jsoo_create_file;
  globalThis.jsoo_fs_tmp = [];
  return 0;
}

//Provides: caml_create_file
export function caml_create_file(name, content) {
  var root = resolve_fs_device(name);
  if (!root.device.register) caml_failwith("cannot register file");
  root.device.register(root.rest, content);
  return 0;
}

//Provides: jsoo_create_file
export function jsoo_create_file(name, content) {
  var name = caml_string_of_jsstring(name);
  var content = caml_string_of_jsbytes(content);
  return caml_create_file(name, content);
}

//Provides: caml_read_file_content
export function caml_read_file_content(name) {
  var name = typeof name === "string" ? caml_string_of_jsstring(name) : name;
  var root = resolve_fs_device(name);
  if (root.device.exists(root.rest)) {
    var file = root.device.open(root.rest, { rdonly: 1 });
    var len = file.length();
    var buf = new Uint8Array(len);
    file.read(buf, 0, len, false);
    return caml_string_of_uint8_array(buf);
  }
  caml_raise_no_such_file(caml_jsstring_of_string(name));
}

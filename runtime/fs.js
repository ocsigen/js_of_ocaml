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

//Provides: caml_trailing_slash
function caml_trailing_slash(name){
  return (name.slice(-1) !== "/") ? (name + "/") : name;
}

//Provides: caml_current_dir
//Requires: caml_trailing_slash, fs_node_supported
if(fs_node_supported () && globalThis.process && globalThis.process.cwd)
  var caml_current_dir = globalThis.process.cwd().replace(/\\/g,'/');
else
  var caml_current_dir =  "/static";
caml_current_dir = caml_trailing_slash(caml_current_dir);

//Provides: caml_get_root
//Requires: path_is_absolute
function caml_get_root(path){
  var x = path_is_absolute(path);
  if (!x) return;
  return x[0] + "/"}

//Provides: caml_root
//Requires: caml_get_root, caml_current_dir, caml_failwith
var caml_root = caml_get_root(caml_current_dir) || caml_failwith("unable to compute caml_root");


//Provides: MlFile
function MlFile(){  }

//Provides: path_is_absolute
//Requires: fs_node_supported
function make_path_is_absolute() {
  function posix(path) {
    if (path.charAt(0) === '/') return ["", path.substring(1)];
    return;
  }

  function win32(path) {
    // https://github.com/nodejs/node/blob/b3fcc245fb25539909ef1d5eaa01dbf92e168633/lib/path.js#L56
    var splitDeviceRe = /^([a-zA-Z]:|[\\/]{2}[^\\/]+[\\/]+[^\\/]+)?([\\/])?([\s\S]*?)$/;
    var result = splitDeviceRe.exec(path);
    var device = result[1] || '';
    var isUnc = Boolean(device && device.charAt(1) !== ':');

    // UNC paths are always absolute
    if (Boolean(result[2] || isUnc)) {
      var root = (result[1] || '');
      var sep = (result[2] || '');
      return [root, path.substring(root.length + sep.length)]
    }
    return;
  }
  if(fs_node_supported () && globalThis.process && globalThis.process.platform) {
    return globalThis.process.platform === 'win32' ? win32 : posix;
  }
  else return posix
}
var path_is_absolute = make_path_is_absolute();

//Provides: caml_make_path
//Requires: caml_current_dir
//Requires: caml_jsstring_of_string, path_is_absolute
function caml_make_path (name) {
  name=caml_jsstring_of_string(name);
  if( !path_is_absolute(name) )
    name = caml_current_dir + name;
  var comp0 = path_is_absolute(name);
  var comp = comp0[1].split("/");
  var ncomp = []
  for(var i = 0; i<comp.length; i++){
    switch(comp[i]){
    case "..": if(ncomp.length>1) ncomp.pop(); break;
    case ".": break;
    default: ncomp.push(comp[i]);break
    }
  }
  ncomp.unshift(comp0[0]);
  ncomp.orig = name;
  return ncomp;
}

//Provides:jsoo_mount_point
//Requires: MlFakeDevice, MlNodeDevice, caml_root, fs_node_supported
var jsoo_mount_point = []
if (fs_node_supported()) {
  jsoo_mount_point.push({path:caml_root,device:new MlNodeDevice(caml_root)});
} else {
  jsoo_mount_point.push({path:caml_root,device:new MlFakeDevice(caml_root)});
}
jsoo_mount_point.push({path:"/static/", device:new MlFakeDevice("/static/")});

//Provides:caml_list_mount_point
//Requires: jsoo_mount_point, caml_string_of_jsbytes
function caml_list_mount_point(){
  var prev = 0
  for(var i = 0; i < jsoo_mount_point.length; i++){
    var old = prev;
    prev = [0, caml_string_of_jsbytes(jsoo_mount_point[i].path), old]
  }
  return prev;
}

//Provides: resolve_fs_device
//Requires: caml_make_path, jsoo_mount_point, caml_raise_sys_error, caml_get_root, MlNodeDevice, caml_trailing_slash, fs_node_supported
function resolve_fs_device(name){
  var path = caml_make_path(name);
  var name = path.join("/");
  var name_slash = caml_trailing_slash(name);
  var res;
  for(var i = 0; i < jsoo_mount_point.length; i++) {
    var m = jsoo_mount_point[i];
    if(name_slash.search(m.path) == 0
       && (!res || res.path.length < m.path.length))
      res = {path:m.path,device:m.device,rest:name.substring(m.path.length,name.length)};
  }
  if( !res && fs_node_supported()) {
    var root = caml_get_root(name);
    if (root && root.match(/^[a-zA-Z]:\/$/)){
      var m = {path:root,device:new MlNodeDevice(root)};
      jsoo_mount_point.push(m);
      res = {path:m.path,device:m.device,rest:name.substring(m.path.length,name.length)};
    }
  }
  if( res ) return res;
  caml_raise_sys_error("no device found for " + name_slash);
}

//Provides: caml_mount_autoload
//Requires: MlFakeDevice, caml_make_path, jsoo_mount_point, caml_trailing_slash
function caml_mount_autoload(name,f){
  var path = caml_make_path(name);
  var name = caml_trailing_slash(path.join("/"));
  jsoo_mount_point.push({path:name,device:new MlFakeDevice(name,f)})
  return 0;
}

//Provides: caml_unmount
//Requires: jsoo_mount_point, caml_make_path, caml_trailing_slash
function caml_unmount(name){
  var path = caml_make_path(name);
  var name = caml_trailing_slash(path.join("/"));
  var idx = -1;
  for(var i = 0; i < jsoo_mount_point.length; i++)
    if(jsoo_mount_point[i].path == name) idx = i;
  if(idx > -1) jsoo_mount_point.splice(idx,1);
  return 0
}

//Provides: caml_sys_getcwd
//Requires: caml_current_dir, caml_string_of_jsbytes
function caml_sys_getcwd() {
  return caml_string_of_jsbytes(caml_current_dir);
}

//Provides: caml_sys_chdir
//Requires: caml_current_dir, caml_raise_no_such_file, resolve_fs_device, caml_trailing_slash
function caml_sys_chdir(dir) {
  var root = resolve_fs_device(dir);
  if(root.device.exists(root.rest)) {
    if(root.rest) caml_current_dir = caml_trailing_slash(root.path + root.rest);
    else caml_current_dir = root.path;
    return 0;
  }
  else {
    caml_raise_no_such_file(dir);
  }
}

//Provides: caml_raise_no_such_file
//Requires: caml_raise_sys_error
//Requires: caml_jsbytes_of_string
function caml_raise_no_such_file(name){
  name = caml_jsbytes_of_string(name);
  caml_raise_sys_error (name + ": No such file or directory");
}

//Provides: caml_raise_not_a_dir
//Requires: caml_raise_sys_error
//Requires: caml_jsbytes_of_string
function caml_raise_not_a_dir(name){
  name = caml_jsbytes_of_string(name);
  caml_raise_sys_error (name + ": Not a directory");
}

//Provides: caml_sys_file_exists
//Requires: resolve_fs_device
function caml_sys_file_exists (name) {
  var root = resolve_fs_device(name);
  return root.device.exists(root.rest);
}

//Provides: caml_sys_read_directory
//Requires: caml_string_of_jsbytes
//Requires: caml_raise_not_a_dir, resolve_fs_device
function caml_sys_read_directory(name){
  var root = resolve_fs_device(name);
  var a = root.device.readdir(root.rest);
  var l = new Array(a.length + 1);
  l[0] = 0;
  for(var i=0;i<a.length;i++)
    l[i+1] = caml_string_of_jsbytes(a[i]);
  return l;
}

//Provides: caml_sys_remove
//Requires: caml_raise_no_such_file, resolve_fs_device
function caml_sys_remove(name){
  var root = resolve_fs_device(name);
  var ok = root.device.unlink(root.rest);
  if(ok == 0) caml_raise_no_such_file(name);
  return 0;
}

//Provides: caml_sys_is_directory
//Requires: resolve_fs_device
function caml_sys_is_directory(name){
  var root = resolve_fs_device(name);
  var a = root.device.is_dir(root.rest);
  return a?1:0;
}

//Provides: caml_sys_rename
//Requires: caml_failwith, resolve_fs_device
function caml_sys_rename(o,n){
  var o_root = resolve_fs_device(o);
  var n_root = resolve_fs_device(n);
  if(o_root.device != n_root.device)
    caml_failwith("caml_sys_rename: cannot move file between two filesystem");
  if(!o_root.device.rename)
    caml_failwith("caml_sys_rename: no implemented");
  o_root.device.rename(o_root.rest, n_root.rest);
}

//Provides: caml_sys_mkdir
//Requires: resolve_fs_device, caml_raise_sys_error
function caml_sys_mkdir(name, perm){
  var root = resolve_fs_device(name);
  root.device.mkdir(root.rest,perm);
  return 0;
}

//Provides: caml_sys_rmdir
//Requires: resolve_fs_device, caml_raise_sys_error, caml_raise_not_a_dir
function caml_sys_rmdir(name){
  var root = resolve_fs_device(name);
  root.device.rmdir(root.rest);
  return 0;
}

//Provides: caml_ba_map_file
//Requires: caml_failwith
function caml_ba_map_file(vfd, kind, layout, shared, dims, pos) {
  // var data = caml_global_data.fds[vfd];
  caml_failwith("caml_ba_map_file not implemented");
}

//Provides: caml_ba_map_file_bytecode
//Requires: caml_ba_map_file
function caml_ba_map_file_bytecode(argv,argn){
  return caml_ba_map_file(argv[0],argv[1],argv[2],argv[3],argv[4],argv[5]);
}

//Provides: jsoo_create_file_extern
function jsoo_create_file_extern(name,content){
  if(globalThis.jsoo_create_file)
    globalThis.jsoo_create_file(name,content);
  else {
    if(!globalThis.caml_fs_tmp) globalThis.caml_fs_tmp = [];
    globalThis.caml_fs_tmp.push({name:name,content:content});
  }
  return 0;
}

//Provides: caml_fs_init
//Requires: jsoo_create_file
function caml_fs_init (){
  var tmp=globalThis.caml_fs_tmp
  if(tmp){
    for(var i = 0; i < tmp.length; i++){
      jsoo_create_file(tmp[i].name,tmp[i].content);
    }
  }
  globalThis.jsoo_create_file = jsoo_create_file;
  globalThis.caml_fs_tmp = [];
  return 0;
}

//Provides: caml_create_file
//Requires: caml_failwith, resolve_fs_device
function caml_create_file(name,content) {
  var root = resolve_fs_device(name);
  if(! root.device.register) caml_failwith("cannot register file");
  root.device.register(root.rest,content);
  return 0;
}


//Provides: jsoo_create_file
//Requires: caml_create_file, caml_string_of_jsbytes
function jsoo_create_file(name,content) {
  var name = caml_string_of_jsbytes(name);
  var content = caml_string_of_jsbytes(content);
  return caml_create_file(name, content);
}


//Provides: caml_read_file_content
//Requires: resolve_fs_device, caml_raise_no_such_file, caml_create_bytes, caml_string_of_bytes
//Requires: caml_string_of_jsbytes
function caml_read_file_content (name) {
  var name = (typeof name == "string")?caml_string_of_jsbytes(name):name;
  var root = resolve_fs_device(name);
  if(root.device.exists(root.rest)) {
    var file = root.device.open(root.rest,{rdonly:1});
    var len  = file.length();
    var buf  = caml_create_bytes(len);
    file.read(0,buf,0,len);
    return caml_string_of_bytes(buf)
  }
  caml_raise_no_such_file(name);
}

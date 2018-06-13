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

//Provides: caml_current_dir
if(joo_global_object.process && joo_global_object.process.cwd)
  var caml_current_dir = joo_global_object.process.cwd().replace(/\\/g,'/');
else
  var caml_current_dir =  "/static";
if(caml_current_dir.slice(-1) !== "/") caml_current_dir += "/"

//Provides: caml_root
//Requires: caml_current_dir
var caml_root = caml_current_dir.match(/[^\/]*\//)[0];


//Provides: MlFile
function MlFile(){  }

//Provides: caml_make_path
//Requires: caml_current_dir,MlBytes
function caml_make_path (name) {
  name=(name instanceof MlBytes)?name.toString():name;
  if(name.charCodeAt(0) != 47)
    name = caml_current_dir + name;
  var comp = name.split("/");
  var ncomp = []
  for(var i = 0; i<comp.length; i++){
    switch(comp[i]){
    case "..": if(ncomp.length>1) ncomp.pop(); break;
    case ".": break;
    case "": if(ncomp.length == 0) ncomp.push(""); break;
    default: ncomp.push(comp[i]);break
    }
  }
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
jsoo_mount_point.push({path:caml_root+"static/", device:new MlFakeDevice(caml_root+"static/")});

//Provides:caml_list_mount_point
//Requires: jsoo_mount_point, caml_new_string
function caml_list_mount_point(){
    var prev = 0
    for(var i = 0; i < jsoo_mount_point.length; i++){
        var old = prev;
        prev = [0, caml_new_string(jsoo_mount_point[i].path), old]
    }
    return prev;
}

//Provides: resolve_fs_device
//Requires: caml_make_path, jsoo_mount_point
function resolve_fs_device(name){
  var path = caml_make_path(name);
  var name = path.join("/");
  var name_slash = name + "/";
  var res;
  for(var i = 0; i < jsoo_mount_point.length; i++) {
    var m = jsoo_mount_point[i];
    if(name_slash.search(m.path) == 0
       && (!res || res.path.length < m.path.length))
        res = {path:m.path,device:m.device,rest:name.substring(m.path.length,name.length)};
  }
  return res;
}

//Provides: caml_mount_autoload
//Requires: MlFakeDevice, caml_make_path, jsoo_mount_point
function caml_mount_autoload(name,f){
  var path = caml_make_path(name);
  var name = path.join("/") + "/";
  jsoo_mount_point.push({path:name,device:new MlFakeDevice(name,f)})
  return 0;
}

//Provides: caml_unmount
//Requires: jsoo_mount_point, caml_make_path
function caml_unmount(name){
  var path = caml_make_path(name);
  var name = path.join("/") + "/";
  var idx = -1;
  for(var i = 0; i < jsoo_mount_point.length; i++)
    if(jsoo_mount_point[i].path == name) idx = i;
  if(idx > -1) jsoo_mount_point.splice(idx,1);
  return 0
}

//Provides: caml_sys_getcwd
//Requires: caml_current_dir, caml_new_string
function caml_sys_getcwd() {
  return caml_new_string(caml_current_dir);
}

//Provides: caml_sys_chdir
//Requires: caml_current_dir, caml_raise_no_such_file, resolve_fs_device
function caml_sys_chdir(dir) {
  var root = resolve_fs_device(dir);
  if(root.device.exists(root.rest)) {
    if(root.rest) caml_current_dir = root.path + root.rest + "/";
    else caml_current_dir = root.path;
    return 0;
  }
  else {
    caml_raise_no_such_file(dir);
  }
}

//Provides: caml_raise_no_such_file
//Requires: MlBytes, caml_raise_sys_error
function caml_raise_no_such_file(name){
  name = (name instanceof MlBytes)?name.toString():name;
  caml_raise_sys_error (name + ": No such file or directory");
}

//Provides: caml_raise_not_a_dir
//Requires: MlBytes, caml_raise_sys_error
function caml_raise_not_a_dir(name){
  name = (name instanceof MlBytes)?name.toString():name;
  caml_raise_sys_error (name + ": Not a directory");
}

//Provides: caml_sys_file_exists
//Requires: resolve_fs_device
function caml_sys_file_exists (name) {
  var root = resolve_fs_device(name);
  return root.device.exists(root.rest);
}

//Provides: caml_sys_read_directory
//Requires: caml_new_string
//Requires: caml_raise_not_a_dir, resolve_fs_device
function caml_sys_read_directory(name){
  var root = resolve_fs_device(name);
  var a = root.device.readdir(root.rest);
  var l = new Array(a.length + 1);
  l[0] = 0;
  for(var i=0;i<a.length;i++)
    l[i+1] = caml_new_string(a[i]);
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

//Provides: caml_create_file_extern
function caml_create_file_extern(name,content){
  if(joo_global_object.caml_create_file)
    joo_global_object.caml_create_file(name,content);
  else {
    if(!joo_global_object.caml_fs_tmp) joo_global_object.caml_fs_tmp = [];
    joo_global_object.caml_fs_tmp.push({name:name,content:content});
  }
  return 0;
}

//Provides: caml_fs_init
//Requires: caml_create_file
function caml_fs_init (){
  var tmp=joo_global_object.caml_fs_tmp
  if(tmp){
    for(var i = 0; i < tmp.length; i++){
      caml_create_file(tmp[i].name,tmp[i].content);
    }
  }
  joo_global_object.caml_create_file = caml_create_file;
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

//Provides: caml_read_file_content
//Requires: resolve_fs_device, caml_raise_no_such_file, caml_create_bytes
function caml_read_file_content (name) {
  var root = resolve_fs_device(name);
  if(root.device.exists(root.rest)) {
    var file = root.device.open(root.rest,{rdonly:1});
    var len  = file.length();
    var buf  = caml_create_bytes(len);
    file.read(0,buf,0,len);
    return buf
  }
  caml_raise_no_such_file(name);
}

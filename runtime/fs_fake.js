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

//Provides: MlFakeDevice
//Requires: MlFakeFile, caml_create_bytes
//Requires: caml_raise_sys_error, caml_raise_no_such_file, caml_new_string, caml_string_of_array
//Requires: MlBytes
function MlFakeDevice (root, f) {
  this.content={};
  this.root = root;
  this.lookupFun = f;
}
MlFakeDevice.prototype.nm = function(name) {
  return (this.root + name);
}
MlFakeDevice.prototype.lookup = function(name) {
  if(!this.content[name] && this.lookupFun) {
    var res = this.lookupFun(caml_new_string(this.root), caml_new_string(name));
    if(res != 0) this.content[name]=new MlFakeFile(res[1]);
  }
}
MlFakeDevice.prototype.exists = function(name) {
  // The root of the device exists
  if(name == "") return 1;
  // Check if a directory exists
  var name_slash = (name + "/");
  var r = new RegExp("^" + name_slash);
  for(var n in this.content) {
    if (n.match(r)) return 1
  }
  // Check if a file exists
  this.lookup(name);
  return this.content[name]?1:0;
}
MlFakeDevice.prototype.readdir = function(name) {
  var name_slash = (name == "")?"":(name + "/");
  var r = new RegExp("^" + name_slash + "([^/]*)");
  var seen = {}
  var a = [];
  for(var n in this.content) {
    var m = n.match(r);
    if(m && !seen[m[1]]) {seen[m[1]] = true; a.push(m[1])}
  }
  return a;
}
MlFakeDevice.prototype.is_dir = function(name) {
  var name_slash = (name == "")?"":(name + "/");
  var r = new RegExp("^" + name_slash + "([^/]*)");
  var a = [];
  for(var n in this.content) {
    var m = n.match(r);
    if(m) return 1
  }
  return 0
}
MlFakeDevice.prototype.unlink = function(name) {
  var ok = this.content[name]?true:false;
  delete this.content[name];
  return ok;
}
MlFakeDevice.prototype.open = function(name, f) {
  if(f.rdonly && f.wronly)
    caml_raise_sys_error(this.nm(name) + " : flags Open_rdonly and Open_wronly are not compatible");
  if(f.text && f.binary)
    caml_raise_sys_error(this.nm(name) + " : flags Open_text and Open_binary are not compatible");
  this.lookup(name);
  if (this.content[name]) {
    if (this.is_dir(name)) caml_raise_sys_error(this.nm(name) + " : is a directory");
    if (f.create && f.excl) caml_raise_sys_error(this.nm(name) + " : file already exists");
    var file = this.content[name];
    if(f.truncate) file.truncate();
    return file;
  } else if (f.create) {
    this.content[name] = new MlFakeFile(caml_create_bytes(0));
    return this.content[name];
  } else {
    caml_raise_no_such_file (this.nm(name));
  }
}

MlFakeDevice.prototype.register= function (name,content){
  if(this.content[name]) caml_raise_sys_error(this.nm(name) + " : file already exists");
  if(content instanceof MlBytes)
    this.content[name] = new MlFakeFile(content);
  else if(content instanceof Array)
    this.content[name] = new MlFakeFile(caml_string_of_array(content));
  else if(content.toString) {
    var mlstring = caml_new_string(content.toString());
    this.content[name] = new MlFakeFile(mlstring);
  }
}

MlFakeDevice.prototype.constructor = MlFakeDevice

//Provides: MlFakeFile
//Requires: MlFile
//Requires: caml_create_bytes, caml_ml_bytes_length,caml_blit_bytes
//Requires: caml_bytes_get
function MlFakeFile(content){
  this.data = content;
}
MlFakeFile.prototype = new MlFile ();
MlFakeFile.prototype.truncate = function(len){
  var old = this.data;
  this.data = caml_create_bytes(len|0);
  caml_blit_bytes(old, 0, this.data, 0, len);
}
MlFakeFile.prototype.length = function () {
  return caml_ml_bytes_length(this.data);
}
MlFakeFile.prototype.write = function(offset,buf,pos,len){
  var clen = this.length();
  if(offset + len >= clen) {
    var new_str = caml_create_bytes(offset + len);
    var old_data = this.data;
    this.data = new_str;
    caml_blit_bytes(old_data, 0, this.data, 0, clen);
  }
  caml_blit_bytes(buf, pos, this.data, offset, len);
  return 0
}
MlFakeFile.prototype.read = function(offset,buf,pos,len){
  var clen = this.length();
  caml_blit_bytes(this.data, offset, buf, pos, len);
  return 0
}
MlFakeFile.prototype.read_one = function(offset){
  return caml_bytes_get(this.data, offset);
}
MlFakeFile.prototype.close = function(){

}
MlFakeFile.prototype.constructor = MlFakeFile

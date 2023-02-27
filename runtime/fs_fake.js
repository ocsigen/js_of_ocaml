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
//Requires: MlFakeFile, MlFakeFd, caml_create_bytes
//Requires: caml_raise_sys_error, caml_raise_no_such_file
//Requires: caml_string_of_jsbytes, caml_string_of_jsstring
//Requires: caml_bytes_of_array, caml_bytes_of_string, caml_bytes_of_jsbytes
//Requires: caml_is_ml_bytes, caml_is_ml_string
//Requires: caml_named_value, caml_raise_with_args, caml_named_values
//Requires: make_unix_err_args
function MlFakeDevice (root, f) {
  this.content={};
  this.root = root;
  this.lookupFun = f;
}
MlFakeDevice.prototype.nm = function(name) {
  return (this.root + name);
}
MlFakeDevice.prototype.create_dir_if_needed = function(name) {
  var comp = name.split("/");
  var res = "";
  for(var i = 0; i < comp.length - 1; i++){
    res += comp[i] + "/";
    if(this.content[res]) continue;
    this.content[res] = Symbol("directory");
  }
}
MlFakeDevice.prototype.slash = function(name){
  return /\/$/.test(name)?name:(name + "/");
}
MlFakeDevice.prototype.lookup = function(name) {
  if(!this.content[name] && this.lookupFun) {
    var res = this.lookupFun(caml_string_of_jsbytes(this.root), caml_string_of_jsbytes(name));
    if(res !== 0) {
      this.create_dir_if_needed(name);
      this.content[name]=new MlFakeFile(caml_bytes_of_string(res[1]));
    }
  }
}
MlFakeDevice.prototype.exists = function(name) {
  // The root of the device exists
  if(name == "") return 1;
  // Check if a directory exists
  var name_slash = this.slash(name);
  if(this.content[name_slash]) return 1;
  // Check if a file exists
  this.lookup(name);
  return this.content[name]?1:0;
}
MlFakeDevice.prototype.isFile = function(name) {
  if(this.exists(name) && !this.is_dir(name)) {
    return 1
  }
  else {
    return 0
  }
}
MlFakeDevice.prototype.mkdir = function(name,mode, raise_unix) {
  var unix_error = raise_unix && caml_named_value('Unix.Unix_error');
  if(this.exists(name)) {
    if (unix_error) {
      caml_raise_with_args(unix_error, make_unix_err_args("EEXIST", "mkdir", this.nm(name)));
    }
    else {
      caml_raise_sys_error(name + ": File exists");
    }
  }
  var parent = /^(.*)\/[^/]+/.exec(name);
  parent = (parent && parent[1]) || '';
  if(!this.exists(parent)){
    if (unix_error) {
      caml_raise_with_args(unix_error, make_unix_err_args("ENOENT", "mkdir", this.nm(parent)));
    }
    else {
      caml_raise_sys_error(parent + ": No such file or directory");
    }
  }
  if(!this.is_dir(parent)){
    if (unix_error) {
      caml_raise_with_args(unix_error, make_unix_err_args("ENOTDIR", "mkdir", this.nm(parent)));
    }
    else {
      caml_raise_sys_error(parent + ": Not a directory");
    }
  }
  this.create_dir_if_needed(this.slash(name));
}
MlFakeDevice.prototype.rmdir = function(name, raise_unix) {
  var unix_error = raise_unix && caml_named_value('Unix.Unix_error');
  var name_slash = (name == "")?"":(this.slash(name));
  var r = new RegExp("^" + name_slash + "([^/]+)");
  if(!this.exists(name)) {
    if (unix_error) {
      caml_raise_with_args(unix_error, make_unix_err_args("ENOENT", "rmdir", this.nm(name)));
    }
    else {
      caml_raise_sys_error(name + ": No such file or directory");
    }
  }
  if(!this.is_dir(name)) {
    if (unix_error) {
      caml_raise_with_args(unix_error, make_unix_err_args("ENOTDIR", "rmdir", this.nm(name)));
    }
    else {
      caml_raise_sys_error(name + ": Not a directory");
    }
  }
  for(var n in this.content) {
    if(n.match(r)) {
      if (unix_error) {
        caml_raise_with_args(unix_error, make_unix_err_args("ENOTEMPTY", "rmdir", this.nm(name)));
      } else {
        caml_raise_sys_error(this.nm(name) + ": Directory not empty");
      }
    }
  }
  delete this.content[name_slash];
}
MlFakeDevice.prototype.readdir = function(name) {
  var name_slash = (name == "")?"":(this.slash(name));
  if(!this.exists(name)) {
    caml_raise_sys_error(name + ": No such file or directory");
  }
  if(!this.is_dir(name)) {
    caml_raise_sys_error(name + ": Not a directory");
  }
  var r = new RegExp("^" + name_slash + "([^/]+)");
  var seen = {}
  var a = [];
  for(var n in this.content) {
    var m = n.match(r);
    if(m && !seen[m[1]]) {seen[m[1]] = true; a.push(m[1])}
  }
  return a;
}
MlFakeDevice.prototype.opendir = function(name, raise_unix) {
  var unix_error = raise_unix && caml_named_value('Unix.Unix_error');

  var a = this.readdir(name);
  var c = false;
  var i = 0;
  return { readSync : (function () {
    if (c) {
      if (unix_error) {
        caml_raise_with_args(unix_error, make_unix_err_args("EBADF", "closedir", this.nm(name)));
      }
      else {
        caml_raise_sys_error(name + ": closedir failed");
      }
    }
    if(i == a.length) return null;
    var entry = a[i];
    i++;
    return { name: entry }
  })
    , closeSync: (function () {
      if (c) {
        if (unix_error) {
          caml_raise_with_args(unix_error, make_unix_err_args("EBADF", "closedir", this.nm(name)));
        }
        else {
          caml_raise_sys_error(name + ": closedir failed");
        }
      }
      c = true;
      a = [];
    })
  }
}
MlFakeDevice.prototype.is_dir = function(name) {
  if(name == "")  return true;
  var name_slash = this.slash(name);
  return this.content[name_slash]?1:0;
}
MlFakeDevice.prototype.unlink = function(name) {
  var ok = this.content[name]?true:false;
  delete this.content[name];
  return ok;
}
MlFakeDevice.prototype.open = function(name, f) {
  var file;
  if(f.rdonly && f.wronly)
    caml_raise_sys_error(this.nm(name) + " : flags Open_rdonly and Open_wronly are not compatible");
  if(f.text && f.binary)
    caml_raise_sys_error(this.nm(name) + " : flags Open_text and Open_binary are not compatible");
  this.lookup(name);
  if (this.content[name]) {
    if (this.is_dir(name)) caml_raise_sys_error(this.nm(name) + " : is a directory");
    if (f.create && f.excl) caml_raise_sys_error(this.nm(name) + " : file already exists");
    file = this.content[name];
    if(f.truncate) file.truncate();
  } else if (f.create) {
    this.create_dir_if_needed(name);
    this.content[name] = new MlFakeFile(caml_create_bytes(0));
    file = this.content[name];
  } else {
    caml_raise_no_such_file (this.nm(name));
  }
  return new MlFakeFd(this.nm(name), file, f);
}

MlFakeDevice.prototype.open = function(name, f) {
  var file;
  if(f.rdonly && f.wronly)
    caml_raise_sys_error(this.nm(name) + " : flags Open_rdonly and Open_wronly are not compatible");
  if(f.text && f.binary)
    caml_raise_sys_error(this.nm(name) + " : flags Open_text and Open_binary are not compatible");
  this.lookup(name);
  if (this.content[name]) {
    if (this.is_dir(name)) caml_raise_sys_error(this.nm(name) + " : is a directory");
    if (f.create && f.excl) caml_raise_sys_error(this.nm(name) + " : file already exists");
    file = this.content[name];
    if(f.truncate) file.truncate();
  } else if (f.create) {
    this.create_dir_if_needed(name);
    this.content[name] = new MlFakeFile(caml_create_bytes(0));
    file = this.content[name];
  } else {
    caml_raise_no_such_file (this.nm(name));
  }
  return new MlFakeFd(this.nm(name), file, f);
}

MlFakeDevice.prototype.register= function (name,content){
  var file;
  if(this.content[name]) caml_raise_sys_error(this.nm(name) + " : file already exists");
  if(caml_is_ml_bytes(content))
    file = new MlFakeFile(content);
  if(caml_is_ml_string(content))
    file = new MlFakeFile(caml_bytes_of_string(content));
  else if(content instanceof Array)
    file = new MlFakeFile(caml_bytes_of_array(content));
  else if(typeof content === "string")
    file = new MlFakeFile(caml_bytes_of_jsbytes(content));
  else if(content.toString) {
    var bytes = caml_bytes_of_string(caml_string_of_jsstring(content.toString()));
    file = new MlFakeFile(bytes);
  }
  if(file){
    this.create_dir_if_needed(name);
    this.content[name] = file;
  }
  else caml_raise_sys_error(this.nm(name) + " : registering file with invalid content type");
}

MlFakeDevice.prototype.constructor = MlFakeDevice

//Provides: MlFakeFile
//Requires: MlFile
//Requires: caml_create_bytes, caml_ml_bytes_length, caml_blit_bytes
//Requires: caml_uint8_array_of_bytes, caml_bytes_of_array
function MlFakeFile(content){
  this.data = content;
}
MlFakeFile.prototype = new MlFile ();
MlFakeFile.prototype.constructor = MlFakeFile
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
  caml_blit_bytes(caml_bytes_of_array(buf), pos, this.data, offset, len);
  return 0
}
MlFakeFile.prototype.read = function(offset,buf,pos,len){
  var clen = this.length();
  if(offset + len >= clen) {
    len = clen - offset;
  }
  if(len) {
    var data = caml_create_bytes(len|0);
    caml_blit_bytes(this.data, offset, data, 0, len);
    buf.set(caml_uint8_array_of_bytes(data), pos);
  }
  return len
}


//Provides: MlFakeFd_out
//Requires: MlFakeFile, caml_create_bytes, caml_blit_bytes, caml_bytes_of_array
//Requires: caml_raise_sys_error
function MlFakeFd_out(fd,flags) {
  MlFakeFile.call(this, caml_create_bytes(0));
  this.log = (function (s) { return 0 });
  if(fd == 1 && typeof console.log == "function")
    this.log = console.log;
  else if(fd == 2 && typeof console.error == "function")
    this.log = console.error;
  else if(typeof console.log == "function")
    this.log = console.log
  this.flags = flags;
}
MlFakeFd_out.prototype.length = function() { return 0 }
MlFakeFd_out.prototype.write = function (offset,buf,pos,len) {
  if(this.log) {
    if(len > 0
       && pos >= 0
       && pos+len <= buf.length
       && buf[pos+len-1] == 10)
      len --;
    // Do not output the last \n if present
    // as console logging display a newline at the end
    var src = caml_create_bytes(len);
    caml_blit_bytes(caml_bytes_of_array(buf), pos, src, 0, len);
    this.log(src.toUtf16());
    return 0;
  }
  caml_raise_sys_error(this.fd  + ": file descriptor already closed");
}
MlFakeFd_out.prototype.read = function (offset, buf, pos, len) {
  caml_raise_sys_error(this.fd  + ": file descriptor is write only");
}
MlFakeFd_out.prototype.close = function () {
  this.log = undefined;
}


//Provides: MlFakeFd
//Requires: MlFakeFile
//Requires: caml_raise_sys_error
function MlFakeFd(name, file,flags) {
  this.file = file;
  this.name = name;
  this.flags = flags;
}

MlFakeFd.prototype.err_closed = function () {
  caml_raise_sys_error(this.name  + ": file descriptor already closed");
}
MlFakeFd.prototype.length = function() {
  if(this.file) return this.file.length ()
  this.err_closed();
}
MlFakeFd.prototype.write = function (offset,buf,pos,len) {
  if(this.file) return this.file.write(offset,buf,pos,len)
  this.err_closed();
}
MlFakeFd.prototype.read = function (offset, buf, pos, len) {
  if(this.file) return this.file.read(offset, buf, pos, len)
  this.err_closed();
}
MlFakeFd.prototype.close = function () {
  this.file = undefined;
}

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
function fs_node_supported () {
  return (
    typeof joo_global_object.process !== 'undefined'
      && typeof joo_global_object.process.versions !== 'undefined'
      && typeof joo_global_object.process.versions.node !== 'undefined'
      && joo_global_object.process.platform !== "browser")
}


//Provides: MlNodeDevice
//Requires: MlNodeFile, caml_raise_sys_error
//Requires: caml_raise_with_args, caml_named_value, caml_string_of_jsbytes
function MlNodeDevice(root) {
  this.fs = require('fs');
  this.root = root;
}
MlNodeDevice.prototype.nm = function(name) {
  return (this.root + name);
}
MlNodeDevice.prototype.exists = function(name) {
  try {
    return this.fs.existsSync(this.nm(name))?1:0;
  } catch (err) {
    // TODO: Is it correct to always return a boolean, even on error?
    return 0;
  }
}
MlNodeDevice.prototype.mkdir = function(name, mode) {
  try {
    this.fs.mkdirSync(this.nm(name),{mode:mode});
    return 0
  } catch (err) {
    this.raise_unix_exn_of_nodejs_error(err);
  }
}
MlNodeDevice.prototype.rmdir = function(name) {
  try {
    this.fs.rmdirSync(this.nm(name));
    return 0
  } catch (err) {
    this.raise_unix_exn_of_nodejs_error(err);
  }
}
MlNodeDevice.prototype.readdir = function(name) {
  try {
    return this.fs.readdirSync(this.nm(name));
  } catch (err) {
    this.raise_unix_exn_of_nodejs_error(err);
  }
}
MlNodeDevice.prototype.is_dir = function(name) {
  try {
    return this.fs.statSync(this.nm(name)).isDirectory()?1:0;
  } catch (err) {
    caml_raise_sys_error(err.toString());
  }
}
MlNodeDevice.prototype.unlink = function(name) {
  try {
    var b = this.fs.existsSync(this.nm(name))?1:0;
    this.fs.unlinkSync(this.nm(name));
  } catch (err) {
    this.raise_unix_exn_of_nodejs_error(err);
  }
  return b
}
MlNodeDevice.prototype.open = function(name, f) {
  var consts = require('constants');
  var res = 0;
  for(var key in f){
    switch(key){
    case "rdonly"  : res |= consts.O_RDONLY; break;
    case "wronly"  : res |= consts.O_WRONLY; break;
    case "append"  :
      res |= consts.O_WRONLY | consts.O_APPEND;
      break;
    case "create"   : res |= consts.O_CREAT;    break;
    case "truncate" : res |= consts.O_TRUNC;    break;
    case "excl"     : res |= consts.O_EXCL;     break;
    case "binary"   : res |= consts.O_BINARY;   break;
    case "text"     : res |= consts.O_TEXT;     break;
    case "nonblock" : res |= consts.O_NONBLOCK; break;
    }
  }
  try {
    var fd = this.fs.openSync(this.nm(name), res);
    return new MlNodeFile(fd);
  } catch (err) {
    this.raise_unix_exn_of_nodejs_error(err);
  }
}

MlNodeDevice.prototype.rename = function(o,n) {
  try {
    this.fs.renameSync(this.nm(o), this.nm(n));
  } catch (err) {
    this.raise_unix_exn_of_nodejs_error(err);
  }
}
MlNodeDevice.prototype.raise_unix_exn_of_nodejs_error = function(err) {
  /* ===Unix.error===
   *
   * This array is in order of the variant in OCaml
   */
  var errors = [
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
  var variant = errors.indexOf(err.code);
  if (variant < 0) {
    variant = BLOCK(0, err.errno);
  }
  var args = [
    variant,
    caml_string_of_jsbytes(err.syscall || ""),
    caml_string_of_jsbytes(err.path || "")
  ];
  caml_raise_with_args(caml_named_value("Unix.Unix_error"), args.length, args);
}

MlNodeDevice.prototype.constructor = MlNodeDevice

//Provides: MlNodeFile
//Requires: MlFile, caml_array_of_string, caml_array_of_bytes, caml_bytes_set, caml_raise_sys_error
function MlNodeFile(fd){
  this.fs = require('fs');
  this.fd = fd;
}
MlNodeFile.prototype = new MlFile ();

MlNodeFile.prototype.truncate = function(len){
  try {
    this.fs.ftruncateSync(this.fd,len|0)
  } catch (err) {
    caml_raise_sys_error(err.toString());
  }
}
MlNodeFile.prototype.length = function () {
  try {
    return this.fs.fstatSync(this.fd).size;
  } catch (err) {
    caml_raise_sys_error(err.toString());
  }
}
MlNodeFile.prototype.write = function(offset,buf,buf_offset,len){
  var a = caml_array_of_string(buf);
  if(! (a instanceof joo_global_object.Uint8Array))
    a = new joo_global_object.Uint8Array(a);
  var buffer = joo_global_object.Buffer.from(a);
  try {
    this.fs.writeSync(this.fd, buffer, buf_offset, len, offset);
  } catch (err) {
    caml_raise_sys_error(err.toString());
  }
  return 0;
}
MlNodeFile.prototype.read = function(offset,buf,buf_offset,len){
  var a = caml_array_of_bytes(buf);
  if(! (a instanceof joo_global_object.Uint8Array))
    a = new joo_global_object.Uint8Array(a);
  var buffer = joo_global_object.Buffer.from(a);
  try {
    this.fs.readSync(this.fd, buffer, buf_offset, len, offset);
  } catch (err) {
    caml_raise_sys_error(err.toString());
  }
  for(var i = 0; i < len; i++){
    caml_bytes_set(buf,buf_offset + i,buffer[buf_offset+i]);
  }
  return 0
}
MlNodeFile.prototype.read_one = function(offset){
  var a = new joo_global_object.Uint8Array(1);
  var buffer = joo_global_object.Buffer.from(a);
  try {
    this.fs.readSync(this.fd, buffer, 0, 1, offset);
  } catch (err) {
    caml_raise_sys_error(err.toString());
  }
  return buffer[0];
}
MlNodeFile.prototype.close = function(){
  try {
    this.fs.closeSync(this.fd);
  } catch (err) {
    caml_raise_sys_error(err.toString());
  }
}

MlNodeFile.prototype.constructor = MlNodeFile;

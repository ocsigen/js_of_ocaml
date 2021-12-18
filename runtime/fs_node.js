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
    typeof globalThis.process !== 'undefined'
      && typeof globalThis.process.versions !== 'undefined'
      && typeof globalThis.process.versions.node !== 'undefined')
}
//Provides: fs_node_supported
//If: browser
function fs_node_supported () {
  return false
}


//Provides: MlNodeDevice
//Requires: MlNodeFile, caml_raise_sys_error, caml_raise_with_args
//Requires: make_unix_err_args, caml_named_value, caml_string_of_jsstring
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
    return 0;
  }
}
MlNodeDevice.prototype.mkdir = function(name, mode, raise_unix) {
  try {
    this.fs.mkdirSync(this.nm(name),{mode:mode});
    return 0
  } catch (err) {
    this.raise_nodejs_error(err, raise_unix);
  }
}
MlNodeDevice.prototype.rmdir = function(name, raise_unix) {
  try {
    this.fs.rmdirSync(this.nm(name));
    return 0
  } catch (err) {
    this.raise_nodejs_error(err, raise_unix);
  }
}
MlNodeDevice.prototype.readdir = function(name, raise_unix) {
  try {
    return this.fs.readdirSync(this.nm(name));
  } catch (err) {
    this.raise_nodejs_error(err, raise_unix);
  }
}
MlNodeDevice.prototype.is_dir = function(name) {
  try {
    return this.fs.statSync(this.nm(name)).isDirectory()?1:0;
  } catch (err) {
    caml_raise_sys_error(err.toString());
  }
}
MlNodeDevice.prototype.unlink = function(name, raise_unix) {
  try {
    var b = this.fs.existsSync(this.nm(name))?1:0;
    this.fs.unlinkSync(this.nm(name));
    return b;
  } catch (err) {
    this.raise_nodejs_error(err, raise_unix);
  }
}
MlNodeDevice.prototype.open = function(name, f, raise_unix) {
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
    this.raise_nodejs_error(err, raise_unix);
  }
}

MlNodeDevice.prototype.rename = function(o, n, raise_unix) {
  try {
    this.fs.renameSync(this.nm(o), this.nm(n));
  } catch (err) {
    this.raise_nodejs_error(err, raise_unix);
  }
}
MlNodeDevice.prototype.stat = function(name, raise_unix) {
  try {
    var js_stats = this.fs.statSync(this.nm(name));
    return this.stats_from_js(js_stats);
  } catch (err) {
    this.raise_nodejs_error(err, raise_unix);
  }
}
MlNodeDevice.prototype.lstat = function(name, raise_unix) {
  try {
    var js_stats = this.fs.lstatSync(this.nm(name));
    return this.stats_from_js(js_stats);
  } catch (err) {
    this.raise_nodejs_error(err, raise_unix);
  }
}
MlNodeDevice.prototype.symlink = function(to_dir, target, path, raise_unix) {
  try {
    this.fs.symlinkSync(this.nm(target), this.nm(path), to_dir ? 'dir' : 'file');
    return 0;
  } catch (err) {
    this.raise_nodejs_error(err, raise_unix);
  }
}
MlNodeDevice.prototype.readlink = function(name, raise_unix) {
  try {
    var link = this.fs.readlinkSync(this.nm(name), 'utf8');
    return caml_string_of_jsstring(link);
  } catch (err) {
    this.raise_nodejs_error(err, raise_unix);
  }
}
MlNodeDevice.prototype.raise_nodejs_error = function(err, raise_unix) {
  var unix_error = caml_named_value("Unix.Unix_error");
  if (raise_unix && unix_error) {
    var args = make_unix_err_args(err.code, err.syscall, err.path, err.errno);
    caml_raise_with_args(unix_error, args);
  } else {
    caml_raise_sys_error(err.toString());
  }
}
MlNodeDevice.prototype.stats_from_js = function(js_stats) {
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
  } else if (js_stats.isDirectory()) {
    file_kind = 1;
  } else if (js_stats.isCharacterDevice()) {
    file_kind = 2;
  } else if (js_stats.isBlockDevice()) {
    file_kind = 3;
  } else if (js_stats.isSymbolicLink()) {
    file_kind = 4;
  } else if (js_stats.isFIFO()) {
    file_kind = 5;
  } else if (js_stats.isSocket()) {
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

MlNodeDevice.prototype.constructor = MlNodeDevice

//Provides: MlNodeDevice
//If: browser
function MlNodeDevice() {
}

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
  if(! (a instanceof globalThis.Uint8Array))
    a = new globalThis.Uint8Array(a);
  var buffer = globalThis.Buffer.from(a);
  try {
    this.fs.writeSync(this.fd, buffer, buf_offset, len, offset);
  } catch (err) {
    caml_raise_sys_error(err.toString());
  }
  return 0;
}
MlNodeFile.prototype.read = function(offset,buf,buf_offset,len){
  var a = caml_array_of_bytes(buf);
  if(! (a instanceof globalThis.Uint8Array))
    a = new globalThis.Uint8Array(a);
  var buffer = globalThis.Buffer.from(a);
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
  var a = new globalThis.Uint8Array(1);
  var buffer = globalThis.Buffer.from(a);
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

//Provides: MlNodeFile
//If: browser
function MlNodeFile(){
}

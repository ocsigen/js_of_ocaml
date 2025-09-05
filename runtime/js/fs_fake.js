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
//Requires: caml_raise_system_error
class MlFakeDevice {
  constructor(root, f) {
    this.content = {};
    this.root = root;
    this.lookupFun = f;
  }

  nm(name) {
    return this.root + name;
  }

  create_dir_if_needed(name) {
    var comp = name.split("/");
    var res = "";
    for (var i = 0; i < comp.length - 1; i++) {
      res += comp[i] + "/";
      if (this.content[res]) continue;
      this.content[res] = Symbol("directory");
    }
  }

  slash(name) {
    return /\/$/.test(name) ? name : name + "/";
  }

  lookup(name) {
    if (!this.content[name] && this.lookupFun) {
      var res = this.lookupFun(
        caml_string_of_jsstring(this.root),
        caml_string_of_jsstring(name),
      );
      if (res !== 0) {
        this.create_dir_if_needed(name);
        this.content[name] = new MlFakeFile(caml_bytes_of_string(res[1]));
      }
    }
  }

  exists(name, do_not_lookup) {
    // The root of the device exists
    if (name === "") return 1;
    // Check if a directory exists
    var name_slash = this.slash(name);
    if (this.content[name_slash]) return 1;
    // Check if a file exists
    if (!do_not_lookup) this.lookup(name);
    return this.content[name] ? 1 : 0;
  }

  isFile(name) {
    if (this.exists(name) && !this.is_dir(name)) {
      return 1;
    } else {
      return 0;
    }
  }

  mkdir(name, mode, raise_unix) {
    if (this.exists(name))
      caml_raise_system_error(
        raise_unix,
        "EEXIST",
        "mkdir",
        "file already exists",
        this.nm(name),
      );
    var parent = /^(.*)\/[^/]+/.exec(name);
    parent = parent?.[1] || "";
    if (!this.exists(parent))
      caml_raise_system_error(
        raise_unix,
        "ENOENT",
        "mkdir",
        "no such file or directory",
        this.nm(name),
      );
    if (!this.is_dir(parent))
      caml_raise_system_error(
        raise_unix,
        "ENOTDIR",
        "mkdir",
        "not a directory",
        this.nm(name),
      );
    this.create_dir_if_needed(this.slash(name));
  }

  rmdir(name, raise_unix) {
    var name_slash = name === "" ? "" : this.slash(name);
    var r = new RegExp("^" + name_slash + "([^/]+)");
    if (!this.exists(name))
      caml_raise_system_error(
        raise_unix,
        "ENOENT",
        "rmdir",
        "no such file or directory",
        this.nm(name),
      );
    if (!this.is_dir(name))
      caml_raise_system_error(
        raise_unix,
        "ENOTDIR",
        "rmdir",
        "not a directory",
        this.nm(name),
      );
    for (var n in this.content) {
      if (n.match(r))
        caml_raise_system_error(
          raise_unix,
          "ENOTEMPTY",
          "rmdir",
          "directory not empty",
          this.nm(name),
        );
    }
    delete this.content[name_slash];
  }

  readdir(name) {
    var name_slash = name === "" ? "" : this.slash(name);
    if (!this.exists(name)) {
      caml_raise_sys_error(name + ": No such file or directory");
    }
    if (!this.is_dir(name)) {
      caml_raise_sys_error(name + ": Not a directory");
    }
    var r = new RegExp("^" + name_slash + "([^/]+)");
    var seen = {};
    var a = [];
    for (var n in this.content) {
      var m = n.match(r);
      if (m && !seen[m[1]]) {
        seen[m[1]] = true;
        a.push(m[1]);
      }
    }
    return a;
  }

  opendir(name, raise_unix) {
    var a = this.readdir(name);
    var c = false;
    var i = 0;
    return {
      readSync: function () {
        if (c)
          caml_raise_system_error(
            raise_unix,
            "EBADF",
            "readdir",
            "bad file descriptor",
          );
        if (i === a.length) return null;
        var entry = a[i];
        i++;
        return { name: entry };
      },
      closeSync: function () {
        if (c)
          caml_raise_system_error(
            raise_unix,
            "EBADF",
            "readdir",
            "bad file descriptor",
          );
        c = true;
        a = [];
      },
    };
  }

  is_dir(name) {
    if (name === "") return true;
    var name_slash = this.slash(name);
    return this.content[name_slash] ? 1 : 0;
  }

  unlink(name, raise_unix) {
    if (!this.exists(name, true)) {
      // [true] means no "lookup" if not found.
      caml_raise_system_error(
        raise_unix,
        "ENOENT",
        "unlink",
        "no such file or directory",
        name,
      );
    }
    delete this.content[name];
    return 0;
  }

  access(name, f, raise_unix) {
    var file;
    this.lookup(name);
    if (this.content[name]) {
      if (this.is_dir(name))
        caml_raise_system_error(
          raise_unix,
          "EACCESS",
          "access",
          "permission denied,",
          this.nm(name),
        );
    } else {
      caml_raise_no_such_file(this.nm(name), raise_unix);
    }
    return 0;
  }

  open(name, f, _perms, raise_unix) {
    var file;
    this.lookup(name);
    if (this.content[name]) {
      if (this.is_dir(name))
        caml_raise_system_error(
          raise_unix,
          "EISDIR",
          "open",
          "illegal operation on a directory",
          this.nm(name),
        );
      if (f.create && f.excl)
        caml_raise_system_error(
          raise_unix,
          "EEXIST",
          "open",
          "file already exists",
          this.nm(name),
        );
      file = this.content[name];
      if (f.truncate) file.truncate();
    } else if (f.create) {
      this.create_dir_if_needed(name);
      this.content[name] = new MlFakeFile(caml_create_bytes(0));
      file = this.content[name];
    } else {
      caml_raise_no_such_file(this.nm(name), raise_unix);
    }
    return new MlFakeFd(this.nm(name), file, f);
  }

  truncate(name, len, raise_unix) {
    var file;
    this.lookup(name);
    if (this.content[name]) {
      if (this.is_dir(name))
        caml_raise_system_error(
          raise_unix,
          "EISDIR",
          "open",
          "illegal operation on a directory",
          this.nm(name),
        );
      file = this.content[name];
      file.truncate(len);
    } else {
      caml_raise_no_such_file(this.nm(name), raise_unix);
    }
  }

  register(name, content) {
    var file;
    if (this.content[name])
      caml_raise_sys_error(this.nm(name) + " : file already exists");
    if (caml_is_ml_bytes(content)) file = new MlFakeFile(content);
    if (caml_is_ml_string(content))
      file = new MlFakeFile(caml_bytes_of_string(content));
    else if (Array.isArray(content))
      file = new MlFakeFile(caml_bytes_of_array(content));
    else if (typeof content === "string")
      file = new MlFakeFile(caml_bytes_of_jsbytes(content));
    else if (content.toString) {
      var bytes = caml_bytes_of_string(
        caml_string_of_jsstring(content.toString()),
      );
      file = new MlFakeFile(bytes);
    }
    if (file) {
      this.create_dir_if_needed(name);
      this.content[name] = file;
    } else
      caml_raise_sys_error(
        this.nm(name) + " : registering file with invalid content type",
      );
  }
}

//Provides: MlFakeFile
//Requires: MlFile
//Requires: caml_create_bytes, caml_ml_bytes_length, caml_blit_bytes
//Requires: caml_uint8_array_of_bytes, caml_bytes_of_uint8_array
class MlFakeFile extends MlFile {
  constructor(content) {
    super();
    this.data = content;
  }

  truncate(len) {
    var old = this.data;
    this.data = caml_create_bytes(len | 0);
    caml_blit_bytes(old, 0, this.data, 0, len);
  }

  length() {
    return caml_ml_bytes_length(this.data);
  }

  write(offset, buf, pos, len) {
    var clen = this.length();
    if (offset + len >= clen) {
      var new_str = caml_create_bytes(offset + len);
      var old_data = this.data;
      this.data = new_str;
      caml_blit_bytes(old_data, 0, this.data, 0, clen);
    }
    caml_blit_bytes(
      caml_bytes_of_uint8_array(buf),
      pos,
      this.data,
      offset,
      len,
    );
    return len;
  }

  read(offset, buf, pos, len) {
    var clen = this.length();
    if (offset + len >= clen) {
      len = clen - offset;
    }
    if (len) {
      var data = caml_create_bytes(len | 0);
      caml_blit_bytes(this.data, offset, data, 0, len);
      buf.set(caml_uint8_array_of_bytes(data), pos);
    }
    return len;
  }
}

//Provides: MlFakeFd_out
//Requires: MlFakeFile, caml_create_bytes, caml_blit_bytes, caml_bytes_of_uint8_array
//Requires: caml_raise_system_error
class MlFakeFd_out extends MlFakeFile {
  constructor(fd, flags) {
    super(caml_create_bytes(0));
    this.log = function (s) {
      return 0;
    };
    if (fd === 1 && typeof console.log === "function") this.log = console.log;
    else if (fd === 2 && typeof console.error === "function")
      this.log = console.error;
    else if (typeof console.log === "function") this.log = console.log;
    this.flags = flags;
  }

  length() {
    return 0;
  }

  truncate(len, raise_unix) {
    caml_raise_system_error(
      raise_unix,
      "EINVAL",
      "ftruncate",
      "invalid argument",
    );
  }

  write(buf, pos, len, raise_unix) {
    var written = len;
    if (this.log) {
      if (
        len > 0 &&
        pos >= 0 &&
        pos + len <= buf.length &&
        buf[pos + len - 1] === 10
      )
        len--;
      // Do not output the last \n if present
      // as console logging display a newline at the end
      var src = caml_create_bytes(len);
      caml_blit_bytes(caml_bytes_of_uint8_array(buf), pos, src, 0, len);
      this.log(src.toUtf16());
      return written;
    }
    caml_raise_system_error(
      raise_unix,
      "EBADF",
      "write",
      "bad file descriptor",
    );
  }

  read(buf, pos, len, raise_unix) {
    caml_raise_system_error(raise_unix, "EBADF", "read", "bad file descriptor");
  }

  seek(len, whence, raise_unix) {
    caml_raise_system_error(raise_unix, "ESPIPE", "lseek", "illegal seek");
  }

  close() {
    this.log = undefined;
  }

  check_stream_semantics(cmd) {}
}

//Provides: MlFakeFd
//Requires: MlFakeFile
//Requires: caml_raise_system_error
class MlFakeFd {
  constructor(name, file, flags) {
    this.file = file;
    this.name = name;
    this.flags = flags;
    this.offset = 0;
    this.seeked = false;
  }

  err_closed(cmd, raise_unix) {
    caml_raise_system_error(raise_unix, "EBADF", cmd, "bad file descriptor");
  }

  length() {
    if (this.file) return this.file.length();
    this.err_closed("length");
  }

  truncate(len, raise_unix) {
    if (this.file) {
      if (!(this.flags.wronly || this.flags.rdwr))
        caml_raise_system_error(
          raise_unix,
          "EINVAL",
          "truncate",
          "invalid argument",
        );
      return this.file.truncate(len);
    }
    this.err_closed("truncate", raise_unix);
  }

  write(buf, pos, len, raise_unix) {
    if (this.file && (this.flags.wronly || this.flags.rdwr)) {
      var offset = this.offset;
      len = this.file.write(offset, buf, pos, len);
      this.offset += len;
      return len;
    }
    this.err_closed("write", raise_unix);
  }

  read(buf, pos, len, raise_unix) {
    if (this.file && !this.flags.wronly) {
      var offset = this.offset;
      len = this.file.read(offset, buf, pos, len);
      this.offset += len;
      return len;
    }
    this.err_closed("read", raise_unix);
  }

  seek(offset, whence, raise_unix) {
    switch (whence) {
      case 0:
        break;
      case 1:
        offset += this.offset;
        break;
      case 2:
        offset += this.length();
        break;
    }
    if (offset < 0)
      caml_raise_system_error(
        raise_unix,
        "EINVAL",
        "lseek",
        "invalid argument",
      );
    this.offset = offset;
    this.seeked = true;
  }

  close() {
    if (!this.file) this.err_closed("close");
    this.file = undefined;
  }

  check_stream_semantics(cmd) {
    if (!this.file) return this.err_closed(cmd, /* raise Unix_error */ 1);
  }
}

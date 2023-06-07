(* Js_of_ocaml tests
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2022 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Util

let%expect_test "static eval of string get" =
  let use_jsoo_exports st =
    let open Js_of_ocaml_compiler in
    let traverse = new Js_traverse.free in
    let _ = traverse#program [ st ] in
    let jsoo_exports =
      Javascript.ident (Stdlib.Utf8_string.of_string_exn "jsoo_exports")
    in
    Javascript.IdentSet.mem jsoo_exports traverse#get_use
    || Javascript.IdentSet.mem jsoo_exports traverse#get_def
  in
  let clean program =
    let clean_statement st =
      let open Js_of_ocaml_compiler.Javascript in
      match st with
      | Function_declaration (name, (k, param, body, loc1)), loc2 -> (
          match List.filter use_jsoo_exports body with
          | [] -> None
          | body -> Some (Function_declaration (name, (k, param, body, loc1)), loc2))
      | ( Expression_statement (ECall (EFun (name, (k, param, body, loc1)), ANormal, a, l))
        , loc ) -> (
          match List.filter use_jsoo_exports body with
          | [] -> None
          | body ->
              Some
                ( Expression_statement
                    (ECall (EFun (name, (k, param, body, loc1)), ANormal, a, l))
                , loc ))
      | _, _ -> Some st
    in
    List.filter_map clean_statement program
  in
  let program =
    compile_and_parse_whole_program
      ~flags:
        [ "--wrap-with-fun"
        ; "Loader"
        ; "--target-env"
        ; "browser"
        ; "--no-extern-fs"
        ; "--enable"
        ; "vardecl"
        ]
      {|
      external pure_js_expr : string -> 'a = "caml_pure_js_expr"
      external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
      let x = 3
      let () = set (pure_js_expr "jsoo_exports") (pure_js_expr "'x'") x  |}
  in
  print_program (clean program);
  [%expect
    {|
    function Loader(globalThis){
     var jsoo_exports = {};
     return caml_register_named_value
             (caml_string_of_jsbytes("Pervasives.do_at_exit"), do_at_exit),
            jsoo_exports["x"] = 3,
            do_at_exit(0),
            jsoo_exports;
    }
    typeof module === "object" && module.exports && (module["exports"] = Loader);
    //end |}];
  let program =
    compile_and_parse_whole_program
      ~flags:
        [ "--wrap-with-fun"
        ; "Loader"
        ; "--target-env"
        ; "browser"
        ; "--no-extern-fs"
        ; "--enable"
        ; "vardecl"
        ]
      {|
      external pure_js_expr : string -> 'a = "caml_pure_js_expr"
      external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
      let x = 3
      let () = if false then set (pure_js_expr "jsoo_exports") (pure_js_expr "'x'") x  |}
  in
  print_program (clean program);
  [%expect
    {|
    function Loader(globalThis){
     var jsoo_exports = {};
     return caml_register_named_value
             (caml_string_of_jsbytes("Pervasives.do_at_exit"), do_at_exit),
            do_at_exit(0),
            jsoo_exports;
    }
    typeof module === "object" && module.exports && (module["exports"] = Loader);
    //end |}];
  let program =
    compile_and_parse_whole_program
      ~flags:[ "--target-env"; "browser"; "--no-extern-fs"; "--enable"; "vardecl" ]
      {|
      external pure_js_expr : string -> 'a = "caml_pure_js_expr"
      external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
      let x = 3
      let () = set (pure_js_expr "jsoo_exports") (pure_js_expr "'x'") x  |}
  in
  print_program (clean program);
  [%expect
    {|
    (function(Object){
       typeof globalThis !== "object"
       &&
        (this
          ? get()
          : (Object.defineProperty
             (Object.prototype, "_T_", {configurable: true, get: get}),
            _T_));
       function get(){
        var global = this || self;
        global.globalThis = global, delete Object.prototype._T_;
       }
      }
      (Object),
     function(globalThis){
       "use strict";
       var
        jsoo_exports = typeof module === "object" && module.exports || globalThis;
       function caml_atomic_load(ref){return ref[1];}
       function caml_call_gen(f, args){
        var
         g,
         n = f.l >= 0 ? f.l : f.l = f.length,
         argsLen = args.length,
         d = n - argsLen;
        if(d == 0)
         return f.apply(null, args);
        else if(d < 0){
         if(g = f.apply(null, args.slice(0, n)), typeof g !== "function")
          return g;
         return caml_call_gen(g, args.slice(n));
        }
        else{
         switch(d){
           case 1:
            {
             g =
              function(x){
               var i, nargs = new Array(argsLen + 1);
               for(i = 0; i < argsLen; i++) nargs[i] = args[i];
               return nargs[argsLen] = x, f.apply(null, nargs);
              };
             break;
            }
           case 2:
            {
             g =
              function(x, y){
               var i, nargs = new Array(argsLen + 2);
               for(i = 0; i < argsLen; i++) nargs[i] = args[i];
               return nargs[argsLen] = x,
                      nargs[argsLen + 1] = y,
                      f.apply(null, nargs);
              };
             break;
            }
           default:
            g =
             function(){
              var
               i,
               extra_args = arguments.length == 0 ? 1 : arguments.length,
               nargs = new Array(args.length + extra_args);
              for(i = 0; i < args.length; i++) nargs[i] = args[i];
              for(i = 0; i < arguments.length; i++)
               nargs[args.length + i] = arguments[i];
              return caml_call_gen(f, nargs);
             };
         }
         return g.l = d, g;
        }
       }
       function jsoo_sys_getenv(n){
        var process = globalThis.process;
        if(process && process.env && process.env[n] != undefined)
         return process.env[n];
        if(globalThis.jsoo_static_env && globalThis.jsoo_static_env[n])
         return globalThis.jsoo_static_env[n];
       }
       var caml_record_backtrace_flag = 0;
       (function(){
          var i, l, r = jsoo_sys_getenv("OCAMLRUNPARAM");
          if(r !== undefined){
           l = r.split(",");
           for(i = 0; i < l.length; i++)
            if(l[i] == "b"){
             caml_record_backtrace_flag = 1;
             break;
            }
            else if(l[i].startsWith("b="))
             caml_record_backtrace_flag = + l[i].slice(2);
            else
             continue;
          }
         }
         ());
       function caml_exn_with_js_backtrace(exn, force){
        (! exn.js_error || force || exn[0] == 248)
        &&
         (exn.js_error =
          new globalThis.Error("Js exception containing backtrace"));
        return exn;
       }
       function caml_maybe_attach_backtrace(exn, force){
        return caml_record_backtrace_flag
                ? caml_exn_with_js_backtrace(exn, force)
                : exn;
       }
       function caml_subarray_to_jsbytes(a, i, len){
        var f = String.fromCharCode;
        if(i == 0 && len <= 4096 && len == a.length) return f.apply(null, a);
        var s = "";
        for(; 0 < len; i += 1024, len -= 1024)
         s += f.apply(null, a.slice(i, i + Math.min(len, 1024)));
        return s;
       }
       function caml_raise_with_arg(tag, arg){
        throw caml_maybe_attach_backtrace([0, tag, arg]);
       }
       function caml_str_repeat(n, s){
        if(n == 0) return "";
        if(s.repeat) return s.repeat(n);
        var r = "", l = 0;
        for(;;){
         n & 1 && (r += s);
         if(n >>= 1, n == 0) return r;
         s += s, l++, l == 9 && s.slice(0, 1);
        }
       }
       function caml_convert_string_to_bytes(s){
        s.t == 2
         ? s.c += caml_str_repeat(s.l - s.c.length, "\0")
         : s.c = caml_subarray_to_jsbytes(s.c, 0, s.c.length);
        s.t = 0;
       }
       function jsoo_is_ascii(s){
        var i;
        if(s.length < 24){
         for(i = 0; i < s.length; i++) if(s.charCodeAt(i) > 127) return false;
         return true;
        }
        else
         return ! /[^\x00-\x7f]/.test(s);
       }
       function caml_utf16_of_utf8(s){
        var v, t, l, j, i, c2, c1, c, b;
        for(b = "", t = "", i = 0, l = s.length; i < l; i++){
         if(c1 = s.charCodeAt(i), c1 < 0x80){
          for(j = i + 1; j < l && (c1 = s.charCodeAt(j)) < 0x80; j++) ;
          j - i > 512
           ? (t.substr(0, 1), b += t, t = "", b += s.slice(i, j))
           : t += s.slice(i, j);
          if(j == l) break;
          i = j;
         }
         v = 1,
         ++i < l && ((c2 = s.charCodeAt(i)) & - 64) == 128
         &&
          (c = c2 + (c1 << 6),
           c1 < 0xe0
            ? (v = c - 0x3080, v < 0x80 && (v = 1))
            : (v
              = 2,
              ++i < l && ((c2 = s.charCodeAt(i)) & - 64) == 128
              &&
               (c = c2 + (c << 6),
                c1 < 0xf0
                 ? (v
                   = c - 0xe2080,
                   (v < 0x800 || v >= 0xd7ff && v < 0xe000) && (v = 2))
                 : (v
                   = 3,
                   ++i < l && ((c2 = s.charCodeAt(i)) & - 64) == 128 && c1 < 0xf5
                   &&
                    (v = c2 - 0x3c82080 + (c << 6),
                     (v < 0x10000 || v > 0x10ffff) && (v = 3))))));
         if(v < 4)
          i -= v, t += "\ufffd";
         else if(v > 0xffff)
          t += String.fromCharCode(0xd7c0 + (v >> 10), 0xdc00 + (v & 0x3FF));
         else
          t += String.fromCharCode(v);
         t.length > 1024 && (t.substr(0, 1), b += t, t = "");
        }
        return b + t;
       }
       function MlBytes(tag, contents, length){
        this.t = tag, this.c = contents, this.l = length;
       }
       var caml_global_data = [0];
       MlBytes.prototype.toString =
        function(){
         switch(this.t){
           case 9:
            return this.c;
           default: caml_convert_string_to_bytes(this);
           case 0:
            if(jsoo_is_ascii(this.c)) return this.t = 9, this.c; this.t = 8;
           case 8:
            return this.c;
         }
        },
       MlBytes.prototype.toUtf16 =
        function(){
         var r = this.toString();
         if(this.t == 9) return r;
         return caml_utf16_of_utf8(r);
        },
       MlBytes.prototype.slice =
        function(){
         var content = this.t == 4 ? this.c.slice() : this.c;
         return new MlBytes(this.t, content, this.l);
        };
       function caml_bytes_of_jsbytes(s){return new MlBytes(0, s, s.length);}
       function caml_string_of_jsbytes(s){return caml_bytes_of_jsbytes(s);}
       function caml_raise_with_string(tag, msg){
        caml_raise_with_arg(tag, caml_string_of_jsbytes(msg));
       }
       function caml_raise_sys_error(msg){
        caml_raise_with_string(caml_global_data.Sys_error, msg);
       }
       function caml_ml_flush(chanid){
        var chan = caml_ml_channels[chanid];
        chan.opened || caml_raise_sys_error("Cannot flush a closed channel");
        if(! chan.buffer || chan.buffer_curr == 0) return 0;
        chan.output
         ? chan.output(caml_subarray_to_jsbytes(chan.buffer, 0, chan.buffer_curr))
         : chan.file.write(chan.offset, chan.buffer, 0, chan.buffer_curr);
        return chan.offset += chan.buffer_curr, chan.buffer_curr = 0, 0;
       }
       function caml_sys_open_for_node(fd, flags){return null;}
       function fs_node_supported(){return false;}
       function caml_jsbytes_of_string(s){
        return s.t & 6 && caml_convert_string_to_bytes(s), s.c;
       }
       function caml_jsstring_of_string(s){return s.toUtf16();}
       function make_path_is_absolute(){
        function posix(path){
         if(path.charAt(0) === "/") return ["", path.substring(1)];
         return;
        }
        function win32(path){
         var
          root,
          sep,
          splitDeviceRe =
            /^([a-zA-Z]:|[\\/]{2}[^\\/]+[\\/]+[^\\/]+)?([\\/])?([\s\S]*?)$/,
          result = splitDeviceRe.exec(path),
          device = result[1] || "",
          isUnc = Boolean(device && device.charAt(1) !== ":");
         if(Boolean(result[2] || isUnc))
          return root = result[1] || "",
                 sep = result[2] || "",
                 [root, path.substring(root.length + sep.length)];
         return;
        }
        return fs_node_supported() && globalThis.process
                && globalThis.process.platform
                ? globalThis.process.platform === "win32" ? win32 : posix
                : posix;
       }
       function caml_trailing_slash(name){
        return name.slice(- 1) !== "/" ? name + "/" : name;
       }
       var
        caml_ml_channels = new Array(),
        caml_sys_fds = new Array(3),
        path_is_absolute = make_path_is_absolute();
       fs_node_supported() && globalThis.process && globalThis.process.cwd
        ? caml_current_dir = globalThis.process.cwd().replace(/\\/g, "/")
        : caml_current_dir = "/static";
       function caml_make_path(name){
        var i;
        name = caml_jsstring_of_string(name),
        path_is_absolute(name) || (name = caml_current_dir + name);
        var
         comp0 = path_is_absolute(name),
         comp = comp0[1].split("/"),
         ncomp = [];
        for(i = 0; i < comp.length; i++)
         switch(comp[i]){
           case "..":
            ncomp.length > 1 && ncomp.pop(); break;
           case ".": break;
           case "": break;
           default: ncomp.push(comp[i]); break;
         }
        return ncomp.unshift(comp0[0]), ncomp.orig = name, ncomp;
       }
       function caml_utf8_of_utf16(s){
        var t, l, j, i, d, c, b;
        for(b = "", t = b, i = 0, l = s.length; i < l; i++){
         if(c = s.charCodeAt(i), c < 0x80){
          for(j = i + 1; j < l && (c = s.charCodeAt(j)) < 0x80; j++) ;
          j - i > 512
           ? (t.substr(0, 1), b += t, t = "", b += s.slice(i, j))
           : t += s.slice(i, j);
          if(j == l) break;
          i = j;
         }
         if(c < 0x800)
          t += String.fromCharCode(0xc0 | c >> 6),
          t += String.fromCharCode(0x80 | c & 0x3f);
         else if(c < 0xd800 || c >= 0xdfff)
          t +=
           String.fromCharCode
            (0xe0 | c >> 12, 0x80 | c >> 6 & 0x3f, 0x80 | c & 0x3f);
         else if
          (c >= 0xdbff || i + 1 == l || (d = s.charCodeAt(i + 1)) < 0xdc00
           || d > 0xdfff)
          t += "\xef\xbf\xbd";
         else
          i++,
          c = (c << 10) + d - 0x35fdc00,
          t +=
           String.fromCharCode
            (0xf0 | c >> 18,
             0x80 | c >> 12 & 0x3f,
             0x80 | c >> 6 & 0x3f,
             0x80 | c & 0x3f);
         t.length > 1024 && (t.substr(0, 1), b += t, t = "");
        }
        return b + t;
       }
       function caml_bytes_of_utf16_jsstring(s){
        var tag = 9;
        jsoo_is_ascii(s) || (tag = 8, s = caml_utf8_of_utf16(s));
        return new MlBytes(tag, s, s.length);
       }
       function caml_string_of_jsstring(s){
        return caml_bytes_of_utf16_jsstring(s);
       }
       function make_unix_err_args(code, syscall, path, errno){
        var variant = unix_error.indexOf(code);
        if(variant < 0){errno == null && (errno = - 9999); variant = [0, errno];}
        var
         args =
           [variant,
            caml_string_of_jsstring(syscall || ""),
            caml_string_of_jsstring(path || "")];
        return args;
       }
       function caml_named_value(nm){return caml_named_values[nm];}
       function caml_raise_with_args(tag, args){
        throw caml_maybe_attach_backtrace([0, tag].concat(args));
       }
       function caml_is_ml_bytes(s){return s instanceof MlBytes;}
       function caml_is_ml_string(s){return caml_is_ml_bytes(s);}
       function caml_bytes_of_array(a){
        a instanceof Uint8Array || (a = new Uint8Array(a));
        return new MlBytes(4, a, a.length);
       }
       function caml_bytes_of_string(s){return s;}
       function caml_raise_no_such_file(name){
        caml_raise_sys_error(name + ": No such file or directory");
       }
       function caml_convert_bytes_to_array(s){
        var a = new Uint8Array(s.l), b = s.c, l = b.length, i = 0;
        for(; i < l; i++) a[i] = b.charCodeAt(i);
        for(l = s.l; i < l; i++) a[i] = 0;
        return s.c = a, s.t = 4, a;
       }
       function caml_uint8_array_of_bytes(s){
        s.t != 4 && caml_convert_bytes_to_array(s);
        return s.c;
       }
       function caml_invalid_argument(msg){
        caml_raise_with_string(caml_global_data.Invalid_argument, msg);
       }
       function caml_create_bytes(len){
        len < 0 && caml_invalid_argument("Bytes.create");
        return new MlBytes(len ? 2 : 9, "", len);
       }
       function caml_ml_bytes_length(s){return s.l;}
       function caml_blit_bytes(s1, i1, s2, i2, len){
        var l, i, c2, c1;
        if(len == 0) return 0;
        if(i2 == 0 && (len >= s2.l || s2.t == 2 && len >= s2.c.length))
         s2.c =
          s1.t == 4
           ? caml_subarray_to_jsbytes(s1.c, i1, len)
           : i1 == 0 && s1.c.length == len ? s1.c : s1.c.substr(i1, len),
         s2.t = s2.c.length == s2.l ? 0 : 2;
        else if(s2.t == 2 && i2 == s2.c.length)
         s2.c +=
          s1.t == 4
           ? caml_subarray_to_jsbytes(s1.c, i1, len)
           : i1 == 0 && s1.c.length == len ? s1.c : s1.c.substr(i1, len),
         s2.t = s2.c.length == s2.l ? 0 : 2;
        else{
         s2.t != 4 && caml_convert_bytes_to_array(s2);
         if(c1 = s1.c, c2 = s2.c, s1.t == 4)
          if(i2 <= i1)
           for(i = 0; i < len; i++) c2[i2 + i] = c1[i1 + i];
          else
           for(i = len - 1; i >= 0; i--) c2[i2 + i] = c1[i1 + i];
         else{
          l = Math.min(len, c1.length - i1);
          for(i = 0; i < l; i++) c2[i2 + i] = c1.charCodeAt(i1 + i);
          for(; i < len; i++) c2[i2 + i] = 0;
         }
        }
        return 0;
       }
       function MlFile(){}
       function MlFakeFile(content){this.data = content;}
       var
        caml_current_dir = caml_trailing_slash(caml_current_dir),
        unix_error =
          ["E2BIG",
           "EACCES",
           "EAGAIN",
           "EBADF",
           "EBUSY",
           "ECHILD",
           "EDEADLK",
           "EDOM",
           "EEXIST",
           "EFAULT",
           "EFBIG",
           "EINTR",
           "EINVAL",
           "EIO",
           "EISDIR",
           "EMFILE",
           "EMLINK",
           "ENAMETOOLONG",
           "ENFILE",
           "ENODEV",
           "ENOENT",
           "ENOEXEC",
           "ENOLCK",
           "ENOMEM",
           "ENOSPC",
           "ENOSYS",
           "ENOTDIR",
           "ENOTEMPTY",
           "ENOTTY",
           "ENXIO",
           "EPERM",
           "EPIPE",
           "ERANGE",
           "EROFS",
           "ESPIPE",
           "ESRCH",
           "EXDEV",
           "EWOULDBLOCK",
           "EINPROGRESS",
           "EALREADY",
           "ENOTSOCK",
           "EDESTADDRREQ",
           "EMSGSIZE",
           "EPROTOTYPE",
           "ENOPROTOOPT",
           "EPROTONOSUPPORT",
           "ESOCKTNOSUPPORT",
           "EOPNOTSUPP",
           "EPFNOSUPPORT",
           "EAFNOSUPPORT",
           "EADDRINUSE",
           "EADDRNOTAVAIL",
           "ENETDOWN",
           "ENETUNREACH",
           "ENETRESET",
           "ECONNABORTED",
           "ECONNRESET",
           "ENOBUFS",
           "EISCONN",
           "ENOTCONN",
           "ESHUTDOWN",
           "ETOOMANYREFS",
           "ETIMEDOUT",
           "ECONNREFUSED",
           "EHOSTDOWN",
           "EHOSTUNREACH",
           "ELOOP",
           "EOVERFLOW"],
        caml_named_values = {};
       MlFakeFile.prototype = new MlFile(),
       MlFakeFile.prototype.constructor = MlFakeFile,
       MlFakeFile.prototype.truncate =
        function(len){
         var old = this.data;
         this.data = caml_create_bytes(len | 0),
         caml_blit_bytes(old, 0, this.data, 0, len);
        },
       MlFakeFile.prototype.length =
        function(){return caml_ml_bytes_length(this.data);},
       MlFakeFile.prototype.write =
        function(offset, buf, pos, len){
         var new_str, old_data, clen = this.length();
         offset + len >= clen
         &&
          (new_str = caml_create_bytes(offset + len),
           old_data = this.data,
           this.data = new_str,
           caml_blit_bytes(old_data, 0, this.data, 0, clen));
         return caml_blit_bytes
                 (caml_bytes_of_array(buf), pos, this.data, offset, len),
                0;
        },
       MlFakeFile.prototype.read =
        function(offset, buf, pos, len){
         var data, clen = this.length();
         offset + len >= clen && (len = clen - offset);
         len
         &&
          (data = caml_create_bytes(len | 0),
           caml_blit_bytes(this.data, offset, data, 0, len),
           buf.set(caml_uint8_array_of_bytes(data), pos));
         return len;
        };
       function MlFakeFd(name, file, flags){
        this.file = file, this.name = name, this.flags = flags;
       }
       MlFakeFd.prototype.err_closed =
        function(){
         caml_raise_sys_error(this.name + ": file descriptor already closed");
        },
       MlFakeFd.prototype.length =
        function(){if(this.file) return this.file.length(); this.err_closed();},
       MlFakeFd.prototype.write =
        function(offset, buf, pos, len){
         if(this.file) return this.file.write(offset, buf, pos, len);
         this.err_closed();
        },
       MlFakeFd.prototype.read =
        function(offset, buf, pos, len){
         if(this.file) return this.file.read(offset, buf, pos, len);
         this.err_closed();
        },
       MlFakeFd.prototype.close = function(){this.file = undefined;};
       function MlFakeDevice(root, f){
        this.content = {}, this.root = root, this.lookupFun = f;
       }
       MlFakeDevice.prototype.nm = function(name){return this.root + name;},
       MlFakeDevice.prototype.create_dir_if_needed =
        function(name){
         var i, comp = name.split("/"), res = "";
         for(i = 0; i < comp.length - 1; i++){
          if(res += comp[i] + "/", this.content[res]) continue;
          this.content[res] = Symbol("directory");
         }
        },
       MlFakeDevice.prototype.slash =
        function(name){return /\/$/.test(name) ? name : name + "/";},
       MlFakeDevice.prototype.lookup =
        function(name){
         var res;
         ! this.content[name] && this.lookupFun
         &&
          (res =
            this.lookupFun
             (caml_string_of_jsbytes(this.root), caml_string_of_jsbytes(name)),
           res !== 0
           &&
            (this.create_dir_if_needed(name),
             this.content[name] = new MlFakeFile(caml_bytes_of_string(res[1]))));
        },
       MlFakeDevice.prototype.exists =
        function(name){
         if(name == "") return 1;
         var name_slash = this.slash(name);
         if(this.content[name_slash]) return 1;
         return this.lookup(name), this.content[name] ? 1 : 0;
        },
       MlFakeDevice.prototype.isFile =
        function(name){return this.exists(name) && ! this.is_dir(name) ? 1 : 0;},
       MlFakeDevice.prototype.mkdir =
        function(name, mode, raise_unix){
         var unix_error = raise_unix && caml_named_value("Unix.Unix_error");
         this.exists(name)
         &&
          (unix_error
            ? caml_raise_with_args
              (unix_error, make_unix_err_args("EEXIST", "mkdir", this.nm(name)))
            : caml_raise_sys_error(name + ": File exists"));
         var parent = /^(.*)\/[^/]+/.exec(name);
         parent = parent && parent[1] || "",
         this.exists(parent)
         ||
          (unix_error
            ? caml_raise_with_args
              (unix_error, make_unix_err_args("ENOENT", "mkdir", this.nm(parent)))
            : caml_raise_sys_error(parent + ": No such file or directory"));
         this.is_dir(parent)
         ||
          (unix_error
            ? caml_raise_with_args
              (unix_error,
               make_unix_err_args("ENOTDIR", "mkdir", this.nm(parent)))
            : caml_raise_sys_error(parent + ": Not a directory"));
         this.create_dir_if_needed(this.slash(name));
        },
       MlFakeDevice.prototype.rmdir =
        function(name, raise_unix){
         var
          n,
          unix_error = raise_unix && caml_named_value("Unix.Unix_error"),
          name_slash = name == "" ? "" : this.slash(name),
          r = new RegExp("^" + name_slash + "([^/]+)");
         this.exists(name)
         ||
          (unix_error
            ? caml_raise_with_args
              (unix_error, make_unix_err_args("ENOENT", "rmdir", this.nm(name)))
            : caml_raise_sys_error(name + ": No such file or directory"));
         this.is_dir(name)
         ||
          (unix_error
            ? caml_raise_with_args
              (unix_error, make_unix_err_args("ENOTDIR", "rmdir", this.nm(name)))
            : caml_raise_sys_error(name + ": Not a directory"));
         for(var n in this.content)
          n.match(r)
          &&
           (unix_error
             ? caml_raise_with_args
               (unix_error,
                make_unix_err_args("ENOTEMPTY", "rmdir", this.nm(name)))
             : caml_raise_sys_error(this.nm(name) + ": Directory not empty"));
         delete this.content[name_slash];
        },
       MlFakeDevice.prototype.readdir =
        function(name){
         var m, n, name_slash = name == "" ? "" : this.slash(name);
         this.exists(name)
         || caml_raise_sys_error(name + ": No such file or directory");
         this.is_dir(name) || caml_raise_sys_error(name + ": Not a directory");
         var r = new RegExp("^" + name_slash + "([^/]+)"), seen = {}, a = [];
         for(var n in this.content)
          m = n.match(r), m && ! seen[m[1]] && (seen[m[1]] = true, a.push(m[1]));
         return a;
        },
       MlFakeDevice.prototype.opendir =
        function(name, raise_unix){
         var
          unix_error = raise_unix && caml_named_value("Unix.Unix_error"),
          a = this.readdir(name),
          c = false,
          i = 0;
         return {readSync:
                 function(){
                  c
                  &&
                   (unix_error
                     ? caml_raise_with_args
                       (unix_error,
                        make_unix_err_args("EBADF", "closedir", this.nm(name)))
                     : caml_raise_sys_error(name + ": closedir failed"));
                  if(i == a.length) return null;
                  var entry = a[i];
                  return i++, {name: entry};
                 },
                 closeSync:
                 function(){
                  c
                  &&
                   (unix_error
                     ? caml_raise_with_args
                       (unix_error,
                        make_unix_err_args("EBADF", "closedir", this.nm(name)))
                     : caml_raise_sys_error(name + ": closedir failed"));
                  c = true, a = [];
                 }};
        },
       MlFakeDevice.prototype.is_dir =
        function(name){
         if(name == "") return true;
         var name_slash = this.slash(name);
         return this.content[name_slash] ? 1 : 0;
        },
       MlFakeDevice.prototype.unlink =
        function(name){
         var ok = this.content[name] ? true : false;
         return delete this.content[name], ok;
        },
       MlFakeDevice.prototype.open =
        function(name, f){
         var file;
         f.rdonly && f.wronly
         &&
          caml_raise_sys_error
           (this.nm(name)
            + " : flags Open_rdonly and Open_wronly are not compatible");
         f.text && f.binary
         &&
          caml_raise_sys_error
           (this.nm(name)
            + " : flags Open_text and Open_binary are not compatible");
         if(this.lookup(name), this.content[name]){
          this.is_dir(name)
          && caml_raise_sys_error(this.nm(name) + " : is a directory");
          f.create && f.excl
          && caml_raise_sys_error(this.nm(name) + " : file already exists");
          file = this.content[name], f.truncate && file.truncate();
         }
         else if(f.create)
          this.create_dir_if_needed(name),
          this.content[name] = new MlFakeFile(caml_create_bytes(0)),
          file = this.content[name];
         else
          caml_raise_no_such_file(this.nm(name));
         return new MlFakeFd(this.nm(name), file, f);
        },
       MlFakeDevice.prototype.open =
        function(name, f){
         var file;
         f.rdonly && f.wronly
         &&
          caml_raise_sys_error
           (this.nm(name)
            + " : flags Open_rdonly and Open_wronly are not compatible");
         f.text && f.binary
         &&
          caml_raise_sys_error
           (this.nm(name)
            + " : flags Open_text and Open_binary are not compatible");
         if(this.lookup(name), this.content[name]){
          this.is_dir(name)
          && caml_raise_sys_error(this.nm(name) + " : is a directory");
          f.create && f.excl
          && caml_raise_sys_error(this.nm(name) + " : file already exists");
          file = this.content[name], f.truncate && file.truncate();
         }
         else if(f.create)
          this.create_dir_if_needed(name),
          this.content[name] = new MlFakeFile(caml_create_bytes(0)),
          file = this.content[name];
         else
          caml_raise_no_such_file(this.nm(name));
         return new MlFakeFd(this.nm(name), file, f);
        },
       MlFakeDevice.prototype.register =
        function(name, content){
         var file, bytes;
         this.content[name]
         && caml_raise_sys_error(this.nm(name) + " : file already exists");
         caml_is_ml_bytes(content) && (file = new MlFakeFile(content));
         if(caml_is_ml_string(content))
          file = new MlFakeFile(caml_bytes_of_string(content));
         else if(content instanceof Array)
          file = new MlFakeFile(caml_bytes_of_array(content));
         else if(typeof content === "string")
          file = new MlFakeFile(caml_bytes_of_jsbytes(content));
         else if(content.toString)
          bytes =
           caml_bytes_of_string(caml_string_of_jsstring(content.toString())),
          file = new MlFakeFile(bytes);
         file
          ? (this.create_dir_if_needed(name), this.content[name] = file)
          : caml_raise_sys_error
            (this.nm(name) + " : registering file with invalid content type");
        },
       MlFakeDevice.prototype.constructor = MlFakeDevice;
       function MlNodeDevice(){}
       function caml_get_root(path){
        var x = path_is_absolute(path);
        if(! x) return;
        return x[0] + "/";
       }
       function caml_failwith(msg){
        caml_global_data.Failure
        ||
         (caml_global_data.Failure =
          [248, caml_string_of_jsbytes("Failure"), - 3]);
        caml_raise_with_string(caml_global_data.Failure, msg);
       }
       var
        caml_root =
          caml_get_root(caml_current_dir)
          || caml_failwith("unable to compute caml_root"),
        jsoo_mount_point = [];
       fs_node_supported()
        ? jsoo_mount_point.push
          ({path: caml_root, device: new MlNodeDevice(caml_root)})
        : jsoo_mount_point.push
          ({path: caml_root, device: new MlFakeDevice(caml_root)});
       jsoo_mount_point.push
        ({path: "/static/", device: new MlFakeDevice("/static/")});
       function resolve_fs_device(name){
        var
         i,
         m,
         res,
         root,
         path = caml_make_path(name),
         name_slash = (name = path.join("/"), caml_trailing_slash(name));
        for(i = 0; i < jsoo_mount_point.length; i++)
         m = jsoo_mount_point[i],
         name_slash.search(m.path) == 0
         && (! res || res.path.length < m.path.length)
         &&
          (res =
           {path: m.path,
            device: m.device,
            rest: name.substring(m.path.length, name.length)});
        ! res && fs_node_supported()
        &&
         (root = caml_get_root(name),
          root && root.match(/^[a-zA-Z]:\/$/)
          &&
           (m = {path: root, device: new MlNodeDevice(root)},
            jsoo_mount_point.push(m),
            res =
             {path: m.path,
              device: m.device,
              rest: name.substring(m.path.length, name.length)}));
        if(res) return res;
        caml_raise_sys_error("no device found for " + name_slash);
       }
       function MlFakeFd_out(fd, flags){
        if
         (MlFakeFile.call(this, caml_create_bytes(0)),
          this.log = function(s){return 0;},
          fd == 1 && typeof console.log == "function")
         this.log = console.log;
        else if(fd == 2 && typeof console.error == "function")
         this.log = console.error;
        else if(typeof console.log == "function") this.log = console.log;
        this.flags = flags;
       }
       MlFakeFd_out.prototype.length = function(){return 0;},
       MlFakeFd_out.prototype.write =
        function(offset, buf, pos, len){
         var src;
         if(this.log){
          len > 0 && pos >= 0 && pos + len <= buf.length
          && buf[pos + len - 1] == 10
          && len--;
          return src = caml_create_bytes(len),
                 caml_blit_bytes(caml_bytes_of_array(buf), pos, src, 0, len),
                 this.log(src.toUtf16()),
                 0;
         }
         caml_raise_sys_error(this.fd + ": file descriptor already closed");
        },
       MlFakeFd_out.prototype.read =
        function(offset, buf, pos, len){
         caml_raise_sys_error(this.fd + ": file descriptor is write only");
        },
       MlFakeFd_out.prototype.close = function(){this.log = undefined;};
       function caml_sys_open_internal(file, idx){
        idx == undefined && (idx = caml_sys_fds.length);
        return caml_sys_fds[idx] = file, idx | 0;
       }
       function caml_sys_open(name, flags, _perms){
        var f = {};
        while(flags){
         switch(flags[1]){
           case 0:
            f.rdonly = 1; break;
           case 1:
            f.wronly = 1; break;
           case 2:
            f.append = 1; break;
           case 3:
            f.create = 1; break;
           case 4:
            f.truncate = 1; break;
           case 5:
            f.excl = 1; break;
           case 6:
            f.binary = 1; break;
           case 7:
            f.text = 1; break;
           case 8:
            f.nonblock = 1; break;
         }
         flags = flags[2];
        }
        f.rdonly && f.wronly
        &&
         caml_raise_sys_error
          (caml_jsbytes_of_string(name)
           + " : flags Open_rdonly and Open_wronly are not compatible");
        f.text && f.binary
        &&
         caml_raise_sys_error
          (caml_jsbytes_of_string(name)
           + " : flags Open_text and Open_binary are not compatible");
        var root = resolve_fs_device(name), file = root.device.open(root.rest, f);
        return caml_sys_open_internal(file, undefined);
       }
       (function(){
          function file(fd, flags){
           return fs_node_supported()
                   ? caml_sys_open_for_node(fd, flags)
                   : new MlFakeFd_out(fd, flags);
          }
          caml_sys_open_internal
           (file(0, {rdonly: 1, altname: "/dev/stdin", isCharacterDevice: true}),
            0),
          caml_sys_open_internal
           (file(1, {buffered: 2, wronly: 1, isCharacterDevice: true}), 1),
          caml_sys_open_internal
           (file(2, {buffered: 2, wronly: 1, isCharacterDevice: true}), 2);
         }
         ());
       function caml_ml_open_descriptor_in(fd){
        var file = caml_sys_fds[fd];
        file.flags.wronly && caml_raise_sys_error("fd " + fd + " is writeonly");
        var
         refill = null,
         channel =
           {file: file,
            offset: file.flags.append ? file.length() : 0,
            fd: fd,
            opened: true,
            out: false,
            buffer_curr: 0,
            buffer_max: 0,
            buffer: new Uint8Array(65536),
            refill: refill};
        return caml_ml_channels[channel.fd] = channel, channel.fd;
       }
       function caml_ml_open_descriptor_out(fd){
        var file = caml_sys_fds[fd];
        file.flags.rdonly && caml_raise_sys_error("fd " + fd + " is readonly");
        var
         buffered = file.flags.buffered !== undefined ? file.flags.buffered : 1,
         channel =
           {file: file,
            offset: file.flags.append ? file.length() : 0,
            fd: fd,
            opened: true,
            out: true,
            buffer_curr: 0,
            buffer: new Uint8Array(65536),
            buffered: buffered};
        return caml_ml_channels[channel.fd] = channel, channel.fd;
       }
       function caml_ml_out_channels_list(){
        var c, l = 0;
        for(c = 0; c < caml_ml_channels.length; c++)
         caml_ml_channels[c] && caml_ml_channels[c].opened
         && caml_ml_channels[c].out
         && (l = [0, caml_ml_channels[c].fd, l]);
        return l;
       }
       function caml_build_symbols(toc){
        var symb, i;
        while(toc)
         if(caml_jsstring_of_string(toc[1][1]) == "SYJS"){symb = toc[1][2]; break;}
         else
          toc = toc[2];
        var r = {};
        if(symb)
         for(i = 1; i < symb.length; i++)
          r[caml_jsstring_of_string(symb[i][1])] = symb[i][2];
        return r;
       }
       function caml_register_global(n, v, name_opt){
        var nid, name;
        if(name_opt)
         if(name = name_opt, globalThis.toplevelReloc)
          n = caml_callback(globalThis.toplevelReloc, [name]);
         else if(caml_global_data.toc){
          caml_global_data.symbols
          || (caml_global_data.symbols = caml_build_symbols(caml_global_data.toc));
          nid = caml_global_data.symbols[name],
          nid >= 0
           ? n = nid
           : caml_failwith("caml_register_global: cannot locate " + name);
         }
        caml_global_data[n + 1] = v, name_opt && (caml_global_data[name_opt] = v);
       }
       function caml_register_named_value(nm, v){
        return caml_named_values[caml_jsbytes_of_string(nm)] = v, 0;
       }
       function caml_wrap_exception(e){
        var exn;
        {
         if(e instanceof Array) return e;
         if
          (globalThis.RangeError && e instanceof globalThis.RangeError
           && e.message
           && e.message.match(/maximum call stack/i))
          exn = caml_global_data.Stack_overflow;
         else if
          (globalThis.InternalError && e instanceof globalThis.InternalError
           && e.message
           && e.message.match(/too much recursion/i))
          exn = caml_global_data.Stack_overflow;
         else if(e instanceof globalThis.Error && caml_named_value("jsError"))
          exn = [0, caml_named_value("jsError"), e];
         else
          exn = [0, caml_global_data.Failure, caml_string_of_jsstring(String(e))];
         e instanceof globalThis.Error && (exn.js_error = e);
         return exn;
        }
       }
       function caml_is_special_exception(exn){
        switch(exn[2]){case - 8:case - 11:case - 12: return 1;default: return 0;
        }
       }
       function caml_format_exception(exn){
        var bucket, i, start, v, r = "";
        if(exn[0] == 0){
         r += exn[1][1],
         exn.length == 3 && exn[2][0] == 0 && caml_is_special_exception(exn[1])
          ? (bucket = exn[2], start = 1)
          : (start = 2, bucket = exn);
         r += "(";
         for(i = start; i < bucket.length; i++){
          i > start && (r += ", ");
          if(v = bucket[i], typeof v == "number")
           r += v.toString();
          else if(v instanceof MlBytes)
           r += '"' + v.toString() + '"';
          else if(typeof v == "string")
           r += '"' + v.toString() + '"';
          else
           r += "_";
         }
         r += ")";
        }
        else if(exn[0] == 248) r += exn[1];
        return r;
       }
       function caml_fatal_uncaught_exception(err){
        var msg, handler, at_exit;
        if(err instanceof Array && (err[0] == 0 || err[0] == 248))
         if
          (handler = caml_named_value("Printexc.handle_uncaught_exception"),
           handler)
          caml_callback(handler, [err, false]);
         else{
          msg = caml_format_exception(err),
          at_exit = caml_named_value("Pervasives.do_at_exit"),
          at_exit && caml_callback(at_exit, [0]);
          if(console.error("Fatal error: exception " + msg + "\n"), err.js_error)
           throw err.js_error;
         }
        else
         throw err;
       }
       function caml_setup_uncaught_exception_handler(){
        var process = globalThis.process;
        if(process && process.on)
         process.on
          ("uncaughtException",
           function(err, origin){
            caml_fatal_uncaught_exception(err), process.exit(2);
           });
        else if(globalThis.addEventListener)
         globalThis.addEventListener
          ("error",
           function(event){
            event.error && caml_fatal_uncaught_exception(event.error);
           });
       }
       var caml_callback = caml_call_gen;
       caml_setup_uncaught_exception_handler();
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) == 1
                ? f(a0)
                : caml_call_gen(f, [a0]);
       }
       var
        Out_of_memory = [248, caml_string_of_jsbytes("Out_of_memory"), - 1],
        Sys_error = [248, caml_string_of_jsbytes("Sys_error"), - 2],
        Failure = [248, caml_string_of_jsbytes("Failure"), - 3],
        Invalid_argument = [248, caml_string_of_jsbytes("Invalid_argument"), - 4],
        End_of_file = [248, caml_string_of_jsbytes("End_of_file"), - 5],
        Division_by_zero = [248, caml_string_of_jsbytes("Division_by_zero"), - 6],
        Not_found = [248, caml_string_of_jsbytes("Not_found"), - 7],
        Match_failure = [248, caml_string_of_jsbytes("Match_failure"), - 8],
        Stack_overflow = [248, caml_string_of_jsbytes("Stack_overflow"), - 9],
        Sys_blocked_io = [248, caml_string_of_jsbytes("Sys_blocked_io"), - 10],
        Assert_failure = [248, caml_string_of_jsbytes("Assert_failure"), - 11],
        Undefined_recursive_module =
          [248, caml_string_of_jsbytes("Undefined_recursive_module"), - 12],
        flush_all =
          (caml_register_global
            (11, Undefined_recursive_module, "Undefined_recursive_module"),
           caml_register_global(10, Assert_failure, "Assert_failure"),
           caml_register_global(9, Sys_blocked_io, "Sys_blocked_io"),
           caml_register_global(8, Stack_overflow, "Stack_overflow"),
           caml_register_global(7, Match_failure, "Match_failure"),
           caml_register_global(6, Not_found, "Not_found"),
           caml_register_global(5, Division_by_zero, "Division_by_zero"),
           caml_register_global(4, End_of_file, "End_of_file"),
           caml_register_global(3, Invalid_argument, "Invalid_argument"),
           caml_register_global(2, Failure, "Failure"),
           caml_register_global(1, Sys_error, "Sys_error"),
           caml_register_global(0, Out_of_memory, "Out_of_memory"),
           caml_ml_open_descriptor_in(0),
           caml_ml_open_descriptor_out(1),
           caml_ml_open_descriptor_out(2),
           function(param){
            var l, a, _a_, param$0 = caml_ml_out_channels_list(0);
            for(;;){
             if(! param$0) return 0;
             l = param$0[2], a = param$0[1];
             try{caml_ml_flush(a);}
             catch(_b_){
              if(_a_ = caml_wrap_exception(_b_), _a_[1] !== Sys_error)
               throw caml_maybe_attach_backtrace(_a_, 0);
             }
             param$0 = l;
            }
           }),
        exit_function = [0, flush_all];
       function do_at_exit(param){
        return caml_call1(caml_atomic_load(exit_function), 0);
       }
       caml_register_named_value
        (caml_string_of_jsbytes("Pervasives.do_at_exit"), do_at_exit),
       jsoo_exports["x"] = 3,
       do_at_exit(0);
       return;
      }
      (globalThis));
    //end |}];
  let program =
    compile_and_parse_whole_program
      ~flags:[ "--target-env"; "browser"; "--no-extern-fs"; "--enable"; "vardecl" ]
      {|
      external pure_js_expr : string -> 'a = "caml_pure_js_expr"
      external set : 'a -> 'b -> 'c -> unit = "caml_js_set"
      let x = 3
      let () = if false then set (pure_js_expr "jsoo_exports") (pure_js_expr "'x'") x  |}
  in
  print_program (clean program);
  [%expect {|
    (function(Object){
       typeof globalThis !== "object"
       &&
        (this
          ? get()
          : (Object.defineProperty
             (Object.prototype, "_T_", {configurable: true, get: get}),
            _T_));
       function get(){
        var global = this || self;
        global.globalThis = global, delete Object.prototype._T_;
       }
      }
      (Object),
     function(globalThis){
       "use strict";
       function caml_atomic_load(ref){return ref[1];}
       function caml_call_gen(f, args){
        var
         g,
         n = f.l >= 0 ? f.l : f.l = f.length,
         argsLen = args.length,
         d = n - argsLen;
        if(d == 0)
         return f.apply(null, args);
        else if(d < 0){
         if(g = f.apply(null, args.slice(0, n)), typeof g !== "function")
          return g;
         return caml_call_gen(g, args.slice(n));
        }
        else{
         switch(d){
           case 1:
            {
             g =
              function(x){
               var i, nargs = new Array(argsLen + 1);
               for(i = 0; i < argsLen; i++) nargs[i] = args[i];
               return nargs[argsLen] = x, f.apply(null, nargs);
              };
             break;
            }
           case 2:
            {
             g =
              function(x, y){
               var i, nargs = new Array(argsLen + 2);
               for(i = 0; i < argsLen; i++) nargs[i] = args[i];
               return nargs[argsLen] = x,
                      nargs[argsLen + 1] = y,
                      f.apply(null, nargs);
              };
             break;
            }
           default:
            g =
             function(){
              var
               i,
               extra_args = arguments.length == 0 ? 1 : arguments.length,
               nargs = new Array(args.length + extra_args);
              for(i = 0; i < args.length; i++) nargs[i] = args[i];
              for(i = 0; i < arguments.length; i++)
               nargs[args.length + i] = arguments[i];
              return caml_call_gen(f, nargs);
             };
         }
         return g.l = d, g;
        }
       }
       function jsoo_sys_getenv(n){
        var process = globalThis.process;
        if(process && process.env && process.env[n] != undefined)
         return process.env[n];
        if(globalThis.jsoo_static_env && globalThis.jsoo_static_env[n])
         return globalThis.jsoo_static_env[n];
       }
       var caml_record_backtrace_flag = 0;
       (function(){
          var i, l, r = jsoo_sys_getenv("OCAMLRUNPARAM");
          if(r !== undefined){
           l = r.split(",");
           for(i = 0; i < l.length; i++)
            if(l[i] == "b"){
             caml_record_backtrace_flag = 1;
             break;
            }
            else if(l[i].startsWith("b="))
             caml_record_backtrace_flag = + l[i].slice(2);
            else
             continue;
          }
         }
         ());
       function caml_exn_with_js_backtrace(exn, force){
        (! exn.js_error || force || exn[0] == 248)
        &&
         (exn.js_error =
          new globalThis.Error("Js exception containing backtrace"));
        return exn;
       }
       function caml_maybe_attach_backtrace(exn, force){
        return caml_record_backtrace_flag
                ? caml_exn_with_js_backtrace(exn, force)
                : exn;
       }
       function caml_subarray_to_jsbytes(a, i, len){
        var f = String.fromCharCode;
        if(i == 0 && len <= 4096 && len == a.length) return f.apply(null, a);
        var s = "";
        for(; 0 < len; i += 1024, len -= 1024)
         s += f.apply(null, a.slice(i, i + Math.min(len, 1024)));
        return s;
       }
       function caml_raise_with_arg(tag, arg){
        throw caml_maybe_attach_backtrace([0, tag, arg]);
       }
       function caml_str_repeat(n, s){
        if(n == 0) return "";
        if(s.repeat) return s.repeat(n);
        var r = "", l = 0;
        for(;;){
         n & 1 && (r += s);
         if(n >>= 1, n == 0) return r;
         s += s, l++, l == 9 && s.slice(0, 1);
        }
       }
       function caml_convert_string_to_bytes(s){
        s.t == 2
         ? s.c += caml_str_repeat(s.l - s.c.length, "\0")
         : s.c = caml_subarray_to_jsbytes(s.c, 0, s.c.length);
        s.t = 0;
       }
       function jsoo_is_ascii(s){
        var i;
        if(s.length < 24){
         for(i = 0; i < s.length; i++) if(s.charCodeAt(i) > 127) return false;
         return true;
        }
        else
         return ! /[^\x00-\x7f]/.test(s);
       }
       function caml_utf16_of_utf8(s){
        var v, t, l, j, i, c2, c1, c, b;
        for(b = "", t = "", i = 0, l = s.length; i < l; i++){
         if(c1 = s.charCodeAt(i), c1 < 0x80){
          for(j = i + 1; j < l && (c1 = s.charCodeAt(j)) < 0x80; j++) ;
          j - i > 512
           ? (t.substr(0, 1), b += t, t = "", b += s.slice(i, j))
           : t += s.slice(i, j);
          if(j == l) break;
          i = j;
         }
         v = 1,
         ++i < l && ((c2 = s.charCodeAt(i)) & - 64) == 128
         &&
          (c = c2 + (c1 << 6),
           c1 < 0xe0
            ? (v = c - 0x3080, v < 0x80 && (v = 1))
            : (v
              = 2,
              ++i < l && ((c2 = s.charCodeAt(i)) & - 64) == 128
              &&
               (c = c2 + (c << 6),
                c1 < 0xf0
                 ? (v
                   = c - 0xe2080,
                   (v < 0x800 || v >= 0xd7ff && v < 0xe000) && (v = 2))
                 : (v
                   = 3,
                   ++i < l && ((c2 = s.charCodeAt(i)) & - 64) == 128 && c1 < 0xf5
                   &&
                    (v = c2 - 0x3c82080 + (c << 6),
                     (v < 0x10000 || v > 0x10ffff) && (v = 3))))));
         if(v < 4)
          i -= v, t += "\ufffd";
         else if(v > 0xffff)
          t += String.fromCharCode(0xd7c0 + (v >> 10), 0xdc00 + (v & 0x3FF));
         else
          t += String.fromCharCode(v);
         t.length > 1024 && (t.substr(0, 1), b += t, t = "");
        }
        return b + t;
       }
       function MlBytes(tag, contents, length){
        this.t = tag, this.c = contents, this.l = length;
       }
       var caml_global_data = [0];
       MlBytes.prototype.toString =
        function(){
         switch(this.t){
           case 9:
            return this.c;
           default: caml_convert_string_to_bytes(this);
           case 0:
            if(jsoo_is_ascii(this.c)) return this.t = 9, this.c; this.t = 8;
           case 8:
            return this.c;
         }
        },
       MlBytes.prototype.toUtf16 =
        function(){
         var r = this.toString();
         if(this.t == 9) return r;
         return caml_utf16_of_utf8(r);
        },
       MlBytes.prototype.slice =
        function(){
         var content = this.t == 4 ? this.c.slice() : this.c;
         return new MlBytes(this.t, content, this.l);
        };
       function caml_bytes_of_jsbytes(s){return new MlBytes(0, s, s.length);}
       function caml_string_of_jsbytes(s){return caml_bytes_of_jsbytes(s);}
       function caml_raise_with_string(tag, msg){
        caml_raise_with_arg(tag, caml_string_of_jsbytes(msg));
       }
       function caml_raise_sys_error(msg){
        caml_raise_with_string(caml_global_data.Sys_error, msg);
       }
       function caml_ml_flush(chanid){
        var chan = caml_ml_channels[chanid];
        chan.opened || caml_raise_sys_error("Cannot flush a closed channel");
        if(! chan.buffer || chan.buffer_curr == 0) return 0;
        chan.output
         ? chan.output(caml_subarray_to_jsbytes(chan.buffer, 0, chan.buffer_curr))
         : chan.file.write(chan.offset, chan.buffer, 0, chan.buffer_curr);
        return chan.offset += chan.buffer_curr, chan.buffer_curr = 0, 0;
       }
       function caml_sys_open_for_node(fd, flags){return null;}
       function fs_node_supported(){return false;}
       function caml_jsbytes_of_string(s){
        return s.t & 6 && caml_convert_string_to_bytes(s), s.c;
       }
       function caml_jsstring_of_string(s){return s.toUtf16();}
       function make_path_is_absolute(){
        function posix(path){
         if(path.charAt(0) === "/") return ["", path.substring(1)];
         return;
        }
        function win32(path){
         var
          root,
          sep,
          splitDeviceRe =
            /^([a-zA-Z]:|[\\/]{2}[^\\/]+[\\/]+[^\\/]+)?([\\/])?([\s\S]*?)$/,
          result = splitDeviceRe.exec(path),
          device = result[1] || "",
          isUnc = Boolean(device && device.charAt(1) !== ":");
         if(Boolean(result[2] || isUnc))
          return root = result[1] || "",
                 sep = result[2] || "",
                 [root, path.substring(root.length + sep.length)];
         return;
        }
        return fs_node_supported() && globalThis.process
                && globalThis.process.platform
                ? globalThis.process.platform === "win32" ? win32 : posix
                : posix;
       }
       function caml_trailing_slash(name){
        return name.slice(- 1) !== "/" ? name + "/" : name;
       }
       var
        caml_ml_channels = new Array(),
        caml_sys_fds = new Array(3),
        path_is_absolute = make_path_is_absolute();
       fs_node_supported() && globalThis.process && globalThis.process.cwd
        ? caml_current_dir = globalThis.process.cwd().replace(/\\/g, "/")
        : caml_current_dir = "/static";
       function caml_make_path(name){
        var i;
        name = caml_jsstring_of_string(name),
        path_is_absolute(name) || (name = caml_current_dir + name);
        var
         comp0 = path_is_absolute(name),
         comp = comp0[1].split("/"),
         ncomp = [];
        for(i = 0; i < comp.length; i++)
         switch(comp[i]){
           case "..":
            ncomp.length > 1 && ncomp.pop(); break;
           case ".": break;
           case "": break;
           default: ncomp.push(comp[i]); break;
         }
        return ncomp.unshift(comp0[0]), ncomp.orig = name, ncomp;
       }
       function caml_utf8_of_utf16(s){
        var t, l, j, i, d, c, b;
        for(b = "", t = b, i = 0, l = s.length; i < l; i++){
         if(c = s.charCodeAt(i), c < 0x80){
          for(j = i + 1; j < l && (c = s.charCodeAt(j)) < 0x80; j++) ;
          j - i > 512
           ? (t.substr(0, 1), b += t, t = "", b += s.slice(i, j))
           : t += s.slice(i, j);
          if(j == l) break;
          i = j;
         }
         if(c < 0x800)
          t += String.fromCharCode(0xc0 | c >> 6),
          t += String.fromCharCode(0x80 | c & 0x3f);
         else if(c < 0xd800 || c >= 0xdfff)
          t +=
           String.fromCharCode
            (0xe0 | c >> 12, 0x80 | c >> 6 & 0x3f, 0x80 | c & 0x3f);
         else if
          (c >= 0xdbff || i + 1 == l || (d = s.charCodeAt(i + 1)) < 0xdc00
           || d > 0xdfff)
          t += "\xef\xbf\xbd";
         else
          i++,
          c = (c << 10) + d - 0x35fdc00,
          t +=
           String.fromCharCode
            (0xf0 | c >> 18,
             0x80 | c >> 12 & 0x3f,
             0x80 | c >> 6 & 0x3f,
             0x80 | c & 0x3f);
         t.length > 1024 && (t.substr(0, 1), b += t, t = "");
        }
        return b + t;
       }
       function caml_bytes_of_utf16_jsstring(s){
        var tag = 9;
        jsoo_is_ascii(s) || (tag = 8, s = caml_utf8_of_utf16(s));
        return new MlBytes(tag, s, s.length);
       }
       function caml_string_of_jsstring(s){
        return caml_bytes_of_utf16_jsstring(s);
       }
       function make_unix_err_args(code, syscall, path, errno){
        var variant = unix_error.indexOf(code);
        if(variant < 0){errno == null && (errno = - 9999); variant = [0, errno];}
        var
         args =
           [variant,
            caml_string_of_jsstring(syscall || ""),
            caml_string_of_jsstring(path || "")];
        return args;
       }
       function caml_named_value(nm){return caml_named_values[nm];}
       function caml_raise_with_args(tag, args){
        throw caml_maybe_attach_backtrace([0, tag].concat(args));
       }
       function caml_is_ml_bytes(s){return s instanceof MlBytes;}
       function caml_is_ml_string(s){return caml_is_ml_bytes(s);}
       function caml_bytes_of_array(a){
        a instanceof Uint8Array || (a = new Uint8Array(a));
        return new MlBytes(4, a, a.length);
       }
       function caml_bytes_of_string(s){return s;}
       function caml_raise_no_such_file(name){
        caml_raise_sys_error(name + ": No such file or directory");
       }
       function caml_convert_bytes_to_array(s){
        var a = new Uint8Array(s.l), b = s.c, l = b.length, i = 0;
        for(; i < l; i++) a[i] = b.charCodeAt(i);
        for(l = s.l; i < l; i++) a[i] = 0;
        return s.c = a, s.t = 4, a;
       }
       function caml_uint8_array_of_bytes(s){
        s.t != 4 && caml_convert_bytes_to_array(s);
        return s.c;
       }
       function caml_invalid_argument(msg){
        caml_raise_with_string(caml_global_data.Invalid_argument, msg);
       }
       function caml_create_bytes(len){
        len < 0 && caml_invalid_argument("Bytes.create");
        return new MlBytes(len ? 2 : 9, "", len);
       }
       function caml_ml_bytes_length(s){return s.l;}
       function caml_blit_bytes(s1, i1, s2, i2, len){
        var l, i, c2, c1;
        if(len == 0) return 0;
        if(i2 == 0 && (len >= s2.l || s2.t == 2 && len >= s2.c.length))
         s2.c =
          s1.t == 4
           ? caml_subarray_to_jsbytes(s1.c, i1, len)
           : i1 == 0 && s1.c.length == len ? s1.c : s1.c.substr(i1, len),
         s2.t = s2.c.length == s2.l ? 0 : 2;
        else if(s2.t == 2 && i2 == s2.c.length)
         s2.c +=
          s1.t == 4
           ? caml_subarray_to_jsbytes(s1.c, i1, len)
           : i1 == 0 && s1.c.length == len ? s1.c : s1.c.substr(i1, len),
         s2.t = s2.c.length == s2.l ? 0 : 2;
        else{
         s2.t != 4 && caml_convert_bytes_to_array(s2);
         if(c1 = s1.c, c2 = s2.c, s1.t == 4)
          if(i2 <= i1)
           for(i = 0; i < len; i++) c2[i2 + i] = c1[i1 + i];
          else
           for(i = len - 1; i >= 0; i--) c2[i2 + i] = c1[i1 + i];
         else{
          l = Math.min(len, c1.length - i1);
          for(i = 0; i < l; i++) c2[i2 + i] = c1.charCodeAt(i1 + i);
          for(; i < len; i++) c2[i2 + i] = 0;
         }
        }
        return 0;
       }
       function MlFile(){}
       function MlFakeFile(content){this.data = content;}
       var
        caml_current_dir = caml_trailing_slash(caml_current_dir),
        unix_error =
          ["E2BIG",
           "EACCES",
           "EAGAIN",
           "EBADF",
           "EBUSY",
           "ECHILD",
           "EDEADLK",
           "EDOM",
           "EEXIST",
           "EFAULT",
           "EFBIG",
           "EINTR",
           "EINVAL",
           "EIO",
           "EISDIR",
           "EMFILE",
           "EMLINK",
           "ENAMETOOLONG",
           "ENFILE",
           "ENODEV",
           "ENOENT",
           "ENOEXEC",
           "ENOLCK",
           "ENOMEM",
           "ENOSPC",
           "ENOSYS",
           "ENOTDIR",
           "ENOTEMPTY",
           "ENOTTY",
           "ENXIO",
           "EPERM",
           "EPIPE",
           "ERANGE",
           "EROFS",
           "ESPIPE",
           "ESRCH",
           "EXDEV",
           "EWOULDBLOCK",
           "EINPROGRESS",
           "EALREADY",
           "ENOTSOCK",
           "EDESTADDRREQ",
           "EMSGSIZE",
           "EPROTOTYPE",
           "ENOPROTOOPT",
           "EPROTONOSUPPORT",
           "ESOCKTNOSUPPORT",
           "EOPNOTSUPP",
           "EPFNOSUPPORT",
           "EAFNOSUPPORT",
           "EADDRINUSE",
           "EADDRNOTAVAIL",
           "ENETDOWN",
           "ENETUNREACH",
           "ENETRESET",
           "ECONNABORTED",
           "ECONNRESET",
           "ENOBUFS",
           "EISCONN",
           "ENOTCONN",
           "ESHUTDOWN",
           "ETOOMANYREFS",
           "ETIMEDOUT",
           "ECONNREFUSED",
           "EHOSTDOWN",
           "EHOSTUNREACH",
           "ELOOP",
           "EOVERFLOW"],
        caml_named_values = {};
       MlFakeFile.prototype = new MlFile(),
       MlFakeFile.prototype.constructor = MlFakeFile,
       MlFakeFile.prototype.truncate =
        function(len){
         var old = this.data;
         this.data = caml_create_bytes(len | 0),
         caml_blit_bytes(old, 0, this.data, 0, len);
        },
       MlFakeFile.prototype.length =
        function(){return caml_ml_bytes_length(this.data);},
       MlFakeFile.prototype.write =
        function(offset, buf, pos, len){
         var new_str, old_data, clen = this.length();
         offset + len >= clen
         &&
          (new_str = caml_create_bytes(offset + len),
           old_data = this.data,
           this.data = new_str,
           caml_blit_bytes(old_data, 0, this.data, 0, clen));
         return caml_blit_bytes
                 (caml_bytes_of_array(buf), pos, this.data, offset, len),
                0;
        },
       MlFakeFile.prototype.read =
        function(offset, buf, pos, len){
         var data, clen = this.length();
         offset + len >= clen && (len = clen - offset);
         len
         &&
          (data = caml_create_bytes(len | 0),
           caml_blit_bytes(this.data, offset, data, 0, len),
           buf.set(caml_uint8_array_of_bytes(data), pos));
         return len;
        };
       function MlFakeFd(name, file, flags){
        this.file = file, this.name = name, this.flags = flags;
       }
       MlFakeFd.prototype.err_closed =
        function(){
         caml_raise_sys_error(this.name + ": file descriptor already closed");
        },
       MlFakeFd.prototype.length =
        function(){if(this.file) return this.file.length(); this.err_closed();},
       MlFakeFd.prototype.write =
        function(offset, buf, pos, len){
         if(this.file) return this.file.write(offset, buf, pos, len);
         this.err_closed();
        },
       MlFakeFd.prototype.read =
        function(offset, buf, pos, len){
         if(this.file) return this.file.read(offset, buf, pos, len);
         this.err_closed();
        },
       MlFakeFd.prototype.close = function(){this.file = undefined;};
       function MlFakeDevice(root, f){
        this.content = {}, this.root = root, this.lookupFun = f;
       }
       MlFakeDevice.prototype.nm = function(name){return this.root + name;},
       MlFakeDevice.prototype.create_dir_if_needed =
        function(name){
         var i, comp = name.split("/"), res = "";
         for(i = 0; i < comp.length - 1; i++){
          if(res += comp[i] + "/", this.content[res]) continue;
          this.content[res] = Symbol("directory");
         }
        },
       MlFakeDevice.prototype.slash =
        function(name){return /\/$/.test(name) ? name : name + "/";},
       MlFakeDevice.prototype.lookup =
        function(name){
         var res;
         ! this.content[name] && this.lookupFun
         &&
          (res =
            this.lookupFun
             (caml_string_of_jsbytes(this.root), caml_string_of_jsbytes(name)),
           res !== 0
           &&
            (this.create_dir_if_needed(name),
             this.content[name] = new MlFakeFile(caml_bytes_of_string(res[1]))));
        },
       MlFakeDevice.prototype.exists =
        function(name){
         if(name == "") return 1;
         var name_slash = this.slash(name);
         if(this.content[name_slash]) return 1;
         return this.lookup(name), this.content[name] ? 1 : 0;
        },
       MlFakeDevice.prototype.isFile =
        function(name){return this.exists(name) && ! this.is_dir(name) ? 1 : 0;},
       MlFakeDevice.prototype.mkdir =
        function(name, mode, raise_unix){
         var unix_error = raise_unix && caml_named_value("Unix.Unix_error");
         this.exists(name)
         &&
          (unix_error
            ? caml_raise_with_args
              (unix_error, make_unix_err_args("EEXIST", "mkdir", this.nm(name)))
            : caml_raise_sys_error(name + ": File exists"));
         var parent = /^(.*)\/[^/]+/.exec(name);
         parent = parent && parent[1] || "",
         this.exists(parent)
         ||
          (unix_error
            ? caml_raise_with_args
              (unix_error, make_unix_err_args("ENOENT", "mkdir", this.nm(parent)))
            : caml_raise_sys_error(parent + ": No such file or directory"));
         this.is_dir(parent)
         ||
          (unix_error
            ? caml_raise_with_args
              (unix_error,
               make_unix_err_args("ENOTDIR", "mkdir", this.nm(parent)))
            : caml_raise_sys_error(parent + ": Not a directory"));
         this.create_dir_if_needed(this.slash(name));
        },
       MlFakeDevice.prototype.rmdir =
        function(name, raise_unix){
         var
          n,
          unix_error = raise_unix && caml_named_value("Unix.Unix_error"),
          name_slash = name == "" ? "" : this.slash(name),
          r = new RegExp("^" + name_slash + "([^/]+)");
         this.exists(name)
         ||
          (unix_error
            ? caml_raise_with_args
              (unix_error, make_unix_err_args("ENOENT", "rmdir", this.nm(name)))
            : caml_raise_sys_error(name + ": No such file or directory"));
         this.is_dir(name)
         ||
          (unix_error
            ? caml_raise_with_args
              (unix_error, make_unix_err_args("ENOTDIR", "rmdir", this.nm(name)))
            : caml_raise_sys_error(name + ": Not a directory"));
         for(var n in this.content)
          n.match(r)
          &&
           (unix_error
             ? caml_raise_with_args
               (unix_error,
                make_unix_err_args("ENOTEMPTY", "rmdir", this.nm(name)))
             : caml_raise_sys_error(this.nm(name) + ": Directory not empty"));
         delete this.content[name_slash];
        },
       MlFakeDevice.prototype.readdir =
        function(name){
         var m, n, name_slash = name == "" ? "" : this.slash(name);
         this.exists(name)
         || caml_raise_sys_error(name + ": No such file or directory");
         this.is_dir(name) || caml_raise_sys_error(name + ": Not a directory");
         var r = new RegExp("^" + name_slash + "([^/]+)"), seen = {}, a = [];
         for(var n in this.content)
          m = n.match(r), m && ! seen[m[1]] && (seen[m[1]] = true, a.push(m[1]));
         return a;
        },
       MlFakeDevice.prototype.opendir =
        function(name, raise_unix){
         var
          unix_error = raise_unix && caml_named_value("Unix.Unix_error"),
          a = this.readdir(name),
          c = false,
          i = 0;
         return {readSync:
                 function(){
                  c
                  &&
                   (unix_error
                     ? caml_raise_with_args
                       (unix_error,
                        make_unix_err_args("EBADF", "closedir", this.nm(name)))
                     : caml_raise_sys_error(name + ": closedir failed"));
                  if(i == a.length) return null;
                  var entry = a[i];
                  return i++, {name: entry};
                 },
                 closeSync:
                 function(){
                  c
                  &&
                   (unix_error
                     ? caml_raise_with_args
                       (unix_error,
                        make_unix_err_args("EBADF", "closedir", this.nm(name)))
                     : caml_raise_sys_error(name + ": closedir failed"));
                  c = true, a = [];
                 }};
        },
       MlFakeDevice.prototype.is_dir =
        function(name){
         if(name == "") return true;
         var name_slash = this.slash(name);
         return this.content[name_slash] ? 1 : 0;
        },
       MlFakeDevice.prototype.unlink =
        function(name){
         var ok = this.content[name] ? true : false;
         return delete this.content[name], ok;
        },
       MlFakeDevice.prototype.open =
        function(name, f){
         var file;
         f.rdonly && f.wronly
         &&
          caml_raise_sys_error
           (this.nm(name)
            + " : flags Open_rdonly and Open_wronly are not compatible");
         f.text && f.binary
         &&
          caml_raise_sys_error
           (this.nm(name)
            + " : flags Open_text and Open_binary are not compatible");
         if(this.lookup(name), this.content[name]){
          this.is_dir(name)
          && caml_raise_sys_error(this.nm(name) + " : is a directory");
          f.create && f.excl
          && caml_raise_sys_error(this.nm(name) + " : file already exists");
          file = this.content[name], f.truncate && file.truncate();
         }
         else if(f.create)
          this.create_dir_if_needed(name),
          this.content[name] = new MlFakeFile(caml_create_bytes(0)),
          file = this.content[name];
         else
          caml_raise_no_such_file(this.nm(name));
         return new MlFakeFd(this.nm(name), file, f);
        },
       MlFakeDevice.prototype.open =
        function(name, f){
         var file;
         f.rdonly && f.wronly
         &&
          caml_raise_sys_error
           (this.nm(name)
            + " : flags Open_rdonly and Open_wronly are not compatible");
         f.text && f.binary
         &&
          caml_raise_sys_error
           (this.nm(name)
            + " : flags Open_text and Open_binary are not compatible");
         if(this.lookup(name), this.content[name]){
          this.is_dir(name)
          && caml_raise_sys_error(this.nm(name) + " : is a directory");
          f.create && f.excl
          && caml_raise_sys_error(this.nm(name) + " : file already exists");
          file = this.content[name], f.truncate && file.truncate();
         }
         else if(f.create)
          this.create_dir_if_needed(name),
          this.content[name] = new MlFakeFile(caml_create_bytes(0)),
          file = this.content[name];
         else
          caml_raise_no_such_file(this.nm(name));
         return new MlFakeFd(this.nm(name), file, f);
        },
       MlFakeDevice.prototype.register =
        function(name, content){
         var file, bytes;
         this.content[name]
         && caml_raise_sys_error(this.nm(name) + " : file already exists");
         caml_is_ml_bytes(content) && (file = new MlFakeFile(content));
         if(caml_is_ml_string(content))
          file = new MlFakeFile(caml_bytes_of_string(content));
         else if(content instanceof Array)
          file = new MlFakeFile(caml_bytes_of_array(content));
         else if(typeof content === "string")
          file = new MlFakeFile(caml_bytes_of_jsbytes(content));
         else if(content.toString)
          bytes =
           caml_bytes_of_string(caml_string_of_jsstring(content.toString())),
          file = new MlFakeFile(bytes);
         file
          ? (this.create_dir_if_needed(name), this.content[name] = file)
          : caml_raise_sys_error
            (this.nm(name) + " : registering file with invalid content type");
        },
       MlFakeDevice.prototype.constructor = MlFakeDevice;
       function MlNodeDevice(){}
       function caml_get_root(path){
        var x = path_is_absolute(path);
        if(! x) return;
        return x[0] + "/";
       }
       function caml_failwith(msg){
        caml_global_data.Failure
        ||
         (caml_global_data.Failure =
          [248, caml_string_of_jsbytes("Failure"), - 3]);
        caml_raise_with_string(caml_global_data.Failure, msg);
       }
       var
        caml_root =
          caml_get_root(caml_current_dir)
          || caml_failwith("unable to compute caml_root"),
        jsoo_mount_point = [];
       fs_node_supported()
        ? jsoo_mount_point.push
          ({path: caml_root, device: new MlNodeDevice(caml_root)})
        : jsoo_mount_point.push
          ({path: caml_root, device: new MlFakeDevice(caml_root)});
       jsoo_mount_point.push
        ({path: "/static/", device: new MlFakeDevice("/static/")});
       function resolve_fs_device(name){
        var
         i,
         m,
         res,
         root,
         path = caml_make_path(name),
         name_slash = (name = path.join("/"), caml_trailing_slash(name));
        for(i = 0; i < jsoo_mount_point.length; i++)
         m = jsoo_mount_point[i],
         name_slash.search(m.path) == 0
         && (! res || res.path.length < m.path.length)
         &&
          (res =
           {path: m.path,
            device: m.device,
            rest: name.substring(m.path.length, name.length)});
        ! res && fs_node_supported()
        &&
         (root = caml_get_root(name),
          root && root.match(/^[a-zA-Z]:\/$/)
          &&
           (m = {path: root, device: new MlNodeDevice(root)},
            jsoo_mount_point.push(m),
            res =
             {path: m.path,
              device: m.device,
              rest: name.substring(m.path.length, name.length)}));
        if(res) return res;
        caml_raise_sys_error("no device found for " + name_slash);
       }
       function MlFakeFd_out(fd, flags){
        if
         (MlFakeFile.call(this, caml_create_bytes(0)),
          this.log = function(s){return 0;},
          fd == 1 && typeof console.log == "function")
         this.log = console.log;
        else if(fd == 2 && typeof console.error == "function")
         this.log = console.error;
        else if(typeof console.log == "function") this.log = console.log;
        this.flags = flags;
       }
       MlFakeFd_out.prototype.length = function(){return 0;},
       MlFakeFd_out.prototype.write =
        function(offset, buf, pos, len){
         var src;
         if(this.log){
          len > 0 && pos >= 0 && pos + len <= buf.length
          && buf[pos + len - 1] == 10
          && len--;
          return src = caml_create_bytes(len),
                 caml_blit_bytes(caml_bytes_of_array(buf), pos, src, 0, len),
                 this.log(src.toUtf16()),
                 0;
         }
         caml_raise_sys_error(this.fd + ": file descriptor already closed");
        },
       MlFakeFd_out.prototype.read =
        function(offset, buf, pos, len){
         caml_raise_sys_error(this.fd + ": file descriptor is write only");
        },
       MlFakeFd_out.prototype.close = function(){this.log = undefined;};
       function caml_sys_open_internal(file, idx){
        idx == undefined && (idx = caml_sys_fds.length);
        return caml_sys_fds[idx] = file, idx | 0;
       }
       function caml_sys_open(name, flags, _perms){
        var f = {};
        while(flags){
         switch(flags[1]){
           case 0:
            f.rdonly = 1; break;
           case 1:
            f.wronly = 1; break;
           case 2:
            f.append = 1; break;
           case 3:
            f.create = 1; break;
           case 4:
            f.truncate = 1; break;
           case 5:
            f.excl = 1; break;
           case 6:
            f.binary = 1; break;
           case 7:
            f.text = 1; break;
           case 8:
            f.nonblock = 1; break;
         }
         flags = flags[2];
        }
        f.rdonly && f.wronly
        &&
         caml_raise_sys_error
          (caml_jsbytes_of_string(name)
           + " : flags Open_rdonly and Open_wronly are not compatible");
        f.text && f.binary
        &&
         caml_raise_sys_error
          (caml_jsbytes_of_string(name)
           + " : flags Open_text and Open_binary are not compatible");
        var root = resolve_fs_device(name), file = root.device.open(root.rest, f);
        return caml_sys_open_internal(file, undefined);
       }
       (function(){
          function file(fd, flags){
           return fs_node_supported()
                   ? caml_sys_open_for_node(fd, flags)
                   : new MlFakeFd_out(fd, flags);
          }
          caml_sys_open_internal
           (file(0, {rdonly: 1, altname: "/dev/stdin", isCharacterDevice: true}),
            0),
          caml_sys_open_internal
           (file(1, {buffered: 2, wronly: 1, isCharacterDevice: true}), 1),
          caml_sys_open_internal
           (file(2, {buffered: 2, wronly: 1, isCharacterDevice: true}), 2);
         }
         ());
       function caml_ml_open_descriptor_in(fd){
        var file = caml_sys_fds[fd];
        file.flags.wronly && caml_raise_sys_error("fd " + fd + " is writeonly");
        var
         refill = null,
         channel =
           {file: file,
            offset: file.flags.append ? file.length() : 0,
            fd: fd,
            opened: true,
            out: false,
            buffer_curr: 0,
            buffer_max: 0,
            buffer: new Uint8Array(65536),
            refill: refill};
        return caml_ml_channels[channel.fd] = channel, channel.fd;
       }
       function caml_ml_open_descriptor_out(fd){
        var file = caml_sys_fds[fd];
        file.flags.rdonly && caml_raise_sys_error("fd " + fd + " is readonly");
        var
         buffered = file.flags.buffered !== undefined ? file.flags.buffered : 1,
         channel =
           {file: file,
            offset: file.flags.append ? file.length() : 0,
            fd: fd,
            opened: true,
            out: true,
            buffer_curr: 0,
            buffer: new Uint8Array(65536),
            buffered: buffered};
        return caml_ml_channels[channel.fd] = channel, channel.fd;
       }
       function caml_ml_out_channels_list(){
        var c, l = 0;
        for(c = 0; c < caml_ml_channels.length; c++)
         caml_ml_channels[c] && caml_ml_channels[c].opened
         && caml_ml_channels[c].out
         && (l = [0, caml_ml_channels[c].fd, l]);
        return l;
       }
       function caml_build_symbols(toc){
        var symb, i;
        while(toc)
         if(caml_jsstring_of_string(toc[1][1]) == "SYJS"){symb = toc[1][2]; break;}
         else
          toc = toc[2];
        var r = {};
        if(symb)
         for(i = 1; i < symb.length; i++)
          r[caml_jsstring_of_string(symb[i][1])] = symb[i][2];
        return r;
       }
       function caml_register_global(n, v, name_opt){
        var nid, name;
        if(name_opt)
         if(name = name_opt, globalThis.toplevelReloc)
          n = caml_callback(globalThis.toplevelReloc, [name]);
         else if(caml_global_data.toc){
          caml_global_data.symbols
          || (caml_global_data.symbols = caml_build_symbols(caml_global_data.toc));
          nid = caml_global_data.symbols[name],
          nid >= 0
           ? n = nid
           : caml_failwith("caml_register_global: cannot locate " + name);
         }
        caml_global_data[n + 1] = v, name_opt && (caml_global_data[name_opt] = v);
       }
       function caml_register_named_value(nm, v){
        return caml_named_values[caml_jsbytes_of_string(nm)] = v, 0;
       }
       function caml_wrap_exception(e){
        var exn;
        {
         if(e instanceof Array) return e;
         if
          (globalThis.RangeError && e instanceof globalThis.RangeError
           && e.message
           && e.message.match(/maximum call stack/i))
          exn = caml_global_data.Stack_overflow;
         else if
          (globalThis.InternalError && e instanceof globalThis.InternalError
           && e.message
           && e.message.match(/too much recursion/i))
          exn = caml_global_data.Stack_overflow;
         else if(e instanceof globalThis.Error && caml_named_value("jsError"))
          exn = [0, caml_named_value("jsError"), e];
         else
          exn = [0, caml_global_data.Failure, caml_string_of_jsstring(String(e))];
         e instanceof globalThis.Error && (exn.js_error = e);
         return exn;
        }
       }
       function caml_is_special_exception(exn){
        switch(exn[2]){case - 8:case - 11:case - 12: return 1;default: return 0;
        }
       }
       function caml_format_exception(exn){
        var bucket, i, start, v, r = "";
        if(exn[0] == 0){
         r += exn[1][1],
         exn.length == 3 && exn[2][0] == 0 && caml_is_special_exception(exn[1])
          ? (bucket = exn[2], start = 1)
          : (start = 2, bucket = exn);
         r += "(";
         for(i = start; i < bucket.length; i++){
          i > start && (r += ", ");
          if(v = bucket[i], typeof v == "number")
           r += v.toString();
          else if(v instanceof MlBytes)
           r += '"' + v.toString() + '"';
          else if(typeof v == "string")
           r += '"' + v.toString() + '"';
          else
           r += "_";
         }
         r += ")";
        }
        else if(exn[0] == 248) r += exn[1];
        return r;
       }
       function caml_fatal_uncaught_exception(err){
        var msg, handler, at_exit;
        if(err instanceof Array && (err[0] == 0 || err[0] == 248))
         if
          (handler = caml_named_value("Printexc.handle_uncaught_exception"),
           handler)
          caml_callback(handler, [err, false]);
         else{
          msg = caml_format_exception(err),
          at_exit = caml_named_value("Pervasives.do_at_exit"),
          at_exit && caml_callback(at_exit, [0]);
          if(console.error("Fatal error: exception " + msg + "\n"), err.js_error)
           throw err.js_error;
         }
        else
         throw err;
       }
       function caml_setup_uncaught_exception_handler(){
        var process = globalThis.process;
        if(process && process.on)
         process.on
          ("uncaughtException",
           function(err, origin){
            caml_fatal_uncaught_exception(err), process.exit(2);
           });
        else if(globalThis.addEventListener)
         globalThis.addEventListener
          ("error",
           function(event){
            event.error && caml_fatal_uncaught_exception(event.error);
           });
       }
       var caml_callback = caml_call_gen;
       caml_setup_uncaught_exception_handler();
       function caml_call1(f, a0){
        return (f.l >= 0 ? f.l : f.l = f.length) == 1
                ? f(a0)
                : caml_call_gen(f, [a0]);
       }
       var
        Out_of_memory = [248, caml_string_of_jsbytes("Out_of_memory"), - 1],
        Sys_error = [248, caml_string_of_jsbytes("Sys_error"), - 2],
        Failure = [248, caml_string_of_jsbytes("Failure"), - 3],
        Invalid_argument = [248, caml_string_of_jsbytes("Invalid_argument"), - 4],
        End_of_file = [248, caml_string_of_jsbytes("End_of_file"), - 5],
        Division_by_zero = [248, caml_string_of_jsbytes("Division_by_zero"), - 6],
        Not_found = [248, caml_string_of_jsbytes("Not_found"), - 7],
        Match_failure = [248, caml_string_of_jsbytes("Match_failure"), - 8],
        Stack_overflow = [248, caml_string_of_jsbytes("Stack_overflow"), - 9],
        Sys_blocked_io = [248, caml_string_of_jsbytes("Sys_blocked_io"), - 10],
        Assert_failure = [248, caml_string_of_jsbytes("Assert_failure"), - 11],
        Undefined_recursive_module =
          [248, caml_string_of_jsbytes("Undefined_recursive_module"), - 12],
        flush_all =
          (caml_register_global
            (11, Undefined_recursive_module, "Undefined_recursive_module"),
           caml_register_global(10, Assert_failure, "Assert_failure"),
           caml_register_global(9, Sys_blocked_io, "Sys_blocked_io"),
           caml_register_global(8, Stack_overflow, "Stack_overflow"),
           caml_register_global(7, Match_failure, "Match_failure"),
           caml_register_global(6, Not_found, "Not_found"),
           caml_register_global(5, Division_by_zero, "Division_by_zero"),
           caml_register_global(4, End_of_file, "End_of_file"),
           caml_register_global(3, Invalid_argument, "Invalid_argument"),
           caml_register_global(2, Failure, "Failure"),
           caml_register_global(1, Sys_error, "Sys_error"),
           caml_register_global(0, Out_of_memory, "Out_of_memory"),
           caml_ml_open_descriptor_in(0),
           caml_ml_open_descriptor_out(1),
           caml_ml_open_descriptor_out(2),
           function(param){
            var l, a, _a_, param$0 = caml_ml_out_channels_list(0);
            for(;;){
             if(! param$0) return 0;
             l = param$0[2], a = param$0[1];
             try{caml_ml_flush(a);}
             catch(_b_){
              if(_a_ = caml_wrap_exception(_b_), _a_[1] !== Sys_error)
               throw caml_maybe_attach_backtrace(_a_, 0);
             }
             param$0 = l;
            }
           }),
        exit_function = [0, flush_all];
       function do_at_exit(param){
        return caml_call1(caml_atomic_load(exit_function), 0);
       }
       caml_register_named_value
        (caml_string_of_jsbytes("Pervasives.do_at_exit"), do_at_exit),
       do_at_exit(0);
       return;
      }
      (globalThis));
    //end |}]

// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
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

//Provides: caml_js_get_console const
function caml_js_get_console(){
  var f = function(x) {globalThis.WScript.StdOut.Write(x); return 0;};
  return {
    log: f,
    debug: f,
    info: f,
    warn: f,
    error: function(x) {globalThis.WScript.StdErr.Write(x); return 0;}
  };
}

//Provides: caml_bytes_unsafe_get mutable
function caml_bytes_unsafe_get (s, i) {
  switch (s.t & 6) {
  default: /* PARTIAL */
    if (i >= s.c.length) return 0;
  case 0: /* BYTES */
    return s.c.charCodeAt(i);
  case 4: /* ARRAY */
    return s.c._getter(i)
  }
}

//Provides: caml_bytes_unsafe_set
//Requires: caml_convert_bytes_to_array
function caml_bytes_unsafe_set (s, i, c) {
  // The OCaml compiler uses Char.unsafe_chr on integers larger than 255!
  c &= 0xff;
  if (s.t != 4 /* ARRAY */) {
    if (i == s.c.length) {
      s.c += String.fromCharCode (c);
      if (i + 1 == s.l) s.t = 0; /*BYTES | UNKOWN*/
      return 0;
    }
    caml_convert_bytes_to_array (s);
  }
  s.c._setter(i, c);
  return 0;
}

//Provides: caml_convert_bytes_to_array
function caml_convert_bytes_to_array (s) {
  /* Assumes not ARRAY */
  var a = new Uint8Array(s.l);
  var b = s.c, l = b.length, i = 0;
  for (; i < l; i++) a._setter(i, b.charCodeAt(i));
  for (l = s.l; i < l; i++) a._setter(i, 0);
  s.c = a;
  s.t = 4; /* ARRAY */
  return a;
}

//Provides: caml_fill_bytes
//Requires: caml_str_repeat, caml_convert_bytes_to_array
function caml_fill_bytes(s, i, l, c) {
  if (l > 0) {
    if (i == 0 && (l >= s.l || (s.t == 2 /* PARTIAL */ && l >= s.c.length))) {
      if (c == 0) {
        s.c = "";
        s.t = 2; /* PARTIAL */
      } else {
        s.c = caml_str_repeat (l, String.fromCharCode(c));
        s.t = (l == s.l)?0 /* BYTES | UNKOWN */ :2; /* PARTIAL */
      }
    } else {
      if (s.t != 4 /* ARRAY */) caml_convert_bytes_to_array(s);
      for (l += i; i < l; i++) s.c._setter(i, c);
    }
  }
  return 0;
}

//Provides: caml_blit_bytes
//Requires: caml_subarray_to_jsbytes, caml_convert_bytes_to_array
function caml_blit_bytes(s1, i1, s2, i2, len) {
  if (len == 0) return 0;
  if ((i2 == 0) &&
      (len >= s2.l || (s2.t == 2 /* PARTIAL */ && len >= s2.c.length))) {
    s2.c = (s1.t == 4 /* ARRAY */)?
      caml_subarray_to_jsbytes(s1.c, i1, len):
      (i1 == 0 && s1.c.length == len)?s1.c:s1.c.substr(i1, len);
    s2.t = (s2.c.length == s2.l)?0 /* BYTES | UNKOWN */ :2; /* PARTIAL */
  } else if (s2.t == 2 /* PARTIAL */ && i2 == s2.c.length) {
    s2.c += (s1.t == 4 /* ARRAY */)?
      caml_subarray_to_jsbytes(s1.c, i1, len):
      (i1 == 0 && s1.c.length == len)?s1.c:s1.c.substr(i1, len);
    s2.t = (s2.c.length == s2.l)?0 /* BYTES | UNKOWN */ :2; /* PARTIAL */
  } else {
    if (s2.t != 4 /* ARRAY */) caml_convert_bytes_to_array(s2);
    var c1 = s1.c, c2 = s2.c;
    if (s1.t == 4 /* ARRAY */) {
      if (i2 <= i1) {
        for (var i = 0; i < len; i++) c2._setter(i2 + i, c1._getter(i1 + i));
      } else {
        for (var i = len - 1; i >= 0; i--) c2._setter(i2 + i, c1._getter(i1 + i));
      }
    } else {
      var l = Math.min (len, c1.length - i1);
      for (var i = 0; i < l; i++) c2._setter(i2 + i, c1.charCodeAt(i1 + i));
      for (; i < len; i++) c2._setter(i2 + i, 0);
    }
  }
  return 0;
}

//Provides: raw_array_sub
function raw_array_sub (a,i,l) {
  var b = new Array(l);
  for(var j = 0; j < l; j++) b[j] = a._getter(i+j);
  return b
}

//Provides: raw_array_copy
function raw_array_copy (a) {
  var l = a.length;
  var b = new Array(l);
  for(var i = 0; i < l; i++ ) b[i] = a._getter(i);
  return b
}

//Provides: caml_subarray_to_jsbytes
//Requires: raw_array_sub
//Requires: raw_array_copy
function caml_subarray_to_jsbytes (a, i, len) {
  var f = String.fromCharCode;
  if (i == 0 && len <= 4096 && len == a.length)
    return f.apply (null, raw_array_copy(a));
  var s = "";
  for (; 0 < len; i += 1024,len-=1024)
    s += f.apply (null, raw_array_sub(a,i, Math.min(len, 1024)));
  return s;
}

//Provides: caml_js_set_prop (mutable, const, const, const)
//Requires: caml_jsstring_of_string
function caml_js_set_prop(o, p, a, v) {
  p = caml_jsstring_of_string(p);
  switch (a.length) {
  case 2: o[p](a[1]) = v; break;
  case 3: o[p](a[1],a[2]) = v; break;
  case 4: o[p](a[1],a[2],a[3]) = v; break;
  case 5: o[p](a[1],a[2],a[3],a[4]) = v; break;
  case 6: o[p](a[1],a[2],a[3],a[4],a[5]) = v; break;
  case 7: o[p](a[1],a[2],a[3],a[4],a[5],a[6]) = v; break;
  case 8: o[p](a[1],a[2],a[3],a[4],a[5],a[6],a[7]) = v; break;
  }
  return 0;
}

// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2010 Jérôme Vouillon
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

// Invariants
// ==========
// At any time, at least one property of "string", "bytes" or "array"
// is set; if several are set, then their values must correspond.
// If "bytes" is set, then properties "len" and "last" are also set.
// If "array" is set, properties "len" and "last" are also set.
// Properties "len" and "last" may have different values only when
// "string" and "array" are both null.
//
// We use unusual accessors (getLen/get/set) so that this
// implementation of string differs significantly from Javascript.
// This way, using the wrong object is detected early.

//Provides: caml_str_repeat
function caml_str_repeat(n, s) {
  if (s.repeat) return s.repeat(n); // ECMAscript 6 and Firefox 24+
  var r = "", l = 0;
  if (n === 0) return r;
  for(;;) {
    if (n & 1) r += s;
    n >>= 1;
    if (n === 0) return r;
    s += s;
    l++;
    if (l == 9) {
      s.slice(0,1); // flatten the string
      // then, the flattening of the whole string will be faster,
      // as it will be composed of larger pieces
    }
  }
}

//Provides: caml_subarray_to_string
function caml_subarray_to_string (a, i, len) {
  var f = String.fromCharCode;
  if (i == 0 && len <= 4096 && len == a.length) return f.apply (null, a);
  var s = "";
  for (len += i; i < len; i += 1024)
    s += f.apply (null, a.slice(i, Math.min(len, i + 1024)));
  return s;
}

//Provides: MlString
//Requires: caml_raise_with_arg, js_print_stderr, caml_global_data, caml_str_repeat
function MlString(param) {
  this.string = this.array = null;
  if (param !== undefined) {
    this.bytes = this.fullBytes = param;
    this.last = this.len = param.length;
  } else {
    this.bytes = this.fullBytes = null;
    this.len = null; this.last = 0;
  }
}
//This is here to avoid circular deps
function mlstring_bound_error () {
    caml_raise_with_arg(caml_global_data[4],new MlString("index out of bounds"));
}
MlString.prototype = {
  // JS string : Utf16
  //     string:null,
  // byte string
  //     bytes:null,
  //     fullBytes:null,
  // byte array
  //     array:null,
  // length
  //     len:null,
  // last initialized byte
  //     last:0,

  toJsString:function() {
    // assumes this.string == null
    var a = this.getFullBytes();
    try {
      return this.string = decodeURIComponent (escape(a));
    } catch (e){
      js_print_stderr("MlString.toJsString: wrong encoding for \"" + a + "\"");
      return a;
    }
  },

  toBytes:function() {
    // assumes this.bytes == null
    if (this.string != null){
      try {
        var b = unescape (encodeURIComponent (this.string));
      } catch (e) {
        js_print_stderr("MlString.toBytes: wrong encoding for \"" + this.string + "\"");
        var b = this.string;
      }
    } else {
      b = caml_subarray_to_string (a, 0, a.length);
    }
    this.bytes = this.fullBytes = b;
    this.last = this.len = b.length;
    return b;
  },

  getBytes:function() {
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return b;
  },

  getFullBytes:function() {
    var b = this.fullBytes;
    if (b !== null) return b;
    b = this.bytes;
    if (b == null) b = this.toBytes ();
    if (this.last < this.len) {
      this.bytes = (b += caml_str_repeat(this.len - this.last, '\0'));
      this.last = this.len;
    }
    this.fullBytes = b;
    return b;
  },

  toArray:function() {
    // assumes this.array == null
    var b = this.bytes;
    if (b == null) b = this.toBytes ();
    var a = new Array(this.len), l = this.last;
    for (var i = 0; i < l; i++) a[i] = b.charCodeAt(i);
    for (l = this.len; i < l; i++) a[i] = 0;
    this.string = this.bytes = this.fullBytes = null;
    this.last = this.len;
    this.array = a;
    return a;
  },

  getArray:function() {
    var a = this.array;
    if (!a) a = this.toArray();
    return a;
  },

  getLen:function() {
    var len = this.len;
    if (len !== null) return len;
    this.toBytes();
    return this.len;
  },

  toString:function() { var s = this.string; return s?s:this.toJsString(); },

  valueOf:function() { var s = this.string; return s?s:this.toJsString(); },

  get:function (i) {
    var a = this.array;
    if (a) return a[i];
    var b = this.bytes;
    if (b == null) b = this.toBytes();
    return (i<this.last)?b.charCodeAt(i):0;
  },

  safeGet:function (i) {
    if (this.len == null) this.toBytes();
    if ((i < 0) || (i >= this.len)) mlstring_bound_error ();
    return this.get(i);
  },

  set:function (i, c) {
    var a = this.array;
    if (!a) {
      if (this.last == i) {
        this.bytes += String.fromCharCode (c & 0xff);
        this.last ++;
        return 0;
      }
      a = this.toArray();
    } else if (this.bytes != null) {
      this.bytes = this.fullBytes = this.string = null;
    }
    a[i] = c & 0xff;
    return 0;
  },

  safeSet:function (i, c) {
    if (this.len == null) this.toBytes ();
    if ((i < 0) || (i >= this.len)) mlstring_bound_error ();
    return this.set(i, c);
  }
}

// Conversion Javascript -> Caml

//Provides: caml_js_to_string const
//Requires: MlString
function caml_js_to_string(s) {
  var x = new MlString (); x.string = s; return x;
}

// Caml string initialized form an array of bytes
//Provides: caml_string_of_array
//Requires: MlString
function caml_string_of_array (a) {
  var s = new MlString ();
  var len = a.length; s.array = a; s.len = s.last = len;
  return s;
}

//Provides: caml_create_string const
//Requires: MlString,caml_invalid_argument
function caml_create_string(len) {
  if (len < 0) caml_invalid_argument("String.create");
  var s = new MlString();
  s.bytes = ""; s.len = len;
  return s;
}
//Provides: caml_fill_string
//Requires: caml_str_repeat
function caml_fill_string(s, i, l, c) {
  if (l > 0) {
    if (i == 0 && l >= s.last && ((s.bytes != null) || s.array)) {
      if (c == 0) l = 0;
      s.array = s.string = null;
      s.last = l;
      s.bytes = caml_str_repeat (l, String.fromCharCode(c));
      s.fullBytes = (s.len == l)?s.bytes:null;
    } else {
      var a = s.array;
      if (!a) a = s.toArray();
      else if (s.bytes != null) {
        s.bytes = s.fullBytes = s.string = null;
      }
      for (l += i; i < l; i++) a[i] = c;
    }
  }
  return 0;
}
//Provides: caml_string_compare mutable
//Requires: MlString
function caml_string_compare(s1, s2) {
  if (s1.fullBytes != null && s2.fullBytes != null) {
    if (s1.fullBytes < s2.fullBytes) return -1;
    if (s1.fullBytes > s2.fullBytes) return 1;
    return 0;
  }
  var b1 = s1.getFullBytes ();
  var b2 = s2.getFullBytes ();
  if (b1 < b2) return -1;
  if (b1 > b2) return 1;
  return 0;
}
//Provides: caml_string_equal mutable
//Requires: MlString
function caml_string_equal(s1, s2) {
  var b1 = s1.fullBytes;
  var b2 = s2.fullBytes;
  if (b1 != null && b2 != null) return (b1 == b2)?1:0;
  return (s1.getFullBytes () == s2.getFullBytes ())?1:0;
}
//Provides: caml_string_notequal mutable
//Requires: caml_string_equal
function caml_string_notequal(s1, s2) { return 1-caml_string_equal(s1, s2); }
//Provides: caml_string_lessequal
//Requires: MlString
function caml_string_lessequal(s1, s2) {
  if (s1.fullBytes != null && s2.fullBytes != null)
    return s1.fullBytes <= s2.fullBytes;
  return s1.getFullBytes () <= s2.getFullBytes ();
}
//Provides: caml_string_lessthan
//Requires: MlString
function caml_string_lessthan(s1, s2) {
  if (s1.fullBytes != null && s2.fullBytes != null)
    return s1.fullBytes < s2.fullBytes;
  return s1.getFullBytes () < s2.getFullBytes ();
}
//Provides: caml_string_greaterthan
//Requires: caml_string_lessthan
function caml_string_greaterthan(s1, s2) {
  return caml_string_lessthan(s2, s1);
}
//Provides: caml_string_greaterequal
//Requires: caml_string_lessequal
function caml_string_greaterequal(s1, s2) {
  return caml_string_lessequal(s2,s1);
}
//Provides: caml_blit_string
//Requires: MlString, caml_subarray_to_string
function caml_blit_string(s1, i1, s2, i2, len) {
  if (len === 0) return 0;
  if ((i2 == 0) && (s2.last <= len)) {
    s2.array = s2.string = null;
    s2.bytes =
      (s1.bytes != null)?
        s1.bytes.substr (i1, len):
        (s1.array)?
          caml_subarray_to_string(s1.array, i1, len):
          s1.toBytes().substr (i1, len);
    s2.last = s2.bytes.length;
    s2.fullBytes = (s2.len == s2.last)?s2.bytes:null;
  } else if (i2 == s2.last) {
    s2.bytes +=
      (s1.bytes != null)?
        s1.bytes.substr (i1, len):
        (s1.array)?
          caml_subarray_to_string(s1.array, i1, len):
          s1.toBytes().substr (i1, len);
    s2.last = s2.bytes.length;
    s2.fullBytes = (s2.len == s2.last)?s2.bytes:null;
  } else {
    var a2 = s2.array;
    if (!a2) a2 = s2.toArray();
    else if (s2.bytes != null) {
      s2.bytes = s2.fullBytes = s2.string = null;
    }
    var a1 = s1.array;
    if (a1)
      for (var i = 0; i < len; i++) a2 [i2 + i] = a1 [i1 + i];
    else {
      var b = s1.bytes;
      if (b == null) b = s1.toBytes();
      var l = Math.min (len, b.length - i1);
      for (var i = 0; i < l; i++) a2 [i2 + i] = b.charCodeAt(i1 + i);
      for (; i < len; i++) a2 [i2 + i] = 0;
    }
  }
  return 0;
}
//Provides: caml_new_string
//Requires: MlString
function caml_new_string(x){return new MlString(x);}

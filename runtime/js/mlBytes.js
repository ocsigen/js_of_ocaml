// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2010-2014 Jérôme Vouillon
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

// An OCaml string is an object with three fields:
// - tag 't'
// - length 'l'
// - contents 'c'
//
// The contents of the string can be either a JavaScript array or
// a JavaScript string. The length of this string can be less than the
// length of the OCaml string. In this case, remaining bytes are
// assumed to be zeroes. Arrays are mutable but consumes more memory
// than strings. A common pattern is to start from an empty string and
// progressively fill it from the start. Partial strings makes it
// possible to implement this efficiently.
//
// When converting to and from UTF-16, we keep track of whether the
// string is composed only of ASCII characters (in which case, no
// conversion needs to be performed) or not.
//
// The string tag can thus take the following values:
//   full string     BYTE | UNKNOWN:      0
//                   BYTE | ASCII:        9
//                   BYTE | NOT_ASCII:    8
//   string prefix   PARTIAL:             2
//   array           ARRAY:               4
//
// One can use bit masking to discriminate these different cases:
//   known_encoding(x) = x&8
//   is_ascii(x) =       x&1
//   kind(x) =           x&6

import { caml_failwith, caml_invalid_argument } from './fail.js';
import { caml_int64_of_bytes, caml_int64_to_bytes } from './int64.js';

//Provides: caml_str_repeat
export function caml_str_repeat(n, s) {
  return s.repeat(n);
}

//Provides: caml_subarray_to_jsbytes
//Weakdef
// Pre ECMAScript 5, [apply] would not support array-like object.
// In such setup, Typed_array would be implemented as polyfill, and [f.apply] would
// fail here. Mark the primitive as Weakdef, so that people can override it easily.
export function caml_subarray_to_jsbytes(a, i, len) {
  var f = String.fromCharCode;
  if (i === 0 && len <= 4096 && len === a.length) return f.apply(null, a);
  var s = "";
  for (; 0 < len; i += 1024, len -= 1024)
    s += f.apply(null, a.slice(i, i + Math.min(len, 1024)));
  return s;
}

//Provides: caml_sub_uint8_array_to_jsbytes
//Weakdef
// Pre ECMAScript 5, [apply] would not support array-like object.
// In such setup, Typed_array would be implemented as polyfill, and [f.apply] would
// fail here. Mark the primitive as Weakdef, so that people can override it easily.
export function caml_sub_uint8_array_to_jsbytes(a, i, len) {
  var f = String.fromCharCode;
  if (i === 0 && len <= 4096 && len === a.length) return f.apply(null, a);
  var s = "";
  for (; 0 < len; i += 1024, len -= 1024)
    s += f.apply(null, a.subarray(i, i + Math.min(len, 1024)));
  return s;
}

//Provides: jsoo_is_ascii
export function jsoo_is_ascii(s) {
  // The regular expression gets better at around this point for all browsers
  if (s.length < 24) {
    // Spidermonkey gets much slower when s.length >= 24 (on 64 bit archs)
    for (var i = 0; i < s.length; i++) if (s.charCodeAt(i) > 127) return false;
    return true;
  }
  // biome-ignore lint/suspicious/noControlCharactersInRegex: expected
  else return !/[^\x00-\x7f]/.test(s);
}

//Provides: caml_bytes_unsafe_get mutable
export function caml_bytes_unsafe_get(s, i) {
  switch (s.t & 6) {
    case 0 /* BYTES */:
      return s.c.charCodeAt(i);
    case 2 /* PARTIAL */:
      if (i >= s.c.length) return 0;
      return s.c.charCodeAt(i);
    case 4 /* ARRAY */:
      return s.c[i];
  }
}

//Provides: caml_bytes_unsafe_set
export function caml_bytes_unsafe_set(s, i, c) {
  // The OCaml compiler uses Char.unsafe_chr on integers larger than 255!
  c &= 0xff;
  if (s.t !== 4 /* ARRAY */) {
    if (i === s.c.length) {
      s.c += String.fromCharCode(c);
      if (i + 1 === s.l) s.t = 0; /*BYTES | UNKNOWN*/
      return 0;
    }
    caml_convert_bytes_to_array(s);
  }
  s.c[i] = c;
  return 0;
}

//Provides: caml_string_bound_error
export function caml_string_bound_error() {
  caml_invalid_argument("index out of bounds");
}

//Provides: caml_bytes_bound_error
export function caml_bytes_bound_error() {
  caml_invalid_argument("index out of bounds");
}

//Provides: caml_string_get
export function caml_string_get(s, i) {
  if (i >>> 0 >= caml_ml_string_length(s)) caml_string_bound_error();
  return caml_string_unsafe_get(s, i);
}

//Provides: caml_string_get16
export function caml_string_get16(s, i) {
  if (i >>> 0 >= caml_ml_string_length(s) - 1) caml_string_bound_error();
  var b1 = caml_string_unsafe_get(s, i),
    b2 = caml_string_unsafe_get(s, i + 1);
  return (b2 << 8) | b1;
}

//Provides: caml_bytes_get16
export function caml_bytes_get16(s, i) {
  if (i >>> 0 >= s.l - 1) caml_bytes_bound_error();
  var b1 = caml_bytes_unsafe_get(s, i),
    b2 = caml_bytes_unsafe_get(s, i + 1);
  return (b2 << 8) | b1;
}

//Provides: caml_string_get32
export function caml_string_get32(s, i) {
  if (i >>> 0 >= caml_ml_string_length(s) - 3) caml_string_bound_error();
  var b1 = caml_string_unsafe_get(s, i),
    b2 = caml_string_unsafe_get(s, i + 1),
    b3 = caml_string_unsafe_get(s, i + 2),
    b4 = caml_string_unsafe_get(s, i + 3);
  return (b4 << 24) | (b3 << 16) | (b2 << 8) | b1;
}

//Provides: caml_bytes_get32
export function caml_bytes_get32(s, i) {
  if (i >>> 0 >= s.l - 3) caml_bytes_bound_error();
  var b1 = caml_bytes_unsafe_get(s, i),
    b2 = caml_bytes_unsafe_get(s, i + 1),
    b3 = caml_bytes_unsafe_get(s, i + 2),
    b4 = caml_bytes_unsafe_get(s, i + 3);
  return (b4 << 24) | (b3 << 16) | (b2 << 8) | b1;
}

//Provides: caml_string_get64
export function caml_string_get64(s, i) {
  if (i >>> 0 >= caml_ml_string_length(s) - 7) caml_string_bound_error();
  var a = new Array(8);
  for (var j = 0; j < 8; j++) {
    a[7 - j] = caml_string_unsafe_get(s, i + j);
  }
  return caml_int64_of_bytes(a);
}

//Provides: caml_bytes_get64
export function caml_bytes_get64(s, i) {
  if (i >>> 0 >= s.l - 7) caml_bytes_bound_error();
  var a = new Array(8);
  for (var j = 0; j < 8; j++) {
    a[7 - j] = caml_bytes_unsafe_get(s, i + j);
  }
  return caml_int64_of_bytes(a);
}

//Provides: caml_bytes_get
export function caml_bytes_get(s, i) {
  if (i >>> 0 >= s.l) caml_bytes_bound_error();
  return caml_bytes_unsafe_get(s, i);
}

//Provides: caml_string_set
//If: js-string
export function caml_string_set$js_string(_s, _i, _c) {
  caml_failwith("caml_string_set");
}

//Provides: caml_string_set
//If: !js-string
export function caml_string_set$no_js_string(s, i, c) {
  if (i >>> 0 >= s.l) caml_string_bound_error();
  return caml_string_unsafe_set(s, i, c);
}

//Provides: caml_bytes_set16
export function caml_bytes_set16(s, i, i16) {
  if (i >>> 0 >= s.l - 1) caml_bytes_bound_error();
  var b2 = 0xff & (i16 >> 8),
    b1 = 0xff & i16;
  caml_bytes_unsafe_set(s, i + 0, b1);
  caml_bytes_unsafe_set(s, i + 1, b2);
  return 0;
}

//Provides: caml_bytes_set32
export function caml_bytes_set32(s, i, i32) {
  if (i >>> 0 >= s.l - 3) caml_bytes_bound_error();
  var b4 = 0xff & (i32 >> 24),
    b3 = 0xff & (i32 >> 16),
    b2 = 0xff & (i32 >> 8),
    b1 = 0xff & i32;
  caml_bytes_unsafe_set(s, i + 0, b1);
  caml_bytes_unsafe_set(s, i + 1, b2);
  caml_bytes_unsafe_set(s, i + 2, b3);
  caml_bytes_unsafe_set(s, i + 3, b4);
  return 0;
}

//Provides: caml_bytes_set64
export function caml_bytes_set64(s, i, i64) {
  if (i >>> 0 >= s.l - 7) caml_bytes_bound_error();
  var a = caml_int64_to_bytes(i64);
  for (var j = 0; j < 8; j++) {
    caml_bytes_unsafe_set(s, i + 7 - j, a[j]);
  }
  return 0;
}

//Provides: caml_bytes_set
export function caml_bytes_set(s, i, c) {
  if (i >>> 0 >= s.l) caml_bytes_bound_error();
  return caml_bytes_unsafe_set(s, i, c);
}

//Provides: jsoo_text_encoder
export var jsoo_text_encoder = new TextEncoder();

//Provides: jsoo_text_decoder
export var jsoo_text_decoder = new TextDecoder();

//Provides: caml_bytes_of_utf16_jsstring
export function caml_bytes_of_utf16_jsstring(s) {
  if (jsoo_is_ascii(s)) {
    return new MlBytes(9, s, s.length);
  } else {
    var a = jsoo_text_encoder.encode(s);
    return new MlBytes(4, a, a.length);
  }
}

//Provides: MlBytes
export class MlBytes {
  constructor(tag, contents, length) {
    this.t = tag;
    this.c = contents;
    this.l = length;
  }

  toString() {
    switch (this.t) {
      case 9: /*BYTES | ASCII*/
      case 8 /*BYTES | NOT_ASCII*/:
        return this.c;
      case 4: /* ARRAY */
      case 2 /* PARTIAL */:
        // biome-ignore lint/suspicious/noFallthroughSwitchClause:
        caml_convert_string_to_bytes(this);
      // fallthrough
      case 0 /*BYTES | UNKNOWN*/:
        if (jsoo_is_ascii(this.c)) this.t = 9; /*BYTES | ASCII*/
        else this.t = 8; /*BYTES | NOT_ASCII*/
        return this.c;
    }
  }

  toUtf16() {
    if (this.t === 9) return this.c;
    var a = caml_uint8_array_of_bytes(this);
    return jsoo_text_decoder.decode(a);
  }

  slice() {
    var content = this.t === 4 ? this.c.slice() : this.c;
    return new MlBytes(this.t, content, this.l);
  }
}

//Provides: caml_convert_string_to_bytes
export function caml_convert_string_to_bytes(s) {
  /* Assumes not BYTES */
  if (s.t === 2 /* PARTIAL */) s.c += caml_str_repeat(s.l - s.c.length, "\0");
  else s.c = caml_sub_uint8_array_to_jsbytes(s.c, 0, s.c.length);
  s.t = 0; /*BYTES | UNKNOWN*/
}

//Provides: caml_convert_bytes_to_array
export function caml_convert_bytes_to_array(s) {
  /* Assumes not ARRAY */
  var a = new Uint8Array(s.l);
  var b = s.c,
    l = b.length,
    i = 0;
  for (; i < l; i++) a[i] = b.charCodeAt(i);
  for (l = s.l; i < l; i++) a[i] = 0;
  s.c = a;
  s.t = 4; /* ARRAY */
  return a;
}

//Provides: caml_uint8_array_of_bytes mutable
export function caml_uint8_array_of_bytes(s) {
  if (s.t !== 4 /* ARRAY */) caml_convert_bytes_to_array(s);
  return s.c;
}

//Provides: caml_uint8_array_of_string mutable
export function caml_uint8_array_of_string(s) {
  var l = caml_ml_string_length(s);
  var a = new Uint8Array(l);
  var i = 0;
  for (; i < l; i++) a[i] = caml_string_unsafe_get(s, i);
  return a;
}

//Provides: caml_create_string const
//If: !js-string
export function caml_create_string$no_js_string(len) {
  if (len < 0) caml_invalid_argument("String.create");
  return new MlBytes(len ? 2 : 9, "", len);
}

//Provides: caml_create_string const
//If: js-string
export function caml_create_string$js_string(_len) {
  caml_invalid_argument("String.create");
}

//Provides: caml_create_bytes const
export function caml_create_bytes(len) {
  if (len < 0) caml_invalid_argument("Bytes.create");
  return new MlBytes(len ? 2 : 9, "", len);
}

//Provides: caml_string_of_array
export function caml_string_of_array(a) {
  return caml_string_of_jsbytes(caml_subarray_to_jsbytes(a, 0, a.length));
}

//Provides: caml_string_of_uint8_array
//If: js-string
export function caml_string_of_uint8_array$js_string(a) {
  return caml_sub_uint8_array_to_jsbytes(a, 0, a.length);
}

//Provides: caml_string_of_uint8_array
//If: !js-string
export function caml_string_of_uint8_array$no_js_string(a) {
  return caml_bytes_of_uint8_array(a.slice());
}

//Provides: caml_bytes_of_array
export function caml_bytes_of_array(a) {
  if (!(a instanceof Uint8Array)) {
    a = new Uint8Array(a);
  }
  return new MlBytes(4, a, a.length);
}

//Provides: caml_bytes_of_uint8_array
export function caml_bytes_of_uint8_array(a) {
  return new MlBytes(4, a, a.length);
}

//Provides: caml_bytes_compare mutable
export function caml_bytes_compare(s1, s2) {
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c < s2.c ? -1 : s1.c > s2.c ? 1 : 0;
}

//Provides: caml_bytes_equal mutable (const, const)
export function caml_bytes_equal(s1, s2) {
  if (s1 === s2) return 1;
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c === s2.c ? 1 : 0;
}

//Provides: caml_string_notequal mutable (const, const)
export function caml_string_notequal(s1, s2) {
  return 1 - caml_string_equal(s1, s2);
}

//Provides: caml_bytes_notequal mutable (const, const)
export function caml_bytes_notequal(s1, s2) {
  return 1 - caml_bytes_equal(s1, s2);
}

//Provides: caml_bytes_lessequal mutable
export function caml_bytes_lessequal(s1, s2) {
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c <= s2.c ? 1 : 0;
}

//Provides: caml_bytes_lessthan mutable
export function caml_bytes_lessthan(s1, s2) {
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return s1.c < s2.c ? 1 : 0;
}

//Provides: caml_string_greaterequal
export function caml_string_greaterequal(s1, s2) {
  return caml_string_lessequal(s2, s1);
}
//Provides: caml_bytes_greaterequal
export function caml_bytes_greaterequal(s1, s2) {
  return caml_bytes_lessequal(s2, s1);
}

//Provides: caml_string_greaterthan
export function caml_string_greaterthan(s1, s2) {
  return caml_string_lessthan(s2, s1);
}

//Provides: caml_bytes_greaterthan
export function caml_bytes_greaterthan(s1, s2) {
  return caml_bytes_lessthan(s2, s1);
}

//Provides: caml_fill_bytes
//Alias: caml_fill_string
export function caml_fill_bytes(s, i, l, c) {
  if (l > 0) {
    if (i === 0 && (l >= s.l || (s.t === 2 /* PARTIAL */ && l >= s.c.length))) {
      if (c === 0) {
        s.c = "";
        s.t = 2; /* PARTIAL */
      } else {
        s.c = caml_str_repeat(l, String.fromCharCode(c));
        s.t = l === s.l ? 0 /* BYTES | UNKNOWN */ : 2; /* PARTIAL */
      }
    } else {
      if (s.t !== 4 /* ARRAY */) caml_convert_bytes_to_array(s);
      for (l += i; i < l; i++) s.c[i] = c;
    }
  }
  return 0;
}

//Provides: caml_blit_bytes
export function caml_blit_bytes(s1, i1, s2, i2, len) {
  if (len === 0) return 0;
  if (
    i2 === 0 &&
    (len >= s2.l || (s2.t === 2 /* PARTIAL */ && len >= s2.c.length))
  ) {
    s2.c =
      s1.t === 4 /* ARRAY */
        ? caml_sub_uint8_array_to_jsbytes(s1.c, i1, len)
        : i1 === 0 && s1.c.length === len
          ? s1.c
          : s1.c.slice(i1, i1 + len);
    s2.t = s2.c.length === s2.l ? 0 /* BYTES | UNKNOWN */ : 2; /* PARTIAL */
  } else if (s2.t === 2 /* PARTIAL */ && i2 === s2.c.length) {
    s2.c +=
      s1.t === 4 /* ARRAY */
        ? caml_sub_uint8_array_to_jsbytes(s1.c, i1, len)
        : i1 === 0 && s1.c.length === len
          ? s1.c
          : s1.c.slice(i1, i1 + len);
    s2.t = s2.c.length === s2.l ? 0 /* BYTES | UNKNOWN */ : 2; /* PARTIAL */
  } else {
    if (s2.t !== 4 /* ARRAY */) caml_convert_bytes_to_array(s2);
    var c1 = s1.c,
      c2 = s2.c;
    if (s1.t === 4 /* ARRAY */) {
      if (i2 <= i1) {
        for (var i = 0; i < len; i++) c2[i2 + i] = c1[i1 + i];
      } else {
        for (var i = len - 1; i >= 0; i--) c2[i2 + i] = c1[i1 + i];
      }
    } else {
      var l = Math.min(len, c1.length - i1);
      for (var i = 0; i < l; i++) c2[i2 + i] = c1.charCodeAt(i1 + i);
      for (; i < len; i++) c2[i2 + i] = 0;
    }
  }
  return 0;
}

//Provides: caml_blit_string
export function caml_blit_string(a, b, c, d, e) {
  caml_blit_bytes(caml_bytes_of_string(a), b, c, d, e);
  return 0;
}

//Provides: caml_ml_bytes_length const
export function caml_ml_bytes_length(s) {
  return s.l;
}

//Provides: caml_string_concat const
//If: js-string
export function caml_string_concat$js_string(a, b) {
  return a + b;
}

//Provides: caml_string_concat const
//If: !js-string
export function caml_string_concat$no_js_string(s1, s2) {
  s1.t & 6 && caml_convert_string_to_bytes(s1);
  s2.t & 6 && caml_convert_string_to_bytes(s2);
  return new MlBytes(0, s1.c + s2.c, s1.l + s2.l);
}

//Provides: caml_string_unsafe_get const
//If: js-string
export function caml_string_unsafe_get$js_string(s, i) {
  return s.charCodeAt(i);
}

//Provides: caml_ml_string_length const
//If: js-string
export function caml_ml_string_length$js_string(s) {
  return s.length;
}

//Provides: caml_string_compare const
//If: js-string
export function caml_string_compare$js_string(s1, s2) {
  return s1 < s2 ? -1 : s1 > s2 ? 1 : 0;
}

//Provides: caml_string_equal const
//If: js-string
export function caml_string_equal$js_string(s1, s2) {
  if (s1 === s2) return 1;
  return 0;
}

//Provides: caml_string_lessequal const
//If: js-string
export function caml_string_lessequal$js_string(s1, s2) {
  return s1 <= s2 ? 1 : 0;
}

//Provides: caml_string_lessthan const
//If: js-string
export function caml_string_lessthan$js_string(s1, s2) {
  return s1 < s2 ? 1 : 0;
}

//Provides: caml_string_of_bytes
//If: js-string
export function caml_string_of_bytes$js_string(s) {
  s.t & 6 && caml_convert_string_to_bytes(s);
  return caml_string_of_jsbytes(s.c);
}

//Provides: caml_bytes_of_string const
//If: js-string
export function caml_bytes_of_string$js_string(s) {
  return caml_bytes_of_jsbytes(caml_jsbytes_of_string(s));
}

//Provides: caml_string_of_jsbytes const
//If: js-string
export function caml_string_of_jsbytes$js_string(x) {
  return x;
}

//Provides: caml_jsbytes_of_string const
//If: js-string
export function caml_jsbytes_of_string$js_string(x) {
  return x;
}

//Provides: jsoo_text_decoder_buff
export var jsoo_text_decoder_buff = new ArrayBuffer(1024);

//Provides: caml_jsstring_of_string const
//If: js-string
export function caml_jsstring_of_string$js_string(s) {
  if (jsoo_is_ascii(s)) return s;
  var a =
    s.length <= jsoo_text_decoder_buff.length
      ? new Uint8Array(jsoo_text_decoder_buff, 0, s.length)
      : new Uint8Array(s.length);
  for (var i = 0; i < s.length; i++) {
    a[i] = s.charCodeAt(i);
  }
  return jsoo_text_decoder.decode(a);
}

//Provides: caml_string_of_jsstring const
//If: js-string
export function caml_string_of_jsstring$js_string(s) {
  if (jsoo_is_ascii(s)) return caml_string_of_jsbytes(s);
  var a = jsoo_text_encoder.encode(s);
  return caml_string_of_array(a);
}

//Provides: caml_bytes_of_jsbytes const
export function caml_bytes_of_jsbytes(s) {
  return new MlBytes(0, s, s.length);
}

// The section below should be used when use-js-string=false

//Provides: caml_string_unsafe_get const
//If: !js-string
export function caml_string_unsafe_get$no_js_string(s, i) {
  return caml_bytes_unsafe_get(s, i);
}

//Provides: caml_string_unsafe_set
//If: !js-string
export function caml_string_unsafe_set$no_js_string(s, i, c) {
  return caml_bytes_unsafe_set(s, i, c);
}

//Provides: caml_ml_string_length const
//If: !js-string
export function caml_ml_string_length$no_js_string(s) {
  return caml_ml_bytes_length(s);
}

//Provides: caml_string_compare
//If: !js-string
export function caml_string_compare$no_js_string(s1, s2) {
  return caml_bytes_compare(s1, s2);
}

//Provides: caml_string_equal
//If: !js-string
export function caml_string_equal$no_js_string(s1, s2) {
  return caml_bytes_equal(s1, s2);
}

//Provides: caml_string_lessequal
//If: !js-string
export function caml_string_lessequal$no_js_string(s1, s2) {
  return caml_bytes_lessequal(s1, s2);
}

//Provides: caml_string_lessthan
//If: !js-string
export function caml_string_lessthan$no_js_string(s1, s2) {
  return caml_bytes_lessthan(s1, s2);
}

//Provides: caml_string_of_bytes
//If: !js-string
export function caml_string_of_bytes$no_js_string(s) {
  return s;
}

//Provides: caml_bytes_of_string const
//If: !js-string
export function caml_bytes_of_string$no_js_string(s) {
  return s;
}

//Provides: caml_string_of_jsbytes const
//If: !js-string
export function caml_string_of_jsbytes$no_js_string(s) {
  return caml_bytes_of_jsbytes(s);
}

//Provides: caml_jsbytes_of_string const
//If: !js-string
export function caml_jsbytes_of_string$no_js_string(s) {
  s.t & 6 && caml_convert_string_to_bytes(s);
  return s.c;
}

//Provides: caml_jsstring_of_string mutable (const)
//If: !js-string
export function caml_jsstring_of_string$no_js_string(s) {
  return s.toUtf16();
}

//Provides: caml_string_of_jsstring
//If: !js-string
export function caml_string_of_jsstring$no_js_string(s) {
  return caml_bytes_of_utf16_jsstring(s);
}

//Provides: caml_is_ml_bytes
export function caml_is_ml_bytes(s) {
  return s instanceof MlBytes;
}

//Provides: caml_ml_bytes_content
//Returns a (full) string of bytes or an array
export function caml_ml_bytes_content(s) {
  switch (s.t & 6) {
    case 2 /* PARTIAL */:
      caml_convert_string_to_bytes(s);
      return s.c;
    default: /* BYTES or ARRAY */
      return s.c;
  }
}

//Provides: caml_is_ml_string
//If: js-string
export function caml_is_ml_string$js_string(s) {
  // biome-ignore lint/suspicious/noControlCharactersInRegex: expected
  return typeof s === "string" && !/[^\x00-\xff]/.test(s);
}

//Provides: caml_is_ml_string
//If: !js-string
export function caml_is_ml_string$no_js_string(s) {
  return caml_is_ml_bytes(s);
}

// The functions below are deprecated

//Provides: caml_js_to_byte_string const
//Deprecated: Use [caml_string_of_jsbytes] instead
export function caml_js_to_byte_string(s) {
  return caml_string_of_jsbytes(s);
}

//Provides: caml_js_from_string mutable (const)
//Deprecated: Use [caml_jsstring_of_string] instead
export function caml_js_from_string(s) {
  return caml_jsstring_of_string(s);
}

//Provides: caml_to_js_string mutable (const)
//Deprecated: Use [caml_jsstring_of_string] instead
export function caml_to_js_string(s) {
  return caml_jsstring_of_string(s);
}

//Provides: caml_js_to_string const
//Deprecated: Use [caml_string_of_jsstring] instead
export function caml_js_to_string(s) {
  return caml_string_of_jsstring(s);
}

//Provides: caml_array_of_string
//Deprecated: Use [caml_uint8_array_of_string] instead
export function caml_array_of_string(x) {
  return caml_uint8_array_of_string(x);
}

//Provides: caml_array_of_bytes
//Deprecated: Use [caml_uint8_array_of_bytes] instead
export function caml_array_of_bytes(x) {
  return caml_uint8_array_of_bytes(x);
}

//Provides: caml_new_string
//Deprecated: Use [caml_string_of_jsbytes] instead
export function caml_new_string(s) {
  return caml_string_of_jsbytes(s);
}

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

//Provides: caml_format_int const (const, const)
//Requires: caml_parse_format, caml_finish_formatting, caml_str_repeat
//Requires: caml_string_of_jsbytes, caml_jsbytes_of_string
//Alias: caml_int32_format
//Alias: caml_nativeint_format
function caml_format_int(fmt, i) {
  if (caml_jsbytes_of_string(fmt) === "%d")
    return caml_string_of_jsbytes("" + i);
  var f = caml_parse_format(fmt);
  if (i < 0) {
    if (f.signedconv) {
      f.sign = -1;
      i = -i;
    } else i >>>= 0;
  }
  var s = i.toString(f.base);
  if (f.prec >= 0) {
    f.filler = " ";
    var n = f.prec - s.length;
    if (n > 0) s = caml_str_repeat(n, "0") + s;
  }
  return caml_finish_formatting(f, s);
}

//Provides: caml_parse_sign_and_base
//Requires: caml_string_unsafe_get, caml_ml_string_length
function caml_parse_sign_and_base(s) {
  var i = 0,
    len = caml_ml_string_length(s),
    base = 10,
    sign = 1,
    signedness = 1;
  if (len > 0) {
    switch (caml_string_unsafe_get(s, i)) {
      case 45:
        i++;
        sign = -1;
        break;
      case 43:
        i++;
        sign = 1;
        break;
    }
  }
  if (i + 1 < len && caml_string_unsafe_get(s, i) === 48)
    switch (caml_string_unsafe_get(s, i + 1)) {
      case 120:
      case 88:
        signedness = 0;
        base = 16;
        i += 2;
        break;
      case 111:
      case 79:
        signedness = 0;
        base = 8;
        i += 2;
        break;
      case 98:
      case 66:
        signedness = 0;
        base = 2;
        i += 2;
        break;
      case 117:
      case 85:
        signedness = 0;
        i += 2;
        break;
    }
  return [i, sign, base, signedness];
}

//Provides: caml_parse_digit
function caml_parse_digit(c) {
  if (c >= 48 && c <= 57) return c - 48;
  if (c >= 65 && c <= 90) return c - 55;
  if (c >= 97 && c <= 122) return c - 87;
  return -1;
}

//Provides: caml_int_of_string (const)
//Requires: caml_ml_string_length, caml_string_unsafe_get
//Requires: caml_parse_sign_and_base, caml_parse_digit, caml_failwith
//Alias: caml_int32_of_string
//Alias: caml_nativeint_of_string
function caml_int_of_string(s) {
  var r = caml_parse_sign_and_base(s);
  var i = r[0],
    sign = r[1],
    base = r[2],
    signedness = r[3];
  var len = caml_ml_string_length(s);
  var threshold = -1 >>> 0;
  var c = i < len ? caml_string_unsafe_get(s, i) : 0;
  var d = caml_parse_digit(c);
  if (d < 0 || d >= base) caml_failwith("int_of_string");
  var res = d;
  for (i++; i < len; i++) {
    c = caml_string_unsafe_get(s, i);
    if (c === 95) continue;
    d = caml_parse_digit(c);
    if (d < 0 || d >= base) break;
    res = base * res + d;
    if (res > threshold) caml_failwith("int_of_string");
  }
  if (i !== len) caml_failwith("int_of_string");
  // For base different from 10, we expect an unsigned representation,
  // hence any value of 'res' (less than 'threshold') is acceptable.
  // But we have to convert the result back to a signed integer.
  res = sign * res;
  if (signedness && (res | 0) !== res)
    /* Signed representation expected, allow -2^(nbits-1) to 2^(nbits-1) - 1 */
    caml_failwith("int_of_string");
  return res | 0;
}

//Provides: caml_mul const
//Alias: caml_int32_mul
//Alias: caml_nativeint_mul
//Alias: %int_mul
function caml_mul(a, b) {
  return Math.imul(a, b);
}

//Provides: caml_div
//Requires: caml_raise_zero_divide
//Alias: caml_int32_div
//Alias: caml_nativeint_div
//Alias: %int_div
function caml_div(x, y) {
  if (y === 0) caml_raise_zero_divide();
  return (x / y) | 0;
}

//Provides: caml_mod
//Requires: caml_raise_zero_divide
//Alias: caml_int32_mod
//Alias: caml_nativeint_mod
//Alias: %int_mod
function caml_mod(x, y) {
  if (y === 0) caml_raise_zero_divide();
  return x % y;
}

//Provides: caml_bswap16 const
function caml_bswap16(x) {
  return ((x & 0x00ff) << 8) | ((x & 0xff00) >> 8);
}

//Provides: caml_int32_bswap const
//Alias: caml_nativeint_bswap
function caml_int32_bswap(x) {
  return (
    ((x & 0x000000ff) << 24) |
    ((x & 0x0000ff00) << 8) |
    ((x & 0x00ff0000) >>> 8) |
    ((x & 0xff000000) >>> 24)
  );
}
//Provides: caml_int64_bswap const
//Requires: caml_int64_to_bytes, caml_int64_of_bytes
function caml_int64_bswap(x) {
  var y = caml_int64_to_bytes(x);
  return caml_int64_of_bytes([y[7], y[6], y[5], y[4], y[3], y[2], y[1], y[0]]);
}

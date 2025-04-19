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

//Provides: jsoo_dataview
var jsoo_dataview = new DataView(new ArrayBuffer(8));

//Provides: caml_int64_bits_of_float const
//Requires: caml_int64_create_lo_mi_hi
//Requires: jsoo_dataview
function caml_int64_bits_of_float(x) {
  jsoo_dataview.setFloat64(0, x, true);
  var lo32 = jsoo_dataview.getUint32(0, true);
  var hi32 = jsoo_dataview.getUint32(4, true);
  var r1 = lo32 & 0xffffff;
  var r2 = (lo32 >>> 24) | ((hi32 << 8) & 0xffffff);
  var r3 = (hi32 >>> 16) & 0xffff;
  return caml_int64_create_lo_mi_hi(r1, r2, r3);
}

//Provides: caml_int32_bits_of_float const
//Requires: jsoo_dataview
function caml_int32_bits_of_float(x) {
  jsoo_dataview.setFloat32(0, x, true);
  return jsoo_dataview.getUint32(0, true) | 0;
}

//FP literals can be written using the hexadecimal
//notation 0x<mantissa in hex>p<exponent> from ISO C99.
//https://github.com/dankogai/js-hexfloat/blob/master/hexfloat.js
//Provides: caml_hexstring_of_float const
//Requires: caml_string_of_jsstring, caml_str_repeat
function caml_hexstring_of_float(x, prec, style) {
  if (!Number.isFinite(x)) {
    if (Number.isNaN(x)) return caml_string_of_jsstring("nan");
    return caml_string_of_jsstring(x > 0 ? "infinity" : "-infinity");
  }
  var sign = x === 0 && 1 / x === Number.NEGATIVE_INFINITY ? 1 : x >= 0 ? 0 : 1;
  if (sign) x = -x;
  var exp = 0;
  if (x === 0) {
  } else if (x < 1) {
    while (x < 1 && exp > -1022) {
      x *= 2;
      exp--;
    }
  } else {
    while (x >= 2) {
      x /= 2;
      exp++;
    }
  }
  var exp_sign = exp < 0 ? "" : "+";
  var sign_str = "";
  if (sign) sign_str = "-";
  else {
    switch (style) {
      case 43 /* '+' */:
        sign_str = "+";
        break;
      case 32 /* ' ' */:
        sign_str = " ";
        break;
      default:
        break;
    }
  }
  if (prec >= 0 && prec < 13) {
    /* If a precision is given, and is small, round mantissa accordingly */
    var cst = Math.pow(2, prec * 4);
    x = Math.round(x * cst) / cst;
  }
  var x_str = x.toString(16);
  if (prec >= 0) {
    var idx = x_str.indexOf(".");
    if (idx < 0) {
      x_str += "." + caml_str_repeat(prec, "0");
    } else {
      var size = idx + 1 + prec;
      if (x_str.length < size)
        x_str += caml_str_repeat(size - x_str.length, "0");
      else x_str = x_str.slice(0, size);
    }
  }
  return caml_string_of_jsstring(
    sign_str + "0x" + x_str + "p" + exp_sign + exp.toString(10),
  );
}

//Provides: caml_int64_float_of_bits const
//Requires: jsoo_dataview
function caml_int64_float_of_bits(x) {
  var lo = x.lo;
  var mi = x.mi;
  var hi = x.hi;
  jsoo_dataview.setUint32(0, lo | (mi << 24), true);
  jsoo_dataview.setUint32(4, (mi >>> 8) | (hi << 16), true);
  return jsoo_dataview.getFloat64(0, true);
}

//Provides: caml_nextafter_float const
//Requires: caml_int64_float_of_bits, caml_int64_bits_of_float, caml_int64_add, caml_int64_sub,caml_int64_of_int32
function caml_nextafter_float(x, y) {
  if (Number.isNaN(x) || Number.isNaN(y)) return Number.NaN;
  if (x === y) return y;
  if (x === 0) {
    if (y < 0) return -Math.pow(2, -1074);
    else return Math.pow(2, -1074);
  }
  var bits = caml_int64_bits_of_float(x);
  var one = caml_int64_of_int32(1);
  if (x < y === x > 0) bits = caml_int64_add(bits, one);
  else bits = caml_int64_sub(bits, one);
  return caml_int64_float_of_bits(bits);
}

//Provides: caml_trunc_float const
function caml_trunc_float(x) {
  return Math.trunc(x);
}

//Provides: caml_int32_float_of_bits const
//Requires: jsoo_dataview
function caml_int32_float_of_bits(x) {
  jsoo_dataview.setUint32(0, x, true);
  return jsoo_dataview.getFloat32(0, true);
}

//Provides: caml_classify_float const
function caml_classify_float(x) {
  if (Number.isFinite(x)) {
    if (Math.abs(x) >= 2.2250738585072014e-308) return 0;
    if (x !== 0) return 1;
    return 2;
  }
  return Number.isNaN(x) ? 4 : 3;
}
//Provides: caml_modf_float const
function caml_modf_float(x) {
  if (Number.isFinite(x)) {
    var neg = 1 / x < 0;
    x = Math.abs(x);
    var i = Math.floor(x);
    var f = x - i;
    if (neg) {
      i = -i;
      f = -f;
    }
    return [0, f, i];
  }
  if (Number.isNaN(x)) return [0, Number.NaN, Number.NaN];
  return [0, 1 / x, x];
}
//Provides: caml_ldexp_float const
function caml_ldexp_float(x, exp) {
  exp |= 0;
  if (exp > 1023) {
    exp -= 1023;
    x *= Math.pow(2, 1023);
    if (exp > 1023) {
      // in case x is subnormal
      exp -= 1023;
      x *= Math.pow(2, 1023);
    }
  }
  if (exp < -1023) {
    exp += 1023;
    x *= Math.pow(2, -1023);
  }
  x *= Math.pow(2, exp);
  return x;
}
//Provides: caml_frexp_float const
function caml_frexp_float(x) {
  if (x === 0 || !Number.isFinite(x)) return [0, x, 0];
  var neg = x < 0;
  if (neg) x = -x;
  var exp = Math.max(-1023, Math.floor(Math.log2(x)) + 1);
  x *= Math.pow(2, -exp);
  while (x < 0.5) {
    x *= 2;
    exp--;
  }
  while (x >= 1) {
    x *= 0.5;
    exp++;
  }
  if (neg) x = -x;
  return [0, x, exp];
}

//Provides: caml_float_compare const
function caml_float_compare(x, y) {
  if (x === y) return 0;
  if (x < y) return -1;
  if (x > y) return 1;
  if (!Number.isNaN(x)) return 1;
  if (!Number.isNaN(y)) return -1;
  return 0;
}

//Provides: caml_copysign_float const
function caml_copysign_float(x, y) {
  if (y === 0) y = 1 / y;
  x = Math.abs(x);
  return y < 0 ? -x : x;
}

//Provides: caml_signbit_float const
//Alias: caml_signbit
function caml_signbit_float(x) {
  if (x === 0) x = 1 / x;
  return x < 0 ? 1 : 0;
}

//Provides: caml_expm1_float const
function caml_expm1_float(x) {
  return Math.expm1(x);
}
//Provides: caml_exp2_float const
function caml_exp2_float(x) {
  return Math.pow(2, x);
}
//Provides: caml_log1p_float const
function caml_log1p_float(x) {
  return Math.log1p(x);
}
//Provides: caml_log2_float const
function caml_log2_float(x) {
  return Math.log2(x);
}
//Provides: caml_hypot_float const
function caml_hypot_float(x, y) {
  return Math.hypot(x, y);
}
//Provides: caml_log10_float const
function caml_log10_float(x) {
  return Math.log10(x);
}
//Provides: caml_cosh_float const
function caml_cosh_float(x) {
  return Math.cosh(x);
}
//Provides: caml_acosh_float const
function caml_acosh_float(x) {
  return Math.acosh(x);
}
//Provides: caml_sinh_float const
function caml_sinh_float(x) {
  return Math.sinh(x);
}
//Provides: caml_asinh_float const
function caml_asinh_float(x) {
  return Math.asinh(x);
}
//Provides: caml_tanh_float const
function caml_tanh_float(x) {
  return Math.tanh(x);
}
//Provides: caml_atanh_float const
function caml_atanh_float(x) {
  return Math.atanh(x);
}
//Provides: caml_round_float const
function caml_round_float(x) {
  if (x >= 0) {
    var y = Math.floor(x);
    return x - y >= 0.5 ? y + 1 : y;
  } else {
    var y = Math.ceil(x);
    return y - x >= 0.5 ? y - 1 : y;
  }
}
//Provides: caml_cbrt_float const
function caml_cbrt_float(x) {
  return Math.cbrt(x);
}

//Provides: caml_erf_float const
function caml_erf_float(x) {
  var a1 = 0.254829592;
  var a2 = -0.284496736;
  var a3 = 1.421413741;
  var a4 = -1.453152027;
  var a5 = 1.061405429;
  var p = 0.3275911;

  var sign = 1;
  if (x < 0) {
    sign = -1;
  }
  x = Math.abs(x);
  var t = 1.0 / (1.0 + p * x);
  var y =
    1.0 - ((((a5 * t + a4) * t + a3) * t + a2) * t + a1) * t * Math.exp(-x * x);

  return sign * y;
}

//Provides: caml_erfc_float const
//Requires: caml_erf_float
function caml_erfc_float(x) {
  return 1 - caml_erf_float(x);
}

//Provides: caml_fma_float const
function caml_fma_float(x, y, z) {
  var SPLIT = Math.pow(2, 27) + 1;
  var MIN_VALUE = Math.pow(2, -1022);
  var EPSILON = Math.pow(2, -52);
  var C = 416;
  var A = Math.pow(2, +C);
  var B = Math.pow(2, -C);

  function multiply(a, b) {
    var at = SPLIT * a;
    var ahi = at - (at - a);
    var alo = a - ahi;
    var bt = SPLIT * b;
    var bhi = bt - (bt - b);
    var blo = b - bhi;
    var p = a * b;
    var e = ahi * bhi - p + ahi * blo + alo * bhi + alo * blo;
    return {
      p: p,
      e: e,
    };
  }

  function add(a, b) {
    var s = a + b;
    var v = s - a;
    var e = a - (s - v) + (b - v);
    return {
      s: s,
      e: e,
    };
  }

  function adjust(x, y) {
    return x !== 0 && y !== 0 && SPLIT * x - (SPLIT * x - x) === x
      ? x * (1 + (x < 0 ? -1 : +1) * (y < 0 ? -1 : +1) * EPSILON)
      : x;
  }

  if (x === 0 || y === 0 || !Number.isFinite(x) || !Number.isFinite(y)) {
    return x * y + z;
  }
  if (z === 0) {
    return x * y;
  }
  if (!Number.isFinite(z)) {
    return z;
  }

  var scale = 1;
  while (Math.abs(x) > A) {
    scale *= A;
    x *= B;
  }
  while (Math.abs(y) > A) {
    scale *= A;
    y *= B;
  }
  if (scale === 1 / 0) {
    return x * y * scale;
  }
  while (Math.abs(x) < B) {
    scale *= B;
    x *= A;
  }
  while (Math.abs(y) < B) {
    scale *= B;
    y *= A;
  }
  if (scale === 0) {
    return z;
  }

  var xs = x;
  var ys = y;
  var zs = z / scale;

  if (Math.abs(zs) > (Math.abs(xs * ys) * 4) / EPSILON) {
    return z;
  }
  if (Math.abs(zs) < (((Math.abs(xs * ys) * EPSILON) / 4) * EPSILON) / 4) {
    zs = (z < 0 ? -1 : +1) * MIN_VALUE;
  }

  var xy = multiply(xs, ys);
  var s = add(xy.p, zs);
  var u = add(xy.e, s.e);
  var i = add(s.s, u.s);

  var f = i.s + adjust(i.e, u.e);
  if (f === 0) {
    return f;
  }

  var fs = f * scale;
  if (Math.abs(fs) > MIN_VALUE) {
    return fs;
  }

  // It is possible that there was extra rounding for a denormalized value.
  return fs + adjust(f - fs / scale, i.e) * scale;
}

//Provides: caml_format_float const
//Requires: caml_str_repeat, caml_parse_format, caml_finish_formatting
function caml_format_float(fmt, x) {
  function toFixed(x, dp) {
    if (Math.abs(x) < 1.0) {
      return x.toFixed(dp);
    } else {
      var e = Number.parseInt(x.toString().split("+")[1]);
      if (e > 20) {
        e -= 20;
        x /= Math.pow(10, e);
        x += caml_str_repeat(e, "0");
        if (dp > 0) {
          x = x + "." + caml_str_repeat(dp, "0");
        }
        return x;
      } else return x.toFixed(dp);
    }
  }
  var s,
    f = caml_parse_format(fmt);
  var prec = f.prec < 0 ? 6 : f.prec;
  if (x < 0 || (x === 0 && 1 / x === Number.NEGATIVE_INFINITY)) {
    f.sign = -1;
    x = -x;
  }
  if (Number.isNaN(x)) {
    s = "nan";
    f.filler = " ";
  } else if (!Number.isFinite(x)) {
    s = "inf";
    f.filler = " ";
  } else
    switch (f.conv) {
      case "e":
        var s = x.toExponential(prec);
        // exponent should be at least two digits
        var i = s.length;
        if (s.charAt(i - 3) === "e")
          s = s.slice(0, i - 1) + "0" + s.slice(i - 1);
        break;
      case "f":
        s = toFixed(x, prec);
        break;
      case "g":
        prec = prec ? prec : 1;
        s = x.toExponential(prec - 1);
        var j = s.indexOf("e");
        var exp = +s.slice(j + 1);
        if (exp < -4 || x >= 1e21 || x.toFixed(0).length > prec) {
          // remove trailing zeroes
          var i = j - 1;
          while (s.charAt(i) === "0") i--;
          if (s.charAt(i) === ".") i--;
          s = s.slice(0, i + 1) + s.slice(j);
          i = s.length;
          if (s.charAt(i - 3) === "e")
            s = s.slice(0, i - 1) + "0" + s.slice(i - 1);
          break;
        } else {
          var p = prec;
          if (exp < 0) {
            p -= exp + 1;
            s = x.toFixed(p);
          } else while (((s = x.toFixed(p)), s.length > prec + 1)) p--;
          if (p) {
            // remove trailing zeroes
            var i = s.length - 1;
            while (s.charAt(i) === "0") i--;
            if (s.charAt(i) === ".") i--;
            s = s.slice(0, i + 1);
          }
        }
        break;
    }
  return caml_finish_formatting(f, s);
}

//Provides: caml_float_of_string (const)
//Requires: caml_failwith, caml_jsbytes_of_string
function caml_float_of_string(s) {
  var res;
  var r_float = /^ *[-+]?(?:\d*\.?\d+|\d+\.?\d*)(?:[eE][-+]?\d+)?$/;
  s = caml_jsbytes_of_string(s);
  res = +s;
  //Fast path
  if (!Number.isNaN(res) && r_float.test(s)) return res;
  s = s.replace(/_/g, "");
  res = +s;
  if ((!Number.isNaN(res) && r_float.test(s)) || /^[+-]?nan$/i.test(s))
    return res;
  var m = /^ *([+-]?)0x([0-9a-f]+)\.?([0-9a-f]*)(p([+-]?[0-9]+))?$/i.exec(s);
  //          1        2             3           5
  if (m) {
    var m3 = m[3].replace(/0+$/, "");
    var mantissa = Number.parseInt(m[1] + m[2] + m3, 16);
    var exponent = (+m[5] || 0) - 4 * m3.length;
    res = mantissa * Math.pow(2, exponent);
    return res;
  }
  if (/^\+?inf(inity)?$/i.test(s)) return Number.POSITIVE_INFINITY;
  if (/^-inf(inity)?$/i.test(s)) return Number.NEGATIVE_INFINITY;
  caml_failwith("float_of_string");
}

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

//Provides: jsoo_floor_log2
const log2_ok = Math.log2 && Math.log2(1.1235582092889474e307) === 1020;
function jsoo_floor_log2(x) {
  if (log2_ok) return Math.floor(Math.log2(x));
  let i = 0;
  if (x === 0) return Number.NEGATIVE_INFINITY;
  if (x >= 1) {
    while (x >= 2) {
      x /= 2;
      i++;
    }
  } else {
    while (x < 1) {
      x *= 2;
      i--;
    }
  }
  return i;
}

//Provides: caml_int64_bits_of_float const
//Requires: jsoo_floor_log2, caml_int64_create_lo_mi_hi
function caml_int64_bits_of_float(x) {
  if (!Number.isFinite(x)) {
    if (Number.isNaN(x)) return caml_int64_create_lo_mi_hi(1, 0, 0x7ff0);
    if (x > 0) return caml_int64_create_lo_mi_hi(0, 0, 0x7ff0);
    return caml_int64_create_lo_mi_hi(0, 0, 0xfff0);
  }
  const sign =
    x === 0 && 1 / x === Number.NEGATIVE_INFINITY
      ? 0x8000
      : x >= 0
        ? 0
        : 0x8000;
  if (sign) x = -x;
  // Int64.bits_of_float 1.1235582092889474E+307 = 0x7fb0000000000000L
  // using Math.LOG2E*Math.log(x) in place of Math.log2 result in precision lost
  let exp = jsoo_floor_log2(x) + 1023;
  if (exp <= 0) {
    exp = 0;
    x /= 2 ** -1026;
  } else {
    x /= 2 ** (exp - 1027);
    if (x < 16) {
      x *= 2;
      exp -= 1;
    }
    if (exp === 0) {
      x /= 2;
    }
  }
  const k = 2 ** 24;
  let r3 = x | 0;
  x = (x - r3) * k;
  const r2 = x | 0;
  x = (x - r2) * k;
  const r1 = x | 0;
  r3 = (r3 & 0xf) | sign | (exp << 4);
  return caml_int64_create_lo_mi_hi(r1, r2, r3);
}

//Provides: caml_int32_bits_of_float const
//Requires: jsoo_floor_log2
function caml_int32_bits_of_float(x) {
  const float32a = new Float32Array(1);
  float32a[0] = x;
  const int32a = new Int32Array(float32a.buffer);
  return int32a[0] | 0;
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
  const sign =
    x === 0 && 1 / x === Number.NEGATIVE_INFINITY ? 1 : x >= 0 ? 0 : 1;
  if (sign) x = -x;
  let exp = 0;
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
  const exp_sign = exp < 0 ? "" : "+";
  let sign_str = "";
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
    const cst = 2 ** (prec * 4);
    x = Math.round(x * cst) / cst;
  }
  let x_str = x.toString(16);
  if (prec >= 0) {
    const idx = x_str.indexOf(".");
    if (idx < 0) {
      x_str += `.${caml_str_repeat(prec, "0")}`;
    } else {
      const size = idx + 1 + prec;
      if (x_str.length < size)
        x_str += caml_str_repeat(size - x_str.length, "0");
      else x_str = x_str.substr(0, size);
    }
  }
  return caml_string_of_jsstring(
    `${sign_str}0x${x_str}p${exp_sign}${exp.toString(10)}`,
  );
}

//Provides: caml_int64_float_of_bits const
function caml_int64_float_of_bits(x) {
  const lo = x.lo;
  const mi = x.mi;
  const hi = x.hi;
  const exp = (hi & 0x7fff) >> 4;
  if (exp === 2047) {
    if ((lo | mi | (hi & 0xf)) === 0)
      return hi & 0x8000 ? Number.NEGATIVE_INFINITY : Number.POSITIVE_INFINITY;
    return Number.NaN;
  }
  const k = 2 ** -24;
  let res = (lo * k + mi) * k + (hi & 0xf);
  if (exp > 0) {
    res += 16;
    res *= 2 ** (exp - 1027);
  } else res *= 2 ** -1026;
  if (hi & 0x8000) res = -res;
  return res;
}

//Provides: caml_nextafter_float const
//Requires: caml_int64_float_of_bits, caml_int64_bits_of_float, caml_int64_add, caml_int64_sub,caml_int64_of_int32
function caml_nextafter_float(x, y) {
  if (Number.isNaN(x) || Number.isNaN(y)) return Number.NaN;
  if (x === y) return y;
  if (x === 0) {
    if (y < 0) return -(2 ** -1074);
    return 2 ** -1074;
  }
  let bits = caml_int64_bits_of_float(x);
  const one = caml_int64_of_int32(1);
  if (x < y === x > 0) bits = caml_int64_add(bits, one);
  else bits = caml_int64_sub(bits, one);
  return caml_int64_float_of_bits(bits);
}

//Provides: caml_trunc_float
function caml_trunc_float(x) {
  return Math.trunc(x);
}

//Provides: caml_int32_float_of_bits const
function caml_int32_float_of_bits(x) {
  const int32a = new Int32Array(1);
  int32a[0] = x;
  const float32a = new Float32Array(int32a.buffer);
  return float32a[0];
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
    const neg = 1 / x < 0;
    x = Math.abs(x);
    let i = Math.floor(x);
    let f = x - i;
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
    x *= 2 ** 1023;
    if (exp > 1023) {
      // in case x is subnormal
      exp -= 1023;
      x *= 2 ** 1023;
    }
  }
  if (exp < -1023) {
    exp += 1023;
    x *= 2 ** -1023;
  }
  x *= 2 ** exp;
  return x;
}
//Provides: caml_frexp_float const
//Requires: jsoo_floor_log2
function caml_frexp_float(x) {
  if (x === 0 || !Number.isFinite(x)) return [0, x, 0];
  const neg = x < 0;
  if (neg) x = -x;
  let exp = Math.max(-1023, jsoo_floor_log2(x) + 1);
  x *= 2 ** -exp;
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
  if (x === x) return 1;
  if (y === y) return -1;
  return 0;
}

//Provides: caml_copysign_float const
function caml_copysign_float(x, y) {
  if (y === 0) y = 1 / y;
  x = Math.abs(x);
  return y < 0 ? -x : x;
}

//Provides: caml_signbit_float const
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
  return 2 ** x;
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
    const y = Math.floor(x);
    return x - y >= 0.5 ? y + 1 : y;
  }
  const y = Math.ceil(x);
  return y - x >= 0.5 ? y - 1 : y;
}
//Provides: caml_cbrt_float const
function caml_cbrt_float(x) {
  return Math.cbrt(x);
}

//Provides: caml_erf_float const
function caml_erf_float(x) {
  const a1 = 0.254829592;
  const a2 = -0.284496736;
  const a3 = 1.421413741;
  const a4 = -1.453152027;
  const a5 = 1.061405429;
  const p = 0.3275911;

  let sign = 1;
  if (x < 0) {
    sign = -1;
  }
  x = Math.abs(x);
  const t = 1.0 / (1.0 + p * x);
  const y =
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
  const SPLIT = 2 ** 27 + 1;
  const MIN_VALUE = 2 ** -1022;
  const EPSILON = 2 ** -52;
  const C = 416;
  const A = 2 ** +C;
  const B = 2 ** -C;

  function multiply(a, b) {
    const at = SPLIT * a;
    const ahi = at - (at - a);
    const alo = a - ahi;
    const bt = SPLIT * b;
    const bhi = bt - (bt - b);
    const blo = b - bhi;
    const p = a * b;
    const e = ahi * bhi - p + ahi * blo + alo * bhi + alo * blo;
    return {
      p: p,
      e: e,
    };
  }

  function add(a, b) {
    const s = a + b;
    const v = s - a;
    const e = a - (s - v) + (b - v);
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

  if (
    x === 0 ||
    x !== x ||
    x === +1 / 0 ||
    x === -1 / 0 ||
    y === 0 ||
    y !== y ||
    y === +1 / 0 ||
    y === -1 / 0
  ) {
    return x * y + z;
  }
  if (z === 0) {
    return x * y;
  }
  if (z !== z || z === +1 / 0 || z === -1 / 0) {
    return z;
  }

  let scale = 1;
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

  const xs = x;
  const ys = y;
  let zs = z / scale;

  if (Math.abs(zs) > (Math.abs(xs * ys) * 4) / EPSILON) {
    return z;
  }
  if (Math.abs(zs) < (((Math.abs(xs * ys) * EPSILON) / 4) * EPSILON) / 4) {
    zs = (z < 0 ? -1 : +1) * MIN_VALUE;
  }

  const xy = multiply(xs, ys);
  const s = add(xy.p, zs);
  const u = add(xy.e, s.e);
  const i = add(s.s, u.s);

  const f = i.s + adjust(i.e, u.e);
  if (f === 0) {
    return f;
  }

  const fs = f * scale;
  if (Math.abs(fs) > MIN_VALUE) {
    return fs;
  }

  // It is possible that there was extra rounding for a denormalized value.
  return fs + adjust(f - fs / scale, i.e) * scale;
}

//Provides: caml_format_float const
//Requires: caml_parse_format, caml_finish_formatting
function caml_format_float(fmt, x) {
  function toFixed(x, dp) {
    if (Math.abs(x) < 1.0) {
      return x.toFixed(dp);
    }
    let e = Number.parseInt(x.toString().split("+")[1]);
    if (e > 20) {
      e -= 20;
      x /= 10 ** e;
      x += new Array(e + 1).join("0");
      if (dp > 0) {
        x = `${x}.${new Array(dp + 1).join("0")}`;
      }
      return x;
    }
    return x.toFixed(dp);
  }
  let s;
  const f = caml_parse_format(fmt);
  let prec = f.prec < 0 ? 6 : f.prec;
  if (x < 0 || (x === 0 && 1 / x === Number.NEGATIVE_INFINITY)) {
    f.sign = -1;
    x = -x;
  }
  if (Number.isNaN(x)) {
    s = "nan";
    f.filler = " ";
  } else if (Number.isFinite(x))
    switch (f.conv) {
      case "e": {
        s = x.toExponential(prec);
        // exponent should be at least two digits
        const i = s.length;
        if (s.charAt(i - 3) === "e")
          s = `${s.slice(0, i - 1)}0${s.slice(i - 1)}`;
        break;
      }
      case "f":
        s = toFixed(x, prec);
        break;
      case "g": {
        prec = prec ? prec : 1;
        s = x.toExponential(prec - 1);
        const j = s.indexOf("e");
        const exp = +s.slice(j + 1);
        if (exp < -4 || x >= 1e21 || x.toFixed(0).length > prec) {
          // remove trailing zeroes
          let i = j - 1;
          while (s.charAt(i) === "0") i--;
          if (s.charAt(i) === ".") i--;
          s = s.slice(0, i + 1) + s.slice(j);
          i = s.length;
          if (s.charAt(i - 3) === "e")
            s = `${s.slice(0, i - 1)}0${s.slice(i - 1)}`;
          break;
        }
        let p = prec;
        if (exp < 0) {
          p -= exp + 1;
          s = x.toFixed(p);
        } else while ((s = x.toFixed(p)) && s.length > prec + 1) p--;
        if (p) {
          // remove trailing zeroes
          let i = s.length - 1;
          while (s.charAt(i) === "0") i--;
          if (s.charAt(i) === ".") i--;
          s = s.slice(0, i + 1);
        }
        break;
      }
    }
  else {
    s = "inf";
    f.filler = " ";
  }
  return caml_finish_formatting(f, s);
}

//Provides: caml_float_of_string (const)
//Requires: caml_failwith, caml_jsbytes_of_string
function caml_float_of_string(s) {
  let res;
  s = caml_jsbytes_of_string(s);
  res = +s;
  if (s.length > 0 && res === res) return res;
  s = s.replace(/_/g, "");
  res = +s;
  if ((s.length > 0 && res === res) || /^[+-]?nan$/i.test(s)) return res;
  const m = /^ *([+-]?)0x([0-9a-f]+)\.?([0-9a-f]*)(p([+-]?[0-9]+))?/i.exec(s);
  //          1        2             3           5
  if (m) {
    const m3 = m[3].replace(/0+$/, "");
    const mantissa = Number.parseInt(m[1] + m[2] + m3, 16);
    const exponent = (m[5] | 0) - 4 * m3.length;
    res = mantissa * 2 ** exponent;
    return res;
  }
  if (/^\+?inf(inity)?$/i.test(s)) return Number.POSITIVE_INFINITY;
  if (/^-inf(inity)?$/i.test(s)) return Number.NEGATIVE_INFINITY;
  caml_failwith("float_of_string");
}

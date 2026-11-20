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
  var lo = lo32 & 0xffffff;
  var mi = (lo32 >>> 24) | ((hi32 << 8) & 0xffffff);
  var hi = (hi32 >>> 16) & 0xffff;
  // V8 uses signaling NaNs as sentinel. So, NaNs are made quiet when
  // they are stored in an array. Make them quiet here so that we get
  // consistent results.
  if ((hi & 0x7ff8) === 0x7ff0 && (mi | lo | (hi & 0xf)) !== 0) hi |= 8;
  return caml_int64_create_lo_mi_hi(lo, mi, hi);
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
//Requires: caml_string_of_jsstring, caml_str_repeat, jsoo_dataview
function caml_hexstring_of_float(x, prec, style) {
  // As in the C runtime, the sign (or the style character) is also
  // printed for infinities and NaNs
  jsoo_dataview.setFloat64(0, x, true);
  var sign = jsoo_dataview.getUint8(7) >> 7;
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
  if (!Number.isFinite(x))
    return caml_string_of_jsstring(
      sign_str + (Number.isNaN(x) ? "nan" : "infinity"),
    );
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
  if (prec >= 0 && prec < 13) {
    /* If a precision is given, and is small, round mantissa accordingly,
       ties to even (the scaling by a power of two is exact) */
    var cst = Math.pow(2, prec * 4);
    var y = x * cst;
    var r = Math.round(y);
    if (r - y === 0.5 && r % 2 !== 0) r -= 1;
    x = r / cst;
  }
  var x_str = x.toString(16);
  var idx = x_str.indexOf(".");
  if (prec === 0) {
    /* No decimal point */
    if (idx >= 0) x_str = x_str.slice(0, idx);
  } else if (prec > 0) {
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
  // V8 uses signaling NaNs as sentinel. So, NaNs are made quiet when
  // they are stored in an array. Make them quiet here so that we get
  // consistent results.
  if ((hi & 0x7ff8) === 0x7ff0 && (mi | lo | (hi & 0xf)) !== 0) hi |= 8;
  jsoo_dataview.setUint32(0, lo | (mi << 24), true);
  jsoo_dataview.setUint32(4, (mi >>> 8) | (hi << 16), true);
  return jsoo_dataview.getFloat64(0, true);
}

//Provides: caml_nextafter_float const
//Requires: jsoo_dataview
function caml_nextafter_float(x, y) {
  if (Number.isNaN(x) || Number.isNaN(y)) return Number.NaN;
  if (x === y) return y;
  if (x === 0) {
    if (y < 0) return -Math.pow(2, -1074);
    else return Math.pow(2, -1074);
  }
  // Add one to or subtract one from the bit pattern, as two 32-bit halves
  jsoo_dataview.setFloat64(0, x, true);
  var lo = jsoo_dataview.getUint32(0, true);
  var hi = jsoo_dataview.getUint32(4, true);
  if (x < y === x > 0) {
    // away from zero
    lo = (lo + 1) >>> 0;
    if (lo === 0) hi++;
  } else {
    if (lo === 0) hi--;
    lo = (lo - 1) >>> 0;
  }
  jsoo_dataview.setUint32(0, lo, true);
  jsoo_dataview.setUint32(4, hi, true);
  return jsoo_dataview.getFloat64(0, true);
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
  // musl's scalbn: scaling down goes through 2^-1022 * 2^53 so that
  // the intermediate values stay normal; the only rounding happens in
  // the final multiplication
  exp |= 0;
  if (exp > 1023) {
    exp -= 1023;
    x *= Math.pow(2, 1023);
    if (exp > 1023) {
      // in case x is subnormal
      exp -= 1023;
      x *= Math.pow(2, 1023);
      if (exp > 1023) exp = 1023;
    }
  } else if (exp < -1022) {
    exp += 1022 - 53;
    x *= Math.pow(2, -1022) * Math.pow(2, 53);
    if (exp < -1022) {
      exp += 1022 - 53;
      x *= Math.pow(2, -1022) * Math.pow(2, 53);
      if (exp < -1022) exp = -1022;
    }
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

// erf and erfc are ported from FreeBSD's msun (s_erf.c), through musl:
//
// Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
//
// Developed at SunPro, a Sun Microsystems, Inc. business.
// Permission to use, copy, modify, and distribute this
// software is freely granted, provided that this notice
// is preserved.
//
// The Wasm runtime (float.wat) performs the same operations in the same
// order.

//Provides: caml_erf_ratio const
// (erf(x) - x) / x, for |x| < 0.84375, as a function of z = x * x
function caml_erf_ratio(z) {
  var pp0 = 0.12837916709551256,
    pp1 = -0.3250421072470015,
    pp2 = -0.02848174957559851,
    pp3 = -0.005770270296489442,
    pp4 = -0.000023763016656650163,
    qq1 = 0.39791722395915535,
    qq2 = 0.0650222499887673,
    qq3 = 0.005081306281875766,
    qq4 = 0.00013249473800432164,
    qq5 = -0.000003960228278775368;
  var r = pp0 + z * (pp1 + z * (pp2 + z * (pp3 + z * pp4)));
  var s = 1 + z * (qq1 + z * (qq2 + z * (qq3 + z * (qq4 + z * qq5))));
  return r / s;
}

//Provides: caml_erfc_abs const
//Requires: jsoo_dataview
// erfc(x), for 0.84375 <= x < 28
function caml_erfc_abs(x) {
  var s, r, q;
  if (x < 1.25) {
    // erf(1 + s) - erx
    var erx = 0.8450629115104675,
      pa0 = -0.0023621185607526594,
      pa1 = 0.41485611868374833,
      pa2 = -0.3722078760357013,
      pa3 = 0.31834661990116175,
      pa4 = -0.11089469428239668,
      pa5 = 0.035478304325618236,
      pa6 = -0.002166375594868791,
      qa1 = 0.10642088040084423,
      qa2 = 0.540397917702171,
      qa3 = 0.07182865441419627,
      qa4 = 0.12617121980876164,
      qa5 = 0.01363708391202905,
      qa6 = 0.011984499846799107;
    s = x - 1;
    r =
      pa0 + s * (pa1 + s * (pa2 + s * (pa3 + s * (pa4 + s * (pa5 + s * pa6)))));
    q = 1 + s * (qa1 + s * (qa2 + s * (qa3 + s * (qa4 + s * (qa5 + s * qa6)))));
    return 1 - erx - r / q;
  }
  // erfc(x) = exp(-x * x - 0.5625 + r / q) / x
  s = 1 / (x * x);
  if (x < 2.8571414947509766) {
    // x < 1 / 0.35
    var ra0 = -0.009864944034847148,
      ra1 = -0.6938585727071818,
      ra2 = -10.558626225323291,
      ra3 = -62.375332450326006,
      ra4 = -162.39666946257347,
      ra5 = -184.60509290671104,
      ra6 = -81.2874355063066,
      ra7 = -9.814329344169145,
      sa1 = 19.651271667439257,
      sa2 = 137.65775414351904,
      sa3 = 434.56587747522923,
      sa4 = 645.3872717332679,
      sa5 = 429.00814002756783,
      sa6 = 108.63500554177944,
      sa7 = 6.570249770319282,
      sa8 = -0.0604244152148581;
    r =
      ra0 +
      s *
        (ra1 +
          s * (ra2 + s * (ra3 + s * (ra4 + s * (ra5 + s * (ra6 + s * ra7))))));
    q =
      1 +
      s *
        (sa1 +
          s *
            (sa2 +
              s *
                (sa3 +
                  s * (sa4 + s * (sa5 + s * (sa6 + s * (sa7 + s * sa8)))))));
  } else {
    var rb0 = -0.0098649429247001,
      rb1 = -0.799283237680523,
      rb2 = -17.757954917754752,
      rb3 = -160.63638485582192,
      rb4 = -637.5664433683896,
      rb5 = -1025.0951316110772,
      rb6 = -483.5191916086514,
      sb1 = 30.33806074348246,
      sb2 = 325.7925129965739,
      sb3 = 1536.729586084437,
      sb4 = 3199.8582195085955,
      sb5 = 2553.0504064331644,
      sb6 = 474.52854120695537,
      sb7 = -22.44095244658582;
    r =
      rb0 + s * (rb1 + s * (rb2 + s * (rb3 + s * (rb4 + s * (rb5 + s * rb6)))));
    q =
      1 +
      s *
        (sb1 +
          s * (sb2 + s * (sb3 + s * (sb4 + s * (sb5 + s * (sb6 + s * sb7))))));
  }
  // z is x with the low 32 bits of its significand cleared, so that
  // z * z is exact and -x * x = -z * z + (z - x) * (z + x)
  jsoo_dataview.setFloat64(0, x, true);
  jsoo_dataview.setUint32(0, 0, true);
  var z = jsoo_dataview.getFloat64(0, true);
  return (Math.exp(-z * z - 0.5625) * Math.exp((z - x) * (z + x) + r / q)) / x;
}

//Provides: caml_erf_float const
//Requires: caml_erf_ratio, caml_erfc_abs
function caml_erf_float(x) {
  var a = Math.abs(x);
  if (a < 0.84375) {
    if (a < 3.725290298461914e-9)
      // |x| < 2^-28 (the scaling avoids underflows)
      return 0.125 * (8 * x + 1.0270333367641007 * x);
    return x + x * caml_erf_ratio(x * x);
  }
  var y;
  if (a < 6) y = 1 - caml_erfc_abs(a);
  else if (a >= 6) y = 1;
  else return x; // NaN
  return x < 0 ? -y : y;
}

//Provides: caml_erfc_float const
//Requires: caml_erf_ratio, caml_erfc_abs
function caml_erfc_float(x) {
  var a = Math.abs(x);
  if (a < 0.84375) {
    if (a < 1.3877787807814457e-17) return 1 - x; // |x| < 2^-56
    var y = caml_erf_ratio(x * x);
    if (x < 0.25) return 1 - (x + x * y);
    return 0.5 - (x - 0.5 + x * y);
  }
  if (a < 28) return x < 0 ? 2 - caml_erfc_abs(a) : caml_erfc_abs(a);
  if (a >= 28) return x < 0 ? 2 : 0;
  return x; // NaN
}

//Provides: caml_fma_float const
//Requires: jsoo_dataview
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
  // z / scale, without arithmetic on a subnormal z, which is slow
  var zs;
  if (scale === 1) {
    zs = z;
  } else if (Math.abs(z) >= MIN_VALUE) {
    zs = z / scale;
  } else if (scale > 1) {
    // negligible: only its sign matters
    zs = (z < 0 ? -1 : +1) * MIN_VALUE;
  } else {
    // z = m * 2^-1074, and 2^-1074 / scale = (2^-1022 / scale) * 2^-52 is
    // normal
    jsoo_dataview.setFloat64(0, z, true);
    var m =
      (jsoo_dataview.getUint32(4, true) & 0xfffff) * 4294967296 +
      jsoo_dataview.getUint32(0, true);
    zs = (z < 0 ? -m : m) * ((MIN_VALUE / scale) * EPSILON);
  }

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

  if (scale >= 1) {
    return f * scale;
  }
  // 2^-1022 in the scaled domain
  var G = MIN_VALUE / scale;
  if (Math.abs(f) >= G) {
    return f * scale;
  }

  // Subnormal result, computed without arithmetic on subnormals, which is
  // slow. Round f to the grid of the result (G * 2^-52) by adding K = ±G.
  // Rounding f rather than the exact sum may create a tie or resolve one
  // the wrong way: correct it in the direction of the rest of the exact
  // sum, i.s + i.e + u.e - f (i.e alone can have the wrong sign when u.e
  // moved f past a tie). The result is t * scale, with t a multiple of the
  // grid: build it from the integer t / (G * 2^-52).
  var K = f < 0 ? -G : G;
  var rc = f + K;
  var t = rc + adjust(f - (rc - K), i.s - f + i.e || u.e) - K;
  var r = Math.abs(t / (G * EPSILON));
  var hi = Math.floor(r / 4294967296);
  jsoo_dataview.setUint32(4, (f < 0 ? 0x80000000 : 0) | hi, true);
  jsoo_dataview.setUint32(0, r - hi * 4294967296, true);
  return jsoo_dataview.getFloat64(0, true);
}

//Provides: caml_format_float const
//Requires: caml_str_repeat, caml_parse_format, caml_finish_formatting
function caml_format_float(fmt, x) {
  // Exact decimal expansion through BigInt, for precisions beyond
  // toFixed/toExponential's limit of 100 and for %f of values >= 1e21
  function decompose(x) {
    // x (finite, >= 0) is m * 2^e
    var dv = new DataView(new ArrayBuffer(8));
    dv.setFloat64(0, x);
    var hi = dv.getUint32(0),
      lo = dv.getUint32(4);
    var eb = (hi >>> 20) & 0x7ff;
    var m = (BigInt(hi & 0xfffff) << 32n) | BigInt(lo);
    if (eb === 0) return [m, -1074];
    return [m | (1n << 52n), eb - 1075];
  }
  function exact_scaled(x, k) {
    // round_half_even(x * 10^k) as a BigInt
    var d = decompose(x);
    var num = d[0],
      den = 1n;
    if (k >= 0) num *= 10n ** BigInt(k);
    else den = 10n ** BigInt(-k);
    if (d[1] >= 0) num <<= BigInt(d[1]);
    else den <<= BigInt(-d[1]);
    var q = num / den,
      r2 = (num % den) * 2n;
    if (r2 > den || (r2 === den && q & 1n)) q += 1n;
    return q;
  }
  function exact_fixed(x, prec) {
    // toFixed, exactly
    var q = exact_scaled(x, prec).toString();
    if (prec === 0) return q;
    if (q.length <= prec) q = caml_str_repeat(prec + 1 - q.length, "0") + q;
    return q.slice(0, q.length - prec) + "." + q.slice(q.length - prec);
  }
  function exact_exponential(x, prec) {
    // toExponential, exactly
    if (x === 0)
      return (prec > 0 ? "0." + caml_str_repeat(prec, "0") : "0") + "e+0";
    var e10 = Math.floor(Math.log10(x));
    for (;;) {
      // we want round(x * 10^(prec - e10)) to have exactly prec + 1
      // digits; the estimate of e10 is off by at most one (and
      // rounding can add a digit), so adjust and retry
      var s = exact_scaled(x, prec - e10).toString();
      if (s.length === prec + 1) {
        var m = prec > 0 ? s.charAt(0) + "." + s.slice(1) : s;
        return m + "e" + (e10 < 0 ? "-" : "+") + Math.abs(e10);
      }
      e10 += s.length - (prec + 1);
    }
  }
  // Rounding x at 10^-k is a tie iff 2 * x * 10^k is an odd integer,
  // that is, iff y = x * 2^(k+1) is an odd integer (5^k is odd) and,
  // when k < 0, a multiple of 5^-k. Scaling by a power of two is
  // exact, so this test is exact.
  function is_tie(x, k) {
    var y = x * 2 ** (k + 1);
    return y % 2 === 1 && (k >= 0 || y % 5 ** -k === 0);
  }
  // toFixed/toExponential round ties away from zero; C rounds them to
  // even. s is x rounded at 10^-k, the digit at 10^-k being s[i]: on a
  // tie, that digit is one too many iff it is odd.
  function round_half_even(s, i, x, k) {
    var c = s.charCodeAt(i);
    return c % 2 === 1 && is_tie(x, k)
      ? s.slice(0, i) + String.fromCharCode(c - 1) + s.slice(i + 1)
      : s;
  }
  function toExponential(x, prec) {
    if (prec > 100) return exact_exponential(x, prec);
    var s = x.toExponential(prec);
    var i = s.indexOf("e");
    return round_half_even(s, i - 1, x, prec - +s.slice(i + 1));
  }
  function toFixed(x, dp) {
    if (dp > 100 || x >= 1e21) return exact_fixed(x, dp);
    var s = x.toFixed(dp);
    return round_half_even(s, s.length - 1, x, dp);
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
        var s = toExponential(x, prec);
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
        s = toExponential(x, prec - 1);
        var j = s.indexOf("e");
        var exp = +s.slice(j + 1);
        if (exp < -4 || exp >= prec) {
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
            s = toFixed(x, p);
          } else while (((s = toFixed(x, p)), s.length > prec + 1)) p--;
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

//Provides: caml_parse_float
//Requires: caml_failwith, caml_jsbytes_of_string
function caml_parse_float(s, err_msg) {
  var res;
  var r_float = /^[ \t\n\v\f\r]*[-+]?(?:\d*\.?\d+|\d+\.?\d*)(?:[eE][-+]?\d+)?$/;
  s = caml_jsbytes_of_string(s);
  res = +s;
  //Fast path
  if (!Number.isNaN(res) && r_float.test(s)) return res;
  s = s.replace(/_/g, "");
  res = +s;
  if (
    (!Number.isNaN(res) && r_float.test(s)) ||
    /^[ \t\n\v\f\r]*[+-]?nan$/i.test(s)
  )
    return res;
  var m =
    /^[ \t\n\v\f\r]*([+-]?)0x([0-9a-f]*)\.?([0-9a-f]*)(?:p([+-]?[0-9]+))?$/i.exec(
      s,
    );
  //          1        2             3               4
  if (m && m[2].length + m[3].length > 0) {
    var sign = m[1] === "-" ? -1 : 1;
    var mant = BigInt("0x0" + m[2] + m[3]);
    var e = (+m[4] || 0) - 4 * m[3].length;
    if (mant === 0n) return sign * 0;
    // Round [mant * 2^e] to the nearest representable double
    // (round-to-nearest, ties-to-even), exactly
    for (;;) {
      var bl = mant.toString(2).length; // bit length
      var be = bl - 1 + e; // exponent of the leading bit
      if (be > 1023) return sign * Number.POSITIVE_INFINITY;
      if (be < -1075) return sign * 0;
      // number of mantissa bits a double can hold at this magnitude
      var p = be >= -1022 ? 53 : 53 + 1022 + be;
      var shift = bl - p;
      if (shift <= 0)
        // [mant * 2^e] is representable: both operations are exact
        return sign * Number(mant) * Math.pow(2, e);
      var keep = mant >> BigInt(shift);
      var rem = mant & ((1n << BigInt(shift)) - 1n);
      var half = 1n << BigInt(shift - 1);
      if (rem > half || (rem === half && keep & 1n)) keep += 1n;
      if (keep < (p > 0 ? 1n << BigInt(p) : 1n))
        return sign * Number(keep) * Math.pow(2, e + shift);
      // the carry needs one more bit: renormalize and round again
      mant = keep;
      e += shift;
    }
  }
  if (/^[ \t\n\v\f\r]*\+?inf(inity)?$/i.test(s))
    return Number.POSITIVE_INFINITY;
  if (/^[ \t\n\v\f\r]*-inf(inity)?$/i.test(s)) return Number.NEGATIVE_INFINITY;
  caml_failwith(err_msg);
}

//Provides: caml_float_of_string (const)
//Requires: caml_parse_float
function caml_float_of_string(s) {
  return caml_parse_float(s, "float_of_string");
}

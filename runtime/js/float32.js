/*
    32-bit floats are represented as javascript numbers, i.e. 64-bit floats.
    Each operation is performed in 64-bit precision and then rounded to the
    nearest 32-bit float. This is not identical to using true 32-bit operations.
    For example, if rounding an exact result to 64 bits places it halfway
    between the two nearest 32-bit numbers, rounding it again to 32 bits
    may not result in the closest 32-bit number to the exact result.

    Marshalled float32s therefore look like normal floats. This means that
    javascript programs are not be able to read float32 data marshalled
    by native programs and vice versa.
*/

//Provides: caml_float_of_float32 const
//Version: >= 5.2
//If: oxcaml
function caml_float_of_float32(x) {
  return x;
}

//Provides: caml_float32_of_float const
//Version: >= 5.2
//If: oxcaml
function caml_float32_of_float(x) {
  return Math.fround(x);
}

//Provides: caml_float32_of_int const
//Version: >= 5.2
//If: oxcaml
function caml_float32_of_int(x) {
  return Math.fround(x);
}

//Provides: caml_int_of_float32 const
//Version: >= 5.2
//If: oxcaml
function caml_int_of_float32(x) {
  return x | 0;
}

//Provides: caml_float32_of_bits_bytecode const
//Requires: caml_int32_float_of_bits
//Version: >= 5.2
//If: oxcaml
const caml_float32_of_bits_bytecode = caml_int32_float_of_bits;

//Provides: caml_float32_to_bits_bytecode const
//Requires: caml_int32_bits_of_float
//Version: >= 5.2
//If: oxcaml
const caml_float32_to_bits_bytecode = caml_int32_bits_of_float;

//Provides: caml_float32_round_odd const
//Version: >= 5.2
//If: oxcaml
// Rounding a value to a double, then to a float32, can give a different
// result than rounding it directly to a float32: the first rounding can land
// exactly halfway between two float32 values. This does not happen if the
// first rounding is to odd (the double with an odd significand among the two
// surrounding the value, when it is not representable). Given the double
// [s] nearest to a value and the sign of their difference [err] (value - s),
// this returns the value rounded to odd.
var caml_float32_round_odd = (function () {
  var dv = new DataView(new ArrayBuffer(8));
  return function (s, err) {
    if (err === 0 || s === 0 || !Number.isFinite(s)) return s;
    dv.setFloat64(0, s);
    var lo = dv.getUint32(4);
    if (lo & 1) return s;
    // move one ulp toward the value: up in magnitude iff err and s have the
    // same sign (lo is even, so there is no carry when going up)
    if (err > 0 === s > 0) dv.setUint32(4, lo + 1);
    else if (lo !== 0) dv.setUint32(4, lo - 1);
    else {
      dv.setUint32(4, 0xffffffff);
      dv.setUint32(0, dv.getUint32(0) - 1);
    }
    return dv.getFloat64(0);
  };
})();

//Provides: caml_float32_of_int64_bytecode const
//Requires: caml_int64_to_float, caml_int64_hi32, caml_int64_lo32
//Requires: caml_float32_round_odd
//Version: >= 5.2
//If: oxcaml
function caml_float32_of_int64_bytecode(x) {
  var d = caml_int64_to_float(x);
  // d is exact below 2^53; at 2^53, rounding d to a float32 gives the same
  // result as rounding x (at most 2^53 + 1)
  if (Math.abs(d) <= 9007199254740992) return Math.fround(d);
  var e =
    ((BigInt(caml_int64_hi32(x)) << 32n) | BigInt(caml_int64_lo32(x) >>> 0)) -
    BigInt(d);
  return Math.fround(caml_float32_round_odd(d, e > 0n ? 1 : e < 0n ? -1 : 0));
}

//Provides: caml_float32_to_int64_bytecode const
//Requires: caml_int64_of_float
//Version: >= 5.2
//If: oxcaml
const caml_float32_to_int64_bytecode = caml_int64_of_float;

//Provides: caml_float32_exact_compare
//Version: >= 5.2
//If: oxcaml
// The sign of |v| - a, where v is the exact value of the float literal s (a
// finite literal accepted by caml_parse_float) and a is a positive double
function caml_float32_exact_compare(s, a) {
  s = s.replace(/_/g, "").trim();
  var c = s.charAt(0);
  if (c === "-" || c === "+") s = s.slice(1);
  // v = num * base^exp
  var num, base, exp;
  var m = /^0[xX]([0-9a-fA-F]*)\.?([0-9a-fA-F]*)(?:[pP]([-+]?[0-9]+))?$/.exec(
    s,
  );
  if (m) {
    num = BigInt("0x0" + m[1] + m[2]);
    base = 2n;
    exp = (+m[3] || 0) - 4 * m[2].length;
  } else {
    m = /^([0-9]*)\.?([0-9]*)(?:[eE]([-+]?[0-9]+))?$/.exec(s);
    num = BigInt("0" + m[1] + m[2]);
    base = 10n;
    exp = (+m[3] || 0) - m[2].length;
  }
  // a = mant * 2^e
  var dv = new DataView(new ArrayBuffer(8));
  dv.setFloat64(0, a);
  var hi = dv.getUint32(0),
    lo = dv.getUint32(4);
  var eb = (hi >>> 20) & 0x7ff;
  var mant = (BigInt(hi & 0xfffff) << 32n) | BigInt(lo),
    e = -1074;
  if (eb !== 0) {
    mant |= 1n << 52n;
    e = eb - 1075;
  }
  var l = num,
    r = mant;
  if (exp >= 0) l *= base ** BigInt(exp);
  else r *= base ** BigInt(-exp);
  if (e >= 0) r <<= BigInt(e);
  else l <<= BigInt(-e);
  return l > r ? 1 : l < r ? -1 : 0;
}

//Provides: caml_float32_of_string (const)
//Requires: caml_parse_float, caml_float32_exact_compare, caml_jsbytes_of_string
//Version: >= 5.2
//If: oxcaml
var caml_float32_of_string = (function () {
  var f32 = new Float32Array(1),
    u32 = new Uint32Array(f32.buffer);
  function neighbor(f, d) {
    f32[0] = f;
    u32[0] += d;
    return f32[0];
  }
  return function (x) {
    var d = caml_parse_float(x, "float32_of_string");
    var f = Math.fround(d);
    if (f === d || Number.isNaN(d)) return f;
    // Rounding the literal to the double d may have moved it to the
    // midpoint between two float32 values; rounding d to a float32 then
    // picks the even one, which is wrong if the literal is on the other
    // side of the midpoint. In that case, compare the literal with d.
    var a = Math.abs(d),
      lo,
      hi;
    if (Math.abs(f) > a) {
      hi = Math.abs(f);
      lo = neighbor(hi, -1);
    } else {
      lo = Math.abs(f);
      hi = neighbor(lo, 1);
    }
    var mid =
      hi === Number.POSITIVE_INFINITY ? 2 ** 128 - 2 ** 103 : (lo + hi) / 2;
    if (a !== mid) return f;
    var c = caml_float32_exact_compare(caml_jsbytes_of_string(x), a);
    if (c === 0) return f;
    return (c > 0 ? hi : lo) * Math.sign(d);
  };
})();

//Provides: caml_format_float32 const
//Requires: caml_format_float
//Version: >= 5.2
//If: oxcaml
const caml_format_float32 = caml_format_float;

//Provides: caml_float32_compare const
//Requires: caml_float_compare
//Version: >= 5.2
//If: oxcaml
const caml_float32_compare = caml_float_compare;

//Provides: caml_add_float32 const
//Version: >= 5.2
//If: oxcaml
function caml_add_float32(x, y) {
  return Math.fround(x + y);
}

//Provides: caml_sub_float32 const
//Version: >= 5.2
//If: oxcaml
function caml_sub_float32(x, y) {
  return Math.fround(x - y);
}

//Provides: caml_mul_float32 const
//Version: >= 5.2
//If: oxcaml
function caml_mul_float32(x, y) {
  return Math.fround(x * y);
}

//Provides: caml_div_float32 const
//Version: >= 5.2
//If: oxcaml
function caml_div_float32(x, y) {
  return Math.fround(x / y);
}

//Provides: caml_fmod_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_fmod_float32_bytecode(x, y) {
  return Math.fround(x % y);
}

//Provides: caml_neg_float32 const
//Version: >= 5.2
//If: oxcaml
function caml_neg_float32(x) {
  return -x; // Result is exact
}

//Provides: caml_abs_float32 const
//Version: >= 5.2
//If: oxcaml
function caml_abs_float32(x) {
  return Math.abs(x); // Result is exact
}

//Provides: caml_modf_float32 const
//Requires: caml_modf_float
//Version: >= 5.2
//If: oxcaml
const caml_modf_float32 = caml_modf_float; // Result is exact

//Provides: caml_acos_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_acos_float32_bytecode(x) {
  return Math.fround(Math.acos(x));
}

//Provides: caml_asin_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_asin_float32_bytecode(x) {
  return Math.fround(Math.asin(x));
}

//Provides: caml_atan_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_atan_float32_bytecode(x) {
  return Math.fround(Math.atan(x));
}

//Provides: caml_atan2_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_atan2_float32_bytecode(x, y) {
  return Math.fround(Math.atan2(x, y));
}

//Provides: caml_ceil_float32_bytecode const
//Alias: caml_simd_float32_round_pos_inf_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_ceil_float32_bytecode(x) {
  return Math.fround(Math.ceil(x));
}

//Provides: caml_cos_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_cos_float32_bytecode(x) {
  return Math.fround(Math.cos(x));
}

//Provides: caml_exp_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_exp_float32_bytecode(x) {
  return Math.fround(Math.exp(x));
}

//Provides: caml_floor_float32_bytecode const
//Alias: caml_simd_float32_round_neg_inf_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_floor_float32_bytecode(x) {
  return Math.fround(Math.floor(x));
}

//Provides: caml_log_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_log_float32_bytecode(x) {
  return Math.fround(Math.log(x));
}

//Provides: caml_power_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_power_float32_bytecode(x, y) {
  return Math.fround(Math.pow(x, y));
}

//Provides: caml_sin_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_sin_float32_bytecode(x) {
  return Math.fround(Math.sin(x));
}

//Provides: caml_sqrt_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_sqrt_float32_bytecode(x) {
  return Math.fround(Math.sqrt(x));
}

//Provides: caml_tan_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_tan_float32_bytecode(x) {
  return Math.fround(Math.tan(x));
}

//Provides: caml_nextafter_float32_bytecode const
//Requires: caml_int32_bits_of_float, caml_int32_float_of_bits
//Version: >= 5.2
//If: oxcaml
function caml_nextafter_float32_bytecode(x, y) {
  if (Number.isNaN(x) || Number.isNaN(y)) return Number.NaN;
  if (x === y) return y;
  if (x === 0) {
    if (y < 0) return -Math.pow(2, -149);
    else return Math.pow(2, -149);
  }
  var bits = caml_int32_bits_of_float(x);
  if (x < y === x > 0) bits++;
  else bits--;
  return caml_int32_float_of_bits(bits);
}

//Provides: caml_trunc_float32_bytecode const
//Alias: caml_simd_float32_round_towards_zero_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_trunc_float32_bytecode(x) {
  return Math.fround(Math.trunc(x));
}

//Provides: caml_classify_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_classify_float32_bytecode(x) {
  if (Number.isFinite(x)) {
    if (Math.abs(x) >= 1.1754943508222875e-38) return 0;
    if (x !== 0) return 1;
    return 2;
  }
  return Number.isNaN(x) ? 4 : 3;
}

//Provides: caml_ldexp_float32_bytecode const
//Requires: caml_ldexp_float
//Version: >= 5.2
//If: oxcaml
function caml_ldexp_float32_bytecode(x, y) {
  return Math.fround(caml_ldexp_float(x, y));
}

//Provides: caml_frexp_float32 const
//Requires: caml_frexp_float
//Version: >= 5.2
//If: oxcaml
const caml_frexp_float32 = caml_frexp_float; // Result is exact

//Provides: caml_copysign_float32_bytecode const
//Requires: caml_copysign_float
//Version: >= 5.2
//If: oxcaml
const caml_copysign_float32_bytecode = caml_copysign_float; // Result is exact

//Provides: caml_signbit_float32_bytecode const
//Requires: caml_signbit_float
//Version: >= 5.2
//If: oxcaml
const caml_signbit_float32_bytecode = caml_signbit_float;

//Provides: caml_expm1_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_expm1_float32_bytecode(x) {
  return Math.fround(Math.expm1(x));
}

//Provides: caml_exp2_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_exp2_float32_bytecode(x) {
  return Math.fround(Math.pow(2, x));
}

//Provides: caml_log1p_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_log1p_float32_bytecode(x) {
  return Math.fround(Math.log1p(x));
}

//Provides: caml_log2_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_log2_float32_bytecode(x) {
  return Math.fround(Math.log2(x));
}

//Provides: caml_hypot_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_hypot_float32_bytecode(x, y) {
  return Math.fround(Math.hypot(x, y));
}

//Provides: caml_log10_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_log10_float32_bytecode(x) {
  return Math.fround(Math.log10(x));
}

//Provides: caml_cosh_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_cosh_float32_bytecode(x) {
  return Math.fround(Math.cosh(x));
}

//Provides: caml_acosh_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_acosh_float32_bytecode(x) {
  return Math.fround(Math.acosh(x));
}

//Provides: caml_sinh_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_sinh_float32_bytecode(x) {
  return Math.fround(Math.sinh(x));
}

//Provides: caml_asinh_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_asinh_float32_bytecode(x) {
  return Math.fround(Math.asinh(x));
}

//Provides: caml_tanh_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_tanh_float32_bytecode(x) {
  return Math.fround(Math.tanh(x));
}

//Provides: caml_atanh_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_atanh_float32_bytecode(x) {
  return Math.fround(Math.atanh(x));
}

//Provides: caml_round_float32_bytecode const
//Requires: caml_round_float
//Alias: caml_simd_float32_round_current_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_round_float32_bytecode(x) {
  return Math.fround(caml_round_float(x));
}

//Provides: caml_cbrt_float32_bytecode const
//Version: >= 5.2
//If: oxcaml
function caml_cbrt_float32_bytecode(x) {
  return Math.fround(Math.cbrt(x));
}

//Provides: caml_erf_float32_bytecode const
//Requires: caml_erf_float
//Version: >= 5.2
//If: oxcaml
function caml_erf_float32_bytecode(x) {
  return Math.fround(caml_erf_float(x));
}

//Provides: caml_erfc_float32_bytecode const
//Requires: caml_erfc_float
//Version: >= 5.2
//If: oxcaml
function caml_erfc_float32_bytecode(x) {
  return Math.fround(caml_erfc_float(x));
}

//Provides: caml_fma_float32_bytecode const
//Requires: caml_float32_round_odd
//Version: >= 5.2
//If: oxcaml
function caml_fma_float32_bytecode(x, y, z) {
  // The product of two float32 values is exact as a double; [s + e] is the
  // exact value of [x * y + z] (TwoSum)
  var p = x * y;
  var s = p + z;
  var b = s - p;
  var e = p - (s - b) + (z - b);
  return Math.fround(caml_float32_round_odd(s, e));
}

//Provides: caml_simd_float32_min_bytecode const
//Version: >= 5.2
//If: oxcaml
// Mirrors x86 MINSS: returns y when the comparison fails (NaN or equal,
// including ±0), unlike Math.min which propagates NaN and treats -0 < +0.
function caml_simd_float32_min_bytecode(x, y) {
  return x < y ? x : y;
}

//Provides: caml_simd_float32_max_bytecode const
//Version: >= 5.2
//If: oxcaml
// Mirrors x86 MAXSS; see caml_simd_float32_min_bytecode.
function caml_simd_float32_max_bytecode(x, y) {
  return x > y ? x : y;
}

//Provides: caml_simd_cast_float32_int64_bytecode const
//Requires: caml_int64_of_float
//Version: >= 5.2
//If: oxcaml
function caml_simd_cast_float32_int64_bytecode(x) {
  return caml_int64_of_float(Math.round(x));
}

//Provides: caml_ba_uint8_getf32
//Requires: caml_ba_uint8_get32, caml_float32_of_bits_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_ba_uint8_getf32(ba, i) {
  return caml_float32_of_bits_bytecode(caml_ba_uint8_get32(ba, i));
}

//Provides: caml_ba_uint8_getf32u
//Requires: caml_ba_uint8_get32u, caml_float32_of_bits_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_ba_uint8_getf32u(ba, i) {
  return caml_float32_of_bits_bytecode(caml_ba_uint8_get32u(ba, i));
}

//Provides: caml_ba_uint8_setf32
//Requires: caml_ba_uint8_set32, caml_float32_to_bits_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_ba_uint8_setf32(ba, i, v) {
  return caml_ba_uint8_set32(ba, i, caml_float32_to_bits_bytecode(v));
}

//Provides: caml_ba_uint8_setf32u
//Requires: caml_ba_uint8_set32u, caml_float32_to_bits_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_ba_uint8_setf32u(ba, i, v) {
  return caml_ba_uint8_set32u(ba, i, caml_float32_to_bits_bytecode(v));
}

//Provides: caml_string_getf32
//Requires: caml_string_get32, caml_float32_of_bits_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_string_getf32(ba, i) {
  return caml_float32_of_bits_bytecode(caml_string_get32(ba, i));
}

//Provides: caml_string_getf32u
//Requires: caml_string_get32u, caml_float32_of_bits_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_string_getf32u(ba, i) {
  return caml_float32_of_bits_bytecode(caml_string_get32u(ba, i));
}

//Provides: caml_bytes_getf32
//Requires: caml_bytes_get32, caml_float32_of_bits_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_bytes_getf32(ba, i) {
  return caml_float32_of_bits_bytecode(caml_bytes_get32(ba, i));
}

//Provides: caml_bytes_getf32u
//Requires: caml_bytes_get32u, caml_float32_of_bits_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_bytes_getf32u(ba, i) {
  return caml_float32_of_bits_bytecode(caml_bytes_get32u(ba, i));
}

//Provides: caml_bytes_setf32
//Requires: caml_bytes_set32, caml_float32_to_bits_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_bytes_setf32(ba, i, v) {
  return caml_bytes_set32(ba, i, caml_float32_to_bits_bytecode(v));
}

//Provides: caml_bytes_setf32u
//Requires: caml_bytes_set32u, caml_float32_to_bits_bytecode
//Version: >= 5.2
//If: oxcaml
function caml_bytes_setf32u(ba, i, v) {
  return caml_bytes_set32u(ba, i, caml_float32_to_bits_bytecode(v));
}

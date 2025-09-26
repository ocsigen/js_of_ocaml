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
//Version: >= 5.2, < 5.3
//OxCaml
function caml_float_of_float32(x) {
  return x;
}

//Provides: caml_float32_of_float const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_float32_of_float(x) {
  return Math.fround(x);
}

//Provides: caml_float32_of_int const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_float32_of_int(x) {
  return Math.fround(x);
}

//Provides: caml_int_of_float32 const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_int_of_float32(x) {
  return x | 0;
}

//Provides: caml_float32_of_bits_bytecode const
//Requires: caml_int32_float_of_bits
//Version: >= 5.2, < 5.3
//OxCaml
const caml_float32_of_bits_bytecode = caml_int32_float_of_bits;

//Provides: caml_float32_to_bits_bytecode const
//Requires: caml_int32_bits_of_float
//Version: >= 5.2, < 5.3
//OxCaml
const caml_float32_to_bits_bytecode = caml_int32_bits_of_float;

//Provides: caml_float32_of_int64_bytecode const
//Requires: caml_int64_to_float
//Version: >= 5.2, < 5.3
//OxCaml
function caml_float32_of_int64_bytecode(x) {
  return Math.fround(caml_int64_to_float(x));
}

//Provides: caml_float32_to_int64_bytecode const
//Requires: caml_int64_of_float
//Version: >= 5.2, < 5.3
//OxCaml
const caml_float32_to_int64_bytecode = caml_int64_of_float;

//Provides: caml_float32_of_string (const)
//Requires: caml_parse_float
//Version: >= 5.2, < 5.3
//OxCaml
function caml_float32_of_string(x) {
  return Math.fround(caml_parse_float(x, "float32_of_string"));
}

//Provides: caml_format_float32 const
//Requires: caml_format_float
//Version: >= 5.2, < 5.3
//OxCaml
const caml_format_float32 = caml_format_float;

//Provides: caml_float32_compare const
//Requires: caml_float_compare
//Version: >= 5.2, < 5.3
//OxCaml
const caml_float32_compare = caml_float_compare;

//Provides: caml_add_float32 const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_add_float32(x, y) {
  return Math.fround(x + y);
}

//Provides: caml_sub_float32 const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_sub_float32(x, y) {
  return Math.fround(x - y);
}

//Provides: caml_mul_float32 const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_mul_float32(x, y) {
  return Math.fround(x * y);
}

//Provides: caml_div_float32 const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_div_float32(x, y) {
  return Math.fround(x / y);
}

//Provides: caml_fmod_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_fmod_float32_bytecode(x, y) {
  return Math.fround(x % y);
}

//Provides: caml_neg_float32 const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_neg_float32(x) {
  return -x; // Result is exact
}

//Provides: caml_abs_float32 const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_abs_float32(x) {
  return Math.abs(x); // Result is exact
}

//Provides: caml_modf_float32 const
//Requires: caml_modf_float
//Version: >= 5.2, < 5.3
//OxCaml
const caml_modf_float32 = caml_modf_float; // Result is exact

//Provides: caml_acos_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_acos_float32_bytecode(x) {
  return Math.fround(Math.acos(x));
}

//Provides: caml_asin_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_asin_float32_bytecode(x) {
  return Math.fround(Math.asin(x));
}

//Provides: caml_atan_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atan_float32_bytecode(x) {
  return Math.fround(Math.atan(x));
}

//Provides: caml_atan2_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atan2_float32_bytecode(x, y) {
  return Math.fround(Math.atan2(x, y));
}

//Provides: caml_ceil_float32_bytecode const
//Alias: caml_simd_float32_round_pos_inf_bytecode
//Version: >= 5.2, < 5.3
//OxCaml
function caml_ceil_float32_bytecode(x) {
  return Math.fround(Math.ceil(x));
}

//Provides: caml_cos_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_cos_float32_bytecode(x) {
  return Math.fround(Math.cos(x));
}

//Provides: caml_exp_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_exp_float32_bytecode(x) {
  return Math.fround(Math.exp(x));
}

//Provides: caml_floor_float32_bytecode const
//Alias: caml_simd_float32_round_neg_inf_bytecode
//Version: >= 5.2, < 5.3
//OxCaml
function caml_floor_float32_bytecode(x) {
  return Math.fround(Math.floor(x));
}

//Provides: caml_log_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_log_float32_bytecode(x) {
  return Math.fround(Math.log(x));
}

//Provides: caml_power_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_power_float32_bytecode(x, y) {
  return Math.fround(Math.pow(x, y));
}

//Provides: caml_sin_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_sin_float32_bytecode(x) {
  return Math.fround(Math.sin(x));
}

//Provides: caml_sqrt_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_sqrt_float32_bytecode(x) {
  return Math.fround(Math.sqrt(x));
}

//Provides: caml_tan_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_tan_float32_bytecode(x) {
  return Math.fround(Math.tan(x));
}

//Provides: caml_nextafter_float32_bytecode const
//Requires: caml_int32_bits_of_float, caml_int32_float_of_bits
//Version: >= 5.2, < 5.3
//OxCaml
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
//Version: >= 5.2, < 5.3
//OxCaml
function caml_trunc_float32_bytecode(x) {
  return Math.fround(Math.trunc(x));
}

//Provides: caml_classify_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
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
//Version: >= 5.2, < 5.3
//OxCaml
function caml_ldexp_float32_bytecode(x, y) {
  return Math.fround(caml_ldexp_float(x, y));
}

//Provides: caml_frexp_float32 const
//Requires: caml_frexp_float
//Version: >= 5.2, < 5.3
//OxCaml
const caml_frexp_float32 = caml_frexp_float; // Result is exact

//Provides: caml_copysign_float32_bytecode const
//Requires: caml_copysign_float
//Version: >= 5.2, < 5.3
//OxCaml
const caml_copysign_float32_bytecode = caml_copysign_float; // Result is exact

//Provides: caml_signbit_float32_bytecode const
//Requires: caml_signbit_float
//Version: >= 5.2, < 5.3
//OxCaml
const caml_signbit_float32_bytecode = caml_signbit_float;

//Provides: caml_expm1_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_expm1_float32_bytecode(x) {
  return Math.fround(Math.expm1(x));
}

//Provides: caml_exp2_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_exp2_float32_bytecode(x) {
  return Math.fround(Math.pow(2, x));
}

//Provides: caml_log1p_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_log1p_float32_bytecode(x) {
  return Math.fround(Math.log1p(x));
}

//Provides: caml_log2_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_log2_float32_bytecode(x) {
  return Math.fround(Math.log2(x));
}

//Provides: caml_hypot_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_hypot_float32_bytecode(x, y) {
  return Math.fround(Math.hypot(x, y));
}

//Provides: caml_log10_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_log10_float32_bytecode(x) {
  return Math.fround(Math.log10(x));
}

//Provides: caml_cosh_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_cosh_float32_bytecode(x) {
  return Math.fround(Math.cosh(x));
}

//Provides: caml_acosh_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_acosh_float32_bytecode(x) {
  return Math.fround(Math.acosh(x));
}

//Provides: caml_sinh_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_sinh_float32_bytecode(x) {
  return Math.fround(Math.sinh(x));
}

//Provides: caml_asinh_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_asinh_float32_bytecode(x) {
  return Math.fround(Math.asinh(x));
}

//Provides: caml_tanh_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_tanh_float32_bytecode(x) {
  return Math.fround(Math.tanh(x));
}

//Provides: caml_atanh_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_atanh_float32_bytecode(x) {
  return Math.fround(Math.atanh(x));
}

//Provides: caml_round_float32_bytecode const
//Requires: caml_round_float
//Alias: caml_simd_float32_round_current_bytecode
//Version: >= 5.2, < 5.3
//OxCaml
function caml_round_float32_bytecode(x) {
  return Math.fround(caml_round_float(x));
}

//Provides: caml_cbrt_float32_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_cbrt_float32_bytecode(x) {
  return Math.fround(Math.cbrt(x));
}

//Provides: caml_erf_float32_bytecode const
//Requires: caml_erf_float
//Version: >= 5.2, < 5.3
//OxCaml
function caml_erf_float32_bytecode(x) {
  return Math.fround(caml_erf_float(x));
}

//Provides: caml_erfc_float32_bytecode const
//Requires: caml_erfc_float
//Version: >= 5.2, < 5.3
//OxCaml
function caml_erfc_float32_bytecode(x) {
  return Math.fround(caml_erfc_float(x));
}

//Provides: caml_fma_float32_bytecode const
//Requires: caml_fma_float
//Version: >= 5.2, < 5.3
//OxCaml
function caml_fma_float32_bytecode(x, y, z) {
  return Math.fround(caml_fma_float(x, y, z));
}

//Provides: caml_simd_float32_min_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_simd_float32_min_bytecode(x, y) {
  return Math.min(x, y);
}

//Provides: caml_simd_float32_max_bytecode const
//Version: >= 5.2, < 5.3
//OxCaml
function caml_simd_float32_max_bytecode(x, y) {
  return Math.max(x, y);
}

//Provides: caml_simd_cast_float32_int64_bytecode const
//Requires: caml_int64_of_float
//Version: >= 5.2, < 5.3
//OxCaml
function caml_simd_cast_float32_int64_bytecode(x) {
  return caml_int64_of_float(Math.round(x));
}

//Provides: caml_ba_uint8_getf32
//Requires: caml_ba_uint8_get32, caml_float32_of_bits_bytecode
//Version: >= 5.2, < 5.3
//OxCaml
function caml_ba_uint8_getf32(ba, i) {
  return caml_float32_of_bits_bytecode(caml_ba_uint8_get32(ba, i));
}

//Provides: caml_ba_uint8_setf32
//Requires: caml_ba_uint8_set32, caml_float32_to_bits_bytecode
//Version: >= 5.2, < 5.3
//OxCaml
function caml_ba_uint8_setf32(ba, i, v) {
  return caml_ba_uint8_set32(ba, i, caml_float32_to_bits_bytecode(v));
}

//Provides: caml_string_getf32
//Requires: caml_string_get32, caml_float32_of_bits_bytecode
//Version: >= 5.2, < 5.3
//OxCaml
function caml_string_getf32(ba, i) {
  return caml_float32_of_bits_bytecode(caml_string_get32(ba, i));
}

//Provides: caml_bytes_getf32
//Requires: caml_bytes_get32, caml_float32_of_bits_bytecode
//Version: >= 5.2, < 5.3
//OxCaml
function caml_bytes_getf32(ba, i) {
  return caml_float32_of_bits_bytecode(caml_bytes_get32(ba, i));
}

//Provides: caml_bytes_setf32
//Requires: caml_bytes_set32, caml_float32_to_bits_bytecode
//Version: >= 5.2, < 5.3
//OxCaml
function caml_bytes_setf32(ba, i, v) {
  return caml_bytes_set32(ba, i, caml_float32_to_bits_bytecode(v));
}

//Provides: caml_is_boot_compiler
//Version: >= 5.2, < 5.3
//OxCaml
function caml_is_boot_compiler(_unit) {
  return 0;
}

///////// CORE_KERNEL
//Provides: int_math_int_pow_stub
function int_math_int_pow_stub(base, exponent){
  var one = 1;
  var mul = [one, base, one, one];
  var res = one;
  while (!exponent==0) {
    mul[1] = (mul[1] * mul[3]) | 0;
    mul[2] = (mul[1] * mul[1]) | 0;
    mul[3] = (mul[2] * mul[1]) | 0;
    res = (res * mul[exponent& 3]) | 0;
    exponent = exponent >> 2;
  }
  return res;
}

//Provides: int_math_int64_pow_stub
//Requires: caml_int64_mul, caml_int64_is_zero, caml_int64_shift_right_unsigned
function int_math_int64_pow_stub(base, exponent){
  var one = [255,1,0,0];
  var mul = [one, base, one, one];
  var res = one;
  while (!caml_int64_is_zero(exponent)) {
    mul[1] = caml_int64_mul(mul[1],mul[3]);
    mul[2] = caml_int64_mul(mul[1],mul[1]);
    mul[3] = caml_int64_mul(mul[2],mul[1]);
    res = caml_int64_mul(res, mul[exponent[1]& 3]);
    exponent = caml_int64_shift_right_unsigned(exponent, 2);
  }
  return res;
}

//Provides: int_math_int_popcount
function int_math_int_popcount(v) {
  v = v - ((v >>> 1) & 0x55555555);
  v = (v & 0x33333333) + ((v >>> 2) & 0x33333333);
  return ((v + (v >>> 4) & 0xF0F0F0F) * 0x1010101) >>> 24;
}

//Provides: caml_hash_string
//Requires: caml_hash
function caml_hash_string(s) {
  return caml_hash(1,1,0,s)
}
//Provides: caml_hash_double
//Requires: caml_hash
function caml_hash_double(d) {
  return caml_hash(1,1,0,d);
}

//Provides: core_heap_block_is_heap_block
function core_heap_block_is_heap_block(x){
  return +(x instanceof Array);
}

//Provides: core_array_unsafe_int_blit
//Requires: caml_array_blit
var core_array_unsafe_int_blit = caml_array_blit
//Provides: core_array_unsafe_float_blit
//Requires: caml_array_blit
var core_array_unsafe_float_blit = caml_array_blit

//Provides: core_kernel_time_ns_gettime_or_zero
//Requires: caml_int64_mul, caml_int64_of_float, caml_int64_of_int32
var ms_to_nano = caml_int64_of_int32(1000*1000);
function core_kernel_time_ns_gettime_or_zero(){
  var ms = Date.now();
  var ms_i64 = caml_int64_of_float(ms);
  return caml_int64_mul(ms_i64,ms_to_nano);
}
//Provides: core_kernel_time_ns_format
//Requires: caml_to_js_string, caml_js_to_string
function core_kernel_time_ns_format(time,format){
  var d = new Date(time * 1000);
  var formatjs = caml_to_js_string(format);
  var jstring = joo_global_object.strftime(formatjs, d);
  return caml_js_to_string(jstring);
}

//Provides: core_kernel_gc_compactions
function core_kernel_gc_compactions () { return 0 }
//Provides: core_kernel_gc_heap_chunks
function core_kernel_gc_heap_chunks () { return 0 }
//Provides: core_kernel_gc_heap_words
function core_kernel_gc_heap_words () { return 0 }
//Provides: core_kernel_gc_major_collections
function core_kernel_gc_major_collections () { return 0 }
//Provides: core_kernel_gc_major_plus_minor_words
function core_kernel_gc_major_plus_minor_words () { return 0 }
//Provides: core_kernel_gc_major_words
function core_kernel_gc_major_words () { return 0 }
//Provides: core_kernel_gc_minor_collections
function core_kernel_gc_minor_collections () { return 0 }
//Provides: core_kernel_gc_minor_words
function core_kernel_gc_minor_words () { return 0 }
//Provides: core_kernel_gc_promoted_words
function core_kernel_gc_promoted_words () { return 0 }
//Provides: core_kernel_gc_top_heap_words
function core_kernel_gc_top_heap_words () { return 0 }
//Provides: clear_caml_backtrace_pos
function clear_caml_backtrace_pos () { return 0 }

//Provides: internalhash_fold_int64
//Requires: caml_hash_mix_int64
var internalhash_fold_int64 = caml_hash_mix_int64
//Provides: internalhash_fold_int
//Requires: caml_hash_mix_int
var internalhash_fold_int = caml_hash_mix_int
//Provides: internalhash_fold_float
//Requires: caml_hash_mix_float
var internalhash_fold_float = caml_hash_mix_float
//Provides: internalhash_fold_string
//Requires: caml_hash_mix_string
var internalhash_fold_string = caml_hash_mix_string
//Provides: internalhash_fold_bigstring
//Requires: caml_hash_mix_bigstring
var internalhash_fold_bigstring = caml_hash_mix_bigstring
//Provides: internalhash_get_hash_value
//Requires: caml_hash_mix_final
function internalhash_get_hash_value (seed) {
  var h = caml_hash_mix_final(seed);
  return h & 0x3FFFFFFF;
}

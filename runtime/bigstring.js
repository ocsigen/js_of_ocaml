///////// BIGSTRING

//Provides: caml_hash_mix_bigstring
//Requires: caml_hash_mix_bytes_arr
function caml_hash_mix_bigstring(h, bs) {
  return caml_hash_mix_bytes_arr(h,bs.data);
}

//Provides: bigstring_to_array_buffer mutable
function bigstring_to_array_buffer(bs) {
  return bs.data.buffer
}

//Provides: bigstring_to_typed_array mutable
function bigstring_to_typed_array(bs) {
  return bs.data
}

//Provides: bigstring_of_array_buffer mutable
//Requires: caml_ba_create_unsafe
function bigstring_of_array_buffer(ab) {
  var ta = new globalThis.Uint8Array(ab);
  return caml_ba_create_unsafe(12, 0, [ta.length], ta);
}

//Provides: bigstring_of_typed_array mutable
//Requires: caml_ba_create_unsafe
function bigstring_of_typed_array(ba) {
  var ta = new globalThis.Uint8Array(ba.buffer, ba.byteOffset, ba.length * ba.BYTES_PER_ELEMENT);
  return caml_ba_create_unsafe(12, 0, [ta.length], ta);
}

//Provides: caml_bigstring_memcmp
//Requires: caml_ba_get_1
function caml_bigstring_memcmp(s1, pos1, s2, pos2, len){
  for (var i = 0; i < len; i++) {
    var a = caml_ba_get_1(s1,pos1 + i);
    var b = caml_ba_get_1(s2,pos2 + i);
    if (a < b) return -1;
    if (a > b) return 1;
  }
  return 0;
}

//Provides: caml_bigstring_blit_ba_to_ba
//Requires: caml_invalid_argument, caml_array_bound_error
function caml_bigstring_blit_ba_to_ba(ba1, pos1, ba2, pos2, len){
  if(12 != ba1.kind)
    caml_invalid_argument("caml_bigstring_blit_ba_to_ba: kind mismatch");
  if(12 != ba2.kind)
    caml_invalid_argument("caml_bigstring_blit_ba_to_ba: kind mismatch");
  if(len == 0) return 0;
  var ofs1 = ba1.offset(pos1);
  var ofs2 = ba2.offset(pos2);
  if(ofs1 + len > ba1.data.length){
    caml_array_bound_error();
  }
  if(ofs2 + len > ba2.data.length){
    caml_array_bound_error();
  }
  var slice = ba1.data.subarray(ofs1,ofs1+len);
  ba2.data.set(slice,pos2);
  return 0
}

//Provides: caml_bigstring_blit_string_to_ba
//Requires: caml_invalid_argument, caml_array_bound_error, caml_array_of_string
//Requires: caml_ml_string_length
function caml_bigstring_blit_string_to_ba(str1, pos1, ba2, pos2, len){
  if(12 != ba2.kind)
    caml_invalid_argument("caml_bigstring_blit_string_to_ba: kind mismatch");
  if(len == 0) return 0;
  var ofs2 = ba2.offset(pos2);
  if(pos1 + len > caml_ml_string_length(str1)) {
    caml_array_bound_error();
  }
  if(ofs2 + len > ba2.data.length) {
    caml_array_bound_error();
  }
  var slice = caml_array_of_string(str1).slice(pos1,pos1 + len);
  ba2.data.set(slice,ofs2);
  return 0
}

//Provides: caml_bigstring_blit_bytes_to_ba
//Requires: caml_invalid_argument, caml_array_bound_error, caml_array_of_bytes
//Requires: caml_ml_bytes_length
function caml_bigstring_blit_bytes_to_ba(str1, pos1, ba2, pos2, len){
  if(12 != ba2.kind)
    caml_invalid_argument("caml_bigstring_blit_string_to_ba: kind mismatch");
  if(len == 0) return 0;
  var ofs2 = ba2.offset(pos2);
  if(pos1 + len > caml_ml_bytes_length(str1)) {
    caml_array_bound_error();
  }
  if(ofs2 + len > ba2.data.length) {
    caml_array_bound_error();
  }
  var slice = caml_array_of_bytes(str1).slice(pos1,pos1 + len);
  ba2.data.set(slice,ofs2);
  return 0
}

//Provides: caml_bigstring_blit_ba_to_bytes
//Requires: caml_invalid_argument, caml_array_bound_error
//Requires: caml_blit_bytes, caml_bytes_of_array
//Requires: caml_ml_bytes_length
function caml_bigstring_blit_ba_to_bytes(ba1, pos1, bytes2, pos2, len){
  if(12 != ba1.kind)
    caml_invalid_argument("caml_bigstring_blit_string_to_ba: kind mismatch");
  if(len == 0) return 0;
  var ofs1 = ba1.offset(pos1);
  if(ofs1 + len > ba1.data.length){
    caml_array_bound_error();
  }
  if(pos2 + len > caml_ml_bytes_length(bytes2)){
    caml_array_bound_error();
  }
  var slice = ba1.data.slice(ofs1, ofs1+len);
  caml_blit_bytes(caml_bytes_of_array(slice), 0, bytes2, pos2, len);
  return 0
}

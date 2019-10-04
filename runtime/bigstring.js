///////// BIGSTRING
//Provides: bigstring_alloc
//Requires: caml_ba_create
//Weakdef
//Will be defined in Base
function bigstring_alloc(_,size){
  return caml_ba_create(12, 0, [0,size]);
}

//Provides: bigstring_destroy_stub
//Requires: caml_invalid_argument, caml_ba_create_unsafe
//Weakdef
//Will be defined in Core_kernel
function bigstring_destroy_stub(v_bstr) {
  if (v_bstr.hasOwnProperty('__is_deallocated')) {
    caml_invalid_argument("bigstring_destroy: bigstring is already deallocated");
  }
  // Mutate the original bigstring in-place, to simulate what the C version does
  v_bstr.__is_deallocated = true;
  v_bstr.data = new v_bstr.data.__proto__.constructor(0);
  v_bstr.dims = [0];
  return 0;
}

//Provides: bigstring_blit_bigstring_bytes_stub
//Requires: caml_bytes_set, caml_ba_get_1
//Weakdef
//Will be defined in Base
function bigstring_blit_bigstring_bytes_stub(v_bstr, v_src_pos, v_str, v_dst_pos, v_len){
  for(var i = 0; i < v_len; i++){
    var c = caml_ba_get_1(v_bstr,v_src_pos + i);
    caml_bytes_set(v_str,v_dst_pos + i,c);
  }
  return 0;
}

//Provides: bigstring_blit_bigstring_string_stub
//Requires: bigstring_blit_bigstring_bytes_stub
//From cstruct
var bigstring_blit_bigstring_string_stub = bigstring_blit_bigstring_bytes_stub

//Provides: caml_blit_bigstring_to_string
//Requires: bigstring_blit_bigstring_bytes_stub
//From cstruct
var caml_blit_bigstring_to_string = bigstring_blit_bigstring_bytes_stub

//Provides: bigstring_blit_string_bigstring_stub
//Requires: caml_string_get, caml_ba_set_1
//Weakdef
//Will be defined in Base
function bigstring_blit_string_bigstring_stub(v_str, v_src_pos, v_bstr, v_dst_pos, v_len){
  for (var i = 0; i < v_len; i++) caml_ba_set_1(v_bstr,v_dst_pos + i,caml_string_get(v_str,v_src_pos + i));
  return 0;
}

//Provides: bigstring_blit_bytes_bigstring_stub
//Requires: caml_bytes_get, caml_ba_set_1
//Weakdef
//Will be defined in Base
function bigstring_blit_bytes_bigstring_stub(v_str, v_src_pos, v_bstr, v_dst_pos, v_len){
  for (var i = 0; i < v_len; i++) caml_ba_set_1(v_bstr,v_dst_pos + i,caml_bytes_get(v_str,v_src_pos + i));
  return 0;
}
//Provides: caml_blit_string_to_bigstring
//Requires: bigstring_blit_string_bigstring_stub
var caml_blit_string_to_bigstring = bigstring_blit_string_bigstring_stub

//Provides: bigstring_blit_stub
//Requires: caml_ba_get_1, caml_ba_set_1
//Weakdef
//Will be defined in Base
function bigstring_blit_stub(s1, i1, s2, i2, len){
  for (var i = 0; i < len; i++) caml_ba_set_1(s2,i2 + i,caml_ba_get_1(s1,i1 + i));
  return 0;
}

//Provides: bigstring_memcmp_stub
//Requires: caml_ba_get_1
//Weakdef
//Will be defined in Base
function bigstring_memcmp_stub(v_s1, v_s1_pos, v_s2, v_s2_pos, v_len){
  for (var i = 0; i < v_len; i++) {
    var a = caml_ba_get_1(v_s1,v_s1_pos + i);
    var b = caml_ba_get_1(v_s2,v_s2_pos + i);
    if (a < b) return -1;
    if (a > b) return 1;
  }
  return 0;
}

//Provides: bigstring_find
//Requires: caml_ba_get_1
//Weakdef
//Will be defined in Base
function bigstring_find(bs, chr, pos, len){
  while(len > 0){
    if(caml_ba_get_1(bs,pos) == chr) return pos;
    pos++;
    len--;
  }
  return -1;
}

//Provides: bigstring_to_array_buffer mutable
//js_of_ocaml lib
function bigstring_to_array_buffer(bs) {
  return bs.data.buffer
}

//Provides: bigstring_of_array_buffer mutable
//Requires: caml_ba_create_unsafe
//js_of_ocaml lib
function bigstring_of_array_buffer(ab) {
  var ta = new joo_global_object.Uint8Array(ab);
  return caml_ba_create_unsafe(12, 0, [ta.length], ta);
}

//Provides: caml_hash_mix_bigstring
//Requires: caml_hash_mix_string_arr
function caml_hash_mix_bigstring(h, bs) {
  return caml_hash_mix_string_arr(h,bs.data);
}

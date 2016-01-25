///////// BIGSTRING
//Provides: bigstring_alloc
//Requires: caml_ba_create
function bigstring_alloc(_,size){
  return caml_ba_create(12, 0, [0,size]);
}

//Provides: bigstring_destroy_stub
function bigstring_destroy_stub(_v) {
  return 0; // noop
}

//Provides: bigstring_blit_bigstring_string_stub
//Requires: caml_string_set, caml_ba_get_1
function bigstring_blit_bigstring_string_stub(v_bstr, v_src_pos, v_str, v_dst_pos, v_len){
  for (var i = 0; i < v_len; i++) caml_string_set(v_str,v_dst_pos + i,caml_ba_get_1(v_bstr,v_src_pos + i));
  return 0;
}

//Provides: caml_blit_bigstring_to_string
//Requires: bigstring_blit_bigstring_string_stub
var caml_blit_bigstring_to_string = bigstring_blit_bigstring_string_stub

//Provides: bigstring_blit_string_bigstring_stub
//Requires: caml_string_get, caml_ba_set_1
function bigstring_blit_string_bigstring_stub(v_str, v_src_pos, v_bstr, v_dst_pos, v_len){
  for (var i = 0; i < v_len; i++) caml_ba_set_1(v_bstr,v_dst_pos + i,caml_string_get(v_str,v_src_pos + i));
  return 0;
}

//Provides: caml_blit_string_to_bigstring
//Requires: bigstring_blit_string_bigstring_stub
var caml_blit_string_to_bigstring = bigstring_blit_string_bigstring_stub

//Provides: bigstring_blit_stub
//Requires: caml_ba_get_1, caml_ba_set_1
function bigstring_blit_stub(s1, i1, s2, i2, len){
  for (var i = 0; i < len; i++) caml_ba_set_1(s2,i2 + i,caml_ba_get_1(s1,i1 + i));
  return 0;
}

//Provides: bigstring_memcmp_stub
//Requires: caml_ba_get_1
function bigstring_memcmp_stub(v_s1, v_s1_pos, v_s2, v_s2_pos, v_len){
  for (var i = 0; i < v_len; i++) {
    var a = caml_ba_get_1(v_s1,v_s1_pos + i);
    var b = caml_ba_get_1(v_s2,v_s2_pos + i);
    if (a < b) return -1;
    if (a > b) return 1;
  }
  return 0;
}

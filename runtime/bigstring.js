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

//Provides: bigstring_find
//Requires: caml_ba_get_1
function bigstring_find(bs, chr, pos, len){
  while(len > 0){
    if(caml_ba_get_1(bs,pos) == chr) return pos;
    pos++;
    len--;
  }
  return -1;
}

//Provides: bigstring_to_array_buffer mutable
function bigstring_to_array_buffer(bs) {
  return bs.data.buffer
}

//Provides: bigstring_of_array_buffer mutable
//Requires: caml_ba_create_from
function bigstring_of_array_buffer(ab) {
  var ta = new joo_global_object.Uint8Array(ab);
  return caml_ba_create_from(ta, null, 0, 12, 0, [ta.length])
}

//Provides: bigstring_marshal_data_size_stub mutable
//Requires: caml_failwith, caml_ba_uint8_get32
function bigstring_marshal_data_size_stub (s, ofs) {
  if (caml_ba_uint8_get32(s, ofs) != (0x8495A6BE|0))
    caml_failwith("Marshal.data_size: bad object");
  return (caml_ba_uint8_get32(s, ofs + 4));
}

//Provides: bigstring_unmarshal_stub mutable
//Requires: BigStringReader, caml_input_value_from_reader
function bigstring_unmarshal_stub(s,ofs) {
  var reader = new BigStringReader (s, typeof ofs=="number"?ofs:ofs[0]);
  return caml_input_value_from_reader(reader, ofs)
}


//Provides: bigstring_marshal_stub mutable
//Requires: caml_output_val, bigstring_alloc, caml_ba_set_1
function bigstring_marshal_stub (v, _fl) {
  /* ignores flags... */
  var arr = caml_output_val (v);
  var bs  = bigstring_alloc(0,arr.length);
  for(var i = 0; i < arr.length; i++){
    caml_ba_set_1(bs, i, arr[i]);
  }
  return bs;
}

//Provides: bigstring_marshal_blit_stub
//Requires: caml_output_val, caml_failwith, caml_ba_set_1
function bigstring_marshal_blit_stub (s, ofs, len, v, _fl) {
  /* ignores flags... */
  var t = caml_output_val (v);
  if (t.length > len) caml_failwith ("Marshal.to_buffer: buffer overflow");
  for(var i = 0; i < t.length; i++){
    caml_ba_set_1(s, (i + ofs), t[i]);
  }
  return t.length;
}

//Provides: caml_hash_mix_bigstring
//Requires: caml_hash_mix_string_arr
function caml_hash_mix_bigstring(h, bs) {
    return caml_hash_mix_string_arr(h,bs.data);
}

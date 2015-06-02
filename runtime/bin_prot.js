///////// BIN_PROT

//Provides: bin_prot_get_float_offset
//Requires: caml_float_of_bytes, caml_ba_get_1
function bin_prot_get_float_offset(a,p){
  var t = new Array(8);;
  for (var i = 0;i < 8;i++) t[i] = caml_ba_get_1(a,p++);
  var v = caml_float_of_bytes (t);
  return [254,v];
}


//Provides: bin_prot_blit_buf_float_array_stub
//Requires: caml_array_set, caml_ba_get_1
function bin_prot_blit_buf_float_array_stub(v_src_pos, v_buf, v_dst_pos, v_arr, v_len){
  var c;
  var t = new Array(8);;
  for(var i = 0; i < v_len; i++){
    for (var j = 0;j < 8;j++) t[j] = caml_ba_get_1(v_buf,v_src_pos+j+(i*8));
    caml_array_set(v_arr,v_dst_pos+i,c);
  }
  return 0
}
//Provides: bin_prot_blit_buf_string_stub
//Requires: caml_ba_get_1, caml_string_unsafe_set
function bin_prot_blit_buf_string_stub(v_src_pos, v_buf, v_dst_pos, v_str, v_len){
  var c;
  for(var i = 0; i < v_len; i++){
    c = caml_ba_get_1(v_buf,v_src_pos+i);
    caml_string_unsafe_set(v_str,v_dst_pos+i,c);
  }
  return 0
}
//Provides: bin_prot_blit_float_array_buf_stub
//Requires: caml_array_get, caml_ba_set_1
function bin_prot_blit_float_array_buf_stub(v_src_pos, v_arr, v_dst_pos, v_buf, v_len){
  var c;
  for(var i = 0; i < v_len; i++){
    c = caml_array_get(v_arr,v_src_pos+i);
    for (var j = 0;j < 8;j++) caml_ba_set_1(v_buf,v_dst_pos+j+(i*8));
  }
  return 0
}
//Provides: bin_prot_blit_string_buf_stub
//Requires: caml_string_unsafe_get, caml_ba_set_1
function bin_prot_blit_string_buf_stub (v_src_pos, v_str, v_dst_pos, v_buf, v_len){
  var c;
  for(var i = 0; i < v_len; i++){
    c = caml_string_unsafe_get(v_str,v_src_pos+i);
    caml_ba_set_1(v_buf,v_dst_pos+i,c);
  }
  return 0
}

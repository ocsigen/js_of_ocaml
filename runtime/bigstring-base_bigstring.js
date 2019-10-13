
//The following are defined in Base_bigstring
//There are just provided here for compatibility reasons

//Provides: bigstring_alloc
//Requires: caml_ba_create
//Weakdef
function bigstring_alloc(_,size){ return caml_ba_create(12, 0, [0,size]) }
//Provides: bigstring_blit_stub
//Requires: caml_bigstring_blit_ba_to_ba
//Weakdef
var bigstring_blit_stub = caml_bigstring_blit_ba_to_ba
//Provides: bigstring_blit_bytes_bigstring_stub
//Requires: caml_bigstring_blit_string_to_ba
//Weakdef
var bigstring_blit_bytes_bigstring_stub = caml_bigstring_blit_string_to_ba
//Provides: bigstring_blit_string_bigstring_stub
//Requires: caml_bigstring_blit_string_to_ba
//Weakdef
var bigstring_blit_string_bigstring_stub = caml_bigstring_blit_string_to_ba
//Provides: bigstring_blit_bigstring_bytes_stub
//Requires: caml_bigstring_blit_ba_to_bytes
//Weakdef
var bigstring_blit_bigstring_bytes_stub = caml_bigstring_blit_ba_to_bytes;
//Provides: bigstring_blit_bigstring_string_stub
//Requires: caml_bigstring_blit_ba_to_bytes
//Weakdef
var bigstring_blit_bigstring_string_stub = caml_bigstring_blit_ba_to_bytes
//Provides: bigstring_memcmp_stub
//Requires: caml_bigstring_memcmp
//Weakdef
var bigstring_memcmp_stub = caml_bigstring_memcmp
//Provides: bigstring_find
//Requires: caml_ba_get_1
//Weakdef
function bigstring_find(bs, chr, pos, len){
  while(len > 0){
    if(caml_ba_get_1(bs,pos) == chr) return pos;
    pos++;
    len--;
  }
  return -1;
}

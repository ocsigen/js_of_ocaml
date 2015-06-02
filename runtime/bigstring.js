///////// BIGSTRING
//Provides: bigstring_alloc
//Requires: caml_ba_create
function bigstring_alloc(_,size){
  return caml_ba_create(12, 0, [0,size]);
}

// bigstring_blit_bigstring_string_stub
// bigstring_blit_string_bigstring_stub
// bigstring_blit_stub

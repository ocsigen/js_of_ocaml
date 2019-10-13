//The following are defined in Cstruct
//There are just provided here for compatibility reasons

//Provides: caml_blit_bigstring_to_bigstring
//Requires: caml_bigstring_blit_ba_to_ba
//Weakdef
var caml_blit_bigstring_to_bigstring = caml_bigstring_blit_ba_to_ba
//Provides: caml_blit_bigstring_to_string
//Requires: caml_bigstring_blit_ba_to_bytes
//Weakdef
var caml_blit_bigstring_to_string = caml_bigstring_blit_ba_to_bytes
//Provides: caml_blit_string_to_bigstring
//Requires: caml_bigstring_blit_string_to_ba
//Weakdef
var caml_blit_string_to_bigstring = caml_bigstring_blit_string_to_ba

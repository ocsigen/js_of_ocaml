//The following are defined in Core_kernel
//There are just provided here for compatibility reasons

//Provides: bigstring_destroy_stub
//Requires: caml_invalid_argument, caml_ba_create_unsafe
//Weakdef
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

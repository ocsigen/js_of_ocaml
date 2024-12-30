//Provides: test_lib_jsoo_a
//Requires: caml_string_of_jsbytes
//Requires: caml_ml_output
//Requires: caml_ml_string_length
function test_lib_jsoo_a(unit) {
  var s = caml_string_of_jsbytes("test_lib_jsoo_a: ok\n");
  caml_ml_output(2, s, 0, caml_ml_string_length(s));
  return 42;
}

//Provides: caml_unregister_named_value (const)
//Requires: caml_named_values, caml_jsbytes_of_string
function caml_unregister_named_value(nm) {
  nm = caml_jsbytes_of_string(nm);
  delete caml_named_values[nm];
  return 0;
}

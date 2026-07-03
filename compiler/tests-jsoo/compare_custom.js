// Test helpers reproducing a zarith_stubs_js-like custom block: small values
// are plain immediates while large ones are custom blocks whose custom
// operations only register a [compare] (used, like the native [compare_ext],
// to compare the block against an immediate).

//Provides: test_zt_value
function test_zt_value(z) {
  return typeof z === "number" ? z : z.v;
}

//Provides: test_zt_compare
//Requires: test_zt_value
function test_zt_compare(a, b) {
  var av = test_zt_value(a);
  var bv = test_zt_value(b);
  return av < bv ? -1 : av > bv ? 1 : 0;
}

//Provides: test_zt_register
//Requires: caml_custom_ops, test_zt_compare
function test_zt_register(unit) {
  caml_custom_ops._zt = { compare: test_zt_compare };
  return 0;
}

//Provides: test_zt_make
function test_zt_make(v) {
  return { caml_custom: "_zt", v: v };
}

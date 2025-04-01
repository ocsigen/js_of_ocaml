//Provides: caml_lxm_M
//Requires: caml_int64_of_string
//Requires: caml_string_of_jsstring
//Version: >= 5
var caml_lxm_M = caml_int64_of_string(
  caml_string_of_jsstring("0xd1342543de82ef95"),
);

//Provides: caml_lxm_daba
//Requires: caml_int64_of_string
//Requires: caml_string_of_jsstring
//Version: >= 5
var caml_lxm_daba = caml_int64_of_string(
  caml_string_of_jsstring("0xdaba0b6eb09322e3"),
);

//Provides: caml_lxm_next const
//Requires: caml_int64_shift_left
//Requires: caml_int64_shift_right_unsigned
//Requires: caml_int64_or
//Requires: caml_int64_xor
//Requires: caml_int64_add
//Requires: caml_int64_mul
//Requires: caml_ba_get_1
//Requires: caml_ba_set_1
//Requires: caml_lxm_M
//Requires: caml_lxm_daba
//Version: >= 5
function caml_lxm_next(v) {
  function shift_l(x, k) {
    return caml_int64_shift_left(x, k);
  }
  function shift_r(x, k) {
    return caml_int64_shift_right_unsigned(x, k);
  }
  function or(a, b) {
    return caml_int64_or(a, b);
  }
  function xor(a, b) {
    return caml_int64_xor(a, b);
  }
  function add(a, b) {
    return caml_int64_add(a, b);
  }
  function mul(a, b) {
    return caml_int64_mul(a, b);
  }
  function rotl(x, k) {
    return or(shift_l(x, k), shift_r(x, 64 - k));
  }
  function get(a, i) {
    return caml_ba_get_1(a, i);
  }
  function set(a, i, x) {
    return caml_ba_set_1(a, i, x);
  }
  var M = caml_lxm_M;
  var daba = caml_lxm_daba;
  var z, q0, q1;
  var st = v;
  var a = get(st, 0);
  var s = get(st, 1);
  var x0 = get(st, 2);
  var x1 = get(st, 3);
  /* Combining operation */
  z = add(s, x0);
  /* Mixing function */
  z = mul(xor(z, shift_r(z, 32)), daba);
  z = mul(xor(z, shift_r(z, 32)), daba);
  z = xor(z, shift_r(z, 32));
  /* LCG update */
  set(st, 1, add(mul(s, M), a));
  /* XBG update */
  var q0 = x0;
  var q1 = x1;
  q1 = xor(q1, q0);
  q0 = rotl(q0, 24);
  q0 = xor(xor(q0, q1), shift_l(q1, 16));
  q1 = rotl(q1, 37);
  set(st, 2, q0);
  set(st, 3, q1);
  /* Return result */
  return z;
}

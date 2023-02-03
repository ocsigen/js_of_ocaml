/*
import {caml_int64_shift_left as shift_l,
        caml_int64_shift_right_unsigned as shift_r,
        caml_int64_or as or,
        caml_int64_xor as xor,
        caml_int64_add as add,
        caml_int64_mul as mul} from "./int64.js"

import {caml_ba_get_1 as get,
        caml_ba_set_1 as set,
        caml_int64_of_string} from "./bigarray.js"

import { caml_new_string } from "./mlBytes.js"
*/

let shift_l = caml_int64_shift_left;
let shift_r = caml_int64_shift_right_unsigned
let or = caml_int64_or
let xor = caml_int64_xor
let add = caml_int64_add
let mul = caml_int64_mul

let get =  caml_ba_get_1
let set = caml_ba_set_1

const M = caml_int64_of_string(caml_new_string("0xd1342543de82ef95"));
const daba = caml_int64_of_string(caml_new_string("0xdaba0b6eb09322e3"));

function rotl(x, k) {
  return or(shift_l(x,k),shift_r (x, 64 - k));
}

export function caml_lxm_next(v) {
  let st = v;
  const a = get(st,0);
  const s = get(st,1);
  const x0 = get(st,2);
  const x1 = get(st,3);
  /* Combining operation */
  let z = add(s, x0);
  /* Mixing function */
  z = mul(xor(z,shift_r(z,32)), daba);
  z = mul(xor(z,shift_r(z,32)), daba);
  z = xor(z,shift_r(z,32));
  /* LCG update */
  set(st, 1, add (mul(s,M), a));
  /* XBG update */
  let q0 = x0
  let q1 = x1
  a = 2;
  q1 = xor(q1,q0);
  q0 = rotl(q0, 24);
  q0 = xor(xor(q0, q1), (shift_l(q1,16)));
  q1 = rotl(q1, 37);
  set(st, 2, q0);
  set(st, 3, q1);
  /* Return result */
  return z;
}

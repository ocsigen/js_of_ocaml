//Provides: caml_lxm_umulh const
//Version: >= 5
// High 32 bits of the product of two 32-bit integers, read as unsigned
function caml_lxm_umulh(a, b) {
  var a0 = a & 0xffff,
    a1 = a >>> 16,
    b0 = b & 0xffff,
    b1 = b >>> 16;
  var m1 = a1 * b0 + ((a0 * b0) >>> 16);
  var m2 = a0 * b1 + (m1 & 0xffff);
  return (a1 * b1 + (m1 >>> 16) + (m2 >>> 16)) | 0;
}

//Provides: caml_lxm_next mutable
//Requires: caml_int64_create_lo_hi, caml_lxm_umulh
//Version: >= 5
function caml_lxm_next(v) {
  // The state is a bigarray of four int64 (a, s, x0, x1), stored as
  // (low, high) pairs of 32-bit integers. The 64-bit arithmetic is done
  // on such pairs.
  var st = v.data;
  var al = st[0],
    ah = st[1],
    sl = st[2],
    sh = st[3],
    x0l = st[4],
    x0h = st[5],
    x1l = st[6],
    x1h = st[7];
  var l, h;
  /* Combining operation: z = s + x0 */
  l = (sl >>> 0) + (x0l >>> 0);
  var zh = (sh + x0h + (l > 0xffffffff ? 1 : 0)) | 0,
    zl = l | 0;
  /* Mixing function: z = (z ^ (z >>> 32)) * daba, twice, then
     z ^ (z >>> 32), with daba = 0xdaba0b6eb09322e3 */
  zl ^= zh;
  h =
    (caml_lxm_umulh(zl, 0xb09322e3) +
      Math.imul(zl, 0xdaba0b6e) +
      Math.imul(zh, 0xb09322e3)) |
    0;
  zl = Math.imul(zl, 0xb09322e3);
  zh = h;
  zl ^= zh;
  h =
    (caml_lxm_umulh(zl, 0xb09322e3) +
      Math.imul(zl, 0xdaba0b6e) +
      Math.imul(zh, 0xb09322e3)) |
    0;
  zl = Math.imul(zl, 0xb09322e3);
  zh = h;
  zl ^= zh;
  /* LCG update: s = s * M + a, with M = 0xd1342543de82ef95 */
  h =
    (caml_lxm_umulh(sl, 0xde82ef95) +
      Math.imul(sl, 0xd1342543) +
      Math.imul(sh, 0xde82ef95)) |
    0;
  l = (Math.imul(sl, 0xde82ef95) >>> 0) + (al >>> 0);
  st[2] = l | 0;
  st[3] = (h + ah + (l > 0xffffffff ? 1 : 0)) | 0;
  /* XBG update */
  /* q1 = x1 ^ x0 */
  var q1l = x1l ^ x0l,
    q1h = x1h ^ x0h;
  /* q0 = rotl(x0, 24) ^ q1 ^ (q1 << 16) */
  var q0l = (x0l << 24) | (x0h >>> 8),
    q0h = (x0h << 24) | (x0l >>> 8);
  q0l ^= q1l ^ (q1l << 16);
  q0h ^= q1h ^ ((q1h << 16) | (q1l >>> 16));
  /* q1 = rotl(q1, 37) */
  st[4] = q0l;
  st[5] = q0h;
  st[6] = (q1h << 5) | (q1l >>> 27);
  st[7] = (q1l << 5) | (q1h >>> 27);
  /* Return result */
  return caml_int64_create_lo_hi(zl, zh);
}
